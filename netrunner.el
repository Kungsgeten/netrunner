;;; netrunner.el --- Create Android: Netrunner decklists using Company, Helm and org-mode

;; Copyright (C) 2016-- Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/netrunner.el
;; Version: 1.00
;; Keywords: games
;; Package-Requires: ((emacs "25.0") (popup "0.5.3") (company "0.9.0") (helm "1.9.5"))

;;; Commentary:

;; netrunner.el fetches data from NetrunnerDB.com, making it easy to create
;; Android: Netrunner decklists in Emacs (like any hacker should). This package
;; adds a backend for Company mode, adds "card info" links into org-mode, and
;; allows searching for cards with Helm.
;; 
;; In an org-mode buffer, run `netrunner-toggle-netrunner-buffer' (or add
;; "# -*- netrunner-buffer: t; -*-" to the top of the buffer). Now the company
;; backend should work for completing card names. You could also use
;; `helm-netrunner' to list cards.
;; 
;; If you to wish to preview card images in Helm, the images has to be
;; downloaded locally.  The images will be placed in `netrunner-image-dir' (a
;; directory called netrunner-images inside your `user-emacs-directory' by
;; default). Use `netrunner-download-all-images' to download images.

;;; Code:

(eval-when-compile
  (defvar url-http-codes)
  (defvar url-http-end-of-headers))

(require 'url)
(require 'json)
(require 'popup)
(require 'cl-lib)
(require 'company)
(require 'subr-x)
(require 'helm)


;; JSON

(defun netrunner-api-get (tail)
  "Get json data from NetrunnerDB API, with TAIL as argument.
An example for TAIL is \"cards/\" in order to get all cards."
  (with-current-buffer
      (url-retrieve-synchronously (format "http://netrunnerdb.com/api/%s"
                                          tail))
    (goto-char url-http-end-of-headers)
    (json-read)))

(defvar netrunner-cards (netrunner-api-get "cards/")
  "A list of all Netrunner cards and their json data.")

(defun netrunner-card-get-value (card property)
  "From Netrunner CARD json data, get PROPERTY."
  (cdr (assoc property card)))

(defun netrunner-card-property-p (card property value)
  "Do the CARD PROPERTY match VALUE?"
  (equal (netrunner-card-get-value card property) value))

(defun netrunner-filter (list &rest keyvalues)
  "Take LIST and only return entries which match KEYVALUES.
KEYVALUES are tuples with a keyword and a value." 
  (dolist (kv keyvalues)
    (setq list
          (cl-remove-if-not
           (lambda (card)
             (netrunner-card-property-p card (car kv) (cdr kv)))
           list)))
  list)

(defun netrunner-filter-first (list &rest keyvalues)
  "Runs `netrunner-filter' but only returns first match."
  (elt (apply 'netrunner-filter list keyvalues) 0))


;; Parsing

(defun netrunner-parse (card &optional omit-title)
  "Parse CARD data into an org-mode string.
If OMIT-TITLE, then do not include title in result string."
  (if (netrunner-card-property-p card 'type "Identity")
      (if (netrunner-card-property-p card 'side "Corp")
          (netrunner-parse-corp-identity card omit-title)
        (netrunner-parse-runner-identity card omit-title)) 
    (netrunner-parse-generic card omit-title)))

(defun netrunner-substitute-string (string)
  "Replaces characters in STRING from NetrunnerDB to org-mode syntax."
  (setq string (replace-regexp-in-string "</?strong>" "*" string))
  (setq string (replace-regexp-in-string "\\\[Subroutine\\\]" "↳" string))
  (setq string (replace-regexp-in-string "\\\[Click\\\]" "*Click*" string))
  (setq string (replace-regexp-in-string "\\\[Recurring Credits\\\]" "↶$" string))
  (setq string (replace-regexp-in-string "\\\[Credits\\\]" "$" string)))

(defun netrunner-parse-corp-identity (card &optional omit-title)
  "Parse CARD, which is a corp identity.
If OMIT-TITLE, then do not include title in result string."
  (format "%sIdentity: %s - %s/%s%s\n\n%s - %s"
          (if omit-title
              ""
            (format "%s\n" (netrunner-card-get-value card 'title))) 
          (netrunner-card-get-value card 'subtype)
          (netrunner-card-get-value card 'minimumdecksize)
          (or (netrunner-card-get-value card 'influencelimit) "∞")
          (if-let (text (netrunner-card-get-value card 'text)) 
              (format "\n\n%s" (netrunner-substitute-string text)) "") 
          (netrunner-card-get-value card 'faction)
          (netrunner-card-get-value card 'setname)))

(defun netrunner-parse-runner-identity (card &optional omit-title)
  "Parse CARD, which is a runner identity.
If OMIT-TITLE, then do not include title in result string."
  (format "%sIdentity: %s - %s/%s - %sL%s\n\n%s - %s"
          (if omit-title
              ""
            (format "%s\n" (netrunner-card-get-value card 'title))) 
          (netrunner-card-get-value card 'subtype)
          (netrunner-card-get-value card 'minimumdecksize)
          (or (netrunner-card-get-value card 'influencelimit) "∞")
          (netrunner-card-get-value card 'baselink)
          (if-let (text (netrunner-card-get-value card 'text)) 
              (format "\n\n%s" (netrunner-substitute-string text)) "")
          (netrunner-card-get-value card 'faction)
          (netrunner-card-get-value card 'setname)))

(defun netrunner-parse-generic (card &optional omit-title)
  "Parse CARD, which isn't an identity.
If OMIT-TITLE, then do not include title in result string."
  (format "%s%s%s%s%s%s%s%s%s\n\n%s%s - %s"
          (if omit-title
              ""
            (format "%s\n" (netrunner-card-get-value card 'title)))
          (netrunner-card-get-value card 'type)
          (let ((subtype (netrunner-card-get-value card 'subtype)))
            (if (and subtype (> (length subtype) 0))
                (format ": %s" subtype)
              ""))
          (if-let (cost (netrunner-card-get-value card 'cost)) 
              (format " • %s$" cost) "")
          (if-let (agendapoints (netrunner-card-get-value card 'agendapoints)) 
              (format " • %s/%s"
                      (netrunner-card-get-value card 'advancementcost)
                      agendapoints)
            "")
          (if-let (trash (netrunner-card-get-value card 'trash)) 
              (format " %sT" trash) "")
          (if-let ((mu (netrunner-card-get-value card 'memoryunits))) 
              (format " %sMU" mu) "") 
          (if-let ((strength (netrunner-card-get-value card 'strength))) 
              (format "\n\n*Strength*: %s" strength) "")
          (if-let (text (netrunner-card-get-value card 'text)) 
              (format "\n\n%s" (netrunner-substitute-string text)) "")
          (let ((factioncost (netrunner-card-get-value card 'factioncost)))
            (if (and factioncost (> factioncost 0))
                (concat (make-string (netrunner-card-get-value card 'factioncost) ?•) " ")
              ""))
          (netrunner-card-get-value card 'faction)
          (netrunner-card-get-value card 'setname)))

(eval-after-load "org"
  '(org-add-link-type
    "netrunner"
    (lambda (handle)
      (popup-tip
       (netrunner-parse (netrunner-filter-first netrunner-cards
                                                `(code . ,handle)))))))


;; Company mode stuff

(defvar netrunner-buffer nil
  "If non nil, this buffer can use `company-netrunner-backend'.")
(make-variable-buffer-local 'netrunner-buffer)
(put 'netrunner-buffer 'safe-local-variable #'booleanp)

;;;###autoload
(defun netrunner-toggle-netrunner-buffer ()
  "Toggles if this is a `netrunner-buffer' or not."
  (interactive)
  (setq netrunner-buffer (not netrunner-buffer))
  (message (if netrunner-buffer
               "Netrunner buffer ON."
             "Netrunner buffer OFF.")))

(defun company-netrunner-backend (command &optional arg &rest ignored)
  "Backend for inserting card names in Android: Netrunner."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-netrunner-backend))
    (prefix (and (eq major-mode 'org-mode)
                 netrunner-buffer
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (mapcar (lambda (card) (netrunner-card-get-value card 'title))
              netrunner-cards)))
    (meta (netrunner-parse
           (netrunner-filter-first netrunner-cards
                                   `(title . ,arg))))
    (annotation
     (let* ((card (netrunner-filter-first netrunner-cards `(title . ,arg)))
            (type (netrunner-card-get-value card 'type))
            (faction (netrunner-card-get-value card 'faction))) 
       (format " %s: %s" faction type)))
    (post-completion 
     (search-backward arg)
     (replace-match
      (format "[[netrunner:%s][%s]]"
              (netrunner-card-get-value
               (netrunner-filter-first netrunner-cards `(title . ,arg))
               'code)
              arg)
      t t))))

(add-to-list 'company-backends 'company-netrunner-backend)


;; Download images
(defvar netrunner-image-dir (expand-file-name "netrunner_images" user-emacs-directory))

(defun netrunner-download-all-images ()
  "Try to download images from all cards from NetrunnerDB into `netrunner-image-dir'."
  (interactive)
  (unless (file-exists-p netrunner-image-dir)
    (make-directory netrunner-image-dir))
  (mapc #'netrunner-download-image netrunner-cards))

(defun netrunner-download-image (card)
  "Download CARD image into `netrunner-image-dir' from NetrunnerDB."
  (let ((link (concat "http://netrunnerdb.com" (netrunner-card-get-value card 'imagesrc)))
        (filename (concat netrunner-image-dir "/img" (netrunner-card-get-value card 'code) ".png")))
    (unless (file-exists-p filename)
      (url-retrieve
       link
       (lambda (status filename buffer)
         ;; Write current buffer to FILENAME
         ;; and update inline images in BUFFER
         (let ((err (plist-get status :error)))
           (if err (error
                    "\"%s\" %s" link
                    (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
         (delete-region
          (point-min)
          (progn
            (re-search-forward "\n\n" nil 'move)
            (point)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region nil nil filename nil nil nil nil)))
       (list
        (expand-file-name filename)
        (current-buffer))
       nil t)
      (sleep-for 0 100))
    filename))

(defun netrunner-image (card)
  "Return an image of CARD if its in `netrunner-image-dir', else nil."
  (let ((filename (expand-file-name
                   (concat "img" (netrunner-card-get-value card 'code) ".png")
                   netrunner-image-dir)))
    (if (file-exists-p filename)
        (create-image filename)
      nil)))


;; Helm stuff

(defun helm-netrunner--parse-candidate (card)
  "Parse CARD into a string suitable for helm-netrunner."
  (let ((title (netrunner-card-get-value card 'title))
        (text (concat
               (if-let (text (netrunner-card-get-value card 'text)) 
                   (replace-regexp-in-string
                    "\n" "/" (netrunner-substitute-string text))
                 "")))
        (faction (concat
                  " "
                  (netrunner-card-get-value card 'faction)
                  (let ((factioncost (netrunner-card-get-value card 'factioncost)))
                    (if (and factioncost (> factioncost 0))
                        (concat ": " (make-string (netrunner-card-get-value card 'factioncost) ?•))
                      ""))))
        (type (concat
               (netrunner-card-get-value card 'type)
               (let ((subtype (netrunner-card-get-value card 'subtype)))
                 (if (and subtype (> (length subtype) 0))
                     (format ": %s" subtype)
                   ""))))
        (values (concat
                 (if-let (decksize (netrunner-card-get-value card 'minimumdecksize))
                     (format "%s/%s"
                             decksize 
                             (or (netrunner-card-get-value card 'influencelimit) "∞"))
                   "")
                 (if-let (link (netrunner-card-get-value card 'baselink))
                     (format " %sL" link)
                   "")
                 (if-let (agendapoints (netrunner-card-get-value card 'agendapoints)) 
                     (format "%s/%s"
                             (netrunner-card-get-value card 'advancementcost)
                             agendapoints)
                   "")
                 (if-let (cost (netrunner-card-get-value card 'cost)) 
                     (format "%s$" cost) "")               
                 (if-let (trash (netrunner-card-get-value card 'trash)) 
                     (format " %sT" trash) "")
                 (if-let ((mu (netrunner-card-get-value card 'memoryunits))) 
                     (format " %sMU" mu) "")               
                 (if-let ((strength (netrunner-card-get-value card 'strength))) 
                     (format " Str: %s" strength) ""))))
    (add-text-properties 0 (length title) '(face default) title)
    (add-text-properties 0 (length values) '(face shadow) values)
    (add-text-properties 0 (length type) '(face (bold shadow)) type)
    (add-text-properties 0 (length text) '(face (italic shadow)) text)
    (cons (concat title "  " type " • " values
                  (when (> (length text) 0) " • ") text
                  faction)
          card)))

(defun helm-netrunner--parse-candidates (cards)
  "Turns CARDS to a list of helm-netrunner candidate strings."
  (mapcar #'helm-netrunner--parse-candidate cards))

(defun helm-netrunner--candidates-card-list (ignored)
  "Inserts an org-mode list of Helm card candidates."
  (cl-loop for cand in (helm-marked-candidates)
           do
           (insert (format "- [[netrunner:%s][%s]]\n"
                           (netrunner-card-get-value cand 'code)
                           (netrunner-card-get-value cand 'title)))))

(defun helm-netrunner--candidates-netrunnerdb (ignored)
  "Browse NetrunnerDB for Helm card candidates."
  (cl-loop for cand in (helm-marked-candidates)
           do
           (browse-url
            (netrunner-card-get-value cand 'url))))

(defun helm-netrunner--candidates-ancur (ignored)
  "Browse Ancur for Helm card candidates."
  (cl-loop for cand in (helm-marked-candidates)
           do
           (browse-url
            (netrunner-card-get-value cand 'ancur_link))))

(defun helm-netrunner--persistent-action (cand)
  "Show Netrunner card text in separate buffer."
  (switch-to-buffer (get-buffer-create " *helm-netrunner persistent*"))
  (fundamental-mode)
  (erase-buffer)
  (when-let (image (netrunner-image cand))
    (insert-image image)
    (insert "\n\n"))
  (insert
   (netrunner-parse cand t))
  (beginning-of-buffer))

(defun helm-source--netrunner (name cards)
  "Create a helm source NAME with Android: Netrunner CARDS."
  (helm-build-sync-source name
    :candidates (helm-netrunner--parse-candidates cards)
    :persistent-action #'helm-netrunner--persistent-action
    :persistent-help "Show card text."
    :follow t
    :action (helm-make-actions
             "Insert" #'helm-netrunner--candidates-card-list
             "NetrunnerDB" #'helm-netrunner--candidates-netrunnerdb
             "Ancur" #'helm-netrunner--candidates-ancur)))

;;;###autoload
(defun helm-netrunner ()
  "Helm for Android: Netrunner cards."
  (interactive)
  (helm
   :sources `(,(helm-source--netrunner "Corp Neutral"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Neutral")
                                                         '(side . "Corp")))
              ,(helm-source--netrunner "Haas-Bioroid"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Haas-Bioroid")))
              ,(helm-source--netrunner "Jinteki"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Jinteki")))
              ,(helm-source--netrunner "NBN"
                                       (netrunner-filter netrunner-cards 
                                                         '(faction . "NBN")))
              ,(helm-source--netrunner "Weyland Consortium"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Weyland Consortium")))
              ,(helm-source--netrunner "Runner Neutral"
                                       (netrunner-filter netrunner-cards
                                                         '(side . "Runner")
                                                         '(faction . "Neutral")))
              ,(helm-source--netrunner "Anarch"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Anarch")))
              ,(helm-source--netrunner "Criminal"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Criminal")))
              ,(helm-source--netrunner "Shaper"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Shaper")))
              ,(helm-source--netrunner "Apex"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Apex")))
              ,(helm-source--netrunner "Sunny Lebeau"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Sunny Lebeau")))
              ,(helm-source--netrunner "Adam"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Adam"))))
   :buffer "*helm netrunner*"))

;;;###autoload
(defun helm-netrunner-corp ()
  "Helm for corp cards in Android: Netrunner."
  (interactive)
  (helm
   :sources `(,(helm-source--netrunner "Neutral"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Neutral")
                                                         '(side . "Corp")))
              ,(helm-source--netrunner "Haas-Bioroid"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Haas-Bioroid")))
              ,(helm-source--netrunner "Jinteki"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Jinteki")))
              ,(helm-source--netrunner "NBN"
                                       (netrunner-filter netrunner-cards 
                                                         '(faction . "NBN")))
              ,(helm-source--netrunner "Weyland Consortium"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Weyland Consortium"))))
   :buffer "*helm netrunner*"))

;;;###autoload
(defun helm-netrunner-runner ()
  "Helm for runner cards in Android: Netrunner."
  (interactive)
  (helm
   :sources `(,(helm-source--netrunner "Runner Neutral"
                                       (netrunner-filter netrunner-cards
                                                         '(side . "Runner")
                                                         '(faction . "Neutral")))
              ,(helm-source--netrunner "Anarch"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Anarch")))
              ,(helm-source--netrunner "Criminal"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Criminal")))
              ,(helm-source--netrunner "Shaper"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Shaper")))
              ,(helm-source--netrunner "Apex"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Apex")))
              ,(helm-source--netrunner "Sunny Lebeau"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Sunny Lebeau")))
              ,(helm-source--netrunner "Adam"
                                       (netrunner-filter netrunner-cards
                                                         '(faction . "Adam"))))
   :buffer "*helm netrunner*"))

(provide 'netrunner)
;;; netrunner.el ends here
