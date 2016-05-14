(require 'json)
(require 'popup)
(require 'cl-lib)
(require 'company)
(require 'subr-x)

(defun netrunner-api-get (tail)
  "Get json data from NetrunnerDB API, with TAIL as argument.
An example for TAIL is \"cards/\" in order to get all cards."
  (with-current-buffer
      (url-retrieve-synchronously (format "http://netrunnerdb.com/api/%s"
                                          tail))
    (goto-char url-http-end-of-headers)
    (json-read)))

(defvar netrunner-cards (netrunner-api-get "cards/"))

(defun netrunner-card-get-value (card property)
  (cdr (assoc property card)))

(defun netrunner-card-property-p (card property value)
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

(defun netrunner-menu (cards)
  (popup-menu*
   (mapcar (lambda (card)
             (popup-make-item
              (netrunner-card-get-value card 'title)
              :document (netrunner-parse card t)))
           cards)))

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
          (if-let (subtype (netrunner-card-get-value card 'subtype))
              (when (> (length subtype) 0)
                (format ": %s" subtype))
            "")
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
          (if-let ((factioncost (netrunner-card-get-value card 'factioncost)))
              (when (> factioncost 0)
                (concat (make-string (netrunner-card-get-value card 'factioncost) ?•) " "))
            "")
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

(defconst netrunner-card-names
  (mapcar (lambda (x)
            (netrunner-card-get-value x 'title))
          netrunner-cards)
  "A list of the names of all cards in Android: Netrunner.")

(defvar netrunner-buffer nil
  "If non nil, this buffer can use `company-netrunner-backend'.")
(make-variable-buffer-local 'netrunner-buffer)

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
      netrunner-card-names))
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
      t))))

(add-to-list 'company-backends 'company-netrunner-backend)
