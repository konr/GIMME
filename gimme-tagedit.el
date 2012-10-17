
(require 'ecoli)

(defvar gimme-tagedit-buffer-name "GIMME - Tagedit")
(defvar gimme-tagedit-largecol-width 60)
(defvar gimme-tagedit-smallcol-width 10)
(defvar gimme-tagedit-tags '(artist album title genre url))
(defvar gimme-tagedit-last-used nil)

(defvar gimme-tagedit-map
  (let ((map (ecoli-gen-basic-map)))
    (define-key map (kbd "j")          'gimme-tagedit-next)
    (define-key map (kbd "k")          'gimme-tagedit-previous)
    (define-key map (kbd "l")          'gimme-tagedit-goto-next-field)
    (define-key map (kbd "h")          'gimme-tagedit-goto-prev-field)
    (define-key map (kbd "1")          'gimme-tagedit-show-1)
    (define-key map (kbd "2")          'gimme-tagedit-show-2)
    (define-key map (kbd "3")          'gimme-tagedit-show-3)
    (define-key map (kbd "4")          'gimme-tagedit-show-4)
    (define-key map (kbd "5")          'gimme-tagedit-show-5)
    (define-key map (kbd "s")          'gimme-tagedit-scan)
    (define-key map (kbd "S")          'gimme-tagedit-scan-range)
    (define-key map (kbd "TAB")        'gimme-tagedit-goto-next-field)
    (define-key map (kbd "<backtab>")  'gimme-tagedit-goto-prev-field)
    (define-key map (kbd "<return>")   'gimme-tagedit-set-current-field)
    (define-key map (kbd "S-<return>") 'gimme-tagedit-set-current-field-to-last)
    map))

(defun gimme-tagedit-gen-format-lists ()
  "It's complicated to explain. Try it on IELM!"
  (let* ((fancy (->> gimme-tagedit-tags (mapcar (lambda (x) (->> x symbol-name (format "%%%s") intern)))))
         (format-string (->> fancy (mapcar (lambda (x) "%s ")) (apply 'concat))))
    (loop for i from 0 upto (1- (length fancy)) collecting
          `(apply #'propertize
                  (format "%s %s %s %s %s"
                          ,@(loop for j from 0 upto (1- (length fancy)) if (= i j) collect
                                  `(propertize (string-expanded ,(nth i fancy) gimme-tagedit-largecol-width)
                                               'font-lock-face `(:foreground ,(color-for ,(nth i fancy))))
                                  if (not (= i j)) collect
                                  `(propertize (string-expanded ,(nth j fancy)  gimme-tagedit-smallcol-width)
                                               'font-lock-face `(:foreground ,(color-for ,(nth j fancy))))))
                  (plist-del plist 'font-lock-face)))))

(defun gimme-update-header ()
  (let ((header (funcall ecoli-format-function '(artist "Artist" album "Album" title "Title" genre "Genre" url "URL"))))
    (goto-char (point-min))
    (when ecoli-min (delete-region (point-min) ecoli-min))
    (insert header) (insert ecoli-separator)
    (insert (make-string (length header) ?-)) (insert ecoli-separator)))

(defun gimme-tagedit-mode ()
  (let ((buffer (current-buffer)))
    (gimme-update-header)
    (setq-local ecoli-min (point))
    (use-local-map gimme-tagedit-map)
    (dolist (datum data) (ecoli-append buffer datum))))

(defun gimme-tagedit ()
  (interactive)
  (let* ((plists xoxotinha ;(ecoli-plists-on-region)
                 )
         (plists (mapi1000 (lambda (i x) (plist-put x 'pos i)) plists))
         (plist `(buffer-name ,gimme-tagedit-buffer-name data ,plists
                              ecoli-formats ,(gimme-tagedit-gen-format-lists)
                              ecoli-before-update-function gimme-update-header))
         (setup-plist `(ecoli-separator "\n"
                                        ecoli-format-function gimme-string))
         (function #'gimme-tagedit-mode)
         (buffer (ecoli-gen-buffer plist setup-plist function)))))

(defun gimme-tagedit-fields-position ()
  (let* ((both (->> ecoli-formats caddar cddr (mapcar (lambda (x) (-> x cdadr)))
                    (mapcar (lambda (x) (list (car x) (eval (cadr x))))) transpose))
         (pos (cadr both)) (names (mapcar (lambda (x) (-> x symbol-name (substring 1) intern)) (car both))))
    (->> (loop for i from 0 upto (1- (length pos)) collecting
               (loop for j from 0 upto i summing (nth j pos)))
         butlast (map 'list #'+ '(1 2 3 4 5)) (cons 0) (list names))))

(defun gimme-tagedit-show-1 () (interactive) (gimme-tagedit-show-field 0))
(defun gimme-tagedit-show-2 () (interactive) (gimme-tagedit-show-field 1))
(defun gimme-tagedit-show-3 () (interactive) (gimme-tagedit-show-field 2))
(defun gimme-tagedit-show-4 () (interactive) (gimme-tagedit-show-field 3))
(defun gimme-tagedit-show-5 () (interactive) (gimme-tagedit-show-field 4))


(defun gimme-tagedit-show-field (goal)
  (interactive)
  (let* ((fields (cadr (gimme-tagedit-fields-position))) (len (length fields))
         (delta (map 'list #'- (cdr fields) (butlast fields)))
         (min (apply #'min delta))
         (curr (loop for i in delta and j = 0 then (1+ j)
                     if (> i min) return j
                     finally return (length delta)))
         (pos (- (point) (line-beginning-position) -1))
         (goal♥ (-> goal (- 1) (+ len) (mod len))))
    (loop for i = curr then (mod (1+ i) len)
          while (not (= i goal♥))
          doing (setq-local ecoli-formats (append (cdr ecoli-formats) (list (car ecoli-formats))))
          finally do (ecoli-toggle-view))
    (gimme-tagedit-goto-field goal)))

(defun gimme-tagedit-goto-next-field ()
  (interactive)
  (let ((poss (cadr (gimme-tagedit-fields-position)))
        (pos (- (point) (line-beginning-position))))
    (goto-char (loop for i in poss if (> i pos) return (+ i (line-beginning-position))
                     finally return (1+ (line-end-position))))
    (when (= (point-max) (point)) (goto-char ecoli-min))))

(defun gimme-tagedit-goto-prev-field ()
  (interactive)
  (when (>= (1+ ecoli-min) (point)) (goto-char (point-max)))
  (let ((poss (-> (gimme-tagedit-fields-position) cadr reverse))
        (pos (- (point) (line-beginning-position))))
    (goto-char (loop for i in poss if (< i pos) return (+ i (line-beginning-position))
                     finally return (- (line-beginning-position) (- (car poss) (cadr poss)))))))

(defun gimme-tagedit-goto-field (n)
  (interactive)
  (let* ((pos (line-beginning-position)))
    (->> (gimme-tagedit-fields-position) cadr (mapcar (lambda (x) (+ x pos)))
         (nth n) goto-char)))


(defun gimme-tagedit-get-property (property)
  (get-text-property (point) property))

(defun gimme-tagedit-current-property (&optional fields)
  (let ((both (mapcar #'reverse (or fields (gimme-tagedit-fields-position))))
        (pos (- (point) (line-beginning-position))))
    (loop for i in (cadr both) and j = 0 then (1+ j)
          when (>= pos i) return (nth j (car both)))))

(defun gimme-tagedit-current-position (&optional fields)
  (let ((both (mapcar #'reverse (or fields (gimme-tagedit-fields-position))))
        (pos (- (point) (line-beginning-position))))
    (loop for i in (cadr both) and j = 0 then (1+ j)
          when (>= pos i) return (-> both cadr length (- j) (- 1)))))


(defun message-safe (string)
  (message (replace-regexp-in-string "%" "%%" string)))

(defun gimme-tagedit-set-current-field (&optional new)
  (interactive)
  (when (get-text-property (point) 'pos)
    (let* ((pos (gimme-tagedit-current-position))
           (field (gimme-tagedit-current-property))
           (value (gimme-tagedit-get-property field))
           (new (or new (read-from-minibuffer (format "%s -> " field) value))))
      (-> (point) text-properties-at (plist-put field new) ecoli-update-item)
      (setq-local gimme-tagedit-last-used new)
      (gimme-tagedit-goto-field pos))))

(defun gimme-tagedit-set-current-field-to-last ()
  (interactive)
  (gimme-tagedit-set-current-field gimme-tagedit-last-used))

(defun gimme-tagedit-previous ()
  (interactive)
  (previous-line))

(defun gimme-tagedit-next ()
  (interactive)
  (next-line))

(defvar gimme-tagedit-scan-map
  (let ((map minibuffer-local-map))
    (define-key map (kbd "TAB") 'gimme-tagedit-recalculate-tags)
    map)
  "Tagedit's scan keymap")

(defun gimme-tagedit-recalculate-tags ()
  "Recalculates the tags, given the pattern in the minibuffer."
  (interactive)
  (let* ((regexp (minibuffer-contents))
         (url (with-current-buffer gimme-tagedit-buffer-name url))
         (plist (gimme-tagedit-scan-regexp url regexp))
         (formatted-string
          (loop for x = plist then (cddr x) while x
                collecting (format "$%s = \"%s\"; " (car x) (cadr x))
                into strings and finally return (apply #'concat strings))))
    (message-safe formatted-string)))

(defun gimme-tagedit-scan-regexp (string regexp)
  "Scans all fields, given the regexp"
  (let* ((new-regexp (replace-regexp-in-string "$[a-zA-Z]\+" "\\\\\(\.\*\\\\\)"
                                               regexp))
         (symbols (mapcar (lambda (x) (intern (substring (car x) 1)))
                          (regexp-all-matches regexp "\\($[a-zA-Z]\+\\)")))
         (matches (car (regexp-all-matches string new-regexp))))
    (loop for i = 0 then (1+ i) while (nth i matches)
          collecting (list (nth i symbols) (nth i matches)) into alist
          and finally return (mapcan (lambda (x) x) alist))))

(defun gimme-tagedit-scan (&optional try-previous)
  "Scans fields from the URL."
  (interactive)
  (let* ((prev (text-properties-at (point)))
         (url (decode-percent-encoding (plist-get prev 'url)))
         (_ (setq-local url url))
         (regexp (read-from-minibuffer (format "%s> " url) "" gimme-tagedit-scan-map))
         (new (gimme-tagedit-scan-regexp url regexp)))
    (ecoli-update-item (plist-add new prev))))

(defun gimme-tagedit-scan-range (&optional try-previous)
  "Scans fields from the URL."
  (interactive)
  (let* ((prev (text-properties-at (point)))
         (url (decode-percent-encoding (plist-get prev 'url)))
         (_ (setq-local url url))
         (regexp (read-from-minibuffer (format "%s> " url) "" gimme-tagedit-scan-map))
         (new (gimme-tagedit-scan-regexp url regexp)))
    (loop for p in (ecoli-plists-on-region)
          doing (-> p (plist-get 'url) (gimme-tagedit-scan-regexp regexp)
                    (plist-add p) ecoli-update-item))))

(provide 'gimme-tagedit)
