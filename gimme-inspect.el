(defvar gimme-inspect-buffer-name "GIMME - Inspect")
(defvar gimme-inspect-max-length 50)

(defvar gimme-inspect-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    (define-key map (kbd "RET") 'gimme-inspect-change-current-line-prompt)
    (define-key map (kbd "S-<return>") (lambda () (interactive) (gimme-inspect-change-current-line-prompt t)))
    (define-key map (kbd "C-S-<return>")
      'gimme-tagwriter-apply-previous-function-to-all-songs)
    (define-key map (kbd "W") 'gimme-inspect-write-to-xmms2)
    (define-key map (kbd "?") 'gimme-inspect-print-current-value)
    (define-key map (kbd "y") 'gimme-inspect-yank-current-value)
    (define-key map (kbd "S") 'gimme-tagwriter-scan-current)
    (define-key map (kbd "TAB") 'gimme-inspect-next-line)
    (define-key map (kbd "<backtab>") 'gimme-inspect-prev-line)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Inspect mode's keymap")

(defun gimme-inspect-mode ()
  "Displays a playlist"
  (interactive)
  (font-lock-mode t)
  (use-local-map gimme-inspect-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-inspect-mode
        mode-name gimme-inspect-buffer-name)
  (setq-local previous-function nil))

(defun gimme-inspect (plist top-message change-function)
  ""
  (interactive)
  (let* ((strs (mapcar (lambda (x) (format "%s" x)) plist))
         (max (loop for x =  strs then (cddr x) while x
                    maximizing (length (car x)) into i
                    maximizing (length (cadr x)) into j
                    and finally return (list i j)))
         (max (mapcar (lambda (x) (min x gimme-inspect-max-length)) max))
         (total (+ 7 (apply #'+ max))))
    (gimme-on-buffer
     gimme-inspect-buffer-name
     (delete-region (point-min) (point-max))
     (insert (format "%s\n\n   ---\n\n" top-message))
     (setq-local table-first-line (line-at-pos))
     (loop for x = strs then (cddr x) while x doing
           (gimme-inspect-insert-row (car x) (cadr x) (car max) (cadr max)))
     (setq-local initial-data plist)
     (setq-local table-last-line (1- (line-at-pos)))
     (gimme-inspect-mode)
     (switch-to-buffer gimme-inspect-buffer-name))))

(defun gimme-inspect-next-line ()
  (interactive)
  (let ((line (max (1- table-first-line) (line-at-pos))))
    (sane-goto-line (if (<= table-last-line line) table-first-line (1+ line)))))

(defun gimme-inspect-prev-line ()
  (interactive)
  (let* ((line (line-at-pos))
         (line (min (if (or (> line table-last-line) (< line table-first-line))
                        (1- table-first-line) line))))
    (sane-goto-line (if (>= table-first-line line) table-last-line (1- line)))))

(defun gimme-inspect-get-current-value ()
  (when (gimme-inspect-on-table-p)
    (get-text-property (- (line-end-position) 3) 'data)))

(defun gimme-inspect-print-current-value ()
  (interactive)
  (message (gimme-inspect-get-current-value)))

(defun gimme-inspect-yank-current-value ()
  (interactive)
  (let ((val (gimme-inspect-get-current-value)))
    (with-temp-buffer (insert val) (kill-ring-save (point-min) (point-max))
                      (message "Yanked: %s" val))))


(defun gimme-inspect-insert-row (key val key-size val-size)
  (goto-char (point-max))
  (insert (format "| %s | %s |\n"
                  (propertize (string-expanded key key-size) 'data key)
                  (propertize (string-expanded val val-size) 'font-lock-face `(:foreground ,(color-for val)) 'data val))))

(defun gimme-inspect-on-table-p ()
  (and (>= (line-at-pos) table-first-line) (<= (line-at-pos) table-last-line)))

(defun gimme-inspect-change-current-line (new)
  (interactive)
  (when (gimme-inspect-on-table-p)
    (unlocking-buffer
     (save-excursion
       (let* ((beg (line-beginning-position)) (end (line-end-position))
              (old (gimme-inspect-get-current-value))
              (string (split-string (buffer-substring-no-properties beg end) "|"))
              (beg (+ beg 3 (length (nth 1 string)))) (end (- (line-end-position) 2))
              (new-string (propertize (string-expanded new gimme-inspect-max-length)
                                      'font-lock-face `(:foreground ,(color-for new) :weight bold) 'data new)))
         (unless (string= old new)
           (delete-region beg end) (goto-char beg) (insert new-string)
           (gimme-inspect-adjust-table)))))))

(defun gimme-inspect-change-current-line-prompt (&optional reuse)
  (interactive)
  (gimme-inspect-change-current-line
   (read-from-minibuffer "Change to/with? " (if reuse (gimme-inspect-get-current-value) ""))))

(defun gimme-inspect-adjust-table ()
  (interactive)
  (let* ((beg (progn (sane-goto-line table-first-line) (beginning-of-line) (point)))
         (end (progn (sane-goto-line table-last-line) (end-of-line) (point)))
         (maxes (butlast (split-string (buffer-substring beg end) "\n")))
         (all-props (mapcar (lambda (x) (text-properties-at (- (length x) 3) x)) maxes))
         (maxes (mapcar (lambda (x) (length (replace-regexp-in-string " *|$" "" x))) maxes))
         (max (apply #'max maxes)))
    (sane-goto-line table-first-line)
    (dotimes (i (length maxes))
      (let* ((beg (+ (line-beginning-position) (nth i maxes)))
             (props (nth i all-props)))
        (delete-region beg (line-end-position))
        (goto-char (line-end-position))
        (insert (apply #'propertize (format " %s|" (make-string (- max (nth i maxes) ) ? )) props))
        (next-line)))))


(defun gimme-conf ()
  (interactive)
  (gimme-send-message "(conf)\n"))

(defun gimme-print-conf (alist)
  (let ((plist (flatten alist))
        (msg "These are the configuration options of XMMS2")
        (function (lambda (x))))
    (gimme-inspect plist msg function)))

(defun gimme-inspect-write-to-xmms2 ()
  (interactive)
  (save-excursion
    (let* ((beg (progn (sane-goto-line table-first-line) (beginning-of-line) (point)))
           (end (progn (sane-goto-line table-last-line) (end-of-line) (point)))
           (plist (loop for key in (range-to-plists beg end)
                        when key collect (plist-get it 'data)))
           (alist (plist-to-pseudo-alist plist)))
      (gimme-send-message "(conf_save %s)\n"  (prin1-to-string alist)))))


(provide 'gimme-inspect)
