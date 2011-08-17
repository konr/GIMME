
;; A whole new set of functions because autocomplete doesn't go back,
;; ie, won't complete artist:BACH to artist:"Johann Sebastian Bach"

(defvar gimme-autocomplete-buffer-name "*GIMME Completions*")
(defvar gimme-autocomplete-prompt "> ")
(defvar gimme-autocomplete-done-with-autocompletions t)

(defun gimme-autocomplete-set-minibuffer (text)
  (interactive)
  (when text
    (let* ((incomplete (minibuffer-contents))
           (alternatives (gimme-possible-completions incomplete)))
      (kill-region (+ (length gimme-autocomplete-prompt) (point-min)) (point-max))
      (insert text))))

(defun gimme-autocomplete-minibuffer-string ()
  (let* ((incomplete (minibuffer-contents))
         (alternatives (gimme-possible-completions incomplete)))
    (buffer-substring (+ (length gimme-autocomplete-prompt) (point-min)) (point-max))))

(defvar gimme-autocomplete-prompt-map
  (let ((map minibuffer-local-map))
    (define-key map (kbd "TAB") 'gimme-autocomplete-toggle-alternatives-on-minibuffer)
    (define-key map (kbd "<backtab>") (lambda () (interactive) (gimme-autocomplete-toggle-alternatives-on-minibuffer -1)))
    (define-key map (kbd "SPC") (lambda () (interactive) (setq gimme-autocomplete-done-with-autocompletions t) (insert " ")))
    map)
  "FIXME: I must write lots of doc strings :(")

(defun gimme-autocomplete-prompt (prompt)
  (interactive)
  (setq gimme-autocomplete-done-with-autocompletions t)
  (setq gimme-autocomplete-prompt prompt)
  (read-from-minibuffer gimme-autocomplete-prompt nil gimme-autocomplete-prompt-map))

(defun gimme-possible-completions (incomplete)
  "FIXME: Not sure if it would scale for 10k+ entries"
  (let* ((key (replace-regexp-in-string "^\\(.* \\)\?\\([^ :]\+\\)\\(:.*\\)\?$" "\\2" incomplete))
         (value (replace-regexp-in-string ".*:\\(.*\\)$" "\\1" incomplete))
         (key-p (or (string-match key value) (string= "" incomplete)))
         (values (loop for x in gimme-mlib-overview if (equal (car x) key) return (cdadr x)))
         (values (if key-p (mapcar (lambda (x) (format "%s:" (car x)))
                                   (remove-if-not (lambda (x) (string-match (format "%s.*" key) (car x))) gimme-mlib-overview))
                   (remove-if-not (lambda (x) (string-match (format ".*%s.*" (downcase value)) (downcase x))) values)))
         (with-previous (mapcar (lambda (x) (replace-regexp-in-string (format "%s$" (if key-p key value))
                                                                      (if key-p x (format "'%s'" x)) incomplete)) values)))
    with-previous))

(defun gimme-autocomplete-display-completion-buffer (incomplete)
  (let* ((buf (get-buffer-create gimme-autocomplete-buffer-name))
         (standard-output buf)
         (alternatives (gimme-possible-completions incomplete)))
    (with-current-buffer buf
      (unlocking-buffer
       (delete-region (point-min) (point-max))
       (kill-all-local-variables)
       (dolist (alternative alternatives) (insert (format "%s\n" alternative)))
       (completion-setup-function)
       (font-lock-mode 1)
       (setq-local text incomplete)))))

(defun gimme-autocomplete-toggle-alternatives-on-minibuffer (&optional inc)
  (interactive)
  (let ((text (gimme-autocomplete-minibuffer-string)))
    (when (or gimme-autocomplete-done-with-autocompletions (not (get-buffer gimme-autocomplete-buffer-name)))
      (add-hook 'after-change-functions (lambda (&rest a) (setq gimme-autocomplete-done-with-autocompletions t)) nil t)
      (gimme-autocomplete-display-completion-buffer text))
    (display-buffer gimme-autocomplete-buffer-name)
    (gimme-autocomplete-set-minibuffer
     (with-current-buffer gimme-autocomplete-buffer-name
       (let* ((min 4) (max (1- (line-number-at-pos (point-max)))) (inc (or inc 1))
	      (cur (text-property-any (point-min) (point-max) 'face 'highlight))
              (cur (if cur (line-number-at-pos cur) (1- min)))
              (next (+ inc cur)) (next (if (> next max) min (if (< next min) max next))))
         (when (>= max min)
           (sane-goto-line cur)
           (unlocking-buffer
            (remove-text-properties (line-beginning-position) (1+ (line-end-position)) '(face nil))
            (sane-goto-line next)
	    (goto-char (line-beginning-position))
            (put-text-property (line-beginning-position) (1+ (line-end-position)) 'face 'highlight))
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
    (setq gimme-autocomplete-done-with-autocompletions nil)))

(defun gimme-toggle-alternatives-on-minibuffer ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive)
  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (eq this-command last-command)
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
        (with-current-buffer (window-buffer window)
          (if (pos-visible-in-window-p (point-max) window)
              ;; If end is in view, scroll up to the beginning.
              (set-window-start window (point-min) nil)
            ;; Else scroll down one screen.
            (scroll-other-window))
          nil)

      )))

(provide 'gimme-autocomplete)
