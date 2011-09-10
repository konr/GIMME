;;; gimme-autocomplete.el --- GIMME Interesting Music on My Emacs

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: XMMS2, mp3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; A whole new set of autocomplete functions, necessary because
;; vanilla autocompletion doesn't delete/correct characters, ie,
;; 'artist:BACH' is not completable to 'artist:"Johann Sebastian
;; Bach"'

;;; Code

(defvar gimme-autocomplete-prompt-map
  (let ((map (copy-tree minibuffer-local-map)))
    (define-key map (kbd "TAB") 'gimme-autocomplete-toggle-alternatives-on-minibuffer)
    (define-key map (kbd "<backtab>") (lambda () (interactive) (gimme-autocomplete-toggle-alternatives-on-minibuffer -1)))
    (define-key map (kbd "SPC") (lambda () (interactive) (setq gimme-autocomplete-done-with-autocompletions t) (insert " ")))
    map)
  "GIMME-autocomplete's keymap")

(defun gimme-autocomplete-prompt (prompt collection)
  "Helps you build a valid collection using the content of cached collections, which might be outdated."
  (interactive)
  (setq gimme-autocomplete-done-with-autocompletions t)
  (setq gimme-autocomplete-current-coll collection)
  (setq gimme-autocomplete-prompt prompt)
  (read-from-minibuffer gimme-autocomplete-prompt nil gimme-autocomplete-prompt-map))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-autocomplete-set-minibuffer (text)
  "Sets the text of the current buffer, supposedly the minibuffer to the string given. This function is necessary because one cannot delete the prompt characters."
  (interactive)
  (when text
    (let* ((incomplete (minibuffer-contents))
           (alternatives (gimme-possible-completions incomplete)))
      (kill-region (+ (length gimme-autocomplete-prompt) (point-min)) (point-max))
      (insert text))))

(defun gimme-autocomplete-minibuffer-string ()
  "Gets the text on minibuffer minus the prompt."
  (let* ((incomplete (minibuffer-contents))
         (alternatives (gimme-possible-completions incomplete)))
    (buffer-substring (+ (length gimme-autocomplete-prompt) (point-min)) (point-max))))

(defun gimme-possible-completions (incomplete)
  "Given a string, returns the possible completions. 'ar' would get us ('artist:' 'starred:') and 'artist:BACH' would get ('artist: Johann Sebastian Bach')."
  (let* ((key (replace-regexp-in-string "^\\(.* \\)\?\\([^ :]\+\\)\\(:.*\\)\?$" "\\2" incomplete))
         (value (replace-regexp-in-string ".*:\\(.*\\)$" "\\1" incomplete))
         (key-p (or (string-match key value) (string= "" incomplete)))
         (completions (or (plist-get-with-equal gimme-mlib-cache-plist gimme-autocomplete-current-coll) gimme-mlib-cache-global))
         (values (loop for x in completions if (equal (car x) key) return (cdadr x)))
         (values (if key-p (mapcar (lambda (x) (format "%s:" (car x)))
                                   (remove-if-not (lambda (x) (string-match (format "%s.*" key) (car x))) completions))
                   (remove-if-not (lambda (x) (string-match (format ".*%s.*" (downcase value)) (downcase x))) values)))
         (with-previous (mapcar (lambda (x) (replace-regexp-in-string (format "%s$" (if key-p key value))
                                                                 (if key-p x (format "'%s'" (replace-regexp-in-string "'" "\\\\\\\\'" x))) incomplete)) values)))
    with-previous))

(defun gimme-autocomplete-display-completion-buffer (incomplete)
  "Generates a buffer with the possible completions of an incomplete input."
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
  "Alternates between the various possible completions of the current input in the minibuffer."
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

(provide 'gimme-autocomplete)
;;; gimme-autocomplete.el ends here
