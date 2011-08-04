(defun gimme-lyrics-display (plist lyrics)
  (let* ((plist (append '(gimme-buffer-type lyrics) plist))
         (title (plist-get plist 'title)) (source (plist-get plist 'source))
         (title (format "GIMME - Lyrics for %s" title))
         (header (format "<font color=%s>(source: %s)</font><p>" (color-for source) source))
         (formatted (replace-regexp-in-string "\n" "\n  " (format "  %s" lyrics))))
    (gimme-on-buffer
     (gimme-gen-buffer plist)
     (insert header)
     (insert formatted)
     (goto-char (point-min))
     (htmlr-render)
     (gimme-lyrics-mode))))

(defun gimme-lyrics-mode ()
     (use-local-map gimme-filter-map)
     (font-lock-mode t)
     (setq major-mode 'gimme-lyrics-mode mode-name "gimme-lyrics-mode"))

(defvar gimme-lyrics-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    map)
  "Keymap for GIMME's lyrics' view")

(defun gimme-fetch-lyrics ()
  (interactive)
  (let ((plist (text-properties-at (point))))
    (gimme-send-message "(fetch_lyrics %s)\n" (prin1-to-string plist))))


(provide 'gimme-lyrics)

