(require 'htmlr)

(defun gimme-lyrics-display (plist lyrics)
  (let* ((plist (append '(gimme-buffer-type lyrics) plist))
         (title (plist-get plist 'title)) (source (plist-get plist 'source))
         (title (format "GIMME - Lyrics for %s" title))
         (header (format "(source: %s)\n\n" source))
         (formatted (replace-regexp-in-string "" "" lyrics)))
    (gimme-on-buffer
     (gimme-gen-buffer plist)
     (insert formatted)
     (goto-char (point-min)) (htmlr-render)
     ;; FIXME: improve the rendering lib instead of inserting formatted text
     (goto-char (point-min)) (insert (propertize header 'font-lock-face `(:foreground ,(color-for source))))
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

