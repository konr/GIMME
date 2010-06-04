

(defun gimme-focused-play () ;; FIXME: Message "tickle" received
  (interactive)
  (gimme-send-message
   (format "%s\n" (list 'playn (get-text-property (point) 'pos)))))

(defun gimme-focused-delete ()
  (interactive) ;; Not sure if it's too selfish to consider vimpulse users :P
  (setq gimme-last-ids nil)
  (let* ((bounds (cond ((ignore-errors (car (vimpulse-get-bounds))) (vimpulse-get-bounds))
                       ((mark) (cons (min (point) (mark)) (max (point) (mark))))
                       (t (bounds-of-thing-at-point 'line))))
         (min (car bounds))
         (max (or (cdr bounds) (cadr bounds))))
    (loop for pos = min then (next-property-change pos)
          while (and pos (< pos max))
          doing (let ((pos (get-text-property pos 'pos))
                      (id (get-text-property pos 'id)))
                  (gimme-send-message (format "%s\n" (list 'remove pos)))
                  (setq gimme-last-ids (append gimme-last-ids (list (list 'id id 'pos pos)))))))
  (set-mark nil))

(defun gimme-playlist ()
  (interactive)
  (get-buffer-create gimme-buffer-name)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-playlist-mode)
     (clipboard-kill-region 1 (point-max))
     (save-excursion
       (gimme-send-message "(list)\n")))
    (switch-to-buffer (get-buffer gimme-buffer-name)))) ;; FIXME: Quite redundant and ugly

(defun gimme-center ()
  "Center buffer on currently playing song"
  (interactive)
  (with-current-buffer gimme-buffer-name
    (let ((h-beg (text-property-any (point-min) (point-max) 'face 'highlight)))
      (goto-char h-beg))))

(defun gimme-toggle-view ()
  ;; FIXME: check perfomance on large playlists, but I think I'll change it avoid reloading the playlist
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  (gimme-playlist))

(defun gimme-paste-deleted (undo)
  (interactive)
  (ignore-errors
    (if gimme-last-ids
        (let ((pos (get-text-property (point) 'pos)))
          (dolist (plist gimme-last-ids)
            (setq pos (if undo (getf plist 'pos) (1+ pos)))
            ;; (next-line) FIXME: This won't work multiple times
            (gimme-send-message (format "%s\n" (list 'gimme (getf plist 'id) pos)))))
      (message "No songs on the clipboard!")))
  (setq gimme-last-ids nil)
  (message (if undo "Undo!" "Paste!")))






(provide 'gimme-playlist)
