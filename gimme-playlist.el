(defvar gimme-playlist-header "GIMME - Playlist view")
(defvar gimme-playlist-mode-functions
  '(gimme-set-playing gimme-set-playing gimme-update-playlist
                      gimme-insert-song gimme-set-title message
                      gimme-update-tags))


(defun gimme-focused-play () ;; FIXME: Message "tickle" received
  (interactive)
  (gimme-send-message
   (format "%s\n" (list 'playn (get-text-property (point) 'pos)))))

(defun gimme-focused-delete ()
  (interactive)
  (if (use-region-p)
      (let* ((min (min (point) (mark)))
             (max (max (point) (mark)))
             (min (progn (goto-char min) (line-beginning-position)))
             (max (+ 1 (progn (goto-char max) (line-end-position)))))
        (kill-ring-save min max))
    (kill-ring-save (line-beginning-position) (line-end-position)))
  (let ((items (let ((last (car kill-ring)))
                 (loop for pos = 0 then (next-property-change pos last)
                       while pos collecting (get-text-property pos 'pos last)))))
    (dolist (item items)
      (gimme-send-message (format "%s\n" (list 'remove (car items))))))) ;; FIXME: item -> car items


(defun gimme-playlist ()
  (interactive)
  (get-buffer-create gimme-buffer-name)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-playlist-mode)
     (clipboard-kill-region 1 (point-max))
     (gimme-set-title gimme-playlist-header)
     (save-excursion
       (gimme-send-message "(list)\n")))
    (setq gimme-current-mode 'playlist)
    (switch-to-buffer (get-buffer gimme-buffer-name)))) ;; FIXME: Quite redundant and ugly

(defun gimme-center ()
  "Center buffer on currently playing song"
  (interactive)
  (with-current-buffer gimme-buffer-name
    (let ((h-beg (text-property-any (point-min) (point-max) 'face 'highlight)))
      (goto-char h-beg))))

(defun gimme-update-tags (plist)
  (interactive)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (let* ((beg (text-property-any (point-min) (point-max) 'id (getf plist 'id)))
            (end (next-property-change beg))
            (pos (get-text-property (point) 'pos)))
       (unless (plist-subset plist (text-properties-at beg))
         (kill-region beg end)
         (save-excursion
           (goto-char beg)
           (insert (gimme-string (plist-put plist 'pos pos)))))))))


(defun gimme-paste-deleted (undo)
  (interactive)
  (let* ((last (car kill-ring))
         (pos (get-text-property (point) 'pos))
         (ids (loop for pos = 0 then (next-property-change pos last)
                    while pos collecting (get-text-property pos 'id last))))
    (dolist (id ids)
      (setq pos (+ 1 pos))
      (gimme-send-message (format "%s\n" (list 'insert id pos))))
    (message "Paste!")))

(defun gimme-focused-delete (delete-p)
  (interactive)
  (if (use-region-p)
      (let* ((min (min (point) (mark)))
             (max (max (point) (mark)))
             (min (progn (goto-char min) (line-beginning-position)))
             (max (+ 1 (progn (goto-char max) (line-end-position)))))
        (kill-ring-save min max))
    (kill-ring-save (line-beginning-position) (line-end-position)))
  (let ((items (let ((last (car kill-ring)))
                 (loop for pos = 0 then (next-property-change pos last)
                       while pos collecting (get-text-property pos 'pos last)))))
    (dolist (item items)
      (gimme-send-message (format "%s\n" (list 'remove (car items))))) ;; FIXME: item -> car items
    (message (format "%s" items))))

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'gimme-filter)
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "T") 'gimme-update-tags-prompt)
    (define-key map (kbd "S") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map [remap kill-line] '(lambda () (interactive) (gimme-focused-delete t)))
    (define-key map (kbd "d") 'kill-line)
    (define-key map [remap yank] (lambda () (interactive) (gimme-paste-deleted nil)))
    (define-key map (kbd "p") 'yank)
    (define-key map (kbd "s") 'gimme-stop)
    (define-key map (kbd "SPC") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "l") 'gimme-center)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") 'gimme-inc_vol) ;; FIXME: Better names, please!
    (define-key map (kbd "+") 'gimme-inc_vol)
    (define-key map (kbd "-") 'gimme-dec_vol)
    map))


(defun gimme-update-tags-prompt ()
  (interactive)
  (let ((alist (mapcar (lambda (n) (if (member (car n) '(id face pos)) n
                                (list (car n) (format "\"%s\""
                                                      (read-from-minibuffer
                                                       (format "%s? " (car n))
                                                       (format "%s" (cadr n)))))))
                       (plist-to-pseudo-alist (text-properties-at (point))))))
    (gimme-send-message (format "%s\n" (list 'update_tags alist)))))



(provide 'gimme-playlist)
