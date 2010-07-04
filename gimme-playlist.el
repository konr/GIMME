(defvar gimme-playlist-header "GIMME - Playlist view")
(defvar gimme-playlist-mode-functions
  '(gimme-set-playing gimme-set-playing gimme-update-playlist
                      gimme-insert-song gimme-set-title message))

(defvar gimme-playlist-formats '("%artist > %title"
                                 "%title"
                                 "%artist > %album > %title"))

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
  (let* ((last (car kill-ring))
         (pos (get-text-property (point) 'pos))
         (ids (loop for pos = 0 then (next-property-change pos last)
                    while pos collecting (get-text-property pos 'id last))))
    (dolist (id ids) 
      (setq pos (+ 1 pos))
      (message (format "%s\n" (list 'insert id pos))))
    (message "Paste!")))

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
      (gimme-send-message (format "%s\n" (list 'remove (car items))))) ;; FIXME: item -> car items
    (message (format "%s" items))))

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "S") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map [remap kill-line] 'gimme-focused-delete)
    (define-key map (kbd "d") 'kill-line)
    (define-key map [remap kill-line] 'gimme-focused-delete)
    (define-key map (kbd "p") '(lambda () (interactive) (gimme-paste-deleted nil)))
    (define-key map (kbd "u") '(lambda () (interactive) (gimme-paste-deleted t)))
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





(provide 'gimme-playlist)
