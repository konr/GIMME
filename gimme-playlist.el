(defvar gimme-playlist-header "GIMME - Playlist view")
(defvar gimme-playlist-mode-functions
  '(gimme-set-playing gimme-update-playlist
                      gimme-insert-song gimme-set-title message
                      gimme-update-tags gimme-update-playtime))


(defun gimme-playlist ()
  "Sets up the buffer"
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-playlist-mode)
     (gimme-set-title gimme-playlist-header)
     (clipboard-kill-region 1 (point-max))
     (gimme-send-message "(list %s)\n" gimme-session))
    (setq gimme-current-mode 'playlist)
    (switch-to-buffer (get-buffer gimme-buffer-name))))

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'gimme-filter)
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "T") 'gimme-update-tags-prompt)
    (define-key map (kbd "H") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map [remap kill-line] '(lambda () (interactive) (gimme-focused-delete t)))
    (define-key map (kbd "d") 'kill-line)
    (define-key map [remap yank] (lambda () (interactive) (gimme-paste-deleted nil)))
    (define-key map (kbd "p") 'yank)
    (define-key map (kbd "S") 'gimme-sort)
    (define-key map (kbd "s") 'gimme-toggle-sort)
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
    (define-key map (kbd "?") 'gimme-focused-url)
    (define-key map (kbd "*") 'gimme-toggle-star)
    (define-key map (kbd "[") (lambda () (interactive) (gimme-seek -1000  t)))
    (define-key map (kbd "{") (lambda () (interactive) (gimme-seek -10000 t)))
    (define-key map (kbd "]") (lambda () (interactive) (gimme-seek  1000  t)))
    (define-key map (kbd "}") (lambda () (interactive) (gimme-seek  10000 t)))
    (define-key map (kbd "g") 'gimme-goto-pos)
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-set-playing (pos)
  "Highlights the currently played song"
  (when (get-buffer gimme-buffer-name)
    (with-current-buffer gimme-buffer-name
      (unlocking-buffer
       (let* ((h-beg t) (h-end t))
         (while h-beg
           (setq h-beg (text-property-any (point-min) (point-max) 'face 'highlight))
           (setq h-end (or (next-property-change (or h-beg (point-min))) (point-max)))
           (when h-beg (remove-text-properties h-beg h-end '(face nil))
                 (message (format "%d" h-beg)))))
       (let* ((beg (text-property-any (point-min) (point-max) 'pos pos))
              (end (next-property-change (or beg (point-min)))))
         (when beg (put-text-property beg (or end (point-max)) 'face 'highlight)))))))




(defun gimme-update-tags (plist-b)
  ""
  (with-current-buffer gimme-buffer-name
    (let* ((id (getf plist-b 'id))
           (pos-list (range-to-plists (point-min) (point-max)))
           (pos-list (remove-if-not (lambda (n) (equal id (getf n 'id))) pos-list))
           (pos-list (mapcar (lambda (n) (getf n 'pos)) pos-list)))
      (dolist (pos pos-list)
        (let* ((beg (text-property-any (point-min) (point-max) 'pos pos))
               (end (or (next-property-change beg) (point-max))))
          (unless (plist-subset plist-b (text-properties-at beg))
            (unlocking-buffer
             (plist-put plist-b 'pos pos)
             (plist-put plist-b 'font-lock-face nil)
                                        ; FIXME: For some reason,  when a duplicated
                                        ; song is starred or changed, the plist will
                                        ; contain font-lock-face, which can't be 
                                        ; evaluated. The previous line """Fixes""" it,
                                        ; but I guess that this bug will eventually 
                                        ; show up in other parts of the code. 
                                        ; 
                                        ; FIXME: Double check the macros and rewrite
                                        ; the code in a more functional style
            (kill-region beg end)
            (save-excursion
              (goto-char beg)
              (insert (gimme-string plist-b))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-sort ()
  "Sorts the current playlist"
  (interactive)
  (gimme-send-message "(sort %s)\n" (car gimme-sort-criteria)))

(defun gimme-toggle-sort ()
  "Cycle through the sort criteria"
  (interactive)
  (setq gimme-sort-criteria (append (cdr gimme-sort-criteria)
                                    (list (car gimme-sort-criteria))))
  (message (format "Sorting now by %s%s" (caar gimme-sort-criteria)
                   (apply #'concat (mapcar (lambda (n) (format " > %s" n))
                                           (cdar gimme-sort-criteria))))))

(defun gimme-focused-play ()
  "Plays the currently focused song"
  (interactive)
  (let ((pos (get-text-property (point) 'pos)))
    (when pos (gimme-send-message "(playn %s)\n" pos))))

(defun gimme-paste-deleted (undo)
  (interactive)
  (let* ((last (car kill-ring))
         (pos (get-text-property (point) 'pos))
         (ids (loop for pos = 0 then (next-property-change pos last)
                    while pos collecting (get-text-property pos 'id last))))
    (when (and pos ids)
      (dolist (id ids)
        (setq pos (+ 1 pos))
        (gimme-send-message "(insert %s %s)\n" id pos)))))

(defun gimme-continue-deleting ()
  ""
  (when gimme-delete-stack
    (gimme-send-message "(remove %s)\n" (car gimme-delete-stack))
    (setq gimme-delete-stack (cdr gimme-delete-stack))))

(defun gimme-focused-delete (delete-p)
  "Deletes the currently focused song."
  ;; FIXME: When you delete the currently playing song, the playlist state gets inconsistent on the server
  (interactive)
  (if (use-region-p)
      (let* ((min (min (point) (mark)))
             (max (max (point) (mark)))
             (min (progn (goto-char min) (line-beginning-position)))
             (max (+ 1 (progn (goto-char max) (line-end-position)))))
        (kill-ring-save min max))
    (kill-ring-save (line-beginning-position) (line-end-position)))
  (let ((items (loop for pos = 0 then (next-property-change pos (car kill-ring))
                     while pos collecting (get-text-property pos 'pos (car kill-ring)))))
    (unless (null (car items))
      (gimme-send-message "(remove %s)\n" (car gimme-delete-stack))
      (gimme-continue-deleting))))


(defun gimme-focused-url ()
  "Asks for the song's current URL."
  (interactive)
  (gimme-send-message "(url %s)\n" (get-text-property (point) 'id)))

(defun gimme-center ()
  "Centers buffer on currently playing song"
  (interactive)
  (with-current-buffer gimme-buffer-name
    (let ((h-beg (text-property-any (point-min) (point-max) 'face 'highlight)))
      (if h-beg (goto-char h-beg) (message "Not on the playlist.")))))

(defun gimme-seek (time relativep)
  "Jumps to a different position. Time must be given in miliseconds."
  (interactive)
  (gimme-send-message "(%s \"%d\")\n" (if relativep "seek_rel" "seek") time))

(defun gimme-goto-pos ()
  "Prompts for a position, to be given in percentage or [[hours:]minutes:]seconds"
  (interactive)
  (let* ((input (read-from-minibuffer "Goto: "))
         (time (cond ((string-match "^[0-9:]\+$" input)
                      (reduce (lambda (x y) (+ (* 60 x) y))
                              (mapcar #'string-to-int (split-string input ":"))))
                     ((string-match "^[0-9]\+%$" input)
                      (/ (* (gimme-current-duration)
                            (string-to-int (substring input 0 -1)))
                         100000))
                     (t nil))))
    (if time (gimme-seek (* 1000 time) nil) (message "Unrecognized format!"))))

(defun gimme-toggle-star ()
  "Adds/removes a star property"
  (interactive)
  (let* ((plist (text-properties-at (point)))
         (starredp (getf plist 'starred))
         (string (if (string= "t" starredp) "nil" "t"))
         (plist (append plist `(starred ,string)))
         (alist (remove-if (lambda (n) (member (car n) '(face font-lock-face)))
                           (plist-to-pseudo-alist plist)))
         (alist (mapcar (lambda (n) `(,(car n) ,(if (stringp (cadr n))
                                                    (format "\"%s\"" (decode-coding-string (cadr n) 'utf-8))
                                                  (cadr n)))) alist)))
    (gimme-send-message "(update_tags %s)\n" alist)))

(defun gimme-update-tags-prompt ()
  "Prompts and updates title/artist and album"
  (interactive)
  (let* ((alist (remove-if (lambda (n) (member (car n) '(face font-lock-face)))
                           (plist-to-pseudo-alist (text-properties-at (point)))))
         (alist (mapcar (lambda (n) (if (stringp (cadr n))
                                        (list (car n) (decode-coding-string (cadr n) 'utf-8))
                                      n)) alist))
         (alist (mapcar (lambda (n) (if (member (car n) '(title album artist))
                                        (list (car n) (format "\"%s\""
                                                              (read-from-minibuffer
                                                               (format "%s? " (car n))
                                                               (format "%s" (cadr n)))))
                                      n)) ; FIXME: Assuming no whitespace. Allow only number/strings!
                        alist)))
    (gimme-send-message "(update_tags %s)\n" alist)))



(provide 'gimme-playlist)
