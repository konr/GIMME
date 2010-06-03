(defvar gimme-process)
(defvar gimme-executable "gimme.rb")
(defvar gimme-last-ids nil)
(defvar gimme-buffer-name "GIMME")
(defvar gimme-playlist-header "GIMME - Playlist view")
(defvar gimme-filter-remainder "")
(defvar gimme-playlist-formats '("%artist > %title"
                                 "%title"
                                 "%artist > %album > %title"))


(defun gimme-reset ()
  (setq gimme-last-ids nil)
  (setq gimme-filter-remainder ""))

(defun alpha-blend (c1 c2 a)
  "The resulting color of merging the c1 with alpha a on a background of color c2"
  (let* ((colors (mapcar (lambda (c) (list (substring c 1 3)
                                      (substring c 3 5)
                                      (substring c 5 7)))
                         (list c1 c2)))
         (colors (mapcar (lambda (c) (mapcar (lambda (e) (string-to-number e 16)) c))
                         colors))
         (color (map 'list (lambda (c1 c2) (format "%.2x" (+ (* (- 1 a) c1)
                                                        (* a c2))))
                     (nth 0 colors) (nth 1 colors))))
    (apply 'concat "#" color)))

(defmacro unlocking-buffer (&rest body)
  `(progn (toggle-read-only nil)
          ,@body
          (toggle-read-only t)))

(defun gimme-center ()
  "Center buffer on currently playing song"
  (interactive)
  (with-current-buffer gimme-buffer-name
    (let ((h-beg (text-property-any (point-min) (point-max) 'face 'highlight)))
      (goto-char h-beg))))

(defun gimme-set-playing (pos)
  "Highlights the currently played song"
  ;; FIXME can assume again a single highlighted track, so it's better to revert to a simpler version of the function
  (when (get-buffer gimme-buffer-name)
    (with-current-buffer gimme-buffer-name
      (unlocking-buffer
       (let* ((h-beg t) (h-end t))
         (while h-beg
           (setq h-beg (text-property-any (point-min) (point-max) 'face 'highlight))
           (setq h-end (next-property-change (or h-beg (point-min))))
           (when h-beg (remove-text-properties h-beg h-end '(face nil)))))
       (let* ((beg (text-property-any (point-min) (point-max) 'pos pos))
              (end (next-property-change (or beg (point-min)))))
         (when beg (put-text-property beg end 'face 'highlight)))))))

;; FIXME: Filter somehow the sexps allowing some only a preselection of them
(defun eval-all-sexps (s)
  (let ((s (concat gimme-filter-remainder s)))
    (setq gimme-filter-remainder
          (loop for x = (ignore-errors (read-from-string s))
                then (ignore-errors (read-from-string (substring s position)))
                while x
                summing (or (cdr x) 0) into position
                doing (eval (car x))
                finally (return (substring s position))))))

(defun gimme-init ()
  (get-buffer-create gimme-buffer-name)
  (dolist (proc (remove-if-not (lambda (el) (string-match "GIMME" el))
                               (mapcar #'process-name (process-list))))
    (kill-process proc))
  (setq gimme-process
        (start-process-shell-command
         gimme-buffer-name nil
         (format "ruby %s"
                 (expand-file-name
                  (concat
                   (file-name-directory
                    (or load-file-name buffer-file-name)) gimme-executable)))))
  (set-process-filter gimme-process (lambda (a b) (eval-all-sexps b))))

(defun gimme-send-message (message)
  (process-send-string gimme-process message))

(defmacro gimme-generate-commands (&rest args)
  ;; FIXME: Too ugly :(
  `(mapcar 'eval
           ',(mapcar (lambda (f)
                       `(fset ',(read (format "gimme-%s" f))
                              (lambda () (interactive)
                                (process-send-string gimme-process ,(format "%s\n" (list f))))))
                     args)))


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

(defun gimme-update-pos (fun min max)
  (with-current-buffer gimme-buffer-name
    (loop for beg = min then end
          and end = (next-property-change min) then (next-property-change end)
          while (and end (<= end max))
          doing (put-text-property beg end 'pos
                                   (funcall fun (get-text-property beg 'pos))))))

(defun gimme-insert-song (plist append)
  (with-current-buffer gimme-buffer-name
    (save-excursion
      (unlocking-buffer
       (goto-char (if append (point-max)
                    (or (text-property-any (point-min) (point-max)
                                           'pos (getf plist 'pos)) (point-max))))
       (let ((beg (point-marker)))
         (let ((line (car gimme-playlist-formats)))
           (dolist (token '(("%artist" artist) ("%title" title) ("%album" album)))
             (setq line (replace-regexp-in-string (nth 0 token)
                                                  (or (getf plist (nth 1 token)) "nil")
                                                  line)))
           (insert (decode-coding-string line 'utf-8)))
         (insert "\n")
         (add-text-properties beg (point-marker) plist)
         (unless append (gimme-update-pos #'1+ (point-marker) (point-max))))))))

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


(defun gimme-update-playlist (plist)
  ;; FIXME: Deal with multiple playlists(?)
  ;; FIXME: Bloody mess and weird variable names!
  ;; FIXME: add ?== insert?
  ;; Tested:
  ;;  - Remove
  ;;  - Shuffle
  ;;  - Add
  ;;  - Insert
  (case (getf plist 'type)
    ('add    (progn (gimme-insert-song plist t)   (message "Song added!")))
    ('insert (progn (gimme-insert-song plist nil) (message "Song added!")))
    ('remove (progn (when (get-buffer gimme-buffer-name)
                      (with-current-buffer gimme-buffer-name
                        (unlocking-buffer
                         (let* ((beg (text-property-any (point-min) (point-max) 'pos (getf plist 'position)))
                                (end (next-property-change beg)))
                           (when (and beg end) ;; XMMS2 wrongly
                             (clipboard-kill-region beg end)
                             (gimme-update-pos #'1- (point) (point-max)))))))
                    (message "Song removed!")))
    ('move (message "Playlist updated! (moving element)"))
    ('shuffle (progn (gimme-playlist) (message "Playlist shuffled!")))
    ('clear   (progn (gimme-playlist) (message "Playlist cleared!")))
    ('sort    (progn (gimme-playlist) (message "Playlist updated! (sorting list)")))
    ('update  (progn (gimme-playlist) (message "Playlist updated! (updating list)")))))

(defun gimme-toggle-view ()
  ;; FIXME: check perfomance on large playlists, but I think I'll change it avoid reloading the playlist
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  (gimme-playlist))

(defun gimme-set-title (title)
  "Where is my lexical scope when I need it? :("
  (setq gimme-playlist-header title)
  (setq header-line-format
        '(:eval (substring gimme-playlist-header
                           (min (length gimme-playlist-header)
                                (window-hscroll))))))

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

;; Keymap

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "S") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map (kbd "d") 'gimme-focused-delete)
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

;; Temp

(defun gimme-playlist-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-playlist-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-playlist-mode
        mode-name "gimme-playlist"))


(defun gimme ()
  (interactive)
  (gimme-reset)
  (gimme-init)
  (gimme-playlist))

;; Init

(gimme-generate-commands clear shuffle play pause next prev stop toggle
                         inc_vol dec_vol current)
(provide 'gimme)
