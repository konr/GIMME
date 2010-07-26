(defvar gimme-process)
(defvar gimme-executable "gimme.rb")
(defvar gimme-fullpath (expand-file-name
                        (concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         gimme-executable)))
(defvar gimme-current-mode 'playlist)
(defvar gimme-buffer-name "GIMME")
(defvar gimme-session 0)
(defvar gimme-colls-prefix "gimme-")
(defvar gimme-filter-remainder "")
(defvar gimme-debug nil)
(defvar gimme-buffers nil)

(defvar gimme-delete-stack nil)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun gimme-reset () (setq gimme-filter-remainder ""))

(defun gimme-extract-needed-tags ()
  (let* ((l (flatten gimme-playlist-formats))
         (l (remove-if-not (lambda (n) (and (symbolp n)
                                       (string-match "^%" (format "%s" n)))) l))
         (l (mapcar (lambda (n) (substring (format "%s" n) 1))
                    (remove-duplicates l))))
    l))

(defun eval-all-sexps (s)
  (let ((s (concat gimme-filter-remainder s)))
    (setq gimme-filter-remainder
          (loop for x = (ignore-errors (read-from-string s))
                then (ignore-errors (read-from-string (substring s position)))
                while x
                summing (or (cdr x) 0) into position
                doing (let* ((s (car x))
                             (f (caar x))
                             (ok (member f (case gimme-current-mode
                                             (tree  gimme-tree-mode-functions)
                                             (playlist gimme-playlist-mode-functions)
                                             (filter   gimme-filter-mode-functions)))))
                        (when gimme-debug
                          (message (format "GIMME (%s): %s" (if ok "ACK" "NAK") (if (= gimme-debug 2) s f))))
                        (when ok (eval (car x))))
                finally (return (substring s position))))))

(defun gimme-current-duration ()
  ;; FIXME: Won't work on filter view etc
  (with-current-buffer gimme-buffer-name
    (let* ((cur (text-property-any (point-min) (point-max) 'face 'highlight))
           (max (if cur (get-text-property cur 'duration) 0)))
      max)))

(defun gimme-update-playtime (time max)
  (when (get-buffer gimme-buffer-name)
    (with-current-buffer gimme-buffer-name
      (let* ((time (format-seconds "%.2m:%.2s" (/ time 1000)))
             (max  (format-seconds "%.2m:%.2s" (/ max 1000)))
             (msg (format "GIMME: %s/%s" time max)))
        (comment setq gimme-buffer-name msg)
        (comment rename-buffer gimme-buffer-name)
        (gimme-set-title msg)))))

(defun gimme-init ()
  "Creates the buffer and manages the processes"
  (get-buffer-create gimme-buffer-name)
  (dolist (proc (remove-if-not (lambda (el) (string-match "GIMME" el))
                               (mapcar #'process-name (process-list))))
    (kill-process proc))
  (setq gimme-process
        (start-process-shell-command
         gimme-buffer-name nil
         (format "ruby %s" gimme-fullpath )))
  (set-process-filter gimme-process (lambda (a b) (eval-all-sexps b))))

(defun gimme-send-message (&rest args)
  "Formats the arguments using (format) then sends the resulting string to the process."
  (let ((message (apply #'format args)))
    (when gimme-debug (message message))
    (process-send-string gimme-process message)))

(defmacro gimme-generate-commands (&rest args)
  ;; FIXME: Too ugly :(
  `(mapcar 'eval
           ',(mapcar (lambda (f)
                       `(fset ',(read (format "gimme-%s" f))
                              (lambda () (interactive)
                                (process-send-string gimme-process ,(format "%s\n" (list f))))))
                     args)))


(defun gimme-update-pos (fun min max)
  ;; FIXME: Not working
  (with-current-buffer gimme-buffer-name
    (loop for beg = min then end
          and end = (or (next-property-change min) max)
          then (or (next-property-change end) max)
          while (and (<= end max) (< beg end))
          doing (put-text-property
                 beg end 'pos
                 (funcall fun (get-text-property beg 'pos))))))

(defun gimme-insert-song (session plist append)
  (when (= session gimme-session)
    (with-current-buffer gimme-buffer-name
      (save-excursion
        (unlocking-buffer
         (goto-char (if append (point-max)
                      (or (text-property-any (point-min) (point-max)
                                             'pos (getf plist 'pos)) (point-max))))
         (insert (gimme-string plist))
         (unless append (gimme-update-pos #'1+ (point-marker) (point-max))))))))

(defun gimme-string (plist)
  "Receives a song represented as a plist and binds each key as %key to be used by the formatting functions at gimme-playlist-formats"
  ;; FIXME: GENSYM
  (eval `(let ((plist ',plist)
               ,@(mapcar (lambda (n) (list (intern (format "%%%s" (car n))) (if (and (symbolp (cdr n)) (not (null (cdr n)))) (list 'quote (cdr n)) (cdr n))))
                         (plist-to-alist plist)))
           (eval (car gimme-playlist-formats)))))


(defmacro what (number) `(format "%s3" ',(type-of number)))

(defun gimme-update-playlist (plist)
  ;; FIXME: Deal with multiple playlists(?)
  ;; FIXME: Bloody mess and weird variable names!
  ;; FIXME: add ?== insert?
  ;; Not-Implemented:
  ;;  - Move
  (case (getf plist 'type)
    ('add    (progn (gimme-insert-song gimme-session plist t)   (message "Song added!")))
    ('insert (progn (gimme-insert-song gimme-session plist nil) (message "Song added!")))
    ('remove (progn
               (setq gimme-last-del (getf plist 'pos))
               (when gimme-debug (message (format "%s" plist)))
               (when (get-buffer gimme-buffer-name)
                 (with-current-buffer gimme-buffer-name
                   (unlocking-buffer
                    (let* ((beg (text-property-any (point-min) (point-max) 'pos
                                                   (getf plist 'pos)))
                           (end (or (next-property-change (or beg (point-min)))
                                    (point-max))))
                      (when (and beg end)
                        (clipboard-kill-region beg end)
                        (gimme-update-pos #'1- (point) (point-max)))))))
               (message "Song removed!")))
    ('move (progn (gimme-playlist) (message "Playlist shuffled!"))(message "Playlist updated! (moving element)"))
    ('shuffle (progn (gimme-playlist) (message "Playlist shuffled!")))
    ('clear   (progn (gimme-playlist) (message "Playlist cleared!")))
    ('sort    (progn (gimme-playlist) (message "Playlist updated! (sorting list)")))
    ('update  (progn (gimme-playlist) (message "Playlist updated! (updating list)")))))

(defun gimme-set-title (title)
  "Where is my lexical scope when I need it? :("
  (setq gimme-playlist-header title)
  (setq header-line-format
        '(:eval (substring gimme-playlist-header
                           (min (length gimme-playlist-header)
                                (window-hscroll))))))


(defun gimme-toggle-view ()
  ;; FIXME: check perfomance on large playlists,
  ;; but I think I'll change it avoid reloading the playlist
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  (gimme-current-mode))


(defun gimme-playlist-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-playlist-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-playlist-mode
        mode-name "gimme-playlist"))

(defun gimme-tree-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-tree-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-tree-mode
        mode-name "gimme-tree"))

(defun gimme-filter-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-filter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-filter-mode
        mode-name "gimme-filter"))

(defun gimme-current-mode ()
  (interactive)
  (funcall (case gimme-current-mode
             (tree 'gimme-tree)
             (filter 'gimme-filter)
             (playlist 'gimme-playlist))))

(defun gimme ()
  (interactive)
  (gimme-reset)
  (gimme-init)
  (gimme-send-message (format "(set_atribs %s)\n" (gimme-extract-needed-tags)))
  (gimme-current-mode))

;; Init

(gimme-generate-commands clear shuffle play pause next prev stop toggle
                         inc_vol dec_vol current)
(require 'gimme-playlist)
(require 'gimme-tree)
(require 'gimme-filter)
(require 'gimme-utils)
(require 'gimme-config)
(provide 'gimme)
