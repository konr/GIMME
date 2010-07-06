(defvar gimme-process)
(defvar gimme-debug t)
(defvar gimme-executable "gimme.rb")
(defvar gimme-fullpath (expand-file-name
                        (concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         gimme-executable)))
(defvar gimme-current-mode nil)
(defvar gimme-buffer-name "GIMME")
(defvar gimme-colls-prefix "gimme-")
(defvar gimme-filter-header "GIMME - Filter view")
(defvar gimme-filter-remainder "")
(defvar gimme-debug nil)
(defvar gimme-playlist-formats '("%artist > %title"
                                 "%title"
                                 "%artist > %album > %title"))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun gimme-reset ()
  (setq gimme-last-ids nil)
  (setq gimme-filter-remainder ""))


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
                                             (playlist gimme-playlist-mode-functions)
                                             (filter   gimme-filter-mode-functions)))))
                        (when gimme-debug
                          (message (format "GIMME (%s): %s" (if ok "ACK" "NAK") f)))
                        (when ok (eval (car x))))
                finally (return (substring s position))))))

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


(defun gimme-update-pos (fun min max)
  ;; FIXME: Not working
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
         (insert (gimme-string plist))
         (unless append (gimme-update-pos #'1+ (point-marker) (point-max)))))))

(defun gimme-string (plist)
  (let ((line (car gimme-playlist-formats)))
    (dolist (token '(("%artist" artist) ("%title" title) ("%album" album)))
      (setq line (replace-regexp-in-string (nth 0 token)
                                           (or (getf plist (nth 1 token)) "nil")
                                           line)))
    (apply #'propertize (format "%s\n" (decode-coding-string line 'utf-8))
                plist)))

(defun gimme-update-playlist (plist)
  ;; FIXME: Deal with multiple playlists(?)
  ;; FIXME: Bloody mess and weird variable names!
  ;; FIXME: add ?== insert?
  ;; Not-Implemented:
  ;;  - Move
  (case (getf plist 'type)
    ('add    (progn (gimme-insert-song plist t)   (message "Song added!")))
    ('insert (progn (gimme-insert-song plist nil) (message "Song added!")))
    ('remove (progn
               (when gimme-debug (message (format "%s" plist)))
               (when (get-buffer gimme-buffer-name)
                 (with-current-buffer gimme-buffer-name
                   (unlocking-buffer
                    (let* ((beg (text-property-any (point-min) (point-max) 'pos (getf plist 'pos)))
                           (end (next-property-change beg)))
                      (when (and beg end)
                        (clipboard-kill-region beg end)
                        (gimme-update-pos #'1- (point) (point-max)))))))
               (message "Song removed!")))
    ('move (message "Playlist updated! (moving element)"))
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

;; Keymap


(defun gimme-playlist-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-playlist-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-playlist-mode
        mode-name "gimme-playlist"))

(defun gimme-filter-mode ()
  "FIXME: Write something here"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-filter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-filter-mode
        mode-name "gimme-filter"))

(defun gimme ()
  (interactive)
  (gimme-reset)
  (gimme-init)
  (gimme-playlist))

;; Init

(gimme-generate-commands clear shuffle play pause next prev stop toggle
                         inc_vol dec_vol current)
(require 'gimme-playlist)
(require 'gimme-filter)
(require 'gimme-utils)
(provide 'gimme)
