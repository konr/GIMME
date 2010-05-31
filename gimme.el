(defvar gimme-process)
;; Not working on some computers
(defvar gimme-executable "ruby ~/Projetos/GIMME/gimme.rb")
(defvar gimme-buffer)
(defvar gimme-buffer-name "GIMME")
(defvar gimme-playlist-header "GIMME - Playlist view")
(defvar gimme-filter-remainder "")
(defvar gimme-playlist-formats '("%artist > %title"
                                 "%title"
                                 "%artist > %album > %title"))

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

(defun gimme-set-playing (id)
  "Highlights the currently played song"
  (when (get-buffer gimme-buffer-name)
    (with-current-buffer gimme-buffer-name
      (let* ((h-beg t) (h-end t))
        (while h-beg
          (setq h-beg (text-property-any (point-min) (point-max) 'face 'highlight))
          (setq h-end (next-property-change (or h-beg (point-min))))
          (when h-beg (remove-text-properties h-beg h-end '(face nil)))))
      (let* ((beg (text-property-any (point-min) (point-max) 'id id))
             (end (next-property-change (or beg (point-min)))))
        (when beg (put-text-property beg end 'face 'highlight))))))

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
  (setq gimme-process
        (start-process-shell-command ""
                                     nil gimme-executable))
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
    (gimme-playlist-mode)
    (setq header-line-format '(:eval (substring gimme-playlist-header
                                                (min (length gimme-playlist-header)
                                                     (window-hscroll)))))
    (clipboard-kill-region 1 (point-max))
    (save-excursion
      (gimme-send-message "(list)\n")))
  (switch-to-buffer (get-buffer gimme-buffer-name))) ;; FIXME: Quite redundant and ugly


(defun gimme-append-to-buffer (plist)
  (set-buffer gimme-buffer-name)
  (with-current-buffer gimme-buffer-name
    (save-excursion
      (goto-char (point-max))
      (let ((beg (point-marker)))
        (let ((line (car gimme-playlist-formats)))
          (dolist (token '(("%artist" artist) ("%title" title) ("%album" album)))
            (setq line (replace-regexp-in-string (nth 0 token)
                                                 (or (getf plist (nth 1 token)) "nil")
                                                 line)))
          (insert line))
        (insert "\n")
        (add-text-properties beg (point-marker) plist)))))

(defun gimme-focused-play () ;; FIXME: Message "tickle" received
  (interactive)
  (message (format "%s" (get-text-property (point) 'pos)))
  (gimme-send-message
   (format "%s\n" (list 'playn (get-text-property (point) 'pos)))))

(defun gimme-update-playlist ()
  (gimme-playlist)) ;; FIXME: this approach unfortunately won't work :(

(defun gimme-toggle-view ()
  ;; FIXME: check perfomance on large playlists, but I think I'll change it avoid reloading the playlist
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  (gimme-playlist))

;; Keymap

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "q") 'gimme-close)
    (define-key map (kbd "n") 'gimme-next)
    (define-key map (kbd "p") 'gimme-prev)
    (define-key map (kbd "s") 'gimme-stop)
    (define-key map (kbd "P") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
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
;; Init

(gimme-init)
(gimme-generate-commands play pause next prev stop toggle inc_vol dec_vol current)
(provide 'gimme)
