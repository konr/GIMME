(defvar *gimme-process*)
(defvar *gimme-executable*)
(defvar *gimme-buffer*)
(defvar *gimme-buffer-name*)


;; Aux

(defun eval-all-sexps (s)
  (let ((session (random 1000))) ; FIXME: debugging
    (loop for x = (ignore-errors (read-from-string s))
          then (ignore-errors (read-from-string (substring s position)))
          while x
          summing (or (cdr x) 0) into position
          doing (progn
                                        ; (message "session: %d - x = %s" session x) ; FIXME: Debugging
                  (eval (car x))))))

(defun ordinary-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun gimme-init ()
  (get-buffer-create *gimme-buffer-name*)
  (setq *gimme-process*
        (start-process-shell-command
         ""
         nil *gimme-executable*))
  (set-process-filter *gimme-process* (lambda (a b) (eval-all-sexps b))))

(defun gimme-send-message (message)
  (process-send-string *gimme-process* message))

(defmacro gimme-generate-commands (&rest args)
  ;; FIXME: Too ugly :(
  `(mapcar 'eval
           ',(mapcar (lambda (f)
                       `(fset ',(read (format "gimme-%s" f))
                              (lambda () (interactive)
                                (process-send-string *gimme-process* ,(format "%s\n" (list f))))))
                     args)))


(defun gimme-playlist ()
  (interactive)
  (gimme-playlist-mode)
  (get-buffer-create *gimme-buffer-name*)
  (with-current-buffer *gimme-buffer-name*
    (setq header-line-format '(:eval (substring "GIMME - Playlist view"
                                                (min (length my-header)
                                                     (window-hscroll)))))
    (clipboard-kill-region 1 (point-max))
    (save-excursion
      (gimme-send-message "(list)\n"))))


(defun gimme-append-to-buffer (plist)
  (set-buffer *gimme-buffer-name*)
  (with-current-buffer *gimme-buffer-name*
    (goto-char (point-max))
    (let ((beg (point-marker)))
      (insert (getf plist 'title))
      (insert "\n")
      (add-text-properties beg (point-marker) plist))))

(defun gimme-focused-play () ;; FIXME: Message "tickle" received
  (interactive)
  (gimme-send-message
   (format "%s\n" (list 'playn (get-text-property (point)'tracknr)))))

;; Keymap

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gimme-focused-play)
    map))
; (fset 'gimme-playlist-map gimme-playlist-map)

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

(setq *gimme-executable* "ruby ~/Projetos/GIMME/gimme.rb")
(setq *gimme-buffer-name* "*gimme*")
(gimme-init)
(gimme-generate-commands play pause next prev)
(provide 'gimme)
