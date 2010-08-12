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
(defvar gimme-debug 0)
(defvar gimme-buffers nil)
(defvar gimme-playtime nil)

(defvar gimme-delete-stack nil)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun gimme-extract-needed-tags ()
  "Informs the ruby client of all %variables required by the config file"
  (let* ((l (flatten gimme-playlist-formats))
         (l (remove-if-not (lambda (n) (and (symbolp n)
                                       (string-match "^%" (format "%s" n)))) l))
         (l (mapcar (lambda (n) (substring (format "%s" n) 1))
                    (remove-duplicates l))))
    l))

(defun eval-all-sexps (s)
  "Evaluates all sexps from the string. As it will probably encounter a broken sexp, a variable is used to store the remainder to be used in future calls"
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
                        (when (> gimme-debug 0)
                          (message (format "GIMME (%s): %s" (if ok "ACK" "NAK") (if (>= gimme-debug 2) f s))))
                        (when (and ok (> 3 gimme-debug)) (eval (car x))))
                finally (return (substring s position))))))

(defun gimme-update-playtime (time max)
  "Updates the playtime in the gimme-playtime variable"
  (setq gimme-playtime `((time . ,time) (max . ,max))))

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
    (when (> gimme-debug 0) (message message))
    (process-send-string gimme-process message)))

(defmacro gimme-generate-commands (&rest args)
  "Generates commands that don't require arguments"
  `(mapcar 'eval
           ',(mapcar (lambda (f)
                       `(fset ',(read (format "gimme-%s" f))
                              (lambda () (interactive)
                                (process-send-string gimme-process 
                                                     ,(format "%s\n" (list f))))))
                     args)))


(defun gimme-string (plist)
  "Receives a song represented as a plist and binds each key as %key to be used by the formatting functions at gimme-playlist-formats"
  (eval `(let ((plist ',plist)
               ,@(mapcar (lambda (n) (list (intern (format "%%%s" (car n))) 
                                      (if (and (symbolp (cdr n)) (not (null (cdr n)))) 
                                          (list 'quote (cdr n)) (cdr n))))
                         (plist-to-alist plist)))
           (eval (car gimme-playlist-formats)))))


(defun gimme-set-title (title)
  "Changes the header of a buffer"
  (setq gimme-playlist-header title)
  (setq header-line-format
        '(:eval (substring (decode-coding-string gimme-playlist-header 'utf-8)
                           (min (length gimme-playlist-header)
                                (window-hscroll))))))


(defun gimme-toggle-view ()
  "Cycle through the views defined in gimme-config"
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  (gimme-current-mode))

(defun gimme-current-mode ()
  "Funcalls the current mode"
  (interactive)
  (funcall (case gimme-current-mode
             (tree 'gimme-tree)
             (filter 'gimme-filter)
             (playlist 'gimme-playlist))))

(defun gimme ()
  "The XMMS2 player we all love"
  (interactive)
  (setq gimme-filter-remainder "")
  (gimme-init)
  (gimme-send-message (format "(set_atribs %s)\n" (gimme-extract-needed-tags)))
  (gimme-current-mode))

;;;;;;;;;;
;; Init ;;
;;;;;;;;;;

(gimme-generate-commands clear shuffle play pause next prev stop toggle current)
(require 'gimme-utils)
(require 'gimme-playlist)
(require 'gimme-tree)
(require 'gimme-filter)
(require 'gimme-status-mode)
(require 'gimme-config)
(provide 'gimme)
