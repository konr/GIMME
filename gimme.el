(defvar *gimme-process*)
(defvar *gimme-executable*)

(defun gimme-init ()
  (let ((b (generate-new-buffer "*gimme*")))
    (setq *gimme-process*
          (start-process-shell-command
           (buffer-name b)
           b *gimme-executable*))
    (set-process-filter *gimme-process* (lambda (proc txt) (eval (read txt))))))

(defun gimme-send-message (message)
  (process-send-string *gimme-process* message))

(defmacro gimme-generate-commands (&rest args)
  ;; FIXME: Too ugly :(
  `(mapcar 'eval 
          ',(mapcar (lambda (f)
             `(fset ',(read (format "gimme-%s" f))
                    (lambda nil (interactive)
                      (process-send-string *gimme-process* ,(format "%s\n" f)))))
           args)))


;; Init

(setq *gimme-executable* "ruby ~/Projetos/GIMME/gimme.rb")
(gimme-init)
(gimme-generate-commands play pause)
(provide 'gimme)
