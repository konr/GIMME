(defvar *process*)

(defun init ()
  (let ((b (generate-new-buffer "*gimme*")))
    (setq *process*
          (start-process-shell-command
           (buffer-name b)
           b "ruby ~/Projetos/GIMME/gimme.rb"))
    (set-process-filter foo (lambda (proc txt) (eval (read txt))))))

(defun test ()
  (process-send-string foo "test\n"))





