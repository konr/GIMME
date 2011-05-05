;; Observer pattern, Hooks and Changes


(defvar hooker-vars nil)
(defvar hooker-ops nil)
(defvar hooker-hooks nil)

(defun plist-del (plist index)
  (if (equal index (car plist)) (cddr plist)
    `(,(car plist) ,(cadr plist)
      ,@(plist-del (cddr plist) index))))

(defun hooker-var-add (name &optional initial-value)
  (setq hooker-vars (plist-put hooker-vars name initial-value))
  (setq hooker-hooks (plist-put hooker-hooks name nil))
  (setq hooker-ops (plist-put hooker-ops name nil)))

(defun hooker-var-del (name)
  (setq hooker-vars (plist-del hooker-vars name))
  (setq hooker-hooks (plist-del hooker-hooks name)))

(defun hooker-hook (name function)
  ;; FIXME set functions
  (setq hooker-hooks
        (plist-put hooker-hooks name (cons function (plist-get hooker-hooks name)))))

(defun hooker-unhook (name function)
  (setq hooker-hooks
        (plist-put hooker-hooks name (remove-if (lambda (n) (equal n function))
                                                (plist-get hooker-hooks name)))))

(defun hooker-get (name) (plist-get hooker-vars name))
(defun hooker-set (name value)
  (let ((old (hooker-get name)))
    (setq hooker-vals (plist-put hooker-vars name value))
    (let ((new (hooker-get name)))
      (dolist (fun (plist-get hooker-hooks name))
        (funcall fun old new)))))

(defun hooker-op-def (var-name fun-name fun)
  (let ((new (plist-put (plist-get hooker-ops var-name) 'fun-name fun)))
    (setq hooker-ops (plist-put hooker-ops var-name new))))


(provide 'hooker)
