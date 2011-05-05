(provide 'observer)
(defvar observer-plist nil)

(defun observer-var-add (name &optional initial-value)
  (setq observer-plist (plist-put observer-plist name initial-value)))

(defun change-observed-data (data value)
  )
