(defun gimme-add-path-to-playlist (path)
  "Adds a given path to the collection and playlist. Requires a fullpath"
  (gimme-send-message "(add %s)\n" (prin1-to-string (format "file://%s" path))))

(defun gimme-add-marked-to-the-playlist-recursively ()
  (interactive)
  (let* ((initial (dired-get-marked-files))
	 (expanded (gimme-expand-directories initial)))
    (dolist (item expanded) (gimme-add-path-to-playlist item))))

(defun gimme-expand-directories (files)
  (mapcan (lambda (x) (if (file-directory-p x) 
		     (gimme-expand-directories (directory-files x t))
		   (list x))) (remove-if (lambda (x) (string-match "/\\.\\.?$" x)) files)))

(provide 'gimme-etc)
