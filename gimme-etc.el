(defun gimme-add-path-to-playlist (path)
  "Adds a given path to the collection and playlist. Requires a fullpath"
  (gimme-send-message "(add %s)\n" (prin1-to-string (format "file://%s" path))))

(provide 'gimme-etc)
