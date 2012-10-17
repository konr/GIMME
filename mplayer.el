(require 'cl)

(defun expand-directories (files)
  "Given a list of files and directories, expand the directories recursively, adding their children to list"
  (mapcan (lambda (x) (if (file-directory-p x) (expand-directories (directory-files x t))
                   (list x))) (remove-if (lambda (x) (string-match "/\\.\\.?$" x)) files)))


(defvar mplayer-buffer "MPlayer buffer")
(defvar mplayer-directories '("/opt/Media"))
(defvar mplayer-process nil)
(defvar mplayer-last 0)
(defvar mplayer-files (expand-directories mplayer-directories))

(defun mplayer-play (&optional id)
  (let* ((id (or id mplayer-last))
         (file (nth id mplayer-files)))
    (mplayer-quit)
    (setq mplayer-last id
          mplayer-process
          (start-process-shell-command
           mplayer-buffer nil (format "mplayer '%s'" file)))
    (set-process-filter mplayer-process #'mplayer-filter)))

(defun mplayer-filter (a command)
  (let* ((safe (replace-regexp-in-string "%" "%%" command))
         (split (split-string safe))
         (cur (nth 1 split)) (total (nth 4 split)))
    (when (> (length split) 4)
      (message (format "(gimme-update-playtime %s %s)\n" cur total)))
    (when (string-match "(End of file)" safe)
      (mplayer-next))))

(defun mplayer-send (message)
  (ignore-errors (process-send-string mplayer-process message)))

(defun mplayer-next ()
  (let ((next (if (>= mplayer-last (length mplayer-files)) 0
                (1+ mplayer-last))))
    (mplayer-play next)))

(defun mplayer-prev ()
  (let ((prev (if (<= mplayer-last 0) (1- (length mplayer-files))
                (1- mplayer-last))))
    (mplayer-play prev)))

(defun mplayer-quit ()
  (mplayer-send "q"))

(defun mplayer-pause ()
  (mplayer-send " "))

(while t (princ (eval (read))))
