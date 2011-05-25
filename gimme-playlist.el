;;; gimme-playlist.el ends here
;;; gimme-playlist.el --- GIMME's playlist-view

;; Author: Konrad Scorciapino <scorciapino@gmail.com>
;; Keywords: XMMS2, mp3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code

(defun gimme-playlist ()
  "Sets up the buffer"
  (interactive)
  (gimme-playlist-update "Default"))

(defun gimme-playlist-update (playlist)
  (let ((buffer-name (format "GIMME - Playlist (%s)" playlist)))
    (gimme-on-buffer buffer-name
                     (gimme-playlist-mode)
                     (gimme-set-title gimme-playlist-header)
                     (kill-region 1 (point-max))
                     (gimme-send-message "(list %s %s)\n"
                                         (prin1-to-string playlist) ;; FIXME
                                         (prin1-to-string buffer-name))
                     (make-local-variable 'gimme-buffer-type)
                     (setq gimme-buffer-type 'playlist)
                     (make-local-variable 'gimme-playlist-name)
                     (setq gimme-playlist-name playlist))
    (run-hooks 'gimme-goto-buffer-hook)
    (switch-to-buffer (get-buffer buffer-name))))

(defvar gimme-playlist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") (lambda () (interactive)
                                (gimme-filter (read-from-minibuffer "S: "))))
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "T") 'gimme-update-tags-prompt)
    (define-key map (kbd "H") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map [remap kill-line] '(lambda () (interactive) (gimme-focused-delete t)))
    (define-key map (kbd "d") 'kill-line)
    (define-key map [remap yank] (lambda () (interactive) (gimme-paste-deleted nil)))
    (define-key map (kbd "p") 'yank)
    (define-key map (kbd "S") 'gimme-sort)
    (define-key map (kbd "s") 'gimme-toggle-sort)
    (define-key map (kbd "SPC") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    (define-key map (kbd "l") 'gimme-center)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") (lambda () (interactive) (gimme-vol gimme-vol-delta)))
    (define-key map (kbd "+") (lambda () (interactive) (gimme-vol gimme-vol-delta)))
    (define-key map (kbd "-") (lambda () (interactive) (gimme-vol (- gimme-vol-delta))))
    (define-key map (kbd "?") 'gimme-focused-url)
    (define-key map (kbd "*") 'gimme-toggle-star)
    (define-key map (kbd "[") (lambda () (interactive) (gimme-seek -1000  t)))
    (define-key map (kbd "{") (lambda () (interactive) (gimme-seek -10000 t)))
    (define-key map (kbd "]") (lambda () (interactive) (gimme-seek  1000  t)))
    (define-key map (kbd "}") (lambda () (interactive) (gimme-seek  10000 t)))
    (define-key map (kbd "g") 'gimme-goto-pos)
    map)
  "Playlist-view's keymap")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-broadcast-playlist (plist)
  "Called by the playlist_changed broadcast"
  ;; FIXME: Not seriouly implemented: Move
  (let* ((name (getf plist 'name))
         (buffer (gimme-first-buffer-with-vars 'gimme-buffer-type 'playlist
                                               'gimme-playlist-name name)))
    (case (getf plist 'type)
      ('add    (progn (run-hook-with-args 'gimme-broadcast-pl-add-hook plist)
                      (gimme-insert-song buffer plist t)
                      (message "Song added!")))
      ('insert (progn (run-hook-with-args 'gimme-broadcast-pl-insert-hook plist)
                      (gimme-insert-song buffer plist nil)
                      (message "Song added!")))
      ('remove (progn (run-hook-with-args 'gimme-broadcast-pl-remove-hook plist)
                      (with-current-buffer (get-buffer buffer)
                        (unlocking-buffer
                         (let* ((beg (text-property-any (point-min) (point-max) 'pos
                                                        (getf plist 'pos)))
                                (end (or (next-property-change (or beg (point-min)))
                                         (point-max))))
                           (when (and beg end)
                             (clipboard-kill-region beg end)
                             (gimme-update-pos buffer #'1- (point) (point-max))))))))
      ('move    (progn (run-hook-with-args 'gimme-broadcast-pl-move-hook plist)
                       (gimme-playlist-update name)
                       (message "Playlist updated! (moving element)")))
      ('shuffle (progn (run-hook-with-args 'gimme-broadcast-pl-shuffle-hook plist)
                       (gimme-playlist-update name)
                       (message "Playlist shuffled!")))
      ('clear   (progn (run-hook-with-args 'gimme-broadcast-pl-clear-hook plist)
                       (gimme-playlist-update name)
                       (message "Playlist cleared!")))
      ('sort    (progn (run-hook-with-args 'gimme-broadcast-pl-sort-hook plist)
                       (gimme-playlist-update name)
                       (message "Playlist updated! (sorting list)")))
      ('update  (progn (run-hook-with-args 'gimme-broadcast-pl-update-hook plist)
                       (gimme-playlist-update name)
                       (message "Playlist updated! (updating list)"))))))
(defun gimme-update-pos (buffer fun min max)
  "Updates the position of all elements betweeen beg and pos with function fun"
  (with-current-buffer buffer
    (loop for beg = min then end
          and end = (or (next-property-change min) max)
          then (or (next-property-change end) max)
          while (and (<= end max) (< beg end))
          doing (put-text-property
                 beg end 'pos
                 (funcall fun (get-text-property beg 'pos))))))

(defun gimme-set-playing (playlist pos)
  "Highlights the currently played song"
  (let ((buffer (gimme-first-buffer-with-vars 'gimme-buffer-type 'playlist
                                              'gimme-playlist-name playlist)))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (unlocking-buffer
         (let* ((h-beg t) (h-end t))
           (while h-beg
             (setq h-beg (text-property-any (point-min) (point-max) 'face 'highlight))
             (setq h-end (or (next-property-change (or h-beg (point-min))) (point-max)))
             (when h-beg (remove-text-properties h-beg h-end '(face nil)))))
         (let* ((beg (text-property-any (point-min) (point-max) 'pos pos))
                (end (next-property-change (or beg (point-min)))))
           (when beg
             (put-text-property beg (or end (point-max)) 'face 'highlight))))))))

(defun gimme-update-tags (plist-b)
  "Updates the tags of song, whose id must be at the plist"
  (when (get-buffer gimme-buffer-name)
    (with-current-buffer gimme-buffer-name
      (let* ((id (getf plist-b 'id))
             (pos-list (range-to-plists (point-min) (point-max)))
             (pos-list (remove-if-not (lambda (n) (equal id (getf n 'id))) pos-list))
             (pos-list (mapcar (lambda (n) (getf n 'pos)) pos-list)))
        (dolist (pos pos-list)
          (let* ((beg (text-property-any (point-min) (point-max) 'pos pos))
                 (end (or (next-property-change beg) (point-max))))
            (unless (plist-subset plist-b (text-properties-at beg))
              (unlocking-buffer
               (plist-put plist-b 'pos pos)
               (plist-put plist-b 'font-lock-face nil)
                                        ; FIXME: For some reason,  when a duplicated
                                        ; song is starred or changed, the plist will
                                        ; contain font-lock-face, which can't be
                                        ; evaluated. The previous line """Fixes""" it,
                                        ; but I guess that this bug will eventually
                                        ; show up in other parts of the code.
                                        ;
                                        ; FIXME: Double check the macros and rewrite
                                        ; the code in a more functional style
               (kill-region beg end)
               (save-excursion
                 (goto-char beg)
                 (insert (gimme-string plist-b)))))))))))

(defun gimme-playlist-mode ()
  "Displays a playlist"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-playlist-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-playlist-mode
        mode-name "gimme-playlist"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-insert-song (buffer plist append)
  "Inserts (or appends) an element matching the plist
#FIXME: For now still accepting strings"
  (let ((buffer (if (or (bufferp buffer) (stringp buffer)) buffer
                  (gimme-first-buffer-with-vars buffer))))
    (gimme-on-buffer
     buffer
     (goto-char (if append (point-max)
                  (or (text-property-any (point-min) (point-max)
                                         'pos (getf plist 'pos)) (point-max))))
     (insert (gimme-string plist))
     (unless append (gimme-update-pos buffer #'1+ (point-marker) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-vol (delta)
  "Increases the current volume by delta (can be non-positive, too)"
  (interactive)
  (when (integerp delta)
    (gimme-send-message "(vol %d)\n" delta)))

(defun gimme-sort ()
  "Sorts the current playlist"
  (interactive)
  (gimme-send-message "(sort %s)\n" (car gimme-sort-criteria)))

(defun gimme-toggle-sort ()
  "Cycle through the sort criteria"
  (interactive)
  (setq gimme-sort-criteria (append (cdr gimme-sort-criteria)
                                    (list (car gimme-sort-criteria))))
  (message (format "Sorting now by %s%s" (caar gimme-sort-criteria)
                   (apply #'concat (mapcar (lambda (n) (format " > %s" n))
                                           (cdar gimme-sort-criteria))))))

(defun gimme-focused-play ()
  "Plays the currently focused song"
  (interactive)
  (let ((pos (get-text-property (point) 'pos)))
    (when pos (gimme-send-message "(playn %s)\n" pos))))

(defun gimme-paste-deleted (undo)
  "Pastes a song at the car of the kill-ring"
  (interactive)
  (let* ((last (car kill-ring))
         (pos (get-text-property (point) 'pos))
         (ids (loop for pos = 0 then (next-property-change pos last)
                    while pos collecting (get-text-property pos 'id last))))
    (when (and pos ids)
      (dolist (id ids)
        (setq pos (+ 1 pos))
        (gimme-send-message "(insert %s %s)\n" pos id)))))


(defun gimme-focused-delete (delete-p)
  "Deletes the currently focused song."
  (interactive)
  (if (use-region-p)
      (let* ((min (min (point) (mark)))
             (max (max (point) (mark)))
             (min (progn (goto-char min) (line-beginning-position)))
             (max (+ 1 (progn (goto-char max) (line-end-position)))))
        (kill-ring-save min max))
    (kill-ring-save (line-beginning-position) (line-end-position)))
  (let ((items (loop for pos = 0 then (next-property-change pos (car kill-ring))
                     while pos collecting (get-text-property pos 'pos (car kill-ring)))))
    (unless (null (car items))
      (dolist (item (list (car items)))  ;; FIXME
        (gimme-send-message "(remove %d)\n" item)))))


(defun gimme-focused-url ()
  "Asks for the song's current URL."
  (interactive)
  (message (get-text-property (point) 'url)))

(defun gimme-center ()
  "Centers buffer on currently playing song"
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((h-beg (text-property-any (point-min) (point-max) 'face 'highlight)))
      (if h-beg (goto-char h-beg) (message "Not on the playlist.")))))

(defun gimme-seek (time relativep)
  "Jumps to a different position. Time must be given in miliseconds."
  (interactive)
  (gimme-send-message "(%s \"%d\")\n" (if relativep "seek_rel" "seek") time))

(defun gimme-goto-pos ()
  "Prompts for a position, to be given in percentage or [[hours:]minutes:]seconds"
  (interactive)
  (let* ((input (read-from-minibuffer "Goto: "))
         (time (cond ((string-match "^[0-9:]\+$" input)
                      (reduce (lambda (x y) (+ (* 60 x) y))
                              (mapcar #'string-to-int (split-string input ":"))))
                     ((string-match "^[0-9]\+%$" input)
                      (/ (* (or (cdr (assoc 'max gimme-playtime)) 0)
                            (string-to-int (substring input 0 -1)))
                         100000))
                     (t nil))))
    (if time (gimme-seek (* 1000 time) nil) (message "Unrecognized format!"))))

(defun gimme-toggle-star ()
  "Adds/removes a star property"
  (interactive)
  (let* ((plist (text-properties-at (point)))
         (starredp (getf plist 'starred))
         (string (if (string= "t" starredp) "nil" "t"))
         (plist (append plist `(starred ,string)))
         (alist (remove-if (lambda (n) (member (car n) '(face font-lock-face)))
                           (plist-to-pseudo-alist plist)))
         (alist (mapcar (lambda (n) `(,(car n) ,(if (stringp (cadr n))
                                               (format "%s" (decode-coding-string (cadr n) 'utf-8))
                                             (cadr n)))) alist)))
    (gimme-send-message "(update_tags %s)\n" (prin1-to-string alist))))

(defun gimme-update-tags-prompt ()
  "Prompts and updates title/artist and album"
  (interactive)
  (let* ((alist (remove-if (lambda (n) (member (car n) '(face font-lock-face)))
                           (plist-to-pseudo-alist (text-properties-at (point)))))
         (alist (mapcar (lambda (n) (if (stringp (cadr n))
                                   (list (car n) (decode-coding-string (cadr n) 'utf-8))
                                 n)) alist))
         (alist (mapcar (lambda (n) (if (member (car n) '(title album artist))
                                   (list (car n) (format "%s"
                                                         (read-from-minibuffer
                                                          (format "%s? " (car n))
                                                          (format "%s" (cadr n)))))
                                 n))
                        alist)))
    (gimme-send-message "(update_tags %s)\n" (prin1-to-string alist))))



(provide 'gimme-playlist)
;;; gimme-playlist.el ends here
