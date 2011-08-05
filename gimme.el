;;; gimme.el --- GIMME Interesting Music on My Emacs

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

;;; Commentary

;; GIMME (GIMME Interesting Music on My Emacs) is an XMMS2 client
;; originally developed for Google's Summer of Code. Kudos to them
;; and to DraX for the Support.

;; GIMME works by using collections as search results, and its multiple
;; views allows you to do that in multiple ways. As of GIMME 1.0, you
;; can search and narrow searching using filter-view and better visualize
;; it using bookmark-view.

;;; Code

(defvar gimme-process nil
  "Reference to the ruby process")
(defvar gimme-executable "gimme.rb"
  "The name of the ruby file")
(defvar gimme-fullpath (expand-file-name
                        (concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         gimme-executable))
  "The fullname of the ruby file")
(defvar gimme-current-mode 'playlist
  "In which mode GIMME current is")
(defvar gimme-buffer-name "GIMME"
  "GIMME's buffer name")
(defvar gimme-filter-remainder ""
  "Variable used to hold incomplete sexps received from the ruby process")
(defvar gimme-debug 0
  "To debug.
  0: Do nothing extra
  1: Prints the functions being called by the ruby process
  2: Print the whole sexps
  3: Print the whole sexps and do not evaluate them")
(defvar gimme-playtime nil
  "Variable used to hold the current track's duration and playtime")
(defvar gimme-current nil
  "The current collection. Can be a string or an idlist")
(defvar gimme-bookmarks nil
  "Collections not saved on the core")

(defvar gimme-bookmark-header "GIMME - bookmark View" "Initial header")
(defvar gimme-playlist-header "GIMME - Playlist view" "Initial header")
(defvar gimme-filter-header "GIMME" "Initial header")


;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun gimme-buffers (&optional function)
  "Get all buffers used by GIMME. FIXME: mode"
  (let* ((modes '(gimme-playlist-mode gimme-filter-mode gimme-bookmark-mode))
         (buffers (remove-if-not (lambda (buf) (member (major-mode buf) modes))
                                 (buffer-list)))
         (filtered (remove-if-not function buffers)))
    filtered))

(defun gimme-first-buffer-with-vars (&rest plist)
  (let* ((all (gimme-buffers)) (alist (plist-to-alist plist))
         (consed (mapcar (lambda (el) (cons el (buffer-local-variables el))) all)))
    (caar (reduce (lambda (coll pair)
                    (remove-if-not (lambda (el) (equal (cdr pair)
                                                  (cdr (assoc (car pair) (cdr el)))))
                                   coll)) alist :initial-value consed))))

(defun gimme-gen-buffer (plist)
  "FIXME: Filter Collection; hook; header not working"
  (let* ((type (getf plist 'gimme-buffer-type))
         (type-s (case type ('collection "Collection") ('playlist "Playlist")
                       ('lyrics "Lyrics")))
         (name-s (case type
                   ('collection (getf plist 'gimme-collection-title))
                   ('playlist (getf plist 'gimme-playlist-name))
                   ('lyrics (getf plist 'title))))
         (buffer-name (decode-coding-string
                       (format "GIMME - %s (%s)" type-s name-s) 'utf-8)))
    (gimme-on-buffer buffer-name
                     (comment setq header-line-format
                              `(:eval (decode-coding-string ,buffer-name 'utf-8)))
                     (case type
                       ('playlist (gimme-playlist-mode))
                       ('collection (gimme-filter-mode)))
                     (kill-region 1 (point-max))
                     (loop for x = plist then (cddr x)
                           while x doing (progn (make-local-variable (car x))
                                                (set (car x) (cadr x)))))
    (switch-to-buffer (get-buffer buffer-name))
    buffer-name))


(defun gimme-extract-needed-tags ()
  "Informs the ruby client of all %variables required by the config file"
  (let* ((l (flatten gimme-playlist-formats))
         (l (remove-if-not (lambda (n) (and (symbolp n)
                                       (string-match "^%" (format "%s" n)))) l))
         (l (mapcar (lambda (n) (substring (format "%s" n) 1))
                    (remove-duplicates l))))
    l))

(defun gimme-eval-all-sexps (s)
  "Evaluates all sexps from the string. As it will probably encounter a broken sexp, a variable is used to store the remainder to be used in future calls"
  (let ((s (decode-coding-string (concat gimme-filter-remainder s) 'utf-8)))
    (setq gimme-filter-remainder
          (loop for x = (ignore-errors (read-from-string s))
                then (ignore-errors (read-from-string (substring s position)))
                while x
                summing (or (cdr x) 0) into position
                doing (let* ((s (decode-strings-in-tree (car x) 'utf-8))
                             (f (caar x)))
                        (when (> gimme-debug 0)
                          (message (format "GIMME: %s" (if (>= gimme-debug 2) s f))))
                        (when (> 3 gimme-debug)
                          (ignore-errors (eval s))))
                finally (return (substring s position))))))


(defun gimme-update-playtime (time max)
  "Updates the playtime in the gimme-playtime variable"
  (setq gimme-playtime `((time . ,time) (max . ,max))))

(defmacro gimme-on-buffer (name &rest body)
  "FIXME: Gimme v2"
  `(with-current-buffer (get-buffer-create ,name)
     (unlocking-buffer (save-excursion (goto-char (point-max)) ,@body))))

(defun gimme-init ()
  "Creates the buffer and manages the processes"
  (dolist (proc (remove-if-not (lambda (el) (string-match "GIMME" el))
                               (mapcar #'process-name (process-list))))
    (kill-process proc))
  (setq gimme-process
        (start-process-shell-command
         gimme-buffer-name nil
         (format "ruby %s" gimme-fullpath )))
  (set-process-filter gimme-process (lambda (a b) (gimme-eval-all-sexps b))))

(defun gimme-send-message (&rest args)
  "Formats the arguments using (format) then sends the resulting string to the process."
  (let* ((message (apply #'format args))
         (message (replace-regexp-in-string "%" "%%" message)))
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
  (let ((plist (plist-put plist 'font-lock-face nil)))
    (eval `(let ((plist ',plist)
                 ,@(mapcar (lambda (n) (list (intern (format "%%%s" (car n)))
                                        (if (and (symbolp (cdr n)) (not (null (cdr n))))
                                            (list 'quote (cdr n)) (cdr n))))
                           (plist-to-alist plist)))
             (eval (car gimme-playlist-formats))))))


(defun gimme-set-title (title)
  "Changes the header of a buffer"
  (setq header-line-format
        `(:eval (substring (decode-coding-string ,title 'utf-8)
                           (min (length ,title)
                                (window-hscroll))))))

(defun gimme-toggle-view ()
  "Cycle through the views defined in gimme-config"
  (interactive)
  (setq gimme-playlist-formats
        (append (cdr gimme-playlist-formats)
                (list (car gimme-playlist-formats))))
  ;; Won't work with the macro because of the (goto (point-max))
  (unlocking-buffer
   (save-excursion
     (let* ((pos (point)) (line (line-number-at-pos))
            (data (range-to-plists (point-min) (point-max)))
            (data (mapcar (lambda (n) (gimme-string (plist-put n 'font-lock-face nil))) data))
            (len (length data)))
       ;; Silly but required so that the cursor won't change its
       ;; position after killing text
       (goto-char (point-min))
       (loop for n from 0 upto (- line 2) doing (insert (nth n data)))
       (move-beginning-of-line 1) (kill-line (- line 2))
       (kill-region (point) (point-max))
       (loop for n from (- line 1) upto (1- len) doing (insert (nth n data)))
       (goto-line line)))))

(defun gimme-restart ()
  (setq gimme-filter-remainder "")
  (gimme-init)
  (gimme-send-message (format "(set_atribs %s)\n" (gimme-extract-needed-tags))))

(defun gimme ()
  "The XMMS2 interface we all love"
  (interactive)
  (gimme-restart)
  (gimme-playlist))

;;;;;;;;;;
;; Init ;;
;;;;;;;;;;

(gimme-generate-commands clear shuffle play pause next prev stop toggle current)
(require 'gimme-utils)
(require 'gimme-playlist)
(require 'gimme-bookmark)
(require 'gimme-filter)
(require 'gimme-status-mode)
(require 'gimme-custom)
(require 'gimme-etc)
(require 'gimme-tagwriter)
(require 'gimme-lyrics)
(require 'gimme-inspect)
(provide 'gimme)

;;; gimme.el ends here

