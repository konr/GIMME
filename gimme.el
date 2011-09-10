;;; gimme.el --- GIMME Interesting Music on My Emacs

;; Author: Konrad Scorciapino <konr@konr.mobi>
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

;; GIMME works by using collections as search results, and its
;; multiple views allows you to do that in multiple ways. Check out
;; more at http://gimmeplayer.org

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables used in GIMME ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gimme-process nil           "Reference to the ruby process")
(defvar gimme-executable "gimme.rb" "The name of the ruby file")
(defvar gimme-filter-remainder ""   "Variable used to hold incomplete sexps received from the ruby process")

(defvar gimme-fullpath (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) gimme-executable)) "The fullname of the ruby file")
(defvar gimme-bookmark-minimal-collection-list '(((0 ("reference" "All Media" "title" "All Media") nil))) "The minimal collection you must have access to, which is just the Universe collection.")
(defvar gimme-anonymous-collections gimme-bookmark-minimal-collection-list "The list to which the collections you create while explore mlib will go.")

(defvar gimme-playtime nil               "Variable used to hold the current track's duration and playtime")
(defvar gimme-mlib-cache-plist nil       "Contains cached information on the current collection")
(defvar gimme-mlib-cache-global nil      "Contains cached information on the universal collection")

(defvar gimme-autocomplete-prompt "> "                 "The prompt an user gets when asking for completion. Internal variable used to avoid passing a needed parameter to every function around")
(defvar gimme-autocomplete-done-with-autocompletions t "Internal variable, set when the user stops tabbing to autocomplete")
(defvar gimme-autocomplete-current-coll nil            "Internal variable, used to avoid passing a needed parameter to every function around.")

(defvar gimme-augmented-info-buffer-name "GIMME - Information on" "Prefix of the buffer used to show information of an artist.")
(defvar gimme-autocomplete-buffer-name "*GIMME Completions*"      "The name of the buffer used to show possible candidates for completion.")
(defvar gimme-bookmark-name "GIMME - Bookmarks"                   "The name of the buffer used to show your collections.")
(defvar gimme-inspect-buffer-name "GIMME - Inspect"               "The name of the buffer used to inspect hashes")
(defvar gimme-tagwriter-buffer-name "GIMME - Tagwriter"           "The name of the buffer used to write tags in a fancier way")

(defvar gimme-bookmark-facets
  '("genre" "artist" "album" "timesplayed") "The facets of the collections that are worth displaying.")
(defvar gimme-inspect-max-length 50         "Maximum length of the string that will be used in every field of gimme-inspect")
(defvar gimme-tagwriter-max-length 33       "Maximum length of the string that will be used in every field of gimme-tagwriter")

(defvar gimme-debug 0
  "To debug.
  0: Do nothing extra
  1: Prints the functions being called by the ruby process
  2: Print the whole sexps
  3: Print the whole sexps and do not evaluate them")

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun gimme-buffers (&optional function)
  "Get all buffers used by GIMME. FIXME: mode"
  (let* ((modes '(gimme-playlist-mode gimme-collection-mode gimme-bookmark-mode))
         (buffers (remove-if-not (lambda (buf) (member (major-mode buf) modes))
                                 (buffer-list)))
         (filtered (remove-if-not function buffers)))
    filtered))

(defun gimme-first-buffer-with-vars (&rest plist)
  "Gets a GIMME buffer matching the variables of a plist"
  (let* ((all (gimme-buffers)) (alist (plist-to-alist plist))
         (consed (mapcar (lambda (el) (cons el (buffer-local-variables el))) all)))
    (caar (reduce (lambda (coll pair)
                    (remove-if-not (lambda (el) (equal (cdr pair)
                                                  (cdr (assoc (car pair) (cdr el)))))
                                   coll)) alist :initial-value consed))))

(defun gimme-gen-buffer (plist)
  "Generates a Buffer and sets the variables correctly"
  (let* ((type (getf plist 'gimme-buffer-type))
         (type-s (case type ('collection "Collection") ('playlist "Playlist")
                       ('lyrics "Lyrics")))
         (name-s (case type
                   ('collection (getf plist 'gimme-collection-title))
                   ('playlist (getf plist 'gimme-playlist-name))
                   ('lyrics (getf plist 'title))))
         (buffer-name (decode-coding-string
                       (format "GIMME - %s (%s)" type-s name-s) 'utf-8))
         (facet (plist-get plist 'gimme-collection-facet)))
    (gimme-on-buffer buffer-name
                     (setq-local formats gimme-playlist-formats)
                     (kill-local-variable 'gimme-collection-facet) ;; FIXME lousy
                     (case type
                       ('playlist (gimme-playlist-mode))
                       ('collection (gimme-collection-mode facet)))
                     (delete-region 1 (point-max))
                     (when facet (insert (format "Collection: %s\nFacet: %s\n---\n\n"
                                                 (propertize name-s 'font-lock-face `(:foreground ,(color-for name-s)))
                                                 (propertize facet 'font-lock-face `(:weight bold)))))
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
  (setq gimme-playtime `((time . ,time) (max . ,(if (numberp max) max time)))))

(defmacro gimme-on-buffer (name &rest body)
  "Macro that does all the nasty things required to modify a buffer"
  `(with-current-buffer (get-buffer-create ,name)
     (unlocking-buffer (save-excursion (goto-char (point-max)) ,@body))))

(defun gimme-init ()
  "Creates the buffer and manages the processes"
  (dolist (proc (remove-if-not (lambda (el) (string-match "GIMME" el))
                               (mapcar #'process-name (process-list))))
    (kill-process proc))
  (setq gimme-process
        (start-process-shell-command
         "GIMME" nil
         (format "ruby %s" gimme-fullpath )))
  (set-process-filter gimme-process (lambda (a b) (gimme-eval-all-sexps b))))

(defun gimme-send-message (&rest args)
  "Formats the arguments using (format) then sends the resulting string to the process."
  (let* ((message (apply #'format args))
         (message (replace-regexp-in-string "%" "%%" message)))
    (when (> gimme-debug 0) (message message))
    (process-send-string gimme-process message)))

(defun gimme-string (plist)
  "Receives a song represented as a plist and binds each key as %key to be used by the formatting functions at gimme-playlist-formats"
  (let ((plist (plist-put plist 'font-lock-face nil)))
    (eval `(let ((plist ',plist)
                 ,@(mapcar (lambda (n) (list (intern (format "%%%s" (car n)))
                                        (if (and (symbolp (cdr n)) (not (null (cdr n))))
                                            (list 'quote (cdr n)) (cdr n))))
                           (plist-to-alist plist)))
             (eval (car formats))))))

(defun gimme-coll-overview (name data)
  "Caches the data present in a collection."
  (if name (setq gimme-mlib-cache-plist (plist-put gimme-mlib-cache-plist name data))
    (setq gimme-mlib-cache-global data)))

(defun gimme-try-the-best-to-ensure-fancy-features ()
  "Makes sure that GIMME will be able to do at least a general-scoped autocompletion and that the help features will be loaded."
  (unless gimme-mlib-cache-global (gimme-send-message "(coll_overview)\n"))
  (gimme-send-message "(get_help_info)\n"))

(defun gimme-toggle-view ()
  "Cycle through the views defined in gimme-config."
  (interactive)
  (setq-local formats (append (cdr formats) (list (car formats))))
  ;; Won't work with the macro because of the (goto (point-max))
  (unlocking-buffer
   (save-excursion
     (let* ((pos (point)) (line (line-number-at-pos))
            (data (range-to-plists (point-min) (point-max)))
            (data (mapcar (lambda (n) (gimme-string (plist-put n 'font-lock-face nil))) data))
            (len (length data)))
       ;; Silly but required so that the cursor won't change its
       ;; position after killing text.
       (goto-char (point-min))
       (loop for n from 0 upto (- line 2) doing (insert (nth n data)))
       (move-beginning-of-line 1) (kill-line (- line 2))
       (delete-region (point) (point-max))
       (loop for n from (- line 1) upto (1- len) doing (insert (nth n data)))
       (goto-line line)))))

(defun gimme-make-basic-map ()
  "Generates a map with common, basic functionalities."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   'kill-current-buffer)
    (define-key map (kbd "j")   'next-line)
    (define-key map (kbd "k")   'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    (define-key map (kbd "?")   'gimme-help-show-keybindings)
    (define-key map (kbd "=")   'gimme-increase-volume)
    (define-key map (kbd "+")   'gimme-increase-volume)
    (define-key map (kbd "-")   'gimme-decrease-volume)
    map))

(defun gimme ()
  "The XMMS2 interface we all love."
  (interactive)
  (setq gimme-filter-remainder "")
  (gimme-init)
  (gimme-send-message (format "(set_atribs %s)\n" (gimme-extract-needed-tags)))
  (gimme-playlist)
  (gimme-try-the-best-to-ensure-fancy-features))

;;;;;;;;;;
;; Init ;;
;;;;;;;;;;

(require 'htmlr)

(require 'gimme-augmented)
(require 'gimme-autocomplete)
(require 'gimme-bookmark)
(require 'gimme-collection)
(require 'gimme-emacs)
(require 'gimme-eq)
(require 'gimme-custom)
(require 'gimme-help)
(require 'gimme-inspect)
(require 'gimme-playlist)
(require 'gimme-status-mode)
(require 'gimme-tagwriter)
(require 'gimme-utils)

(provide 'gimme)
;;; gimme.el ends here

