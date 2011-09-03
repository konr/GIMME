;;; gimme-collection.el --- GIMME's filter-view

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

;; These are the functions used to operate on collections, both when
;; viewed as a list of songs and when faceted.

;;; Code

(defun gimme-collection ()
  "Sets up the buffer. FIXME: Should be implemented in a more robust way."
  (interactive)
  (let* ((buffer-name "GIMME - Collection (All media)")
         (buffer (car (remove-if-not (lambda (x) (string= (buffer-name x) buffer-name)) (buffer-list)))))
    (if buffer (switch-to-buffer buffer) (gimme-send-message "(pcol)\n"))))

(defvar gimme-collection-map
  (let ((map (gimme-make-basic-map)))
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "<") 'gimme-parent-col)
    (define-key map (kbd ">") 'gimme-child-col)
    (define-key map (kbd "a") 'gimme-collection-append-focused)
    (define-key map (kbd "RET") 'gimme-collection-play-focused)
    (define-key map (kbd "A") 'gimme-collection-append-current-collection)
    (define-key map (kbd "f") 'gimme-collection-same)
    (define-key map (kbd "!") 'gimme-collection-toggle-faceted)
    map)
  "Filter-view's keymap")

(defvar gimme-faceted-map
  (let ((map (gimme-make-basic-map)))
    (define-key map (kbd "TAB") 'gimme-faceted-change-facet)
    (define-key map (kbd "<backtab>") 'gimme-faceted-change-facet-to-prev)
    (define-key map (kbd "<") 'gimme-parent-col-with-facets)
    (define-key map (kbd ">") 'gimme-child-col-with-facets)
    (define-key map (kbd "RET") 'gimme-faceted-subcol)
    (define-key map (kbd "C-M-S-<return>") 'gimme-collection-append-current-collection)
    (define-key map (kbd "S-<return>") 'gimme-faceted-subcol-append)
    (define-key map (kbd "A") 'gimme-faceted-subcol-append)
    (define-key map (kbd "!") 'gimme-collection-toggle-faceted)
    (define-key map (kbd "T") 'gimme-faceted-change-tags-of-subcol)
    map)
  "Keymap for browsing collections in a faceted way")


(defun gimme-collection-mode (&optional facet)
  "Manipulate collections"
  (interactive)
  (font-lock-mode t)
  (use-local-map (if facet gimme-faceted-map gimme-collection-map))
  (setq-local groups 0)
  (setq truncate-lines t)
  (setq major-mode 'gimme-collection-mode mode-name "gimme-collection"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-child-col-with-facets ()
  "Creates a new collection intersecting the search criteria and the current collection and displays it with facets"
  (interactive) (gimme-child-col t))

(defun gimme-parent-col-with-facets ()
  "Jumps to the current collection's parent collection and displays it with facets."
  (interactive) (gimme-parent-col t))

(defun gimme-child-col (&optional faceted)
  "Creates and displays a new collection intersecting the search criteria and the current collection"
  (interactive)
  (let* ((parent gimme-collection-name)
         (name (gimme-autocomplete-prompt (format "%s > " gimme-collection-title) parent))
         (message (format "(%s %s %s)\n" (if faceted "faceted_subcol" "subcol")
                          (hyg-prin1 parent) (hyg-prin1 name))))
    (gimme-send-message message)))

(defun gimme-parent-col (&optional faceted)
  "Jumps to the current collection's parent collection."
  (interactive)
  (let* ((message (format "(supcol %s %s)\n" (hyg-prin1 gimme-collection-name) (if faceted "t" ""))))
    (gimme-send-message message)))

(defun gimme-collection-append-focused ()
  "Appends to the current playlist the focused song"
  (interactive)
  (gimme-send-message "(add %s)\n" (get-text-property (point) 'id)))

(defun gimme-collection-play-focused ()
  "Appends to the current playlist the focused song and then play it"
  (interactive)
  (gimme-send-message "(addplay %s)\n" (get-text-property (point) 'id)))

(defun gimme-collection-append-collection ()
  "Appends to the current playlist the entire collection"
  (interactive)
  (message "Appending songs to the playlist...")
  (dolist (el (range-to-plists (point-min) (point-max)))
    (gimme-send-message (format "(add %d)\n" (getf el 'id)))))

(defun gimme-collection-same ()
  "Creates a subcollection matching some this song's criteria"
  (interactive)
  (let* ((parent (gimme-bookmark-current-ref))
         (name (completing-read
                "Filter? "
                (mapcar (lambda (n) (format "%s:%s"
                                       (car n) (hyg-prin1 (decode-coding-string (cdr n) 'utf-8))))
                        (remove-if (lambda (m) (member (car m) '(id duration font-lock-face)))
                                   (plist-to-alist (text-properties-at (point)))))))
         (message (format "(subcol %s %s)\n" parent (hyg-prin1 name))))
    (gimme-send-message message)))


(defun gimme-faceted-change-facet-to-prev ()
  "Shows the current collection through the previous facet."
  (interactive)
  (gimme-faceted-change-facet t))

(defun gimme-faceted-change-facet (&optional prev-p)
  "Shows the current collection through the next facet."
  (interactive)
  (let* ((coll gimme-collection-name)
         (facet (gimme-toggle-facet t prev-p))
         (message (format "(faceted_pcol %s %s)\n" (hyg-prin1 coll) (hyg-prin1 facet))))
    (when coll (gimme-send-message message))))

(defun gimme-faceted-subcol-append ()
  "Appends to the playlist a sub-collection containing the current group of the collection."
  (interactive) (gimme-faceted-subcol t))

(defun gimme-faceted-subcol (&optional append)
  "Eithers displays or appends to the playlist a sub-collection containing the current group of the collection."
  (interactive)
  (let* ((parent gimme-collection-name)
         (key gimme-collection-facet)
         (val (get-text-property (point) 'data))
         (pattern (format "%s:'%s'" key val))
         (message (format "(%s %s %s)\n" (if append "append_subcol" "faceted_subcol")
                          (hyg-prin1 parent) (hyg-prin1 pattern))))
    (when val (gimme-send-message message))))

(defun gimme-collection-toggle-faceted ()
  "Toggles between displaying the current collection as a list of tracks and faceted."
  (interactive)
  (let* ((facet (if (boundp 'gimme-collection-facet) nil (car gimme-bookmark-facets)))
         (function (if facet "faceted_pcol" "pcol"))
         (coll gimme-collection-name))
    (gimme-send-message "(%s %s)\n" function (hyg-prin1 coll))))

(defun gimme-collection-append-current-collection ()
  "Appends current collection to the playlist"
  (interactive)
  (gimme-send-message  "(append_coll %s)\n" (hyg-prin1 gimme-collection-name)))

(defun gimme-faceted-change-tags-of-subcol ()
  "Changes the current group's value of the tag used as facet to another thing."
  (interactive)
  (let* ((coll (hyg-prin1 gimme-collection-name))
         (subcol (hyg-prin1 (get-text-property (point) 'data)))
         (key (hyg-prin1 gimme-collection-facet))
         (val (hyg-prin1 (completing-read-with-whitespace
                          (format "Change %s to: " subcol) (gimme-faceted-collect-subcols)))))
    (gimme-send-message "(subcol_change_tags %s %s %s %s)\n" coll subcol key val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-faceted-collect-subcols ()
  "Auxiliary function used to collect all the possible values of the current tag/facet"
  (save-excursion
    (goto-char (point-min))
    (next-line 3)
    (loop for i upto (1- groups) doing (next-line)
          collecting (get-text-property (point) 'data))))


(defun gimme-faceted-insert-group (buffer key val)
  "Inserts into the buffer a line that represents a group of tracks with the same value of the current facet"
  (when buffer
    (let ((buffer (if (or (bufferp buffer) (stringp buffer)) buffer (apply #'gimme-first-buffer-with-vars buffer))))
      (gimme-on-buffer
       buffer
       (setq-local groups (1+ groups))
       (goto-char (point-max))
       (insert (propertize (format "%s [%s]\n" key val) 'font-lock-face `(:foreground ,(color-for key)) 'data key))))))


(provide 'gimme-collection)
;;; gimme-collection.el ends here
