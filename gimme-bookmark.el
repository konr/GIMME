;;; gimme-bookmark.el --- GIMME's bookmark-view

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

;; gimme-bookmark shows the collections you currently have, both those
;; stored in the XMMS2 server, the "Saved Collections" and the
;; transient ones you create when searching, the "History".

;;; Code

(defun gimme-bookmark ()
  "Goes to bookmark-mode, which shows the collections you current have."
  (interactive)
  (gimme-send-message "(colls %s)\n" (hyg-prin1 gimme-bookmark-name)))

(defvar gimme-bookmark-map
  (let ((map (gimme-make-basic-map)))
    (define-key map (kbd "TAB") 'gimme-toggle-facet)
    (define-key map (kbd "RET") 'gimme-bookmark-view-collection-with-facets)
    (define-key map (kbd ">")  'gimme-child-coll-of-current)
    (define-key map (kbd "S-<return>") 'gimme-bookmark-view-collection )
    (define-key map (kbd "SPC") 'gimme-bookmark-toggle-highlighting)
    (define-key map (kbd "d") 'gimme-bookmark-delete-coll)
    (define-key map (kbd "r") 'gimme-bookmark-rename-coll)
    (define-key map (kbd "S") 'gimme-bookmark-save-collection)
    (define-key map (kbd "a") 'gimme-bookmark-combine-collections)
    (define-key map (kbd "A") 'gimme-bookmark-append-to-playlist)
    map)
  "bookmark-map's keymap")

(define-derived-mode gimme-bookmark-mode fundamental-mode
  "Used on GIMME" ""
  (use-local-map gimme-bookmark-map)
  (setq mode-name "gimme-bookmark"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-toggle-facet (&optional silent backwards)
  "Toggles the default facet used when displaying a new collection."
  (interactive)
  (setq gimme-bookmark-facets (if backwards (append (last gimme-bookmark-facets) (butlast gimme-bookmark-facets))
                                (append (cdr gimme-bookmark-facets) (list (car gimme-bookmark-facets)))))
  (unless silent
    (message "(%s)%s" (car gimme-bookmark-facets) (apply #'concat (mapcar (lambda (x) (format " %s" x)) (cdr gimme-bookmark-facets)))))
  (car gimme-bookmark-facets))

(defun gimme-child-coll-of-current ()
  "Generates a child collection, containing an intersection of what you currently have a pattern given."
  (interactive)
  (let* ((props (text-properties-at (point)))
         (coll (or (plist-get props 'coll) (plist-get props 'ref)))
         (name (gimme-autocomplete-prompt "> " coll))
         (message (format "(faceted_subcol %s %s)\n" (hyg-prin1 coll)
                          (hyg-prin1 name))))
    (gimme-send-message message)))

(defun gimme-bookmark-append-to-playlist ()
  "Appends the currently focused collection to the playlist."
  (interactive)
  (let ((coll (or (get-text-property (point) 'coll)
                  (get-text-property (point) 'ref))))
    (when coll (gimme-send-message  "(append_coll %s)\n" (hyg-prin1 coll)))))

(defun gimme-bookmark-toggle-highlighting ()
  "Highlights a collection, to be used by `gimme-bookmark-combine-collections'"
  (interactive)
  (when (or (get-text-property (point) 'coll) (get-text-property (point) 'ref))
    (unlocking-buffer
     (save-excursion
       (let* ((beg (progn (beginning-of-line) (point)))
              (end (progn (end-of-line) (1+ (point))))
              (face (get-text-property beg 'face)))
         (if (not (equal face 'highlight))
             (progn (put-text-property beg end 'oldface face)
                    (put-text-property beg end 'face 'highlight))
           (put-text-property beg end 'face
                              (get-text-property beg 'oldface))))))))

(defun gimme-bookmark-combine-collections ()
  "Combine the highlighted collections with a logic operation, such as AND or OR"
  (interactive)
  (let* ((colls (get-bounds-where
                 (lambda (x) (equal (get-text-property x 'face) 'highlight))))
         (data (mapcar (lambda (x) (or (get-text-property (car x) 'coll)
                                  (get-text-property (car x) 'ref))) colls))
         (as-strings (mapcar (lambda (x) (format " %s" (hyg-prin1 x))) data)))
    (if (= (length colls) 0) (message "No collections selected!")
      (let* ((ops (if (= (length as-strings) 1) '("not") '("and" "or")))
             (op (completing-read "Combine with? " ops)))
        (if (member op ops)
            (gimme-send-message "(combine %s () (%s))\n" (hyg-prin1 op)
                                (apply #'concat as-strings))
          (message "Invalid operation!"))))))


(defun gimme-bookmark-view-collection-with-facets ()
  "Jumps to faceted-view with the focused collection as the current collection."
  (interactive)
  (gimme-bookmark-view-collection t))

(defun gimme-bookmark-view-collection (&optional faceted-p)
  "Jumps to collection-view with the focused collection as the current collection."
  (interactive)
  (let* ((coll (hyg-prin1 (or (get-text-property (point) 'coll) (get-text-property (point) 'ref))))
         (facet (car gimme-bookmark-facets))
         (message (if faceted-p (format "(faceted_pcol %s)\n" coll)
                    (format "(pcol %s)\n" coll))))
    (when coll 
      (gimme-send-message message))))

(defun gimme-bookmark-delete-coll ()
  "Deletes focused collection"
  (interactive)
  (let* ((coll (get-text-property (point) 'coll))
         (ref (get-text-property (point) 'ref))
         (buffer (get-buffer gimme-bookmark-name)))
    (if coll
        (let* ((elements (gimme-bookmark-get-children coll t))
               (bounds
                (mapcar (lambda (x) (car (get-bounds-where
                                     (lambda (y) (equal x (get-text-property y 'coll))))))
                        elements))
               (bounds (reverse bounds)))
          (setq gimme-anonymous-collections (gimme-delete-collection coll))
          (when buffer
            (gimme-on-buffer buffer
                             (dolist (x bounds) (delete-region (car x) (cadr x))))))
      (when ref (gimme-send-message "(dcol %s)\n" (hyg-prin1 ref)))))
  (unless gimme-anonymous-collections
    (setq gimme-anonymous-collections gimme-bookmark-minimal-collection-list)))

(defun gimme-bookmark-rename-coll ()
  "Renames the focused coll"
  (interactive)
  (if (get-text-property (point) 'pos)
      (let* ((old  (get-text-property (point) 'name))
             (new  (read-from-minibuffer "New name: "))
             (pos  (get-text-property (point) 'pos))
             (node (gimme-bookmark-get-node pos))
             (data (plist-put (car node) 'name new))
             (bounds (list (line-beginning-position) (line-end-position))))
        (setcar node data))
    (when (get-text-property (point) 'ref)
      (gimme-send-message "(rcol %s %s)\n"
                          (hyg-prin1 (get-text-property (point) 'ref))
                          (hyg-prin1 (read-from-minibuffer "New name: "))))))


(defun gimme-bookmark-save-collection ()
  "Saves the focused collection on the core"
  (interactive)
  (let* ((coll  (get-text-property (point) 'coll))
         (name (read-from-minibuffer "Save as: ")))
    (when coll (gimme-send-message (format "(savecol %s %s)\n"
                                           (hyg-prin1 coll)
                                           (hyg-prin1 name))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-bookmark-colorize (text)
  "Colorizes a string that starts with asterisks, as org-mode headers."
  (let ((asterisks (replace-regexp-in-string "^\\(\*+\\).*" "\\1" text)))
    (propertize text 'font-lock-face `(:foreground ,(color-for asterisks)))))

(defun gimme-dfs-on-colls (function &optional colls)
  "Function must have 2 arguments: The collection on a list with its children and
   the children of the parent collection"
  (loop for coll in (or colls gimme-anonymous-collections)
        collecting `(,(funcall function coll colls)
                     ,@(when (cdr coll) (gimme-dfs-on-colls function (cdr coll))))))

(defun gimme-delete-collection (target &optional colls)
  "<thunk> konr: Well, *sometimes* [delete] deletes by side effect.  In this
           case it's probably just returning the cdr of seq."
  (loop for coll in (or colls gimme-anonymous-collections)
        if (not (equal (car coll) target)) collect
        `(,(car coll) ,@(when (cdr coll)
                          (gimme-delete-collection target (cdr coll)))) end))


(defun gimme-bookmark-dfsmap (fun &optional colls flat)
  "Maps via depth-first search."
  (loop for coll in (or colls gimme-anonymous-collections)
        collecting `(,(funcall fun coll)
                     ,@(when (cdr coll) (gimme-bookmark-dfsmap fun (cdr coll) flat)))
        into elements and finally return
        (if flat (apply #'append elements) elements)))

(defun gimme-bookmark-get-parents (target &optional colls stack)
  "Gets the parents of a collection."
  (loop for coll in (or colls gimme-anonymous-collections)
        if (equal (car coll) target) return stack
        else when (cdr coll) collect
        (gimme-bookmark-get-parents target (cdr coll) (cons (car coll) stack)) into ch
        finally return (car (remove-if #'null ch))))

(defun gimme-bookmark-get-children (target &optional including-self)
  "Gets the children of a collection."
  (let* ((element (list (gimme-bookmark-get-node target gimme-anonymous-collections)))
         (colls (gimme-bookmark-dfsmap (lambda (x) (car x)) element t)))
    (if including-self colls (cdr colls))))

(defun gimme-bookmark-dfs (colls &optional depth)
  "Processes a list of nested collections returning a nice string with everything colored and propertized."
  (let ((depth (or depth 2)))
    (when colls
      (loop for coll in colls collecting
            (concat
             (propertize
              (gimme-bookmark-colorize
               (format "%s %s\n" (make-string depth ?*)
                       ;; FIXME: getf doesn't work with strings :(
                       (loop for x = (cadar coll)
                             then (cddr x) while x
                             if (equal (car x) "title") return (cadr x)
                             finally return "Anonymous collection")))
              'coll (car coll))
             (or (gimme-bookmark-dfs (cdr coll) (1+ depth)) ""))
            into strings and finally return (apply #'concat strings)))))

(defun gimme-bookmark-get-node (node &optional nodes)
  "Gets the full plist of a given node."
  (if (null node) nodes
    (when nodes
      (or (car (remove-if-not (lambda (x) (equal (car x) node)) nodes))
          (gimme-bookmark-get-node node (apply #'append (mapcar #'rest nodes)))))))

(defun gimme-bookmark-add-child (data parent)
  "Adds a child to a parent."
  (let* ((bookmark-buffer (get-buffer gimme-bookmark-name))
         (title (loop for pair = (cadr data) then (cddr pair)
                      while pair if (string= (car pair) "title") return (cadr pair)))
         (parent (or (gimme-bookmark-get-node parent gimme-anonymous-collections)
                     (progn (gimme-bookmark-add-child parent nil)
                            (gimme-bookmark-get-node parent
                                                     gimme-anonymous-collections))))
         (children (or (cdr parent) parent)) (on-coll `((,data))))
    (if gimme-anonymous-collections
        (unless (remove-if-not
                 (lambda (c) (and (listp c) (equal (car c) data))) children)
          (nconc children on-coll))
      (setq gimme-anonymous-collections on-coll))
    (when bookmark-buffer
      (gimme-on-buffer
       bookmark-buffer
       (let* ((pos (car (get-bounds-where
                         (lambda (x) (equal (car parent)
                                       (get-text-property x 'coll))))))
              (ast (when pos (replace-regexp-in-string
                              "^\\(\*+\\).*\n" "\\1*"
                              (buffer-substring (car pos) (cadr pos))))))
         (set-text-properties 0 (length ast) nil ast)
         (when pos
           (goto-char (cadr pos))
           (insert (gimme-bookmark-colorize
                    (propertize (format "%s %s\n" ast title) 'coll data)))))))))

(defun gimme-bookmark-add-pos (bookmark &optional pos)
  "Returns a bookmark with a 'pos' attribute on the plist"
  (loop for p from 1 upto (1- (length bookmark))
        collecting (gimme-bookmark-add-pos (nth p bookmark) (append pos (list p)))
        into children
        finally return (cons (append (car bookmark) `(pos ,pos)) children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-register-coll (coll parent)
  "Adds a collection to `gimme-anonymous-collections'."
  (unless (gimme-bookmark-get-node parent)
    (gimme-bookmark-add-child parent gimme-anonymous-collections))
  (gimme-bookmark-add-child coll parent))

(defun gimme-bookmark-colls (buffer list)
  "Prints the available collections as a tree"
  (let* ((list (remove-if (lambda (n) (member n '("Default" "_active"))) list))
         (list (mapcar (lambda (n) (decode-coding-string n 'utf-8)) list)))
    (gimme-on-buffer buffer
                     (delete-region 1 (point-max))
                     (insert (gimme-bookmark-colorize "* History\n"))
                     (insert (gimme-bookmark-dfs gimme-anonymous-collections))
                     (insert (gimme-bookmark-colorize "\n"))
                     (insert (gimme-bookmark-colorize (format "* Saved collections\n")))
                     (dolist (el list)
                       (insert (gimme-bookmark-colorize
                                (propertize (format "** %s\n" el) 'ref el))))
                     (gimme-bookmark-colorize "\n")
                     (gimme-bookmark-mode)
                     (run-hooks 'gimme-goto-buffer-hook)
                     (switch-to-buffer (get-buffer gimme-bookmark-name)))))

(defun gimme-coll-changed (plist)
  "Called by the braodcast functions"
  (let ((buffer (get-buffer gimme-bookmark-name))
        (type      (getf plist 'type))
        (name      (decode-coding-string (getf plist 'name) 'utf-8))
        (namespace (getf plist 'namespace))
        (newname   (decode-coding-string (or (getf plist 'newname) "") 'utf-8)))
    (when buffer
      (gimme-on-buffer
       buffer
       (case type
         ('add
          (let ((max (cadar (last (get-bounds-where
                                   (lambda (x) (get-text-property x 'ref)))))))
            (progn (run-hook-with-args 'gimme-broadcast-coll-add-hook plist)
                   (and (goto-char max)
                        (insert (gimme-bookmark-colorize
                                 (propertize (format "** %s\n" name) 'ref name)))
                        (message (format "Collection %s added!" name))))))
         ('rename
          (let ((bounds (car (get-bounds-where
                              (lambda (x) (string= name (get-text-property x 'ref)))))))
            (delete-region (car bounds) (cadr bounds))
            (goto-char (car bounds))
            (insert (gimme-bookmark-colorize
                     (propertize (format "** %s\n" newname) 'ref newname)))))
         ('remove
          (let ((bounds (car (get-bounds-where
                              (lambda (x) (string= name (get-text-property x 'ref)))))))
            (delete-region (car bounds) (cadr bounds)))))))))

(provide 'gimme-bookmark)
;;; gimme-bookmark.el ends here
