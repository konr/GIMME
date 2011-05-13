;;; gimme-tree.el --- GIMME's tree-view

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

(defun gimme-tree ()
  "Tree-view"
  (interactive)
  (let ((buffer-name "GIMME - Bookmarks"))
    (gimme-on-buffer buffer-name
                     (kill-region 1 (point-max))
                     (gimme-tree-read-from-disk)
                     (gimme-tree-mode)
                     (gimme-set-title gimme-tree-header))
    (gimme-send-message "(colls %s)\n" (prin1-to-string buffer-name))
    (run-hooks 'gimme-goto-buffer-hook)
    (switch-to-buffer (get-buffer buffer-name))))

(defvar gimme-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'gimme-filter)
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map (kbd "SPC") 'gimme-toggle-collection)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") (lambda () (interactive) (gimme-vol gimme-vol-delta)))
    (define-key map (kbd "+") (lambda () (interactive) (gimme-vol gimme-vol-delta)))
    (define-key map (kbd "-") (lambda () (interactive) (gimme-vol (- gimme-vol-delta))))
    (define-key map (kbd "RET") 'gimme-tree-view-collection)
    (define-key map (kbd "d") 'gimme-tree-delete-coll)
    (define-key map (kbd "r") 'gimme-tree-rename-coll)
    (define-key map (kbd "s") 'gimme-tree-save-collection)
    (define-key map (kbd "S") '(lambda () (interactive) (gimme-tree-save-collection t)))
    map)
  "Tree-map's keymap")

(define-derived-mode gimme-tree-mode font-lock-mode
  ;; FIXME: Find out why deriving from font-lock-face won't colorize the the songs
  (use-local-map gimme-tree-map)
  (setq major-mode 'gimme-tree-mode)
  (font-lock-add-keywords nil
                          '(("^\\* .*\n"                         . 'gimme-tree-level-1)
                            ("^\\*\\* .*\n"                      . 'gimme-tree-level-2)
                            ("^\\*\\*\\* .*\n"                   . 'gimme-tree-level-3)
                            ("^\\*\\*\\*\\* .*\n"                . 'gimme-tree-level-4)
                            ("^\\*\\*\\*\\*\\* .*\n"             . 'gimme-tree-level-5)
                            ("^\\*\\*\\*\\*\\*\\* .*\n"          . 'gimme-tree-level-6)
                            ("^\\*\\*\\*\\*\\*\\*\\* .*\n"       . 'gimme-tree-level-7)
                            ("^\\*\\*\\*\\*\\*\\*\\*\\** .*\n"   . 'gimme-tree-level-8)))
  (setq mode-name "gimme-tree"))



(defun gimme-tree-colls (session list)
  "Prints the available collections as a tree"
  (message "xoxota")
  (let* ((list (remove-if (lambda (n) (member n '("Default" "_active"))) list))
         (list (mapcar (lambda (n) (decode-coding-string n 'utf-8)) list)))
    (gimme-on-buffer session
                     (dolist (el (gimme-tree-get-trees)) (insert el))
                     (insert (format "\n* Saved collections\n"))
                     (dolist (el list) (insert (propertize (format "** %s\n" el) 'ref el))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tree-view-collection ()
  "Jumps to filter-view with the focused collection as the current"
  (interactive)
  (cond ((get-text-property (point) 'pos)
         (setq gimme-current (get-text-property (point) 'pos))
         (gimme-filter))
        ((get-text-property (point) 'ref)
         (setq gimme-current (get-text-property (point) 'ref))
         (gimme-filter))
        (gimme-current)))

(defun gimme-tree-delete-coll ()
  "Deletes focused collection"
  (interactive)
  (if (get-text-property (point) 'pos)
      (let* ((initial (point))
             (pos (get-text-property (point) 'pos))
             (parent (gimme-tree-get-node (butlast pos)))
             (el (gimme-tree-get-node pos))
             (bounds (get-bounds-where
                      (lambda (n) (sublistp pos (get-text-property n 'pos))))))
        (gimme-tree-delete-from-tree pos)
        (unlocking-buffer (save-excursion (dolist (b (reverse bounds))
                                            (apply #'kill-region b)))))
    (when (get-text-property (point) 'ref)
      (gimme-send-message "(dcol %s)\n"
                          (prin1-to-string (get-text-property (point) 'ref))))))


(defun gimme-tree-rename-coll ()
  "Renames the focused coll"
  (interactive)
  (if (get-text-property (point) 'pos)
      (let* ((old  (get-text-property (point) 'name))
             (new  (read-from-minibuffer "New name: "))
             (pos  (get-text-property (point) 'pos))
             (node (gimme-tree-get-node pos))
             (data (plist-put (car node) 'name new))
             (bounds (list (line-beginning-position) (line-end-position))))
        (setcar node data)
        (unlocking-buffer
         (save-excursion
           (apply #'kill-region bounds)
           (goto-char (car bounds))
           (insert (apply #'propertize
                          (format "%s %s" (make-string (length pos) ?*) new)
                          (plist-put data 'pos pos)))))
        (gimme-tree-write-to-disk))
    (when (get-text-property (point) 'ref)
      (gimme-send-message "(rcol %s %s)\n"
                          (prin1-to-string (get-text-property (point) 'ref))
                          (prin1-to-string (read-from-minibuffer "New name: "))))))


(defun gimme-tree-save-collection (&optional ask)
  "Saves the focused collection on the core"
  (interactive)
  (let* ((ref  (get-text-property (point) 'ref))
         (name (get-text-property (point) 'name))
         (name (if ask (read-from-minibuffer "Save as: ") name)))
    (when ref (gimme-send-message (format "(scol %s %s)\n"
                                          (prin1-to-string ref)
                                          (prin1-to-string name))))))
;;;;;;;;;
;; Aux ;;
;;;;;;;;;
;;
;; Tree is like (plist child1 child2 ...)

(defun gimme-tree-get-node (position)
  "Gets the node - (plist child1 child2 ...) - referenced by the arg"
  (when (listp position)
    (loop for pos = position then (cdr pos)
          and tree = gimme-trees then (nth (car pos) tree)
          while pos
          finally return tree)))

(defun gimme-tree-delete-from-tree (pos)
  "Deletes the node referenced by the argument"
  (delete (gimme-tree-get-node pos)
          (gimme-tree-get-node (butlast pos)))
  (gimme-tree-write-to-disk))

(defun gimme-tree-add-child (data position)
  "A new child at the given position"
  (let* ((tree (gimme-tree-get-node position))
         (len (length tree))
         (position (if (stringp position) nil position)))
    (nconc tree `((,data)))
    (gimme-tree-write-to-disk)
    len))

(defun gimme-tree-current-ref ()
  "Returns either a string or an idlist"
  (if (listp gimme-current)
      (getf (car (gimme-tree-get-node gimme-current)) 'ref)
    (prin1-to-string gimme-current)))

(defun gimme-tree-current-data ()
  "Gets the plist of the current collection"
  (car (gimme-tree-get-node gimme-current)))

(defun gimme-tree-walk (function tree &optional depth mapcarp)
  "Walks on the tree collecting the funcall. Note that you can use the depth variable."
  (let ((depth (if depth depth 0)))
    (if (null tree) nil
      (cons (funcall function tree)
            (funcall (if mapcarp #'mapcar #'mapcan)
                     (lambda (n) (gimme-tree-walk function n (1+ depth) mapcarp))
                     (cdr tree))))))

(defun gimme-tree-get-trees ()
  "Prints gimme-trees with the right number of asterisks."
  (cdr (gimme-tree-walk (lambda (n)
                          (apply 'propertize (format "%s %s\n"
                                                     (make-string depth ?*)
                                                     (getf (car n) 'name)) (car n)))
                        (gimme-tree-add-pos gimme-trees))))

(defun gimme-tree-add-pos (tree &optional pos)
  "Returns a tree with a 'pos' attribute on the plist"
  (loop for p from 1 upto (1- (length tree))
        collecting (gimme-tree-add-pos (nth p tree) (append pos (list p)))
        into children
        finally return (cons (append (car tree) `(pos ,pos)) children)))

(defun gimme-tree-read-from-disk ()
  "Reads from disk :P"
  (if (file-readable-p gimme-tree-file)
      (setq gimme-trees
            (with-temp-buffer (insert-file-contents gimme-tree-file)
                              (read (buffer-string))))
    (let ((new '((name "All" ref "\"*\""))))
      (gimme-tree-write-to-disk new)
      (gimme-tree-read-from-disk))))


(defun gimme-tree-write-to-disk (&optional tree)
  "Writes to disk :P"
  (write-region (prin1-to-string (or tree gimme-trees)) nil
                gimme-tree-file))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-coll-changed (plist)
  "Called by the braodcast functions"
  (comment with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (let ((type      (getf plist 'type))
           (name      (decode-coding-string (getf plist 'name) 'utf-8))
           (namespace (getf plist 'namespace))
           (newname   (decode-coding-string (or (getf plist 'newname) "") 'utf-8)))
       (case type
         ('add (run-hook-with-args 'gimme-broadcast-coll-add-hook plist)
               (save-excursion
                 (goto-char (point-max))
                 (insert (propertize (format "** %s\n" name) 'ref name))
                 (message (format "Collection %s added!" name))))
         ('update (progn (run-hook-with-args 'gimme-broadcast-coll-add-hook plist)
                         (message (format "Collection %s updated!" name))))
         ('rename
          (let ((bounds (car (get-bounds-where
                              (lambda (n) (equal (get-text-property n 'ref) name))))))
            (run-hook-with-args 'gimme-broadcast-coll-rename-hook plist)
            (when bounds
              (apply 'kill-region bounds)
              (save-excursion (goto-char (car bounds))
                              (insert (propertize (format "** %s\n" newname) 'ref newname)))
              (message (format "Collection %s renamed to %s!" name newname)))))
         ('remove
          (progn (run-hook-with-args 'gimme-broadcast-coll-remove-hook plist)
                 (apply 'kill-region
                        (car (get-bounds-where
                              (lambda (n) (equal (get-text-property n 'ref) name)))))
                 (message (format "Collection %s removed!" name)))))))))

(require 'gimme-tree-faces)
(provide 'gimme-tree)
;;; gimme-tree.el ends here
