(defvar gimme-tree-header "GIMME - Tree View")
(defvar gimme-tree-mode-functions
  '(message gimme-update-playtime gimme-tree-colls))
(defvar gimme-current nil)



(defun gimme-tree ()
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'tree)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (clipboard-kill-region 1 (point-max))
     (gimme-tree-mode)
     (gimme-tree-read-from-disk)
     (ignore-errors (viper-change-state-to-emacs)) ;; FIXME: Temporary
     (gimme-set-title gimme-tree-header)
     (gimme-send-message "(colls %s)\n" gimme-session)
     (switch-to-buffer (get-buffer gimme-buffer-name)))))

(defvar gimme-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'gimme-filter)
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map (kbd "SPC") 'gimme-toggle-collection)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") 'gimme-inc_vol) ;; FIXME: Better names, please!
    (define-key map (kbd "+") 'gimme-inc_vol)
    (define-key map (kbd "-") 'gimme-dec_vol)
    (define-key map (kbd "RET") 'gimme-tree-view-collection)
    (define-key map (kbd "d") 'gimme-tree-delete-coll)
    map))
(define-derived-mode gimme-tree-mode font-lock-mode
  ;; FIXME: Find out why deriving from font-lock-face won't colorize the the songs
  (interactive)
  (use-local-map gimme-tree-map)
  (setq major-mode 'gimme-tree-mode)
  (font-lock-add-keywords nil
                          '(("^\\* .*"                         . 'gimme-tree-level-1)
                            ("^\\*\\* .*"                      . 'gimme-tree-level-2)
                            ("^\\*\\*\\* .*"                   . 'gimme-tree-level-3)
                            ("^\\*\\*\\*\\* .*"                . 'gimme-tree-level-4)
                            ("^\\*\\*\\*\\*\\* .*"             . 'gimme-tree-level-5)
                            ("^\\*\\*\\*\\*\\*\\* .*"          . 'gimme-tree-level-6)
                            ("^\\*\\*\\*\\*\\*\\*\\* .*"       . 'gimme-tree-level-7)
                            ("^\\*\\*\\*\\*\\*\\*\\*\\** .*"   . 'gimme-tree-level-8)))
  (setq mode-name "gimme-tree") )

(defun gimme-tree-colls (session list)
  "Prints the available collections as a tree"
  (let* ((list (remove-if (lambda (n) (member n '("Default" "_active"))) list))
         (list (mapcar (lambda (n) (decode-coding-string n 'utf-8)) list)))
    (with-current-buffer gimme-buffer-name
      (unlocking-buffer
       (save-excursion
         (dolist (el (gimme-tree-get-trees)) (insert el))
         (insert (format "\n* Saved collections\n"))
         (dolist (el list) (insert (propertize (format "** %s\n" el) 'ref el))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tree-view-collection ()
  (interactive)
  (cond ((get-text-property (point) 'pos)
         (setq gimme-current (get-text-property (point) 'pos))
         (gimme-filter))
        ((get-text-property (point) 'ref)
         (setq gimme-current (get-text-property (point) 'ref))
         (gimme-filter))
        (gimme-current)))

(defun gimme-tree-delete-coll ()
  (interactive)
  (if (get-text-property (point) 'pos)
      (let* ((initial (point))
             (pos (get-text-property (point) 'pos))
             (parent (gimme-tree-get-node (butlast pos)))
             (el (gimme-tree-get-node pos)))
        (loop for beg = (point-min) then end
              and end = (next-property-change (or beg (point-min)))
              while end
              and when (not (sublistp pos (get-text-property beg 'pos)))
              collect (buffer-substring beg end) into strings end
              finally do
              (unlocking-buffer
               (kill-region (point-min) (point-max))
               (dolist (s strings) (insert s))
               (goto-char initial)))
        (gimme-tree-delete-from-tree pos))
    (when (get-text-property (point) 'ref)
      (gimme-send-message "(dcol \"%s\")" (get-text-property (point) 'ref)))))


;;;;;;;;;
;; Aux ;;
;;;;;;;;;
;;
;; Tree is like (plist child1 child2 ...)

(defun gimme-tree-get-node (position)
  ""
  (loop for pos = position then (cdr pos)
        and tree = gimme-trees then (nth (car pos) tree)
        while pos
        finally return tree))

(defun gimme-tree-delete-from-tree (pos)
  ""
  (delete (gimme-tree-get-node pos) 
          (gimme-tree-get-node (butlast pos)))
  (gimme-tree-write-to-disk))

(defun gimme-tree-add-child (data position)
  (let* ((tree (gimme-tree-get-node position))
         (len (length tree)))
    (nconc tree `((,data)))
    (gimme-tree-write-to-disk)
    len))

(defun gimme-tree-current-ref ()
  (if (listp gimme-current)
      (getf (car (gimme-tree-get-node gimme-current)) 'ref)
    (prin1-to-string gimme-current)))

(defun gimme-tree-current-data ()
  (car (gimme-tree-get-node gimme-current)))

(defun gimme-tree-walk (function tree &optional depth mapcarp)
  (let ((depth (if depth depth 0)))
    (if (null tree) nil
      (cons (funcall function tree)
            (funcall (if mapcarp #'mapcar #'mapcan)
                     (lambda (n) (gimme-tree-walk function n (1+ depth) mapcarp))
                     (cdr tree))))))

(defun gimme-tree-get-trees ()
  (cdr (gimme-tree-walk (lambda (n)
                          (apply 'propertize (format "%s %s\n"
                                                     (make-string depth ?*)
                                                     (getf (car n) 'name)) (car n)))
                        (gimme-tree-add-pos gimme-trees))))

(defun gimme-tree-add-pos (tree &optional pos)
  (loop for p from 1 upto (1- (length tree))
        collecting (gimme-tree-add-pos (nth p tree) (append pos (list p)))
        into children
        finally return (cons (append (car tree) `(pos ,pos)) children)))

(defun gimme-tree-read-from-disk ()
  ""
  (setq gimme-trees
        (read
         (if (file-readable-p gimme-tree-file)
             (shell-command-to-string (format "cat %s" gimme-tree-file))
           (progn (gimme-tree-write-to-disk '((name "All" ref "\"*\"")))
                  (shell-command-to-string (format "cat %s" gimme-tree-file)))))))

(defun gimme-tree-write-to-disk (&optional tree)
  ""
  (write-region (prin1-to-string (or tree gimme-trees)) nil
                gimme-tree-file))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gimme-tree-faces)
(provide 'gimme-tree)

