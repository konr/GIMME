(defvar gimme-tree-header "GIMME - Tree View")
(defvar gimme-tree-mode-functions
  '(message gimme-update-playtime gimme-tree-colls))
(defvar gimme-trees '((name "All" ref nil pos nil)))
(defvar gimme-position nil)

(defun gimme-tree ()
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'tree)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (clipboard-kill-region 1 (point-max))
     (gimme-tree-mode)
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
    map))

(define-derived-mode gimme-tree-mode outline-mode
  ;; FIXME: Find out why deriving from font-lock-face won't colorize the the songs
  (interactive)
  (use-local-map gimme-tree-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-tree-mode)
  (font-lock-add-keywords 'gimme-tree-mode
                          '(("^\* .*" . 'gimme-tree-level-1)
                            ("^\*\* .*" . 'gimme-tree-level-2)
                            ("^\*\*\* .*" . 'gimme-tree-level-3)
                            ("^\*\*\*\* .*" . 'gimme-tree-level-4)
                            ("^\*\*\*\*\* .*" . 'gimme-tree-level-5)
                            ("^\*\*\*\*\*\* .*" . 'gimme-tree-level-6)
                            ("^\*\*\*\*\*\*\* .*" . 'gimme-tree-level-7)
                            ("^\*\*\*\*\*\*\*\* .*" . 'gimme-tree-level-8)))
  (setq mode-name "gimme-tree") )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tree-view-collection ()
  (interactive)
  (setq gimme-position (get-text-property (point) 'pos))
  (gimme-filter))


;;;;;;;;;
;; Aux ;;
;;;;;;;;;
;;
;; Tree is like (plist child1 child2 ...)

(defun gimme-tree-get-node (position)
  (loop for pos = position then (cdr pos)
        and tree = gimme-trees then (nth (car pos) tree)
        while pos
        finally return tree))

(defun gimme-tree-add-child (data position)
  ;; FIXME: Ugliest function EVER
  (let* ((tree (gimme-tree-get-node position))
         (len (length tree)))
    (nconc tree `((,(append `(pos ,(append position `(,len))) data))))
    len))

(defun gimme-tree-current-ref ()
  (getf (car (gimme-tree-get-node gimme-position))
              'ref))

(defun gimme-tree-current-data ()
  (car (gimme-tree-get-node gimme-position)))

(defun gimme-tree-walk (function tree &optional depth)
  (let ((depth (if depth depth 0)))
    (if (null tree) nil
      (cons (funcall function tree)
            (mapcan (lambda (n) (gimme-tree-walk function n (1+ depth)))
                    (cdr tree))))))

(defun gimme-tree-get-trees ()
  (gimme-tree-walk (lambda (n)
                     (apply 'propertize (format "%s%s %s\n"
                                                (case depth (0 "*") (1 "\n") (t ""))
                                                (make-string depth ?*)
                                                (getf (car n) 'name)) (car n)))
                   gimme-trees 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tree-colls (session list)
  "Prints the available collections as a tree"
  (let* ((list (remove-if (lambda (n) (member n '("Default" "_active"))) list))
         (list (mapcar (lambda (n) (decode-coding-string n 'utf-8)) list)))
    (with-current-buffer gimme-buffer-name
      (unlocking-buffer
       (save-excursion
         (dolist (el (gimme-tree-get-trees)) (insert el))
         (insert (format "\n* Saved collections\n"))
         (dolist (el list) (insert (format "** %s\n" el))))))))

(provide 'gimme-tree)

