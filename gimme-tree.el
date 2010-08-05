(defvar gimme-tree-header "GIMME - Tree View")
(defvar gimme-tree-mode-functions
  '(message gimme-update-playtime gimme-tree-colls))
(defvar gimme-trees (make-hash-table))

(defun gimme-tree ()
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'tree)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-tree-mode)
     (clipboard-kill-region 1 (point-max))
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
  (setq mode-name "gimme-tree"))

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
         (comment insert (format "%s\n" (gimme-process-branch tree))) ;; FIXME: comment
         (insert (format "* Saved collections\n"))
         (dolist (el list)
           (insert (format "** %s\n" el))))))))

(defun gimme-process-branch (branch)
  "Formats a sexp that is either a node (number name) or a tree (node (children-tree) (childen-tree) ...) as '* parent 1\n**child 1\n**child 2\n*parent 1\n"
  (flet ((aux (branch s)
              (mapcan (lambda (n) (if (listp (car n)) (aux n (format "*%s" s))
                               (list (format "%s%s\n" s (cadar branch))))) branch)))
    (replace-regexp-in-string "^\* " "\n\* "
                              (reduce #'concat (aux branch " ")))))


(provide 'gimme-tree)

