(defvar gimme-tree-header "GIMME - Tree View")
(defvar gimme-tree-mode-functions
  '(message gimme-update-playtime gimme-tree-colls))

(defun gimme-tree ()
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'tree)

  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-filter-mode)
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
    (define-key map (kbd "SPC") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") 'gimme-inc_vol) ;; FIXME: Better names, please!
    (define-key map (kbd "+") 'gimme-inc_vol)
    (define-key map (kbd "-") 'gimme-dec_vol)
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Called by the ruby process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tree-colls (session tree)
  "Prints the available collections as a tree"
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (save-excursion
       (insert (format "%s\n" (gimme-process-branch tree)))))))

(defun gimme-process-branch (branch)
  "Formats a sexp that is either a node (number name) or a tree (node (children-tree) (childen-tree) ...) as '* parent 1\n**child 1\n**child 2\n*parent 1\n"
  (flet ((aux (branch s)
              (mapcan (lambda (n) (if (listp (car n)) (aux n (format "*%s" s))
                               (list (format "%s%s\n" s (cadar branch))))) branch)))
    (reduce #'concat (aux branch " "))))


(provide 'gimme-tree)

