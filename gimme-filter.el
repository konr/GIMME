(defvar gimme-filter-collections (list "*")) ;; FIXME: Better name?
(defvar gimme-filter-header "GIMME - Filter View")
(defvar gimme-filter-mode-functions
  '(gimme-insert-song gimme-set-title message
                      gimme-filter-set-current-col))

(defun gimme-filter ()
  (interactive)
  (gimme-new-session)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'filter)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-filter-mode)
     (clipboard-kill-region 1 (point-max))
     (gimme-set-title gimme-filter-header)
     (save-excursion
       (gimme-send-message (format "(pcol \"%s\" %s)\n"
                                   (car gimme-filter-collections) gimme-session))))
    (switch-to-buffer (get-buffer gimme-buffer-name)))) ;; FIXME: Quite redundant and ugly

(defun gimme-child-col ()
  (interactive)
  (let* ((parent (car gimme-filter-collections))
         (name (read-from-minibuffer (format "%s > " parent)))
         (message (format "(subcol \"%s\" \"%s\")\n" parent name)))
    (if (string-match ":" name)
        (gimme-send-message message)
      (message "Use the format 'key:val'"))))

(defun gimme-parent-col ()
  (interactive)
  (setq gimme-filter-collections
        (if (null (cddr gimme-filter-collections))
            (list "*") (cdr gimme-filter-collections)))
  (gimme-filter))

(defun gimme-filter-set-current-col (name)
  (setq gimme-filter-collections (cons name gimme-filter-collections))
  (gimme-filter))

(defun gimme-filter-append-focused ()
  (interactive)
  (gimme-send-message (format "(add %s)\n" (get-text-property (point) 'id))))

(defun gimme-filter-play-focused ()
  (interactive)
  (gimme-send-message (format "(addplay %s)\n" (get-text-property (point) 'id))))

(defun gimme-filter-append-collection ()
  (interactive)
  (gimme-send-message (format "(addcol \"%s\")\n" (car gimme-filter-collections))))

(defvar gimme-filter-map
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

    (define-key map (kbd "<") 'gimme-parent-col)
    (define-key map (kbd ">") 'gimme-child-col)
    (define-key map (kbd "a") 'gimme-filter-append-focused)
    (define-key map (kbd "RET") 'gimme-filter-play-focused)
    (define-key map (kbd "A") 'gimme-filter-append-collection)
    map))

(provide 'gimme-filter)
