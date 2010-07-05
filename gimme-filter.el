(defvar gimme-filter-current-collection nil)

(defun gimme-filter ()
  (interactive)
  (get-buffer-create gimme-buffer-name)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-filter-mode)
     (clipboard-kill-region 1 (point-max))
     (save-excursion
       (gimme-send-message "(list)\n")))
    (switch-to-buffer (get-buffer gimme-buffer-name)))) ;; FIXME: Quite redundant and ugly

(defvar gimme-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gimme-focused-play)
    (define-key map (kbd "C") 'gimme-clear)
    (define-key map (kbd "S") 'gimme-shuffle)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map (kbd "d") 'gimme-focused-delete)
    (define-key map (kbd "p") '(lambda () (interactive) (gimme-paste-deleted nil)))
    (define-key map (kbd "u") '(lambda () (interactive) (gimme-paste-deleted t)))
    (define-key map (kbd "s") 'gimme-stop)
    (define-key map (kbd "SPC") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "l") 'gimme-center)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") 'gimme-inc_vol) ;; FIXME: Better names, please!
    (define-key map (kbd "+") 'gimme-inc_vol)
    (define-key map (kbd "-") 'gimme-dec_vol)
    map))
