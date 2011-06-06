;;; gimme-filter.el --- GIMME's filter-view

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

(defun gimme-filter ()
  "Sets up the buffer. FIXME: Should be implemented in a more robust way."
  (interactive)
  (let* ((buffer-name "GIMME - Collection (All media)") 
	(buffer (car (remove-if-not (lambda (x) (string= (buffer-name x) buffer-name))
				    (buffer-list)))))
    (if buffer (switch-to-buffer buffer) (gimme-send-message "(pcol)\n"))))

(defvar gimme-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map (kbd "SPC") 'gimme-toggle)
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
    (define-key map (kbd "<") 'gimme-parent-col)
    (define-key map (kbd ">") 'gimme-child-col)
    (define-key map (kbd "a") 'gimme-filter-append-focused)
    (define-key map (kbd "RET") 'gimme-filter-play-focused)
    (define-key map (kbd "A") 'gimme-filter-append-collection)
    (define-key map (kbd "f") 'gimme-filter-same)
    map)
  "Filter-view's keymap")

(defun gimme-filter-mode ()
  "Manipulate collections"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gimme-filter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-filter-mode
        mode-name "gimme-filter"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-child-col ()
  "Creates and displays a new collection intersecting the search criteria and the current collection"
  (interactive)
  (let* ((parent gimme-collection-name)
         (name (read-from-minibuffer (format "%s > " gimme-collection-title)))
         (message (format "(subcol %s %s)\n" (prin1-to-string parent) (prin1-to-string name))))
    (setq gimme-new-collection-name (format "%s" name))
    (gimme-send-message message)))

(defun gimme-parent-col ()
  "Jumps to the current collection's parent collection."
  (interactive)
  (let* ((message (format "(supcol %s)\n" (prin1-to-string gimme-collection-name))))
    (gimme-send-message message)))

(defun gimme-filter-append-focused ()
  "Appends to the current playlist the focused song"
  (interactive)
  (gimme-send-message "(add %s)\n" (get-text-property (point) 'id)))

(defun gimme-filter-play-focused ()
  "Appends to the current playlist the focused song and then play it"
  (interactive)
  (gimme-send-message "(addplay %s)\n" (get-text-property (point) 'id)))

(defun gimme-filter-append-collection ()
  "Appends to the current playlist the entire collection"
  (interactive)
  (message "Appending songs to the playlist...")
  (dolist (el (range-to-plists (point-min) (point-max)))
    (gimme-send-message (format "(add %d)\n" (getf el 'id)))))

(defun gimme-filter-same ()
  "Creates a subcollection matching some this song's criteria"
  (interactive)
  (let* ((parent (gimme-bookmark-current-ref))
         (name (completing-read
                "Filter? "
                (mapcar (lambda (n) (format "%s:%s"
                                       (car n) (prin1-to-string
                                                (decode-coding-string (cdr n) 'utf-8))))
                        (remove-if (lambda (m) (member (car m)
                                                  '(id duration font-lock-face)))
                                   (plist-to-alist (text-properties-at (point)))))))
         (message (format "(subcol %s %s)\n" parent (prin1-to-string name))))
    (setq gimme-new-collection-name (format "%s" name))
    (gimme-send-message message)))

(defun gimme-filter-get-breadcrumbs ()
  "Returns the current position as, eg, foo > bar > baz"
  (if (listp gimme-current)
      (loop for x = gimme-current then (butlast x)
            collecting (getf (car (gimme-bookmark-get-node x)) 'name) into names
            while x
            finally return (format "%s%s"
                                   (apply #'concat (mapcar (lambda (n) (format "%s > " n))
                                                           (reverse (cdr names))))
                                   (car names)))
    (format "%s" gimme-current)))

(provide 'gimme-filter)
;;; gimme-filter.el ends here
