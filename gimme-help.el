;;; gimme-help.el --- GIMME Interesting Music on My Emacs

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

;; This file contains functions related to the help functionalities,
;; including integration with the wiki.

;;; Code

(defun gimme-help-set-properties (pseudo-plists)
  (let* ((plists (mapcar (lambda (y) (loop for x = y then (cddr x) while x
                                      collecting (list (intern (car x)) (cadr x))
                                      into alist and finally return (apply #'append alist)))
                         pseudo-plists))
         (fancier (loop for plist in plists collecting (list (intern (plist-get plist 'name)) plist)
                        into alist and finally return (apply #'append alist))))
    (setq gimme-song-properties fancier)))

(defun gimme-help-show-keybindings ()
  "Shows the available keybindings"
  (interactive)
  (let* ((name "GIMME - Keybindings")
         (map (cdr (current-local-map)))
         (docs (loop for k in map when (and (atom (car k)) (functionp (cdr k)))
                     collect `(key ,(if (numberp (car k)) (key-description (format "%c" (car k)))
                                      (format "%s" (car k))) function ,(cdr k) docs ,(documentation (cdr k)))
                     into list-that-needs-to-be-reverted
                     finally return (reverse list-that-needs-to-be-reverted)))
         (header "Here are the currently set keybindings.\n\n---\n\n"))
    (gimme-on-buffer
     name
     (delete-region (point-min) (point-max))
     (insert header)
     (loop for plist in docs doing
           (let* ((key (propertize (plist-get plist 'key) 'font-lock-face '(:weight bold)))
                  (nodocs (propertize "FIXME: NO DOCS FOUND!" 'font-lock-face '(:foreground "#ff0000")))
                  (docs (car (split-string (or (plist-get plist 'docs) nodocs) "\n"))))
             (insert (format "%s%s%s\n" key
                             (make-string (ceiling (/ (- (* 2.0 tab-width) (length key)) tab-width)) ?\t) docs))))
     (font-lock-mode 1)
     (use-local-map (gimme-make-basic-map))
     (switch-to-buffer name))))

(defun gimme-help-get-property (name)
  "Shows the help information, if available, of a text property."
  (let* ((name (intern name))
         (plist (plist-get gimme-song-properties name)))
    (message (or (plist-get plist 'description) "No description available!"))))


(defun gimme-help-generate-cute-keyboard (plists)
  "Displays the help information in a cute keyboard."
  (let* ((plist (loop for x in plists collecting (list (plist-get x 'key) x) into alists and finally return (apply #'append alists)))
         (kb "  __________________________________________________
 [[(<escape>)j  L(<f1>)I(<f2>)I(<f3>)I(<f4>)j L(<f1>)I(<f2>)I(<f3>)I(<f4>)j L(<f1>)I(<f2>)I(<f3>)I(<f4>)j   ]
 | _______________________________  _____  _________|
 |[__I(q)I(w)I(e)I(r)I(t)I(y)I(u)I(i)I(o)I(p)I(')I([)] [__I__] [_I_I_I_]|
 |[___I_I_I_I_I_I_I_I_I_I_I_I_L  I   ___   [_I_I_I_]|
 |[__I_I_I_I_I_I_I_I_I_I_I_I_I_L_I __I_]_  [_I_I_T ||
 |[___I_I_I_I_I_I_I_I_I_I_I_I____] [_I_I_] [___I_I_j|
 | [__I__I_________________I__L_]                   |
 l__________________________________________________j")
	 (function `(lambda (x) (let* ((k (substring x 1 -1)) 
				  (p (plist-get-with-equal ',plist k)))
			     (if p (propertize (plist-get p 'key) 'plist p) k))))
         (filled (replace-regexp-in-string "\([^\\(]*\)" function kb)))
    filled))

(provide 'gimme-help)
;;; gimme-help.el ends here
