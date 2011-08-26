;;; gimme-emacs.el --- GIMME Interesting Music on My Emacs

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

;; Functions that interact with other Emacs packages.

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-add-path-to-playlist (path)
  "Adds a given path to the collection and playlist. Requires a fullpath"
  (gimme-send-message "(add %s)\n" (hyg-prin1 (format "file://%s" path))))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(defun gimme-add-marked-to-the-playlist-recursively ()
  "Add marked files to XMMS2"
  (interactive)
  (let* ((initial (dired-get-marked-files))
         (expanded (expand-directories initial)))
    (dolist (item expanded) (gimme-add-path-to-playlist item))))


(provide 'gimme-emacs)
;;; gimme-emacs.el ends here
