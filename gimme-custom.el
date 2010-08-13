;;; gimme-custom.el --- GIMME's customizable variables

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

(defgroup gimme nil
  "XMMS2 player for GNU Emacs"
  :tag "GIMME"
  :group 'multimedia)


(defcustom gimme-playlist-formats
  '((if (string= "nil" %starred)
        (apply #'propertize (decode-coding-string (format "%s\n" %title) 'utf-8)
               (plist-put plist 'font-lock-face `(:foreground ,(color-for %album))))
      (apply #'propertize (decode-coding-string (format "* %s\n" %title) 'utf-8)
             (plist-put plist 'font-lock-face 
                        `(:foreground ,(color-for %album) :weight bold))))

    (if (and (string= "nil" %title) (string= "nil" %album) (string= "nil" %artist))
        (apply #'propertize (decode-coding-string (format "%s\n" %url) 'utf-8)
               (plist-put plist 'font-lock-face `(:foreground ,(color-for %url))))
      (apply #'propertize (decode-coding-string 
                           (format "%s > %s > %s\n" %artist %album %title) 'utf-8)
             (plist-put plist 'font-lock-face `(:foreground ,(color-for %artist))))))
  "A list of sexps that can be toggled and will be evaluated for every collection/playlist entry with %variables and 'plist' bound to its mlib tags"
  :group 'gimme
  :type '(repeat sexp))

(defcustom gimme-colors (appropriate-colors)
  "A list of colors to be used on the playlist entries"
  :group 'gimme
  :type 'sexp)

(defcustom gimme-sort-criteria '((artist album tracknr) (artist title) (title))
  "List of mlib tags to be used when sorting the collection"
  :group 'gimme
  :type '(repeat sexp))

(defcustom gimme-tree-file "~/.gimmetree"
  "Where the Tree (storing temp collections and their relationships) will be stored"
  :group 'gimme
  :type 'file)

(defcustom gimme-vol-delta 5
  "Raises/lowers the volume by this factor"
  :group 'gimme
  :type 'integer)

(gimme-status-mode)

(provide 'gimme-custom)
;;; gimme-custom.el ends here
