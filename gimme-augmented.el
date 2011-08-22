;;; gimme-autocomplete.el --- GIMME Interesting Music on My Emacs

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

;;; Commentary

;; Fancy functions that improve GIMME with data that is not available
;; in your computer. Freebase, Libre.fm, Lyrics support: all of these
;; should go here, although as of GIMME 2, they are not much than
;; interfaces, as the functionality is implemented in Ruby.

;; Notice that some of these functions are not bound, and I don't know
;; if they should be.

;;; Code

(defvar gimme-augmented-lyrics-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "C-f") 'scroll-up)
    (define-key map (kbd "C-b") 'scroll-down)
    map)
  "Keymap for GIMME's lyrics' view")

;;;;;;;;;;;;;;;;;;;;
;; Called by Ruby ;;
;;;;;;;;;;;;;;;;;;;;

(defun gimme-augmented-lyrics-display (plist data)
  "Displays the lyrics of a song, described by plist."
  (let* ((plist (append '(gimme-buffer-type lyrics) plist))
         (title (plist-get plist 'title)) (source (plist-get plist 'source))
         (title (format "GIMME - Lyrics for %s" title))
         (header (format "(source: %s)\n\n" source))
         (formatted (replace-regexp-in-string "" "" data)))
    (gimme-on-buffer
     (gimme-gen-buffer plist)
     (insert formatted)
     (goto-char (point-min)) (htmlr-render)
     (goto-char (point-min)) (insert (propertize header 'font-lock-face `(:foreground ,(color-for source))))
     (gimme-augmented-lyrics-mode))))

(defun gimme-augmented-show-info (data artist)
  "Displays information of an artist"
  (let ((buffer (format "%s \"%s\"" gimme-augmented-info-buffer-name artist)))
    (gimme-on-buffer
     buffer
     (delete-region (point-min) (point-max))
     (insert (propertize "* A Short Biography\n\n" 'font-lock-face `(:foreground ,(color-for "*"))))
     (insert data)
     (font-lock-mode)
     (use-local-map gimme-augmented-lyrics-map)
     (switch-to-buffer buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-augmented-lyrics-mode ()
  "Just allows coloring and exiting"
  (use-local-map gimme-augmented-lyrics-map)
  (font-lock-mode t)
  (setq major-mode 'gimme-augmented-lyrics-mode mode-name "gimme-lyrics-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-augmented-fetch-lyrics ()
  "Asks for lyrics for the current track"
  (interactive)
  (let ((plist (text-properties-at (point))))
    (gimme-send-message "(fetch_lyrics %s)\n" (prin1-to-string plist))))

(defun gimme-augmented-get-influencees ()
  "Gets a collection of music created by people influenced by the current artist"
  (interactive)
  (let* ((artist (get-text-property (point) 'artist)))
    (gimme-send-message "(get_influencees %s)\n" (prin1-to-string artist))))

(defun gimme-augmented-get-influences ()
  "Gets a collection of music created by people who influenced the current artist"
  (interactive)
  (let* ((artist (get-text-property (point) 'artist)))
    (gimme-send-message "(get_influences %s)\n" (prin1-to-string artist))))

(defun gimme-augmented-get-similar ()
  "Gets a collection of music created by people of the same genres. As any artist has dozens of genres,
this means that it'll get a very broad range of results"
  (interactive)
  (let* ((artist (get-text-property (point) 'artist)))
    (gimme-send-message "(get_similar %s)\n" (prin1-to-string artist))))

(defun gimme-augmented-ask-for-info ()
  "Asks for information on the current artist"
  (interactive)
  (let* ((artist (get-text-property (point) 'artist)))
    (gimme-send-message "(get_artist_info %s)\n" (prin1-to-string artist))))

(provide 'gimme-augmented)
;;; gimme-augmented.el ends here
