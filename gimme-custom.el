;;; gimme-custom.el --- GIMME's customizable variables

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

;; Customizations. Play around with the color functions, at least!

;;; Code

(defgroup gimme nil
  "XMMS2 player for GNU Emacs"
  :tag "GIMME"
  :group 'multimedia)


(defcustom gimme-playlist-formats
  '((apply #'propertize
           (decode-coding-string
            (format "| %s | %s | %s | %s |\n"
                    (string-expanded (format "%s" %timesplayed) 3 t)
                    (string-expanded %artist 22)
                    (string-expanded %album  20)
                    (string-expanded %title  30)) 'utf-8)
           (plist-put plist 'font-lock-face
                      `(:foreground ,(color-for %artist)
                                    ,@(unless (string= "nil" %starred)
                                        `(:weight bold)))))
    (apply #'propertize (decode-coding-string
                         (format "%s%s\n"
                                 (if (string= %starred "nil") "" "* ") %title) 'utf-8)
           (plist-put plist 'font-lock-face
                      `(:foreground ,(color-for %album)
                                    ,@(unless (string= "nil" %starred)
                                        `(:weight bold)))))
    (if (and (string= "nil" %title) (string= "nil" %album) (string= "nil" %artist))
        (apply #'propertize (decode-coding-string (format "%s\n" %url) 'utf-8)
               (plist-put plist 'font-lock-face `(:foreground ,(color-for %url))))
      (apply #'propertize (decode-coding-string
                           (format "%s > %s\n" %genre %title) 'utf-8)
             (plist-put plist 'font-lock-face `(:foreground ,(color-for %genre))))))
  "A list of sexps that can be toggled and will be evaluated for every collection/playlist entry with %variables and 'plist' bound to its mlib tags"
  :group 'gimme
  :type '(repeat sexp))


(defcustom gimme-sort-criteria
  '((genre artist album title)
    (artist album tracknr) (artist title) (album title) (title) (timesplayed))
  "List of mlib tags to be used when sorting the collection"
  :group 'gimme
  :type '(repeat sexp))

(defcustom gimme-vol-delta 5
  "Raises/lowers the volume by this factor"
  :group 'gimme
  :type 'integer)

(defcustom gimme-eq-time 2
  "Time until the equalizer, when changed, disappears from the modeline, in seconds."
  :group 'gimme
  :type 'integer)

(defcustom gimme-presets
  "FIXME: To be done"
  '("1965" (-20 -16 -7 -4 -4 -4 -7 -7 3 3 -2 -4 4 1 1 -4 -6 -12)
    "Air" (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 2)
    "Brittle" (-12 -10 -9 -8 -7 -6 -5 -3 -2 -2 -2 -2 -1 1 4 4 1 0)
    "Car Stereo" (-5 0 1 0 0 -4 -4 -5 -5 -5 -3 -2 -2 0 1 0 -2 -5)
    "Classic V" (5 2 0 -2 -5 -6 -8 -8 -7 -7 -4 -3 -1 1 3 5 5 4)
    "Clear" (1 1 0 0 0 -3 0 0 0 0 0 0 0 0 2 2 2 1)
    "Dark" (-6 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -5 -8 -10 -12 -14 -18 -18)
    "DEATH" (20 17 12 8 4 0 0 0 0 0 0 0 0 0 0 0 0 0)
    "Drums" (2 1 0 0 0 -2 0 -2 0 0 0 0 2 0 0 3 0 0)
    "Flat" (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    "Home Theater" (5 2 0 -2 -3 -5 -6 -6 -5 -2 -1 0 -1 -3 3 4 3 0)
    "Loudness" (4 4 4 2 -2 -2 -2 -2 -2 -2 -2 -4 -10 -7 0 3 4 4)
    "Metal" (4 5 5 3 0 -1 -2 -1 0 1 1 1 1 0 -1 -1 -1 -1)
    "Pop" (6 5 3 0 -2 -4 -4 -6 -3 1 0 0 2 1 2 4 5 6)
    "Premaster" (0 1 3 0 -3 -3 0 0 0 2 0 0 3 0 3 1 3 2)
    "Presence" (0 0 0 0 0 0 0 0 0 3 5 4 3 2 0 0 0 0)
    "Punch & Sparkle" (3 5 3 -1 -3 -5 -5 -3 -2 1 1 1 0 2 1 3 5 3)
    "Shimmer" (0 0 0 -2 -2 -7 -5 0 0 0 0 0 4 1 3 3 4 0)
    "Soft Bass" (3 5 4 0 -13 -7 -5 -5 -1 2 5 1 -1 -1 -2 -7 -9 -14)
    "Strings" (-3 -4 -4 -5 -5 -4 -4 -3 -2 -2 -2 -2 -1 2 3 0 -2 -2))
  :group 'gimme
  :type '(repeat sexp))


;;;;;;;;;;;;
;; Colors ;;
;;;;;;;;;;;;

(defun color-string-to-list (string)
  "#ff0000 to (255 0 0)"
  (mapcar (lambda (x) (string-to-number x 16))
          (list (substring string 1 3) (substring string 3 5) (substring string 5 7))))

(defun color-list-to-string (color)
  "#(255 0 0) to #ff0000"
  (apply #'format "#%.2x%.2x%.2x" color))

(defun appropriatep (color &optional min-diff)
  "Returns non-nil if the color is appropriate to your background, according to the w3c"
  (flet ((brightness (n) (/ (apply #'+ (map 'list (lambda (x y) (* x y)) '(299 587 114) n))
                            1000)))
    (let* ((min-diff (or min-diff 125)) ;; According to w3c
           (bg (color-values (cdr (assoc 'background-color (frame-parameters)))))
           (bg (mapcar (lambda (x) (lsh x -8)) bg))
           (fg (color-string-to-list color))
           (bg-b (brightness bg)) (fg-b (brightness fg)))
      (< min-diff (abs (- bg-b fg-b))))))

(defun expand-list-of-colors (list &optional times)
  "Expand a list of colors with variations."
  (loop for color in list collecting
	(loop for i upto (or times 10) collecting (tilt-color color))
	into lists and finally return (flatten lists)))

(defun tilt-color (color &optional h s v)
  "Varies in contrast, brightness etc a color"
  (let* ((h (or h 0)) (s (or s 0.3)) (v (or v 0.3)) (delta (list h s v)) 
	 (delta (mapcar (lambda (x) (or (ignore-errors (mod (random) x)) 0)) delta))
	 (color-in-list (mapcar (lambda (x) (/ x 255.0)) (color-string-to-list color)))
	 (hsv (apply #'hexrgb-rgb-to-hsv color-in-list))
	 (hsv (map 'list (lambda (x Δ) (if (> (+ x Δ) 1) (- x Δ) (+ x Δ))) hsv delta))
	 (back-to-list (mapcar (lambda (x) (* x 255)) (apply #'hexrgb-hsv-to-rgb hsv)))
	 (rgb (color-list-to-string back-to-list)))
    rgb))

(defun all-colors (&optional appropriate step)
  "Returns all colors that, according to w3c, will be readable on your background"
  (let* ((step (or step #x33))
         (colors (loop summing step into n and until (>= n #x100) collect (list n)))
         (colors (cons '(0) colors))
         (all (mapcan (lambda (n) (mapcar (lambda (m) (append n m)) colors)) colors))
         (all (mapcan (lambda (n) (mapcar (lambda (m) (append n m)) colors)) all))
         (all (mapcar #'color-list-to-string all)))
    (if appropriate (remove-if-not #'appropriatep all) all)))

(defun high-contrast ()
  "The name reminds of those bizarre Windows 3.11 color themes, but it's actually OK."
  (remove-if-not (lambda (x) (appropriatep x 200)) (all-colors)))

(defun traditional-japanese-colors (&optional appropriate)
  "Thanks for brainwashing me, mr. Hara"
  (flet ((brightness (n) (/ (apply #'+ (map 'list (lambda (x y) (* x y)) '(299 587 114) n))
                            1000)))
    (let* ((all '("#F08F90" "#F47983" "#DB5A6B" "#C93756" "#FCC9B9" "#FFB3A7"
                  "#F2666C" "#F58F84" "#AC8181" "#B95754" "#C91F37" "#9D2933"
                  "#7B3B3A" "#F7665A" "#B56C60" "#97645A" "#A24F46" "#C3272B"
                  "#8F1D21" "#672422" "#BC2D29" "#5E2824" "#8B352D" "#FA7B62"
                  "#F8674F" "#DC3023" "#AB4C3D" "#934337" "#9D2B22" "#913228"
                  "#6F3028" "#351E1C" "#F35336" "#D34E36" "#CF3A24" "#A13D2D"
                  "#913225" "#752E23" "#F9906F" "#FF7952" "#F07F5E" "#E68364"
                  "#FF4E20" "#E35C38" "#CB6649" "#B35C44" "#B14A30" "#9B533F"
                  "#8C4736" "#60281E" "#542D24" "#4C221B" "#9F7462" "#B64925"
                  "#592B1F" "#351F19" "#F57F4F" "#EC8254" "#9F5233" "#EC956C"
                  "#985538" "#824B35" "#FFA26B" "#FCA474" "#FF8936" "#FA9258"
                  "#FB8136" "#8F583C" "#2E211B" "#AB6134" "#CA6924" "#FFA565"
                  "#D57835" "#C66B27" "#C66B27" "#985629" "#8C5939" "#6A432D"
                  "#593A27" "#C48E69" "#BE7F51" "#7D4E2D" "#B7702D" "#6B4423"
                  "#F7BB7D" "#FFA400" "#FFA631" "#E08A1E" "#CB7E1F" "#C57F2E"
                  "#785E49" "#FFB95A" "#FAA945" "#CE9F6F" "#BB8141" "#FFB61E"
                  "#FFB94E" "#E2BE9F" "#E69B3A" "#E29C45" "#B0927A" "#826B58"
                  "#7F6B5D" "#7F5D3B" "#665343" "#4C3D30" "#A17917" "#896C39"
                  "#5C4827" "#E3B130" "#E2B13C" "#F3C13A" "#D3B17D" "#AA8736"
                  "#957B38" "#D9B611" "#645530" "#BDA928" "#BBA46D" "#9C8A4D"
                  "#534A32" "#473F2D" "#8B7D3A" "#524B2A" "#3B3429" "#857C55"
                  "#5E5545" "#7A942E" "#4D4B3A" "#BCB58C" "#8DB255" "#8C9E5E"
                  "#5B8930" "#52593B" "#454D32" "#8C9C76" "#6B9362" "#817B69"
                  "#5E644F" "#374231" "#2A603B" "#A5BA93" "#898A74" "#407A52"
                  "#3D5D42" "#3D4035" "#006442" "#656255" "#224634" "#2D4436"
                  "#2E372E" "#5A6457" "#749F8D" "#819C8B" "#3A6960" "#3A403B"
                  "#2B3733" "#354E4B" "#203838" "#757D75" "#4F4944" "#2B3736"
                  "#86ABA5" "#6A7F7A" "#C6C2B6" "#48929B" "#006C7F" "#455859"
                  "#5C544E" "#264348" "#364141" "#1D697C" "#317589" "#4D646C"
                  "#044F67" "#344D56" "#3D4C51" "#4D8FAC" "#252321" "#5D8CAE"
                  "#192236" "#181B26" "#1F4788" "#003171" "#1B294B" "#78779B"
                  "#191F45" "#766980" "#5A4F74" "#89729E" "#614E6E" "#875F9A"
                  "#5D3F6A" "#976E9A" "#3F313A" "#2B2028" "#3A243B" "#A87CA0"
                  "#8D608C" "#5B3256" "#4F284B" "#23191E" "#763568" "#BB7796"
                  "#491E3C" "#755D5B" "#63424B" "#6D2B50" "#4D3B3C" "#A4345D"
                  "#8F4155" "#43242A" "#512C31" "#7E2639" "#59292C" "#44312E"
                  "#FFDDCA" "#B9A193" "#97867C" "#6E5F57" "#4B3C39" "#393432"
                  "#352925" "#27221F" "#171412" "#ebf6f7")))
      (if appropriate (remove-if-not #'appropriatep all) all))))

(defun brazilian-flag-colors ()
  "Ouviram do Ipiranga às margens pláaacidas..."
  (let* ((initial '("#00A859" "#FFCC29" "#3E4095")))
    (expand-list-of-colors initial)))

(defun american-flag-colors ()
  "I'll keep my guns, my freedom and my money!"
  (let* ((initial '("#000000" "#B22234" "#3C3B6E")))
    (expand-list-of-colors initial)))

(defcustom gimme-colors (traditional-japanese-colors)
  "A list of colors to be used on the playlist entries"
  :group 'gimme
  :type 'sexp)


(provide 'gimme-custom)
;;; gimme-custom.el ends here
