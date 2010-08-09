;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

;; Without gray and black
(defvar cool-colors '("gainsboro" "white smoke" "white" "red" "orange red" "dark orange" "orange" "gold" "yellow" "chartreuse" "lawn green" "green" "spring green" "medium spring green" "cyan" "deep sky blue" "blue" "medium blue" "dark violet" "dark magenta" "magenta" "dark red" "brown" "firebrick" "indian red" "light coral" "salmon" "light salmon" "tomato" "coral" "dark salmon" "rosy brown" "sienna" "saddle brown" "chocolate" "peru" "sandy brown" "burlywood" "tan" "navajo white" "wheat" "dark goldenrod" "goldenrod" "light goldenrod" "pale goldenrod" "cornsilk" "dark khaki" "khaki" "lemon chiffon" "dark olive green" "olive drab" "yellow green" "green yellow" "light green" "forest green" "dark green" "lime green" "pale green" "dark sea green" "sea green" "medium sea green" "light sea green" "medium aquamarine" "aquamarine" "dark cyan" "dark turquoise" "medium turquoise" "turquoise" "pale turquoise" "powder blue" "light blue" "sky blue" "light sky blue" "cadet blue" "steel blue" "royal blue" "dodger blue" "cornflower blue" "light steel blue" "dark blue" "navy" "midnight blue" "dark slate blue" "slate blue" "medium slate blue" "light slate blue" "medium purple" "blue violet" "purple" "dark orchid" "medium orchid" "orchid" "thistle" "plum" "violet" "medium violet red" "violet red" "pale violet red" "maroon" "deep pink" "hot pink" "pink" "light pink" "snow" "misty rose" "seashell" "peach puff" "linen" "antique white" "bisque" "papaya whip" "moccasin" "blanched almond" "old lace" "floral white" "beige" "light yellow" "light goldenrod yellow" "ivory" "honeydew" "mint cream" "light cyan" "azure" "alice blue" "lavender" "ghost white" "lavender blush" "red4" "red3" "red2" "red1" "OrangeRed4" "OrangeRed3" "OrangeRed2" "OrangeRed1" "DarkOrange4" "DarkOrange3" "DarkOrange2" "DarkOrange1" "orange4" "orange3" "orange2" "orange1" "gold4" "gold3" "gold2" "gold1" "yellow4" "yellow3" "yellow2" "yellow1" "chartreuse4" "chartreuse3" "chartreuse2" "chartreuse1" "green4" "green3" "green2" "green1" "SpringGreen4" "SpringGreen3" "SpringGreen2" "SpringGreen1" "cyan4" "cyan3" "cyan2" "cyan1" "turquoise4" "turquoise3" "turquoise2" "turquoise1" "DeepSkyBlue4" "DeepSkyBlue3" "DeepSkyBlue2" "DeepSkyBlue1" "blue4" "blue3" "blue2" "blue1" "magenta4" "magenta3" "magenta2" "magenta1" "brown4" "brown3" "brown2" "brown1" "firebrick4" "firebrick3" "firebrick2" "firebrick1" "IndianRed4" "IndianRed3" "IndianRed2" "IndianRed1" "RosyBrown4" "RosyBrown3" "RosyBrown2" "RosyBrown1" "snow4" "snow3" "snow2" "snow1" "MistyRose4" "MistyRose3" "MistyRose2" "MistyRose1" "tomato4" "tomato3" "tomato2" "tomato1" "coral4" "coral3" "coral2" "coral1" "salmon4" "salmon3" "salmon2" "salmon1" "LightSalmon4" "LightSalmon3" "LightSalmon2" "LightSalmon1" "sienna4" "sienna3" "sienna2" "sienna1" "chocolate4" "chocolate3" "chocolate2" "chocolate1" "seashell4" "seashell3" "seashell2" "seashell1" "PeachPuff4" "PeachPuff3" "PeachPuff2" "PeachPuff1" "tan4" "tan3" "tan2" "tan1" "bisque4" "bisque3" "bisque2" "bisque1" "AntiqueWhite4" "AntiqueWhite3" "AntiqueWhite2" "AntiqueWhite1" "burlywood4" "burlywood3" "burlywood2" "burlywood1" "NavajoWhite4" "NavajoWhite3" "NavajoWhite2" "NavajoWhite1" "wheat4" "wheat3" "wheat2" "wheat1" "DarkGoldenrod4" "DarkGoldenrod3" "DarkGoldenrod2" "DarkGoldenrod1" "goldenrod4" "goldenrod3" "goldenrod2" "goldenrod1" "cornsilk4" "cornsilk3" "cornsilk2" "cornsilk1" "LightGoldenrod4" "LightGoldenrod3" "LightGoldenrod2" "LightGoldenrod1" "LemonChiffon4" "LemonChiffon3" "LemonChiffon2" "LemonChiffon1" "khaki4" "khaki3" "khaki2" "khaki1" "LightYellow4" "LightYellow3" "LightYellow2" "LightYellow1" "ivory4" "ivory3" "ivory2" "ivory1" "OliveDrab4" "OliveDrab3" "OliveDrab2" "OliveDrab1" "DarkOliveGreen4" "DarkOliveGreen3" "DarkOliveGreen2" "DarkOliveGreen1" "PaleGreen4" "PaleGreen3" "PaleGreen2" "PaleGreen1" "DarkSeaGreen4" "DarkSeaGreen3" "DarkSeaGreen2" "DarkSeaGreen1" "honeydew4" "honeydew3" "honeydew2" "honeydew1" "SeaGreen4" "SeaGreen3" "SeaGreen2" "SeaGreen1" "aquamarine4" "aquamarine3" "aquamarine2" "aquamarine1" "PaleTurquoise4" "PaleTurquoise3" "PaleTurquoise2" "PaleTurquoise1" "LightCyan4" "LightCyan3" "LightCyan2" "LightCyan1" "azure4" "azure3" "azure2" "azure1" "CadetBlue4" "CadetBlue3" "CadetBlue2" "CadetBlue1" "LightBlue4" "LightBlue3" "LightBlue2" "LightBlue1" "LightSkyBlue4" "LightSkyBlue3" "LightSkyBlue2" "LightSkyBlue1" "SkyBlue4" "SkyBlue3" "SkyBlue2" "SkyBlue1" "SteelBlue4" "SteelBlue3" "SteelBlue2" "SteelBlue1" "DodgerBlue4" "DodgerBlue3" "DodgerBlue2" "DodgerBlue1" "LightSteelBlue4" "LightSteelBlue3" "LightSteelBlue2" "LightSteelBlue1" "RoyalBlue4" "RoyalBlue3" "RoyalBlue2" "RoyalBlue1" "SlateBlue4" "SlateBlue3" "SlateBlue2" "SlateBlue1" "MediumPurple4" "MediumPurple3" "MediumPurple2" "MediumPurple1" "purple4" "purple3" "purple2" "purple1" "DarkOrchid4" "DarkOrchid3" "DarkOrchid2" "DarkOrchid1" "MediumOrchid4" "MediumOrchid3" "MediumOrchid2" "MediumOrchid1" "thistle4" "thistle3" "thistle2" "thistle1" "plum4" "plum3" "plum2" "plum1" "orchid4" "orchid3" "orchid2" "orchid1" "maroon4" "maroon3" "maroon2" "maroon1" "DeepPink4" "DeepPink3" "DeepPink2" "DeepPink1" "HotPink4" "HotPink3" "HotPink2" "HotPink1" "VioletRed4" "VioletRed3" "VioletRed2" "VioletRed1" "LavenderBlush4" "LavenderBlush3" "LavenderBlush2" "LavenderBlush1" "PaleVioletRed4" "PaleVioletRed3" "PaleVioletRed2" "PaleVioletRed1" "pink4" "pink3" "pink2" "pink1" "LightPink4" "LightPink3" "LightPink2" "LightPink1"))

(defun alpha-blend (c1 c2 a)
  "The resulting color of merging the c1 with alpha a on a background of color c2"
  (let* ((colors (mapcar (lambda (c) (list (substring c 1 3)
                                           (substring c 3 5)
                                           (substring c 5 7)))
                         (list c1 c2)))
         (colors (mapcar (lambda (c) (mapcar (lambda (e) (string-to-number e 16)) c))
                         colors))
         (color (map 'list (lambda (c1 c2) (format "%.2x" (+ (* (- 1 a) c1)
                                                             (* a c2))))
                     (nth 0 colors) (nth 1 colors))))
    (apply 'concat "#" color)))

(defun gimme-new-session () (setq gimme-session (random)))

(defmacro unlocking-buffer (&rest body)
  `(progn (toggle-read-only nil)
          ,@body
          (toggle-read-only t)))

(defun gimme-debug (&rest args)
  (let ((buffer-name (format "%s-debug" gimme-buffer-name)))
    (get-buffer-create buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (mapcar #'insert args))))

(defun plist-to-alist (p)
  (loop for x = p then (cddr x) while x
        collecting (cons (car x) (cadr x))))

(defun plist-to-pseudo-alist (p)
  ;; FIXME: The sexp library won't work otherwise
  (loop for x = p then (cddr x) while x
        collecting (list (car x) (cadr x))))

(defun plist-subset (small big)
  "Returns non-nil if all key/vals in small are also in big"
  (let ((keys (loop for s = small then (cddr s) while s
                    collecting (car s))))
    (every (lambda (n) (equal (getf small n) (getf big n))) keys)))

(defun color-for (string)
  (let* ((colors cool-colors)
         (len (length colors))
         (hash (string-to-number (substring (md5 (if (stringp string) string "")) 0 6) 16)))
    (nth (mod hash len) colors)))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defmacro comment (&rest rest))

(defun range-to-plists (p1 p2)
  ""
  (let ((min (min p1 p2)) (max (max p1 p2)))
    (loop for point = min then (next-property-change point)
          while (and point (> max point))
          collecting (text-properties-at point))))


(defun sublistp (l1 l2)
  "Is l a sublist of L?"
    (loop for l = l1 then (cdr l)
          and L = l2 then (cdr L)
          while (and l L (equal (car l) (car L)))
          collecting (list l L) into mu
          finally return 
          (cond ((and (not l)  L)      t)
                ((and l       (not L)) nil)
                ((and (not l) (not L)) t)
                ((and l       L)       nil))))

(defun get-bounds-where (f)
  (loop for beg = (point-min) then end
        and end = (next-property-change (point-min)) 
        then (next-property-change (or end (point-min)))
        while end
        and when (funcall f beg)
        collect (list beg end) end))

(provide 'gimme-utils)
