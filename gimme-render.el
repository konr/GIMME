



(defun gimme-render (html)
  (let* ((html (gimme-render-process-pairs html))
         (html (gimme-render-process-lone-tags html)))
    html))

(defun gimme-render-process-pairs (string)
  (loop for x = string then (replace-regexp-in-string "<\\([^> ]+\\)[^>]*>\\([^(</\\1>)]\+\\)*</\\1>"
                                                      #'gimme-render-process-match x)
        and y = (format "%sa" string) then x until (string= x y) and finally return x))

(defun gimme-render-process-lone-tags (string)
  "FIXME: tdb"
  (replace-regexp-in-string "<[^>]\+>"
                            (lambda (string)
                              (let* ((tag-and-attribs (substring string 1 (1- (length string))))
                                     (tag-and-attribs (downcase tag-and-attribs))
                                     (space (or (string-match " " tag-and-attribs) (length tag-and-attribs)))
                                     (tag (substring tag-and-attribs 0 space))
                                     (attribs (substring tag-and-attribs space))
                                     (attribs (regexp-all-matches attribs "\\([^= ]\+\\)=\"\\([^\"]\+\\)\"[ \$]")))
                                (cond
                                 ((string= tag "br") "\n")
                                 ((string= tag "hr") "\n--------\n")
                                 (t ""))))
                            string))

(defun gimme-render-process-match (string)
  "The matched string will have a tag in the beginning and one at the end, like <b>Foobar</b>"
  (let* ((fixed (replace-regexp-in-string "^[^>]\+>\\(.*\\)<[^<]\+$" "\\1" string))
         (tag-and-attribs (replace-regexp-in-string "^<\\([^>]\+\\)>.*" "\\1" string))
         (tag-and-attribs (downcase tag-and-attribs))
         (space (or (string-match " " tag-and-attribs) (length tag-and-attribs)))
         (tag (substring tag-and-attribs 0 space))
         (attribs (substring tag-and-attribs space))
         (attribs (regexp-all-matches attribs "\\([^= ]\+\\)=\"\\([^\"]\+\\)\"[ \$]"))
         (props (get-text-property 0 'font-lock-face fixed)))
    (cond
     ((string= "b" tag) (propertize fixed 'font-lock-face (plist-put props :weight 'bold)))
     ((string= "i" tag) (propertize fixed 'font-lock-face (plist-put props :slant 'italic)))
     ((string= "p" tag) (format "\n%s\n" fixed))
     (t fixed))))

(defun append-font-lock-pair (text pair)
  "buggy function!!"
  (message text)
  (let* ((chars (butlast (cdr (split-string text ""))))
         (combined (mapcar (lambda (x) (propertize x 'font-lock-face
                                              (apply #'plist-put (get-text-property 0 'font-lock-face x) pair))) chars))
         (reconstructed (apply #'concat combined)))
    reconstructed))
