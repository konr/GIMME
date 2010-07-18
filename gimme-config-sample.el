(defvar gimme-playlist-formats '((apply #'propertize (decode-coding-string (format "%s\n" %title) 'utf-8)
                                        (plist-put plist 'font-lock-face `(:foreground ,(color-for %album))))
                                 (apply #'propertize (decode-coding-string (format "%s > %s\n" %artist %title) 'utf-8)
                                        (plist-put plist 'font-lock-face `(:foreground ,(color-for %artist))))))

(defvar gimme-sort-criteria '((album artist) (artist title) (title)))


(comment if (null %starred)
         (apply #'propertize (decode-coding-string (format "%s\n" %title) 'utf-8)
                (plist-put plist 'font-lock-face `(:foreground ,(color-for %album))))
         (apply #'propertize (decode-coding-string (format "* %s\n" %title) 'utf-8)
                (plist-put plist 'font-lock-face `(:foreground ,(color-for %album)))) )

(provide 'gimme-config)
