(defvar gimme-playlist-formats '((if (string= "nil" %starred)
                                     (apply #'propertize (decode-coding-string (format "%s\n" %title) 'utf-8)
                                            (plist-put plist 'font-lock-face `(:foreground ,(color-for %album))))
                                   (apply #'propertize (decode-coding-string (format "* %s\n" %title) 'utf-8)
                                          (plist-put plist 'font-lock-face `(:foreground ,(color-for %album) :weight bold))))
                                 (apply #'propertize (decode-coding-string (format "%s > %s > %s\n" %artist %album %title) 'utf-8)
                                        (plist-put plist 'font-lock-face `(:foreground ,(color-for %artist))))))

(defvar gimme-sort-criteria '((artist album tracknr) (artist title) (title)))


(provide 'gimme-config)
