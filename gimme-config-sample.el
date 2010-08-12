(defvar gimme-playlist-formats '((if (string= "nil" %starred)
                                     (apply #'propertize (decode-coding-string (format "%s\n" %title) 'utf-8)
                                            (plist-put plist 'font-lock-face `(:foreground ,(color-for %album))))
                                   (apply #'propertize (decode-coding-string (format "* %s\n" %title) 'utf-8)
                                          (plist-put plist 'font-lock-face `(:foreground ,(color-for %album) :weight bold))))
                                 
                                 (if (and (string= "nil" %title) (string= "nil" %album) (string= "nil" %artist))
                                     (apply #'propertize (decode-coding-string (format "%s\n" %url) 'utf-8)
                                            (plist-put plist 'font-lock-face `(:foreground ,(color-for %url))))
                                   (apply #'propertize (decode-coding-string (format "%s > %s > %s\n" %artist %album %title) 'utf-8)
                                          (plist-put plist 'font-lock-face `(:foreground ,(color-for %artist)))))))

(defvar gimme-sort-criteria '((artist album tracknr) (artist title) (title)))
(defvar gimme-tree-file "~/.gimmetree")
(defvar gimme-vol-delta 5)

(gimme-status-mode)

(provide 'gimme-config)


