(require 'hooker)
(provide 'gimmer-model)


;; Volume
(hooker-var-add 'gimme-volume 0)
(hooker)
(hooker-hook 'gimme-volume (lambda (old new) (message "Volume set to %s" new)))

;; Playlist
(hooker-var-add 'gimme-playlist nil)
(hooker-change)
