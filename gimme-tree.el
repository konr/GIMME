(defvar gimme-tree-header "GIMME - Tree View")
(defvar gimme-tree-mode-functions
  '(message))

(defvar gimme-tree "                            +
                           XXX
                          XXXXX
                         XXXXXXX
                        XXXXXXXXX
                       'BUON ANNO'
                      'JOYEUX NOEL'
                     'VESELE VANOCE'
                    'MELE KALIKIMAKA'
                   'NODLAG SONA DHUIT'
                  'BLWYDDYN NEWYDD DDA'
                       'GOD  JUL'
                      'FELIZ NATAL'
                      'BOAS FESTAS'
                     'FELIZ NAVIDAD'
                    'MERRY CHRISTMAS'
                   'KALA CHRISTOUGENA'
                  'VROLIJK  KERSTFEEST'
                 'FROHLICHE WEIHNACHTEN'
                'BUON  NATALE-GODT NYTAR'
               'HUAN YING SHENG TAN CHIEH'
              'WESOLYCH SWIAT-SRETAN BOZIC'
             'MOADIM LESIMHA-LINKSMU KALEDU'
            'HAUSKAA JOULUA-AID SAID MOUBARK'
                 ''N  PRETTIG  KERSTMIS'
                'ONNZLLISTA UUTTA VUOTTA'
               'Z ROZHDESTYOM  KHRYSTOVYM'
              'NADOLIG LLAWEN-GOTT NYTTSAR'
             'FELIC NADAL-GOJAN KRISTNASKON'
            'S  NOVYM  GODOM-FELIZ ANO NUEVO'
           'GLEDILEG JOL-NOELINIZ KUTLU OLSUM'
          'EEN GELUKKIG NIEUWJAAR-SRETAN BOSIC'
         'KRIHSTLINDJA GEZUAR-KALA CHRISTOUGENA'
        'SELAMAT HARI NATAL - LAHNINGU NAJU METU'
             'SARBATORI FERICITE-BUON  ANNO'
            'ZORIONEKO GABON-HRISTOS SE RODI'
           'BOLDOG KARACSONNY-VESELE  VIANOCE '
          'MERRY CHRISTMAS  - -  HAPPY NEW YEAR'
         'ROOMSAID JOULU PUHI -KUNG HO SHENG TEN'
        'FELICES PASUAS-EIN GLUCKICHES    NEWJAHR'
       'PRIECIGUS ZIEMAN SVETKUS  SARBATORI VESLLE'
      'BONNE ANNEBLWYDDYN NEWYDD DDADR  FELIZ  NATAL'
                          XXXXX
                          XXXXX
                          XXXXX
                      XXXXXXXXXXXXX ")

(defun gimme-tree ()
  (interactive)
  (get-buffer-create gimme-buffer-name)
  (setq gimme-current-mode 'tree)
  (with-current-buffer gimme-buffer-name
    (unlocking-buffer
     (gimme-filter-mode)
     (clipboard-kill-region 1 (point-max))
     (gimme-set-title gimme-tree-header)
     (insert gimme-tree))
    (switch-to-buffer (get-buffer gimme-buffer-name))))

(defvar gimme-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'gimme-filter)
    (define-key map (kbd "@") 'gimme-tree)
    (define-key map (kbd "#") 'gimme-playlist)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer gimme-buffer-name)))
    (define-key map (kbd "SPC") 'gimme-toggle)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "J") 'gimme-next)
    (define-key map (kbd "K") 'gimme-prev)
    (define-key map (kbd "TAB") 'gimme-toggle-view)
    (define-key map (kbd "=") 'gimme-inc_vol) ;; FIXME: Better names, please!
    (define-key map (kbd "+") 'gimme-inc_vol)
    (define-key map (kbd "-") 'gimme-dec_vol)
    map))

(provide 'gimme-tree)

