(deftheme Angel
  "Personal customizations")

(custom-theme-set-variables
 'Angel
 '(magit-use-overlays t)
 '(fci-rule-color "#0A333C")
 '(fci-rule-width 1)
 '(cursor-type (quote (bar . 2))))

(custom-theme-set-faces
 'Angel
 '(fringe ((t (:background "white"))))
 '(magit-item-highlight ((t nil)))
 '(default ((t (:inherit nil :background "White" :foreground "Black" :foundry "nil" :family "Inconsolata")))))

(provide-theme 'Angel) 

