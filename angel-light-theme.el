(deftheme angel-light
  "A light theme based on angry fruit salad.")

(custom-theme-set-variables
 'angel-light
 '(magit-use-overlays t)
 '(setq fci-rule-color "#0A333C")
 '(fci-rule-width 1)
 '(cursor-type (quote (bar . 2))))

(custom-theme-set-faces
 'angel-light
 '(fringe ((t (:background "White"))))
 '(magit-item-highlight ((t nil)))
 '(default ((t (:inherit nil :background "White" :foreground "#111" :foundry "nil")))))

(provide-theme 'angel-light)
