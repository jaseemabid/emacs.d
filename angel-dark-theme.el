(deftheme angel-dark
  "A dark theme based on solarized dark.")

(custom-theme-set-variables
 'angel-dark
 '(magit-use-overlays t)
 '(fci-rule-width 1)
 '(cursor-type (quote (bar . 2))))

(custom-theme-set-faces
 'angel-dark
 '(magit-item-highdark ((t nil))))

(setq solarized-height-plus-1 1.1
      solarized-height-plus-2 1.1
      solarized-height-plus-3 1.1
      solarized-height-plus-4 1.1
      solarized-use-less-bold t
      solarized-use-more-italic t
      solarized-use-variable-pitch nil)

;; [todo] - Derive angel-dark from solarized
;; [todo] - Disable solarized with angel-dark
(load-theme 'solarized-dark t)

(provide-theme 'angel-dark)
