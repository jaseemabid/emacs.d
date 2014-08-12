(deftheme angel-dark
  "A dark theme based on solarized dark.")

;; [todo] - Derive angel-dark from solarized
;; [todo] - Disable solarized with angel-dark
(load-theme 'solarized-dark t)

(custom-theme-set-variables
 'angel-dark
 '(magit-use-overlays t)
 '(fci-rule-width 1)
 '(cursor-type (quote (bar . 2)))
 '(solarized-height-plus-1 1)
 '(solarized-height-plus-2 1)
 '(solarized-height-plus-3 1)
 '(solarized-height-plus-4 1)
 '(solarized-use-less-bold t)
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil))

(custom-theme-set-faces
 'angel-dark
 '(magit-item-highdark ((t nil))))

(provide-theme 'angel-dark)
