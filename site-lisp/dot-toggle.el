;; The Toggle-Map and Wizardry
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html

(define-prefix-command 'j/toggle-map)

;; The manual recommends C-c for user keys, but C-x t is always free,
;; whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'j/toggle-map)

(define-key j/toggle-map "-" 'visual-line-mode)
(define-key j/toggle-map "c" 'column-number-mode)
(define-key j/toggle-map "d" 'toggle-debug-on-error)
(define-key j/toggle-map "e" 'toggle-debug-on-error)
(define-key j/toggle-map "f" 'auto-fill-mode)
(define-key j/toggle-map "h" 'global-hl-line-mode)
(define-key j/toggle-map "l" 'linum-mode)
;; menu bar for exploring new modes
(define-key j/toggle-map "m" 'menu-bar-mode)
(define-key j/toggle-map "p" 'paredit-mode)
(define-key j/toggle-map "q" 'toggle-debug-on-quit)
(define-key j/toggle-map "t" 'j/toggle-theme)
(define-key j/toggle-map "w" 'whitespace-mode)
;; Generalized `read-only-mode'
(define-key j/toggle-map "r" 'dired-toggle-read-only)
(define-key j/toggle-map "|" 'fci-mode)

(provide 'dot-toggle)
