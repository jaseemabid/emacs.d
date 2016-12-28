;;; dot-theme --- Custom theme

(defun j/toggle-theme ()
  "Switch b/w angel dark and light themes."
  (interactive)
  (if (member 'angel-dark custom-enabled-themes)
      (progn
        (disable-theme 'solarized-dark)
        (disable-theme 'angel-dark)
        (load-theme 'angel-light t))
    (disable-theme 'angel-light)
    (load-theme 'angel-dark t)))

(if window-system
    (load-theme 'angel-light t))

(provide 'dot-theme)
