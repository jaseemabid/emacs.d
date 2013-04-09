;; Real programmers use the real lambda
(defun sm-lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil)))))

  (font-lock-add-keywords
   nil `(("\\<function\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil)))))
  )

(add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
(add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
(add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)

(add-hook 'js2-mode-hook 'sm-lambda-mode-hook)
