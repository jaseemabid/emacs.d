;; ------------------------------------
;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/eshell/init.el
;; Jaseem Abid <jaseemabid@gmail.com>
;; ------------------------------------

(use-package eshell
  :init (bind-key "C-z" 'eshell)
  :bind (("<down>" . next-line)
         ("<up>" . previous-line)
         ("C-a" . eshell-bol)
         ("C-l" . eshell/clear))
  :config
  (setq eshell-buffer-maximum-lines 4096)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-z") 'bury-buffer)))

  ;; Open files and go places like we see from error messages, ie: path:line:col
  (defadvice find-file (around find-file-line-number
                               (path &optional wildcards) activate)
    "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
    (save-match-data
      (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path))
             (line-no (and match
                           (match-string 2 path)
                           (string-to-number (match-string 2 path))))
             (col-no (and match
                          (match-string 3 path)
                          (string-to-number (match-string 3 path))))
             (path (if match (match-string 1 path) path)))
        ad-do-it
        (when line-no
          ;; goto-line is for interactive use
          (goto-char (point-min))
          (forward-line (1- line-no))
          (when (> col-no 0)
            (forward-char (1- col-no)))))))

  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))

  (defun eshell/clear ()
    "clear shell buffer"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (eshell-send-input)))
