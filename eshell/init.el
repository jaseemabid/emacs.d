;; ------------------------------------
;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/eshell/init.el
;; Jaseem Abid <jaseemabid@gmail.com>
;; ------------------------------------

;; Open files and go places like we see from error messages, ie: path:line:col

;; [todo] - Make `find-file-line-number' work for emacsclient as well
;; [todo] - Make `find-file-line-number' check if the file exists
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
    (erase-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "<down>") 'next-line)
            (local-set-key (kbd "<up>") 'previous-line)
            (local-set-key (kbd "C-a") 'eshell-bol)
            (local-set-key (kbd "C-l") 'eshell/clear)
            (local-set-key (kbd "C-z") 'bury-buffer)))

(global-set-key (kbd "C-z") 'eshell)

(setq eshell-buffer-maximum-lines 4096)
