;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/after-init.el
;; Load extra packages installed with elpa
;; Jaseem Abid <jaseemabid@gmail.com>

;; centered cursor mode
(require 'centered-cursor-mode)
(global-centered-cursor-mode t)
;; Snippets
(require 'yasnippet)
(yas-global-mode 1)
;; Explain an HTTP status code.
;; Run it with M-x hc
(require 'httpcode)
;; Hackernews :)
(require 'hackernews)
;; Multiple cursors
;; Tutorial : http://emacsrocks.com/e13.html
(require 'multiple-cursors)
;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-M-e") 'er/expand-region)
(global-set-key (kbd "C-M-i") 'er/mark-inner-tag)
(global-set-key (kbd "C-M-o") 'er/mark-outer-tag)
;; git-commit-mode
(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'git-commit-mode-magit-setup)
;; Remember mode
(require 'remember)
;; Coffee-mode
(require 'coffee-mode)
;; Rainbow-mode
(require 'rainbow-mode)
;; rinari
(require 'rinari)
(global-rinari-mode)

;; Haskell mode stuff
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")
