;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/init.el
;; Jaseem Abid <jaseemabid@gmail.com>

;; ;; Custom vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-fill-mode 1)
 '(c-basic-indent 4)
 '(c-default-style "k&r")
 '(case-fold-search t)
 '(column-number-mode 1)
 '(default-major-mode (quote org-mode) t)
 '(default-tab-width 4 t)
 '(fill-column 80)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode 1)
 '(nxml-child-indent 4)
 '(ring-bell-function (quote ignore) t)
 '(setq visible-bell t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 '(default ((t (:family "Droid Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; Custom packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ;; Custom keybindings
(define-key global-map "\C-x\C-u" 'undo)
(define-key global-map "\C-x\C-b" 'electric-buffer-list)
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "C-M-f") 'comment-or-uncomment-region)

;; Easy buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; ;;  Essential extra modes

;; Line number mode
(global-linum-mode t)
;; Start emacs as a server everytime
(server-start t)
;; Type over a region
(pending-delete-mode t)
;; Sessions
(desktop-save-mode t)
;; ido mode :)
(ido-mode t)
;; Snippets
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; Hooks

;; Default indentation is usually 2 spaces, changing to 4.
(add-hook 'html-mode-hook
		  (lambda ()
			(set (make-local-variable 'sgml-basic-offset) 4)))

;; delete \b at line ends before saving a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Fixing some insane text conventions
(setq sentence-end-double-space nil)

;; Rainbow mode for css-mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; auto-fill-mode for text-mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Make
;; y/n suffice for yes/no q
(fset 'yes-or-no-p 'y-or-n-p)

;; ;; Display tweaks

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
	  (list (format "%s %%S: %%j " (system-name))
			'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Color themes
;; (load "~/.emacs.d/elpa/color-theme-20080305.834/color-theme.el")
;; (color-theme-initialize)
;; (color-theme-comidia)
