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
 '(tool-bar-mode nil) ;; Who needs all the bars?
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; Start maximized
(set-frame-parameter nil 'fullscreen 'maximized)

;; Custom packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; ;; Custom keybindings

;; Easy buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Undo without leaving the ctrl key
(define-key global-map "\C-x\C-u" 'undo)

;; Because buffer list suck
(define-key global-map "\C-x\C-b" 'electric-buffer-list)

;; Comment/uncomment block
(global-set-key (kbd "C-M-f") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Eval buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; Mark all
(global-set-key (kbd "C-c a") 'mark-whole-buffer)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Navigation bindings
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-c C-m") 'menu-bar-mode)

;; Remember mode
(define-key global-map (kbd "C-c r") 'remember)
(define-key global-map (kbd "C-c R") 'remember-region)

;; ;;  Essential extra modes

;; Line number mode
(global-linum-mode t)

;; Start emacs as a server everytime
(load "server")
(unless (server-running-p) (server-start))

;; Type over a region
(pending-delete-mode t)

;; Sessions
(desktop-save-mode t)

;; ido mode :)
(ido-mode t)

;; ;; Hooks

;; Default indentation is usually 2 spaces, changing to 4.
;; Better navigation
(add-hook 'html-mode-hook
		  (lambda ()
			(set (make-local-variable 'sgml-basic-offset) 4)
			(define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
			(define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
			)
		  )

;; delete \b at line ends before saving a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Fixing some insane text conventions
(setq sentence-end-double-space nil)

;; Rainbow mode for css and html mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; auto-fill-mode for text-mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Handle extra filetypes

;; PKGBUILKD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; HTML mode
(autoload 'html-mode "html-mode" "html Mode." t)
(add-to-list 'auto-mode-alist '("\\.ext\\'" . html-mode))

;; LESS mode
(autoload 'css-mode "css-mode" "CSS Mode." t)
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

;; ORG mode
(require 'org)
;; (autoload 'org-mode "org-mode" "ORG Mode." t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "markdown Mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Make
;; y/n suffice for yes/no q
(fset 'yes-or-no-p 'y-or-n-p)

;; ;; Display tweaks

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
	  (list (format "%s %%S: %%j " (system-name))
			'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; Load extra packages installed with elpa
(add-hook 'after-init-hook
		  '(lambda ()
			 (load-file "~/.emacs.d/after-init.el")))
