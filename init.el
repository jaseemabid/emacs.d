;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/init.el
;; Jaseem Abid <jaseemabid@gmail.com>

;; ;; Identity
(setq user-full-name "Jaseem Abid")
(setq user-mail-address "jaseemabid@gmail.com")

;; ;; Startup screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
	  default-major-mode 'org-mode)

;; ;; Fonts and text styling
(set-default-font "Inconsolata-11")

(auto-fill-mode 1)

(setq c-basic-indent 4
	  c-default-style "k&r"
	  case-fold-search t
	  column-number-mode 1
	  default-tab-width 4
	  fill-column 80
	  nxml-child-indent 4
	  transient-mark-mode t)

;; ;; Display tweaks

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
	  (list (format "%s %%S: %%j " (system-name))
			'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Color themes
(add-to-list 'custom-theme-load-path
			 "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; Start maximized
(set-frame-parameter nil 'fullscreen 'maximized)

;; No bars and buttons
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Visible bell
(setq ring-bell-function 'ignore
	  visible-bell t
	  show-paren-mode t)

;; Custom packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Install all packages requied
(load-file "~/.emacs.d/elpa-list.el")

;; Start emacs as a server everytime
(load "server")
(unless (server-running-p) (server-start))

;; ;; Custom keybindings

;; Easy buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; bind Caps-Lock to M-x
(if (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))
(global-set-key [f13] 'execute-extended-command)

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

;; Minimap mode
(global-set-key (kbd "C-c C-s") 'minimap-create)
(global-set-key (kbd "C-c C-t") 'minimap-kill)

;; Remember mode
(define-key global-map (kbd "C-c r") 'remember)
(define-key global-map (kbd "C-c R") 'remember-region)

;; Magit mode
(define-key global-map (kbd "C-c i") 'magit-status)
(define-key global-map (kbd "C-c g") 'magit-status)

;; Bury buffer
(define-key global-map (kbd "C-c b") 'bury-buffer)

;; Sensible backup files
(setq backup-directory-alist
	  '(("." . "~/.emacs.d/backups")))

;; ;;  Essential extra modes

;; Line number mode
(global-linum-mode t)
(global-hl-line-mode t)

;; Type over a region
(pending-delete-mode t)

;; Sessions
(desktop-save-mode t)
(setq desktop-buffers-not-to-save
	  (concat "\\("
			  "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
			  "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
			  "\\)$"))

(define-key global-map (kbd "C-c s") 'desktop-save-in-desktop-dir)

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

;; Guru-mode to do it the right way
;; (add-hook 'prog-mode-hook 'turn-on-guru-mode)

;; Handle extra filetypes

;; PKGBUILKD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; HTML mode
(add-to-list 'auto-mode-alist '("\\.ext\\'" . html-mode))

;; LESS/SASS/CSS mode
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Coffee mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Nginx-mode
(add-to-list 'auto-mode-alist '("nginx.conf" . nginx-mode))

;; Make
;; y/n suffice for yes/no q
(fset 'yes-or-no-p 'y-or-n-p)

;; Load extra packages installed with elpa
(add-hook 'after-init-hook
		  '(lambda ()
			 (load-file "~/.emacs.d/after-init.el")))

;; Real programmers use the real lambda
(load-file "~/.emacs.d/lambda-fontify.el")
