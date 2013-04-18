;; ------------------------------------
;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/init.el
;; Jaseem Abid <jaseemabid@gmail.com>
;; ------------------------------------

;; --------
;; Identity
;; ---------
(setq user-full-name "Jaseem Abid")
(setq user-mail-address "jaseemabid@gmail.com")

;; ------------
;; Server start
;; ------------
(server-start)

;; ---------------
;; Custom packages
;; ---------------
(require 'cl)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Install all packages requied
;; (load-file "~/.emacs.d/elpa-list.el")

(package-initialize)

;; (defun jaseem/packages-installed-p ()
;;   (loop for pkg in jaseem/packages
;;         when (not (package-installed-p pkg)) do (return nil)
;;         finally (return t)))

;; (unless (jaseem/packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg jaseem/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))

;; ---------
;; Autoloads
;; ---------
(require 'centered-cursor-mode)
(require 'coffee-mode)
(require 'hackernews)
(require 'httpcode)
(require 'lorem-ipsum)
(require 'magit)
(require 'rainbow-mode)
(require 'remember)
(require 'rinari)
(require 'yasnippet)


;; -----------------
;; General settings
;; -----------------
(setq-default blink-matching-delay .25
			  blink-matching-paren t
			  c-basic-indent 4
			  c-basic-offset 4
			  c-default-style "k&r"
			  case-fold-search t
			  column-number-mode t
			  default-major-mode 'org-mode
			  default-tab-width 4
			  display-battery-mode t
			  fill-adapt-mode t
			  fill-column 80
			  font-lock-maximum-decoration t
			  indent-tabs-mode t
			  inhibit-startup-message t
			  next-line-add-newlines nil
			  nxml-child-indent 4
			  quack-pretty-lambda-p t
			  require-final-newline t
			  resize-minibuffer-frame t
			  ring-bell-function 'ignore
			  sentence-end-double-space nil
			  show-paren-mode t
			  tab-width 4
			  transient-mark-mode t
			  uniquify-buffer-name-style 'forward
			  vc-follow-symlinks t
			  visible-bell t)

;; Make y/n suffice for yes/no q
(fset 'yes-or-no-p 'y-or-n-p)

;; ----------------------
;; Fonts and text styling
;; ----------------------
(set-frame-font "Inconsolata-11")

;; --------------
;; Display tweaks
;; --------------

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

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mirah$" . mirah-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))
(add-to-list 'auto-mode-alist '("nginx.conf" . nginx-mode))

;; ------------------
;; Custom keybindings
;; ------------------

;; Easy buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-c b") 'bury-buffer)

;; Navigation bindings
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

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

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-c C-m") 'menu-bar-mode)

;; Sensible backup files
(setq backup-directory-alist
	  '(("." . "~/.emacs.d/backups")))

;; --------------------------
;; Mode level customizations
;; --------------------------

;; text-mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; prog-mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'turn-on-guru-mode)

;; Minimap mode
;; (global-set-key (kbd "C-c C-s") 'minimap-create)
;; (global-set-key (kbd "C-c C-t") 'minimap-kill)

;; remember-mode
(define-key global-map (kbd "C-c r") 'remember)
(define-key global-map (kbd "C-c R") 'remember-region)

;; magit
(define-key global-map (kbd "C-c i") 'magit-status)
(define-key global-map (kbd "C-c g") 'magit-status)
(define-key global-map (kbd "C-c l") 'magit-log-simple)

(setq magit-commit-all-when-nothing-staged nil
      magit-revert-item-confirm t
      magit-process-connection-type nil
      process-connection-type nil)

(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-edit-mode-hook 'git-commit-mode-magit-setup)

;; line-number-mode
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

;; ido mode
(ido-mode t)

;; auto-fill
(auto-fill-mode 1)

;; html-mode
;; Better navigation
(add-hook 'html-mode-hook
		  (lambda ()
			"html-mode-hook"
			(set (make-local-variable 'sgml-basic-offset) 4)
			(define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
			(define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
			)
		  )

;;coffee-mode
(add-hook 'coffee-mode-hook
		  (lambda ()
			"coffee-mode-hook"
			(make-local-variable 'tab-width)
			(set 'tab-width 2)
			)
		  )

;; rainbow-mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; rinari
(global-rinari-mode)

;; centered cursor mode
;; (global-centered-cursor-mode t)

;; Snippets
(yas-global-mode 1)

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")


;; Write file hook
(add-hook 'write-file-hooks
		  (lambda ()
			;; delete \b at line ends before saving a file
			(delete-trailing-whitespace)
			)
		  )

;; Real programmers use the real lambda
(load-file "~/.emacs.d/lambda-fontify.el")
