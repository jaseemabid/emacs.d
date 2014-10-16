;; ------------------------------------
;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; ~/.emacs.d/init.el
;; Jaseem Abid <jaseemabid@gmail.com>
;; ------------------------------------

;; --------
;; Identity
;; --------
(setq init-file-user "jaseem"
      user-full-name "Jaseem Abid"
      user-nick "jaseemabid"
      user-mail-address "jaseemabid@gmail.com")

;; --------
;; Encoding
;; --------
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ---------------
;; Custom Packages
;; ---------------
(require 'cl)
(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
						 ("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Install all packages required
(load-file "~/.emacs.d/elpa-list.el")

(package-initialize)

(defun jaseem/packages-installed-p ()
  (loop for pkg in jaseem/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (jaseem/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jaseem/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; ---------
;; Autoloads
;; ---------
(require 'ace-jump-mode)
(require 'auto-complete-config)
(require 'coffee-mode)
(require 'diminish)
(require 'dired)
(require 'dired-details)
(require 'edit-server)
(require 'fill-column-indicator)
(require 'flx-ido)
(require 'flyspell)
(require 'httpcode)
(require 'lorem-ipsum)
(require 'magit)
(require 'projectile)
(require 'paredit)
(require 'python-pep8)
(require 'python-pylint)
(require 'rainbow-mode)
(require 'recentf)
(require 'remember)
(require 'rinari)
(require 'saveplace)
(require 'server)
(require 'uniquify)
(require 'web-beautify)
(require 'yasnippet)
(require 'zone)

;; -----------------
;; General settings
;; -----------------
(setq case-fold-search t
      column-number-mode t
      default-major-mode 'org-mode
      inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function 'ignore
      save-place t
      transient-mark-mode t
      vc-follow-symlinks t
      visible-bell t)

;; ----------------------------------
;; Indentation, layout and whitespace
;; ----------------------------------
(setq c-basic-indent 4
      c-basic-offset 4
      c-default-style "k&r"
      fill-adapt-mode t
      next-line-add-newlines nil
      nxml-child-indent 4
      require-final-newline t
      sentence-end-double-space nil)

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4)

;; Set the default browser to Chrome on linux
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome"))

;; Make y/n suffice for yes/no q
(fset 'yes-or-no-p 'y-or-n-p)

;; ----------------------
;; Fonts and text styling
;; ----------------------
(setq font-lock-maximum-decoration t)
;; Larger fonts for the mac
(if (eq system-type 'darwin)
    (set-frame-font "Inconsolata-14")
  (set-frame-font "Inconsolata-11"))

;; --------------
;; Display tweaks
;; --------------

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Prevent accidentally suspending the frame
(global-unset-key (kbd "C-x C-z"))

;; Always split horizontally
(setq split-height-threshold most-positive-fixnum)

;; Mac specific stuff
(when (eq system-type 'darwin)
  ;; "Mac hook"
  (setq mac-command-modifier 'control
        mac-option-modifier 'meta
        ns-use-srgb-colorspace t
        ns-alternate-modifier 'meta
        ns-auto-hide-menu-bar nil
        ns-command-modifier 'control
        ns-control-modifier 'control
        ns-function-modifier 'control))

;; No bars and buttons on linux, show a menu bar on mac anyway
(if (eq system-type 'darwin)
    (menu-bar-mode t)
  (menu-bar-mode -1))

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\._\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))

;; ------------------
;; Custom keybindings
;; ------------------

;; buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-c b") 'bury-buffer)

;; buffer splitting like tmux
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)

;; buffer list suck
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; comment/uncomment block
(global-set-key (kbd "C-M-f") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; eval buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; flyspell
(add-to-list 'exec-path "/usr/local/bin")
(global-set-key (kbd "C-c f") 'flyspell-buffer)
(global-set-key (kbd "C-.") 'flyspell-correct-word-before-point)
(setq ispell-program-name "aspell")

;; powerful counterparts ?
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; navigation bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; sorting and aligning
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s r") 'reverse-region)

;; undo without leaving the ctrl key
(define-key global-map "\C-x\C-u" 'undo)

;; string-replace
(global-set-key (kbd "C-l") 'replace-string)

;; webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; ------------------------
;; Mode level customization
;; ------------------------

;; ace-jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; auto-fill
(auto-fill-mode 1)

;; auto-complete mode
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.aspell.en.pws")
(ac-flyspell-workaround)
(global-set-key (kbd "M-/") 'auto-complete)
(add-to-list 'ac-modes 'git-commit-mode)
(add-to-list 'ac-modes 'org-mode)

;; bookmarks
(defun ido-bookmark-jump ()
  "*Switch to bookmark interactively using `ido'."
  (interactive)
  (bookmark-jump
   (ido-completing-read "Bookmark: " (bookmark-all-names))))

(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)

;; coffee-mode
(setq coffee-tab-width 2)

;; desktop-mode
(desktop-save-mode t)
(setq desktop-files-not-to-save "^$")
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'help-mode)
(add-to-list 'desktop-modes-not-to-save 'magit-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'completion-list-mode)
(define-key global-map (kbd "C-c s") 'desktop-save-in-desktop-dir)

;; Diminish mode
(diminish 'auto-complete-mode)
(diminish 'flyspell-mode)
(diminish 'magit-auto-revert-mode)
(diminish 'paredit-mode " φ")
(diminish 'yas-minor-mode)

;; dired-mode
(defun j/dired-open-here ()
  "Open current directory."
  (interactive)
  (dired (file-name-directory (or buffer-file-name "~/"))))

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'j/dired-open-here)
(dired-details-install)
(setq dired-details-hidden-string "")

(eval-after-load 'dired
  (lambda ()
    (define-key dired-mode-map (kbd "C-x d") 'bury-buffer)))

;; Edit server
;; Chrome extension[1]  to edit textareas in Emacs
(edit-server-start)

;; emacs-lisp-mode
(global-set-key (kbd "C-h C-f") 'find-function)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "ξ")))

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; eshell
(load-file "~/.emacs.d/eshell/init.el")

;; fci
(setq fci-rule-column 81)

;; God mode
;; [todo] - Cursor color wont be updated in case of a theme change
(setq j/god-cursor-enabled "red"
      j/god-cursor-disabled (cdr (assoc 'cursor-color  (frame-parameters))))

(defun j/god-cursor ()
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        j/god-cursor-enabled
                      j/god-cursor-disabled)))

(add-hook 'god-mode-enabled-hook 'j/god-cursor)
(add-hook 'god-mode-disabled-hook 'j/god-cursor)

(global-set-key (kbd "<escape>") 'god-local-mode)

;; Haskell mode
(setq haskell-mode-hook '(imenu-add-menubar-index
                          turn-on-eldoc-mode
                          turn-on-haskell-doc
                          turn-on-haskell-indentation
                          turn-on-haskell-doc-mode))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; Haskell shell stuff
(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-interactive-popup-errors nil)
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; html-mode
;; Better navigation
(add-hook 'html-mode-hook
          (lambda ()
            "html-mode-hook"
            (set (make-local-variable 'sgml-basic-offset) 2)
            (set (make-local-variable 'tab-width) 2)
            (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
            (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)))

;; ido-mode
(setq ido-enable-flex-matching t
      ido-all-frames 'visible
      ido-case-fold t
      ido-use-faces nil
      ido-create-new-buffer 'prompt
      ido-everywhere t)
(ido-mode t)
(flx-ido-mode t)

;; js-mode
(defalias 'js-mode 'js2-mode)

;; js2-mode
(defun j/js-insert-debugger ()
  "Insert a debugger statement at point"
  (interactive)
  (insert "debugger;"))

(eval-after-load 'js2-mode
  (lambda ()
    (define-key js2-mode-map  (kbd "C-c d")  'j/js-insert-debugger)))

(add-hook 'js2-mode-hook
          (lambda () (setq mode-name "JS2")))

(setq-default js-indent-level 4
              js2-allow-keywords-as-property-names t
              js2-auto-insert-catch-block t
              js2-concat-multiline-strings t
              js2-global-externs '("$" "Y" "YUI" "_")
              js2-highlight-level 3
              js2-include-browser-externs t
              js2-include-node-externs t)

;; js2-refactor-mode
(js2r-add-keybindings-with-prefix "C-c C-r")

;; ledger mode
 (setq ledger-post-use-completion-engine :ido
       ledger-use-iso-dates t)

;; line-number-mode
(global-linum-mode t)
(global-hl-line-mode t)

;; magit
(define-key global-map (kbd "C-c i") 'magit-status)
(define-key global-map (kbd "C-c C-i") 'magit-status)
(define-key global-map (kbd "C-c g") 'magit-status)
(define-key global-map (kbd "C-c l") 'magit-log-simple)
(setq magit-commit-all-when-nothing-staged nil
      magit-revert-item-confirm t
      magit-process-connection-type nil
      process-connection-type nil)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)

;; org-mode
(setq org-agenda-files `("~/Notes/todo.org")
      org-agenda-timegrid-use-ampm 1 ;; 12hr format for agenda view
      org-default-notes-file "~/Notes/todo.org"
      org-directory "~/Notes"
      org-return-follows-link t
      org-src-fontify-natively t
      org-startup-folded nil)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)
;; org-indent is messing up layout once in a while.
;; (add-hook 'org-mode-hook 'org-indent-mode)

;; org-present mode
(defvar org-present-text-scale 10)
(add-hook 'org-present-mode-hook (lambda ()
                                   (linum-mode -1)
                                   (org-present-big)
                                   (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook (lambda ()
                                        (linum-mode t)
                                        (org-present-small)
                                        (org-remove-inline-images)))

;; Projectile mode
(projectile-global-mode)
(setq projectile-mode-line
      (quote (:eval (format " [ρ %s]" (projectile-project-name)))))


;; Python mode
(defun j/python-insert-debugger ()
  "Insert a debugger statement at point"
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(defun j/python-method-space-replace ()
  "SPC while naming a defined method insert an underscore"
  (interactive)
  (if (and (looking-back "def .+")
           (not (and
                 (looking-at ".*)")
                 (looking-back "(.*"))))
      (insert "_")
    (insert " ")))

(setq-default python-fill-docstring-style 'pep-257-nn)

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (setq mode-name "Py")))
(eval-after-load 'python
  (lambda ()
    (define-key python-mode-map (kbd "C-c d") 'j/python-insert-debugger)
    (define-key python-mode-map (kbd "SPC") 'j/python-method-space-replace)))

;; prog-mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'fci-mode)

;; Rectangle mode
(defalias 'rectangle-insert-string 'string-insert-rectangle
  "Because I think so!

`string-insert-rectangle' should have been called
`rectangle-insert-string' because its coming from the rectangle
package. Its so much easier to remember/ido-complete stuff if
they have sane names.

Emacs lisp really need namespaces and closures.")

;; rainbow-mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; rainbow-delimiters-mode
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; rinari
(global-rinari-mode)

;; Recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; root-edit : never save file as root
(when (= (user-uid) 0)
  (read-only-mode t))

;; server-mode
(unless (server-running-p)
  (server-start))

;; show parentheses
(show-paren-mode t)

;; smex-mode
;; bind Caps-Lock to smex
(when (eq window-system 'x)
  (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; snippets
(yas-global-mode 1)

;; text-mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; tramp-mode
(setq tramp-default-host "localhost"
      tramp-auto-save-directory "~/.emacs.d/auto-save-list"
      tramp-default-method "scp")

;; web-beautify mode
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c w") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c w") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c w") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c w") 'web-beautify-css))

;; type over a region
(pending-delete-mode t)

;; The Toggle-Map and Wizardry
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html

(define-prefix-command 'j/toggle-map)

;; The manual recommends C-c for user keys, but C-x t is always free,
;; whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'j/toggle-map)

(define-key j/toggle-map "c" 'column-number-mode)
(define-key j/toggle-map "d" 'toggle-debug-on-error)
(define-key j/toggle-map "e" 'toggle-debug-on-error)
(define-key j/toggle-map "f" 'auto-fill-mode)
(define-key j/toggle-map "h" 'global-hl-line-mode)
(define-key j/toggle-map "l" 'global-linum-mode)
;; menu bar for exploring new modes
(define-key j/toggle-map "m" 'menu-bar-mode)
(define-key j/toggle-map "p" 'paredit-mode)
(define-key j/toggle-map "q" 'toggle-debug-on-quit)
(define-key j/toggle-map "t" 'j/toggle-theme)
(define-key j/toggle-map "w" 'whitespace-mode)
;; Generalized `read-only-mode'
(define-key j/toggle-map "r" 'dired-toggle-read-only)
(define-key j/toggle-map "|" 'fci-mode)

(autoload 'dired-toggle-read-only "dired" nil t)

;; uniquify buffers
(setq uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 1)

;; write file hook
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; zoom
(global-set-key (kbd "M-+") 'text-scale-adjust)
(global-set-key (kbd "M--") 'text-scale-adjust)
(global-set-key (kbd "M-0") 'text-scale-adjust)

;; Real programmers use the real lambda
(load-file "~/.emacs.d/lambda-fontify.el")

;; Snippets
(load-file "~/.emacs.d/snippets.el")

;; Private setup, passwords and key
(let ((private-file "~/.emacs.d/private.el"))
  (when (file-readable-p private-file)
    (load-file private-file)))

;; Load ERC late, after private
(load-file "~/.emacs.d/erc.el")

;; Custom theme
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
    (load-theme 'angel-dark t))

;; Footnotes
;; 1: https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
