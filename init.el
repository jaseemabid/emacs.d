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
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Install all packages required
(load-file "~/.emacs.d/elpa-list.el")
(package-initialize)

;; Polyfill `package-install-selected-packages`, for Emacs < 25
(unless (fboundp 'package-install-selected-packages)
  (progn
    (require 'cl)
    (defun package-install-selected-packages ()
      (dolist (p package-selected-packages)
        (when (not (package-installed-p p))
          (package-install p))))))

;; Fetch packages the first time
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

(package-install-selected-packages)

(eval-and-compile
  (require 'use-package))

;; Set the paths early
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Load lisp files from site-lisp
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

;; -----------------
;; General settings
;; -----------------
(setq-default c-basic-indent 4
              c-basic-offset 4
              c-default-style nil
              case-fold-search t
              column-number-mode t
              create-lockfiles nil
              fill-adapt-mode t
              fill-column 80
              indent-tabs-mode nil
              initial-scratch-message nil
              inhibit-startup-screen t
              major-mode 'org-mode
              mouse-autoselect-window t
              next-line-add-newlines nil
              nxml-child-indent 4
              require-final-newline t
              ring-bell-function 'ignore
              sentence-end-double-space nil
              tab-width 4
              transient-mark-mode t
              vc-follow-symlinks t
              visible-bell t
              truncate-lines t)

(setq initial-buffer-choice
      (lambda ()
        (org-agenda-list)
        (bury-buffer (get-buffer "*scratch*"))
        (get-buffer "*Org Agenda*")))

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

;; Snippets
(load-file "~/.emacs.d/snippets.el")

;; Prevent accidentally suspending the frame
(unbind-key "C-x C-z")

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
        ns-function-modifier 'hyper)

  ;; Set single keys shortcuts for most common operations
  (bind-key "H--" 'split-window-below)
  (bind-key "H-\\" 'split-window-right)
  (bind-key "H-k" 'kill-this-buffer)
  (bind-key "H-t" 'transpose-windows)
  (bind-key "H-l" 'sort-lines)
  (bind-key "H-o" 'find-file)
  (bind-key "H-s" 'occur))

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

;; ---------------
;; Top level hooks
;; --------------
(add-hook 'find-file-hook 'j/find-file-large-hook)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\._\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.wf\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))

;; ------------------
;; Custom keybindings
;; ------------------

;; Buffer management
(bind-key "C-c b" 'bury-buffer)
(bind-key "C-x <down>" 'windmove-down)
(bind-key "C-x <left>" 'windmove-left)
(bind-key "C-x <right>" 'windmove-right)
(bind-key "C-x <up>" 'windmove-up)
(bind-key "C-x C-<left>" 'switch-to-prev-buffer)
(bind-key "C-x C-<right>" 'switch-to-next-buffer)
(bind-key "C-x |" 'split-window-right)
(bind-key "C-x -" 'split-window-below)

;; eval and replace
(bind-key "C-c e" 'eval-and-replace)

;; powerful counterparts ?
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; navigation bindings
(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)

;; sorting and aligning
(bind-key "M-s a" 'align-regexp)
(bind-key "M-s l" 'sort-lines)
(bind-key "M-s r" 'reverse-region)

;; string-replace
(bind-key "C-l" 'replace-string)

;; ----------------------
;; Package customizations
;; ----------------------
(use-package abbrev
  :diminish abbrev-mode

  :config
  ;; stop asking whether to save newly added abbrev when quitting emacs
  (setq save-abbrevs nil)
  (setq-default abbrev-mode t)

  (define-abbrev-table 'global-abbrev-table
    '(
      ;; math/unicode symbols
      ("8N" "ℕ")
      ("8R" "ℝ")
      ("8in" "∈")
      ("8nin" "∉")
      ("8inf" "∞")
      ("8luv" "♥")
      ("8smly" "☺")
      ("8star" "★")
      ("8meh" "¯\\_(ツ)_/¯")

      ;; email
      ("8me" "jaseemabid@gmail.com")
      ("8i" "Jaseem Abid")

      ;; normal english words
      ("8alt" "alternative")
      ("8char" "character")
      ("8def" "definition")
      ("8bg" "background")
      ("8kb" "keyboard")
      ("8ex" "example")
      ("8env" "environment")
      ("8var" "variable")
      ("8cp" "computer"))))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package auto-complete
  :bind ("M-/" . auto-complete)
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.aspell.en.pws")
  (ac-flyspell-workaround)
  (add-to-list 'ac-modes 'git-commit-mode)
  (add-to-list 'ac-modes 'org-mode))

(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :config
  (setq ahs-idle-interval 0.2)
  (global-auto-highlight-symbol-mode t)
  (setq ahs-default-range 'ahs-range-whole-buffer))

(use-package bookmark
  :bind ("C-x r" . bookmark-map))

(use-package delsel
  :config
  ;; type over a region
  (pending-delete-mode t))

(use-package desktop
  :bind ("C-c s" . desktop-save-in-desktop-dir)
  :init
  (desktop-save-mode t)
  :config
  (setq desktop-files-not-to-save "^$"
        desktop-path '("~/.emacs.d/"))
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'help-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'completion-list-mode))

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x d" . j/dired-open-here))
  :config
  (use-package dired-details
    :config
    (dired-details-install)
    (setq dired-details-hidden-string ""))
  ;; [todo] - Replace define-key with :bind
  (define-key  dired-mode-map "r" 'j/dired-open-external)
  ;; Set the name of the host and current path/file in title bar:
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
  (defun j/dired-open-here ()
    "Open current directory."
    (interactive)
    (dired (file-name-directory (or buffer-file-name "~/"))))
  (defun j/dired-open-external ()
    "In dired, open the file named on this line with external tool."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s.." file)
      (call-process "open" nil 0 nil file)
      (message "Opening %s done" file))))

(use-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setq edit-server-default-major-mode 'org-mode
        edit-server-new-frame nil)
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer))

(use-package edts
  :diminish edts-mode
  :init
  (setq edts-doc-style 'buffer
        edts-inhibit-package-check t
        edts-man-root "~/.emacs.d/edts/doc/18.2.1")
  (add-hook 'after-init-hook
            (defun j/edts-setup ()
              (require 'edts-start)
              (use-package eproject-extras
                :demand t
                :diminish eproject-mode
                :config
                (define-key eproject-mode-map (kbd "C-c b") 'bury-buffer)))))

(use-package emacs-lisp-mode
  :bind (("M-." . find-function-at-point)
         ("C-h C-f" . find-function))
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ξ"))))

(use-package erlang
  :init
  (add-to-list 'auto-mode-alist '("\\.P\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.E\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.S\\'" . erlang-mode))
  :config
  (bind-key "C-c l" 'magit-log-head  erlang-mode-map)
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq mode-name "erl"
                    erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir  "/usr/local/lib/erlang"))))

(use-package etags
  :config
  (setq tags-revert-without-query t))

(use-package face-remap
  :bind (("M-+" . text-scale-adjust)
         ("M--" . text-scale-adjust)
         ("M-0" . text-scale-adjust)))

(use-package files
  :bind ("<f5>" . revert-buffer))

(use-package fill-column-indicator
  :config
  (setq fci-rule-column 81)
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function nil
        flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '("../_build/default/lib/coracle/ebin")
        flycheck-check-syntax-automatically '(save)
        flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flyspell
  :bind (("C-c f" . flyspell-buffer)
         ("C-." . flyspell-correct-word-before-point))
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell")
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package god-mode
  :bind ("<escape>" . god-local-mode)
  :config
  (defvar j/god-cursor-enabled "red")
  (defvar j/god-cursor-disabled (cdr (assoc 'cursor-color (frame-parameters))))
  (defun j/god-cursor ()
    (set-cursor-color (if (or god-local-mode buffer-read-only)
                          j/god-cursor-enabled
                        j/god-cursor-disabled)))
  (add-hook 'god-mode-enabled-hook 'j/god-cursor)
  (add-hook 'god-mode-disabled-hook 'j/god-cursor))

(use-package haskell-mode
  :config
  (setq haskell-compile-cabal-build-command "cd %s && stack build"
        haskell-compile-cabal-build-alt-command "cd %s && stack build"
        haskell-compile-command "stack build")
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq haskell-indent-spaces 4
                    haskell-indentation-left-offset 4
                    haskell-tags-on-save t)
              ;; Ignore compiled Haskell files in filename completions
              (add-to-list 'completion-ignored-extensions ".hi")))
  ;; Haskell shell stuff
  (eval-after-load "haskell-mode"
    '(progn
       (setq haskell-interactive-popup-errors nil)
       (bind-keys :map haskell-mode-map
                  ("C-c C-b" . haskell-interactive-switch)
                  ("C-c C-d" . nil)
                  ("C-c C-i" . haskell-process-do-info)
                  ("C-c C-l" . haskell-process-load-file)
                  ("C-c C-t" . haskell-process-do-type)
                  ("C-c C-z" . haskell-interactive-switch)
                  ("C-c M-." . nil)
                  ("C-x C-d" . nil)
                  ("M-." . haskell-mode-tag-find)))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ido
  :config
  (setq ido-all-frames 'visible
        ido-case-fold t
        ido-create-new-buffer 'prompt)
  (ido-mode 1)
  (ido-everywhere 1)
  (use-package flx-ido
    :config
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))
  (define-key bookmark-map "b" 'ido-bookmark-jump)
  (defun ido-bookmark-jump ()
    "*Switch to bookmark interactively using `ido'."
    (interactive)
    (bookmark-jump
     (ido-completing-read "Bookmark: " (bookmark-all-names)))))

(use-package js2-mode
  :bind (("C-c d" . j/js-insert-debugger)
         ("M-RET" . electric-indent-just-newline))
  :init
  (defalias 'js-mode 'js2-mode)
  :config
  (defun j/js-insert-debugger ()
    "Insert a debugger statement at point"
    (interactive)
    (insert "debugger;"))
  (add-hook 'js2-mode-hook
            (lambda () (setq mode-name "JS2")))
  (setq-default js-indent-level 4
                js2-allow-keywords-as-property-names t
                js2-auto-insert-catch-block t
                js2-concat-multiline-strings t
                js2-global-externs '("$")
                js2-highlight-level 3
                js2-include-browser-externs t
                js2-include-node-externs t))

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (setq ledger-post-use-completion-engine :ido
        ledger-use-iso-dates t))

(use-package linum
  :config
  (global-linum-mode t)
  (global-hl-line-mode t))

(use-package magit
  :bind (("C-c i"   . magit-status)
         ("C-c C-i" . magit-status)
         ("C-c g"   . magit-status))
  :config
  (bind-key "C-c l" 'magit-log-head  prog-mode-map)
  (bind-key "C-c l" 'magit-log-head  dired-mode-map)
  (setq magit-process-connection-type nil
        magit-push-always-verify nil
        magit-revision-show-gravatars nil
        process-connection-type nil)
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  (add-hook 'magit-log-edit-mode-hook 'flyspell-mode))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package newcomment
  :bind ("C-c c" . comment-or-uncomment-region))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c r" . org-capture))
  :config
  (bind-key "C-c l" 'j/org-insert-link org-mode-map)
  (setq org-agenda-files `("~/Notes")
        org-agenda-timegrid-use-ampm t ;; 12hr format for agenda view
        org-default-notes-file "~/Notes/todo.org"
        org-directory "~/Notes"
        org-log-done 'time
        org-return-follows-link t
        org-src-fontify-natively t
        org-startup-folded nil
        org-capture-templates
        '(("t" "Add personal todo" entry (file+headline "~/Notes/todo.org" "Tasks")
           "* TODO %?\n  %i"
           :kill-buffer t
           :empty-lines 1)
          ("w" "Add a work todo" entry (file+headline "~/Notes/work.org" "Tasks")
           "* TODO %?\n  %i"
           :kill-buffer t)
          ("r" "Refile" plain (file "~/Notes/refile.org")
           "%?\n %i"
           :kill-buffer t)
          ("b" "Reading" entry (file+headline "~/Notes/reading.org" "Reading")
           "** %^{title}\n   %T\n\n%?"
           :kill-buffer t)
          ("j" "Journal" plain (file (format "%s%s.org" "~/Notes/Journal/"
                                             (format-time-string "%d %m %Y")))
           "%U\n\n%?%i"
           :kill-buffer t
           :unnarrowed t))))

(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paren
  :config (show-paren-mode t))

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line "")
  (projectile-global-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (bind-keys :map python-mode-map
             ("C-c d" . j/python-insert-debugger)
             ("SPC" . j/python-method-space-replace))
  (setq-default python-fill-docstring-style 'pep-257-nn)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq mode-name "Py")))
  (defun j/python-insert-debugger ()
    "Insert a debugger statement at point"
    (interactive)
    (insert "import ipdb; ipdb.set_trace()"))
  (defun j/python-method-space-replace ()
    "SPC while naming a defined method insert an underscore"
    (interactive)
    (if (and (looking-back "def .+")
             (not (and
                   (looking-at ".*)")
                   (looking-back "(.*"))))
        (insert "_")
      (insert " "))))

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))

(use-package rect
  :config
  (defalias 'rectangle-insert-string 'string-insert-rectangle))

(use-package restclient
  :mode ( "\\.rest\\'" . restclient-mode))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/places"))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package sgml-mode
  :bind (("<M-left>" . sgml-skip-tag-backward)
         ("<M-right>" . sgml-skip-tag-forward))
  :config
  (add-hook 'html-mode-hook
            (lambda ()
              (set (make-local-variable 'sgml-basic-offset) 2)
              (set (make-local-variable 'tab-width) 2))))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :init
  (when (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'")))

(use-package subword
  :diminish superword-mode
  :config (global-superword-mode t))

(use-package tramp
  :config
  (setq tramp-auto-save-directory "~/.emacs.d/auto-save-list"
        tramp-completion-reread-directory-timeout nil
        tramp-connection-timeout 30
        tramp-default-host "localhost"
        tramp-default-method "scp"))

(use-package undo-tree
  :bind ("C-\\" . undo)
  :config (global-undo-tree-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(require 'dot-toggle)
(require 'dot-lambda-fontify)

;; Private setup, passwords and key
(let ((private-file "~/.emacs.d/private.el"))
  (when (file-readable-p private-file)
    (load-file private-file)))

;; Note, load ERC only after private
(load-file "~/.emacs.d/erc.el")
(load-file "~/.emacs.d/eshell/init.el")

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
    (load-theme 'angel-light t))
