;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Configuration						  ;;
;; ~/.emacs.d/erc.el						  ;;
;; 								  ;;
;; Inspired by GH/bbatsov/emacs-dev-kit/blob/master/erc-config.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable services mode
(erc-services-mode 1)

;; Exclude boring stuff from tracking
(erc-track-mode t)

(setq erc-email-userid user-mail-address
      erc-nick user-nick
      erc-user-full-name  user-full-name

      ;; Freenode channels
      erc-autojoin-channels-alist '(("freenode.net"
                                     "#haskell" "#emacs" "#sqlalchemy"
                                     "#pyramid" "#pyconindia" "#ledger"))

      ;; Interpret mIRC-style color commands in IRC chats
      erc-interpret-mirc-color t

      erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords `((freenode (("jaseemabid" . ,freenode-password))))

      ;; Logging
      erc-enable-logging t
      erc-log-mode t
      erc-log-insert-log-on-open t
      erc-save-buffer-on-part t
      erc-log-channels-directory "~/.erc/logs/"

      erc-prompt (lambda () (format " %s>" (buffer-name)))

      ;; Hide join part messages
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; Create log dir if it doesn't exist yet
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(add-hook 'erc-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (erc-spelling-mode)))

;; Entry point to start ERC
(defun j/erc ()
  (interactive)
  (erc :server "irc.freenode.net"
       :full-name user-full-name
       :password freenode-password))
