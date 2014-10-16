;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Configuration						  ;;
;; ~/.emacs.d/erc.el						  ;;
;; 								  ;;
;; Inspired by GH/bbatsov/emacs-dev-kit/blob/master/erc-config.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'erc)
(require 'erc-autoaway)
(require 'erc-button)
(require 'erc-list)
(require 'erc-log)
(require 'erc-log)
(require 'erc-match)
(require 'erc-menu)
(require 'erc-networks)
(require 'erc-notify)
(require 'erc-notify)
(require 'erc-ring)
(require 'erc-services)
(require 'erc-services)
(require 'erc-spelling)
(require 'erc-spelling)
(require 'erc-stamp)
(require 'erc-track)

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

      erc-prompt (lambda () (concat " " (buffer-name) "> "))

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
