;; Custom ERC settings file
(defun erc-custom()
  (interactive)

  (require 'erc-join)
  ;; (require 'erc-view-log)
  (require 'erc-services)

  (erc-services-mode 1)
  (erc-autojoin-mode 1)

  ;; Show log files like real buffers
  ;; (add-to-list 'auto-mode-alist '("\\.erclogs/.*\\.log" . erc-view-log-mode))

  (load "~/.erc-credentials")

  (setq-default erc-autojoin-channels-alist '(
											  ("freenode.net"
											   "#Node.js"
											   "#RubyOnRails"
											   "#archlinux"
											   "#bittorrent"
											   "#chennai-hackers"
											   "#curl"
											   "#documentcloud"
											   "#fossmeet"
											   "#fsug-calicut"
											   "#git"
											   "#github"
											   "#hasgeek"
											   "#healthlucid"
                                               "#nginx"
                                               "#nm"
                                               "#smc-project"
                                               "#v8"
                                               "#xmonad"))
				erc-away-nickname "jaseemabid|away"
				erc-enable-logging t
				erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
				erc-log-channels-directory "~/.erc/logs/"
				erc-log-mode t
				erc-log-write-after-insert t
				erc-log-write-after-send t
				erc-nickserv-passwords '((freenode (("jaseemabid" . freenode-jaseemabid-pass))))
				erc-prompt-for-nickserv-password nil
				erc-save-buffer-on-part nil
				erc-email-userid "jaseemabid@gmail.com"
				erc-nick "jaseemabid"
				erc-prompt-for-password t
				erc-server "irc.freenode.net"
				erc-system-name "theblackpearl"
				erc-user-full-name "Jaseem Abid"
                )
  (erc)
  )
