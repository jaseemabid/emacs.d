;; MODES

(require 'battery)
(setq battery-mode-line-format "#%b %p %t")
(setq battery-load-critical 7)
(setq battery-load-low 25)
(display-battery-mode t)

(require 'network-speed)
(setq network-speed-update-interval 5)
(setq network-speed-precision 1)
(setq network-speed-interface-list (if (string= (shell-command-to-string "ifconfig | grep wlan0") "")
                                       '("eth0")
                                       '("wlan0")))
(setq network-speed-format-string "%NI#%RB#%TB#%RX#%TX#%AX")
(network-speed-start)

(require 'cpu-stats)
(setq cpu-usage-format "%A %C0 %C1")
(cpu-usage-start)

(require 'memory-stats)
(setq memory-usage-format "%R %F %S")
(memory-usage-start)

(require 'misc-stats)

;; FACES:

(defmacro alias-face (name face)
  "Creates an alias `name' to face `face'."
  `(progn (defface ,name '((default :inherit ,face :slant r))
            "A macro-defined alias face."
            :group 'default)
          (defvar ,name ',name)))

(alias-face my-green-face  font-lock-constant-face)
(alias-face my-yellow-face font-lock-function-name-face)
(alias-face my-red-face    font-lock-warning-face)

(alias-face my-important-face   font-lock-keyword-face)
(alias-face my-unimportant-face font-lock-comment-face)
(alias-face my-note-face        font-lock-doc-face)

(defvar my-battery-status-alist '(("#" "discharging")
                                  ("#+" "charging" my-green-face)
                                  ("#-" "low" my-yellow-face)
                                  ("#!" "critically low" my-red-face)))

(defvar my-vc-alist '((ignored "Ignored" my-unimportant-face)
                      ;; Everything is ok:
                      (up-to-date "Up to date" my-green-face)
                      ;; Kinda important:
                      (unregistered "Unknown" my-yellow-face)
                      (edited "Edited" my-yellow-face)
                      (added "Added" my-yellow-face)
                      ;; Most important:
                      (removed "Scheduled for removal" my-red-face)
                      (conflict "Has conflicts" my-red-face)
                      (missing "Missing" my-red-face)
                      (needs-update "Needs update" my-red-face)
                      (needs-merge "Needs merge" my-red-face)
                      (unlocked-changes "Has unlocked changes" my-red-face)))

(defvar my-load-average-threshold 5.0)

;; IMAGES

(defvar my-load-average-image nil)
(defvar my-battery-status-image nil)
(defvar my-cpu-usage-image nil)
(defvar my-network-load-image nil)
(defvar my-ram-usage-image nil)

(defvar display-mode-line-images t)

(defun my-create-image (filename)
  (create-image (concat my-stuff-dir filename)
                'xpm nil
                :ascent 'center))

(defvar my-images-alist
  '((my-network-load-image . "updown3.xpm")
    (my-cpu-usage-image . "cpu3.xpm")
    (my-load-average-image . "tux3.xpm")
    (my-battery-status-image . "battery3.xpm")
    (my-ram-usage-image . "ram3.xpm")
    (my-uptime-image . "emacs2.xpm")))

(defun* toggle-mode-line-images (&optional (arg (not display-mode-line-images)))
  "Display various icons in the mode-line."
  (interactive)
  (setq display-mode-line-images arg)
  (if display-mode-line-images
      (mapc (lambda (nf)
              (set (car nf)
                   (my-create-image (cdr nf))))
            my-images-alist)
      (mapc (lambda (i)
              (set (car i) nil))
            my-images-alist)))

(toggle-mode-line-images t)

;; MODELINE
;; Eh a cool guy, overrides the default mode-line-format and doesn't afraid of anything.

(setq-default mode-line-format
  (list "-"
    'mode-line-mule-info
    '(:eval (cond (buffer-read-only
                   (propertize "%%%%" 'face 'my-yellow-face
                                      'help-echo "Buffer is read-only."
                                      'mouse-face 'mode-line-highlight))
                  ((buffer-modified-p)
                   (propertize "**" 'face 'my-red-face
                                    'help-echo "Buffer has been modified."
                                    'mouse-face 'mode-line-highlight))
                  (t (propertize "--" 'help-echo "Buffer is unmodified."
                                      'mouse-face 'mode-line-highlight))))
    '(:eval (propertize "%@" 'help-echo (concat "Default directory is: " default-directory)
                             'mouse-face 'mode-line-highlight))

    "  "
    '(:eval (let ((file-name (buffer-file-name)))
              (if file-name
                  (let ((state-face (assoc (vc-state file-name) my-vc-alist))
                        (revision (vc-working-revision file-name))
                        (backend (vc-backend file-name)))
                    (propertize (concat "%b" (when revision
                                               (concat " [" revision "]")))
                                        'face (caddr state-face)
                                        'mouse-face 'mode-line-highlight
                                        'help-echo (concat file-name "\n"
                                                           (if backend
                                                               (concat "Version controlled by "
                                                                       (symbol-name backend) "\n"
                                                                       "Status: " (cadr state-face))
                                                               "Not version controlled."))))
                   "%b")))
    "  "

    'mode-line-position

    "%[["
    ;; NOTE Fucking Org-Agenda, man...
    '(:eval (let ((name (if (stringp mode-name) mode-name (car mode-name)))
                  (rest (when (listp mode-name) (cdr mode-name))))
              (cons (propertize name 'face 'my-important-face
                                     'help-echo buffer-file-coding-system)
                    rest)))

    'minor-mode-alist
    "%]]"

    ;; ERC track with minimal distraction.
    ;; FIXME Frame title doesn't work when iconified.
    '(:eval (progn (setq icon-title-format
                         (setq frame-title-format
                               (concat "Emacs" erc-modified-channels-object)))
                   erc-modified-channels-object))

    ;; Current battery status.
    ;; NOTE Required some hacking - defining custom format-battery-string breaks transient-mark-mode.
    '(:eval (let* ((battery (split-string battery-mode-line-string))
                   (battery-level (string-to-number (cadr battery)))
                   (battery-icon (if my-battery-status-image
                                     (propertize "⚡" 'display my-battery-status-image)
                                     "⚡")))
              (unless (string= (cadr battery) "N/A")
                (propertize (concat " " battery-icon (cadr battery) "%%")
                            ;; NOTE battery-status-function inconvinience workarround.
                            'face (caddr (assoc (cond ((string= (car battery) "#+") "#+")
                                                      ((<= battery-level battery-load-critical) "#!")
                                                      ((<= battery-level battery-load-low) "#-")
                                                      (t (car battery)))
                                              my-battery-status-alist))
                            'help-echo (format "Battery status: %s\nTime remaining: %sh"
                                               (cadr (assoc (car battery)
                                                             my-battery-status-alist))
                                               (caddr battery))
                            'mouse-face 'mode-line-highlight))))

    ;; CPU usage.
    '(:eval (let* ((usage (split-string cpu-usage-mode-line-string))
                   (usage-level (string-to-number (car usage)))
                   (cpu-icon (if my-cpu-usage-image
                                (propertize "C" 'display my-cpu-usage-image)
                                "C")))
              (propertize (concat " " cpu-icon (car usage) "%%")
                          'face (cond ((>= usage-level 90.0) 'my-red-face)
                                      ((>= usage-level 75.0) 'my-yellow-face))
                          'mouse-face 'mode-line-highlight
                          'help-echo (concat "Usage:\n"
                                             "CPU0: " (cadr usage) "%\n"
                                             "CPU1: " (caddr usage) "%\n"
                                             "Average: " (car usage) "%\n"))))

    ;; RAM usage.
    '(:eval (let* ((usage (split-string memory-usage-mode-line-string))
                   (free-mem (string-to-number (cadr usage)))
                   (ram-icon (if my-ram-usage-image
                                 (propertize "M" 'display my-ram-usage-image)
                                 "M")))
              (propertize (concat " " ram-icon (car usage) "%%")
                          'face (cond ((<= free-mem 256) 'my-red-face)
                                      ((<= free-mem 512) 'my-yellow-face))
                          'mouse-face 'mode-line-highlight
                          'help-echo (concat "Main memory:\n"
                                             "Usage: " (car usage) "%\n"
                                             "Free: " (cadr usage) " MB\n"
                                             "Swap:\n"
                                             "Usage: " (caddr usage) "%\n"))))

    ;; Current system load average.
    '(:eval (let* ((load (mapcar (lambda (x) (/ x 100.0))
                                 (load-average)))
                   (load5 (cadr load))
                   (load-icon (if my-load-average-image
                                  (propertize "S" 'display my-load-average-image)
                                  "S")))
              (propertize (concat " " load-icon (number-to-string load5))
                          'face (cond ((>= load5 my-load-average-threshold)
                                       'my-red-face)
                                      ((>= load5 (/ my-load-average-threshold 2.0))
                                       ' my-yellow-face))
                          'help-echo (format (concat "Average system load:\n"
                                                     "1 minute: %s\n"
                                                     "5 minutes: %s\n"
                                                     "15 minutes: %s")
                                             (car load)
                                             (cadr load)
                                             (caddr load))
                          'mouse-face 'mode-line-highlight)))

    ;; Network load.
    '(:eval (let ((net (split-string network-speed-mode-line-string "#"))
                  (net-icon (if my-network-load-image
                                (propertize "⇅" 'display my-network-load-image)
                                "⇅")))
              (propertize (concat " " net-icon (nth 5 net))
                          'help-echo (format (concat "Interface: %s\n"
                                                     "Received bytes: %s\n"
                                                     "Transmitted bytes: %s\n"
                                                     "Download speed: %s\n"
                                                     "Upload speed: %s")
                                                     (car net)
                                                     (nth 1 net)
                                                     (nth 2 net)
                                                     (nth 3 net)
                                                     (nth 4 net))
                          'mouse-face 'mode-line-highlight)))

    ;; Current uptimes.
    '(:eval (propertize (concat " "
                                (if my-uptime-image
                                    (propertize "⌛" 'display my-uptime-image)
                                    "⌛")
                                (emacs-uptime "%h:%.2m"))
                        'help-echo (concat (format-time-string "Date: %Y-%m-%d\nTime: %H:%M:%S\n")
                                           (emacs-uptime "Emacs uptime: %H, %M\n")
                                           (format-seconds
                                             "System uptime: %H, %M"
                                             (- (float-time (current-time))
                                                (string-to-number
                                                  (shell-command-to-string
                                                    "cat /proc/stat | awk '{if(NR == 6) print $2}'")))))
                        'mouse-face 'mode-line-highlight))))
