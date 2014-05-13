;; --------------------------------------------------- ;;
;; Collection of snippets and macros that don't fit anywhere else ;;
;; --------------------------------------------------- ;;

(defmacro replace-in-file (from-string to-string)
  `(progn
     (goto-char (point-min))
     (while (search-forward ,from-string nil t)
       (replace-match ,to-string nil t))))

(defun cleanup-fancy-quotes ()
  (interactive)
  (progn
    (replace-in-file "’" "'")
    (replace-in-file "“" "\"")
    (replace-in-file "”" "\"")
    (replace-in-file "" "")))

(defun smart-open-line ()
  "Shortcut for C-e RET"
  (interactive)
  (move-end-of-line nil)
  (newline))
(global-set-key [(shift return)] 'smart-open-line)

;; Occur in isearch
;; http://www.emacswiki.org/emacs/OccurFromIsearch
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
	(occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-c o") 'isearch-occur)

(defun insert-timestamp ()
  "Insert time-stamp at the current `point'."
  (interactive)
  (insert (format-time-string "%A %d %B %Y %r %Z")))

;; Custom macros
(fset 'sink
	  [?R ?~ ?/ ?. ?l ?o ?c ?a ?l ?/ ?s ?h ?a ?r ?e ?/ ?e ?x ?t ?r ?a ?s tab return])

(fset 'doc
	  [?R ?~ ?/ ?D ?o ?c ?u ?m tab ?U ?n tab return])

(fset 'dot-link
	  [S-end ?\M-w ?S ?~ ?/ ?. ?\C-y return])

(fset 'music-add
	  [S-end ?\M-w ?S ?~ ?/ ?M ?e return ?P ?l ?a return ?\C-y return])
(global-set-key (kbd "C-c ma") 'music-add)
