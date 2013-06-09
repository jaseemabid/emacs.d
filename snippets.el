;; --------------------------------------------------- ;;
;; Collection of snippets and macros that don't fit anywhere else ;;
;; --------------------------------------------------- ;;

(defun passwords-edit ()
  "Edit the passwords file."
  (interactive)
  (find-file "~/Notes/Passwords.org.gpg"))

;; sink
(fset 'sink
   [?R ?~ ?/ ?. ?l ?o ?c ?a ?l ?/ ?s ?h ?a ?r ?e ?/ ?e ?x ?t ?r ?a ?s tab return])

;; Occur in isearch
;; http://www.emacswiki.org/emacs/OccurFromIsearch
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
	(occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-c o") 'isearch-occur)
