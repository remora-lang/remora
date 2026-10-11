(require 'treesit)
;; (require 'scheme-mode)

(defvar remora-ts-font-lock-rules
  '(
    :language remora
	      :feature comment
	      ((comment) @font-lock-comment-face)

	      :language remora
	      :feature string
	      ((stringexp) @font-lock-string-face)

	      :language remora
	      :feature binding
	      ([(entrydecl id: (id) @font-lock-function-name-face)
		(valnotypebind id: (id) @font-lock-function-name-face)
		(valtypebind id: (id) @font-lock-function-name-face)
		(regfunbind id: (id) @font-lock-function-name-face)
		(atfunbind id: (atid) @font-lock-function-name-face)
		(tfunbind id: (id) @font-lock-function-name-face)
		(ifunbind id: (id) @font-lock-function-name-face)
		(typebind id: (_) @font-lock-function-name-face)
		(ispacebind ispacevar: (_) @font-lock-function-name-face)
		])

	      :language remora
	      :feature type
	      ([(immedtype) @font-lock-type-face
		(bracketarraytype) @font-lock-type-face
		(aarraytype) @font-lock-type-face
		(generalfntype) @font-lock-type-face
		(simplefntype) @font-lock-type-face
		(foralltype) @font-lock-type-face
		(pitype) @font-lock-type-face
		(sigmatype) @font-lock-type-face
		])

	      ;; :language remora
	      ;; :feature funcall
	      ;; ([(applicationexp) @font-lock-function-call-face
	      ;; 	(tappexp) @font-lock-function-call-face
	      ;; 	(iappexp) @font-lock-function-call-face
	      ;; 	(atappexp) @font-lock-function-call-face
	      ;;  ])

	      :language remora
	      :feature keyword
	      (["entry" "def" "unbox" "array" "frame" "t-app" "i-app"
		"let" "val" "fun" "t-fun" "i-fun"] @font-lock-keyword-face)

	      :language remora
	      :feature type
	      ([(ERROR) @font-lock-warning-face
		(MISSING) @font-lock-warning-face])
	      ))

(defvar remora-ts-indent-rules
  '((remora
     ((parent-is "program") parent-bol 0)
     ((parent-is "regfunbind") parent 2)
     ((parent-is "atfunbind") parent 2)
     ((parent-is "valnotypebind") parent 2)
     ((parent-is "valtypebind") parent 2)
     ((parent-is "letexp") (nth-sibling 2 t) 0)
     ((parent-is "bracketsexp") parent 1)
     ((parent-is "applicationexp") parent 2)
     ((parent-is "tappexp") parent 2)
     ((parent-is "iappexp") parent 2)
     ((parent-is "atappexp") parent 2)
     (no-node parent 0)
      )))
    

(defun remora-ts-setup ()
  "Setup treesit for remora-ts-mode."
  (interactive)
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     remora-ts-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
	      '((comment string)
		(binding keyword errors)
		(type funcall)
		))
  (setq-local treesit-font-lock-level 5)

  (setq-local treesit-simple-indent-rules remora-ts-indent-rules)

  (treesit-major-mode-setup))

(define-derived-mode remora-ts-mode scheme-mode "Remora[ts]"
	"Major mode for editing Remora with tree-sitter."
	:syntax-table scheme-mode-syntax-table

	(setq-local font-lock-defaults nil)
	(when (treesit-ready-p 'remora)
	  (treesit-parser-create 'remora)
	  (remora-ts-setup)))

(provide 'remora-ts-mode)
