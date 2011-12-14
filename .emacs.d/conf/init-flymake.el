;; flymake

;; (require 'flymake)
;; (set-face-background 'flymake-errline "red4")
;; (set-face-background 'flymake-warnline "dark slate blue")

;; ;; for latex
;; (defun flymake-get-tex-args (file-name)
;;   (list "platex" (list "-file-line-error" "-interaction=nonstopmode" file-name)))
;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)))

;; ;; for C++
;; (defun flymake-cc-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))


;; ;; Makefile 
;; (defun flymake-simple-make-or-generic-init (cmd &optional opts)
;;   (if (file-exists-p "Makefile")
;;       (flymake-simple-make-init)
;;     (flymake-simple-generic-init cmd opts)))

;; (defun flymake-c-init ()
;;   (flymake-simple-make-or-generic-init
;;    "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

;; (defun flymake-cc-init ()
;;   (flymake-simple-make-or-generic-init
;;    "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

;; (push '("\\.[cC]\\'" flymake-c-init) flymake-allowed-file-name-masks)
;; (push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)

;; ;(add-hook 'c++-mode-hook
;; ;          '(lambda ()
;; ;             (flymake-mode t)))

;; ;; for Python
;; (defun flymake-pylint-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "epylint" (list local-file))))
;; (push '("\\.py\\'" flymake-pylint-init) flymake-allowed-file-name-masks)

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)))

