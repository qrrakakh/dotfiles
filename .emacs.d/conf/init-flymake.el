;; flymake

(when nil
  (require 'flymake)
  (set-face-background 'flymake-errline "red4")
  (set-face-background 'flymake-warnline "dark slate blue")
  (load-library "flymake-cursor")

  ;; for latex
  (defun flymake-get-tex-args (file-name)
    (list "platex" (list "-file-line-error" "-interaction=nonstopmode" file-name)))
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (flymake-mode t)))

  ;; for C/C++
  (defun flymake-c-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
  ;; (defun flymake-c-init ()
  ;;   (flymake-simple-make-or-generic-init
  ;;    "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
  (push '("\\.[cC]\\'" flymake-c-init) flymake-allowed-file-name-masks)
  (add-hook 'c++-mode-hook '(lambda () (flymake-mode t)))

  (defun flymake-cc-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "g++" (list "--std=c++0x" "-Wall" "-Wextra" "-fsyntax-only" local-file))))
  ;; (defun flymake-cc-init ()
  ;;   (flymake-simple-make-or-generic-init
  ;;    "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
  (push '("\\.cpp$" flymake-cc-init ) flymake-allowed-file-name-masks)

  (add-hook 'c++-mode-hook '(lambda () (flymake-mode t)))



  ;; Makefile 
  (defun flymake-simple-make-or-generic-init (cmd &optional opts)
    (if (file-exists-p "Makefile")
        (flymake-simple-make-init)
      (flymake-simple-generic-init cmd opts)))


  ;; for Python -> write in init-python.el

;;;;  flymake for ruby
  ;; Invoke ruby with '-c' to get syntax checking
  (defun flymake-ruby-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "ruby" (list "-c" local-file))))
  (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)


  (add-hook
   'ruby-mode-hook
   '(lambda ()
      ;; Don't want flymake mode for ruby regions in rhtml files
      (if (not (null buffer-file-name)) (flymake-mode))))
  )
