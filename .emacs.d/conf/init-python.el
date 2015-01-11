
;; load python-pep8
(load "python-pep8")

(defcustom python-pep8-command "/usr/bin/pep8"
  "PEP8 command."
  :type '(file)
  :group 'python-pep8)


;; load python-mode
(when nil
  (load "python-mode")
  ;;(autoload 'python-mode "python-mode" "Python editing mode." t)
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (local-set-key "\C-c\ p" 'python-pep8))))
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))
  ;; flymake
  (add-hook 'python-mode-hook 'flymake-find-file-hook)
  (when (load "flymake" t)
    (defun flymake-pylint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "~/local/bin/pyck" (list local-file))))
    (push '("\\.py\\'" flymake-pylint-init) flymake-allowed-file-name-masks))
  )
;;(add-hook 'python-mode-hook
;;          '(lambda ()
;;             (flymake-mode t)))


