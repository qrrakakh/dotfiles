(package-initialize)
(when (require 'package nil t)
  (add-to-list 'package-archives '
               ("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '
               ("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '
               ("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-user-dir "~/.local/lib/emacs/package/")
  (package-initialize)

  (defvar installing-package-list
    '(
      anything
      auto-complete
      company company-go
      concurrent
      ctable
      deferred
      epg
      go-mode
      google-c-style
      highlight-indentation
      imenu+
      ipython
      jedi jedi-core
      popup
      python-environment
      python-pep8
      scala-mode
      ))

  (defun install-using-packages ()
    (let ((not-installed
           (loop for x in installing-package-list
                 when (not (package-installed-p x))
                 collect x)))
      (when not-installed
        (package-refresh-contents)
        (dolist (pkg not-installed)
          (package-install pkg))))
    )
  )
(install-using-packages)
