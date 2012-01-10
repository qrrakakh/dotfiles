;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Identify the environments:
;; http://d.hatena.ne.jp/hito-d/20060220#1140445790

;; OS
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (or (equal system-type 'usg-unix-v)
          (or  (equal system-type 'berkeley-unix)
               (equal system-type 'cygwin)))))

(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; consider cygwin as UNIX
  (equal system-type 'cygwin))

(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))

(defvar run-darwin (equal system-type 'darwin))

;; Kind of Emacsen and its version
(if 0 ()
  (defvar run-emacs20
    (and (equal emacs-major-version 20)
         (null (featurep 'xemacs))))
  (defvar run-emacs21
    (and (equal emacs-major-version 21)
         (null (featurep 'xemacs))))
  (defvar run-meadow1 (and run-meadow run-emacs20))
  (defvar run-meadow2 (and run-meadow run-emacs21))
  )

(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow3 (and run-meadow run-emacs22))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))
(defvar run-carbon-emacs (and run-darwin window-system))

;(eval-when-compile
;  (require 'cl))

;; Add the argument to load-path
(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))
(add-to-load-path "~/.emacs.d/conf")

;;  Add "$HOME/local/emacs/site-lisp" and its subdirectory to load-path
(defconst local-elisp-directory "~/local/lib/emacs" "The directory for my elisp file.")

(dolist (dir (let ((dir (expand-file-name local-elisp-directory)))
               (list dir (format "%s%d" dir emacs-major-version))))
  (when (and (stringp dir) (file-directory-p dir))
    (let ((default-directory dir))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;; eval-safe
;; http://www.sodan.org/~knagano/emacs/dotemacs.html#eval-safe
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;; Load settings

(load "init-global")
(load "init-window")
(load "init-keymaps")
(load "init-color")
(load "init-shell")
(load "init-languages")
(load "init-flymake")

(load "init-fortran")
(load "init-package")
(load "init-ruby")
(load "init-lua")
(load "init-csharp")
(load "init-scala")
(load "init-haskell")
(load "init-scheme")
(load "init-golang")
(load "init-gnuplot")
(load "init-octave")
(load "init-ffpp")

(load "init-anything")
(load "init-yatex")
;;(load "init-ecb")
;;(load "init-howm")
(load "init-twitter")
;(load "init-auto-install")
(load "init-ispell")
(load "init-tramp")
;;(load "init-ess")

;;(load "init-highlighting")
;;(load "init-minibuf")
;;(load "init-killring")
;;(load "init-abbrves")
;;(load "init-templates")
;;(load "init-dired")
;;(load "init-migemo")
;;(load "init-tags")
;;(load "init-vcs")
;;(load "init-html")
;;(load "init-css")
;;(load "init-javascript")
;;(load "init-c")
;;(load "init-perl")
;;(load "init-perlysense")
;(load "init-fetchmail")
;;(load "init-php")
;;(load "init-lisp")
;;(load "init-elscreen")
;;(load "init-taskpaper")

;; For Meadow
(when (and run-w32 run-meadow)
  (load "init-meadow") )

;; For Linux
(when run-linux
  (load "init-linux"))

;; For Cygwin
(when run-cygwin
  (load "init-cygwin"))

;; For OS X
(when run-darwin
  (load "init-mac"))


(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)