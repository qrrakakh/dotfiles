;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 実行環境を判別する。
;; http://d.hatena.ne.jp/hito-d/20060220#1140445790

;; OSを判別
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (or (equal system-type 'usg-unix-v)
          (or  (equal system-type 'berkeley-unix)
               (equal system-type 'cygwin)))))

(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwinもunixグループにしておく
  (equal system-type 'cygwin))

(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))

(defvar run-darwin (equal system-type 'darwin))

;; Emacsenの種類とヴァージョンを判別
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

;; ユーティリティ関数

;; [2008-03-13]
;; clはどこで使ってるかわからんので、とりあえずrequireしとく。

(eval-when-compile
  (require 'cl))

;; [2008-03-13]
;; add-to-load-path追加

;; set load-path to ~/.emacs.d/packages
(defun my-site-lisp-registration (directory)
  (let ((dirs nil))
    (mapcar (lambda (x)
              (and (file-directory-p x)
                   (not (member (file-name-nondirectory x) '("CVS" "cvs" ".svn")))
                   (setq load-path (cons (file-name-as-directory x) load-path))
                   (my-site-lisp-registration x) dirs))
            (directory-files directory t "^[^\.]"))
    dirs))

(let ((my-site-lisp-dir (convert-standard-filename "~/.emacs.d/packages")))
  (if (file-directory-p my-site-lisp-dir)
      (my-site-lisp-registration my-site-lisp-dir)))

;; 引数をload-pathへ追加する
(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))

;; elispと設定ファイルのディレクトリをload-pathに追加
(setq load-path
      (append (loop with root = (expand-file-name "~/.emacs.d/elisp")
		    with rest = `(,root)
		    with cur
		    while rest
		    do (setq cur (pop rest))
		    if (and (not (member cur '("." "..")))
			    (file-directory-p cur))
		    collect (progn (setq rest (append (mapcar (lambda (s) (concat root "/" s))
							      (remove-if (lambda (p) (member p '("." "..")))
									 (directory-files cur))) rest))
				   cur))
	      load-path))

(add-to-load-path "~/.emacs.d/conf")

;; eval-safe
;; 安全な評価。評価に失敗してもそこで止まらない。
;; http://www.sodan.org/~knagano/emacs/dotemacs.html#eval-safe
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;; 個別の設定をロードしまくりパート

;; 特定ディレクトリ以下を自動でロードするようにしてもいいけど、順番とか、
;; これやっぱ外しておこうとかいうのを調整するのが面倒。

(load "init-global")
(load "init-window")
(load "init-keymaps")
(load "init-color")
(load "init-shell")
(load "init-languages")
(load "init-flymake")

(load "init-fortran")
(load "init-ruby")
(load "init-csharp")
(load "init-scala")
(load "init-haskell")
(load "init-scheme")
(load "init-golang")
(load "init-gnuplot")
(load "init-octave")
(load "init-ffpp")

(load "init-yatex")
;;(load "init-ecb")
;;(load "init-howm")
(load "init-twitter")
(load "init-auto-install")
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
;;(load "init-yaml")
;;(load "init-hatena")
;;(load "init-elscreen")
;;(load "init-taskpaper")
;; [2008-03-13]
;; mmm-modeってば、なんか動かないんだよなー。

;; Meadow用設定を読み込む
(when (and run-w32 run-meadow)
  (load "init-meadow") )

;; Linux用個別設定
(when run-linux
  (load "init-linux"))

;; Cygwin Settings
(when run-cygwin
  (load "init-cygwin"))

;; Mac用設定を読み込む
(when run-darwin
  (load "init-mac"))


(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)
