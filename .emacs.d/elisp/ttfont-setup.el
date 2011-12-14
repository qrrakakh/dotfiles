;;; -*- syntax: elisp; coding: iso-2022-jp-unix; -*-

;; Copyright (C) 2003, 2004 Yasutaka SHINDOH (新堂安孝) <ring-pub at fan.gr.jp>

;; Author: Yasutaka SHINDOH (新堂安孝) <ring-pub at fan.gr.jp>
;; Keywords: Meadow TrueType Font

;;; Usage:

;; (when (require 'ttfont-setup nil t)
;;   (ttfont-setup))

;;; Code:

;; Default で使用する TrueType Fonts 名とその設定
(defvar ttfont-setup-def "VL Gothic")
(defvar ttfont-setup-def-number 49)
(defvar ttfont-setup-def-size 16)
(defvar ttfont-setup-fix "VL Gothic")
(defvar ttfont-setup-fix-width 'normal)
(defvar ttfont-setup-fix-height 1.0)
(defvar ttfont-setup-var "VL PGothic")
(defvar ttfont-setup-var-width 'normal)
(defvar ttfont-setup-var-height 1.0)

;; Font Set の作成と登録
(defun ttfont-setup (&optional size unuse font number set)
  (interactive)
  (let ((fn (or font ttfont-setup-def))
	(n (or number ttfont-setup-def-number))
	(s (- (or size ttfont-setup-def-size))))
    (let ((fs (concat (or set fn) " / " (number-to-string (- s)))))
      (w32-add-font
       fs
       `((spec
	  ;; ascii
	  ((:char-spec ascii :height any)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 nil nil nil 0 1 3 ,n))
	  ((:char-spec ascii :height any :weight bold)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 nil nil nil 0 1 3 ,n)
	   ((spacing . -1)))
	  ((:char-spec ascii :height any :weight normal :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 t nil nil 0 1 3 ,n))
	  ((:char-spec ascii :height any :weight bold :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 t nil nil 0 1 3 ,n)
	   ((spacing . -1)))
	  ;; katakana-jisx0201
	  ((:char-spec katakana-jisx0201 :height any)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 nil nil nil 128 1 3 ,n)
	   ((encoding . 1-byte-set-msb)))
	  ((:char-spec katakana-jisx0201 :height any :weight bold)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 nil nil nil 128 1 3 ,n)
	   ((encoding . 1-byte-set-msb) (spacing . -1)))
	  ((:char-spec katakana-jisx0201 :height any :weight normal :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 t nil nil 128 1 3 ,n)
	   ((encoding . 1-byte-set-msb)))
	  ((:char-spec katakana-jisx0201 :height any :weight bold :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 t nil nil 128 1 3 ,n)
	   ((encoding . 1-byte-set-msb) (spacing . -1)))
	  ;; japanese-jisx0208
	  ((:char-spec japanese-jisx0208 :height any)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 nil nil nil 128 1 3 ,n))
	  ((:char-spec japanese-jisx0208 :height any :weight bold)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 nil nil nil 128 1 3 ,n)
	   ((spacing . -1)))
	  ((:char-spec japanese-jisx0208 :height any :weight normal :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 400 0 t nil nil 128 1 3 ,n))
	  ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
	   strict
	   (w32-logfont ,fn 0 ,s 700 0 t nil nil 128 1 3 ,n)
	   ((spacing . -1))))))
    (unless unuse
      ;; フォントセットを使う
      (add-to-list 'default-frame-alist (cons 'font fs))
      ;; IME のフォント
      (add-to-list 'default-frame-alist
		   `(ime-font
		     w32-logfont ,fn 0 0 400 0 nil nil nil 128 1 3 ,n)))
      ;; その他
      (setq scalable-fonts-allowed t)
      (set-face-attribute 'fixed-pitch nil
			  :family ttfont-setup-fix
			  :width ttfont-setup-fix-width
			  :height ttfont-setup-fix-height)
      (set-face-attribute 'variable-pitch nil
			  :family ttfont-setup-var
			  :width ttfont-setup-var-width
			  :height ttfont-setup-var-height))))

;; provide
(provide 'ttfont-setup)

;;; end
