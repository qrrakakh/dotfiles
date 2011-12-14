;; -*- coding: iso-2022-jp-3  -*-
;;; jisx0213-util.el
;;;     --- Some data and utility for JIS X 0213

;; Copyright (C) 2000 KAWABATA, Taichi <batta@beige.ocn.ne.jp>

;; Keywords: Tamago, egg, multilingual, JIS X 0213

(require 'x0213-csys)

;; $(OITK~!'(B translate-string$(O4X?t$,$"$l$P!"$3$s$J(BCCL$(O$OITMW$@$C$?!D(B

(define-ccl-program jisx0213-to-jisx0208
  `(1
    (loop
     (read-multibyte-character r0 r1)
     (translate-character jisx0213-to-jisx0208/0212 r0 r1)
     (write-multibyte-character r0 r1)
     (repeat))))

(define-ccl-program jisx0208-to-jisx0213
  `(1
    (loop
     (read-multibyte-character r0 r1)
     (translate-character jisx0208-to-jisx0213 r0 r1)
     (write-multibyte-character r0 r1)
     (repeat))))

(defun jisx0213-to-jisx0208-string (string)
  (ccl-execute-on-string 'jisx0213-to-jisx0208 
                         (make-vector 9 nil) string))

(defun jisx0208-to-jisx0213-string (string)
  (ccl-execute-on-string 'jisx0208-to-jisx0213
                         (make-vector 9 nil) string))

(defun make-jisx0213-char-list (from to)
  (setq from (string-to-char
              (jisx0208-to-jisx0213-string
               (char-to-string from))))
  (setq to   (string-to-char
              (jisx0208-to-jisx0213-string
               (char-to-string to))))
  (mapcar '(lambda (x)
             (let ((split (split-char x)))
               (setcar split 'japanese-jisx0213-1)
               (apply 'make-char split)))
          (make-chars-list from to)))

;; JIS X 0213 $(OIUB0=q(B 4 $(O$K4p$E$/9g@.2DG=$JJ8;z72(B

(defvar jisx0213-combining-chars
  `(?$(O+R(B ?$(O+W(B 
    ,@(make-jisx0213-char-list ?$(O+Y(B?$(O+_(B)
    ,@(make-jisx0213-char-list ?$(O+g(B?$(O+~(B)))

(provide 'x0213-util)
