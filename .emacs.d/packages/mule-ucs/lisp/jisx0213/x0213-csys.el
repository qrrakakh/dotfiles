;;  -*- coding: iso-2022-7bit  -*-
;;;  x0213-csys.el --- Coding System Definition for JIS X 0213.

;; Copyright (C) 2000 KAWABATA, Taichi
;;                    Miyashita Hisashi

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, JIS X 0213

;; This program defines coding-system described in JIS X 0213 standard.

(eval-when-compile (require 'cl))
(require 'x0213-cdef)

(eval-when-compile
  (require 'x0213-sjis))

(eval-and-compile
  (defun make-list-of-range (from to)
    "Return the list of integers ranging from FROM to TO."
    (let (result)
      (while (<= from to)
	(setq result (cons to result)
	      to (1- to)))
      result))

;;; character list maker.
;;; only for 94x94 characters.
  (defun make-chars-list (from to)
    (let* ((from-split (split-char from))
	   (from-cs    (car from-split))
	   (from-row   (- (elt from-split 1) 33))
	   (from-col   (- (elt from-split 2) 33))
	   (from-num   (+ (* 94 from-row) from-col))
	   (to-split (split-char to))
	   (to-row   (- (elt to-split 1) 33))
	   (to-col   (- (elt to-split 2) 33))
	   (to-num   (+ (* 94 to-row) to-col))
	   table)
      (while (<= from-num to-num)
	(setq table
	      (cons (make-char from-cs
			       (+ (/ to-num 94) 33)
			       (+ (% to-num 94) 33))
		    table))
	(setq to-num (1- to-num)))
      table))

  (defun make-jisx0208-to-0213-translation-pair (char)
    (let* ((split (split-char char))
           (x (cadr split))
           (y (caddr split)))
      (list (cons (make-char 'japanese-jisx0208 x y)
                  (make-char 'japanese-jisx0213-1 x y)))))

  (defun make-jisx0208-to-0213-translation-pairs (from to)
    (let* ((table (make-chars-list from to)))
      (mapcar '(lambda (char)
                 (let* ((split (split-char char))
                        (x (cadr split))
                        (y (caddr split)))
                   (cons (make-char 'japanese-jisx0208 x y)
                         (make-char 'japanese-jisx0213-1 x y))))
              table)))

  (defun make-jisx0213-to-0208-translation-pairs (from to)
    (let* ((table (make-chars-list from to)))
      (mapcar '(lambda (char) 
		 (let* ((split (split-char char))
                        (x (cadr split))
                        (y (caddr split)))
		   (cons (make-char 'japanese-jisx0213-1 x y)
                         (make-char 'japanese-jisx0208 x y))))
	      table))))

(eval-when-compile
  (define-translation-table
    'jisx0208-to-jisx0213
    nil)
  (define-translation-table
    'jisx0208/0212-to-jisx0213
    nil)
  (define-translation-table
    'jisx0213-to-jisx0208/0212
    nil)
  (define-translation-table
    'jisx0208-to-jisx0213-restricted
    nil))

;; translation table

(define-translation-table 
  'jisx0208-to-jisx0213
  (list (cons (make-char 'japanese-jisx0208)
              (make-char 'japanese-jisx0213-1))))

(define-translation-table 
 'jisx0208/0212-to-jisx0213
  (list (cons (make-char 'japanese-jisx0208)
              (make-char 'japanese-jisx0213-1))
        (cons (make-char 'japanese-jisx0212)
              (make-char 'japanese-jisx0213-2))))

(define-translation-table
  'jisx0213-to-jisx0208/0212
  (eval-when-compile
    (make-translation-table
     (nconc 
      (mapcar '(lambda (x) 
                 (cons (make-char 'japanese-jisx0213-1 (+ 32 x))
                       (make-char 'japanese-jisx0208 (+ 32 x))))
              `(1 ,@(make-list-of-range 16 46)
                  ,@(make-list-of-range 48 83)))
      (make-jisx0213-to-0208-translation-pairs ?$B"!(B ?$B".(B)
      (make-jisx0213-to-0208-translation-pairs ?$B":(B ?$B"A(B)
      (make-jisx0213-to-0208-translation-pairs ?$B"J(B ?$B"P(B)
      (make-jisx0213-to-0208-translation-pairs ?$B"\(B ?$B"j(B)
      (make-jisx0213-to-0208-translation-pairs ?$B"r(B ?$B"y(B)
      (make-jisx0213-to-0208-translation-pairs ?$B"~(B ?$B"~(B)
      (make-jisx0213-to-0208-translation-pairs ?$B#0(B ?$B#9(B)
      (make-jisx0213-to-0208-translation-pairs ?$B#A(B ?$B#Z(B)
      (make-jisx0213-to-0208-translation-pairs ?$B#a(B ?$B#z(B)
      (make-jisx0213-to-0208-translation-pairs ?$B$!(B ?$B$s(B)
      (make-jisx0213-to-0208-translation-pairs ?$B%!(B ?$B%v(B)
      (make-jisx0213-to-0208-translation-pairs ?$B&!(B ?$B&8(B)
      (make-jisx0213-to-0208-translation-pairs ?$B&A(B ?$B&X(B)
      (make-jisx0213-to-0208-translation-pairs ?$B'!(B ?$B'A(B)
      (make-jisx0213-to-0208-translation-pairs ?$B'Q(B ?$B'q(B)
      (make-jisx0213-to-0208-translation-pairs ?$B(!(B ?$B(@(B)
      (make-jisx0213-to-0208-translation-pairs ?$BO!(B ?$BOS(B)
      (make-jisx0213-to-0208-translation-pairs ?$Bt!(B ?$Bt&(B)
      (mapcar '(lambda (x) 
                 (cons (make-char 'japanese-jisx0213-2 (+ 32 x))
                       (make-char 'japanese-jisx0212 (+ 32 x))))
              `(2 6 7 9 10 11
                  ,@(make-list-of-range 16 77)))))))

;; The following translation table assures that JIS X 0208 characters
;; prohibited in ISO-2022-JP-3 encoding will all be translated to
;; equivalent JIS X 0213 characters.  
(define-translation-table
  'jisx0208-to-jisx0213-restricted
  (eval-when-compile
    (make-translation-table
     (nconc 
      (make-jisx0208-to-0213-translation-pairs ?$(O"/(B ?$(O#/(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O#:(B ?$(O#@(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O#[(B ?$(O#`(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O#{(B ?$(O#~(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O$t(B ?$(O${(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O%w(B ?$(O%~(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O&9(B ?$(O&@(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O&Y(B ?$(O&~(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O'B(B ?$(O'P(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O'r(B ?$(O(^(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O(g(B ?$(O(|(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O)!(B ?$(O,s(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O,}(B ?$(O-W(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O-_(B ?$(O-o(B)
      (make-jisx0208-to-0213-translation-pair ?$(O-s(B)
      (make-jisx0208-to-0213-translation-pair ?$(O-x(B)
      (make-jisx0208-to-0213-translation-pair ?$(O-y(B)
      (make-jisx0208-to-0213-translation-pair ?$(O-}(B)
      (make-jisx0208-to-0213-translation-pair ?$(O-~(B)
      (make-jisx0208-to-0213-translation-pairs ?$(O."(B ?$(O/}(B)
      (make-jisx0208-to-0213-translation-pair ?$(O0"(B)
      (make-jisx0208-to-0213-translation-pair ?$(O03(B)
      (make-jisx0208-to-0213-translation-pair ?$(O0o(B)
      (make-jisx0208-to-0213-translation-pair ?$(O1Z(B)
      (make-jisx0208-to-0213-translation-pair ?$(O1k(B)
      (make-jisx0208-to-0213-translation-pair ?$(O1o(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2#(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2)(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2*(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2+(B)
      (make-jisx0208-to-0213-translation-pair ?$(O29(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2R(B)
      (make-jisx0208-to-0213-translation-pair ?$(O2y(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3$(B)
      (make-jisx0208-to-0213-translation-pair ?$(O34(B)
      (make-jisx0208-to-0213-translation-pair ?$(O35(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3B(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3I(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3e(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3i(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3l(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3v(B)
      (make-jisx0208-to-0213-translation-pair ?$(O3z(B)
      (make-jisx0208-to-0213-translation-pair ?$(O42(B)
      (make-jisx0208-to-0213-translation-pair ?$(O4A(B)
      (make-jisx0208-to-0213-translation-pair ?$(O4C(B)
      (make-jisx0208-to-0213-translation-pair ?$(O4R(B)
      (make-jisx0208-to-0213-translation-pair ?$(O4o(B)
      (make-jisx0208-to-0213-translation-pair ?$(O4{(B)
      (make-jisx0208-to-0213-translation-pair ?$(O5'(B)
      (make-jisx0208-to-0213-translation-pair ?$(O5u(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6"(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6?(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6A(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6F(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6P(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6`(B)
      (make-jisx0208-to-0213-translation-pair ?$(O6m(B)
      (make-jisx0208-to-0213-translation-pair ?$(O70(B)
      (make-jisx0208-to-0213-translation-pair ?$(O7G(B)
      (make-jisx0208-to-0213-translation-pair ?$(O7[(B)
      (make-jisx0208-to-0213-translation-pair ?$(O7b(B)
      (make-jisx0208-to-0213-translation-pair ?$(O8&(B)
      (make-jisx0208-to-0213-translation-pair ?$(O84(B)
      (make-jisx0208-to-0213-translation-pair ?$(O9\(B)
      (make-jisx0208-to-0213-translation-pair ?$(O9m(B)
      (make-jisx0208-to-0213-translation-pair ?$(O9r(B)
      (make-jisx0208-to-0213-translation-pair ?$(O9u(B)
      (make-jisx0208-to-0213-translation-pair ?$(O;&(B)
      (make-jisx0208-to-0213-translation-pair ?$(O;c(B)
      (make-jisx0208-to-0213-translation-pair ?$(O;k(B)
      (make-jisx0208-to-0213-translation-pair ?$(O<H(B)
      (make-jisx0208-to-0213-translation-pair ?$(O<I(B)
      (make-jisx0208-to-0213-translation-pair ?$(O<Q(B)
      (make-jisx0208-to-0213-translation-pair ?$(O<R(B)
      (make-jisx0208-to-0213-translation-pair ?$(O<T(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=+(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=-(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=K(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=k(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=m(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=o(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=p(B)
      (make-jisx0208-to-0213-translation-pair ?$(O=t(B)
      (make-jisx0208-to-0213-translation-pair ?$(O>D(B)
      (make-jisx0208-to-0213-translation-pair ?$(O>M(B)
      (make-jisx0208-to-0213-translation-pair ?$(O>U(B)
      (make-jisx0208-to-0213-translation-pair ?$(O>_(B)
      (make-jisx0208-to-0213-translation-pair ?$(O>u(B)
      (make-jisx0208-to-0213-translation-pair ?$(O?@(B)
      (make-jisx0208-to-0213-translation-pair ?$(O?Y(B)
      (make-jisx0208-to-0213-translation-pair ?$(O@%(B)
      (make-jisx0208-to-0213-translation-pair ?$(O@a(B)
      (make-jisx0208-to-0213-translation-pair ?$(O@f(B)
      (make-jisx0208-to-0213-translation-pair ?$(OA((B)
      (make-jisx0208-to-0213-translation-pair ?$(OAD(B)
      (make-jisx0208-to-0213-translation-pair ?$(OAN(B)
      (make-jisx0208-to-0213-translation-pair ?$(OAX(B)
      (make-jisx0208-to-0213-translation-pair ?$(OA_(B)
      (make-jisx0208-to-0213-translation-pair ?$(OAc(B)
      (make-jisx0208-to-0213-translation-pair ?$(OA}(B)
      (make-jisx0208-to-0213-translation-pair ?$(OA~(B)
      (make-jisx0208-to-0213-translation-pair ?$(OB#(B)
      (make-jisx0208-to-0213-translation-pair ?$(OB((B)
      (make-jisx0208-to-0213-translation-pair ?$(OBM(B)
      (make-jisx0208-to-0213-translation-pair ?$(OBv(B)
      (make-jisx0208-to-0213-translation-pair ?$(OC2(B)
      (make-jisx0208-to-0213-translation-pair ?$(OC=(B)
      (make-jisx0208-to-0213-translation-pair ?$(OCv(B)
      (make-jisx0208-to-0213-translation-pair ?$(OCx(B)
      (make-jisx0208-to-0213-translation-pair ?$(OD'(B)
      (make-jisx0208-to-0213-translation-pair ?$(OD((B)
      (make-jisx0208-to-0213-translation-pair ?$(ODM(B)
      (make-jisx0208-to-0213-translation-pair ?$(ODO(B)
      (make-jisx0208-to-0213-translation-pair ?$(OD[(B)
      (make-jisx0208-to-0213-translation-pair ?$(ODw(B)
      (make-jisx0208-to-0213-translation-pair ?$(OE6(B)
      (make-jisx0208-to-0213-translation-pair ?$(OE?(B)
      (make-jisx0208-to-0213-translation-pair ?$(OET(B)
      (make-jisx0208-to-0213-translation-pair ?$(OEW(B)
      (make-jisx0208-to-0213-translation-pair ?$(OEn(B)
      (make-jisx0208-to-0213-translation-pair ?$(OEs(B)
      (make-jisx0208-to-0213-translation-pair ?$(OEx(B)
      (make-jisx0208-to-0213-translation-pair ?$(OFA(B)
      (make-jisx0208-to-0213-translation-pair ?$(OFB(B)
      (make-jisx0208-to-0213-translation-pair ?$(OFM(B)
      (make-jisx0208-to-0213-translation-pair ?$(OFq(B)
      (make-jisx0208-to-0213-translation-pair ?$(OFv(B)
      (make-jisx0208-to-0213-translation-pair ?$(OG9(B)
      (make-jisx0208-to-0213-translation-pair ?$(OG_(B)
      (make-jisx0208-to-0213-translation-pair ?$(OGh(B)
      (make-jisx0208-to-0213-translation-pair ?$(OH.(B)
      (make-jisx0208-to-0213-translation-pair ?$(OH0(B)
      (make-jisx0208-to-0213-translation-pair ?$(OHK(B)
      (make-jisx0208-to-0213-translation-pair ?$(OHU(B)
      (make-jisx0208-to-0213-translation-pair ?$(OH\(B)
      (make-jisx0208-to-0213-translation-pair ?$(OHj(B)
      (make-jisx0208-to-0213-translation-pair ?$(OI0(B)
      (make-jisx0208-to-0213-translation-pair ?$(OIP(B)
      (make-jisx0208-to-0213-translation-pair ?$(OIQ(B)
      (make-jisx0208-to-0213-translation-pair ?$(OIR(B)
      (make-jisx0208-to-0213-translation-pair ?$(OIS(B)
      (make-jisx0208-to-0213-translation-pair ?$(OIn(B)
      (make-jisx0208-to-0213-translation-pair ?$(OJ!(B)
      (make-jisx0208-to-0213-translation-pair ?$(OJ;(B)
      (make-jisx0208-to-0213-translation-pair ?$(OJ=(B)
      (make-jisx0208-to-0213-translation-pair ?$(OJY(B)
      (make-jisx0208-to-0213-translation-pair ?$(OJb(B)
      (make-jisx0208-to-0213-translation-pair ?$(OKK(B)
      (make-jisx0208-to-0213-translation-pair ?$(OKO(B)
      (make-jisx0208-to-0213-translation-pair ?$(OKh(B)
      (make-jisx0208-to-0213-translation-pair ?$(OKj(B)
      (make-jisx0208-to-0213-translation-pair ?$(OKy(B)
      (make-jisx0208-to-0213-translation-pair ?$(OLH(B)
      (make-jisx0208-to-0213-translation-pair ?$(OLM(B)
      (make-jisx0208-to-0213-translation-pair ?$(OLa(B)
      (make-jisx0208-to-0213-translation-pair ?$(OLy(B)
      (make-jisx0208-to-0213-translation-pair ?$(OM4(B)
      (make-jisx0208-to-0213-translation-pair ?$(OMZ(B)
      (make-jisx0208-to-0213-translation-pair ?$(OMi(B)
      (make-jisx0208-to-0213-translation-pair ?$(OMj(B)
      (make-jisx0208-to-0213-translation-pair ?$(OMs(B)
      (make-jisx0208-to-0213-translation-pair ?$(ON4(B)
      (make-jisx0208-to-0213-translation-pair ?$(ON:(B)
      (make-jisx0208-to-0213-translation-pair ?$(ONP(B)
      (make-jisx0208-to-0213-translation-pair ?$(ON^(B)
      (make-jisx0208-to-0213-translation-pair ?$(ON`(B)
      (make-jisx0208-to-0213-translation-pair ?$(ONq(B)
      (make-jisx0208-to-0213-translation-pair ?$(ONr(B)
      (make-jisx0208-to-0213-translation-pair ?$(ON}(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO#(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO-(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO/(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO6(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO9(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO:(B)
      (make-jisx0208-to-0213-translation-pair ?$(OO?(B)
      (make-jisx0208-to-0213-translation-pairs ?$(OOU(B ?$(OO}(B)
      (make-jisx0208-to-0213-translation-pair ?$(OPV(B)
      (make-jisx0208-to-0213-translation-pair ?$(OTd(B)
      (make-jisx0208-to-0213-translation-pair ?$(OYx(B)
      (make-jisx0208-to-0213-translation-pair ?$(OZ9(B)
      (make-jisx0208-to-0213-translation-pair ?$(O[X(B)
      (make-jisx0208-to-0213-translation-pair ?$(O[m(B)
      (make-jisx0208-to-0213-translation-pair ?$(O^9(B)
      (make-jisx0208-to-0213-translation-pair ?$(O^u(B)
      (make-jisx0208-to-0213-translation-pair ?$(O_f(B)
      (make-jisx0208-to-0213-translation-pair ?$(O`v(B)
      (make-jisx0208-to-0213-translation-pair ?$(Obh(B)
      (make-jisx0208-to-0213-translation-pair ?$(Obj(B)
      (make-jisx0208-to-0213-translation-pair ?$(Oc^(B)
      (make-jisx0208-to-0213-translation-pair ?$(OdF(B)
      (make-jisx0208-to-0213-translation-pair ?$(Oi"(B)
      (make-jisx0208-to-0213-translation-pair ?$(Oi.(B)
      (make-jisx0208-to-0213-translation-pair ?$(OiZ(B)
      (make-jisx0208-to-0213-translation-pair ?$(Oj$(B)
      (make-jisx0208-to-0213-translation-pair ?$(Ok](B)
      (make-jisx0208-to-0213-translation-pair ?$(OlM(B)
      (make-jisx0208-to-0213-translation-pair ?$(Omn(B)
      (make-jisx0208-to-0213-translation-pair ?$(OpW(B)
      (make-jisx0208-to-0213-translation-pair ?$(Opt(B)
      (make-jisx0208-to-0213-translation-pair ?$(OrM(B)
      (make-jisx0208-to-0213-translation-pair ?$(Ort(B)
      (make-jisx0208-to-0213-translation-pairs ?$(Ot!(B ?$(Ot&(B)
      (make-jisx0208-to-0213-translation-pairs ?$(Ot((B ?$(O~y(B)
      ))))

;;;
;;; JIS X 0213$B$N(BISO-2022$B7OE}$N(Bcoding-system$B$NDj5A(B
;;;

(make-coding-system
 'iso-2022-jp-3-compatible 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3),
compatible to ISO-2022-JP."
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   ;; All JIS X 0213 characters compatible to JIS X 0208 will be
   ;; translated to JIS X 0208 equivalents before encoding.
   (translation-table-for-encode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'iso-2022-jp-3-strict 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3),
where JIS X 0208 characters would be encoded as ESC $ B as possible as
it can."
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   (translation-table-for-encode . ,(get 'jisx0208-to-jisx0213-restricted
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'iso-2022-jp-3 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3)"
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   ;; All JIS X 0208 characters will be translated to JIS X 0213
   ;; equivalents before encoding.
   (translation-table-for-encode . ,(get 'jisx0208/0212-to-jisx0213 
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'euc-jisx0213 2 ?E
 "ISO 2022 based EUC encoding for JIS X 0213 (MIME:EUC-JISX0213)"
 '(ascii japanese-jisx0213-1 katakana-jisx0201 japanese-jisx0213-2
   short ascii-eol ascii-cntl nil nil single-shift)
 `((safe-charsets ascii katakana-jisx0201 japanese-jisx0208 
                  japanese-jisx0212
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . euc-jisx0213)
   (translation-table-for-encode . ,(get 'jisx0208/0212-to-jisx0213 
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

;;;
;;; Shift-JIS
;;;
(eval-and-compile
  (register-code-conversion-map
   'jisx0213-shift-jis-plain-2-odd-decode-map
   (apply (function vector)
	  ?\xF0
	  (mapcar
	   (lambda (x) (+ x 32))
	   '(1 3 5 13 15 79 81 83 85 87 89 91 93))))
  (register-code-conversion-map
   'jisx0213-shift-jis-plain-2-even-decode-map
   (apply (function vector)
	  ?\xF0
	  (mapcar
	   (lambda (x) (+ x 32))
	   '(8 4 12 14 78 80 82 84 86 88 90 92 94)))))

(defvar shift-jisx0213-coding-system-alist
  '((safe-charsets .
		   (ascii
		    japanese-jisx0208
		    katakana-jisx0201
		    japanese-jisx0213-1
		    japanese-jisx0213-2))
    (mime-charset . shift_jisx0213))
  "An alist for japanese-shift-jisx0213 coding systems.")

(eval-when-compile
  (defun jisx0213-shift-jis-template (tr-table read write &optional macp)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-2 read)
     (if macp
	 (if (eq read 'emacs-mule)
	     '((if (r0 == ?\x0A) ((r0 = ?\x0D))))
	   '((if (r0 == ?\x0D) ((r0 = ?\x0A))))))
     `((translate-character ,tr-table r1 r0))
     (mucs-ccl-write write))))

(mucs-define-package
 x0213-csys

 (mucs-define-conversion
  shift-jisx0213-unix-stream-encoder
  stream
  (1 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis))))

 (mucs-define-conversion
  shift-jisx0213-unix-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule))))

 (mucs-define-conversion
  shift-jisx0213-dos-stream-encoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis-dos))))

 (mucs-define-conversion
  shift-jisx0213-dos-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule-dos))))

 (mucs-define-conversion
  shift-jisx0213-mac-stream-encoder
  stream
  (1 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis t))))

 (mucs-define-conversion
  shift-jisx0213-mac-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule t))))

 ;;coding system definition

 (mucs-define-coding-system
  'japanese-shift-jisx0213-unix ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-unix-stream-decoder
  'shift-jisx0213-unix-stream-encoder
  shift-jisx0213-coding-system-alist
  'unix)

 (mucs-define-coding-system
  'japanese-shift-jisx0213-dos ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-dos-stream-decoder
  'shift-jisx0213-dos-stream-encoder
  shift-jisx0213-coding-system-alist
  'dos)

 (mucs-define-coding-system
  'japanese-shift-jisx0213-mac ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-mac-stream-decoder
  'shift-jisx0213-mac-stream-encoder
  shift-jisx0213-coding-system-alist
  'mac)

 (mucs-define-coding-system
  'japanese-shift-jisx0213 ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-unix-stream-decoder
  'shift-jisx0213-unix-stream-encoder
  shift-jisx0213-coding-system-alist
  [japanese-shift-jisx0213-unix
   japanese-shift-jisx0213-dos
   japanese-shift-jisx0213-mac])

 (mapcar
  (lambda (x)
    (let ((master (car x))
	  (aliases (cdr x)))
      (coding-system-put master 'alias-coding-systems
			 '(japanese-shift-jisx0213))
      (while aliases
	(define-coding-system-alias
	  (car aliases) master)
	(setq aliases (cdr aliases)))))
  '((japanese-shift-jisx0213 shift_jisx0213)
    (japanese-shift-jisx0213-unix shift_jisx0213-unix)
    (japanese-shift-jisx0213-dos shift_jisx0213-dos)
    (japanese-shift-jisx0213-mac shift_jisx0213-mac)))
 )

;;
;; langauge-info-alist update.
;;

(coding-system-put 'japanese-shift-jisx0213 'coding-category
                   'coding-category-sjis)

(set-language-info "Japanese" 'coding-priority
		   (let ((cand
			  '(iso-2022-jp-3-compatible
			    utf-8 utf-16-le utf-16-be
			    euc-jisx0213 japanese-shift-jisx0213
			    iso-2022-jp-2))
			 cs catlist result)
		     (while cand
		       (setq cs (car cand)
			     cand (cdr cand))
		       (if (and (coding-system-p cs)
				(not (eq (coding-system-category cs)
					 'coding-category-ccl))
				(not (memq (coding-system-category cs)
					   catlist)))
			   (setq result (cons cs result)
				 catlist (cons (coding-system-category cs)
					       catlist))))
		     (nreverse result)))

(set-language-info "Japanese" 'coding-system 
                   '(iso-2022-jp euc-jisx0213 iso-2022-jp-3
		     japanese-shift-jisx0213
                     japanese-iso-8bit japanese-shift-jis 
                     japanese-iso-7bit-1978-irv iso-2022-jp-2))

(provide 'x0213-csys)
