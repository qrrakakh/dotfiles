;; -*- coding: iso-2022-jp-3  -*-
;;; egg-sim-jisx0213.el 
;;;     --- Egg Simple Input for JIS X 0213.

;; Copyright (C) 2000 KAWABATA, Taichi <batta@beige.ocn.ne.jp>

;; Keywords: Tamago, egg, multilingual, JIS X 0213

;; JIS X 0213 $(OIUB0=q(B 4 $(O$K4p$E$/5-9fN`J,N`!J<c43JQ99!K(B

(require 'egg-sim)
(require 'x0213-util)
(require 'x0213-char)

(defun make-0213char-list (from to)
  (mapcar '(lambda (x)
             (let ((split (split-char x)))
               (setcar split 'japanese-jisx0213-1)
               (jisx0213-to-jisx0208-string
                (char-to-string (apply 'make-char split)))))
          (make-chars-list from to)))

(defvar egg-sim-jisx0213-menu
  `("JISX0213" .
    (menu "JIS X 0213:" 
          (("JIS1$(OLLF~NO(B" . japanese-jisx0213-1)
           ("JIS2$(OLLF~NO(B" . japanese-jisx0213-1)
           ("$(O$+$J(B" . 
            (menu "$(O$R$i$,$J(B" ,(make-0213char-list ?$(O$!(B ?$(O${(B)))
           ("$(O%+%J(B" . 
            (menu "$(O%+%?%+%J(B" 
                  (,@(make-0213char-list ?$(O%!(B ?$(O%~(B)
                   ,@(make-0213char-list ?$(O&n(B ?$(O&~(B)
                   ,@(make-0213char-list ?$(O'r(B ?$(O'u(B))))
           ("$(O4V3V(B" .
            (menu "$(O4V3VJ8;z(B" (("$(O!!(B($(OOB(B)" . "$(O!!(B") 
                              ("$(O)"(B($(O2$(BNBSP)" . "$(O)"(B") "$(O'}(B")))
           ("$(O5-=R(B" .
            (menu "$(O5-=R5-9f(B" 
                  (("$(O2#K@(B" .
                    (menu "$(O%O%$%U%s!&%@%C%7%e(B"
                          (("$(O!>(B($(O;MJ,%O%$%U%s(B)" . "$(O!>(B")
                           ("$(O#|(B($(OFsJ,%@!<%7(B)" . "$(O#|(B")
                           ("$(O"1(B($(O%O%$%U%s%^%$%J%9(B)" . "$(O"1(B")
                           ("$(O))(B($(O%=%U%H%O%$%U%s(B)" . "$(O))(B") ;; $(O8_49J8;z(B
                           ("$(O!=(B($(O%@%C%7%e(B)" . "$(O!=(B")
                           ("$(O#{(B($(OFs=E%O%$%U%s(B)" . "$(O#{(B"))))
                    ,@(make-0213char-list ?$(O!"(B ?$(O!%(B)
                    ("$(O!&(B($(OOB(B)" . "$(O!&(B") ("$(O).(B($(O2$(B)" . "$(O).(B")
                    ,@(make-0213char-list ?$(O!'(B ?$(O!*(B)
                    "$(O)#(B" "$(O)6(B"
                    ,@(make-0213char-list ?$(O(k(B ?$(O(n(B)
                    ,@(make-0213char-list ?$(O!?(B ?$(O!E(B))))
           ("diacr." .
            (menu "$(O%@%$%"%/%j%F%#%+%k%^!<%/(B"
                  (("$(O!+(B($(OBy2;(B)" . "$(O!+(B")
                   ("$(O!,(B($(OH>By2;(B)" . "$(O!,(B")
                   ,@(make-0213char-list ?$(O!+(B?$(O!0(B)
                   ("$(O!1(B(overline)" . "$(O!1(B")
                   ("$(O)+(B(macron)" . "$(O)+(B")
                   "$(O!2(B" "$(O"2(B" "$(O)/(B" "$(O*"(B" "$(O*-(B" "$(O*1(B" "$(O*6(B" "$(O*X(B" "$(O+X(B"
                   ,@(make-0213char-list ?$(O+S(B ?$(O+V(B)
                   ,@(make-0213char-list ?$(O+`(B ?$(O+f(B))))
           ("diacr.($(O9g@.2D(B)" .
            (menu "$(O%@%$%"%/%j%F%#%+%k(B($(O9g@.2DG=(B)"
                  ,(mapcar 'char-to-string jisx0213-combining-chars)))
           ("$(O$+$J4A;z=`(B" .
            (menu "$(O$+$J!&4A;z$K=`$8$kJ8;z(B"
                  (,@(make-0213char-list ?$(O!3(B ?$(O!<(B)
                   ,@(make-0213char-list ?$(O"3(B ?$(O"9(B))))
           ("$(O3g8L(B" .
            (menu "$(O3g8L(B" 
                  (,@(make-0213char-list ?$(O!F(B ?$(O![(B)
                   ,@(make-0213char-list ?$(O"V(B ?$(O"[(B)
                   "$(O)((B" "$(O)2(B" "$(O-`(B" "$(O-a(B")))
           ("$(O3X=Q(B" . 
            (menu "$(O3X=Q5-9f(B"
                  (,@(make-0213char-list ?$(O!\(B ?$(O!j(B)
                   ,@(make-0213char-list ?$(O":(B ?$(O"U(B)
                   ,@(make-0213char-list ?$(O"\(B ?$(O"q(B)
                   ,@(make-0213char-list ?$(O#[(B ?$(O#](B)
                   "$(O#}(B" "$(O#~(B" "$(O'v(B" "$(O'w(B" "$(O-s(B" "$(O-x(B")))
           ("$(OC10L(B" .
            (menu "$(OC10L!&DL2_(B"
                  (("$(O!k(B($(OEY(B)" . "$(O!k(B")
                   ("$(O!l(B($(OJ,(B)" . "$(O!l(B")
                   ("$(O!m(B($(OIC(B)" . "$(O!m(B")
                   ,@(make-0213char-list ?$(O!n(B ?$(O!s(B)
                   ,@(make-0213char-list ?$(O#^(B ?$(O#`(B)
                   "$(O"r(B" "$(O"s(B" "$(O)!(B" "$(O)$(B"
                   ,@(make-0213char-list ?$(O-@(B ?$(O-V(B)))) ;; $(O8_49J8;z(B
           ;; $(O0lHL$H?^7A$N6hJL$O!"@nH($N(Bdiscretion
           ("$(O0lHL(B" .
            (menu "$(O0lHL(B"
                  (,@(make-0213char-list ?$(O!t(B ?$(O!x(B)
                   "$(O"((B" "$(O")(B" "$(O".(B" "$(O"y(B"
                   ("$(O"/(B($(OCfN)%"%]%9%H%m%U%#(B)" . "$(O"/(B")
                   ("$(O"0(B($(OCfN)0zMQId(B)" . "$(O"0(B")
                   "$(O"w(B" 
                   ("$(O"x(B($(O%@%V%k%@%,!<(B)" . "$(O"x(B")
                   ("$(O,}(B($(O%@%V%k%"%9%F(B)" . "$(O,}(B")
                   "$(O,~(B" 
                   "$(O)%(B" "$(O)&(B" "$(O)*(B" 
                   ("$(O)'(B($(O=w@-=x?t(B)" . "$(O)'(B")
                   ("$(O)1(B($(OCK@-=x?t(B)" . "$(O)1(B")
                   "$(O#=(B" "$(O#>(B" "$(O#?(B" "$(O#@(B"
                   "$(O'{(B" "$(O'}(B")))
           ("$(O?^7A(B" .
            (menu "$(O?^7A!&3((B"
                  (,@(make-0213char-list ?$(O!y(B ?$(O"'(B)
                   "$(O"~(B"
                   ,@(make-0213char-list ?$(O#!(B ?$(O#$(B)
                   "$(O#:(B" "$(O#;(B" "$(O#<(B"
                   ,@(make-0213char-list ?$(O&9(B ?$(O&@(B)
                   ,@(make-0213char-list ?$(O&d(B ?$(O&m(B)
                   ,@(make-0213char-list ?$(O(g(B ?$(O(j(B)
                   "$(O-y(B" "$(O-}(B" "$(O-~(B" "$(O'|(B" "$(O'~(B")))
           ("$(OLp0u(B" .
            (menu "$(OLp0u(B"
                  (,@(make-0213char-list ?$(O"*(B ?$(O"-(B)
                   ,@(make-0213char-list ?$(O#%(B ?$(O#/(B)
                  "$(O"q(B" "$(O"M(B" "$(O"N(B" "$(O'~(B"))) ;; $(O3X=Q5-9f!&?^7A$NLp0u(B
           ("$(O2;Id(B" .
            (menu "$(O2;Id(B"
                  (,@(make-0213char-list ?$(O"t(B ?$(O"v(B)
                   ,@(make-0213char-list ?$(O"z(B ?$(O"}(B))))
           ("$(O?t;z(B" .
            (menu "$(O?t;z!&J,?t(B" 
                  (("$(O?t;z(B" .
                    (menu "$(O?t;z(B" ,(make-0213char-list ?$(O#0(B ?$(O#9(B)))
                   ("$(O%m!<%^?t;z(B"  . 
                    (menu "$(O%m!<%^?t;z(B"
                          (,@(make-0213char-list ?$(O,5(B ?$(O,@(B)
                           ,@(make-0213char-list ?$(O-5(B ?$(O-?(B)
                           "$(O-W(B")))
                   ("$(O4]IU$-(B" . 
                    (menu "$(O4]IU$-?t;z(B" 
                          (,@(make-0213char-list ?$(O-!(B ?$(O-4(B)
                           ,@(make-0213char-list ?$(O(A(B ?$(O(^(B))))
                   ("$(O9u4](B" .
                    (menu "$(O9u4]IU$-?t;z(B" ,(make-0213char-list ?$(O,!(B ?$(O,4(B)))
                   ("$(OFs=E4](B" .
                    (menu "$(OFs=E4]IU$-?t;z(B" ,(make-0213char-list ?$(O&Z(B ?$(O&c(B)))
                   ("$(OJ,?t(B" .
                    (menu "$(OJ,?t(B"
                          (,@(make-0213char-list ?$(O)3(B ?$(O)5(B)
                           ,@(make-0213char-list ?$(O'x(B ?$(O'z(B))))
                   ("$(O>eIU$-(B" .
                    (menu "$(O>eIU$-(B" ("$(O)0(B" "$(O),(B" "$(O)-(B"))))))
           ("$(O4]IU$-J8;z(B" . 
            (menu "$(O4]IU$-J8;z(B"
                  (("$(O%i%F%s>.J8;z(B" .
                    (menu "$(O4]IU$-%i%F%s>.J8;z(B"
                          ,(make-0213char-list ?$(O,A(B ?$(O,Z(B)))
                   ("$(O%+%?%+%J(B" .
                    (menu "$(O4]IU$-%+%?%+%J(B"
                          ,(make-0213char-list ?$(O,[(B ?$(O,s(B)))
                   "$(O!w(B" "$(O"Q(B" "$(O"R(B" "$(O"S(B" "$(O)&(B" "$(O)*(B" 
                   ,@(make-0213char-list ?$(O-e(B ?$(O-i(B))))
           ("$(O859f(B" .
            (menu "$(O859f(B" ("$(O-_(B" "$(O-o(B" "$(O-n(B" "$(O-m(B")))
           ("$(ON,9f(B" . 
            (menu "$(ON,9f(B" 
                  (,@(make-0213char-list ?$(O-b(B ?$(O-d(B)
                   ,@(make-0213char-list ?$(O-j(B ?$(O-l(B))))
           ("$(O7S@~!&;u2J(B" . 
            (menu "$(O7S@~!&;u2J5-9f(B" 
                  (,@(make-0213char-list ?$(O(!(B ?$(O(@(B)
                   ,@(make-0213char-list ?$(O'B(B ?$(O'P(B))))
           ("$(O%i%F%sJ8;z(B" .
            (menu "$(O%i%F%sJ8;z(B" 
                  (("$(O4pK\%i%F%sJ8;z(B" .
                    (menu "$(O4pK\%i%F%sJ8;z(B"
                          (,@(make-0213char-list ?$(O#A(B ?$(O#Z(B)
                           ,@(make-0213char-list ?$(O#a(B ?$(O#z(B))))
                   ("$(O=$>~IU$-4pK\%i%F%sBgJ8;z(B" .
                    (menu "$(O=$>~IU$-4pK\%i%F%sBgJ8;z(B"
                          ,(mapcar 
                            'char-to-string
                            (sort-char-by-charname
                             `(?$(O(r(B ?$(O(t(B
                               ,@(make-chars-list ?$(O)7(B ?$(O)S(B)
                               ,@(make-chars-list ?$(O)u(B ?$(O)y(B)
                               ?$(O*!(B
                               ,@(make-chars-list ?$(O*#(B ?$(O*+(B)
                               ,@(make-chars-list ?$(O*9(B ?$(O*G(B)
                               ,@(make-chars-list ?$(O*Y(B ?$(O*^(B))))))
                   ("$(O=$>~IU$-4pK\%i%F%s>.J8;z(B" .
                    (menu "$(O=$>~IU$-4pK\%i%F%s>.J8;z(B"
                          ,(mapcar
                            'char-to-string
                            (sort-char-by-charname
                             `(?$(O(s(B ?$(O(u(B ?$(O(w(B ?$(O(x(B ?$(O(y(B ?$(O(z(B ?$(O({(B ?$(O(|(B
                               ,@(make-chars-list ?$(O)V(B ?$(O)t(B)
                               ,@(make-chars-list ?$(O)z(B ?$(O)~(B)
                               ?$(O*,(B
                               ,@(make-chars-list ?$(O*.(B ?$(O*0(B)
                               ,@(make-chars-list ?$(O*2(B ?$(O*5(B)
                               ,@(make-chars-list ?$(O*7(B ?$(O*8(B)
                               ,@(make-chars-list ?$(O*H(B ?$(O*W(B)
                               ,@(make-chars-list ?$(O*_(B ?$(O*d(B))))))
                   ("$(OHs4pK\%i%F%sJ8;z!&(BIPA$(O5-9f(B" .
                    (menu "$(OHs4pK\%i%F%sJ8;z!&(BIPA$(O5-9f(B"
                          ,(mapcar
                            'char-to-string
                            (sort-char-by-charname
                             `(?$(O)T(B ?$(O)U(B
                               ,@(make-chars-list ?$(O*f(B ?$(O+Q(B)))))))))
           ("$(O%.%j%7%cJ8;z(B" . 
            (menu "$(O%.%j%7%cJ8;z(B"
                  (,@(make-0213char-list ?$(O&!(B ?$(O&8(B)
                   ,@(make-0213char-list ?$(O&A(B ?$(O&Y(B))))
           ("$(O%-%j%kJ8;z(B" . 
            (menu "$(O%-%j%kJ8;z(B"
                  (,@(make-0213char-list ?$(O'!(B ?$(O'A(B)
                   ,@(make-0213char-list ?$(O'Q(B ?$(O'q(B))))
           ("$(OBh;0?e=`(B" .
            (menu "JIS$(OBh;0?e=`(B"
                  (,@(make-0213char-list ?$(O."(B ?$(O/}(B)
                   ,@(make-0213char-list ?$(OOU(B ?$(OO}(B)
                   ,@(make-0213char-list ?$(Ot((B ?$(O~y(B))))
           ("$(OBh;M?e=`(B" .
            (menu "JIS$(OBh;M?e=`(B"
                  (,@(make-char-list 'japanese-jisx0213-2 1 1)
                   ,@(make-char-list 'japanese-jisx0213-2 3 5)
                   ,@(make-char-list 'japanese-jisx0213-2 8 8)
                   ,@(make-char-list 'japanese-jisx0213-2 12 15)
                   ,@(make-char-list 'japanese-jisx0213-2 78 94))))))))

(setcdr (nthcdr 3 (caddr egg-sim-japanese-menu))
        (list egg-sim-jisx0213-menu))

(provide 'egg-sim-jisx0213)
