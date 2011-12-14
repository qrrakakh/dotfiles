JIS X 0213 Emacs$B%-%C%H!";H$$J}%a%b!#(B

 Emacs20$B>e$G!"(BJIS X 0213$B$r;H$&$?$a$N%Q%C%1!<%8$,$3$N%G%#%l%/%H%j$K(B
$BF~$C$F$$$^$9!#$3$N%Q%C%1!<%8$O!"4pK\E*$K@nH((B $BB@0l;a$K$h$C$F:n@.$5$l(B
$B$^$7$?!#(B

$B$3$N%^%K%e%"%k$O!";HMQK!$H!"$3$N%Q%C%1!<%8$K$*$1$k4pK\E*$J(B
$B@_7W$r4JJX$K5-$7$F$"$j$^$9!#(B($B0lIt!"(Bhimi$B$K$h$C$FJdB-$5$l$F$$$^$9!#(B)

o... Install

$B$3$N(Bdirectory$B$r(Bcurrent$B$K$7$F!"(B

emacs -batch -q --no-site-file -l x0213-comp.el

$B$b$7$/$O!"(BMeadow$B$G$"$l$P!"(B

MeadowNT(95).exe -batch -q --no-site-file -l x0213-comp.el

$B$G!"(Bbyte compile$B$G$-$^$9!#$"$H$O!"$3$N(Bdirectory$B$N(B
$BCf$N$9$Y$F$N%U%!%$%k$r(Bload-path$B$NDL$C$?$H$3$m$KCV$$$F$/$@$5$$!#(B
$B$=$N:]!"(BMule-UCS$B$b$-$A$s$H%$%s%9%H!<%k$7$F$*$$$F$/$@$5$$!#(B

o... $B4pK\@_Dj(B

.emacs$B$K(B($B<c$7$/$O!"(Bsite-start.el$B$J$I$NE,@Z$J(Bstart up file$B$K(B)
$B0J2<$N9T$r2C$($k$@$1$G!"DL>o$OLdBj$,$J$$$O$:$G$9!#(B
---
(require 'jisx0213)
---
$B8@8l4D6-(B(language-environment)$B$N@_Dj$r9T$$$?$$>l9g$O!">e5-$N@_Dj$N(B
$B8e$K(B($B=EMW!"$b$7!">e5-$N@_Dj$NA0$K9T$C$?>l9g$O!"(BJIS X 0213 $B4D6-$,(B
$B8=:_$N8@8l4D6-$KF3F~$5$l$^$;$s!#(B)$B0J2<$N@_Dj$r2C$($F$/$@$5$$!#(B
---
(set-language-environment "Japanese")
---

o... Module $B9=@.(B

jisx0213.el   ... setup module
    $B3F(Bmodule$B$r(Brequire$B$9$k$@$1$G$9!#(B

x0213-cdef.el ... charset definition
    define-charset$B$G!"#2$D$N(Bcharset$B$rDj5A$7$F$$$^$9!#(B

x0213-udef.el ... Mule-UCS definition

   Mule-UCS$B$N%Q%C%1!<%8Dj5A%U%!%$%k$G$9!#(B
  un-define.elc$B$K0MB8$7!"(Bun-define$B$rFI$_9~$s$@$"$H!"(BJIS X 0213$B$H(B
  Unicode$B4V$NJQ495,B'$r(Bunicode-basic-translation-rule$B$KF3F~$7$^$9!#(B

x0213-csys.el ... coding system definition

    coding-system$B$H$7$F!"(Biso-2022-jp-3(-strict, -compatible)$B$H!"(B
  euc-jisx0213, shift_jisx0213 $B$rDj5A$7$F$$$^$9!#(Beuc-jisx0213$B!"(B
  shift_jisx0213$B$K$D$$$F$O!"(BJIS X 0213$B$N5,3J=q$r;2>H$7$F$/$@$5$$!#$J$*!"(B
  euc-jisx0213$B$G$O!"(BJIS X 0212$BJ8;z$b(BG3$B$KBP1~$7$^$9$N$G!"(Beuc-japan$B$N>e(B
  $B0L8_49$H$J$C$F$$$^$9!#(B

    iso-2022-jp-3(-strict, -compatible)$B$N%3!<%G%#%s%07O$N0c$$$KIU$$$F(B
  $B$O!"2<$NJ}$r;2>H$7$F$/$@$5$$!#(B

x0213-mime.el ... MIME encoding setup for APEL/FLIM/SEMI

    APEL, FLIM, SEMI$B$N$?$a$N@_Dj%U%!%$%k$G$9!#(B
  JIS X 0213$BJ8;z$r4^$`J8=q$r(BISO-2022-JP-3$B$H$7$F(BMIME$BId9f2=$9$k:]!"(B
  $BJQ?t(B coding-system-for-mime-charset-iso-2022-jp-3 $B$K$h$C$F!"(B
  iso-2022-jp-3-compatible, iso-2022-jp-3-strict, iso-2022-jp-3$B$N$&$A(B
  $B$N$I$l$r;HMQ$9$k$+$rA*Br$9$k$3$H$,$G$-$^$9!#(B
  $BNc$($P!"(B
---
(setq coding-system-for-mime-charset-iso-2022-jp-3 'iso-2022-jp-3)
---
  $B$H!"(B.emacs$B$J$I$N@_Dj%U%!%$%k$K5-=R$9$k$H!"(B`ISO-2022-JP-3' MIME charset
  $B$r!"Id9f2=$b$7$/$OJ#9g2=$9$k$H$-$K(B iso-2022-jp-3 coding system$B$rMQ$$$^$9!#(B

x0213-font.el ... font setup / its encoder definition.

  $B%U%)%s%H$N@_Dj$H%U%)%s%HMQ$N(BShift JISX0213 encoder$B$r@_Dj$9$k%b%8%e!<%k$G$9!#(B

x0213-util.el ... other supplement JIS X 0213 utilities.

  JIS X 0213$B$r07$&>e$G!"M-MQ$J4X?t$J$I$NDj5A$,$J$5$l$F$$$k%b%8%e!<%k$G$9!#(B

x0213-sjis.el ... Shift-JIS encoder and decoder for JIS X 0213
                  This module is required only at byte-compile time.

  Shift JIS encoder$B$*$h$S(Bdecoder$B$N@_Dj$r9T$&%b%8%e!<%k$G$9!#FbItE*$K(B
Mule-UCS$B$K$h$C$FMxMQ$5$l!"(Bx0213-csys.el$B$K$h$C$FI,MW$JItJ,$,MQ$$$i$l$^$9!#(B
$B$J$*!"$3$N%b%8%e!<%k$O%P%$%H%3%s%Q%$%k;~$K$N$_MW5a$5$l!"<B9T;~$K$O(B
$BDL>o!"I,MW$H$5$l$^$;$s!#(B


o... $B%U%)%s%H$N@_Dj(B

  (A) Shift-JIS $B%U%)%s%H$r;H$&!#(B

      charset-registry$B$,!"(B"-shiftjis-0"$B$G$"$k%U%)%s%H$r;HMQ$G$-$^$9!#(B

      $B$=$N$h$&$J%U%)%s%H$NNc$H$7$F$O!"%U%j!<%&%'%"$N(B"W1$B4pK\4A;z%U%)%s(B
      $B%H(B"$B$,$"$j$^$9!#$3$l$O(BTrueType $B%U%)%s%H$G$9$N$G!"Nc$($P0J2<$N$h$&(B
      $B$K(BBDF$B$r:n@.$7$F%7%9%F%`$KAH$_9~$_$^$9!#!J(Bttf2bdf$B$O(B
      FreeType(http://www.freetype.org)$B$NI8=`E:IU%=%U%H$G$9!#!K(B

    ttf2bdf -f waka -t w1 -eid 2 -r 72 -p 24 W1_01.TTF > w1-sjis-24.bdf

      $B$J$*!"(Bw1$B4pK\4A;z%U%)%s%H$O!"0J2<$+$i%@%&%s%m!<%I2DG=$G$9!#(B

         http://www.vector.co.jp/soft/data/writing/se136466.html

  (B) JIS X 0213$B$N(BBDF$B%U%)%s%H$r;H$&!#(B

      charset-registry$B$,!"(B"-jisx0213.2000-1"$B$*$h$S(B"-jisx0213.2000-2"$B$N(B
      $B%U%)%s%H$,;HMQ$G$-$^$9!#(B

      $B:#B<$5$s$,:n$i$l$?%U%j!<$J(B16$B%I%C%H$N(BJIS X 0213 BDF$B%U%)%s%H$O!"(B
          http://www.mars.sphere.ne.jp/imamura/jisx0213.html
          ftp://ftp.m17n.org/pub/character/fonts
      $B$+$iF~<j2DG=!J$K$J$kM=Dj!K$G$9!#(B

  $B$I$A$i$N%U%)%s%H$r;H$&$+$K$h$C$F!"(Bset-fontset-font$B$G@_Dj$9$kCM$,JQ$j(B
  $B$^$9$N$G!"(Bx0213-font.el$B$N@hF,ItJ,$r;29M$K!"3F<+!"I,MW$J$i$P(B.emacs.el
  $B$G!"(Bset-fontset-font$B$r<B9T$7$J$*$7$F2<$5$$!#(B

o... Meadow$B>e$G$N%U%)%s%H@_Dj$K$D$$$F!#(B

  Meadow$B$K$*$$$F$b!"$b$A$m$s!"(BJIS X 0213 font$B$rMQ$$$k$3$H$,=PMh$^$9!#(B

$B!!(B(1) BDF font $B$N@_Dj(B

     (a) ... $B$9$G$K!"(BBDF font$B$r(BMeadow$B>e$GMxMQ$7$F$*$j!"(B
             bdffont.el$B$J$I$N%b%8%e!<%k$r;HMQ$7$F$$$k>l9g!"(B

       bdf-font-file-alist$B$K(B
         (japanese-jisx0213-1 "<jiskan16-2000-1.bdf$B$N(B($BAjBP(B)$B%Q%9(B>" 0)
         (japanese-jisx0213-2 "<jiskan16-2000-1.bdf$B$N(B($BAjBP(B)$B%Q%9(B>" 0)
       $B$N(B2$B$D$N%(%s%H%j$r?7$?$KDI2C$7$F$/$@$5$$!#(B

     (b) ... $B4{B8$N(Bfontset$B$K!"?7$?$K(Bjapanese-jisx0213-1 $B$*$h$S(B
             japanese-jisx0213-2$B$N(Bcharset$B$K(BBDF font$B$r@_Dj$9$k>l9g!#(B

       (w32-auto-regist-bdf-font
        "bdf-font-for-jiskan16-2000-1"
	"<jiskan16-2000-1.bdf$B$N@dBP%Q%9(B>"
	 0)
       (w32-auto-regist-bdf-font
        "bdf-font-for-jiskan16-2000-2"
	"<jiskan16-2000-2.bdf$B$N@dBP%Q%9(B>"
	 0)

       $B$N$h$&$K$7$F!"$^$:!"(BBDF font$B$rEPO?$7$^$9!#(B
       $B$=$N$"$H!";HMQ$7$?$$(Bfontset$B$KBP$7$F!"(B

       (set-fontset-font
        "<$BEPO?$7$?$$(BFONTSET$B$NL>A0(B>"
	'japanese-jisx0213-1
        "bdf-font-for-jiskan16-2000-1")
       (set-fontset-font
        "<$BEPO?$7$?$$(BFONTSET$B$NL>A0(B>"
	'japanese-jisx0213-2
        "bdf-font-for-jiskan16-2000-2")

       $B$N$h$&$K$7$F!"EPO?$r9T$C$F$/$@$5$$!#(B

$B!!(B(2) TrueType font $B$NMxMQ(B

      $B;DG0$J$,$i!"8=>u$N$H$3$m!"(BWindows$B>e$GLdBj$J$/MxMQ$G$-$k(BJIS X
      0213$BJ8;z=89g$r;}$D(BTrueType font$B$NB8:_$O3NG'$G$-$F$*$j$^$;$s!#A0(B
      $B=R$N(B"W1$B4pK\4A;z%U%)%s%H(B"$B$,!"8=>u$GB8:_$9$k$*$=$i$/M#0l$N(BJIS X
      0213$BJ8;z=89g$r$b$D(BTrueType font$B$G$9$,!"I.<T$K$h$C$F3N$+$a$?8B$j(B
      $B$G$O!"(BWindows NT/2000$B>e$G$O!"(BShift_JISX0213 encoding$B$N%U%)%s%H$r(B
      $B07$&$3$H$,=PMh$J$$!"8@$$49$($k$H!"(BJIS X 0213 repertorie$B$rMxMQ$9(B
      $B$k$3$H$,$G$-$^$;$s!#?dB,$K2a$.$^$;$s$,!"$*$=$i$/!"FbIt=hM};~$K(B
      Unicode$B$KD>$9$?$a$@$H;W$o$l$^$9!#$3$N$?$a!"(BCP932$B$GDj$a$i$l$F$$$J(B
      $B$$(Bcode range$B$r;}$DJ8;z$NI=<($,=PMh$J$$$h$&$G$9!#(BWindows 95/98$B$G$O!"(B
      $BMxMQ$G$-$k$h$&$G$9$,(B(GDI$B$G$NFbIt=hM}$,$=$b$=$b(BCP932$B$@$+$i$G$"$m$&!#(B)$B!"(B
      $B>-MhE*$K$O$=$N8B$j$G$O$"$j$^$;$s!#(B
      ("W1$B4pK\4A;z%U%)%s%H(B"$B$,!"(BShift_JISX0213 encoding$B$G$O$J$/!"FH<+$N(B
      encoding $B$K$7$F$"$l$P!"(BMeadow$BB&$G$I$&$K$+$J$C$?$N$+$b$7$l$^$;$s$,!#(B)

      $B$=$N$h$&$JLdBj$,$"$C$F$b!"(BShift_JISX0213 encoding$B$N(BTrueType$B%U%)(B
      $B%s%H$rMxMQ$7$?$$$H$$$&J}$N0Y$K!"$3$N%;%/%7%g%s$G$O!"4JC1$K@_Dj$r(B
      $B>R2p$7$^$9!#$J$*!"4{$K=R$Y$?$H$*$j!"$=$N$h$&$J%U%)%s%H$K$O!"8=>u(B
      $B$G$O!"(B"W1$B4pK\4A;z%U%)%s%H(B" $B$7$+B8:_$7$J$$$H;W$o$l$^$9$,!"$3$N%U%)(B
      $B%s%H$O!"(Bpropotional$B%U%)%s%H$G$"$j!"$7$+$b!"%5%$%:$r>.$5$/$7$?;~(B
      $B$NIJ<A$,Cx$7$/Nt$k$N$G!"8=>u$G$O!";HMQ$r$*4+$a$7$+$M$^$9!#$J$*!"(B
      $B$=$NB>$N(BTrueType font$B$G$b!"(BJIS X 0213 $B%l%Q!<%H%j!<$rJq4^$9$k>l9g(B
      $B$O!";HMQ$G$-$k$+$b$7$^$;$s$,!"$3$N%;%/%7%g%s$G$NFbMF$OM-8z$G$O$J(B
      $B$$2DG=@-$,$"$j$^$9!#$4N;>5$/$@$5$$!#8=>u$G$O!"(Bpropotional $B%U%)%s(B
      $B%H$rL5M}$K(BMeadow$B$G;HMQ$7$?$H$-$K$O!"LdBj$,B?$$$N$G!"2~A1$9$k2DG=(B
      $B@-$O$"$j$^$9$,!"Aa4|$K(BEmacs 21 base$B$K0\9T$9$kJ}$,:,K\E*$JBP=h$K(B
      $B$J$k$H;W$o$l$^$9!#(B($B$N$G(BEmacs20 base$B$N(BMeadow$B$G$OBP:v$O9T$o$J$$$+(B
      $B$b$7$l$^$;$s!#(B)
      
     (a) ... $B<jF0@_Dj$NNc(B

     low level font selection API$B$rMQ$$$F!"<jF0@_Dj$r$7$F$/$@$5$$!#(B
     $B$=$N:]!"(Bencoder$B$K!"(Bshift-jisx0213-font-encoder$B$r;XDj$7$F$/$@$5$$!#(B

     $BNc(B: 
     $BCm(B:(logfont$B$d!"(B<YOUR FONTSET NAME>$B$NItJ,$O!"$*9%$_$N7A$K=$@5$7$F$/$@$5$$!#(B)
---
(let* ((fontsetname "<YOUR FONTSET NAME>")
       (fontname "W1-ShiftJISX0213-font")
       (logfont '(w32-logfont "W1-01xxxx" 8 16 400 0 nil nil nil 128 1 3 2))
       (preference
	(append
	 '((encoding-type . 4)
	   (encoder . shift-jisx0213-font-encoder))
	 (w32-get-logfont-info logfont))))
  (w32-add-font
   fontname
   preference)
  (w32-change-font-logfont
   fontname 0 logfont)
  (set-fontset-font
   fontsetname 'japanese-jisx0213-1 fontname)
  (set-fontset-font
   fontsetname 'japanese-jisx0213-2 fontname))
---

     (b) ... $B$=$NB>(B

     $B>-Mh$N(BMeadow$B$G$O!"(BJIS X 0213 font$B$N<+F0@_Dj5!G=$rAH$_9~$`2DG=@-$,(B
     $B$"$j$^$9!#$b$7!"$=$N$h$&$J<+F0@_Dj5!G=$,M-8z$G$"$k>l9g$K$O!">e5-$N(B
     $B@_Dj$O(Bhigh-level font setlection API$B$GCV$-49$($k$3$H$,$G$-$k$h$&$K(B
     $B$J$k$G$7$g$&!#(B

     $B$3$l$i$N;EMM$O>-MhM=9p$J$/JQ99$5$l$k$3$H$,$"$j$^$9$N$G!"$4Cm0U$/$@$5$$!#(B


o... $B%3!<%I7O!"(B`iso-2022-jp-3-compatible'$B$NCm0U;v9`$H!"(B
     `iso-2022-jp-strict', `iso-2022-jp-3'$B$H$NAj0cE@(B

    JIS X 0213:2000$B$NImB0=q#2!V(BISO-2022-JP-3$BId9f2=I=8=!W$N(B4.1 (e)$B$G$O!"(B
  ISO-2022-JP-3$B$NId9f2=$K:]$7!"(B1B 24 42$B$N@Z$jBX$(Id9f0J9_$G;HMQ$7$F$O(B
  $B$$$1$J$$(BJIS X 0213-1$BJ8;z=89g$rImB0=q#2I=#1$GDj$a$F$$$^$9!#(B

    $B$3$NI=$K$O!"(BJIS X 0208$B$GDj5A$5$l$F$$$J$$J8;z$O$b$A$m$s$N$3$H!"(BJIS
  X 0208$B$H(BJIS X 0213$B$NJq@]4p=`$,JQ99$5$l$?4A;zEy$b4^$^$l$F$$$^$9!#Nc$((B
  $B$P!"!V3z!W!"!V2+!W!"!V2*!W$J$I4A;z$O!"(BISO-2022-JP-3$B$G$O(B 1B 24 42 $B8F(B
  $B$S=P$7$G$N;HMQ$,6X;_$5$l$F$$$^$9!#(B

    $B$7$+$7(BJIS X 0208$B4A;z$NCf$+$i!"$3$l$i(B1B 24 42$B$G$N;HMQ6X;_4A;z$r$$$A(B
  $B$$$A!"(B1B 24 28 4F$B$K@Z$jBX$(D>$7$F$7$^$&$H!"(BISO-2022-JP$B$GJ]B8$5$l$?%U%!(B
  $B%$%k$NFbMF$rFI$_9~$_!":F$SJ]B8$9$k:]$KFbMF$,GK2u$5$l$k2DG=@-$,$"$j$^(B
  $B$9!#(B

    $B$3$l$rKI$0$?$a!"%3!<%I7O(B`iso-2022-jp-3-compatible'$B$G$O!"$3$l$i!V6X(B
  $B;_4A;z!W$b!"(B1B 24 42$B$GId9f2=$7$^$9!#$=$N$?$a!"(B
  iso-2022-jp-3-compatible$B$O!"(Biso-2022-jp$B$N40A4>e0L8_49$H$J$j$^$9!#(B
  $B!J(BJIS X 0208$B$K$*$$$FL$Dj5A$NJ8;z$,(B1B 24 42$B$G8F$P$l$k$3$H$O$"$j$^$;(B
  $B$s!#!K(B

    $B0lJ}!"(Biso-202-jp-3-strict$B$O!"$"$/$^$G(BISO-2022-JP-3$B$K=>$$$D$D$b!"2D(B
  $BG=$J8B$j$N(BJIS X 0208$BJ8;z$r!"(B1B 24 42$B$GId9f2=$7$^$9!#$=$N$?$a!"(B
  iso-2022-jp-3-strict$B$G$GId9f2=$7$?J8=q$O!"(BISO-2022-JP$B$K$7$+BP1~$7$F(B
  $B$$$J$$%(%G%#%?$G$b!V$"$kDxEY$O!WFI$a$k$h$&$K$J$C$F$$$^$9!#(B

    $B$3$l$i$N%3!<%I7O$K$*$1$k!"(B1B 24 42$B$H!"(B1B 24 28 4F$B$N:.:_$,5$$K$J$k(B
  $B>l9g$O!"(BJIS X 0213 $BJ8=q$NJ]B8$r!"%3!<%I7O(B`iso-2022-jp-3' $B$G9T$J$C$F(B
  $B$/$@$5$$!#$3$N%3!<%I7O$G$O!"A4$F$N(BJIS X 0213-1$B$NJ8;z$r(B1B 24 28 4F$B$G(B
  $B8F$S=P$7$^$9!#$?$@$7!"$3$N%3!<%I7O$GJ]B8$7$?J8=q$,!"(BJIS X 0213$B$KL$BP(B
  $B1~$J%(%G%#%?Ey$GFI$a$k2DG=@-$ODc$/$J$j$^$9!#(B

    $B0J2<$K!"!V?92*30!W$r3F%3!<%I7O$G%(%s%3!<%I$7$?>l9g$N=PNONc$r<($7$^(B
  $B$9!#(B

  iso-2022-jp-3-compatible$B!'(B
    1B 24 42 3F 39 32 2A 33 30 1B 28 42
    ESC $ B  ?  9  2  *  3  0  ESC ( B

  iso-2022-jp-3-strict$B!'(B
    1B 24 42 3F 39 1B 24 28 4F 32 2A 1B 24 42 33 30 1B 28 42
    ESC $ B  ?  9  ESC $ (  O  2  * ESC $  B  3  0  ESC ( B

  iso-2022-jp-3$B!'(B
    1B 24 28 4F 3F 39 32 2A 33 30 1B 28 42
    ESC $ (  O  ?  9  2  *  3  0  ESC ( B

o ... JIS X 0213$B$GMxMQ$G$-$k<-=q(B

  http://www.m17n.org/kawabata/x0213dic/ $B$K!"8=:_%U%j!<$G=P2s$C$F$$$k(B
  $B<-=q$r(BJIS X 0213$BMQ$KJQ49$7$?$b$N$rMQ0U$7$?$N$G!"$4MxMQ2<$5$$!#(B

o... $B:#8e$NM=Dj(B

  $B<-=q$N@0Hw(B
  $BF~NO%D!<%k$N@0Hw(B
  $BJQ49MQ%G!<%?$N@0Hw(B
