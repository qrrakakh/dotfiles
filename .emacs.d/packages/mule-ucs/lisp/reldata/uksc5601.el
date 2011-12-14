; -*- coding: iso-2022-7bit  -*-
;;; uksc5601.el --- tables between UCS and KS C 5601-1987

;; Author: Lori Hoerth <lorih@microsoft.com>
;;         K.D.Chang   <a-kchang@microsoft.com>

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, KS C 5601

;; This file is part of Mule-UCS.

;; Mule-UCS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Mule-UCS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is converted from

;;	ftp://ftp.unicode.org/Public/MAPPINGS/EASTASIA/KSC/KSC5601.TXT

;; by MORIOKA Tomohiko <morioka@jaist.ac.jp>.

;;; Code:

(put 'korean-ksc5601 'unicode-assoc
     'ks-c-5601-1987-vs-unicode-assoc)

(defvar
  ks-c-5601-1987-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?$(C!!(B . "0x3000") ; IDEOGRAPHIC SPACE
       (?$(C!"(B . "0x3001") ; IDEOGRAPHIC COMMA
       (?$(C!#(B . "0x3002") ; IDEOGRAPHIC FULL STOP
       (?$(C!$(B . "0x00B7") ; MIDDLE DOT
       (?$(C!%(B . "0x2025") ; TWO DOT LEADER
       (?$(C!&(B . "0x2026") ; HORIZONTAL ELLIPSIS
       (?$(C!'(B . "0x00A8") ; DIAERESIS
       (?$(C!((B . "0x3003") ; DITTO MARK
       (?$(C!)(B . "0x00AD") ; SOFT HYPHEN
       (?$(C!*(B . "0x2015") ; HORIZONTAL BAR
       (?$(C!+(B . "0x2225") ; PARALLEL TO
       (?$(C!,(B . "0xFF3C") ; FULLWIDTH REVERSE SOLIDUS
       (?$(C!-(B . "0x223C") ; TILDE OPERATOR
       (?$(C!.(B . "0x2018") ; LEFT SINGLE QUOTATION MARK
       (?$(C!/(B . "0x2019") ; RIGHT SINGLE QUOTATION MARK
       (?$(C!0(B . "0x201C") ; LEFT DOUBLE QUOTATION MARK
       (?$(C!1(B . "0x201D") ; RIGHT DOUBLE QUOTATION MARK
       (?$(C!2(B . "0x3014") ; LEFT TORTOISE SHELL BRACKET
       (?$(C!3(B . "0x3015") ; RIGHT TORTOISE SHELL BRACKET
       (?$(C!4(B . "0x3008") ; LEFT ANGLE BRACKET
       (?$(C!5(B . "0x3009") ; RIGHT ANGLE BRACKET
       (?$(C!6(B . "0x300A") ; LEFT DOUBLE ANGLE BRACKET
       (?$(C!7(B . "0x300B") ; RIGHT DOUBLE ANGLE BRACKET
       (?$(C!8(B . "0x300C") ; LEFT CORNER BRACKET
       (?$(C!9(B . "0x300D") ; RIGHT CORNER BRACKET
       (?$(C!:(B . "0x300E") ; LEFT WHITE CORNER BRACKET
       (?$(C!;(B . "0x300F") ; RIGHT WHITE CORNER BRACKET
       (?$(C!<(B . "0x3010") ; LEFT BLACK LENTICULAR BRACKET
       (?$(C!=(B . "0x3011") ; RIGHT BLACK LENTICULAR BRACKET
       (?$(C!>(B . "0x00B1") ; PLUS-MINUS SIGN
       (?$(C!?(B . "0x00D7") ; MULTIPLICATION SIGN
       (?$(C!@(B . "0x00F7") ; DIVISION SIGN
       (?$(C!A(B . "0x2260") ; NOT EQUAL TO
       (?$(C!B(B . "0x2264") ; LESS-THAN OR EQUAL TO
       (?$(C!C(B . "0x2265") ; GREATER-THAN OR EQUAL TO
       (?$(C!D(B . "0x221E") ; INFINITY
       (?$(C!E(B . "0x2234") ; THEREFORE
       (?$(C!F(B . "0x00B0") ; DEGREE SIGN
       (?$(C!G(B . "0x2032") ; PRIME
       (?$(C!H(B . "0x2033") ; DOUBLE PRIME
       (?$(C!I(B . "0x2103") ; DEGREE CELSIUS
       (?$(C!J(B . "0x212B") ; ANGSTROM SIGN
       (?$(C!K(B . "0xFFE0") ; FULLWIDTH CENT SIGN
       (?$(C!L(B . "0xFFE1") ; FULLWIDTH POUND SIGN
       (?$(C!M(B . "0xFFE5") ; FULLWIDTH YEN SIGN
       (?$(C!N(B . "0x2642") ; MALE SIGN
       (?$(C!O(B . "0x2640") ; FEMALE SIGN
       (?$(C!P(B . "0x2220") ; ANGLE
       (?$(C!Q(B . "0x22A5") ; UP TACK
       (?$(C!R(B . "0x2312") ; ARC
       (?$(C!S(B . "0x2202") ; PARTIAL DIFFERENTIAL
       (?$(C!T(B . "0x2207") ; NABLA
       (?$(C!U(B . "0x2261") ; IDENTICAL TO
       (?$(C!V(B . "0x2252") ; APPROXIMATELY EQUAL TO OR THE IMAGE OF
       (?$(C!W(B . "0x00A7") ; SECTION SIGN
       (?$(C!X(B . "0x203B") ; REFERENCE MARK
       (?$(C!Y(B . "0x2606") ; WHITE STAR
       (?$(C!Z(B . "0x2605") ; BLACK STAR
       (?$(C![(B . "0x25CB") ; WHITE CIRCLE
       (?$(C!\(B . "0x25CF") ; BLACK CIRCLE
       (?$(C!](B . "0x25CE") ; BULLSEYE
       (?$(C!^(B . "0x25C7") ; WHITE DIAMOND
       (?$(C!_(B . "0x25C6") ; BLACK DIAMOND
       (?$(C!`(B . "0x25A1") ; WHITE SQUARE
       (?$(C!a(B . "0x25A0") ; BLACK SQUARE
       (?$(C!b(B . "0x25B3") ; WHITE UP-POINTING TRIANGLE
       (?$(C!c(B . "0x25B2") ; BLACK UP-POINTING TRIANGLE
       (?$(C!d(B . "0x25BD") ; WHITE DOWN-POINTING TRIANGLE
       (?$(C!e(B . "0x25BC") ; BLACK DOWN-POINTING TRIANGLE
       (?$(C!f(B . "0x2192") ; RIGHTWARDS ARROW
       (?$(C!g(B . "0x2190") ; LEFTWARDS ARROW
       (?$(C!h(B . "0x2191") ; UPWARDS ARROW
       (?$(C!i(B . "0x2193") ; DOWNWARDS ARROW
       (?$(C!j(B . "0x2194") ; LEFT RIGHT ARROW
       (?$(C!k(B . "0x3013") ; GETA MARK
       (?$(C!l(B . "0x226A") ; MUCH LESS-THAN
       (?$(C!m(B . "0x226B") ; MUCH GREATER-THAN
       (?$(C!n(B . "0x221A") ; SQUARE ROOT
       (?$(C!o(B . "0x223D") ; REVERSED TILDE
       (?$(C!p(B . "0x221D") ; PROPORTIONAL TO
       (?$(C!q(B . "0x2235") ; BECAUSE
       (?$(C!r(B . "0x222B") ; INTEGRAL
       (?$(C!s(B . "0x222C") ; DOUBLE INTEGRAL
       (?$(C!t(B . "0x2208") ; ELEMENT OF
       (?$(C!u(B . "0x220B") ; CONTAINS AS MEMBER
       (?$(C!v(B . "0x2286") ; SUBSET OF OR EQUAL TO
       (?$(C!w(B . "0x2287") ; SUPERSET OF OR EQUAL TO
       (?$(C!x(B . "0x2282") ; SUBSET OF
       (?$(C!y(B . "0x2283") ; SUPERSET OF
       (?$(C!z(B . "0x222A") ; UNION
       (?$(C!{(B . "0x2229") ; INTERSECTION
       (?$(C!|(B . "0x2227") ; LOGICAL AND
       (?$(C!}(B . "0x2228") ; LOGICAL OR
       (?$(C!~(B . "0xFFE2") ; FULLWIDTH NOT SIGN
       (?$(C"!(B . "0x21D2") ; RIGHTWARDS DOUBLE ARROW
       (?$(C""(B . "0x21D4") ; LEFT RIGHT DOUBLE ARROW
       (?$(C"#(B . "0x2200") ; FOR ALL
       (?$(C"$(B . "0x2203") ; THERE EXISTS
       (?$(C"%(B . "0x00B4") ; ACUTE ACCENT
       (?$(C"&(B . "0xFF5E") ; FULLWIDTH TILDE
       (?$(C"'(B . "0x02C7") ; CARON
       (?$(C"((B . "0x02D8") ; BREVE
       (?$(C")(B . "0x02DD") ; DOUBLE ACUTE ACCENT
       (?$(C"*(B . "0x02DA") ; RING ABOVE
       (?$(C"+(B . "0x02D9") ; DOT ABOVE
       (?$(C",(B . "0x00B8") ; CEDILLA
       (?$(C"-(B . "0x02DB") ; OGONEK
       (?$(C".(B . "0x00A1") ; INVERTED EXCLAMATION MARK
       (?$(C"/(B . "0x00BF") ; INVERTED QUESTION MARK
       (?$(C"0(B . "0x02D0") ; MODIFIER LETTER TRIANGULAR COLON
       (?$(C"1(B . "0x222E") ; CONTOUR INTEGRAL
       (?$(C"2(B . "0x2211") ; N-ARY SUMMATION
       (?$(C"3(B . "0x220F") ; N-ARY PRODUCT
       (?$(C"4(B . "0x00A4") ; CURRENCY SIGN
       (?$(C"5(B . "0x2109") ; DEGREE FAHRENHEIT
       (?$(C"6(B . "0x2030") ; PER MILLE SIGN
       (?$(C"7(B . "0x25C1") ; WHITE LEFT-POINTING TRIANGLE
       (?$(C"8(B . "0x25C0") ; BLACK LEFT-POINTING TRIANGLE
       (?$(C"9(B . "0x25B7") ; WHITE RIGHT-POINTING TRIANGLE
       (?$(C":(B . "0x25B6") ; BLACK RIGHT-POINTING TRIANGLE
       (?$(C";(B . "0x2664") ; WHITE SPADE SUIT
       (?$(C"<(B . "0x2660") ; BLACK SPADE SUIT
       (?$(C"=(B . "0x2661") ; WHITE HEART SUIT
       (?$(C">(B . "0x2665") ; BLACK HEART SUIT
       (?$(C"?(B . "0x2667") ; WHITE CLUB SUIT
       (?$(C"@(B . "0x2663") ; BLACK CLUB SUIT
       (?$(C"A(B . "0x2299") ; CIRCLED DOT OPERATOR
       (?$(C"B(B . "0x25C8") ; WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
       (?$(C"C(B . "0x25A3") ; WHITE SQUARE CONTAINING BLACK SMALL SQUARE
       (?$(C"D(B . "0x25D0") ; CIRCLE WITH LEFT HALF BLACK
       (?$(C"E(B . "0x25D1") ; CIRCLE WITH RIGHT HALF BLACK
       (?$(C"F(B . "0x2592") ; MEDIUM SHADE
       (?$(C"G(B . "0x25A4") ; SQUARE WITH HORIZONTAL FILL
       (?$(C"H(B . "0x25A5") ; SQUARE WITH VERTICAL FILL
       (?$(C"I(B . "0x25A8") ; SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
       (?$(C"J(B . "0x25A7") ; SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
       (?$(C"K(B . "0x25A6") ; SQUARE WITH ORTHOGONAL CROSSHATCH FILL
       (?$(C"L(B . "0x25A9") ; SQUARE WITH DIAGONAL CROSSHATCH FILL
       (?$(C"M(B . "0x2668") ; HOT SPRINGS
       (?$(C"N(B . "0x260F") ; WHITE TELEPHONE
       (?$(C"O(B . "0x260E") ; BLACK TELEPHONE
       (?$(C"P(B . "0x261C") ; WHITE LEFT POINTING INDEX
       (?$(C"Q(B . "0x261E") ; WHITE RIGHT POINTING INDEX
       (?$(C"R(B . "0x00B6") ; PILCROW SIGN
       (?$(C"S(B . "0x2020") ; DAGGER
       (?$(C"T(B . "0x2021") ; DOUBLE DAGGER
       (?$(C"U(B . "0x2195") ; UP DOWN ARROW
       (?$(C"V(B . "0x2197") ; NORTH EAST ARROW
       (?$(C"W(B . "0x2199") ; SOUTH WEST ARROW
       (?$(C"X(B . "0x2196") ; NORTH WEST ARROW
       (?$(C"Y(B . "0x2198") ; SOUTH EAST ARROW
       (?$(C"Z(B . "0x266D") ; MUSIC FLAT SIGN
       (?$(C"[(B . "0x2669") ; QUARTER NOTE
       (?$(C"\(B . "0x266A") ; EIGHTH NOTE
       (?$(C"](B . "0x266C") ; BEAMED SIXTEENTH NOTES
       (?$(C"^(B . "0x327F") ; KOREAN STANDARD SYMBOL
       (?$(C"_(B . "0x321C") ; PARENTHESIZED HANGUL CIEUC U
       (?$(C"`(B . "0x2116") ; NUMERO SIGN
       (?$(C"a(B . "0x33C7") ; SQUARE CO
       (?$(C"b(B . "0x2122") ; TRADE MARK SIGN
       (?$(C"c(B . "0x33C2") ; SQUARE AM
       (?$(C"d(B . "0x33D8") ; SQUARE PM
       (?$(C"e(B . "0x2121") ; TELEPHONE SIGN
       (?$(C#!(B . "0xFF01") ; FULLWIDTH EXCLAMATION MARK
       (?$(C#"(B . "0xFF02") ; FULLWIDTH QUOTATION MARK
       (?$(C##(B . "0xFF03") ; FULLWIDTH NUMBER SIGN
       (?$(C#$(B . "0xFF04") ; FULLWIDTH DOLLAR SIGN
       (?$(C#%(B . "0xFF05") ; FULLWIDTH PERCENT SIGN
       (?$(C#&(B . "0xFF06") ; FULLWIDTH AMPERSAND
       (?$(C#'(B . "0xFF07") ; FULLWIDTH APOSTROPHE
       (?$(C#((B . "0xFF08") ; FULLWIDTH LEFT PARENTHESIS
       (?$(C#)(B . "0xFF09") ; FULLWIDTH RIGHT PARENTHESIS
       (?$(C#*(B . "0xFF0A") ; FULLWIDTH ASTERISK
       (?$(C#+(B . "0xFF0B") ; FULLWIDTH PLUS SIGN
       (?$(C#,(B . "0xFF0C") ; FULLWIDTH COMMA
       (?$(C#-(B . "0xFF0D") ; FULLWIDTH HYPHEN-MINUS
       (?$(C#.(B . "0xFF0E") ; FULLWIDTH FULL STOP
       (?$(C#/(B . "0xFF0F") ; FULLWIDTH SOLIDUS
       (?$(C#0(B . "0xFF10") ; FULLWIDTH DIGIT ZERO
       (?$(C#1(B . "0xFF11") ; FULLWIDTH DIGIT ONE
       (?$(C#2(B . "0xFF12") ; FULLWIDTH DIGIT TWO
       (?$(C#3(B . "0xFF13") ; FULLWIDTH DIGIT THREE
       (?$(C#4(B . "0xFF14") ; FULLWIDTH DIGIT FOUR
       (?$(C#5(B . "0xFF15") ; FULLWIDTH DIGIT FIVE
       (?$(C#6(B . "0xFF16") ; FULLWIDTH DIGIT SIX
       (?$(C#7(B . "0xFF17") ; FULLWIDTH DIGIT SEVEN
       (?$(C#8(B . "0xFF18") ; FULLWIDTH DIGIT EIGHT
       (?$(C#9(B . "0xFF19") ; FULLWIDTH DIGIT NINE
       (?$(C#:(B . "0xFF1A") ; FULLWIDTH COLON
       (?$(C#;(B . "0xFF1B") ; FULLWIDTH SEMICOLON
       (?$(C#<(B . "0xFF1C") ; FULLWIDTH LESS-THAN SIGN
       (?$(C#=(B . "0xFF1D") ; FULLWIDTH EQUALS SIGN
       (?$(C#>(B . "0xFF1E") ; FULLWIDTH GREATER-THAN SIGN
       (?$(C#?(B . "0xFF1F") ; FULLWIDTH QUESTION MARK
       (?$(C#@(B . "0xFF20") ; FULLWIDTH COMMERCIAL AT
       (?$(C#A(B . "0xFF21") ; FULLWIDTH LATIN CAPITAL LETTER A
       (?$(C#B(B . "0xFF22") ; FULLWIDTH LATIN CAPITAL LETTER B
       (?$(C#C(B . "0xFF23") ; FULLWIDTH LATIN CAPITAL LETTER C
       (?$(C#D(B . "0xFF24") ; FULLWIDTH LATIN CAPITAL LETTER D
       (?$(C#E(B . "0xFF25") ; FULLWIDTH LATIN CAPITAL LETTER E
       (?$(C#F(B . "0xFF26") ; FULLWIDTH LATIN CAPITAL LETTER F
       (?$(C#G(B . "0xFF27") ; FULLWIDTH LATIN CAPITAL LETTER G
       (?$(C#H(B . "0xFF28") ; FULLWIDTH LATIN CAPITAL LETTER H
       (?$(C#I(B . "0xFF29") ; FULLWIDTH LATIN CAPITAL LETTER I
       (?$(C#J(B . "0xFF2A") ; FULLWIDTH LATIN CAPITAL LETTER J
       (?$(C#K(B . "0xFF2B") ; FULLWIDTH LATIN CAPITAL LETTER K
       (?$(C#L(B . "0xFF2C") ; FULLWIDTH LATIN CAPITAL LETTER L
       (?$(C#M(B . "0xFF2D") ; FULLWIDTH LATIN CAPITAL LETTER M
       (?$(C#N(B . "0xFF2E") ; FULLWIDTH LATIN CAPITAL LETTER N
       (?$(C#O(B . "0xFF2F") ; FULLWIDTH LATIN CAPITAL LETTER O
       (?$(C#P(B . "0xFF30") ; FULLWIDTH LATIN CAPITAL LETTER P
       (?$(C#Q(B . "0xFF31") ; FULLWIDTH LATIN CAPITAL LETTER Q
       (?$(C#R(B . "0xFF32") ; FULLWIDTH LATIN CAPITAL LETTER R
       (?$(C#S(B . "0xFF33") ; FULLWIDTH LATIN CAPITAL LETTER S
       (?$(C#T(B . "0xFF34") ; FULLWIDTH LATIN CAPITAL LETTER T
       (?$(C#U(B . "0xFF35") ; FULLWIDTH LATIN CAPITAL LETTER U
       (?$(C#V(B . "0xFF36") ; FULLWIDTH LATIN CAPITAL LETTER V
       (?$(C#W(B . "0xFF37") ; FULLWIDTH LATIN CAPITAL LETTER W
       (?$(C#X(B . "0xFF38") ; FULLWIDTH LATIN CAPITAL LETTER X
       (?$(C#Y(B . "0xFF39") ; FULLWIDTH LATIN CAPITAL LETTER Y
       (?$(C#Z(B . "0xFF3A") ; FULLWIDTH LATIN CAPITAL LETTER Z
       (?$(C#[(B . "0xFF3B") ; FULLWIDTH LEFT SQUARE BRACKET
       (?$(C#\(B . "0xFFE6") ; FULLWIDTH WON SIGN
       (?$(C#](B . "0xFF3D") ; FULLWIDTH RIGHT SQUARE BRACKET
       (?$(C#^(B . "0xFF3E") ; FULLWIDTH CIRCUMFLEX ACCENT
       (?$(C#_(B . "0xFF3F") ; FULLWIDTH LOW LINE
       (?$(C#`(B . "0xFF40") ; FULLWIDTH GRAVE ACCENT
       (?$(C#a(B . "0xFF41") ; FULLWIDTH LATIN SMALL LETTER A
       (?$(C#b(B . "0xFF42") ; FULLWIDTH LATIN SMALL LETTER B
       (?$(C#c(B . "0xFF43") ; FULLWIDTH LATIN SMALL LETTER C
       (?$(C#d(B . "0xFF44") ; FULLWIDTH LATIN SMALL LETTER D
       (?$(C#e(B . "0xFF45") ; FULLWIDTH LATIN SMALL LETTER E
       (?$(C#f(B . "0xFF46") ; FULLWIDTH LATIN SMALL LETTER F
       (?$(C#g(B . "0xFF47") ; FULLWIDTH LATIN SMALL LETTER G
       (?$(C#h(B . "0xFF48") ; FULLWIDTH LATIN SMALL LETTER H
       (?$(C#i(B . "0xFF49") ; FULLWIDTH LATIN SMALL LETTER I
       (?$(C#j(B . "0xFF4A") ; FULLWIDTH LATIN SMALL LETTER J
       (?$(C#k(B . "0xFF4B") ; FULLWIDTH LATIN SMALL LETTER K
       (?$(C#l(B . "0xFF4C") ; FULLWIDTH LATIN SMALL LETTER L
       (?$(C#m(B . "0xFF4D") ; FULLWIDTH LATIN SMALL LETTER M
       (?$(C#n(B . "0xFF4E") ; FULLWIDTH LATIN SMALL LETTER N
       (?$(C#o(B . "0xFF4F") ; FULLWIDTH LATIN SMALL LETTER O
       (?$(C#p(B . "0xFF50") ; FULLWIDTH LATIN SMALL LETTER P
       (?$(C#q(B . "0xFF51") ; FULLWIDTH LATIN SMALL LETTER Q
       (?$(C#r(B . "0xFF52") ; FULLWIDTH LATIN SMALL LETTER R
       (?$(C#s(B . "0xFF53") ; FULLWIDTH LATIN SMALL LETTER S
       (?$(C#t(B . "0xFF54") ; FULLWIDTH LATIN SMALL LETTER T
       (?$(C#u(B . "0xFF55") ; FULLWIDTH LATIN SMALL LETTER U
       (?$(C#v(B . "0xFF56") ; FULLWIDTH LATIN SMALL LETTER V
       (?$(C#w(B . "0xFF57") ; FULLWIDTH LATIN SMALL LETTER W
       (?$(C#x(B . "0xFF58") ; FULLWIDTH LATIN SMALL LETTER X
       (?$(C#y(B . "0xFF59") ; FULLWIDTH LATIN SMALL LETTER Y
       (?$(C#z(B . "0xFF5A") ; FULLWIDTH LATIN SMALL LETTER Z
       (?$(C#{(B . "0xFF5B") ; FULLWIDTH LEFT CURLY BRACKET
       (?$(C#|(B . "0xFF5C") ; FULLWIDTH VERTICAL LINE
       (?$(C#}(B . "0xFF5D") ; FULLWIDTH RIGHT CURLY BRACKET
       (?$(C#~(B . "0xFFE3") ; FULLWIDTH MACRON
       (?$(C$!(B . "0x3131") ; HANGUL LETTER KIYEOK
       (?$(C$"(B . "0x3132") ; HANGUL LETTER SSANGKIYEOK
       (?$(C$#(B . "0x3133") ; HANGUL LETTER KIYEOK-SIOS
       (?$(C$$(B . "0x3134") ; HANGUL LETTER NIEUN
       (?$(C$%(B . "0x3135") ; HANGUL LETTER NIEUN-CIEUC
       (?$(C$&(B . "0x3136") ; HANGUL LETTER NIEUN-HIEUH
       (?$(C$'(B . "0x3137") ; HANGUL LETTER TIKEUT
       (?$(C$((B . "0x3138") ; HANGUL LETTER SSANGTIKEUT
       (?$(C$)(B . "0x3139") ; HANGUL LETTER RIEUL
       (?$(C$*(B . "0x313A") ; HANGUL LETTER RIEUL-KIYEOK
       (?$(C$+(B . "0x313B") ; HANGUL LETTER RIEUL-MIEUM
       (?$(C$,(B . "0x313C") ; HANGUL LETTER RIEUL-PIEUP
       (?$(C$-(B . "0x313D") ; HANGUL LETTER RIEUL-SIOS
       (?$(C$.(B . "0x313E") ; HANGUL LETTER RIEUL-THIEUTH
       (?$(C$/(B . "0x313F") ; HANGUL LETTER RIEUL-PHIEUPH
       (?$(C$0(B . "0x3140") ; HANGUL LETTER RIEUL-HIEUH
       (?$(C$1(B . "0x3141") ; HANGUL LETTER MIEUM
       (?$(C$2(B . "0x3142") ; HANGUL LETTER PIEUP
       (?$(C$3(B . "0x3143") ; HANGUL LETTER SSANGPIEUP
       (?$(C$4(B . "0x3144") ; HANGUL LETTER PIEUP-SIOS
       (?$(C$5(B . "0x3145") ; HANGUL LETTER SIOS
       (?$(C$6(B . "0x3146") ; HANGUL LETTER SSANGSIOS
       (?$(C$7(B . "0x3147") ; HANGUL LETTER IEUNG
       (?$(C$8(B . "0x3148") ; HANGUL LETTER CIEUC
       (?$(C$9(B . "0x3149") ; HANGUL LETTER SSANGCIEUC
       (?$(C$:(B . "0x314A") ; HANGUL LETTER CHIEUCH
       (?$(C$;(B . "0x314B") ; HANGUL LETTER KHIEUKH
       (?$(C$<(B . "0x314C") ; HANGUL LETTER THIEUTH
       (?$(C$=(B . "0x314D") ; HANGUL LETTER PHIEUPH
       (?$(C$>(B . "0x314E") ; HANGUL LETTER HIEUH
       (?$(C$?(B . "0x314F") ; HANGUL LETTER A
       (?$(C$@(B . "0x3150") ; HANGUL LETTER AE
       (?$(C$A(B . "0x3151") ; HANGUL LETTER YA
       (?$(C$B(B . "0x3152") ; HANGUL LETTER YAE
       (?$(C$C(B . "0x3153") ; HANGUL LETTER EO
       (?$(C$D(B . "0x3154") ; HANGUL LETTER E
       (?$(C$E(B . "0x3155") ; HANGUL LETTER YEO
       (?$(C$F(B . "0x3156") ; HANGUL LETTER YE
       (?$(C$G(B . "0x3157") ; HANGUL LETTER O
       (?$(C$H(B . "0x3158") ; HANGUL LETTER WA
       (?$(C$I(B . "0x3159") ; HANGUL LETTER WAE
       (?$(C$J(B . "0x315A") ; HANGUL LETTER OE
       (?$(C$K(B . "0x315B") ; HANGUL LETTER YO
       (?$(C$L(B . "0x315C") ; HANGUL LETTER U
       (?$(C$M(B . "0x315D") ; HANGUL LETTER WEO
       (?$(C$N(B . "0x315E") ; HANGUL LETTER WE
       (?$(C$O(B . "0x315F") ; HANGUL LETTER WI
       (?$(C$P(B . "0x3160") ; HANGUL LETTER YU
       (?$(C$Q(B . "0x3161") ; HANGUL LETTER EU
       (?$(C$R(B . "0x3162") ; HANGUL LETTER YI
       (?$(C$S(B . "0x3163") ; HANGUL LETTER I
       (?$(C$T(B . "0x3164") ; HANGUL FILLER
       (?$(C$U(B . "0x3165") ; HANGUL LETTER SSANGNIEUN
       (?$(C$V(B . "0x3166") ; HANGUL LETTER NIEUN-TIKEUT
       (?$(C$W(B . "0x3167") ; HANGUL LETTER NIEUN-SIOS
       (?$(C$X(B . "0x3168") ; HANGUL LETTER NIEUN-PANSIOS
       (?$(C$Y(B . "0x3169") ; HANGUL LETTER RIEUL-KIYEOK-SIOS
       (?$(C$Z(B . "0x316A") ; HANGUL LETTER RIEUL-TIKEUT
       (?$(C$[(B . "0x316B") ; HANGUL LETTER RIEUL-PIEUP-SIOS
       (?$(C$\(B . "0x316C") ; HANGUL LETTER RIEUL-PANSIOS
       (?$(C$](B . "0x316D") ; HANGUL LETTER RIEUL-YEORINHIEUH
       (?$(C$^(B . "0x316E") ; HANGUL LETTER MIEUM-PIEUP
       (?$(C$_(B . "0x316F") ; HANGUL LETTER MIEUM-SIOS
       (?$(C$`(B . "0x3170") ; HANGUL LETTER MIEUM-PANSIOS
       (?$(C$a(B . "0x3171") ; HANGUL LETTER KAPYEOUNMIEUM
       (?$(C$b(B . "0x3172") ; HANGUL LETTER PIEUP-KIYEOK
       (?$(C$c(B . "0x3173") ; HANGUL LETTER PIEUP-TIKEUT
       (?$(C$d(B . "0x3174") ; HANGUL LETTER PIEUP-SIOS-KIYEOK
       (?$(C$e(B . "0x3175") ; HANGUL LETTER PIEUP-SIOS-TIKEUT
       (?$(C$f(B . "0x3176") ; HANGUL LETTER PIEUP-CIEUC
       (?$(C$g(B . "0x3177") ; HANGUL LETTER PIEUP-THIEUTH
       (?$(C$h(B . "0x3178") ; HANGUL LETTER KAPYEOUNPIEUP
       (?$(C$i(B . "0x3179") ; HANGUL LETTER KAPYEOUNSSANGPIEUP
       (?$(C$j(B . "0x317A") ; HANGUL LETTER SIOS-KIYEOK
       (?$(C$k(B . "0x317B") ; HANGUL LETTER SIOS-NIEUN
       (?$(C$l(B . "0x317C") ; HANGUL LETTER SIOS-TIKEUT
       (?$(C$m(B . "0x317D") ; HANGUL LETTER SIOS-PIEUP
       (?$(C$n(B . "0x317E") ; HANGUL LETTER SIOS-CIEUC
       (?$(C$o(B . "0x317F") ; HANGUL LETTER PANSIOS
       (?$(C$p(B . "0x3180") ; HANGUL LETTER SSANGIEUNG
       (?$(C$q(B . "0x3181") ; HANGUL LETTER YESIEUNG
       (?$(C$r(B . "0x3182") ; HANGUL LETTER YESIEUNG-SIOS
       (?$(C$s(B . "0x3183") ; HANGUL LETTER YESIEUNG-PANSIOS
       (?$(C$t(B . "0x3184") ; HANGUL LETTER KAPYEOUNPHIEUPH
       (?$(C$u(B . "0x3185") ; HANGUL LETTER SSANGHIEUH
       (?$(C$v(B . "0x3186") ; HANGUL LETTER YEORINHIEUH
       (?$(C$w(B . "0x3187") ; HANGUL LETTER YO-YA
       (?$(C$x(B . "0x3188") ; HANGUL LETTER YO-YAE
       (?$(C$y(B . "0x3189") ; HANGUL LETTER YO-I
       (?$(C$z(B . "0x318A") ; HANGUL LETTER YU-YEO
       (?$(C${(B . "0x318B") ; HANGUL LETTER YU-YE
       (?$(C$|(B . "0x318C") ; HANGUL LETTER YU-I
       (?$(C$}(B . "0x318D") ; HANGUL LETTER ARAEA
       (?$(C$~(B . "0x318E") ; HANGUL LETTER ARAEAE
       (?$(C%!(B . "0x2170") ; SMALL ROMAN NUMERAL ONE
       (?$(C%"(B . "0x2171") ; SMALL ROMAN NUMERAL TWO
       (?$(C%#(B . "0x2172") ; SMALL ROMAN NUMERAL THREE
       (?$(C%$(B . "0x2173") ; SMALL ROMAN NUMERAL FOUR
       (?$(C%%(B . "0x2174") ; SMALL ROMAN NUMERAL FIVE
       (?$(C%&(B . "0x2175") ; SMALL ROMAN NUMERAL SIX
       (?$(C%'(B . "0x2176") ; SMALL ROMAN NUMERAL SEVEN
       (?$(C%((B . "0x2177") ; SMALL ROMAN NUMERAL EIGHT
       (?$(C%)(B . "0x2178") ; SMALL ROMAN NUMERAL NINE
       (?$(C%*(B . "0x2179") ; SMALL ROMAN NUMERAL TEN
       (?$(C%0(B . "0x2160") ; ROMAN NUMERAL ONE
       (?$(C%1(B . "0x2161") ; ROMAN NUMERAL TWO
       (?$(C%2(B . "0x2162") ; ROMAN NUMERAL THREE
       (?$(C%3(B . "0x2163") ; ROMAN NUMERAL FOUR
       (?$(C%4(B . "0x2164") ; ROMAN NUMERAL FIVE
       (?$(C%5(B . "0x2165") ; ROMAN NUMERAL SIX
       (?$(C%6(B . "0x2166") ; ROMAN NUMERAL SEVEN
       (?$(C%7(B . "0x2167") ; ROMAN NUMERAL EIGHT
       (?$(C%8(B . "0x2168") ; ROMAN NUMERAL NINE
       (?$(C%9(B . "0x2169") ; ROMAN NUMERAL TEN
       (?$(C%A(B . "0x0391") ; GREEK CAPITAL LETTER ALPHA
       (?$(C%B(B . "0x0392") ; GREEK CAPITAL LETTER BETA
       (?$(C%C(B . "0x0393") ; GREEK CAPITAL LETTER GAMMA
       (?$(C%D(B . "0x0394") ; GREEK CAPITAL LETTER DELTA
       (?$(C%E(B . "0x0395") ; GREEK CAPITAL LETTER EPSILON
       (?$(C%F(B . "0x0396") ; GREEK CAPITAL LETTER ZETA
       (?$(C%G(B . "0x0397") ; GREEK CAPITAL LETTER ETA
       (?$(C%H(B . "0x0398") ; GREEK CAPITAL LETTER THETA
       (?$(C%I(B . "0x0399") ; GREEK CAPITAL LETTER IOTA
       (?$(C%J(B . "0x039A") ; GREEK CAPITAL LETTER KAPPA
       (?$(C%K(B . "0x039B") ; GREEK CAPITAL LETTER LAMDA
       (?$(C%L(B . "0x039C") ; GREEK CAPITAL LETTER MU
       (?$(C%M(B . "0x039D") ; GREEK CAPITAL LETTER NU
       (?$(C%N(B . "0x039E") ; GREEK CAPITAL LETTER XI
       (?$(C%O(B . "0x039F") ; GREEK CAPITAL LETTER OMICRON
       (?$(C%P(B . "0x03A0") ; GREEK CAPITAL LETTER PI
       (?$(C%Q(B . "0x03A1") ; GREEK CAPITAL LETTER RHO
       (?$(C%R(B . "0x03A3") ; GREEK CAPITAL LETTER SIGMA
       (?$(C%S(B . "0x03A4") ; GREEK CAPITAL LETTER TAU
       (?$(C%T(B . "0x03A5") ; GREEK CAPITAL LETTER UPSILON
       (?$(C%U(B . "0x03A6") ; GREEK CAPITAL LETTER PHI
       (?$(C%V(B . "0x03A7") ; GREEK CAPITAL LETTER CHI
       (?$(C%W(B . "0x03A8") ; GREEK CAPITAL LETTER PSI
       (?$(C%X(B . "0x03A9") ; GREEK CAPITAL LETTER OMEGA
       (?$(C%a(B . "0x03B1") ; GREEK SMALL LETTER ALPHA
       (?$(C%b(B . "0x03B2") ; GREEK SMALL LETTER BETA
       (?$(C%c(B . "0x03B3") ; GREEK SMALL LETTER GAMMA
       (?$(C%d(B . "0x03B4") ; GREEK SMALL LETTER DELTA
       (?$(C%e(B . "0x03B5") ; GREEK SMALL LETTER EPSILON
       (?$(C%f(B . "0x03B6") ; GREEK SMALL LETTER ZETA
       (?$(C%g(B . "0x03B7") ; GREEK SMALL LETTER ETA
       (?$(C%h(B . "0x03B8") ; GREEK SMALL LETTER THETA
       (?$(C%i(B . "0x03B9") ; GREEK SMALL LETTER IOTA
       (?$(C%j(B . "0x03BA") ; GREEK SMALL LETTER KAPPA
       (?$(C%k(B . "0x03BB") ; GREEK SMALL LETTER LAMDA
       (?$(C%l(B . "0x03BC") ; GREEK SMALL LETTER MU
       (?$(C%m(B . "0x03BD") ; GREEK SMALL LETTER NU
       (?$(C%n(B . "0x03BE") ; GREEK SMALL LETTER XI
       (?$(C%o(B . "0x03BF") ; GREEK SMALL LETTER OMICRON
       (?$(C%p(B . "0x03C0") ; GREEK SMALL LETTER PI
       (?$(C%q(B . "0x03C1") ; GREEK SMALL LETTER RHO
       (?$(C%r(B . "0x03C3") ; GREEK SMALL LETTER SIGMA
       (?$(C%s(B . "0x03C4") ; GREEK SMALL LETTER TAU
       (?$(C%t(B . "0x03C5") ; GREEK SMALL LETTER UPSILON
       (?$(C%u(B . "0x03C6") ; GREEK SMALL LETTER PHI
       (?$(C%v(B . "0x03C7") ; GREEK SMALL LETTER CHI
       (?$(C%w(B . "0x03C8") ; GREEK SMALL LETTER PSI
       (?$(C%x(B . "0x03C9") ; GREEK SMALL LETTER OMEGA
       (?$(C&!(B . "0x2500") ; BOX DRAWINGS LIGHT HORIZONTAL
       (?$(C&"(B . "0x2502") ; BOX DRAWINGS LIGHT VERTICAL
       (?$(C&#(B . "0x250C") ; BOX DRAWINGS LIGHT DOWN AND RIGHT
       (?$(C&$(B . "0x2510") ; BOX DRAWINGS LIGHT DOWN AND LEFT
       (?$(C&%(B . "0x2518") ; BOX DRAWINGS LIGHT UP AND LEFT
       (?$(C&&(B . "0x2514") ; BOX DRAWINGS LIGHT UP AND RIGHT
       (?$(C&'(B . "0x251C") ; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
       (?$(C&((B . "0x252C") ; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
       (?$(C&)(B . "0x2524") ; BOX DRAWINGS LIGHT VERTICAL AND LEFT
       (?$(C&*(B . "0x2534") ; BOX DRAWINGS LIGHT UP AND HORIZONTAL
       (?$(C&+(B . "0x253C") ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
       (?$(C&,(B . "0x2501") ; BOX DRAWINGS HEAVY HORIZONTAL
       (?$(C&-(B . "0x2503") ; BOX DRAWINGS HEAVY VERTICAL
       (?$(C&.(B . "0x250F") ; BOX DRAWINGS HEAVY DOWN AND RIGHT
       (?$(C&/(B . "0x2513") ; BOX DRAWINGS HEAVY DOWN AND LEFT
       (?$(C&0(B . "0x251B") ; BOX DRAWINGS HEAVY UP AND LEFT
       (?$(C&1(B . "0x2517") ; BOX DRAWINGS HEAVY UP AND RIGHT
       (?$(C&2(B . "0x2523") ; BOX DRAWINGS HEAVY VERTICAL AND RIGHT
       (?$(C&3(B . "0x2533") ; BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
       (?$(C&4(B . "0x252B") ; BOX DRAWINGS HEAVY VERTICAL AND LEFT
       (?$(C&5(B . "0x253B") ; BOX DRAWINGS HEAVY UP AND HORIZONTAL
       (?$(C&6(B . "0x254B") ; BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
       (?$(C&7(B . "0x2520") ; BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
       (?$(C&8(B . "0x252F") ; BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
       (?$(C&9(B . "0x2528") ; BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
       (?$(C&:(B . "0x2537") ; BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
       (?$(C&;(B . "0x253F") ; BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
       (?$(C&<(B . "0x251D") ; BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
       (?$(C&=(B . "0x2530") ; BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
       (?$(C&>(B . "0x2525") ; BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
       (?$(C&?(B . "0x2538") ; BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
       (?$(C&@(B . "0x2542") ; BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
       (?$(C&A(B . "0x2512") ; BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
       (?$(C&B(B . "0x2511") ; BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
       (?$(C&C(B . "0x251A") ; BOX DRAWINGS UP HEAVY AND LEFT LIGHT
       (?$(C&D(B . "0x2519") ; BOX DRAWINGS UP LIGHT AND LEFT HEAVY
       (?$(C&E(B . "0x2516") ; BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
       (?$(C&F(B . "0x2515") ; BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
       (?$(C&G(B . "0x250E") ; BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
       (?$(C&H(B . "0x250D") ; BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
       (?$(C&I(B . "0x251E") ; BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
       (?$(C&J(B . "0x251F") ; BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
       (?$(C&K(B . "0x2521") ; BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
       (?$(C&L(B . "0x2522") ; BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
       (?$(C&M(B . "0x2526") ; BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
       (?$(C&N(B . "0x2527") ; BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
       (?$(C&O(B . "0x2529") ; BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
       (?$(C&P(B . "0x252A") ; BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
       (?$(C&Q(B . "0x252D") ; BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
       (?$(C&R(B . "0x252E") ; BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
       (?$(C&S(B . "0x2531") ; BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
       (?$(C&T(B . "0x2532") ; BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
       (?$(C&U(B . "0x2535") ; BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
       (?$(C&V(B . "0x2536") ; BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
       (?$(C&W(B . "0x2539") ; BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
       (?$(C&X(B . "0x253A") ; BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
       (?$(C&Y(B . "0x253D") ; BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
       (?$(C&Z(B . "0x253E") ; BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
       (?$(C&[(B . "0x2540") ; BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
       (?$(C&\(B . "0x2541") ; BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
       (?$(C&](B . "0x2543") ; BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
       (?$(C&^(B . "0x2544") ; BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
       (?$(C&_(B . "0x2545") ; BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
       (?$(C&`(B . "0x2546") ; BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
       (?$(C&a(B . "0x2547") ; BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
       (?$(C&b(B . "0x2548") ; BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
       (?$(C&c(B . "0x2549") ; BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
       (?$(C&d(B . "0x254A") ; BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
       (?$(C'!(B . "0x3395") ; SQUARE MU L
       (?$(C'"(B . "0x3396") ; SQUARE ML
       (?$(C'#(B . "0x3397") ; SQUARE DL
       (?$(C'$(B . "0x2113") ; SCRIPT SMALL L
       (?$(C'%(B . "0x3398") ; SQUARE KL
       (?$(C'&(B . "0x33C4") ; SQUARE CC
       (?$(C''(B . "0x33A3") ; SQUARE MM CUBED
       (?$(C'((B . "0x33A4") ; SQUARE CM CUBED
       (?$(C')(B . "0x33A5") ; SQUARE M CUBED
       (?$(C'*(B . "0x33A6") ; SQUARE KM CUBED
       (?$(C'+(B . "0x3399") ; SQUARE FM
       (?$(C',(B . "0x339A") ; SQUARE NM
       (?$(C'-(B . "0x339B") ; SQUARE MU M
       (?$(C'.(B . "0x339C") ; SQUARE MM
       (?$(C'/(B . "0x339D") ; SQUARE CM
       (?$(C'0(B . "0x339E") ; SQUARE KM
       (?$(C'1(B . "0x339F") ; SQUARE MM SQUARED
       (?$(C'2(B . "0x33A0") ; SQUARE CM SQUARED
       (?$(C'3(B . "0x33A1") ; SQUARE M SQUARED
       (?$(C'4(B . "0x33A2") ; SQUARE KM SQUARED
       (?$(C'5(B . "0x33CA") ; SQUARE HA
       (?$(C'6(B . "0x338D") ; SQUARE MU G
       (?$(C'7(B . "0x338E") ; SQUARE MG
       (?$(C'8(B . "0x338F") ; SQUARE KG
       (?$(C'9(B . "0x33CF") ; SQUARE KT
       (?$(C':(B . "0x3388") ; SQUARE CAL
       (?$(C';(B . "0x3389") ; SQUARE KCAL
       (?$(C'<(B . "0x33C8") ; SQUARE DB
       (?$(C'=(B . "0x33A7") ; SQUARE M OVER S
       (?$(C'>(B . "0x33A8") ; SQUARE M OVER S SQUARED
       (?$(C'?(B . "0x33B0") ; SQUARE PS
       (?$(C'@(B . "0x33B1") ; SQUARE NS
       (?$(C'A(B . "0x33B2") ; SQUARE MU S
       (?$(C'B(B . "0x33B3") ; SQUARE MS
       (?$(C'C(B . "0x33B4") ; SQUARE PV
       (?$(C'D(B . "0x33B5") ; SQUARE NV
       (?$(C'E(B . "0x33B6") ; SQUARE MU V
       (?$(C'F(B . "0x33B7") ; SQUARE MV
       (?$(C'G(B . "0x33B8") ; SQUARE KV
       (?$(C'H(B . "0x33B9") ; SQUARE MV MEGA
       (?$(C'I(B . "0x3380") ; SQUARE PA AMPS
       (?$(C'J(B . "0x3381") ; SQUARE NA
       (?$(C'K(B . "0x3382") ; SQUARE MU A
       (?$(C'L(B . "0x3383") ; SQUARE MA
       (?$(C'M(B . "0x3384") ; SQUARE KA
       (?$(C'N(B . "0x33BA") ; SQUARE PW
       (?$(C'O(B . "0x33BB") ; SQUARE NW
       (?$(C'P(B . "0x33BC") ; SQUARE MU W
       (?$(C'Q(B . "0x33BD") ; SQUARE MW
       (?$(C'R(B . "0x33BE") ; SQUARE KW
       (?$(C'S(B . "0x33BF") ; SQUARE MW MEGA
       (?$(C'T(B . "0x3390") ; SQUARE HZ
       (?$(C'U(B . "0x3391") ; SQUARE KHZ
       (?$(C'V(B . "0x3392") ; SQUARE MHZ
       (?$(C'W(B . "0x3393") ; SQUARE GHZ
       (?$(C'X(B . "0x3394") ; SQUARE THZ
       (?$(C'Y(B . "0x2126") ; OHM SIGN
       (?$(C'Z(B . "0x33C0") ; SQUARE K OHM
       (?$(C'[(B . "0x33C1") ; SQUARE M OHM
       (?$(C'\(B . "0x338A") ; SQUARE PF
       (?$(C'](B . "0x338B") ; SQUARE NF
       (?$(C'^(B . "0x338C") ; SQUARE MU F
       (?$(C'_(B . "0x33D6") ; SQUARE MOL
       (?$(C'`(B . "0x33C5") ; SQUARE CD
       (?$(C'a(B . "0x33AD") ; SQUARE RAD
       (?$(C'b(B . "0x33AE") ; SQUARE RAD OVER S
       (?$(C'c(B . "0x33AF") ; SQUARE RAD OVER S SQUARED
       (?$(C'd(B . "0x33DB") ; SQUARE SR
       (?$(C'e(B . "0x33A9") ; SQUARE PA
       (?$(C'f(B . "0x33AA") ; SQUARE KPA
       (?$(C'g(B . "0x33AB") ; SQUARE MPA
       (?$(C'h(B . "0x33AC") ; SQUARE GPA
       (?$(C'i(B . "0x33DD") ; SQUARE WB
       (?$(C'j(B . "0x33D0") ; SQUARE LM
       (?$(C'k(B . "0x33D3") ; SQUARE LX
       (?$(C'l(B . "0x33C3") ; SQUARE BQ
       (?$(C'm(B . "0x33C9") ; SQUARE GY
       (?$(C'n(B . "0x33DC") ; SQUARE SV
       (?$(C'o(B . "0x33C6") ; SQUARE C OVER KG
       (?$(C(!(B . "0x00C6") ; LATIN CAPITAL LIGATURE AE
       (?$(C("(B . "0x00D0") ; LATIN CAPITAL LETTER ETH
       (?$(C(#(B . "0x00AA") ; FEMININE ORDINAL INDICATOR
       (?$(C($(B . "0x0126") ; LATIN CAPITAL LETTER H WITH STROKE
       (?$(C(&(B . "0x0132") ; LATIN CAPITAL LIGATURE IJ
       (?$(C(((B . "0x013F") ; LATIN CAPITAL LETTER L WITH MIDDLE DOT
       (?$(C()(B . "0x0141") ; LATIN CAPITAL LETTER L WITH STROKE
       (?$(C(*(B . "0x00D8") ; LATIN CAPITAL LETTER O WITH STROKE
       (?$(C(+(B . "0x0152") ; LATIN CAPITAL LIGATURE OE
       (?$(C(,(B . "0x00BA") ; MASCULINE ORDINAL INDICATOR
       (?$(C(-(B . "0x00DE") ; LATIN CAPITAL LETTER THORN
       (?$(C(.(B . "0x0166") ; LATIN CAPITAL LETTER T WITH STROKE
       (?$(C(/(B . "0x014A") ; LATIN CAPITAL LETTER ENG
       (?$(C(1(B . "0x3260") ; CIRCLED HANGUL KIYEOK
       (?$(C(2(B . "0x3261") ; CIRCLED HANGUL NIEUN
       (?$(C(3(B . "0x3262") ; CIRCLED HANGUL TIKEUT
       (?$(C(4(B . "0x3263") ; CIRCLED HANGUL RIEUL
       (?$(C(5(B . "0x3264") ; CIRCLED HANGUL MIEUM
       (?$(C(6(B . "0x3265") ; CIRCLED HANGUL PIEUP
       (?$(C(7(B . "0x3266") ; CIRCLED HANGUL SIOS
       (?$(C(8(B . "0x3267") ; CIRCLED HANGUL IEUNG
       (?$(C(9(B . "0x3268") ; CIRCLED HANGUL CIEUC
       (?$(C(:(B . "0x3269") ; CIRCLED HANGUL CHIEUCH
       (?$(C(;(B . "0x326A") ; CIRCLED HANGUL KHIEUKH
       (?$(C(<(B . "0x326B") ; CIRCLED HANGUL THIEUTH
       (?$(C(=(B . "0x326C") ; CIRCLED HANGUL PHIEUPH
       (?$(C(>(B . "0x326D") ; CIRCLED HANGUL HIEUH
       (?$(C(?(B . "0x326E") ; CIRCLED HANGUL KIYEOK A
       (?$(C(@(B . "0x326F") ; CIRCLED HANGUL NIEUN A
       (?$(C(A(B . "0x3270") ; CIRCLED HANGUL TIKEUT A
       (?$(C(B(B . "0x3271") ; CIRCLED HANGUL RIEUL A
       (?$(C(C(B . "0x3272") ; CIRCLED HANGUL MIEUM A
       (?$(C(D(B . "0x3273") ; CIRCLED HANGUL PIEUP A
       (?$(C(E(B . "0x3274") ; CIRCLED HANGUL SIOS A
       (?$(C(F(B . "0x3275") ; CIRCLED HANGUL IEUNG A
       (?$(C(G(B . "0x3276") ; CIRCLED HANGUL CIEUC A
       (?$(C(H(B . "0x3277") ; CIRCLED HANGUL CHIEUCH A
       (?$(C(I(B . "0x3278") ; CIRCLED HANGUL KHIEUKH A
       (?$(C(J(B . "0x3279") ; CIRCLED HANGUL THIEUTH A
       (?$(C(K(B . "0x327A") ; CIRCLED HANGUL PHIEUPH A
       (?$(C(L(B . "0x327B") ; CIRCLED HANGUL HIEUH A
       (?$(C(M(B . "0x24D0") ; CIRCLED LATIN SMALL LETTER A
       (?$(C(N(B . "0x24D1") ; CIRCLED LATIN SMALL LETTER B
       (?$(C(O(B . "0x24D2") ; CIRCLED LATIN SMALL LETTER C
       (?$(C(P(B . "0x24D3") ; CIRCLED LATIN SMALL LETTER D
       (?$(C(Q(B . "0x24D4") ; CIRCLED LATIN SMALL LETTER E
       (?$(C(R(B . "0x24D5") ; CIRCLED LATIN SMALL LETTER F
       (?$(C(S(B . "0x24D6") ; CIRCLED LATIN SMALL LETTER G
       (?$(C(T(B . "0x24D7") ; CIRCLED LATIN SMALL LETTER H
       (?$(C(U(B . "0x24D8") ; CIRCLED LATIN SMALL LETTER I
       (?$(C(V(B . "0x24D9") ; CIRCLED LATIN SMALL LETTER J
       (?$(C(W(B . "0x24DA") ; CIRCLED LATIN SMALL LETTER K
       (?$(C(X(B . "0x24DB") ; CIRCLED LATIN SMALL LETTER L
       (?$(C(Y(B . "0x24DC") ; CIRCLED LATIN SMALL LETTER M
       (?$(C(Z(B . "0x24DD") ; CIRCLED LATIN SMALL LETTER N
       (?$(C([(B . "0x24DE") ; CIRCLED LATIN SMALL LETTER O
       (?$(C(\(B . "0x24DF") ; CIRCLED LATIN SMALL LETTER P
       (?$(C(](B . "0x24E0") ; CIRCLED LATIN SMALL LETTER Q
       (?$(C(^(B . "0x24E1") ; CIRCLED LATIN SMALL LETTER R
       (?$(C(_(B . "0x24E2") ; CIRCLED LATIN SMALL LETTER S
       (?$(C(`(B . "0x24E3") ; CIRCLED LATIN SMALL LETTER T
       (?$(C(a(B . "0x24E4") ; CIRCLED LATIN SMALL LETTER U
       (?$(C(b(B . "0x24E5") ; CIRCLED LATIN SMALL LETTER V
       (?$(C(c(B . "0x24E6") ; CIRCLED LATIN SMALL LETTER W
       (?$(C(d(B . "0x24E7") ; CIRCLED LATIN SMALL LETTER X
       (?$(C(e(B . "0x24E8") ; CIRCLED LATIN SMALL LETTER Y
       (?$(C(f(B . "0x24E9") ; CIRCLED LATIN SMALL LETTER Z
       (?$(C(g(B . "0x2460") ; CIRCLED DIGIT ONE
       (?$(C(h(B . "0x2461") ; CIRCLED DIGIT TWO
       (?$(C(i(B . "0x2462") ; CIRCLED DIGIT THREE
       (?$(C(j(B . "0x2463") ; CIRCLED DIGIT FOUR
       (?$(C(k(B . "0x2464") ; CIRCLED DIGIT FIVE
       (?$(C(l(B . "0x2465") ; CIRCLED DIGIT SIX
       (?$(C(m(B . "0x2466") ; CIRCLED DIGIT SEVEN
       (?$(C(n(B . "0x2467") ; CIRCLED DIGIT EIGHT
       (?$(C(o(B . "0x2468") ; CIRCLED DIGIT NINE
       (?$(C(p(B . "0x2469") ; CIRCLED NUMBER TEN
       (?$(C(q(B . "0x246A") ; CIRCLED NUMBER ELEVEN
       (?$(C(r(B . "0x246B") ; CIRCLED NUMBER TWELVE
       (?$(C(s(B . "0x246C") ; CIRCLED NUMBER THIRTEEN
       (?$(C(t(B . "0x246D") ; CIRCLED NUMBER FOURTEEN
       (?$(C(u(B . "0x246E") ; CIRCLED NUMBER FIFTEEN
       (?$(C(v(B . "0x00BD") ; VULGAR FRACTION ONE HALF
       (?$(C(w(B . "0x2153") ; VULGAR FRACTION ONE THIRD
       (?$(C(x(B . "0x2154") ; VULGAR FRACTION TWO THIRDS
       (?$(C(y(B . "0x00BC") ; VULGAR FRACTION ONE QUARTER
       (?$(C(z(B . "0x00BE") ; VULGAR FRACTION THREE QUARTERS
       (?$(C({(B . "0x215B") ; VULGAR FRACTION ONE EIGHTH
       (?$(C(|(B . "0x215C") ; VULGAR FRACTION THREE EIGHTHS
       (?$(C(}(B . "0x215D") ; VULGAR FRACTION FIVE EIGHTHS
       (?$(C(~(B . "0x215E") ; VULGAR FRACTION SEVEN EIGHTHS
       (?$(C)!(B . "0x00E6") ; LATIN SMALL LIGATURE AE
       (?$(C)"(B . "0x0111") ; LATIN SMALL LETTER D WITH STROKE
       (?$(C)#(B . "0x00F0") ; LATIN SMALL LETTER ETH
       (?$(C)$(B . "0x0127") ; LATIN SMALL LETTER H WITH STROKE
       (?$(C)%(B . "0x0131") ; LATIN SMALL LETTER DOTLESS I
       (?$(C)&(B . "0x0133") ; LATIN SMALL LIGATURE IJ
       (?$(C)'(B . "0x0138") ; LATIN SMALL LETTER KRA
       (?$(C)((B . "0x0140") ; LATIN SMALL LETTER L WITH MIDDLE DOT
       (?$(C))(B . "0x0142") ; LATIN SMALL LETTER L WITH STROKE
       (?$(C)*(B . "0x00F8") ; LATIN SMALL LETTER O WITH STROKE
       (?$(C)+(B . "0x0153") ; LATIN SMALL LIGATURE OE
       (?$(C),(B . "0x00DF") ; LATIN SMALL LETTER SHARP S
       (?$(C)-(B . "0x00FE") ; LATIN SMALL LETTER THORN
       (?$(C).(B . "0x0167") ; LATIN SMALL LETTER T WITH STROKE
       (?$(C)/(B . "0x014B") ; LATIN SMALL LETTER ENG
       (?$(C)0(B . "0x0149") ; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
       (?$(C)1(B . "0x3200") ; PARENTHESIZED HANGUL KIYEOK
       (?$(C)2(B . "0x3201") ; PARENTHESIZED HANGUL NIEUN
       (?$(C)3(B . "0x3202") ; PARENTHESIZED HANGUL TIKEUT
       (?$(C)4(B . "0x3203") ; PARENTHESIZED HANGUL RIEUL
       (?$(C)5(B . "0x3204") ; PARENTHESIZED HANGUL MIEUM
       (?$(C)6(B . "0x3205") ; PARENTHESIZED HANGUL PIEUP
       (?$(C)7(B . "0x3206") ; PARENTHESIZED HANGUL SIOS
       (?$(C)8(B . "0x3207") ; PARENTHESIZED HANGUL IEUNG
       (?$(C)9(B . "0x3208") ; PARENTHESIZED HANGUL CIEUC
       (?$(C):(B . "0x3209") ; PARENTHESIZED HANGUL CHIEUCH
       (?$(C);(B . "0x320A") ; PARENTHESIZED HANGUL KHIEUKH
       (?$(C)<(B . "0x320B") ; PARENTHESIZED HANGUL THIEUTH
       (?$(C)=(B . "0x320C") ; PARENTHESIZED HANGUL PHIEUPH
       (?$(C)>(B . "0x320D") ; PARENTHESIZED HANGUL HIEUH
       (?$(C)?(B . "0x320E") ; PARENTHESIZED HANGUL KIYEOK A
       (?$(C)@(B . "0x320F") ; PARENTHESIZED HANGUL NIEUN A
       (?$(C)A(B . "0x3210") ; PARENTHESIZED HANGUL TIKEUT A
       (?$(C)B(B . "0x3211") ; PARENTHESIZED HANGUL RIEUL A
       (?$(C)C(B . "0x3212") ; PARENTHESIZED HANGUL MIEUM A
       (?$(C)D(B . "0x3213") ; PARENTHESIZED HANGUL PIEUP A
       (?$(C)E(B . "0x3214") ; PARENTHESIZED HANGUL SIOS A
       (?$(C)F(B . "0x3215") ; PARENTHESIZED HANGUL IEUNG A
       (?$(C)G(B . "0x3216") ; PARENTHESIZED HANGUL CIEUC A
       (?$(C)H(B . "0x3217") ; PARENTHESIZED HANGUL CHIEUCH A
       (?$(C)I(B . "0x3218") ; PARENTHESIZED HANGUL KHIEUKH A
       (?$(C)J(B . "0x3219") ; PARENTHESIZED HANGUL THIEUTH A
       (?$(C)K(B . "0x321A") ; PARENTHESIZED HANGUL PHIEUPH A
       (?$(C)L(B . "0x321B") ; PARENTHESIZED HANGUL HIEUH A
       (?$(C)M(B . "0x249C") ; PARENTHESIZED LATIN SMALL LETTER A
       (?$(C)N(B . "0x249D") ; PARENTHESIZED LATIN SMALL LETTER B
       (?$(C)O(B . "0x249E") ; PARENTHESIZED LATIN SMALL LETTER C
       (?$(C)P(B . "0x249F") ; PARENTHESIZED LATIN SMALL LETTER D
       (?$(C)Q(B . "0x24A0") ; PARENTHESIZED LATIN SMALL LETTER E
       (?$(C)R(B . "0x24A1") ; PARENTHESIZED LATIN SMALL LETTER F
       (?$(C)S(B . "0x24A2") ; PARENTHESIZED LATIN SMALL LETTER G
       (?$(C)T(B . "0x24A3") ; PARENTHESIZED LATIN SMALL LETTER H
       (?$(C)U(B . "0x24A4") ; PARENTHESIZED LATIN SMALL LETTER I
       (?$(C)V(B . "0x24A5") ; PARENTHESIZED LATIN SMALL LETTER J
       (?$(C)W(B . "0x24A6") ; PARENTHESIZED LATIN SMALL LETTER K
       (?$(C)X(B . "0x24A7") ; PARENTHESIZED LATIN SMALL LETTER L
       (?$(C)Y(B . "0x24A8") ; PARENTHESIZED LATIN SMALL LETTER M
       (?$(C)Z(B . "0x24A9") ; PARENTHESIZED LATIN SMALL LETTER N
       (?$(C)[(B . "0x24AA") ; PARENTHESIZED LATIN SMALL LETTER O
       (?$(C)\(B . "0x24AB") ; PARENTHESIZED LATIN SMALL LETTER P
       (?$(C)](B . "0x24AC") ; PARENTHESIZED LATIN SMALL LETTER Q
       (?$(C)^(B . "0x24AD") ; PARENTHESIZED LATIN SMALL LETTER R
       (?$(C)_(B . "0x24AE") ; PARENTHESIZED LATIN SMALL LETTER S
       (?$(C)`(B . "0x24AF") ; PARENTHESIZED LATIN SMALL LETTER T
       (?$(C)a(B . "0x24B0") ; PARENTHESIZED LATIN SMALL LETTER U
       (?$(C)b(B . "0x24B1") ; PARENTHESIZED LATIN SMALL LETTER V
       (?$(C)c(B . "0x24B2") ; PARENTHESIZED LATIN SMALL LETTER W
       (?$(C)d(B . "0x24B3") ; PARENTHESIZED LATIN SMALL LETTER X
       (?$(C)e(B . "0x24B4") ; PARENTHESIZED LATIN SMALL LETTER Y
       (?$(C)f(B . "0x24B5") ; PARENTHESIZED LATIN SMALL LETTER Z
       (?$(C)g(B . "0x2474") ; PARENTHESIZED DIGIT ONE
       (?$(C)h(B . "0x2475") ; PARENTHESIZED DIGIT TWO
       (?$(C)i(B . "0x2476") ; PARENTHESIZED DIGIT THREE
       (?$(C)j(B . "0x2477") ; PARENTHESIZED DIGIT FOUR
       (?$(C)k(B . "0x2478") ; PARENTHESIZED DIGIT FIVE
       (?$(C)l(B . "0x2479") ; PARENTHESIZED DIGIT SIX
       (?$(C)m(B . "0x247A") ; PARENTHESIZED DIGIT SEVEN
       (?$(C)n(B . "0x247B") ; PARENTHESIZED DIGIT EIGHT
       (?$(C)o(B . "0x247C") ; PARENTHESIZED DIGIT NINE
       (?$(C)p(B . "0x247D") ; PARENTHESIZED NUMBER TEN
       (?$(C)q(B . "0x247E") ; PARENTHESIZED NUMBER ELEVEN
       (?$(C)r(B . "0x247F") ; PARENTHESIZED NUMBER TWELVE
       (?$(C)s(B . "0x2480") ; PARENTHESIZED NUMBER THIRTEEN
       (?$(C)t(B . "0x2481") ; PARENTHESIZED NUMBER FOURTEEN
       (?$(C)u(B . "0x2482") ; PARENTHESIZED NUMBER FIFTEEN
       (?$(C)v(B . "0x00B9") ; SUPERSCRIPT ONE
       (?$(C)w(B . "0x00B2") ; SUPERSCRIPT TWO
       (?$(C)x(B . "0x00B3") ; SUPERSCRIPT THREE
       (?$(C)y(B . "0x2074") ; SUPERSCRIPT FOUR
       (?$(C)z(B . "0x207F") ; SUPERSCRIPT LATIN SMALL LETTER N
       (?$(C){(B . "0x2081") ; SUBSCRIPT ONE
       (?$(C)|(B . "0x2082") ; SUBSCRIPT TWO
       (?$(C)}(B . "0x2083") ; SUBSCRIPT THREE
       (?$(C)~(B . "0x2084") ; SUBSCRIPT FOUR
       (?$(C*!(B . "0x3041") ; HIRAGANA LETTER SMALL A
       (?$(C*"(B . "0x3042") ; HIRAGANA LETTER A
       (?$(C*#(B . "0x3043") ; HIRAGANA LETTER SMALL I
       (?$(C*$(B . "0x3044") ; HIRAGANA LETTER I
       (?$(C*%(B . "0x3045") ; HIRAGANA LETTER SMALL U
       (?$(C*&(B . "0x3046") ; HIRAGANA LETTER U
       (?$(C*'(B . "0x3047") ; HIRAGANA LETTER SMALL E
       (?$(C*((B . "0x3048") ; HIRAGANA LETTER E
       (?$(C*)(B . "0x3049") ; HIRAGANA LETTER SMALL O
       (?$(C**(B . "0x304A") ; HIRAGANA LETTER O
       (?$(C*+(B . "0x304B") ; HIRAGANA LETTER KA
       (?$(C*,(B . "0x304C") ; HIRAGANA LETTER GA
       (?$(C*-(B . "0x304D") ; HIRAGANA LETTER KI
       (?$(C*.(B . "0x304E") ; HIRAGANA LETTER GI
       (?$(C*/(B . "0x304F") ; HIRAGANA LETTER KU
       (?$(C*0(B . "0x3050") ; HIRAGANA LETTER GU
       (?$(C*1(B . "0x3051") ; HIRAGANA LETTER KE
       (?$(C*2(B . "0x3052") ; HIRAGANA LETTER GE
       (?$(C*3(B . "0x3053") ; HIRAGANA LETTER KO
       (?$(C*4(B . "0x3054") ; HIRAGANA LETTER GO
       (?$(C*5(B . "0x3055") ; HIRAGANA LETTER SA
       (?$(C*6(B . "0x3056") ; HIRAGANA LETTER ZA
       (?$(C*7(B . "0x3057") ; HIRAGANA LETTER SI
       (?$(C*8(B . "0x3058") ; HIRAGANA LETTER ZI
       (?$(C*9(B . "0x3059") ; HIRAGANA LETTER SU
       (?$(C*:(B . "0x305A") ; HIRAGANA LETTER ZU
       (?$(C*;(B . "0x305B") ; HIRAGANA LETTER SE
       (?$(C*<(B . "0x305C") ; HIRAGANA LETTER ZE
       (?$(C*=(B . "0x305D") ; HIRAGANA LETTER SO
       (?$(C*>(B . "0x305E") ; HIRAGANA LETTER ZO
       (?$(C*?(B . "0x305F") ; HIRAGANA LETTER TA
       (?$(C*@(B . "0x3060") ; HIRAGANA LETTER DA
       (?$(C*A(B . "0x3061") ; HIRAGANA LETTER TI
       (?$(C*B(B . "0x3062") ; HIRAGANA LETTER DI
       (?$(C*C(B . "0x3063") ; HIRAGANA LETTER SMALL TU
       (?$(C*D(B . "0x3064") ; HIRAGANA LETTER TU
       (?$(C*E(B . "0x3065") ; HIRAGANA LETTER DU
       (?$(C*F(B . "0x3066") ; HIRAGANA LETTER TE
       (?$(C*G(B . "0x3067") ; HIRAGANA LETTER DE
       (?$(C*H(B . "0x3068") ; HIRAGANA LETTER TO
       (?$(C*I(B . "0x3069") ; HIRAGANA LETTER DO
       (?$(C*J(B . "0x306A") ; HIRAGANA LETTER NA
       (?$(C*K(B . "0x306B") ; HIRAGANA LETTER NI
       (?$(C*L(B . "0x306C") ; HIRAGANA LETTER NU
       (?$(C*M(B . "0x306D") ; HIRAGANA LETTER NE
       (?$(C*N(B . "0x306E") ; HIRAGANA LETTER NO
       (?$(C*O(B . "0x306F") ; HIRAGANA LETTER HA
       (?$(C*P(B . "0x3070") ; HIRAGANA LETTER BA
       (?$(C*Q(B . "0x3071") ; HIRAGANA LETTER PA
       (?$(C*R(B . "0x3072") ; HIRAGANA LETTER HI
       (?$(C*S(B . "0x3073") ; HIRAGANA LETTER BI
       (?$(C*T(B . "0x3074") ; HIRAGANA LETTER PI
       (?$(C*U(B . "0x3075") ; HIRAGANA LETTER HU
       (?$(C*V(B . "0x3076") ; HIRAGANA LETTER BU
       (?$(C*W(B . "0x3077") ; HIRAGANA LETTER PU
       (?$(C*X(B . "0x3078") ; HIRAGANA LETTER HE
       (?$(C*Y(B . "0x3079") ; HIRAGANA LETTER BE
       (?$(C*Z(B . "0x307A") ; HIRAGANA LETTER PE
       (?$(C*[(B . "0x307B") ; HIRAGANA LETTER HO
       (?$(C*\(B . "0x307C") ; HIRAGANA LETTER BO
       (?$(C*](B . "0x307D") ; HIRAGANA LETTER PO
       (?$(C*^(B . "0x307E") ; HIRAGANA LETTER MA
       (?$(C*_(B . "0x307F") ; HIRAGANA LETTER MI
       (?$(C*`(B . "0x3080") ; HIRAGANA LETTER MU
       (?$(C*a(B . "0x3081") ; HIRAGANA LETTER ME
       (?$(C*b(B . "0x3082") ; HIRAGANA LETTER MO
       (?$(C*c(B . "0x3083") ; HIRAGANA LETTER SMALL YA
       (?$(C*d(B . "0x3084") ; HIRAGANA LETTER YA
       (?$(C*e(B . "0x3085") ; HIRAGANA LETTER SMALL YU
       (?$(C*f(B . "0x3086") ; HIRAGANA LETTER YU
       (?$(C*g(B . "0x3087") ; HIRAGANA LETTER SMALL YO
       (?$(C*h(B . "0x3088") ; HIRAGANA LETTER YO
       (?$(C*i(B . "0x3089") ; HIRAGANA LETTER RA
       (?$(C*j(B . "0x308A") ; HIRAGANA LETTER RI
       (?$(C*k(B . "0x308B") ; HIRAGANA LETTER RU
       (?$(C*l(B . "0x308C") ; HIRAGANA LETTER RE
       (?$(C*m(B . "0x308D") ; HIRAGANA LETTER RO
       (?$(C*n(B . "0x308E") ; HIRAGANA LETTER SMALL WA
       (?$(C*o(B . "0x308F") ; HIRAGANA LETTER WA
       (?$(C*p(B . "0x3090") ; HIRAGANA LETTER WI
       (?$(C*q(B . "0x3091") ; HIRAGANA LETTER WE
       (?$(C*r(B . "0x3092") ; HIRAGANA LETTER WO
       (?$(C*s(B . "0x3093") ; HIRAGANA LETTER N
       (?$(C+!(B . "0x30A1") ; KATAKANA LETTER SMALL A
       (?$(C+"(B . "0x30A2") ; KATAKANA LETTER A
       (?$(C+#(B . "0x30A3") ; KATAKANA LETTER SMALL I
       (?$(C+$(B . "0x30A4") ; KATAKANA LETTER I
       (?$(C+%(B . "0x30A5") ; KATAKANA LETTER SMALL U
       (?$(C+&(B . "0x30A6") ; KATAKANA LETTER U
       (?$(C+'(B . "0x30A7") ; KATAKANA LETTER SMALL E
       (?$(C+((B . "0x30A8") ; KATAKANA LETTER E
       (?$(C+)(B . "0x30A9") ; KATAKANA LETTER SMALL O
       (?$(C+*(B . "0x30AA") ; KATAKANA LETTER O
       (?$(C++(B . "0x30AB") ; KATAKANA LETTER KA
       (?$(C+,(B . "0x30AC") ; KATAKANA LETTER GA
       (?$(C+-(B . "0x30AD") ; KATAKANA LETTER KI
       (?$(C+.(B . "0x30AE") ; KATAKANA LETTER GI
       (?$(C+/(B . "0x30AF") ; KATAKANA LETTER KU
       (?$(C+0(B . "0x30B0") ; KATAKANA LETTER GU
       (?$(C+1(B . "0x30B1") ; KATAKANA LETTER KE
       (?$(C+2(B . "0x30B2") ; KATAKANA LETTER GE
       (?$(C+3(B . "0x30B3") ; KATAKANA LETTER KO
       (?$(C+4(B . "0x30B4") ; KATAKANA LETTER GO
       (?$(C+5(B . "0x30B5") ; KATAKANA LETTER SA
       (?$(C+6(B . "0x30B6") ; KATAKANA LETTER ZA
       (?$(C+7(B . "0x30B7") ; KATAKANA LETTER SI
       (?$(C+8(B . "0x30B8") ; KATAKANA LETTER ZI
       (?$(C+9(B . "0x30B9") ; KATAKANA LETTER SU
       (?$(C+:(B . "0x30BA") ; KATAKANA LETTER ZU
       (?$(C+;(B . "0x30BB") ; KATAKANA LETTER SE
       (?$(C+<(B . "0x30BC") ; KATAKANA LETTER ZE
       (?$(C+=(B . "0x30BD") ; KATAKANA LETTER SO
       (?$(C+>(B . "0x30BE") ; KATAKANA LETTER ZO
       (?$(C+?(B . "0x30BF") ; KATAKANA LETTER TA
       (?$(C+@(B . "0x30C0") ; KATAKANA LETTER DA
       (?$(C+A(B . "0x30C1") ; KATAKANA LETTER TI
       (?$(C+B(B . "0x30C2") ; KATAKANA LETTER DI
       (?$(C+C(B . "0x30C3") ; KATAKANA LETTER SMALL TU
       (?$(C+D(B . "0x30C4") ; KATAKANA LETTER TU
       (?$(C+E(B . "0x30C5") ; KATAKANA LETTER DU
       (?$(C+F(B . "0x30C6") ; KATAKANA LETTER TE
       (?$(C+G(B . "0x30C7") ; KATAKANA LETTER DE
       (?$(C+H(B . "0x30C8") ; KATAKANA LETTER TO
       (?$(C+I(B . "0x30C9") ; KATAKANA LETTER DO
       (?$(C+J(B . "0x30CA") ; KATAKANA LETTER NA
       (?$(C+K(B . "0x30CB") ; KATAKANA LETTER NI
       (?$(C+L(B . "0x30CC") ; KATAKANA LETTER NU
       (?$(C+M(B . "0x30CD") ; KATAKANA LETTER NE
       (?$(C+N(B . "0x30CE") ; KATAKANA LETTER NO
       (?$(C+O(B . "0x30CF") ; KATAKANA LETTER HA
       (?$(C+P(B . "0x30D0") ; KATAKANA LETTER BA
       (?$(C+Q(B . "0x30D1") ; KATAKANA LETTER PA
       (?$(C+R(B . "0x30D2") ; KATAKANA LETTER HI
       (?$(C+S(B . "0x30D3") ; KATAKANA LETTER BI
       (?$(C+T(B . "0x30D4") ; KATAKANA LETTER PI
       (?$(C+U(B . "0x30D5") ; KATAKANA LETTER HU
       (?$(C+V(B . "0x30D6") ; KATAKANA LETTER BU
       (?$(C+W(B . "0x30D7") ; KATAKANA LETTER PU
       (?$(C+X(B . "0x30D8") ; KATAKANA LETTER HE
       (?$(C+Y(B . "0x30D9") ; KATAKANA LETTER BE
       (?$(C+Z(B . "0x30DA") ; KATAKANA LETTER PE
       (?$(C+[(B . "0x30DB") ; KATAKANA LETTER HO
       (?$(C+\(B . "0x30DC") ; KATAKANA LETTER BO
       (?$(C+](B . "0x30DD") ; KATAKANA LETTER PO
       (?$(C+^(B . "0x30DE") ; KATAKANA LETTER MA
       (?$(C+_(B . "0x30DF") ; KATAKANA LETTER MI
       (?$(C+`(B . "0x30E0") ; KATAKANA LETTER MU
       (?$(C+a(B . "0x30E1") ; KATAKANA LETTER ME
       (?$(C+b(B . "0x30E2") ; KATAKANA LETTER MO
       (?$(C+c(B . "0x30E3") ; KATAKANA LETTER SMALL YA
       (?$(C+d(B . "0x30E4") ; KATAKANA LETTER YA
       (?$(C+e(B . "0x30E5") ; KATAKANA LETTER SMALL YU
       (?$(C+f(B . "0x30E6") ; KATAKANA LETTER YU
       (?$(C+g(B . "0x30E7") ; KATAKANA LETTER SMALL YO
       (?$(C+h(B . "0x30E8") ; KATAKANA LETTER YO
       (?$(C+i(B . "0x30E9") ; KATAKANA LETTER RA
       (?$(C+j(B . "0x30EA") ; KATAKANA LETTER RI
       (?$(C+k(B . "0x30EB") ; KATAKANA LETTER RU
       (?$(C+l(B . "0x30EC") ; KATAKANA LETTER RE
       (?$(C+m(B . "0x30ED") ; KATAKANA LETTER RO
       (?$(C+n(B . "0x30EE") ; KATAKANA LETTER SMALL WA
       (?$(C+o(B . "0x30EF") ; KATAKANA LETTER WA
       (?$(C+p(B . "0x30F0") ; KATAKANA LETTER WI
       (?$(C+q(B . "0x30F1") ; KATAKANA LETTER WE
       (?$(C+r(B . "0x30F2") ; KATAKANA LETTER WO
       (?$(C+s(B . "0x30F3") ; KATAKANA LETTER N
       (?$(C+t(B . "0x30F4") ; KATAKANA LETTER VU
       (?$(C+u(B . "0x30F5") ; KATAKANA LETTER SMALL KA
       (?$(C+v(B . "0x30F6") ; KATAKANA LETTER SMALL KE
       (?$(C,!(B . "0x0410") ; CYRILLIC CAPITAL LETTER A
       (?$(C,"(B . "0x0411") ; CYRILLIC CAPITAL LETTER BE
       (?$(C,#(B . "0x0412") ; CYRILLIC CAPITAL LETTER VE
       (?$(C,$(B . "0x0413") ; CYRILLIC CAPITAL LETTER GHE
       (?$(C,%(B . "0x0414") ; CYRILLIC CAPITAL LETTER DE
       (?$(C,&(B . "0x0415") ; CYRILLIC CAPITAL LETTER IE
       (?$(C,'(B . "0x0401") ; CYRILLIC CAPITAL LETTER IO
       (?$(C,((B . "0x0416") ; CYRILLIC CAPITAL LETTER ZHE
       (?$(C,)(B . "0x0417") ; CYRILLIC CAPITAL LETTER ZE
       (?$(C,*(B . "0x0418") ; CYRILLIC CAPITAL LETTER I
       (?$(C,+(B . "0x0419") ; CYRILLIC CAPITAL LETTER SHORT I
       (?$(C,,(B . "0x041A") ; CYRILLIC CAPITAL LETTER KA
       (?$(C,-(B . "0x041B") ; CYRILLIC CAPITAL LETTER EL
       (?$(C,.(B . "0x041C") ; CYRILLIC CAPITAL LETTER EM
       (?$(C,/(B . "0x041D") ; CYRILLIC CAPITAL LETTER EN
       (?$(C,0(B . "0x041E") ; CYRILLIC CAPITAL LETTER O
       (?$(C,1(B . "0x041F") ; CYRILLIC CAPITAL LETTER PE
       (?$(C,2(B . "0x0420") ; CYRILLIC CAPITAL LETTER ER
       (?$(C,3(B . "0x0421") ; CYRILLIC CAPITAL LETTER ES
       (?$(C,4(B . "0x0422") ; CYRILLIC CAPITAL LETTER TE
       (?$(C,5(B . "0x0423") ; CYRILLIC CAPITAL LETTER U
       (?$(C,6(B . "0x0424") ; CYRILLIC CAPITAL LETTER EF
       (?$(C,7(B . "0x0425") ; CYRILLIC CAPITAL LETTER HA
       (?$(C,8(B . "0x0426") ; CYRILLIC CAPITAL LETTER TSE
       (?$(C,9(B . "0x0427") ; CYRILLIC CAPITAL LETTER CHE
       (?$(C,:(B . "0x0428") ; CYRILLIC CAPITAL LETTER SHA
       (?$(C,;(B . "0x0429") ; CYRILLIC CAPITAL LETTER SHCHA
       (?$(C,<(B . "0x042A") ; CYRILLIC CAPITAL LETTER HARD SIGN
       (?$(C,=(B . "0x042B") ; CYRILLIC CAPITAL LETTER YERU
       (?$(C,>(B . "0x042C") ; CYRILLIC CAPITAL LETTER SOFT SIGN
       (?$(C,?(B . "0x042D") ; CYRILLIC CAPITAL LETTER E
       (?$(C,@(B . "0x042E") ; CYRILLIC CAPITAL LETTER YU
       (?$(C,A(B . "0x042F") ; CYRILLIC CAPITAL LETTER YA
       (?$(C,Q(B . "0x0430") ; CYRILLIC SMALL LETTER A
       (?$(C,R(B . "0x0431") ; CYRILLIC SMALL LETTER BE
       (?$(C,S(B . "0x0432") ; CYRILLIC SMALL LETTER VE
       (?$(C,T(B . "0x0433") ; CYRILLIC SMALL LETTER GHE
       (?$(C,U(B . "0x0434") ; CYRILLIC SMALL LETTER DE
       (?$(C,V(B . "0x0435") ; CYRILLIC SMALL LETTER IE
       (?$(C,W(B . "0x0451") ; CYRILLIC SMALL LETTER IO
       (?$(C,X(B . "0x0436") ; CYRILLIC SMALL LETTER ZHE
       (?$(C,Y(B . "0x0437") ; CYRILLIC SMALL LETTER ZE
       (?$(C,Z(B . "0x0438") ; CYRILLIC SMALL LETTER I
       (?$(C,[(B . "0x0439") ; CYRILLIC SMALL LETTER SHORT I
       (?$(C,\(B . "0x043A") ; CYRILLIC SMALL LETTER KA
       (?$(C,](B . "0x043B") ; CYRILLIC SMALL LETTER EL
       (?$(C,^(B . "0x043C") ; CYRILLIC SMALL LETTER EM
       (?$(C,_(B . "0x043D") ; CYRILLIC SMALL LETTER EN
       (?$(C,`(B . "0x043E") ; CYRILLIC SMALL LETTER O
       (?$(C,a(B . "0x043F") ; CYRILLIC SMALL LETTER PE
       (?$(C,b(B . "0x0440") ; CYRILLIC SMALL LETTER ER
       (?$(C,c(B . "0x0441") ; CYRILLIC SMALL LETTER ES
       (?$(C,d(B . "0x0442") ; CYRILLIC SMALL LETTER TE
       (?$(C,e(B . "0x0443") ; CYRILLIC SMALL LETTER U
       (?$(C,f(B . "0x0444") ; CYRILLIC SMALL LETTER EF
       (?$(C,g(B . "0x0445") ; CYRILLIC SMALL LETTER HA
       (?$(C,h(B . "0x0446") ; CYRILLIC SMALL LETTER TSE
       (?$(C,i(B . "0x0447") ; CYRILLIC SMALL LETTER CHE
       (?$(C,j(B . "0x0448") ; CYRILLIC SMALL LETTER SHA
       (?$(C,k(B . "0x0449") ; CYRILLIC SMALL LETTER SHCHA
       (?$(C,l(B . "0x044A") ; CYRILLIC SMALL LETTER HARD SIGN
       (?$(C,m(B . "0x044B") ; CYRILLIC SMALL LETTER YERU
       (?$(C,n(B . "0x044C") ; CYRILLIC SMALL LETTER SOFT SIGN
       (?$(C,o(B . "0x044D") ; CYRILLIC SMALL LETTER E
       (?$(C,p(B . "0x044E") ; CYRILLIC SMALL LETTER YU
       (?$(C,q(B . "0x044F") ; CYRILLIC SMALL LETTER YA
       (?$(C0!(B . "0xAC00") ; HANGUL SYLLABLE KIYEOK-A
       (?$(C0"(B . "0xAC01") ; HANGUL SYLLABLE KIYEOK-A-KIYEOK
       (?$(C0#(B . "0xAC04") ; HANGUL SYLLABLE KIYEOK-A-NIEUN
       (?$(C0$(B . "0xAC07") ; HANGUL SYLLABLE KIYEOK-A-TIKEUT
       (?$(C0%(B . "0xAC08") ; HANGUL SYLLABLE KIYEOK-A-RIEUL
       (?$(C0&(B . "0xAC09") ; HANGUL SYLLABLE KIYEOK-A-RIEULKIYEOK
       (?$(C0'(B . "0xAC0A") ; HANGUL SYLLABLE KIYEOK-A-RIEULMIEUM
       (?$(C0((B . "0xAC10") ; HANGUL SYLLABLE KIYEOK-A-MIEUM
       (?$(C0)(B . "0xAC11") ; HANGUL SYLLABLE KIYEOK-A-PIEUP
       (?$(C0*(B . "0xAC12") ; HANGUL SYLLABLE KIYEOK-A-PIEUPSIOS
       (?$(C0+(B . "0xAC13") ; HANGUL SYLLABLE KIYEOK-A-SIOS
       (?$(C0,(B . "0xAC14") ; HANGUL SYLLABLE KIYEOK-A-SSANGSIOS
       (?$(C0-(B . "0xAC15") ; HANGUL SYLLABLE KIYEOK-A-IEUNG
       (?$(C0.(B . "0xAC16") ; HANGUL SYLLABLE KIYEOK-A-CIEUC
       (?$(C0/(B . "0xAC17") ; HANGUL SYLLABLE KIYEOK-A-CHIEUCH
       (?$(C00(B . "0xAC19") ; HANGUL SYLLABLE KIYEOK-A-THIEUTH
       (?$(C01(B . "0xAC1A") ; HANGUL SYLLABLE KIYEOK-A-PHIEUPH
       (?$(C02(B . "0xAC1B") ; HANGUL SYLLABLE KIYEOK-A-HIEUH
       (?$(C03(B . "0xAC1C") ; HANGUL SYLLABLE KIYEOK-AE
       (?$(C04(B . "0xAC1D") ; HANGUL SYLLABLE KIYEOK-AE-KIYEOK
       (?$(C05(B . "0xAC20") ; HANGUL SYLLABLE KIYEOK-AE-NIEUN
       (?$(C06(B . "0xAC24") ; HANGUL SYLLABLE KIYEOK-AE-RIEUL
       (?$(C07(B . "0xAC2C") ; HANGUL SYLLABLE KIYEOK-AE-MIEUM
       (?$(C08(B . "0xAC2D") ; HANGUL SYLLABLE KIYEOK-AE-PIEUP
       (?$(C09(B . "0xAC2F") ; HANGUL SYLLABLE KIYEOK-AE-SIOS
       (?$(C0:(B . "0xAC30") ; HANGUL SYLLABLE KIYEOK-AE-SSANGSIOS
       (?$(C0;(B . "0xAC31") ; HANGUL SYLLABLE KIYEOK-AE-IEUNG
       (?$(C0<(B . "0xAC38") ; HANGUL SYLLABLE KIYEOK-YA
       (?$(C0=(B . "0xAC39") ; HANGUL SYLLABLE KIYEOK-YA-KIYEOK
       (?$(C0>(B . "0xAC3C") ; HANGUL SYLLABLE KIYEOK-YA-NIEUN
       (?$(C0?(B . "0xAC40") ; HANGUL SYLLABLE KIYEOK-YA-RIEUL
       (?$(C0@(B . "0xAC4B") ; HANGUL SYLLABLE KIYEOK-YA-SIOS
       (?$(C0A(B . "0xAC4D") ; HANGUL SYLLABLE KIYEOK-YA-IEUNG
       (?$(C0B(B . "0xAC54") ; HANGUL SYLLABLE KIYEOK-YAE
       (?$(C0C(B . "0xAC58") ; HANGUL SYLLABLE KIYEOK-YAE-NIEUN
       (?$(C0D(B . "0xAC5C") ; HANGUL SYLLABLE KIYEOK-YAE-RIEUL
       (?$(C0E(B . "0xAC70") ; HANGUL SYLLABLE KIYEOK-EO
       (?$(C0F(B . "0xAC71") ; HANGUL SYLLABLE KIYEOK-EO-KIYEOK
       (?$(C0G(B . "0xAC74") ; HANGUL SYLLABLE KIYEOK-EO-NIEUN
       (?$(C0H(B . "0xAC77") ; HANGUL SYLLABLE KIYEOK-EO-TIKEUT
       (?$(C0I(B . "0xAC78") ; HANGUL SYLLABLE KIYEOK-EO-RIEUL
       (?$(C0J(B . "0xAC7A") ; HANGUL SYLLABLE KIYEOK-EO-RIEULMIEUM
       (?$(C0K(B . "0xAC80") ; HANGUL SYLLABLE KIYEOK-EO-MIEUM
       (?$(C0L(B . "0xAC81") ; HANGUL SYLLABLE KIYEOK-EO-PIEUP
       (?$(C0M(B . "0xAC83") ; HANGUL SYLLABLE KIYEOK-EO-SIOS
       (?$(C0N(B . "0xAC84") ; HANGUL SYLLABLE KIYEOK-EO-SSANGSIOS
       (?$(C0O(B . "0xAC85") ; HANGUL SYLLABLE KIYEOK-EO-IEUNG
       (?$(C0P(B . "0xAC86") ; HANGUL SYLLABLE KIYEOK-EO-CIEUC
       (?$(C0Q(B . "0xAC89") ; HANGUL SYLLABLE KIYEOK-EO-THIEUTH
       (?$(C0R(B . "0xAC8A") ; HANGUL SYLLABLE KIYEOK-EO-PHIEUPH
       (?$(C0S(B . "0xAC8B") ; HANGUL SYLLABLE KIYEOK-EO-HIEUH
       (?$(C0T(B . "0xAC8C") ; HANGUL SYLLABLE KIYEOK-E
       (?$(C0U(B . "0xAC90") ; HANGUL SYLLABLE KIYEOK-E-NIEUN
       (?$(C0V(B . "0xAC94") ; HANGUL SYLLABLE KIYEOK-E-RIEUL
       (?$(C0W(B . "0xAC9C") ; HANGUL SYLLABLE KIYEOK-E-MIEUM
       (?$(C0X(B . "0xAC9D") ; HANGUL SYLLABLE KIYEOK-E-PIEUP
       (?$(C0Y(B . "0xAC9F") ; HANGUL SYLLABLE KIYEOK-E-SIOS
       (?$(C0Z(B . "0xACA0") ; HANGUL SYLLABLE KIYEOK-E-SSANGSIOS
       (?$(C0[(B . "0xACA1") ; HANGUL SYLLABLE KIYEOK-E-IEUNG
       (?$(C0\(B . "0xACA8") ; HANGUL SYLLABLE KIYEOK-YEO
       (?$(C0](B . "0xACA9") ; HANGUL SYLLABLE KIYEOK-YEO-KIYEOK
       (?$(C0^(B . "0xACAA") ; HANGUL SYLLABLE KIYEOK-YEO-SSANGKIYEOK
       (?$(C0_(B . "0xACAC") ; HANGUL SYLLABLE KIYEOK-YEO-NIEUN
       (?$(C0`(B . "0xACAF") ; HANGUL SYLLABLE KIYEOK-YEO-TIKEUT
       (?$(C0a(B . "0xACB0") ; HANGUL SYLLABLE KIYEOK-YEO-RIEUL
       (?$(C0b(B . "0xACB8") ; HANGUL SYLLABLE KIYEOK-YEO-MIEUM
       (?$(C0c(B . "0xACB9") ; HANGUL SYLLABLE KIYEOK-YEO-PIEUP
       (?$(C0d(B . "0xACBB") ; HANGUL SYLLABLE KIYEOK-YEO-SIOS
       (?$(C0e(B . "0xACBC") ; HANGUL SYLLABLE KIYEOK-YEO-SSANGSIOS
       (?$(C0f(B . "0xACBD") ; HANGUL SYLLABLE KIYEOK-YEO-IEUNG
       (?$(C0g(B . "0xACC1") ; HANGUL SYLLABLE KIYEOK-YEO-THIEUTH
       (?$(C0h(B . "0xACC4") ; HANGUL SYLLABLE KIYEOK-YE
       (?$(C0i(B . "0xACC8") ; HANGUL SYLLABLE KIYEOK-YE-NIEUN
       (?$(C0j(B . "0xACCC") ; HANGUL SYLLABLE KIYEOK-YE-RIEUL
       (?$(C0k(B . "0xACD5") ; HANGUL SYLLABLE KIYEOK-YE-PIEUP
       (?$(C0l(B . "0xACD7") ; HANGUL SYLLABLE KIYEOK-YE-SIOS
       (?$(C0m(B . "0xACE0") ; HANGUL SYLLABLE KIYEOK-O
       (?$(C0n(B . "0xACE1") ; HANGUL SYLLABLE KIYEOK-O-KIYEOK
       (?$(C0o(B . "0xACE4") ; HANGUL SYLLABLE KIYEOK-O-NIEUN
       (?$(C0p(B . "0xACE7") ; HANGUL SYLLABLE KIYEOK-O-TIKEUT
       (?$(C0q(B . "0xACE8") ; HANGUL SYLLABLE KIYEOK-O-RIEUL
       (?$(C0r(B . "0xACEA") ; HANGUL SYLLABLE KIYEOK-O-RIEULMIEUM
       (?$(C0s(B . "0xACEC") ; HANGUL SYLLABLE KIYEOK-O-RIEULSIOS
       (?$(C0t(B . "0xACEF") ; HANGUL SYLLABLE KIYEOK-O-RIEULHIEUH
       (?$(C0u(B . "0xACF0") ; HANGUL SYLLABLE KIYEOK-O-MIEUM
       (?$(C0v(B . "0xACF1") ; HANGUL SYLLABLE KIYEOK-O-PIEUP
       (?$(C0w(B . "0xACF3") ; HANGUL SYLLABLE KIYEOK-O-SIOS
       (?$(C0x(B . "0xACF5") ; HANGUL SYLLABLE KIYEOK-O-IEUNG
       (?$(C0y(B . "0xACF6") ; HANGUL SYLLABLE KIYEOK-O-CIEUC
       (?$(C0z(B . "0xACFC") ; HANGUL SYLLABLE KIYEOK-WA
       (?$(C0{(B . "0xACFD") ; HANGUL SYLLABLE KIYEOK-WA-KIYEOK
       (?$(C0|(B . "0xAD00") ; HANGUL SYLLABLE KIYEOK-WA-NIEUN
       (?$(C0}(B . "0xAD04") ; HANGUL SYLLABLE KIYEOK-WA-RIEUL
       (?$(C0~(B . "0xAD06") ; HANGUL SYLLABLE KIYEOK-WA-RIEULMIEUM
       (?$(C1!(B . "0xAD0C") ; HANGUL SYLLABLE KIYEOK-WA-MIEUM
       (?$(C1"(B . "0xAD0D") ; HANGUL SYLLABLE KIYEOK-WA-PIEUP
       (?$(C1#(B . "0xAD0F") ; HANGUL SYLLABLE KIYEOK-WA-SIOS
       (?$(C1$(B . "0xAD11") ; HANGUL SYLLABLE KIYEOK-WA-IEUNG
       (?$(C1%(B . "0xAD18") ; HANGUL SYLLABLE KIYEOK-WAE
       (?$(C1&(B . "0xAD1C") ; HANGUL SYLLABLE KIYEOK-WAE-NIEUN
       (?$(C1'(B . "0xAD20") ; HANGUL SYLLABLE KIYEOK-WAE-RIEUL
       (?$(C1((B . "0xAD29") ; HANGUL SYLLABLE KIYEOK-WAE-PIEUP
       (?$(C1)(B . "0xAD2C") ; HANGUL SYLLABLE KIYEOK-WAE-SSANGSIOS
       (?$(C1*(B . "0xAD2D") ; HANGUL SYLLABLE KIYEOK-WAE-IEUNG
       (?$(C1+(B . "0xAD34") ; HANGUL SYLLABLE KIYEOK-OE
       (?$(C1,(B . "0xAD35") ; HANGUL SYLLABLE KIYEOK-OE-KIYEOK
       (?$(C1-(B . "0xAD38") ; HANGUL SYLLABLE KIYEOK-OE-NIEUN
       (?$(C1.(B . "0xAD3C") ; HANGUL SYLLABLE KIYEOK-OE-RIEUL
       (?$(C1/(B . "0xAD44") ; HANGUL SYLLABLE KIYEOK-OE-MIEUM
       (?$(C10(B . "0xAD45") ; HANGUL SYLLABLE KIYEOK-OE-PIEUP
       (?$(C11(B . "0xAD47") ; HANGUL SYLLABLE KIYEOK-OE-SIOS
       (?$(C12(B . "0xAD49") ; HANGUL SYLLABLE KIYEOK-OE-IEUNG
       (?$(C13(B . "0xAD50") ; HANGUL SYLLABLE KIYEOK-YO
       (?$(C14(B . "0xAD54") ; HANGUL SYLLABLE KIYEOK-YO-NIEUN
       (?$(C15(B . "0xAD58") ; HANGUL SYLLABLE KIYEOK-YO-RIEUL
       (?$(C16(B . "0xAD61") ; HANGUL SYLLABLE KIYEOK-YO-PIEUP
       (?$(C17(B . "0xAD63") ; HANGUL SYLLABLE KIYEOK-YO-SIOS
       (?$(C18(B . "0xAD6C") ; HANGUL SYLLABLE KIYEOK-U
       (?$(C19(B . "0xAD6D") ; HANGUL SYLLABLE KIYEOK-U-KIYEOK
       (?$(C1:(B . "0xAD70") ; HANGUL SYLLABLE KIYEOK-U-NIEUN
       (?$(C1;(B . "0xAD73") ; HANGUL SYLLABLE KIYEOK-U-TIKEUT
       (?$(C1<(B . "0xAD74") ; HANGUL SYLLABLE KIYEOK-U-RIEUL
       (?$(C1=(B . "0xAD75") ; HANGUL SYLLABLE KIYEOK-U-RIEULKIYEOK
       (?$(C1>(B . "0xAD76") ; HANGUL SYLLABLE KIYEOK-U-RIEULMIEUM
       (?$(C1?(B . "0xAD7B") ; HANGUL SYLLABLE KIYEOK-U-RIEULHIEUH
       (?$(C1@(B . "0xAD7C") ; HANGUL SYLLABLE KIYEOK-U-MIEUM
       (?$(C1A(B . "0xAD7D") ; HANGUL SYLLABLE KIYEOK-U-PIEUP
       (?$(C1B(B . "0xAD7F") ; HANGUL SYLLABLE KIYEOK-U-SIOS
       (?$(C1C(B . "0xAD81") ; HANGUL SYLLABLE KIYEOK-U-IEUNG
       (?$(C1D(B . "0xAD82") ; HANGUL SYLLABLE KIYEOK-U-CIEUC
       (?$(C1E(B . "0xAD88") ; HANGUL SYLLABLE KIYEOK-WEO
       (?$(C1F(B . "0xAD89") ; HANGUL SYLLABLE KIYEOK-WEO-KIYEOK
       (?$(C1G(B . "0xAD8C") ; HANGUL SYLLABLE KIYEOK-WEO-NIEUN
       (?$(C1H(B . "0xAD90") ; HANGUL SYLLABLE KIYEOK-WEO-RIEUL
       (?$(C1I(B . "0xAD9C") ; HANGUL SYLLABLE KIYEOK-WEO-SSANGSIOS
       (?$(C1J(B . "0xAD9D") ; HANGUL SYLLABLE KIYEOK-WEO-IEUNG
       (?$(C1K(B . "0xADA4") ; HANGUL SYLLABLE KIYEOK-WE
       (?$(C1L(B . "0xADB7") ; HANGUL SYLLABLE KIYEOK-WE-SIOS
       (?$(C1M(B . "0xADC0") ; HANGUL SYLLABLE KIYEOK-WI
       (?$(C1N(B . "0xADC1") ; HANGUL SYLLABLE KIYEOK-WI-KIYEOK
       (?$(C1O(B . "0xADC4") ; HANGUL SYLLABLE KIYEOK-WI-NIEUN
       (?$(C1P(B . "0xADC8") ; HANGUL SYLLABLE KIYEOK-WI-RIEUL
       (?$(C1Q(B . "0xADD0") ; HANGUL SYLLABLE KIYEOK-WI-MIEUM
       (?$(C1R(B . "0xADD1") ; HANGUL SYLLABLE KIYEOK-WI-PIEUP
       (?$(C1S(B . "0xADD3") ; HANGUL SYLLABLE KIYEOK-WI-SIOS
       (?$(C1T(B . "0xADDC") ; HANGUL SYLLABLE KIYEOK-YU
       (?$(C1U(B . "0xADE0") ; HANGUL SYLLABLE KIYEOK-YU-NIEUN
       (?$(C1V(B . "0xADE4") ; HANGUL SYLLABLE KIYEOK-YU-RIEUL
       (?$(C1W(B . "0xADF8") ; HANGUL SYLLABLE KIYEOK-EU
       (?$(C1X(B . "0xADF9") ; HANGUL SYLLABLE KIYEOK-EU-KIYEOK
       (?$(C1Y(B . "0xADFC") ; HANGUL SYLLABLE KIYEOK-EU-NIEUN
       (?$(C1Z(B . "0xADFF") ; HANGUL SYLLABLE KIYEOK-EU-TIKEUT
       (?$(C1[(B . "0xAE00") ; HANGUL SYLLABLE KIYEOK-EU-RIEUL
       (?$(C1\(B . "0xAE01") ; HANGUL SYLLABLE KIYEOK-EU-RIEULKIYEOK
       (?$(C1](B . "0xAE08") ; HANGUL SYLLABLE KIYEOK-EU-MIEUM
       (?$(C1^(B . "0xAE09") ; HANGUL SYLLABLE KIYEOK-EU-PIEUP
       (?$(C1_(B . "0xAE0B") ; HANGUL SYLLABLE KIYEOK-EU-SIOS
       (?$(C1`(B . "0xAE0D") ; HANGUL SYLLABLE KIYEOK-EU-IEUNG
       (?$(C1a(B . "0xAE14") ; HANGUL SYLLABLE KIYEOK-YI
       (?$(C1b(B . "0xAE30") ; HANGUL SYLLABLE KIYEOK-I
       (?$(C1c(B . "0xAE31") ; HANGUL SYLLABLE KIYEOK-I-KIYEOK
       (?$(C1d(B . "0xAE34") ; HANGUL SYLLABLE KIYEOK-I-NIEUN
       (?$(C1e(B . "0xAE37") ; HANGUL SYLLABLE KIYEOK-I-TIKEUT
       (?$(C1f(B . "0xAE38") ; HANGUL SYLLABLE KIYEOK-I-RIEUL
       (?$(C1g(B . "0xAE3A") ; HANGUL SYLLABLE KIYEOK-I-RIEULMIEUM
       (?$(C1h(B . "0xAE40") ; HANGUL SYLLABLE KIYEOK-I-MIEUM
       (?$(C1i(B . "0xAE41") ; HANGUL SYLLABLE KIYEOK-I-PIEUP
       (?$(C1j(B . "0xAE43") ; HANGUL SYLLABLE KIYEOK-I-SIOS
       (?$(C1k(B . "0xAE45") ; HANGUL SYLLABLE KIYEOK-I-IEUNG
       (?$(C1l(B . "0xAE46") ; HANGUL SYLLABLE KIYEOK-I-CIEUC
       (?$(C1m(B . "0xAE4A") ; HANGUL SYLLABLE KIYEOK-I-PHIEUPH
       (?$(C1n(B . "0xAE4C") ; HANGUL SYLLABLE SSANGKIYEOK-A
       (?$(C1o(B . "0xAE4D") ; HANGUL SYLLABLE SSANGKIYEOK-A-KIYEOK
       (?$(C1p(B . "0xAE4E") ; HANGUL SYLLABLE SSANGKIYEOK-A-SSANGKIYEOK
       (?$(C1q(B . "0xAE50") ; HANGUL SYLLABLE SSANGKIYEOK-A-NIEUN
       (?$(C1r(B . "0xAE54") ; HANGUL SYLLABLE SSANGKIYEOK-A-RIEUL
       (?$(C1s(B . "0xAE56") ; HANGUL SYLLABLE SSANGKIYEOK-A-RIEULMIEUM
       (?$(C1t(B . "0xAE5C") ; HANGUL SYLLABLE SSANGKIYEOK-A-MIEUM
       (?$(C1u(B . "0xAE5D") ; HANGUL SYLLABLE SSANGKIYEOK-A-PIEUP
       (?$(C1v(B . "0xAE5F") ; HANGUL SYLLABLE SSANGKIYEOK-A-SIOS
       (?$(C1w(B . "0xAE60") ; HANGUL SYLLABLE SSANGKIYEOK-A-SSANGSIOS
       (?$(C1x(B . "0xAE61") ; HANGUL SYLLABLE SSANGKIYEOK-A-IEUNG
       (?$(C1y(B . "0xAE65") ; HANGUL SYLLABLE SSANGKIYEOK-A-THIEUTH
       (?$(C1z(B . "0xAE68") ; HANGUL SYLLABLE SSANGKIYEOK-AE
       (?$(C1{(B . "0xAE69") ; HANGUL SYLLABLE SSANGKIYEOK-AE-KIYEOK
       (?$(C1|(B . "0xAE6C") ; HANGUL SYLLABLE SSANGKIYEOK-AE-NIEUN
       (?$(C1}(B . "0xAE70") ; HANGUL SYLLABLE SSANGKIYEOK-AE-RIEUL
       (?$(C1~(B . "0xAE78") ; HANGUL SYLLABLE SSANGKIYEOK-AE-MIEUM
       (?$(C2!(B . "0xAE79") ; HANGUL SYLLABLE SSANGKIYEOK-AE-PIEUP
       (?$(C2"(B . "0xAE7B") ; HANGUL SYLLABLE SSANGKIYEOK-AE-SIOS
       (?$(C2#(B . "0xAE7C") ; HANGUL SYLLABLE SSANGKIYEOK-AE-SSANGSIOS
       (?$(C2$(B . "0xAE7D") ; HANGUL SYLLABLE SSANGKIYEOK-AE-IEUNG
       (?$(C2%(B . "0xAE84") ; HANGUL SYLLABLE SSANGKIYEOK-YA
       (?$(C2&(B . "0xAE85") ; HANGUL SYLLABLE SSANGKIYEOK-YA-KIYEOK
       (?$(C2'(B . "0xAE8C") ; HANGUL SYLLABLE SSANGKIYEOK-YA-RIEUL
       (?$(C2((B . "0xAEBC") ; HANGUL SYLLABLE SSANGKIYEOK-EO
       (?$(C2)(B . "0xAEBD") ; HANGUL SYLLABLE SSANGKIYEOK-EO-KIYEOK
       (?$(C2*(B . "0xAEBE") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SSANGKIYEOK
       (?$(C2+(B . "0xAEC0") ; HANGUL SYLLABLE SSANGKIYEOK-EO-NIEUN
       (?$(C2,(B . "0xAEC4") ; HANGUL SYLLABLE SSANGKIYEOK-EO-RIEUL
       (?$(C2-(B . "0xAECC") ; HANGUL SYLLABLE SSANGKIYEOK-EO-MIEUM
       (?$(C2.(B . "0xAECD") ; HANGUL SYLLABLE SSANGKIYEOK-EO-PIEUP
       (?$(C2/(B . "0xAECF") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SIOS
       (?$(C20(B . "0xAED0") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SSANGSIOS
       (?$(C21(B . "0xAED1") ; HANGUL SYLLABLE SSANGKIYEOK-EO-IEUNG
       (?$(C22(B . "0xAED8") ; HANGUL SYLLABLE SSANGKIYEOK-E
       (?$(C23(B . "0xAED9") ; HANGUL SYLLABLE SSANGKIYEOK-E-KIYEOK
       (?$(C24(B . "0xAEDC") ; HANGUL SYLLABLE SSANGKIYEOK-E-NIEUN
       (?$(C25(B . "0xAEE8") ; HANGUL SYLLABLE SSANGKIYEOK-E-MIEUM
       (?$(C26(B . "0xAEEB") ; HANGUL SYLLABLE SSANGKIYEOK-E-SIOS
       (?$(C27(B . "0xAEED") ; HANGUL SYLLABLE SSANGKIYEOK-E-IEUNG
       (?$(C28(B . "0xAEF4") ; HANGUL SYLLABLE SSANGKIYEOK-YEO
       (?$(C29(B . "0xAEF8") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-NIEUN
       (?$(C2:(B . "0xAEFC") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-RIEUL
       (?$(C2;(B . "0xAF07") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-SIOS
       (?$(C2<(B . "0xAF08") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-SSANGSIOS
       (?$(C2=(B . "0xAF0D") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-THIEUTH
       (?$(C2>(B . "0xAF10") ; HANGUL SYLLABLE SSANGKIYEOK-YE
       (?$(C2?(B . "0xAF2C") ; HANGUL SYLLABLE SSANGKIYEOK-O
       (?$(C2@(B . "0xAF2D") ; HANGUL SYLLABLE SSANGKIYEOK-O-KIYEOK
       (?$(C2A(B . "0xAF30") ; HANGUL SYLLABLE SSANGKIYEOK-O-NIEUN
       (?$(C2B(B . "0xAF32") ; HANGUL SYLLABLE SSANGKIYEOK-O-NIEUNHIEUH
       (?$(C2C(B . "0xAF34") ; HANGUL SYLLABLE SSANGKIYEOK-O-RIEUL
       (?$(C2D(B . "0xAF3C") ; HANGUL SYLLABLE SSANGKIYEOK-O-MIEUM
       (?$(C2E(B . "0xAF3D") ; HANGUL SYLLABLE SSANGKIYEOK-O-PIEUP
       (?$(C2F(B . "0xAF3F") ; HANGUL SYLLABLE SSANGKIYEOK-O-SIOS
       (?$(C2G(B . "0xAF41") ; HANGUL SYLLABLE SSANGKIYEOK-O-IEUNG
       (?$(C2H(B . "0xAF42") ; HANGUL SYLLABLE SSANGKIYEOK-O-CIEUC
       (?$(C2I(B . "0xAF43") ; HANGUL SYLLABLE SSANGKIYEOK-O-CHIEUCH
       (?$(C2J(B . "0xAF48") ; HANGUL SYLLABLE SSANGKIYEOK-WA
       (?$(C2K(B . "0xAF49") ; HANGUL SYLLABLE SSANGKIYEOK-WA-KIYEOK
       (?$(C2L(B . "0xAF50") ; HANGUL SYLLABLE SSANGKIYEOK-WA-RIEUL
       (?$(C2M(B . "0xAF5C") ; HANGUL SYLLABLE SSANGKIYEOK-WA-SSANGSIOS
       (?$(C2N(B . "0xAF5D") ; HANGUL SYLLABLE SSANGKIYEOK-WA-IEUNG
       (?$(C2O(B . "0xAF64") ; HANGUL SYLLABLE SSANGKIYEOK-WAE
       (?$(C2P(B . "0xAF65") ; HANGUL SYLLABLE SSANGKIYEOK-WAE-KIYEOK
       (?$(C2Q(B . "0xAF79") ; HANGUL SYLLABLE SSANGKIYEOK-WAE-IEUNG
       (?$(C2R(B . "0xAF80") ; HANGUL SYLLABLE SSANGKIYEOK-OE
       (?$(C2S(B . "0xAF84") ; HANGUL SYLLABLE SSANGKIYEOK-OE-NIEUN
       (?$(C2T(B . "0xAF88") ; HANGUL SYLLABLE SSANGKIYEOK-OE-RIEUL
       (?$(C2U(B . "0xAF90") ; HANGUL SYLLABLE SSANGKIYEOK-OE-MIEUM
       (?$(C2V(B . "0xAF91") ; HANGUL SYLLABLE SSANGKIYEOK-OE-PIEUP
       (?$(C2W(B . "0xAF95") ; HANGUL SYLLABLE SSANGKIYEOK-OE-IEUNG
       (?$(C2X(B . "0xAF9C") ; HANGUL SYLLABLE SSANGKIYEOK-YO
       (?$(C2Y(B . "0xAFB8") ; HANGUL SYLLABLE SSANGKIYEOK-U
       (?$(C2Z(B . "0xAFB9") ; HANGUL SYLLABLE SSANGKIYEOK-U-KIYEOK
       (?$(C2[(B . "0xAFBC") ; HANGUL SYLLABLE SSANGKIYEOK-U-NIEUN
       (?$(C2\(B . "0xAFC0") ; HANGUL SYLLABLE SSANGKIYEOK-U-RIEUL
       (?$(C2](B . "0xAFC7") ; HANGUL SYLLABLE SSANGKIYEOK-U-RIEULHIEUH
       (?$(C2^(B . "0xAFC8") ; HANGUL SYLLABLE SSANGKIYEOK-U-MIEUM
       (?$(C2_(B . "0xAFC9") ; HANGUL SYLLABLE SSANGKIYEOK-U-PIEUP
       (?$(C2`(B . "0xAFCB") ; HANGUL SYLLABLE SSANGKIYEOK-U-SIOS
       (?$(C2a(B . "0xAFCD") ; HANGUL SYLLABLE SSANGKIYEOK-U-IEUNG
       (?$(C2b(B . "0xAFCE") ; HANGUL SYLLABLE SSANGKIYEOK-U-CIEUC
       (?$(C2c(B . "0xAFD4") ; HANGUL SYLLABLE SSANGKIYEOK-WEO
       (?$(C2d(B . "0xAFDC") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-RIEUL
       (?$(C2e(B . "0xAFE8") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-SSANGSIOS
       (?$(C2f(B . "0xAFE9") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-IEUNG
       (?$(C2g(B . "0xAFF0") ; HANGUL SYLLABLE SSANGKIYEOK-WE
       (?$(C2h(B . "0xAFF1") ; HANGUL SYLLABLE SSANGKIYEOK-WE-KIYEOK
       (?$(C2i(B . "0xAFF4") ; HANGUL SYLLABLE SSANGKIYEOK-WE-NIEUN
       (?$(C2j(B . "0xAFF8") ; HANGUL SYLLABLE SSANGKIYEOK-WE-RIEUL
       (?$(C2k(B . "0xB000") ; HANGUL SYLLABLE SSANGKIYEOK-WE-MIEUM
       (?$(C2l(B . "0xB001") ; HANGUL SYLLABLE SSANGKIYEOK-WE-PIEUP
       (?$(C2m(B . "0xB004") ; HANGUL SYLLABLE SSANGKIYEOK-WE-SSANGSIOS
       (?$(C2n(B . "0xB00C") ; HANGUL SYLLABLE SSANGKIYEOK-WI
       (?$(C2o(B . "0xB010") ; HANGUL SYLLABLE SSANGKIYEOK-WI-NIEUN
       (?$(C2p(B . "0xB014") ; HANGUL SYLLABLE SSANGKIYEOK-WI-RIEUL
       (?$(C2q(B . "0xB01C") ; HANGUL SYLLABLE SSANGKIYEOK-WI-MIEUM
       (?$(C2r(B . "0xB01D") ; HANGUL SYLLABLE SSANGKIYEOK-WI-PIEUP
       (?$(C2s(B . "0xB028") ; HANGUL SYLLABLE SSANGKIYEOK-YU
       (?$(C2t(B . "0xB044") ; HANGUL SYLLABLE SSANGKIYEOK-EU
       (?$(C2u(B . "0xB045") ; HANGUL SYLLABLE SSANGKIYEOK-EU-KIYEOK
       (?$(C2v(B . "0xB048") ; HANGUL SYLLABLE SSANGKIYEOK-EU-NIEUN
       (?$(C2w(B . "0xB04A") ; HANGUL SYLLABLE SSANGKIYEOK-EU-NIEUNHIEUH
       (?$(C2x(B . "0xB04C") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEUL
       (?$(C2y(B . "0xB04E") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEULMIEUM
       (?$(C2z(B . "0xB053") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEULHIEUH
       (?$(C2{(B . "0xB054") ; HANGUL SYLLABLE SSANGKIYEOK-EU-MIEUM
       (?$(C2|(B . "0xB055") ; HANGUL SYLLABLE SSANGKIYEOK-EU-PIEUP
       (?$(C2}(B . "0xB057") ; HANGUL SYLLABLE SSANGKIYEOK-EU-SIOS
       (?$(C2~(B . "0xB059") ; HANGUL SYLLABLE SSANGKIYEOK-EU-IEUNG
       (?$(C3!(B . "0xB05D") ; HANGUL SYLLABLE SSANGKIYEOK-EU-THIEUTH
       (?$(C3"(B . "0xB07C") ; HANGUL SYLLABLE SSANGKIYEOK-I
       (?$(C3#(B . "0xB07D") ; HANGUL SYLLABLE SSANGKIYEOK-I-KIYEOK
       (?$(C3$(B . "0xB080") ; HANGUL SYLLABLE SSANGKIYEOK-I-NIEUN
       (?$(C3%(B . "0xB084") ; HANGUL SYLLABLE SSANGKIYEOK-I-RIEUL
       (?$(C3&(B . "0xB08C") ; HANGUL SYLLABLE SSANGKIYEOK-I-MIEUM
       (?$(C3'(B . "0xB08D") ; HANGUL SYLLABLE SSANGKIYEOK-I-PIEUP
       (?$(C3((B . "0xB08F") ; HANGUL SYLLABLE SSANGKIYEOK-I-SIOS
       (?$(C3)(B . "0xB091") ; HANGUL SYLLABLE SSANGKIYEOK-I-IEUNG
       (?$(C3*(B . "0xB098") ; HANGUL SYLLABLE NIEUN-A
       (?$(C3+(B . "0xB099") ; HANGUL SYLLABLE NIEUN-A-KIYEOK
       (?$(C3,(B . "0xB09A") ; HANGUL SYLLABLE NIEUN-A-SSANGKIYEOK
       (?$(C3-(B . "0xB09C") ; HANGUL SYLLABLE NIEUN-A-NIEUN
       (?$(C3.(B . "0xB09F") ; HANGUL SYLLABLE NIEUN-A-TIKEUT
       (?$(C3/(B . "0xB0A0") ; HANGUL SYLLABLE NIEUN-A-RIEUL
       (?$(C30(B . "0xB0A1") ; HANGUL SYLLABLE NIEUN-A-RIEULKIYEOK
       (?$(C31(B . "0xB0A2") ; HANGUL SYLLABLE NIEUN-A-RIEULMIEUM
       (?$(C32(B . "0xB0A8") ; HANGUL SYLLABLE NIEUN-A-MIEUM
       (?$(C33(B . "0xB0A9") ; HANGUL SYLLABLE NIEUN-A-PIEUP
       (?$(C34(B . "0xB0AB") ; HANGUL SYLLABLE NIEUN-A-SIOS
       (?$(C35(B . "0xB0AC") ; HANGUL SYLLABLE NIEUN-A-SSANGSIOS
       (?$(C36(B . "0xB0AD") ; HANGUL SYLLABLE NIEUN-A-IEUNG
       (?$(C37(B . "0xB0AE") ; HANGUL SYLLABLE NIEUN-A-CIEUC
       (?$(C38(B . "0xB0AF") ; HANGUL SYLLABLE NIEUN-A-CHIEUCH
       (?$(C39(B . "0xB0B1") ; HANGUL SYLLABLE NIEUN-A-THIEUTH
       (?$(C3:(B . "0xB0B3") ; HANGUL SYLLABLE NIEUN-A-HIEUH
       (?$(C3;(B . "0xB0B4") ; HANGUL SYLLABLE NIEUN-AE
       (?$(C3<(B . "0xB0B5") ; HANGUL SYLLABLE NIEUN-AE-KIYEOK
       (?$(C3=(B . "0xB0B8") ; HANGUL SYLLABLE NIEUN-AE-NIEUN
       (?$(C3>(B . "0xB0BC") ; HANGUL SYLLABLE NIEUN-AE-RIEUL
       (?$(C3?(B . "0xB0C4") ; HANGUL SYLLABLE NIEUN-AE-MIEUM
       (?$(C3@(B . "0xB0C5") ; HANGUL SYLLABLE NIEUN-AE-PIEUP
       (?$(C3A(B . "0xB0C7") ; HANGUL SYLLABLE NIEUN-AE-SIOS
       (?$(C3B(B . "0xB0C8") ; HANGUL SYLLABLE NIEUN-AE-SSANGSIOS
       (?$(C3C(B . "0xB0C9") ; HANGUL SYLLABLE NIEUN-AE-IEUNG
       (?$(C3D(B . "0xB0D0") ; HANGUL SYLLABLE NIEUN-YA
       (?$(C3E(B . "0xB0D1") ; HANGUL SYLLABLE NIEUN-YA-KIYEOK
       (?$(C3F(B . "0xB0D4") ; HANGUL SYLLABLE NIEUN-YA-NIEUN
       (?$(C3G(B . "0xB0D8") ; HANGUL SYLLABLE NIEUN-YA-RIEUL
       (?$(C3H(B . "0xB0E0") ; HANGUL SYLLABLE NIEUN-YA-MIEUM
       (?$(C3I(B . "0xB0E5") ; HANGUL SYLLABLE NIEUN-YA-IEUNG
       (?$(C3J(B . "0xB108") ; HANGUL SYLLABLE NIEUN-EO
       (?$(C3K(B . "0xB109") ; HANGUL SYLLABLE NIEUN-EO-KIYEOK
       (?$(C3L(B . "0xB10B") ; HANGUL SYLLABLE NIEUN-EO-KIYEOKSIOS
       (?$(C3M(B . "0xB10C") ; HANGUL SYLLABLE NIEUN-EO-NIEUN
       (?$(C3N(B . "0xB110") ; HANGUL SYLLABLE NIEUN-EO-RIEUL
       (?$(C3O(B . "0xB112") ; HANGUL SYLLABLE NIEUN-EO-RIEULMIEUM
       (?$(C3P(B . "0xB113") ; HANGUL SYLLABLE NIEUN-EO-RIEULPIEUP
       (?$(C3Q(B . "0xB118") ; HANGUL SYLLABLE NIEUN-EO-MIEUM
       (?$(C3R(B . "0xB119") ; HANGUL SYLLABLE NIEUN-EO-PIEUP
       (?$(C3S(B . "0xB11B") ; HANGUL SYLLABLE NIEUN-EO-SIOS
       (?$(C3T(B . "0xB11C") ; HANGUL SYLLABLE NIEUN-EO-SSANGSIOS
       (?$(C3U(B . "0xB11D") ; HANGUL SYLLABLE NIEUN-EO-IEUNG
       (?$(C3V(B . "0xB123") ; HANGUL SYLLABLE NIEUN-EO-HIEUH
       (?$(C3W(B . "0xB124") ; HANGUL SYLLABLE NIEUN-E
       (?$(C3X(B . "0xB125") ; HANGUL SYLLABLE NIEUN-E-KIYEOK
       (?$(C3Y(B . "0xB128") ; HANGUL SYLLABLE NIEUN-E-NIEUN
       (?$(C3Z(B . "0xB12C") ; HANGUL SYLLABLE NIEUN-E-RIEUL
       (?$(C3[(B . "0xB134") ; HANGUL SYLLABLE NIEUN-E-MIEUM
       (?$(C3\(B . "0xB135") ; HANGUL SYLLABLE NIEUN-E-PIEUP
       (?$(C3](B . "0xB137") ; HANGUL SYLLABLE NIEUN-E-SIOS
       (?$(C3^(B . "0xB138") ; HANGUL SYLLABLE NIEUN-E-SSANGSIOS
       (?$(C3_(B . "0xB139") ; HANGUL SYLLABLE NIEUN-E-IEUNG
       (?$(C3`(B . "0xB140") ; HANGUL SYLLABLE NIEUN-YEO
       (?$(C3a(B . "0xB141") ; HANGUL SYLLABLE NIEUN-YEO-KIYEOK
       (?$(C3b(B . "0xB144") ; HANGUL SYLLABLE NIEUN-YEO-NIEUN
       (?$(C3c(B . "0xB148") ; HANGUL SYLLABLE NIEUN-YEO-RIEUL
       (?$(C3d(B . "0xB150") ; HANGUL SYLLABLE NIEUN-YEO-MIEUM
       (?$(C3e(B . "0xB151") ; HANGUL SYLLABLE NIEUN-YEO-PIEUP
       (?$(C3f(B . "0xB154") ; HANGUL SYLLABLE NIEUN-YEO-SSANGSIOS
       (?$(C3g(B . "0xB155") ; HANGUL SYLLABLE NIEUN-YEO-IEUNG
       (?$(C3h(B . "0xB158") ; HANGUL SYLLABLE NIEUN-YEO-KHIEUKH
       (?$(C3i(B . "0xB15C") ; HANGUL SYLLABLE NIEUN-YE
       (?$(C3j(B . "0xB160") ; HANGUL SYLLABLE NIEUN-YE-NIEUN
       (?$(C3k(B . "0xB178") ; HANGUL SYLLABLE NIEUN-O
       (?$(C3l(B . "0xB179") ; HANGUL SYLLABLE NIEUN-O-KIYEOK
       (?$(C3m(B . "0xB17C") ; HANGUL SYLLABLE NIEUN-O-NIEUN
       (?$(C3n(B . "0xB180") ; HANGUL SYLLABLE NIEUN-O-RIEUL
       (?$(C3o(B . "0xB182") ; HANGUL SYLLABLE NIEUN-O-RIEULMIEUM
       (?$(C3p(B . "0xB188") ; HANGUL SYLLABLE NIEUN-O-MIEUM
       (?$(C3q(B . "0xB189") ; HANGUL SYLLABLE NIEUN-O-PIEUP
       (?$(C3r(B . "0xB18B") ; HANGUL SYLLABLE NIEUN-O-SIOS
       (?$(C3s(B . "0xB18D") ; HANGUL SYLLABLE NIEUN-O-IEUNG
       (?$(C3t(B . "0xB192") ; HANGUL SYLLABLE NIEUN-O-PHIEUPH
       (?$(C3u(B . "0xB193") ; HANGUL SYLLABLE NIEUN-O-HIEUH
       (?$(C3v(B . "0xB194") ; HANGUL SYLLABLE NIEUN-WA
       (?$(C3w(B . "0xB198") ; HANGUL SYLLABLE NIEUN-WA-NIEUN
       (?$(C3x(B . "0xB19C") ; HANGUL SYLLABLE NIEUN-WA-RIEUL
       (?$(C3y(B . "0xB1A8") ; HANGUL SYLLABLE NIEUN-WA-SSANGSIOS
       (?$(C3z(B . "0xB1CC") ; HANGUL SYLLABLE NIEUN-OE
       (?$(C3{(B . "0xB1D0") ; HANGUL SYLLABLE NIEUN-OE-NIEUN
       (?$(C3|(B . "0xB1D4") ; HANGUL SYLLABLE NIEUN-OE-RIEUL
       (?$(C3}(B . "0xB1DC") ; HANGUL SYLLABLE NIEUN-OE-MIEUM
       (?$(C3~(B . "0xB1DD") ; HANGUL SYLLABLE NIEUN-OE-PIEUP
       (?$(C4!(B . "0xB1DF") ; HANGUL SYLLABLE NIEUN-OE-SIOS
       (?$(C4"(B . "0xB1E8") ; HANGUL SYLLABLE NIEUN-YO
       (?$(C4#(B . "0xB1E9") ; HANGUL SYLLABLE NIEUN-YO-KIYEOK
       (?$(C4$(B . "0xB1EC") ; HANGUL SYLLABLE NIEUN-YO-NIEUN
       (?$(C4%(B . "0xB1F0") ; HANGUL SYLLABLE NIEUN-YO-RIEUL
       (?$(C4&(B . "0xB1F9") ; HANGUL SYLLABLE NIEUN-YO-PIEUP
       (?$(C4'(B . "0xB1FB") ; HANGUL SYLLABLE NIEUN-YO-SIOS
       (?$(C4((B . "0xB1FD") ; HANGUL SYLLABLE NIEUN-YO-IEUNG
       (?$(C4)(B . "0xB204") ; HANGUL SYLLABLE NIEUN-U
       (?$(C4*(B . "0xB205") ; HANGUL SYLLABLE NIEUN-U-KIYEOK
       (?$(C4+(B . "0xB208") ; HANGUL SYLLABLE NIEUN-U-NIEUN
       (?$(C4,(B . "0xB20B") ; HANGUL SYLLABLE NIEUN-U-TIKEUT
       (?$(C4-(B . "0xB20C") ; HANGUL SYLLABLE NIEUN-U-RIEUL
       (?$(C4.(B . "0xB214") ; HANGUL SYLLABLE NIEUN-U-MIEUM
       (?$(C4/(B . "0xB215") ; HANGUL SYLLABLE NIEUN-U-PIEUP
       (?$(C40(B . "0xB217") ; HANGUL SYLLABLE NIEUN-U-SIOS
       (?$(C41(B . "0xB219") ; HANGUL SYLLABLE NIEUN-U-IEUNG
       (?$(C42(B . "0xB220") ; HANGUL SYLLABLE NIEUN-WEO
       (?$(C43(B . "0xB234") ; HANGUL SYLLABLE NIEUN-WEO-SSANGSIOS
       (?$(C44(B . "0xB23C") ; HANGUL SYLLABLE NIEUN-WE
       (?$(C45(B . "0xB258") ; HANGUL SYLLABLE NIEUN-WI
       (?$(C46(B . "0xB25C") ; HANGUL SYLLABLE NIEUN-WI-NIEUN
       (?$(C47(B . "0xB260") ; HANGUL SYLLABLE NIEUN-WI-RIEUL
       (?$(C48(B . "0xB268") ; HANGUL SYLLABLE NIEUN-WI-MIEUM
       (?$(C49(B . "0xB269") ; HANGUL SYLLABLE NIEUN-WI-PIEUP
       (?$(C4:(B . "0xB274") ; HANGUL SYLLABLE NIEUN-YU
       (?$(C4;(B . "0xB275") ; HANGUL SYLLABLE NIEUN-YU-KIYEOK
       (?$(C4<(B . "0xB27C") ; HANGUL SYLLABLE NIEUN-YU-RIEUL
       (?$(C4=(B . "0xB284") ; HANGUL SYLLABLE NIEUN-YU-MIEUM
       (?$(C4>(B . "0xB285") ; HANGUL SYLLABLE NIEUN-YU-PIEUP
       (?$(C4?(B . "0xB289") ; HANGUL SYLLABLE NIEUN-YU-IEUNG
       (?$(C4@(B . "0xB290") ; HANGUL SYLLABLE NIEUN-EU
       (?$(C4A(B . "0xB291") ; HANGUL SYLLABLE NIEUN-EU-KIYEOK
       (?$(C4B(B . "0xB294") ; HANGUL SYLLABLE NIEUN-EU-NIEUN
       (?$(C4C(B . "0xB298") ; HANGUL SYLLABLE NIEUN-EU-RIEUL
       (?$(C4D(B . "0xB299") ; HANGUL SYLLABLE NIEUN-EU-RIEULKIYEOK
       (?$(C4E(B . "0xB29A") ; HANGUL SYLLABLE NIEUN-EU-RIEULMIEUM
       (?$(C4F(B . "0xB2A0") ; HANGUL SYLLABLE NIEUN-EU-MIEUM
       (?$(C4G(B . "0xB2A1") ; HANGUL SYLLABLE NIEUN-EU-PIEUP
       (?$(C4H(B . "0xB2A3") ; HANGUL SYLLABLE NIEUN-EU-SIOS
       (?$(C4I(B . "0xB2A5") ; HANGUL SYLLABLE NIEUN-EU-IEUNG
       (?$(C4J(B . "0xB2A6") ; HANGUL SYLLABLE NIEUN-EU-CIEUC
       (?$(C4K(B . "0xB2AA") ; HANGUL SYLLABLE NIEUN-EU-PHIEUPH
       (?$(C4L(B . "0xB2AC") ; HANGUL SYLLABLE NIEUN-YI
       (?$(C4M(B . "0xB2B0") ; HANGUL SYLLABLE NIEUN-YI-NIEUN
       (?$(C4N(B . "0xB2B4") ; HANGUL SYLLABLE NIEUN-YI-RIEUL
       (?$(C4O(B . "0xB2C8") ; HANGUL SYLLABLE NIEUN-I
       (?$(C4P(B . "0xB2C9") ; HANGUL SYLLABLE NIEUN-I-KIYEOK
       (?$(C4Q(B . "0xB2CC") ; HANGUL SYLLABLE NIEUN-I-NIEUN
       (?$(C4R(B . "0xB2D0") ; HANGUL SYLLABLE NIEUN-I-RIEUL
       (?$(C4S(B . "0xB2D2") ; HANGUL SYLLABLE NIEUN-I-RIEULMIEUM-<3/22/95>
       (?$(C4T(B . "0xB2D8") ; HANGUL SYLLABLE NIEUN-I-MIEUM
       (?$(C4U(B . "0xB2D9") ; HANGUL SYLLABLE NIEUN-I-PIEUP
       (?$(C4V(B . "0xB2DB") ; HANGUL SYLLABLE NIEUN-I-SIOS
       (?$(C4W(B . "0xB2DD") ; HANGUL SYLLABLE NIEUN-I-IEUNG
       (?$(C4X(B . "0xB2E2") ; HANGUL SYLLABLE NIEUN-I-PHIEUPH
       (?$(C4Y(B . "0xB2E4") ; HANGUL SYLLABLE TIKEUT-A
       (?$(C4Z(B . "0xB2E5") ; HANGUL SYLLABLE TIKEUT-A-KIYEOK
       (?$(C4[(B . "0xB2E6") ; HANGUL SYLLABLE TIKEUT-A-SSANGKIYEOK
       (?$(C4\(B . "0xB2E8") ; HANGUL SYLLABLE TIKEUT-A-NIEUN
       (?$(C4](B . "0xB2EB") ; HANGUL SYLLABLE TIKEUT-A-TIKEUT
       (?$(C4^(B . "0xB2EC") ; HANGUL SYLLABLE TIKEUT-A-RIEUL
       (?$(C4_(B . "0xB2ED") ; HANGUL SYLLABLE TIKEUT-A-RIEULKIYEOK
       (?$(C4`(B . "0xB2EE") ; HANGUL SYLLABLE TIKEUT-A-RIEULMIEUM
       (?$(C4a(B . "0xB2EF") ; HANGUL SYLLABLE TIKEUT-A-RIEULPIEUP
       (?$(C4b(B . "0xB2F3") ; HANGUL SYLLABLE TIKEUT-A-RIEULHIEUH
       (?$(C4c(B . "0xB2F4") ; HANGUL SYLLABLE TIKEUT-A-MIEUM
       (?$(C4d(B . "0xB2F5") ; HANGUL SYLLABLE TIKEUT-A-PIEUP
       (?$(C4e(B . "0xB2F7") ; HANGUL SYLLABLE TIKEUT-A-SIOS
       (?$(C4f(B . "0xB2F8") ; HANGUL SYLLABLE TIKEUT-A-SSANGSIOS
       (?$(C4g(B . "0xB2F9") ; HANGUL SYLLABLE TIKEUT-A-IEUNG
       (?$(C4h(B . "0xB2FA") ; HANGUL SYLLABLE TIKEUT-A-CIEUC
       (?$(C4i(B . "0xB2FB") ; HANGUL SYLLABLE TIKEUT-A-CHIEUCH
       (?$(C4j(B . "0xB2FF") ; HANGUL SYLLABLE TIKEUT-A-HIEUH
       (?$(C4k(B . "0xB300") ; HANGUL SYLLABLE TIKEUT-AE
       (?$(C4l(B . "0xB301") ; HANGUL SYLLABLE TIKEUT-AE-KIYEOK
       (?$(C4m(B . "0xB304") ; HANGUL SYLLABLE TIKEUT-AE-NIEUN
       (?$(C4n(B . "0xB308") ; HANGUL SYLLABLE TIKEUT-AE-RIEUL
       (?$(C4o(B . "0xB310") ; HANGUL SYLLABLE TIKEUT-AE-MIEUM
       (?$(C4p(B . "0xB311") ; HANGUL SYLLABLE TIKEUT-AE-PIEUP
       (?$(C4q(B . "0xB313") ; HANGUL SYLLABLE TIKEUT-AE-SIOS
       (?$(C4r(B . "0xB314") ; HANGUL SYLLABLE TIKEUT-AE-SSANGSIOS
       (?$(C4s(B . "0xB315") ; HANGUL SYLLABLE TIKEUT-AE-IEUNG
       (?$(C4t(B . "0xB31C") ; HANGUL SYLLABLE TIKEUT-YA
       (?$(C4u(B . "0xB354") ; HANGUL SYLLABLE TIKEUT-EO
       (?$(C4v(B . "0xB355") ; HANGUL SYLLABLE TIKEUT-EO-KIYEOK
       (?$(C4w(B . "0xB356") ; HANGUL SYLLABLE TIKEUT-EO-SSANGKIYEOK
       (?$(C4x(B . "0xB358") ; HANGUL SYLLABLE TIKEUT-EO-NIEUN
       (?$(C4y(B . "0xB35B") ; HANGUL SYLLABLE TIKEUT-EO-TIKEUT
       (?$(C4z(B . "0xB35C") ; HANGUL SYLLABLE TIKEUT-EO-RIEUL
       (?$(C4{(B . "0xB35E") ; HANGUL SYLLABLE TIKEUT-EO-RIEULMIEUM
       (?$(C4|(B . "0xB35F") ; HANGUL SYLLABLE TIKEUT-EO-RIEULPIEUP
       (?$(C4}(B . "0xB364") ; HANGUL SYLLABLE TIKEUT-EO-MIEUM
       (?$(C4~(B . "0xB365") ; HANGUL SYLLABLE TIKEUT-EO-PIEUP
       (?$(C5!(B . "0xB367") ; HANGUL SYLLABLE TIKEUT-EO-SIOS
       (?$(C5"(B . "0xB369") ; HANGUL SYLLABLE TIKEUT-EO-IEUNG
       (?$(C5#(B . "0xB36B") ; HANGUL SYLLABLE TIKEUT-EO-CHIEUCH
       (?$(C5$(B . "0xB36E") ; HANGUL SYLLABLE TIKEUT-EO-PHIEUPH
       (?$(C5%(B . "0xB370") ; HANGUL SYLLABLE TIKEUT-E
       (?$(C5&(B . "0xB371") ; HANGUL SYLLABLE TIKEUT-E-KIYEOK
       (?$(C5'(B . "0xB374") ; HANGUL SYLLABLE TIKEUT-E-NIEUN
       (?$(C5((B . "0xB378") ; HANGUL SYLLABLE TIKEUT-E-RIEUL
       (?$(C5)(B . "0xB380") ; HANGUL SYLLABLE TIKEUT-E-MIEUM
       (?$(C5*(B . "0xB381") ; HANGUL SYLLABLE TIKEUT-E-PIEUP
       (?$(C5+(B . "0xB383") ; HANGUL SYLLABLE TIKEUT-E-SIOS
       (?$(C5,(B . "0xB384") ; HANGUL SYLLABLE TIKEUT-E-SSANGSIOS
       (?$(C5-(B . "0xB385") ; HANGUL SYLLABLE TIKEUT-E-IEUNG
       (?$(C5.(B . "0xB38C") ; HANGUL SYLLABLE TIKEUT-YEO
       (?$(C5/(B . "0xB390") ; HANGUL SYLLABLE TIKEUT-YEO-NIEUN
       (?$(C50(B . "0xB394") ; HANGUL SYLLABLE TIKEUT-YEO-RIEUL
       (?$(C51(B . "0xB3A0") ; HANGUL SYLLABLE TIKEUT-YEO-SSANGSIOS
       (?$(C52(B . "0xB3A1") ; HANGUL SYLLABLE TIKEUT-YEO-IEUNG
       (?$(C53(B . "0xB3A8") ; HANGUL SYLLABLE TIKEUT-YE
       (?$(C54(B . "0xB3AC") ; HANGUL SYLLABLE TIKEUT-YE-NIEUN
       (?$(C55(B . "0xB3C4") ; HANGUL SYLLABLE TIKEUT-O
       (?$(C56(B . "0xB3C5") ; HANGUL SYLLABLE TIKEUT-O-KIYEOK
       (?$(C57(B . "0xB3C8") ; HANGUL SYLLABLE TIKEUT-O-NIEUN
       (?$(C58(B . "0xB3CB") ; HANGUL SYLLABLE TIKEUT-O-TIKEUT
       (?$(C59(B . "0xB3CC") ; HANGUL SYLLABLE TIKEUT-O-RIEUL
       (?$(C5:(B . "0xB3CE") ; HANGUL SYLLABLE TIKEUT-O-RIEULMIEUM
       (?$(C5;(B . "0xB3D0") ; HANGUL SYLLABLE TIKEUT-O-RIEULSIOS
       (?$(C5<(B . "0xB3D4") ; HANGUL SYLLABLE TIKEUT-O-MIEUM
       (?$(C5=(B . "0xB3D5") ; HANGUL SYLLABLE TIKEUT-O-PIEUP
       (?$(C5>(B . "0xB3D7") ; HANGUL SYLLABLE TIKEUT-O-SIOS
       (?$(C5?(B . "0xB3D9") ; HANGUL SYLLABLE TIKEUT-O-IEUNG
       (?$(C5@(B . "0xB3DB") ; HANGUL SYLLABLE TIKEUT-O-CHIEUCH
       (?$(C5A(B . "0xB3DD") ; HANGUL SYLLABLE TIKEUT-O-THIEUTH
       (?$(C5B(B . "0xB3E0") ; HANGUL SYLLABLE TIKEUT-WA
       (?$(C5C(B . "0xB3E4") ; HANGUL SYLLABLE TIKEUT-WA-NIEUN
       (?$(C5D(B . "0xB3E8") ; HANGUL SYLLABLE TIKEUT-WA-RIEUL
       (?$(C5E(B . "0xB3FC") ; HANGUL SYLLABLE TIKEUT-WAE
       (?$(C5F(B . "0xB410") ; HANGUL SYLLABLE TIKEUT-WAE-SSANGSIOS
       (?$(C5G(B . "0xB418") ; HANGUL SYLLABLE TIKEUT-OE
       (?$(C5H(B . "0xB41C") ; HANGUL SYLLABLE TIKEUT-OE-NIEUN
       (?$(C5I(B . "0xB420") ; HANGUL SYLLABLE TIKEUT-OE-RIEUL
       (?$(C5J(B . "0xB428") ; HANGUL SYLLABLE TIKEUT-OE-MIEUM
       (?$(C5K(B . "0xB429") ; HANGUL SYLLABLE TIKEUT-OE-PIEUP
       (?$(C5L(B . "0xB42B") ; HANGUL SYLLABLE TIKEUT-OE-SIOS
       (?$(C5M(B . "0xB434") ; HANGUL SYLLABLE TIKEUT-YO
       (?$(C5N(B . "0xB450") ; HANGUL SYLLABLE TIKEUT-U
       (?$(C5O(B . "0xB451") ; HANGUL SYLLABLE TIKEUT-U-KIYEOK
       (?$(C5P(B . "0xB454") ; HANGUL SYLLABLE TIKEUT-U-NIEUN
       (?$(C5Q(B . "0xB458") ; HANGUL SYLLABLE TIKEUT-U-RIEUL
       (?$(C5R(B . "0xB460") ; HANGUL SYLLABLE TIKEUT-U-MIEUM
       (?$(C5S(B . "0xB461") ; HANGUL SYLLABLE TIKEUT-U-PIEUP
       (?$(C5T(B . "0xB463") ; HANGUL SYLLABLE TIKEUT-U-SIOS
       (?$(C5U(B . "0xB465") ; HANGUL SYLLABLE TIKEUT-U-IEUNG
       (?$(C5V(B . "0xB46C") ; HANGUL SYLLABLE TIKEUT-WEO
       (?$(C5W(B . "0xB480") ; HANGUL SYLLABLE TIKEUT-WEO-SSANGSIOS
       (?$(C5X(B . "0xB488") ; HANGUL SYLLABLE TIKEUT-WE
       (?$(C5Y(B . "0xB49D") ; HANGUL SYLLABLE TIKEUT-WE-IEUNG
       (?$(C5Z(B . "0xB4A4") ; HANGUL SYLLABLE TIKEUT-WI
       (?$(C5[(B . "0xB4A8") ; HANGUL SYLLABLE TIKEUT-WI-NIEUN
       (?$(C5\(B . "0xB4AC") ; HANGUL SYLLABLE TIKEUT-WI-RIEUL
       (?$(C5](B . "0xB4B5") ; HANGUL SYLLABLE TIKEUT-WI-PIEUP
       (?$(C5^(B . "0xB4B7") ; HANGUL SYLLABLE TIKEUT-WI-SIOS
       (?$(C5_(B . "0xB4B9") ; HANGUL SYLLABLE TIKEUT-WI-IEUNG
       (?$(C5`(B . "0xB4C0") ; HANGUL SYLLABLE TIKEUT-YU
       (?$(C5a(B . "0xB4C4") ; HANGUL SYLLABLE TIKEUT-YU-NIEUN
       (?$(C5b(B . "0xB4C8") ; HANGUL SYLLABLE TIKEUT-YU-RIEUL
       (?$(C5c(B . "0xB4D0") ; HANGUL SYLLABLE TIKEUT-YU-MIEUM
       (?$(C5d(B . "0xB4D5") ; HANGUL SYLLABLE TIKEUT-YU-IEUNG
       (?$(C5e(B . "0xB4DC") ; HANGUL SYLLABLE TIKEUT-EU
       (?$(C5f(B . "0xB4DD") ; HANGUL SYLLABLE TIKEUT-EU-KIYEOK
       (?$(C5g(B . "0xB4E0") ; HANGUL SYLLABLE TIKEUT-EU-NIEUN
       (?$(C5h(B . "0xB4E3") ; HANGUL SYLLABLE TIKEUT-EU-TIKEUT
       (?$(C5i(B . "0xB4E4") ; HANGUL SYLLABLE TIKEUT-EU-RIEUL
       (?$(C5j(B . "0xB4E6") ; HANGUL SYLLABLE TIKEUT-EU-RIEULMIEUM
       (?$(C5k(B . "0xB4EC") ; HANGUL SYLLABLE TIKEUT-EU-MIEUM
       (?$(C5l(B . "0xB4ED") ; HANGUL SYLLABLE TIKEUT-EU-PIEUP
       (?$(C5m(B . "0xB4EF") ; HANGUL SYLLABLE TIKEUT-EU-SIOS
       (?$(C5n(B . "0xB4F1") ; HANGUL SYLLABLE TIKEUT-EU-IEUNG
       (?$(C5o(B . "0xB4F8") ; HANGUL SYLLABLE TIKEUT-YI
       (?$(C5p(B . "0xB514") ; HANGUL SYLLABLE TIKEUT-I
       (?$(C5q(B . "0xB515") ; HANGUL SYLLABLE TIKEUT-I-KIYEOK
       (?$(C5r(B . "0xB518") ; HANGUL SYLLABLE TIKEUT-I-NIEUN
       (?$(C5s(B . "0xB51B") ; HANGUL SYLLABLE TIKEUT-I-TIKEUT
       (?$(C5t(B . "0xB51C") ; HANGUL SYLLABLE TIKEUT-I-RIEUL
       (?$(C5u(B . "0xB524") ; HANGUL SYLLABLE TIKEUT-I-MIEUM
       (?$(C5v(B . "0xB525") ; HANGUL SYLLABLE TIKEUT-I-PIEUP
       (?$(C5w(B . "0xB527") ; HANGUL SYLLABLE TIKEUT-I-SIOS
       (?$(C5x(B . "0xB528") ; HANGUL SYLLABLE TIKEUT-I-SSANGSIOS
       (?$(C5y(B . "0xB529") ; HANGUL SYLLABLE TIKEUT-I-IEUNG
       (?$(C5z(B . "0xB52A") ; HANGUL SYLLABLE TIKEUT-I-CIEUC
       (?$(C5{(B . "0xB530") ; HANGUL SYLLABLE SSANGTIKEUT-A
       (?$(C5|(B . "0xB531") ; HANGUL SYLLABLE SSANGTIKEUT-A-KIYEOK
       (?$(C5}(B . "0xB534") ; HANGUL SYLLABLE SSANGTIKEUT-A-NIEUN
       (?$(C5~(B . "0xB538") ; HANGUL SYLLABLE SSANGTIKEUT-A-RIEUL
       (?$(C6!(B . "0xB540") ; HANGUL SYLLABLE SSANGTIKEUT-A-MIEUM
       (?$(C6"(B . "0xB541") ; HANGUL SYLLABLE SSANGTIKEUT-A-PIEUP
       (?$(C6#(B . "0xB543") ; HANGUL SYLLABLE SSANGTIKEUT-A-SIOS
       (?$(C6$(B . "0xB544") ; HANGUL SYLLABLE SSANGTIKEUT-A-SSANGSIOS
       (?$(C6%(B . "0xB545") ; HANGUL SYLLABLE SSANGTIKEUT-A-IEUNG
       (?$(C6&(B . "0xB54B") ; HANGUL SYLLABLE SSANGTIKEUT-A-HIEUH
       (?$(C6'(B . "0xB54C") ; HANGUL SYLLABLE SSANGTIKEUT-AE
       (?$(C6((B . "0xB54D") ; HANGUL SYLLABLE SSANGTIKEUT-AE-KIYEOK
       (?$(C6)(B . "0xB550") ; HANGUL SYLLABLE SSANGTIKEUT-AE-NIEUN
       (?$(C6*(B . "0xB554") ; HANGUL SYLLABLE SSANGTIKEUT-AE-RIEUL
       (?$(C6+(B . "0xB55C") ; HANGUL SYLLABLE SSANGTIKEUT-AE-MIEUM
       (?$(C6,(B . "0xB55D") ; HANGUL SYLLABLE SSANGTIKEUT-AE-PIEUP
       (?$(C6-(B . "0xB55F") ; HANGUL SYLLABLE SSANGTIKEUT-AE-SIOS
       (?$(C6.(B . "0xB560") ; HANGUL SYLLABLE SSANGTIKEUT-AE-SSANGSIOS
       (?$(C6/(B . "0xB561") ; HANGUL SYLLABLE SSANGTIKEUT-AE-IEUNG
       (?$(C60(B . "0xB5A0") ; HANGUL SYLLABLE SSANGTIKEUT-EO
       (?$(C61(B . "0xB5A1") ; HANGUL SYLLABLE SSANGTIKEUT-EO-KIYEOK
       (?$(C62(B . "0xB5A4") ; HANGUL SYLLABLE SSANGTIKEUT-EO-NIEUN
       (?$(C63(B . "0xB5A8") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEUL
       (?$(C64(B . "0xB5AA") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEULMIEUM
       (?$(C65(B . "0xB5AB") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEULPIEUP
       (?$(C66(B . "0xB5B0") ; HANGUL SYLLABLE SSANGTIKEUT-EO-MIEUM
       (?$(C67(B . "0xB5B1") ; HANGUL SYLLABLE SSANGTIKEUT-EO-PIEUP
       (?$(C68(B . "0xB5B3") ; HANGUL SYLLABLE SSANGTIKEUT-EO-SIOS
       (?$(C69(B . "0xB5B4") ; HANGUL SYLLABLE SSANGTIKEUT-EO-SSANGSIOS
       (?$(C6:(B . "0xB5B5") ; HANGUL SYLLABLE SSANGTIKEUT-EO-IEUNG
       (?$(C6;(B . "0xB5BB") ; HANGUL SYLLABLE SSANGTIKEUT-EO-HIEUH
       (?$(C6<(B . "0xB5BC") ; HANGUL SYLLABLE SSANGTIKEUT-E
       (?$(C6=(B . "0xB5BD") ; HANGUL SYLLABLE SSANGTIKEUT-E-KIYEOK
       (?$(C6>(B . "0xB5C0") ; HANGUL SYLLABLE SSANGTIKEUT-E-NIEUN
       (?$(C6?(B . "0xB5C4") ; HANGUL SYLLABLE SSANGTIKEUT-E-RIEUL
       (?$(C6@(B . "0xB5CC") ; HANGUL SYLLABLE SSANGTIKEUT-E-MIEUM
       (?$(C6A(B . "0xB5CD") ; HANGUL SYLLABLE SSANGTIKEUT-E-PIEUP
       (?$(C6B(B . "0xB5CF") ; HANGUL SYLLABLE SSANGTIKEUT-E-SIOS
       (?$(C6C(B . "0xB5D0") ; HANGUL SYLLABLE SSANGTIKEUT-E-SSANGSIOS
       (?$(C6D(B . "0xB5D1") ; HANGUL SYLLABLE SSANGTIKEUT-E-IEUNG
       (?$(C6E(B . "0xB5D8") ; HANGUL SYLLABLE SSANGTIKEUT-YEO
       (?$(C6F(B . "0xB5EC") ; HANGUL SYLLABLE SSANGTIKEUT-YEO-SSANGSIOS
       (?$(C6G(B . "0xB610") ; HANGUL SYLLABLE SSANGTIKEUT-O
       (?$(C6H(B . "0xB611") ; HANGUL SYLLABLE SSANGTIKEUT-O-KIYEOK
       (?$(C6I(B . "0xB614") ; HANGUL SYLLABLE SSANGTIKEUT-O-NIEUN
       (?$(C6J(B . "0xB618") ; HANGUL SYLLABLE SSANGTIKEUT-O-RIEUL
       (?$(C6K(B . "0xB625") ; HANGUL SYLLABLE SSANGTIKEUT-O-IEUNG
       (?$(C6L(B . "0xB62C") ; HANGUL SYLLABLE SSANGTIKEUT-WA
       (?$(C6M(B . "0xB634") ; HANGUL SYLLABLE SSANGTIKEUT-WA-RIEUL
       (?$(C6N(B . "0xB648") ; HANGUL SYLLABLE SSANGTIKEUT-WAE
       (?$(C6O(B . "0xB664") ; HANGUL SYLLABLE SSANGTIKEUT-OE
       (?$(C6P(B . "0xB668") ; HANGUL SYLLABLE SSANGTIKEUT-OE-NIEUN
       (?$(C6Q(B . "0xB69C") ; HANGUL SYLLABLE SSANGTIKEUT-U
       (?$(C6R(B . "0xB69D") ; HANGUL SYLLABLE SSANGTIKEUT-U-KIYEOK
       (?$(C6S(B . "0xB6A0") ; HANGUL SYLLABLE SSANGTIKEUT-U-NIEUN
       (?$(C6T(B . "0xB6A4") ; HANGUL SYLLABLE SSANGTIKEUT-U-RIEUL
       (?$(C6U(B . "0xB6AB") ; HANGUL SYLLABLE SSANGTIKEUT-U-RIEULHIEUH
       (?$(C6V(B . "0xB6AC") ; HANGUL SYLLABLE SSANGTIKEUT-U-MIEUM
       (?$(C6W(B . "0xB6B1") ; HANGUL SYLLABLE SSANGTIKEUT-U-IEUNG
       (?$(C6X(B . "0xB6D4") ; HANGUL SYLLABLE SSANGTIKEUT-WE
       (?$(C6Y(B . "0xB6F0") ; HANGUL SYLLABLE SSANGTIKEUT-WI
       (?$(C6Z(B . "0xB6F4") ; HANGUL SYLLABLE SSANGTIKEUT-WI-NIEUN
       (?$(C6[(B . "0xB6F8") ; HANGUL SYLLABLE SSANGTIKEUT-WI-RIEUL
       (?$(C6\(B . "0xB700") ; HANGUL SYLLABLE SSANGTIKEUT-WI-MIEUM
       (?$(C6](B . "0xB701") ; HANGUL SYLLABLE SSANGTIKEUT-WI-PIEUP
       (?$(C6^(B . "0xB705") ; HANGUL SYLLABLE SSANGTIKEUT-WI-IEUNG
       (?$(C6_(B . "0xB728") ; HANGUL SYLLABLE SSANGTIKEUT-EU
       (?$(C6`(B . "0xB729") ; HANGUL SYLLABLE SSANGTIKEUT-EU-KIYEOK
       (?$(C6a(B . "0xB72C") ; HANGUL SYLLABLE SSANGTIKEUT-EU-NIEUN
       (?$(C6b(B . "0xB72F") ; HANGUL SYLLABLE SSANGTIKEUT-EU-TIKEUT
       (?$(C6c(B . "0xB730") ; HANGUL SYLLABLE SSANGTIKEUT-EU-RIEUL
       (?$(C6d(B . "0xB738") ; HANGUL SYLLABLE SSANGTIKEUT-EU-MIEUM
       (?$(C6e(B . "0xB739") ; HANGUL SYLLABLE SSANGTIKEUT-EU-PIEUP
       (?$(C6f(B . "0xB73B") ; HANGUL SYLLABLE SSANGTIKEUT-EU-SIOS
       (?$(C6g(B . "0xB744") ; HANGUL SYLLABLE SSANGTIKEUT-YI
       (?$(C6h(B . "0xB748") ; HANGUL SYLLABLE SSANGTIKEUT-YI-NIEUN
       (?$(C6i(B . "0xB74C") ; HANGUL SYLLABLE SSANGTIKEUT-YI-RIEUL
       (?$(C6j(B . "0xB754") ; HANGUL SYLLABLE SSANGTIKEUT-YI-MIEUM
       (?$(C6k(B . "0xB755") ; HANGUL SYLLABLE SSANGTIKEUT-YI-PIEUP
       (?$(C6l(B . "0xB760") ; HANGUL SYLLABLE SSANGTIKEUT-I
       (?$(C6m(B . "0xB764") ; HANGUL SYLLABLE SSANGTIKEUT-I-NIEUN
       (?$(C6n(B . "0xB768") ; HANGUL SYLLABLE SSANGTIKEUT-I-RIEUL
       (?$(C6o(B . "0xB770") ; HANGUL SYLLABLE SSANGTIKEUT-I-MIEUM
       (?$(C6p(B . "0xB771") ; HANGUL SYLLABLE SSANGTIKEUT-I-PIEUP
       (?$(C6q(B . "0xB773") ; HANGUL SYLLABLE SSANGTIKEUT-I-SIOS
       (?$(C6r(B . "0xB775") ; HANGUL SYLLABLE SSANGTIKEUT-I-IEUNG
       (?$(C6s(B . "0xB77C") ; HANGUL SYLLABLE RIEUL-A
       (?$(C6t(B . "0xB77D") ; HANGUL SYLLABLE RIEUL-A-KIYEOK
       (?$(C6u(B . "0xB780") ; HANGUL SYLLABLE RIEUL-A-NIEUN
       (?$(C6v(B . "0xB784") ; HANGUL SYLLABLE RIEUL-A-RIEUL
       (?$(C6w(B . "0xB78C") ; HANGUL SYLLABLE RIEUL-A-MIEUM
       (?$(C6x(B . "0xB78D") ; HANGUL SYLLABLE RIEUL-A-PIEUP
       (?$(C6y(B . "0xB78F") ; HANGUL SYLLABLE RIEUL-A-SIOS
       (?$(C6z(B . "0xB790") ; HANGUL SYLLABLE RIEUL-A-SSANGSIOS
       (?$(C6{(B . "0xB791") ; HANGUL SYLLABLE RIEUL-A-IEUNG
       (?$(C6|(B . "0xB792") ; HANGUL SYLLABLE RIEUL-A-CIEUC
       (?$(C6}(B . "0xB796") ; HANGUL SYLLABLE RIEUL-A-PHIEUPH
       (?$(C6~(B . "0xB797") ; HANGUL SYLLABLE RIEUL-A-HIEUH
       (?$(C7!(B . "0xB798") ; HANGUL SYLLABLE RIEUL-AE
       (?$(C7"(B . "0xB799") ; HANGUL SYLLABLE RIEUL-AE-KIYEOK
       (?$(C7#(B . "0xB79C") ; HANGUL SYLLABLE RIEUL-AE-NIEUN
       (?$(C7$(B . "0xB7A0") ; HANGUL SYLLABLE RIEUL-AE-RIEUL
       (?$(C7%(B . "0xB7A8") ; HANGUL SYLLABLE RIEUL-AE-MIEUM
       (?$(C7&(B . "0xB7A9") ; HANGUL SYLLABLE RIEUL-AE-PIEUP
       (?$(C7'(B . "0xB7AB") ; HANGUL SYLLABLE RIEUL-AE-SIOS
       (?$(C7((B . "0xB7AC") ; HANGUL SYLLABLE RIEUL-AE-SSANGSIOS
       (?$(C7)(B . "0xB7AD") ; HANGUL SYLLABLE RIEUL-AE-IEUNG
       (?$(C7*(B . "0xB7B4") ; HANGUL SYLLABLE RIEUL-YA
       (?$(C7+(B . "0xB7B5") ; HANGUL SYLLABLE RIEUL-YA-KIYEOK
       (?$(C7,(B . "0xB7B8") ; HANGUL SYLLABLE RIEUL-YA-NIEUN
       (?$(C7-(B . "0xB7C7") ; HANGUL SYLLABLE RIEUL-YA-SIOS
       (?$(C7.(B . "0xB7C9") ; HANGUL SYLLABLE RIEUL-YA-IEUNG
       (?$(C7/(B . "0xB7EC") ; HANGUL SYLLABLE RIEUL-EO
       (?$(C70(B . "0xB7ED") ; HANGUL SYLLABLE RIEUL-EO-KIYEOK
       (?$(C71(B . "0xB7F0") ; HANGUL SYLLABLE RIEUL-EO-NIEUN
       (?$(C72(B . "0xB7F4") ; HANGUL SYLLABLE RIEUL-EO-RIEUL
       (?$(C73(B . "0xB7FC") ; HANGUL SYLLABLE RIEUL-EO-MIEUM
       (?$(C74(B . "0xB7FD") ; HANGUL SYLLABLE RIEUL-EO-PIEUP
       (?$(C75(B . "0xB7FF") ; HANGUL SYLLABLE RIEUL-EO-SIOS
       (?$(C76(B . "0xB800") ; HANGUL SYLLABLE RIEUL-EO-SSANGSIOS
       (?$(C77(B . "0xB801") ; HANGUL SYLLABLE RIEUL-EO-IEUNG
       (?$(C78(B . "0xB807") ; HANGUL SYLLABLE RIEUL-EO-HIEUH
       (?$(C79(B . "0xB808") ; HANGUL SYLLABLE RIEUL-E
       (?$(C7:(B . "0xB809") ; HANGUL SYLLABLE RIEUL-E-KIYEOK
       (?$(C7;(B . "0xB80C") ; HANGUL SYLLABLE RIEUL-E-NIEUN
       (?$(C7<(B . "0xB810") ; HANGUL SYLLABLE RIEUL-E-RIEUL
       (?$(C7=(B . "0xB818") ; HANGUL SYLLABLE RIEUL-E-MIEUM
       (?$(C7>(B . "0xB819") ; HANGUL SYLLABLE RIEUL-E-PIEUP
       (?$(C7?(B . "0xB81B") ; HANGUL SYLLABLE RIEUL-E-SIOS
       (?$(C7@(B . "0xB81D") ; HANGUL SYLLABLE RIEUL-E-IEUNG
       (?$(C7A(B . "0xB824") ; HANGUL SYLLABLE RIEUL-YEO
       (?$(C7B(B . "0xB825") ; HANGUL SYLLABLE RIEUL-YEO-KIYEOK
       (?$(C7C(B . "0xB828") ; HANGUL SYLLABLE RIEUL-YEO-NIEUN
       (?$(C7D(B . "0xB82C") ; HANGUL SYLLABLE RIEUL-YEO-RIEUL
       (?$(C7E(B . "0xB834") ; HANGUL SYLLABLE RIEUL-YEO-MIEUM
       (?$(C7F(B . "0xB835") ; HANGUL SYLLABLE RIEUL-YEO-PIEUP
       (?$(C7G(B . "0xB837") ; HANGUL SYLLABLE RIEUL-YEO-SIOS
       (?$(C7H(B . "0xB838") ; HANGUL SYLLABLE RIEUL-YEO-SSANGSIOS
       (?$(C7I(B . "0xB839") ; HANGUL SYLLABLE RIEUL-YEO-IEUNG
       (?$(C7J(B . "0xB840") ; HANGUL SYLLABLE RIEUL-YE
       (?$(C7K(B . "0xB844") ; HANGUL SYLLABLE RIEUL-YE-NIEUN
       (?$(C7L(B . "0xB851") ; HANGUL SYLLABLE RIEUL-YE-PIEUP
       (?$(C7M(B . "0xB853") ; HANGUL SYLLABLE RIEUL-YE-SIOS
       (?$(C7N(B . "0xB85C") ; HANGUL SYLLABLE RIEUL-O
       (?$(C7O(B . "0xB85D") ; HANGUL SYLLABLE RIEUL-O-KIYEOK
       (?$(C7P(B . "0xB860") ; HANGUL SYLLABLE RIEUL-O-NIEUN
       (?$(C7Q(B . "0xB864") ; HANGUL SYLLABLE RIEUL-O-RIEUL
       (?$(C7R(B . "0xB86C") ; HANGUL SYLLABLE RIEUL-O-MIEUM
       (?$(C7S(B . "0xB86D") ; HANGUL SYLLABLE RIEUL-O-PIEUP
       (?$(C7T(B . "0xB86F") ; HANGUL SYLLABLE RIEUL-O-SIOS
       (?$(C7U(B . "0xB871") ; HANGUL SYLLABLE RIEUL-O-IEUNG
       (?$(C7V(B . "0xB878") ; HANGUL SYLLABLE RIEUL-WA
       (?$(C7W(B . "0xB87C") ; HANGUL SYLLABLE RIEUL-WA-NIEUN
       (?$(C7X(B . "0xB88D") ; HANGUL SYLLABLE RIEUL-WA-IEUNG
       (?$(C7Y(B . "0xB8A8") ; HANGUL SYLLABLE RIEUL-WAE-SSANGSIOS
       (?$(C7Z(B . "0xB8B0") ; HANGUL SYLLABLE RIEUL-OE
       (?$(C7[(B . "0xB8B4") ; HANGUL SYLLABLE RIEUL-OE-NIEUN
       (?$(C7\(B . "0xB8B8") ; HANGUL SYLLABLE RIEUL-OE-RIEUL
       (?$(C7](B . "0xB8C0") ; HANGUL SYLLABLE RIEUL-OE-MIEUM
       (?$(C7^(B . "0xB8C1") ; HANGUL SYLLABLE RIEUL-OE-PIEUP
       (?$(C7_(B . "0xB8C3") ; HANGUL SYLLABLE RIEUL-OE-SIOS
       (?$(C7`(B . "0xB8C5") ; HANGUL SYLLABLE RIEUL-OE-IEUNG
       (?$(C7a(B . "0xB8CC") ; HANGUL SYLLABLE RIEUL-YO
       (?$(C7b(B . "0xB8D0") ; HANGUL SYLLABLE RIEUL-YO-NIEUN
       (?$(C7c(B . "0xB8D4") ; HANGUL SYLLABLE RIEUL-YO-RIEUL
       (?$(C7d(B . "0xB8DD") ; HANGUL SYLLABLE RIEUL-YO-PIEUP
       (?$(C7e(B . "0xB8DF") ; HANGUL SYLLABLE RIEUL-YO-SIOS
       (?$(C7f(B . "0xB8E1") ; HANGUL SYLLABLE RIEUL-YO-IEUNG
       (?$(C7g(B . "0xB8E8") ; HANGUL SYLLABLE RIEUL-U
       (?$(C7h(B . "0xB8E9") ; HANGUL SYLLABLE RIEUL-U-KIYEOK
       (?$(C7i(B . "0xB8EC") ; HANGUL SYLLABLE RIEUL-U-NIEUN
       (?$(C7j(B . "0xB8F0") ; HANGUL SYLLABLE RIEUL-U-RIEUL
       (?$(C7k(B . "0xB8F8") ; HANGUL SYLLABLE RIEUL-U-MIEUM
       (?$(C7l(B . "0xB8F9") ; HANGUL SYLLABLE RIEUL-U-PIEUP
       (?$(C7m(B . "0xB8FB") ; HANGUL SYLLABLE RIEUL-U-SIOS
       (?$(C7n(B . "0xB8FD") ; HANGUL SYLLABLE RIEUL-U-IEUNG
       (?$(C7o(B . "0xB904") ; HANGUL SYLLABLE RIEUL-WEO
       (?$(C7p(B . "0xB918") ; HANGUL SYLLABLE RIEUL-WEO-SSANGSIOS
       (?$(C7q(B . "0xB920") ; HANGUL SYLLABLE RIEUL-WE
       (?$(C7r(B . "0xB93C") ; HANGUL SYLLABLE RIEUL-WI
       (?$(C7s(B . "0xB93D") ; HANGUL SYLLABLE RIEUL-WI-KIYEOK
       (?$(C7t(B . "0xB940") ; HANGUL SYLLABLE RIEUL-WI-NIEUN
       (?$(C7u(B . "0xB944") ; HANGUL SYLLABLE RIEUL-WI-RIEUL
       (?$(C7v(B . "0xB94C") ; HANGUL SYLLABLE RIEUL-WI-MIEUM
       (?$(C7w(B . "0xB94F") ; HANGUL SYLLABLE RIEUL-WI-SIOS
       (?$(C7x(B . "0xB951") ; HANGUL SYLLABLE RIEUL-WI-IEUNG
       (?$(C7y(B . "0xB958") ; HANGUL SYLLABLE RIEUL-YU
       (?$(C7z(B . "0xB959") ; HANGUL SYLLABLE RIEUL-YU-KIYEOK
       (?$(C7{(B . "0xB95C") ; HANGUL SYLLABLE RIEUL-YU-NIEUN
       (?$(C7|(B . "0xB960") ; HANGUL SYLLABLE RIEUL-YU-RIEUL
       (?$(C7}(B . "0xB968") ; HANGUL SYLLABLE RIEUL-YU-MIEUM
       (?$(C7~(B . "0xB969") ; HANGUL SYLLABLE RIEUL-YU-PIEUP
       (?$(C8!(B . "0xB96B") ; HANGUL SYLLABLE RIEUL-YU-SIOS
       (?$(C8"(B . "0xB96D") ; HANGUL SYLLABLE RIEUL-YU-IEUNG
       (?$(C8#(B . "0xB974") ; HANGUL SYLLABLE RIEUL-EU
       (?$(C8$(B . "0xB975") ; HANGUL SYLLABLE RIEUL-EU-KIYEOK
       (?$(C8%(B . "0xB978") ; HANGUL SYLLABLE RIEUL-EU-NIEUN
       (?$(C8&(B . "0xB97C") ; HANGUL SYLLABLE RIEUL-EU-RIEUL
       (?$(C8'(B . "0xB984") ; HANGUL SYLLABLE RIEUL-EU-MIEUM
       (?$(C8((B . "0xB985") ; HANGUL SYLLABLE RIEUL-EU-PIEUP
       (?$(C8)(B . "0xB987") ; HANGUL SYLLABLE RIEUL-EU-SIOS
       (?$(C8*(B . "0xB989") ; HANGUL SYLLABLE RIEUL-EU-IEUNG
       (?$(C8+(B . "0xB98A") ; HANGUL SYLLABLE RIEUL-EU-CIEUC
       (?$(C8,(B . "0xB98D") ; HANGUL SYLLABLE RIEUL-EU-THIEUTH
       (?$(C8-(B . "0xB98E") ; HANGUL SYLLABLE RIEUL-EU-PHIEUPH
       (?$(C8.(B . "0xB9AC") ; HANGUL SYLLABLE RIEUL-I
       (?$(C8/(B . "0xB9AD") ; HANGUL SYLLABLE RIEUL-I-KIYEOK
       (?$(C80(B . "0xB9B0") ; HANGUL SYLLABLE RIEUL-I-NIEUN
       (?$(C81(B . "0xB9B4") ; HANGUL SYLLABLE RIEUL-I-RIEUL
       (?$(C82(B . "0xB9BC") ; HANGUL SYLLABLE RIEUL-I-MIEUM
       (?$(C83(B . "0xB9BD") ; HANGUL SYLLABLE RIEUL-I-PIEUP
       (?$(C84(B . "0xB9BF") ; HANGUL SYLLABLE RIEUL-I-SIOS
       (?$(C85(B . "0xB9C1") ; HANGUL SYLLABLE RIEUL-I-IEUNG
       (?$(C86(B . "0xB9C8") ; HANGUL SYLLABLE MIEUM-A
       (?$(C87(B . "0xB9C9") ; HANGUL SYLLABLE MIEUM-A-KIYEOK
       (?$(C88(B . "0xB9CC") ; HANGUL SYLLABLE MIEUM-A-NIEUN
       (?$(C89(B . "0xB9CE") ; HANGUL SYLLABLE MIEUM-A-NIEUNHIEUH
       (?$(C8:(B . "0xB9CF") ; HANGUL SYLLABLE MIEUM-A-TIKEUT
       (?$(C8;(B . "0xB9D0") ; HANGUL SYLLABLE MIEUM-A-RIEUL
       (?$(C8<(B . "0xB9D1") ; HANGUL SYLLABLE MIEUM-A-RIEULKIYEOK
       (?$(C8=(B . "0xB9D2") ; HANGUL SYLLABLE MIEUM-A-RIEULMIEUM
       (?$(C8>(B . "0xB9D8") ; HANGUL SYLLABLE MIEUM-A-MIEUM
       (?$(C8?(B . "0xB9D9") ; HANGUL SYLLABLE MIEUM-A-PIEUP
       (?$(C8@(B . "0xB9DB") ; HANGUL SYLLABLE MIEUM-A-SIOS
       (?$(C8A(B . "0xB9DD") ; HANGUL SYLLABLE MIEUM-A-IEUNG
       (?$(C8B(B . "0xB9DE") ; HANGUL SYLLABLE MIEUM-A-CIEUC
       (?$(C8C(B . "0xB9E1") ; HANGUL SYLLABLE MIEUM-A-THIEUTH
       (?$(C8D(B . "0xB9E3") ; HANGUL SYLLABLE MIEUM-A-HIEUH
       (?$(C8E(B . "0xB9E4") ; HANGUL SYLLABLE MIEUM-AE
       (?$(C8F(B . "0xB9E5") ; HANGUL SYLLABLE MIEUM-AE-KIYEOK
       (?$(C8G(B . "0xB9E8") ; HANGUL SYLLABLE MIEUM-AE-NIEUN
       (?$(C8H(B . "0xB9EC") ; HANGUL SYLLABLE MIEUM-AE-RIEUL
       (?$(C8I(B . "0xB9F4") ; HANGUL SYLLABLE MIEUM-AE-MIEUM
       (?$(C8J(B . "0xB9F5") ; HANGUL SYLLABLE MIEUM-AE-PIEUP
       (?$(C8K(B . "0xB9F7") ; HANGUL SYLLABLE MIEUM-AE-SIOS
       (?$(C8L(B . "0xB9F8") ; HANGUL SYLLABLE MIEUM-AE-SSANGSIOS
       (?$(C8M(B . "0xB9F9") ; HANGUL SYLLABLE MIEUM-AE-IEUNG
       (?$(C8N(B . "0xB9FA") ; HANGUL SYLLABLE MIEUM-AE-CIEUC
       (?$(C8O(B . "0xBA00") ; HANGUL SYLLABLE MIEUM-YA
       (?$(C8P(B . "0xBA01") ; HANGUL SYLLABLE MIEUM-YA-KIYEOK
       (?$(C8Q(B . "0xBA08") ; HANGUL SYLLABLE MIEUM-YA-RIEUL
       (?$(C8R(B . "0xBA15") ; HANGUL SYLLABLE MIEUM-YA-IEUNG
       (?$(C8S(B . "0xBA38") ; HANGUL SYLLABLE MIEUM-EO
       (?$(C8T(B . "0xBA39") ; HANGUL SYLLABLE MIEUM-EO-KIYEOK
       (?$(C8U(B . "0xBA3C") ; HANGUL SYLLABLE MIEUM-EO-NIEUN
       (?$(C8V(B . "0xBA40") ; HANGUL SYLLABLE MIEUM-EO-RIEUL
       (?$(C8W(B . "0xBA42") ; HANGUL SYLLABLE MIEUM-EO-RIEULMIEUM
       (?$(C8X(B . "0xBA48") ; HANGUL SYLLABLE MIEUM-EO-MIEUM
       (?$(C8Y(B . "0xBA49") ; HANGUL SYLLABLE MIEUM-EO-PIEUP
       (?$(C8Z(B . "0xBA4B") ; HANGUL SYLLABLE MIEUM-EO-SIOS
       (?$(C8[(B . "0xBA4D") ; HANGUL SYLLABLE MIEUM-EO-IEUNG
       (?$(C8\(B . "0xBA4E") ; HANGUL SYLLABLE MIEUM-EO-CIEUC
       (?$(C8](B . "0xBA53") ; HANGUL SYLLABLE MIEUM-EO-HIEUH
       (?$(C8^(B . "0xBA54") ; HANGUL SYLLABLE MIEUM-E
       (?$(C8_(B . "0xBA55") ; HANGUL SYLLABLE MIEUM-E-KIYEOK
       (?$(C8`(B . "0xBA58") ; HANGUL SYLLABLE MIEUM-E-NIEUN
       (?$(C8a(B . "0xBA5C") ; HANGUL SYLLABLE MIEUM-E-RIEUL
       (?$(C8b(B . "0xBA64") ; HANGUL SYLLABLE MIEUM-E-MIEUM
       (?$(C8c(B . "0xBA65") ; HANGUL SYLLABLE MIEUM-E-PIEUP
       (?$(C8d(B . "0xBA67") ; HANGUL SYLLABLE MIEUM-E-SIOS
       (?$(C8e(B . "0xBA68") ; HANGUL SYLLABLE MIEUM-E-SSANGSIOS
       (?$(C8f(B . "0xBA69") ; HANGUL SYLLABLE MIEUM-E-IEUNG
       (?$(C8g(B . "0xBA70") ; HANGUL SYLLABLE MIEUM-YEO
       (?$(C8h(B . "0xBA71") ; HANGUL SYLLABLE MIEUM-YEO-KIYEOK
       (?$(C8i(B . "0xBA74") ; HANGUL SYLLABLE MIEUM-YEO-NIEUN
       (?$(C8j(B . "0xBA78") ; HANGUL SYLLABLE MIEUM-YEO-RIEUL
       (?$(C8k(B . "0xBA83") ; HANGUL SYLLABLE MIEUM-YEO-SIOS
       (?$(C8l(B . "0xBA84") ; HANGUL SYLLABLE MIEUM-YEO-SSANGSIOS
       (?$(C8m(B . "0xBA85") ; HANGUL SYLLABLE MIEUM-YEO-IEUNG
       (?$(C8n(B . "0xBA87") ; HANGUL SYLLABLE MIEUM-YEO-CHIEUCH
       (?$(C8o(B . "0xBA8C") ; HANGUL SYLLABLE MIEUM-YE
       (?$(C8p(B . "0xBAA8") ; HANGUL SYLLABLE MIEUM-O
       (?$(C8q(B . "0xBAA9") ; HANGUL SYLLABLE MIEUM-O-KIYEOK
       (?$(C8r(B . "0xBAAB") ; HANGUL SYLLABLE MIEUM-O-KIYEOKSIOS
       (?$(C8s(B . "0xBAAC") ; HANGUL SYLLABLE MIEUM-O-NIEUN
       (?$(C8t(B . "0xBAB0") ; HANGUL SYLLABLE MIEUM-O-RIEUL
       (?$(C8u(B . "0xBAB2") ; HANGUL SYLLABLE MIEUM-O-RIEULMIEUM
       (?$(C8v(B . "0xBAB8") ; HANGUL SYLLABLE MIEUM-O-MIEUM
       (?$(C8w(B . "0xBAB9") ; HANGUL SYLLABLE MIEUM-O-PIEUP
       (?$(C8x(B . "0xBABB") ; HANGUL SYLLABLE MIEUM-O-SIOS
       (?$(C8y(B . "0xBABD") ; HANGUL SYLLABLE MIEUM-O-IEUNG
       (?$(C8z(B . "0xBAC4") ; HANGUL SYLLABLE MIEUM-WA
       (?$(C8{(B . "0xBAC8") ; HANGUL SYLLABLE MIEUM-WA-NIEUN
       (?$(C8|(B . "0xBAD8") ; HANGUL SYLLABLE MIEUM-WA-SSANGSIOS
       (?$(C8}(B . "0xBAD9") ; HANGUL SYLLABLE MIEUM-WA-IEUNG
       (?$(C8~(B . "0xBAFC") ; HANGUL SYLLABLE MIEUM-OE
       (?$(C9!(B . "0xBB00") ; HANGUL SYLLABLE MIEUM-OE-NIEUN
       (?$(C9"(B . "0xBB04") ; HANGUL SYLLABLE MIEUM-OE-RIEUL
       (?$(C9#(B . "0xBB0D") ; HANGUL SYLLABLE MIEUM-OE-PIEUP
       (?$(C9$(B . "0xBB0F") ; HANGUL SYLLABLE MIEUM-OE-SIOS
       (?$(C9%(B . "0xBB11") ; HANGUL SYLLABLE MIEUM-OE-IEUNG
       (?$(C9&(B . "0xBB18") ; HANGUL SYLLABLE MIEUM-YO
       (?$(C9'(B . "0xBB1C") ; HANGUL SYLLABLE MIEUM-YO-NIEUN
       (?$(C9((B . "0xBB20") ; HANGUL SYLLABLE MIEUM-YO-RIEUL
       (?$(C9)(B . "0xBB29") ; HANGUL SYLLABLE MIEUM-YO-PIEUP
       (?$(C9*(B . "0xBB2B") ; HANGUL SYLLABLE MIEUM-YO-SIOS
       (?$(C9+(B . "0xBB34") ; HANGUL SYLLABLE MIEUM-U
       (?$(C9,(B . "0xBB35") ; HANGUL SYLLABLE MIEUM-U-KIYEOK
       (?$(C9-(B . "0xBB36") ; HANGUL SYLLABLE MIEUM-U-SSANGKIYEOK
       (?$(C9.(B . "0xBB38") ; HANGUL SYLLABLE MIEUM-U-NIEUN
       (?$(C9/(B . "0xBB3B") ; HANGUL SYLLABLE MIEUM-U-TIKEUT
       (?$(C90(B . "0xBB3C") ; HANGUL SYLLABLE MIEUM-U-RIEUL
       (?$(C91(B . "0xBB3D") ; HANGUL SYLLABLE MIEUM-U-RIEULKIYEOK
       (?$(C92(B . "0xBB3E") ; HANGUL SYLLABLE MIEUM-U-RIEULMIEUM
       (?$(C93(B . "0xBB44") ; HANGUL SYLLABLE MIEUM-U-MIEUM
       (?$(C94(B . "0xBB45") ; HANGUL SYLLABLE MIEUM-U-PIEUP
       (?$(C95(B . "0xBB47") ; HANGUL SYLLABLE MIEUM-U-SIOS
       (?$(C96(B . "0xBB49") ; HANGUL SYLLABLE MIEUM-U-IEUNG
       (?$(C97(B . "0xBB4D") ; HANGUL SYLLABLE MIEUM-U-THIEUTH
       (?$(C98(B . "0xBB4F") ; HANGUL SYLLABLE MIEUM-U-HIEUH
       (?$(C99(B . "0xBB50") ; HANGUL SYLLABLE MIEUM-WEO
       (?$(C9:(B . "0xBB54") ; HANGUL SYLLABLE MIEUM-WEO-NIEUN
       (?$(C9;(B . "0xBB58") ; HANGUL SYLLABLE MIEUM-WEO-RIEUL
       (?$(C9<(B . "0xBB61") ; HANGUL SYLLABLE MIEUM-WEO-PIEUP
       (?$(C9=(B . "0xBB63") ; HANGUL SYLLABLE MIEUM-WEO-SIOS
       (?$(C9>(B . "0xBB6C") ; HANGUL SYLLABLE MIEUM-WE
       (?$(C9?(B . "0xBB88") ; HANGUL SYLLABLE MIEUM-WI
       (?$(C9@(B . "0xBB8C") ; HANGUL SYLLABLE MIEUM-WI-NIEUN
       (?$(C9A(B . "0xBB90") ; HANGUL SYLLABLE MIEUM-WI-RIEUL
       (?$(C9B(B . "0xBBA4") ; HANGUL SYLLABLE MIEUM-YU
       (?$(C9C(B . "0xBBA8") ; HANGUL SYLLABLE MIEUM-YU-NIEUN
       (?$(C9D(B . "0xBBAC") ; HANGUL SYLLABLE MIEUM-YU-RIEUL
       (?$(C9E(B . "0xBBB4") ; HANGUL SYLLABLE MIEUM-YU-MIEUM
       (?$(C9F(B . "0xBBB7") ; HANGUL SYLLABLE MIEUM-YU-SIOS
       (?$(C9G(B . "0xBBC0") ; HANGUL SYLLABLE MIEUM-EU
       (?$(C9H(B . "0xBBC4") ; HANGUL SYLLABLE MIEUM-EU-NIEUN
       (?$(C9I(B . "0xBBC8") ; HANGUL SYLLABLE MIEUM-EU-RIEUL
       (?$(C9J(B . "0xBBD0") ; HANGUL SYLLABLE MIEUM-EU-MIEUM
       (?$(C9K(B . "0xBBD3") ; HANGUL SYLLABLE MIEUM-EU-SIOS
       (?$(C9L(B . "0xBBF8") ; HANGUL SYLLABLE MIEUM-I
       (?$(C9M(B . "0xBBF9") ; HANGUL SYLLABLE MIEUM-I-KIYEOK
       (?$(C9N(B . "0xBBFC") ; HANGUL SYLLABLE MIEUM-I-NIEUN
       (?$(C9O(B . "0xBBFF") ; HANGUL SYLLABLE MIEUM-I-TIKEUT
       (?$(C9P(B . "0xBC00") ; HANGUL SYLLABLE MIEUM-I-RIEUL
       (?$(C9Q(B . "0xBC02") ; HANGUL SYLLABLE MIEUM-I-RIEULMIEUM
       (?$(C9R(B . "0xBC08") ; HANGUL SYLLABLE MIEUM-I-MIEUM
       (?$(C9S(B . "0xBC09") ; HANGUL SYLLABLE MIEUM-I-PIEUP
       (?$(C9T(B . "0xBC0B") ; HANGUL SYLLABLE MIEUM-I-SIOS
       (?$(C9U(B . "0xBC0C") ; HANGUL SYLLABLE MIEUM-I-SSANGSIOS
       (?$(C9V(B . "0xBC0D") ; HANGUL SYLLABLE MIEUM-I-IEUNG
       (?$(C9W(B . "0xBC0F") ; HANGUL SYLLABLE MIEUM-I-CHIEUCH
       (?$(C9X(B . "0xBC11") ; HANGUL SYLLABLE MIEUM-I-THIEUTH
       (?$(C9Y(B . "0xBC14") ; HANGUL SYLLABLE PIEUP-A
       (?$(C9Z(B . "0xBC15") ; HANGUL SYLLABLE PIEUP-A-KIYEOK
       (?$(C9[(B . "0xBC16") ; HANGUL SYLLABLE PIEUP-A-SSANGKIYEOK
       (?$(C9\(B . "0xBC17") ; HANGUL SYLLABLE PIEUP-A-KIYEOKSIOS
       (?$(C9](B . "0xBC18") ; HANGUL SYLLABLE PIEUP-A-NIEUN
       (?$(C9^(B . "0xBC1B") ; HANGUL SYLLABLE PIEUP-A-TIKEUT
       (?$(C9_(B . "0xBC1C") ; HANGUL SYLLABLE PIEUP-A-RIEUL
       (?$(C9`(B . "0xBC1D") ; HANGUL SYLLABLE PIEUP-A-RIEULKIYEOK
       (?$(C9a(B . "0xBC1E") ; HANGUL SYLLABLE PIEUP-A-RIEULMIEUM
       (?$(C9b(B . "0xBC1F") ; HANGUL SYLLABLE PIEUP-A-RIEULPIEUP
       (?$(C9c(B . "0xBC24") ; HANGUL SYLLABLE PIEUP-A-MIEUM
       (?$(C9d(B . "0xBC25") ; HANGUL SYLLABLE PIEUP-A-PIEUP
       (?$(C9e(B . "0xBC27") ; HANGUL SYLLABLE PIEUP-A-SIOS
       (?$(C9f(B . "0xBC29") ; HANGUL SYLLABLE PIEUP-A-IEUNG
       (?$(C9g(B . "0xBC2D") ; HANGUL SYLLABLE PIEUP-A-THIEUTH
       (?$(C9h(B . "0xBC30") ; HANGUL SYLLABLE PIEUP-AE
       (?$(C9i(B . "0xBC31") ; HANGUL SYLLABLE PIEUP-AE-KIYEOK
       (?$(C9j(B . "0xBC34") ; HANGUL SYLLABLE PIEUP-AE-NIEUN
       (?$(C9k(B . "0xBC38") ; HANGUL SYLLABLE PIEUP-AE-RIEUL
       (?$(C9l(B . "0xBC40") ; HANGUL SYLLABLE PIEUP-AE-MIEUM
       (?$(C9m(B . "0xBC41") ; HANGUL SYLLABLE PIEUP-AE-PIEUP
       (?$(C9n(B . "0xBC43") ; HANGUL SYLLABLE PIEUP-AE-SIOS
       (?$(C9o(B . "0xBC44") ; HANGUL SYLLABLE PIEUP-AE-SSANGSIOS
       (?$(C9p(B . "0xBC45") ; HANGUL SYLLABLE PIEUP-AE-IEUNG
       (?$(C9q(B . "0xBC49") ; HANGUL SYLLABLE PIEUP-AE-THIEUTH
       (?$(C9r(B . "0xBC4C") ; HANGUL SYLLABLE PIEUP-YA
       (?$(C9s(B . "0xBC4D") ; HANGUL SYLLABLE PIEUP-YA-KIYEOK
       (?$(C9t(B . "0xBC50") ; HANGUL SYLLABLE PIEUP-YA-NIEUN
       (?$(C9u(B . "0xBC5D") ; HANGUL SYLLABLE PIEUP-YA-PIEUP
       (?$(C9v(B . "0xBC84") ; HANGUL SYLLABLE PIEUP-EO
       (?$(C9w(B . "0xBC85") ; HANGUL SYLLABLE PIEUP-EO-KIYEOK
       (?$(C9x(B . "0xBC88") ; HANGUL SYLLABLE PIEUP-EO-NIEUN
       (?$(C9y(B . "0xBC8B") ; HANGUL SYLLABLE PIEUP-EO-TIKEUT
       (?$(C9z(B . "0xBC8C") ; HANGUL SYLLABLE PIEUP-EO-RIEUL
       (?$(C9{(B . "0xBC8E") ; HANGUL SYLLABLE PIEUP-EO-RIEULMIEUM
       (?$(C9|(B . "0xBC94") ; HANGUL SYLLABLE PIEUP-EO-MIEUM
       (?$(C9}(B . "0xBC95") ; HANGUL SYLLABLE PIEUP-EO-PIEUP
       (?$(C9~(B . "0xBC97") ; HANGUL SYLLABLE PIEUP-EO-SIOS
       (?$(C:!(B . "0xBC99") ; HANGUL SYLLABLE PIEUP-EO-IEUNG
       (?$(C:"(B . "0xBC9A") ; HANGUL SYLLABLE PIEUP-EO-CIEUC
       (?$(C:#(B . "0xBCA0") ; HANGUL SYLLABLE PIEUP-E
       (?$(C:$(B . "0xBCA1") ; HANGUL SYLLABLE PIEUP-E-KIYEOK
       (?$(C:%(B . "0xBCA4") ; HANGUL SYLLABLE PIEUP-E-NIEUN
       (?$(C:&(B . "0xBCA7") ; HANGUL SYLLABLE PIEUP-E-TIKEUT
       (?$(C:'(B . "0xBCA8") ; HANGUL SYLLABLE PIEUP-E-RIEUL
       (?$(C:((B . "0xBCB0") ; HANGUL SYLLABLE PIEUP-E-MIEUM
       (?$(C:)(B . "0xBCB1") ; HANGUL SYLLABLE PIEUP-E-PIEUP
       (?$(C:*(B . "0xBCB3") ; HANGUL SYLLABLE PIEUP-E-SIOS
       (?$(C:+(B . "0xBCB4") ; HANGUL SYLLABLE PIEUP-E-SSANGSIOS
       (?$(C:,(B . "0xBCB5") ; HANGUL SYLLABLE PIEUP-E-IEUNG
       (?$(C:-(B . "0xBCBC") ; HANGUL SYLLABLE PIEUP-YEO
       (?$(C:.(B . "0xBCBD") ; HANGUL SYLLABLE PIEUP-YEO-KIYEOK
       (?$(C:/(B . "0xBCC0") ; HANGUL SYLLABLE PIEUP-YEO-NIEUN
       (?$(C:0(B . "0xBCC4") ; HANGUL SYLLABLE PIEUP-YEO-RIEUL
       (?$(C:1(B . "0xBCCD") ; HANGUL SYLLABLE PIEUP-YEO-PIEUP
       (?$(C:2(B . "0xBCCF") ; HANGUL SYLLABLE PIEUP-YEO-SIOS
       (?$(C:3(B . "0xBCD0") ; HANGUL SYLLABLE PIEUP-YEO-SSANGSIOS
       (?$(C:4(B . "0xBCD1") ; HANGUL SYLLABLE PIEUP-YEO-IEUNG
       (?$(C:5(B . "0xBCD5") ; HANGUL SYLLABLE PIEUP-YEO-THIEUTH
       (?$(C:6(B . "0xBCD8") ; HANGUL SYLLABLE PIEUP-YE
       (?$(C:7(B . "0xBCDC") ; HANGUL SYLLABLE PIEUP-YE-NIEUN
       (?$(C:8(B . "0xBCF4") ; HANGUL SYLLABLE PIEUP-O
       (?$(C:9(B . "0xBCF5") ; HANGUL SYLLABLE PIEUP-O-KIYEOK
       (?$(C::(B . "0xBCF6") ; HANGUL SYLLABLE PIEUP-O-SSANGKIYEOK
       (?$(C:;(B . "0xBCF8") ; HANGUL SYLLABLE PIEUP-O-NIEUN
       (?$(C:<(B . "0xBCFC") ; HANGUL SYLLABLE PIEUP-O-RIEUL
       (?$(C:=(B . "0xBD04") ; HANGUL SYLLABLE PIEUP-O-MIEUM
       (?$(C:>(B . "0xBD05") ; HANGUL SYLLABLE PIEUP-O-PIEUP
       (?$(C:?(B . "0xBD07") ; HANGUL SYLLABLE PIEUP-O-SIOS
       (?$(C:@(B . "0xBD09") ; HANGUL SYLLABLE PIEUP-O-IEUNG
       (?$(C:A(B . "0xBD10") ; HANGUL SYLLABLE PIEUP-WA
       (?$(C:B(B . "0xBD14") ; HANGUL SYLLABLE PIEUP-WA-NIEUN
       (?$(C:C(B . "0xBD24") ; HANGUL SYLLABLE PIEUP-WA-SSANGSIOS
       (?$(C:D(B . "0xBD2C") ; HANGUL SYLLABLE PIEUP-WAE
       (?$(C:E(B . "0xBD40") ; HANGUL SYLLABLE PIEUP-WAE-SSANGSIOS
       (?$(C:F(B . "0xBD48") ; HANGUL SYLLABLE PIEUP-OE
       (?$(C:G(B . "0xBD49") ; HANGUL SYLLABLE PIEUP-OE-KIYEOK
       (?$(C:H(B . "0xBD4C") ; HANGUL SYLLABLE PIEUP-OE-NIEUN
       (?$(C:I(B . "0xBD50") ; HANGUL SYLLABLE PIEUP-OE-RIEUL
       (?$(C:J(B . "0xBD58") ; HANGUL SYLLABLE PIEUP-OE-MIEUM
       (?$(C:K(B . "0xBD59") ; HANGUL SYLLABLE PIEUP-OE-PIEUP
       (?$(C:L(B . "0xBD64") ; HANGUL SYLLABLE PIEUP-YO
       (?$(C:M(B . "0xBD68") ; HANGUL SYLLABLE PIEUP-YO-NIEUN
       (?$(C:N(B . "0xBD80") ; HANGUL SYLLABLE PIEUP-U
       (?$(C:O(B . "0xBD81") ; HANGUL SYLLABLE PIEUP-U-KIYEOK
       (?$(C:P(B . "0xBD84") ; HANGUL SYLLABLE PIEUP-U-NIEUN
       (?$(C:Q(B . "0xBD87") ; HANGUL SYLLABLE PIEUP-U-TIKEUT
       (?$(C:R(B . "0xBD88") ; HANGUL SYLLABLE PIEUP-U-RIEUL
       (?$(C:S(B . "0xBD89") ; HANGUL SYLLABLE PIEUP-U-RIEULKIYEOK
       (?$(C:T(B . "0xBD8A") ; HANGUL SYLLABLE PIEUP-U-RIEULMIEUM
       (?$(C:U(B . "0xBD90") ; HANGUL SYLLABLE PIEUP-U-MIEUM
       (?$(C:V(B . "0xBD91") ; HANGUL SYLLABLE PIEUP-U-PIEUP
       (?$(C:W(B . "0xBD93") ; HANGUL SYLLABLE PIEUP-U-SIOS
       (?$(C:X(B . "0xBD95") ; HANGUL SYLLABLE PIEUP-U-IEUNG
       (?$(C:Y(B . "0xBD99") ; HANGUL SYLLABLE PIEUP-U-THIEUTH
       (?$(C:Z(B . "0xBD9A") ; HANGUL SYLLABLE PIEUP-U-PHIEUPH
       (?$(C:[(B . "0xBD9C") ; HANGUL SYLLABLE PIEUP-WEO
       (?$(C:\(B . "0xBDA4") ; HANGUL SYLLABLE PIEUP-WEO-RIEUL
       (?$(C:](B . "0xBDB0") ; HANGUL SYLLABLE PIEUP-WEO-SSANGSIOS
       (?$(C:^(B . "0xBDB8") ; HANGUL SYLLABLE PIEUP-WE
       (?$(C:_(B . "0xBDD4") ; HANGUL SYLLABLE PIEUP-WI
       (?$(C:`(B . "0xBDD5") ; HANGUL SYLLABLE PIEUP-WI-KIYEOK
       (?$(C:a(B . "0xBDD8") ; HANGUL SYLLABLE PIEUP-WI-NIEUN
       (?$(C:b(B . "0xBDDC") ; HANGUL SYLLABLE PIEUP-WI-RIEUL
       (?$(C:c(B . "0xBDE9") ; HANGUL SYLLABLE PIEUP-WI-IEUNG
       (?$(C:d(B . "0xBDF0") ; HANGUL SYLLABLE PIEUP-YU
       (?$(C:e(B . "0xBDF4") ; HANGUL SYLLABLE PIEUP-YU-NIEUN
       (?$(C:f(B . "0xBDF8") ; HANGUL SYLLABLE PIEUP-YU-RIEUL
       (?$(C:g(B . "0xBE00") ; HANGUL SYLLABLE PIEUP-YU-MIEUM
       (?$(C:h(B . "0xBE03") ; HANGUL SYLLABLE PIEUP-YU-SIOS
       (?$(C:i(B . "0xBE05") ; HANGUL SYLLABLE PIEUP-YU-IEUNG
       (?$(C:j(B . "0xBE0C") ; HANGUL SYLLABLE PIEUP-EU
       (?$(C:k(B . "0xBE0D") ; HANGUL SYLLABLE PIEUP-EU-KIYEOK
       (?$(C:l(B . "0xBE10") ; HANGUL SYLLABLE PIEUP-EU-NIEUN
       (?$(C:m(B . "0xBE14") ; HANGUL SYLLABLE PIEUP-EU-RIEUL
       (?$(C:n(B . "0xBE1C") ; HANGUL SYLLABLE PIEUP-EU-MIEUM
       (?$(C:o(B . "0xBE1D") ; HANGUL SYLLABLE PIEUP-EU-PIEUP
       (?$(C:p(B . "0xBE1F") ; HANGUL SYLLABLE PIEUP-EU-SIOS
       (?$(C:q(B . "0xBE44") ; HANGUL SYLLABLE PIEUP-I
       (?$(C:r(B . "0xBE45") ; HANGUL SYLLABLE PIEUP-I-KIYEOK
       (?$(C:s(B . "0xBE48") ; HANGUL SYLLABLE PIEUP-I-NIEUN
       (?$(C:t(B . "0xBE4C") ; HANGUL SYLLABLE PIEUP-I-RIEUL
       (?$(C:u(B . "0xBE4E") ; HANGUL SYLLABLE PIEUP-I-RIEULMIEUM
       (?$(C:v(B . "0xBE54") ; HANGUL SYLLABLE PIEUP-I-MIEUM
       (?$(C:w(B . "0xBE55") ; HANGUL SYLLABLE PIEUP-I-PIEUP
       (?$(C:x(B . "0xBE57") ; HANGUL SYLLABLE PIEUP-I-SIOS
       (?$(C:y(B . "0xBE59") ; HANGUL SYLLABLE PIEUP-I-IEUNG
       (?$(C:z(B . "0xBE5A") ; HANGUL SYLLABLE PIEUP-I-CIEUC
       (?$(C:{(B . "0xBE5B") ; HANGUL SYLLABLE PIEUP-I-CHIEUCH
       (?$(C:|(B . "0xBE60") ; HANGUL SYLLABLE SSANGPIEUP-A
       (?$(C:}(B . "0xBE61") ; HANGUL SYLLABLE SSANGPIEUP-A-KIYEOK
       (?$(C:~(B . "0xBE64") ; HANGUL SYLLABLE SSANGPIEUP-A-NIEUN
       (?$(C;!(B . "0xBE68") ; HANGUL SYLLABLE SSANGPIEUP-A-RIEUL
       (?$(C;"(B . "0xBE6A") ; HANGUL SYLLABLE SSANGPIEUP-A-RIEULMIEUM
       (?$(C;#(B . "0xBE70") ; HANGUL SYLLABLE SSANGPIEUP-A-MIEUM
       (?$(C;$(B . "0xBE71") ; HANGUL SYLLABLE SSANGPIEUP-A-PIEUP
       (?$(C;%(B . "0xBE73") ; HANGUL SYLLABLE SSANGPIEUP-A-SIOS
       (?$(C;&(B . "0xBE74") ; HANGUL SYLLABLE SSANGPIEUP-A-SSANGSIOS
       (?$(C;'(B . "0xBE75") ; HANGUL SYLLABLE SSANGPIEUP-A-IEUNG
       (?$(C;((B . "0xBE7B") ; HANGUL SYLLABLE SSANGPIEUP-A-HIEUH
       (?$(C;)(B . "0xBE7C") ; HANGUL SYLLABLE SSANGPIEUP-AE
       (?$(C;*(B . "0xBE7D") ; HANGUL SYLLABLE SSANGPIEUP-AE-KIYEOK
       (?$(C;+(B . "0xBE80") ; HANGUL SYLLABLE SSANGPIEUP-AE-NIEUN
       (?$(C;,(B . "0xBE84") ; HANGUL SYLLABLE SSANGPIEUP-AE-RIEUL
       (?$(C;-(B . "0xBE8C") ; HANGUL SYLLABLE SSANGPIEUP-AE-MIEUM
       (?$(C;.(B . "0xBE8D") ; HANGUL SYLLABLE SSANGPIEUP-AE-PIEUP
       (?$(C;/(B . "0xBE8F") ; HANGUL SYLLABLE SSANGPIEUP-AE-SIOS
       (?$(C;0(B . "0xBE90") ; HANGUL SYLLABLE SSANGPIEUP-AE-SSANGSIOS
       (?$(C;1(B . "0xBE91") ; HANGUL SYLLABLE SSANGPIEUP-AE-IEUNG
       (?$(C;2(B . "0xBE98") ; HANGUL SYLLABLE SSANGPIEUP-YA
       (?$(C;3(B . "0xBE99") ; HANGUL SYLLABLE SSANGPIEUP-YA-KIYEOK
       (?$(C;4(B . "0xBEA8") ; HANGUL SYLLABLE SSANGPIEUP-YA-MIEUM
       (?$(C;5(B . "0xBED0") ; HANGUL SYLLABLE SSANGPIEUP-EO
       (?$(C;6(B . "0xBED1") ; HANGUL SYLLABLE SSANGPIEUP-EO-KIYEOK
       (?$(C;7(B . "0xBED4") ; HANGUL SYLLABLE SSANGPIEUP-EO-NIEUN
       (?$(C;8(B . "0xBED7") ; HANGUL SYLLABLE SSANGPIEUP-EO-TIKEUT
       (?$(C;9(B . "0xBED8") ; HANGUL SYLLABLE SSANGPIEUP-EO-RIEUL
       (?$(C;:(B . "0xBEE0") ; HANGUL SYLLABLE SSANGPIEUP-EO-MIEUM
       (?$(C;;(B . "0xBEE3") ; HANGUL SYLLABLE SSANGPIEUP-EO-SIOS
       (?$(C;<(B . "0xBEE4") ; HANGUL SYLLABLE SSANGPIEUP-EO-SSANGSIOS
       (?$(C;=(B . "0xBEE5") ; HANGUL SYLLABLE SSANGPIEUP-EO-IEUNG
       (?$(C;>(B . "0xBEEC") ; HANGUL SYLLABLE SSANGPIEUP-E
       (?$(C;?(B . "0xBF01") ; HANGUL SYLLABLE SSANGPIEUP-E-IEUNG
       (?$(C;@(B . "0xBF08") ; HANGUL SYLLABLE SSANGPIEUP-YEO
       (?$(C;A(B . "0xBF09") ; HANGUL SYLLABLE SSANGPIEUP-YEO-KIYEOK
       (?$(C;B(B . "0xBF18") ; HANGUL SYLLABLE SSANGPIEUP-YEO-MIEUM
       (?$(C;C(B . "0xBF19") ; HANGUL SYLLABLE SSANGPIEUP-YEO-PIEUP
       (?$(C;D(B . "0xBF1B") ; HANGUL SYLLABLE SSANGPIEUP-YEO-SIOS
       (?$(C;E(B . "0xBF1C") ; HANGUL SYLLABLE SSANGPIEUP-YEO-SSANGSIOS
       (?$(C;F(B . "0xBF1D") ; HANGUL SYLLABLE SSANGPIEUP-YEO-IEUNG
       (?$(C;G(B . "0xBF40") ; HANGUL SYLLABLE SSANGPIEUP-O
       (?$(C;H(B . "0xBF41") ; HANGUL SYLLABLE SSANGPIEUP-O-KIYEOK
       (?$(C;I(B . "0xBF44") ; HANGUL SYLLABLE SSANGPIEUP-O-NIEUN
       (?$(C;J(B . "0xBF48") ; HANGUL SYLLABLE SSANGPIEUP-O-RIEUL
       (?$(C;K(B . "0xBF50") ; HANGUL SYLLABLE SSANGPIEUP-O-MIEUM
       (?$(C;L(B . "0xBF51") ; HANGUL SYLLABLE SSANGPIEUP-O-PIEUP
       (?$(C;M(B . "0xBF55") ; HANGUL SYLLABLE SSANGPIEUP-O-IEUNG
       (?$(C;N(B . "0xBF94") ; HANGUL SYLLABLE SSANGPIEUP-OE
       (?$(C;O(B . "0xBFB0") ; HANGUL SYLLABLE SSANGPIEUP-YO
       (?$(C;P(B . "0xBFC5") ; HANGUL SYLLABLE SSANGPIEUP-YO-IEUNG
       (?$(C;Q(B . "0xBFCC") ; HANGUL SYLLABLE SSANGPIEUP-U
       (?$(C;R(B . "0xBFCD") ; HANGUL SYLLABLE SSANGPIEUP-U-KIYEOK
       (?$(C;S(B . "0xBFD0") ; HANGUL SYLLABLE SSANGPIEUP-U-NIEUN
       (?$(C;T(B . "0xBFD4") ; HANGUL SYLLABLE SSANGPIEUP-U-RIEUL
       (?$(C;U(B . "0xBFDC") ; HANGUL SYLLABLE SSANGPIEUP-U-MIEUM
       (?$(C;V(B . "0xBFDF") ; HANGUL SYLLABLE SSANGPIEUP-U-SIOS
       (?$(C;W(B . "0xBFE1") ; HANGUL SYLLABLE SSANGPIEUP-U-IEUNG
       (?$(C;X(B . "0xC03C") ; HANGUL SYLLABLE SSANGPIEUP-YU
       (?$(C;Y(B . "0xC051") ; HANGUL SYLLABLE SSANGPIEUP-YU-IEUNG
       (?$(C;Z(B . "0xC058") ; HANGUL SYLLABLE SSANGPIEUP-EU
       (?$(C;[(B . "0xC05C") ; HANGUL SYLLABLE SSANGPIEUP-EU-NIEUN
       (?$(C;\(B . "0xC060") ; HANGUL SYLLABLE SSANGPIEUP-EU-RIEUL
       (?$(C;](B . "0xC068") ; HANGUL SYLLABLE SSANGPIEUP-EU-MIEUM
       (?$(C;^(B . "0xC069") ; HANGUL SYLLABLE SSANGPIEUP-EU-PIEUP
       (?$(C;_(B . "0xC090") ; HANGUL SYLLABLE SSANGPIEUP-I
       (?$(C;`(B . "0xC091") ; HANGUL SYLLABLE SSANGPIEUP-I-KIYEOK
       (?$(C;a(B . "0xC094") ; HANGUL SYLLABLE SSANGPIEUP-I-NIEUN
       (?$(C;b(B . "0xC098") ; HANGUL SYLLABLE SSANGPIEUP-I-RIEUL
       (?$(C;c(B . "0xC0A0") ; HANGUL SYLLABLE SSANGPIEUP-I-MIEUM
       (?$(C;d(B . "0xC0A1") ; HANGUL SYLLABLE SSANGPIEUP-I-PIEUP
       (?$(C;e(B . "0xC0A3") ; HANGUL SYLLABLE SSANGPIEUP-I-SIOS
       (?$(C;f(B . "0xC0A5") ; HANGUL SYLLABLE SSANGPIEUP-I-IEUNG
       (?$(C;g(B . "0xC0AC") ; HANGUL SYLLABLE SIOS-A
       (?$(C;h(B . "0xC0AD") ; HANGUL SYLLABLE SIOS-A-KIYEOK
       (?$(C;i(B . "0xC0AF") ; HANGUL SYLLABLE SIOS-A-KIYEOKSIOS
       (?$(C;j(B . "0xC0B0") ; HANGUL SYLLABLE SIOS-A-NIEUN
       (?$(C;k(B . "0xC0B3") ; HANGUL SYLLABLE SIOS-A-TIKEUT
       (?$(C;l(B . "0xC0B4") ; HANGUL SYLLABLE SIOS-A-RIEUL
       (?$(C;m(B . "0xC0B5") ; HANGUL SYLLABLE SIOS-A-RIEULKIYEOK
       (?$(C;n(B . "0xC0B6") ; HANGUL SYLLABLE SIOS-A-RIEULMIEUM
       (?$(C;o(B . "0xC0BC") ; HANGUL SYLLABLE SIOS-A-MIEUM
       (?$(C;p(B . "0xC0BD") ; HANGUL SYLLABLE SIOS-A-PIEUP
       (?$(C;q(B . "0xC0BF") ; HANGUL SYLLABLE SIOS-A-SIOS
       (?$(C;r(B . "0xC0C0") ; HANGUL SYLLABLE SIOS-A-SSANGSIOS
       (?$(C;s(B . "0xC0C1") ; HANGUL SYLLABLE SIOS-A-IEUNG
       (?$(C;t(B . "0xC0C5") ; HANGUL SYLLABLE SIOS-A-THIEUTH
       (?$(C;u(B . "0xC0C8") ; HANGUL SYLLABLE SIOS-AE
       (?$(C;v(B . "0xC0C9") ; HANGUL SYLLABLE SIOS-AE-KIYEOK
       (?$(C;w(B . "0xC0CC") ; HANGUL SYLLABLE SIOS-AE-NIEUN
       (?$(C;x(B . "0xC0D0") ; HANGUL SYLLABLE SIOS-AE-RIEUL
       (?$(C;y(B . "0xC0D8") ; HANGUL SYLLABLE SIOS-AE-MIEUM
       (?$(C;z(B . "0xC0D9") ; HANGUL SYLLABLE SIOS-AE-PIEUP
       (?$(C;{(B . "0xC0DB") ; HANGUL SYLLABLE SIOS-AE-SIOS
       (?$(C;|(B . "0xC0DC") ; HANGUL SYLLABLE SIOS-AE-SSANGSIOS
       (?$(C;}(B . "0xC0DD") ; HANGUL SYLLABLE SIOS-AE-IEUNG
       (?$(C;~(B . "0xC0E4") ; HANGUL SYLLABLE SIOS-YA
       (?$(C<!(B . "0xC0E5") ; HANGUL SYLLABLE SIOS-YA-KIYEOK
       (?$(C<"(B . "0xC0E8") ; HANGUL SYLLABLE SIOS-YA-NIEUN
       (?$(C<#(B . "0xC0EC") ; HANGUL SYLLABLE SIOS-YA-RIEUL
       (?$(C<$(B . "0xC0F4") ; HANGUL SYLLABLE SIOS-YA-MIEUM
       (?$(C<%(B . "0xC0F5") ; HANGUL SYLLABLE SIOS-YA-PIEUP
       (?$(C<&(B . "0xC0F7") ; HANGUL SYLLABLE SIOS-YA-SIOS
       (?$(C<'(B . "0xC0F9") ; HANGUL SYLLABLE SIOS-YA-IEUNG
       (?$(C<((B . "0xC100") ; HANGUL SYLLABLE SIOS-YAE
       (?$(C<)(B . "0xC104") ; HANGUL SYLLABLE SIOS-YAE-NIEUN
       (?$(C<*(B . "0xC108") ; HANGUL SYLLABLE SIOS-YAE-RIEUL
       (?$(C<+(B . "0xC110") ; HANGUL SYLLABLE SIOS-YAE-MIEUM
       (?$(C<,(B . "0xC115") ; HANGUL SYLLABLE SIOS-YAE-IEUNG
       (?$(C<-(B . "0xC11C") ; HANGUL SYLLABLE SIOS-EO
       (?$(C<.(B . "0xC11D") ; HANGUL SYLLABLE SIOS-EO-KIYEOK
       (?$(C</(B . "0xC11E") ; HANGUL SYLLABLE SIOS-EO-SSANGKIYEOK
       (?$(C<0(B . "0xC11F") ; HANGUL SYLLABLE SIOS-EO-KIYEOKSIOS
       (?$(C<1(B . "0xC120") ; HANGUL SYLLABLE SIOS-EO-NIEUN
       (?$(C<2(B . "0xC123") ; HANGUL SYLLABLE SIOS-EO-TIKEUT
       (?$(C<3(B . "0xC124") ; HANGUL SYLLABLE SIOS-EO-RIEUL
       (?$(C<4(B . "0xC126") ; HANGUL SYLLABLE SIOS-EO-RIEULMIEUM
       (?$(C<5(B . "0xC127") ; HANGUL SYLLABLE SIOS-EO-RIEULPIEUP
       (?$(C<6(B . "0xC12C") ; HANGUL SYLLABLE SIOS-EO-MIEUM
       (?$(C<7(B . "0xC12D") ; HANGUL SYLLABLE SIOS-EO-PIEUP
       (?$(C<8(B . "0xC12F") ; HANGUL SYLLABLE SIOS-EO-SIOS
       (?$(C<9(B . "0xC130") ; HANGUL SYLLABLE SIOS-EO-SSANGSIOS
       (?$(C<:(B . "0xC131") ; HANGUL SYLLABLE SIOS-EO-IEUNG
       (?$(C<;(B . "0xC136") ; HANGUL SYLLABLE SIOS-EO-PHIEUPH
       (?$(C<<(B . "0xC138") ; HANGUL SYLLABLE SIOS-E
       (?$(C<=(B . "0xC139") ; HANGUL SYLLABLE SIOS-E-KIYEOK
       (?$(C<>(B . "0xC13C") ; HANGUL SYLLABLE SIOS-E-NIEUN
       (?$(C<?(B . "0xC140") ; HANGUL SYLLABLE SIOS-E-RIEUL
       (?$(C<@(B . "0xC148") ; HANGUL SYLLABLE SIOS-E-MIEUM
       (?$(C<A(B . "0xC149") ; HANGUL SYLLABLE SIOS-E-PIEUP
       (?$(C<B(B . "0xC14B") ; HANGUL SYLLABLE SIOS-E-SIOS
       (?$(C<C(B . "0xC14C") ; HANGUL SYLLABLE SIOS-E-SSANGSIOS
       (?$(C<D(B . "0xC14D") ; HANGUL SYLLABLE SIOS-E-IEUNG
       (?$(C<E(B . "0xC154") ; HANGUL SYLLABLE SIOS-YEO
       (?$(C<F(B . "0xC155") ; HANGUL SYLLABLE SIOS-YEO-KIYEOK
       (?$(C<G(B . "0xC158") ; HANGUL SYLLABLE SIOS-YEO-NIEUN
       (?$(C<H(B . "0xC15C") ; HANGUL SYLLABLE SIOS-YEO-RIEUL
       (?$(C<I(B . "0xC164") ; HANGUL SYLLABLE SIOS-YEO-MIEUM
       (?$(C<J(B . "0xC165") ; HANGUL SYLLABLE SIOS-YEO-PIEUP
       (?$(C<K(B . "0xC167") ; HANGUL SYLLABLE SIOS-YEO-SIOS
       (?$(C<L(B . "0xC168") ; HANGUL SYLLABLE SIOS-YEO-SSANGSIOS
       (?$(C<M(B . "0xC169") ; HANGUL SYLLABLE SIOS-YEO-IEUNG
       (?$(C<N(B . "0xC170") ; HANGUL SYLLABLE SIOS-YE
       (?$(C<O(B . "0xC174") ; HANGUL SYLLABLE SIOS-YE-NIEUN
       (?$(C<P(B . "0xC178") ; HANGUL SYLLABLE SIOS-YE-RIEUL
       (?$(C<Q(B . "0xC185") ; HANGUL SYLLABLE SIOS-YE-IEUNG
       (?$(C<R(B . "0xC18C") ; HANGUL SYLLABLE SIOS-O
       (?$(C<S(B . "0xC18D") ; HANGUL SYLLABLE SIOS-O-KIYEOK
       (?$(C<T(B . "0xC18E") ; HANGUL SYLLABLE SIOS-O-SSANGKIYEOK
       (?$(C<U(B . "0xC190") ; HANGUL SYLLABLE SIOS-O-NIEUN
       (?$(C<V(B . "0xC194") ; HANGUL SYLLABLE SIOS-O-RIEUL
       (?$(C<W(B . "0xC196") ; HANGUL SYLLABLE SIOS-O-RIEULMIEUM
       (?$(C<X(B . "0xC19C") ; HANGUL SYLLABLE SIOS-O-MIEUM
       (?$(C<Y(B . "0xC19D") ; HANGUL SYLLABLE SIOS-O-PIEUP
       (?$(C<Z(B . "0xC19F") ; HANGUL SYLLABLE SIOS-O-SIOS
       (?$(C<[(B . "0xC1A1") ; HANGUL SYLLABLE SIOS-O-IEUNG
       (?$(C<\(B . "0xC1A5") ; HANGUL SYLLABLE SIOS-O-THIEUTH
       (?$(C<](B . "0xC1A8") ; HANGUL SYLLABLE SIOS-WA
       (?$(C<^(B . "0xC1A9") ; HANGUL SYLLABLE SIOS-WA-KIYEOK
       (?$(C<_(B . "0xC1AC") ; HANGUL SYLLABLE SIOS-WA-NIEUN
       (?$(C<`(B . "0xC1B0") ; HANGUL SYLLABLE SIOS-WA-RIEUL
       (?$(C<a(B . "0xC1BD") ; HANGUL SYLLABLE SIOS-WA-IEUNG
       (?$(C<b(B . "0xC1C4") ; HANGUL SYLLABLE SIOS-WAE
       (?$(C<c(B . "0xC1C8") ; HANGUL SYLLABLE SIOS-WAE-NIEUN
       (?$(C<d(B . "0xC1CC") ; HANGUL SYLLABLE SIOS-WAE-RIEUL
       (?$(C<e(B . "0xC1D4") ; HANGUL SYLLABLE SIOS-WAE-MIEUM
       (?$(C<f(B . "0xC1D7") ; HANGUL SYLLABLE SIOS-WAE-SIOS
       (?$(C<g(B . "0xC1D8") ; HANGUL SYLLABLE SIOS-WAE-SSANGSIOS
       (?$(C<h(B . "0xC1E0") ; HANGUL SYLLABLE SIOS-OE
       (?$(C<i(B . "0xC1E4") ; HANGUL SYLLABLE SIOS-OE-NIEUN
       (?$(C<j(B . "0xC1E8") ; HANGUL SYLLABLE SIOS-OE-RIEUL
       (?$(C<k(B . "0xC1F0") ; HANGUL SYLLABLE SIOS-OE-MIEUM
       (?$(C<l(B . "0xC1F1") ; HANGUL SYLLABLE SIOS-OE-PIEUP
       (?$(C<m(B . "0xC1F3") ; HANGUL SYLLABLE SIOS-OE-SIOS
       (?$(C<n(B . "0xC1FC") ; HANGUL SYLLABLE SIOS-YO
       (?$(C<o(B . "0xC1FD") ; HANGUL SYLLABLE SIOS-YO-KIYEOK
       (?$(C<p(B . "0xC200") ; HANGUL SYLLABLE SIOS-YO-NIEUN
       (?$(C<q(B . "0xC204") ; HANGUL SYLLABLE SIOS-YO-RIEUL
       (?$(C<r(B . "0xC20C") ; HANGUL SYLLABLE SIOS-YO-MIEUM
       (?$(C<s(B . "0xC20D") ; HANGUL SYLLABLE SIOS-YO-PIEUP
       (?$(C<t(B . "0xC20F") ; HANGUL SYLLABLE SIOS-YO-SIOS
       (?$(C<u(B . "0xC211") ; HANGUL SYLLABLE SIOS-YO-IEUNG
       (?$(C<v(B . "0xC218") ; HANGUL SYLLABLE SIOS-U
       (?$(C<w(B . "0xC219") ; HANGUL SYLLABLE SIOS-U-KIYEOK
       (?$(C<x(B . "0xC21C") ; HANGUL SYLLABLE SIOS-U-NIEUN
       (?$(C<y(B . "0xC21F") ; HANGUL SYLLABLE SIOS-U-TIKEUT
       (?$(C<z(B . "0xC220") ; HANGUL SYLLABLE SIOS-U-RIEUL
       (?$(C<{(B . "0xC228") ; HANGUL SYLLABLE SIOS-U-MIEUM
       (?$(C<|(B . "0xC229") ; HANGUL SYLLABLE SIOS-U-PIEUP
       (?$(C<}(B . "0xC22B") ; HANGUL SYLLABLE SIOS-U-SIOS
       (?$(C<~(B . "0xC22D") ; HANGUL SYLLABLE SIOS-U-IEUNG
       (?$(C=!(B . "0xC22F") ; HANGUL SYLLABLE SIOS-U-CHIEUCH
       (?$(C="(B . "0xC231") ; HANGUL SYLLABLE SIOS-U-THIEUTH
       (?$(C=#(B . "0xC232") ; HANGUL SYLLABLE SIOS-U-PHIEUPH
       (?$(C=$(B . "0xC234") ; HANGUL SYLLABLE SIOS-WEO
       (?$(C=%(B . "0xC248") ; HANGUL SYLLABLE SIOS-WEO-SSANGSIOS
       (?$(C=&(B . "0xC250") ; HANGUL SYLLABLE SIOS-WE
       (?$(C='(B . "0xC251") ; HANGUL SYLLABLE SIOS-WE-KIYEOK
       (?$(C=((B . "0xC254") ; HANGUL SYLLABLE SIOS-WE-NIEUN
       (?$(C=)(B . "0xC258") ; HANGUL SYLLABLE SIOS-WE-RIEUL
       (?$(C=*(B . "0xC260") ; HANGUL SYLLABLE SIOS-WE-MIEUM
       (?$(C=+(B . "0xC265") ; HANGUL SYLLABLE SIOS-WE-IEUNG
       (?$(C=,(B . "0xC26C") ; HANGUL SYLLABLE SIOS-WI
       (?$(C=-(B . "0xC26D") ; HANGUL SYLLABLE SIOS-WI-KIYEOK
       (?$(C=.(B . "0xC270") ; HANGUL SYLLABLE SIOS-WI-NIEUN
       (?$(C=/(B . "0xC274") ; HANGUL SYLLABLE SIOS-WI-RIEUL
       (?$(C=0(B . "0xC27C") ; HANGUL SYLLABLE SIOS-WI-MIEUM
       (?$(C=1(B . "0xC27D") ; HANGUL SYLLABLE SIOS-WI-PIEUP
       (?$(C=2(B . "0xC27F") ; HANGUL SYLLABLE SIOS-WI-SIOS
       (?$(C=3(B . "0xC281") ; HANGUL SYLLABLE SIOS-WI-IEUNG
       (?$(C=4(B . "0xC288") ; HANGUL SYLLABLE SIOS-YU
       (?$(C=5(B . "0xC289") ; HANGUL SYLLABLE SIOS-YU-KIYEOK
       (?$(C=6(B . "0xC290") ; HANGUL SYLLABLE SIOS-YU-RIEUL
       (?$(C=7(B . "0xC298") ; HANGUL SYLLABLE SIOS-YU-MIEUM
       (?$(C=8(B . "0xC29B") ; HANGUL SYLLABLE SIOS-YU-SIOS
       (?$(C=9(B . "0xC29D") ; HANGUL SYLLABLE SIOS-YU-IEUNG
       (?$(C=:(B . "0xC2A4") ; HANGUL SYLLABLE SIOS-EU
       (?$(C=;(B . "0xC2A5") ; HANGUL SYLLABLE SIOS-EU-KIYEOK
       (?$(C=<(B . "0xC2A8") ; HANGUL SYLLABLE SIOS-EU-NIEUN
       (?$(C==(B . "0xC2AC") ; HANGUL SYLLABLE SIOS-EU-RIEUL
       (?$(C=>(B . "0xC2AD") ; HANGUL SYLLABLE SIOS-EU-RIEULKIYEOK
       (?$(C=?(B . "0xC2B4") ; HANGUL SYLLABLE SIOS-EU-MIEUM
       (?$(C=@(B . "0xC2B5") ; HANGUL SYLLABLE SIOS-EU-PIEUP
       (?$(C=A(B . "0xC2B7") ; HANGUL SYLLABLE SIOS-EU-SIOS
       (?$(C=B(B . "0xC2B9") ; HANGUL SYLLABLE SIOS-EU-IEUNG
       (?$(C=C(B . "0xC2DC") ; HANGUL SYLLABLE SIOS-I
       (?$(C=D(B . "0xC2DD") ; HANGUL SYLLABLE SIOS-I-KIYEOK
       (?$(C=E(B . "0xC2E0") ; HANGUL SYLLABLE SIOS-I-NIEUN
       (?$(C=F(B . "0xC2E3") ; HANGUL SYLLABLE SIOS-I-TIKEUT
       (?$(C=G(B . "0xC2E4") ; HANGUL SYLLABLE SIOS-I-RIEUL
       (?$(C=H(B . "0xC2EB") ; HANGUL SYLLABLE SIOS-I-RIEULHIEUH
       (?$(C=I(B . "0xC2EC") ; HANGUL SYLLABLE SIOS-I-MIEUM
       (?$(C=J(B . "0xC2ED") ; HANGUL SYLLABLE SIOS-I-PIEUP
       (?$(C=K(B . "0xC2EF") ; HANGUL SYLLABLE SIOS-I-SIOS
       (?$(C=L(B . "0xC2F1") ; HANGUL SYLLABLE SIOS-I-IEUNG
       (?$(C=M(B . "0xC2F6") ; HANGUL SYLLABLE SIOS-I-PHIEUPH
       (?$(C=N(B . "0xC2F8") ; HANGUL SYLLABLE SSANGSIOS-A
       (?$(C=O(B . "0xC2F9") ; HANGUL SYLLABLE SSANGSIOS-A-KIYEOK
       (?$(C=P(B . "0xC2FB") ; HANGUL SYLLABLE SSANGSIOS-A-KIYEOKSIOS
       (?$(C=Q(B . "0xC2FC") ; HANGUL SYLLABLE SSANGSIOS-A-NIEUN
       (?$(C=R(B . "0xC300") ; HANGUL SYLLABLE SSANGSIOS-A-RIEUL
       (?$(C=S(B . "0xC308") ; HANGUL SYLLABLE SSANGSIOS-A-MIEUM
       (?$(C=T(B . "0xC309") ; HANGUL SYLLABLE SSANGSIOS-A-PIEUP
       (?$(C=U(B . "0xC30C") ; HANGUL SYLLABLE SSANGSIOS-A-SSANGSIOS
       (?$(C=V(B . "0xC30D") ; HANGUL SYLLABLE SSANGSIOS-A-IEUNG
       (?$(C=W(B . "0xC313") ; HANGUL SYLLABLE SSANGSIOS-A-HIEUH
       (?$(C=X(B . "0xC314") ; HANGUL SYLLABLE SSANGSIOS-AE
       (?$(C=Y(B . "0xC315") ; HANGUL SYLLABLE SSANGSIOS-AE-KIYEOK
       (?$(C=Z(B . "0xC318") ; HANGUL SYLLABLE SSANGSIOS-AE-NIEUN
       (?$(C=[(B . "0xC31C") ; HANGUL SYLLABLE SSANGSIOS-AE-RIEUL
       (?$(C=\(B . "0xC324") ; HANGUL SYLLABLE SSANGSIOS-AE-MIEUM
       (?$(C=](B . "0xC325") ; HANGUL SYLLABLE SSANGSIOS-AE-PIEUP
       (?$(C=^(B . "0xC328") ; HANGUL SYLLABLE SSANGSIOS-AE-SSANGSIOS
       (?$(C=_(B . "0xC329") ; HANGUL SYLLABLE SSANGSIOS-AE-IEUNG
       (?$(C=`(B . "0xC345") ; HANGUL SYLLABLE SSANGSIOS-YA-IEUNG
       (?$(C=a(B . "0xC368") ; HANGUL SYLLABLE SSANGSIOS-EO
       (?$(C=b(B . "0xC369") ; HANGUL SYLLABLE SSANGSIOS-EO-KIYEOK
       (?$(C=c(B . "0xC36C") ; HANGUL SYLLABLE SSANGSIOS-EO-NIEUN
       (?$(C=d(B . "0xC370") ; HANGUL SYLLABLE SSANGSIOS-EO-RIEUL
       (?$(C=e(B . "0xC372") ; HANGUL SYLLABLE SSANGSIOS-EO-RIEULMIEUM
       (?$(C=f(B . "0xC378") ; HANGUL SYLLABLE SSANGSIOS-EO-MIEUM
       (?$(C=g(B . "0xC379") ; HANGUL SYLLABLE SSANGSIOS-EO-PIEUP
       (?$(C=h(B . "0xC37C") ; HANGUL SYLLABLE SSANGSIOS-EO-SSANGSIOS
       (?$(C=i(B . "0xC37D") ; HANGUL SYLLABLE SSANGSIOS-EO-IEUNG
       (?$(C=j(B . "0xC384") ; HANGUL SYLLABLE SSANGSIOS-E
       (?$(C=k(B . "0xC388") ; HANGUL SYLLABLE SSANGSIOS-E-NIEUN
       (?$(C=l(B . "0xC38C") ; HANGUL SYLLABLE SSANGSIOS-E-RIEUL
       (?$(C=m(B . "0xC3C0") ; HANGUL SYLLABLE SSANGSIOS-YE-NIEUN
       (?$(C=n(B . "0xC3D8") ; HANGUL SYLLABLE SSANGSIOS-O
       (?$(C=o(B . "0xC3D9") ; HANGUL SYLLABLE SSANGSIOS-O-KIYEOK
       (?$(C=p(B . "0xC3DC") ; HANGUL SYLLABLE SSANGSIOS-O-NIEUN
       (?$(C=q(B . "0xC3DF") ; HANGUL SYLLABLE SSANGSIOS-O-TIKEUT
       (?$(C=r(B . "0xC3E0") ; HANGUL SYLLABLE SSANGSIOS-O-RIEUL
       (?$(C=s(B . "0xC3E2") ; HANGUL SYLLABLE SSANGSIOS-O-RIEULMIEUM
       (?$(C=t(B . "0xC3E8") ; HANGUL SYLLABLE SSANGSIOS-O-MIEUM
       (?$(C=u(B . "0xC3E9") ; HANGUL SYLLABLE SSANGSIOS-O-PIEUP
       (?$(C=v(B . "0xC3ED") ; HANGUL SYLLABLE SSANGSIOS-O-IEUNG
       (?$(C=w(B . "0xC3F4") ; HANGUL SYLLABLE SSANGSIOS-WA
       (?$(C=x(B . "0xC3F5") ; HANGUL SYLLABLE SSANGSIOS-WA-KIYEOK
       (?$(C=y(B . "0xC3F8") ; HANGUL SYLLABLE SSANGSIOS-WA-NIEUN
       (?$(C=z(B . "0xC408") ; HANGUL SYLLABLE SSANGSIOS-WA-SSANGSIOS
       (?$(C={(B . "0xC410") ; HANGUL SYLLABLE SSANGSIOS-WAE
       (?$(C=|(B . "0xC424") ; HANGUL SYLLABLE SSANGSIOS-WAE-SSANGSIOS
       (?$(C=}(B . "0xC42C") ; HANGUL SYLLABLE SSANGSIOS-OE
       (?$(C=~(B . "0xC430") ; HANGUL SYLLABLE SSANGSIOS-OE-NIEUN
       (?$(C>!(B . "0xC434") ; HANGUL SYLLABLE SSANGSIOS-OE-RIEUL
       (?$(C>"(B . "0xC43C") ; HANGUL SYLLABLE SSANGSIOS-OE-MIEUM
       (?$(C>#(B . "0xC43D") ; HANGUL SYLLABLE SSANGSIOS-OE-PIEUP
       (?$(C>$(B . "0xC448") ; HANGUL SYLLABLE SSANGSIOS-YO
       (?$(C>%(B . "0xC464") ; HANGUL SYLLABLE SSANGSIOS-U
       (?$(C>&(B . "0xC465") ; HANGUL SYLLABLE SSANGSIOS-U-KIYEOK
       (?$(C>'(B . "0xC468") ; HANGUL SYLLABLE SSANGSIOS-U-NIEUN
       (?$(C>((B . "0xC46C") ; HANGUL SYLLABLE SSANGSIOS-U-RIEUL
       (?$(C>)(B . "0xC474") ; HANGUL SYLLABLE SSANGSIOS-U-MIEUM
       (?$(C>*(B . "0xC475") ; HANGUL SYLLABLE SSANGSIOS-U-PIEUP
       (?$(C>+(B . "0xC479") ; HANGUL SYLLABLE SSANGSIOS-U-IEUNG
       (?$(C>,(B . "0xC480") ; HANGUL SYLLABLE SSANGSIOS-WEO
       (?$(C>-(B . "0xC494") ; HANGUL SYLLABLE SSANGSIOS-WEO-SSANGSIOS
       (?$(C>.(B . "0xC49C") ; HANGUL SYLLABLE SSANGSIOS-WE
       (?$(C>/(B . "0xC4B8") ; HANGUL SYLLABLE SSANGSIOS-WI
       (?$(C>0(B . "0xC4BC") ; HANGUL SYLLABLE SSANGSIOS-WI-NIEUN
       (?$(C>1(B . "0xC4E9") ; HANGUL SYLLABLE SSANGSIOS-YU-IEUNG
       (?$(C>2(B . "0xC4F0") ; HANGUL SYLLABLE SSANGSIOS-EU
       (?$(C>3(B . "0xC4F1") ; HANGUL SYLLABLE SSANGSIOS-EU-KIYEOK
       (?$(C>4(B . "0xC4F4") ; HANGUL SYLLABLE SSANGSIOS-EU-NIEUN
       (?$(C>5(B . "0xC4F8") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEUL
       (?$(C>6(B . "0xC4FA") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEULMIEUM
       (?$(C>7(B . "0xC4FF") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEULHIEUH
       (?$(C>8(B . "0xC500") ; HANGUL SYLLABLE SSANGSIOS-EU-MIEUM
       (?$(C>9(B . "0xC501") ; HANGUL SYLLABLE SSANGSIOS-EU-PIEUP
       (?$(C>:(B . "0xC50C") ; HANGUL SYLLABLE SSANGSIOS-YI
       (?$(C>;(B . "0xC510") ; HANGUL SYLLABLE SSANGSIOS-YI-NIEUN
       (?$(C><(B . "0xC514") ; HANGUL SYLLABLE SSANGSIOS-YI-RIEUL
       (?$(C>=(B . "0xC51C") ; HANGUL SYLLABLE SSANGSIOS-YI-MIEUM
       (?$(C>>(B . "0xC528") ; HANGUL SYLLABLE SSANGSIOS-I
       (?$(C>?(B . "0xC529") ; HANGUL SYLLABLE SSANGSIOS-I-KIYEOK
       (?$(C>@(B . "0xC52C") ; HANGUL SYLLABLE SSANGSIOS-I-NIEUN
       (?$(C>A(B . "0xC530") ; HANGUL SYLLABLE SSANGSIOS-I-RIEUL
       (?$(C>B(B . "0xC538") ; HANGUL SYLLABLE SSANGSIOS-I-MIEUM
       (?$(C>C(B . "0xC539") ; HANGUL SYLLABLE SSANGSIOS-I-PIEUP
       (?$(C>D(B . "0xC53B") ; HANGUL SYLLABLE SSANGSIOS-I-SIOS
       (?$(C>E(B . "0xC53D") ; HANGUL SYLLABLE SSANGSIOS-I-IEUNG
       (?$(C>F(B . "0xC544") ; HANGUL SYLLABLE IEUNG-A
       (?$(C>G(B . "0xC545") ; HANGUL SYLLABLE IEUNG-A-KIYEOK
       (?$(C>H(B . "0xC548") ; HANGUL SYLLABLE IEUNG-A-NIEUN
       (?$(C>I(B . "0xC549") ; HANGUL SYLLABLE IEUNG-A-NIEUNCIEUC
       (?$(C>J(B . "0xC54A") ; HANGUL SYLLABLE IEUNG-A-NIEUNHIEUH
       (?$(C>K(B . "0xC54C") ; HANGUL SYLLABLE IEUNG-A-RIEUL
       (?$(C>L(B . "0xC54D") ; HANGUL SYLLABLE IEUNG-A-RIEULKIYEOK
       (?$(C>M(B . "0xC54E") ; HANGUL SYLLABLE IEUNG-A-RIEULMIEUM
       (?$(C>N(B . "0xC553") ; HANGUL SYLLABLE IEUNG-A-RIEULHIEUH
       (?$(C>O(B . "0xC554") ; HANGUL SYLLABLE IEUNG-A-MIEUM
       (?$(C>P(B . "0xC555") ; HANGUL SYLLABLE IEUNG-A-PIEUP
       (?$(C>Q(B . "0xC557") ; HANGUL SYLLABLE IEUNG-A-SIOS
       (?$(C>R(B . "0xC558") ; HANGUL SYLLABLE IEUNG-A-SSANGSIOS
       (?$(C>S(B . "0xC559") ; HANGUL SYLLABLE IEUNG-A-IEUNG
       (?$(C>T(B . "0xC55D") ; HANGUL SYLLABLE IEUNG-A-THIEUTH
       (?$(C>U(B . "0xC55E") ; HANGUL SYLLABLE IEUNG-A-PHIEUPH
       (?$(C>V(B . "0xC560") ; HANGUL SYLLABLE IEUNG-AE
       (?$(C>W(B . "0xC561") ; HANGUL SYLLABLE IEUNG-AE-KIYEOK
       (?$(C>X(B . "0xC564") ; HANGUL SYLLABLE IEUNG-AE-NIEUN
       (?$(C>Y(B . "0xC568") ; HANGUL SYLLABLE IEUNG-AE-RIEUL
       (?$(C>Z(B . "0xC570") ; HANGUL SYLLABLE IEUNG-AE-MIEUM
       (?$(C>[(B . "0xC571") ; HANGUL SYLLABLE IEUNG-AE-PIEUP
       (?$(C>\(B . "0xC573") ; HANGUL SYLLABLE IEUNG-AE-SIOS
       (?$(C>](B . "0xC574") ; HANGUL SYLLABLE IEUNG-AE-SSANGSIOS
       (?$(C>^(B . "0xC575") ; HANGUL SYLLABLE IEUNG-AE-IEUNG
       (?$(C>_(B . "0xC57C") ; HANGUL SYLLABLE IEUNG-YA
       (?$(C>`(B . "0xC57D") ; HANGUL SYLLABLE IEUNG-YA-KIYEOK
       (?$(C>a(B . "0xC580") ; HANGUL SYLLABLE IEUNG-YA-NIEUN
       (?$(C>b(B . "0xC584") ; HANGUL SYLLABLE IEUNG-YA-RIEUL
       (?$(C>c(B . "0xC587") ; HANGUL SYLLABLE IEUNG-YA-RIEULPIEUP
       (?$(C>d(B . "0xC58C") ; HANGUL SYLLABLE IEUNG-YA-MIEUM
       (?$(C>e(B . "0xC58D") ; HANGUL SYLLABLE IEUNG-YA-PIEUP
       (?$(C>f(B . "0xC58F") ; HANGUL SYLLABLE IEUNG-YA-SIOS
       (?$(C>g(B . "0xC591") ; HANGUL SYLLABLE IEUNG-YA-IEUNG
       (?$(C>h(B . "0xC595") ; HANGUL SYLLABLE IEUNG-YA-THIEUTH
       (?$(C>i(B . "0xC597") ; HANGUL SYLLABLE IEUNG-YA-HIEUH
       (?$(C>j(B . "0xC598") ; HANGUL SYLLABLE IEUNG-YAE
       (?$(C>k(B . "0xC59C") ; HANGUL SYLLABLE IEUNG-YAE-NIEUN
       (?$(C>l(B . "0xC5A0") ; HANGUL SYLLABLE IEUNG-YAE-RIEUL
       (?$(C>m(B . "0xC5A9") ; HANGUL SYLLABLE IEUNG-YAE-PIEUP
       (?$(C>n(B . "0xC5B4") ; HANGUL SYLLABLE IEUNG-EO
       (?$(C>o(B . "0xC5B5") ; HANGUL SYLLABLE IEUNG-EO-KIYEOK
       (?$(C>p(B . "0xC5B8") ; HANGUL SYLLABLE IEUNG-EO-NIEUN
       (?$(C>q(B . "0xC5B9") ; HANGUL SYLLABLE IEUNG-EO-NIEUNCIEUC
       (?$(C>r(B . "0xC5BB") ; HANGUL SYLLABLE IEUNG-EO-TIKEUT
       (?$(C>s(B . "0xC5BC") ; HANGUL SYLLABLE IEUNG-EO-RIEUL
       (?$(C>t(B . "0xC5BD") ; HANGUL SYLLABLE IEUNG-EO-RIEULKIYEOK
       (?$(C>u(B . "0xC5BE") ; HANGUL SYLLABLE IEUNG-EO-RIEULMIEUM
       (?$(C>v(B . "0xC5C4") ; HANGUL SYLLABLE IEUNG-EO-MIEUM
       (?$(C>w(B . "0xC5C5") ; HANGUL SYLLABLE IEUNG-EO-PIEUP
       (?$(C>x(B . "0xC5C6") ; HANGUL SYLLABLE IEUNG-EO-PIEUPSIOS
       (?$(C>y(B . "0xC5C7") ; HANGUL SYLLABLE IEUNG-EO-SIOS
       (?$(C>z(B . "0xC5C8") ; HANGUL SYLLABLE IEUNG-EO-SSANGSIOS
       (?$(C>{(B . "0xC5C9") ; HANGUL SYLLABLE IEUNG-EO-IEUNG
       (?$(C>|(B . "0xC5CA") ; HANGUL SYLLABLE IEUNG-EO-CIEUC
       (?$(C>}(B . "0xC5CC") ; HANGUL SYLLABLE IEUNG-EO-KHIEUKH
       (?$(C>~(B . "0xC5CE") ; HANGUL SYLLABLE IEUNG-EO-PHIEUPH
       (?$(C?!(B . "0xC5D0") ; HANGUL SYLLABLE IEUNG-E
       (?$(C?"(B . "0xC5D1") ; HANGUL SYLLABLE IEUNG-E-KIYEOK
       (?$(C?#(B . "0xC5D4") ; HANGUL SYLLABLE IEUNG-E-NIEUN
       (?$(C?$(B . "0xC5D8") ; HANGUL SYLLABLE IEUNG-E-RIEUL
       (?$(C?%(B . "0xC5E0") ; HANGUL SYLLABLE IEUNG-E-MIEUM
       (?$(C?&(B . "0xC5E1") ; HANGUL SYLLABLE IEUNG-E-PIEUP
       (?$(C?'(B . "0xC5E3") ; HANGUL SYLLABLE IEUNG-E-SIOS
       (?$(C?((B . "0xC5E5") ; HANGUL SYLLABLE IEUNG-E-IEUNG
       (?$(C?)(B . "0xC5EC") ; HANGUL SYLLABLE IEUNG-YEO
       (?$(C?*(B . "0xC5ED") ; HANGUL SYLLABLE IEUNG-YEO-KIYEOK
       (?$(C?+(B . "0xC5EE") ; HANGUL SYLLABLE IEUNG-YEO-SSANGKIYEOK
       (?$(C?,(B . "0xC5F0") ; HANGUL SYLLABLE IEUNG-YEO-NIEUN
       (?$(C?-(B . "0xC5F4") ; HANGUL SYLLABLE IEUNG-YEO-RIEUL
       (?$(C?.(B . "0xC5F6") ; HANGUL SYLLABLE IEUNG-YEO-RIEULMIEUM
       (?$(C?/(B . "0xC5F7") ; HANGUL SYLLABLE IEUNG-YEO-RIEULPIEUP
       (?$(C?0(B . "0xC5FC") ; HANGUL SYLLABLE IEUNG-YEO-MIEUM
       (?$(C?1(B . "0xC5FD") ; HANGUL SYLLABLE IEUNG-YEO-PIEUP
       (?$(C?2(B . "0xC5FE") ; HANGUL SYLLABLE IEUNG-YEO-PIEUPSIOS
       (?$(C?3(B . "0xC5FF") ; HANGUL SYLLABLE IEUNG-YEO-SIOS
       (?$(C?4(B . "0xC600") ; HANGUL SYLLABLE IEUNG-YEO-SSANGSIOS
       (?$(C?5(B . "0xC601") ; HANGUL SYLLABLE IEUNG-YEO-IEUNG
       (?$(C?6(B . "0xC605") ; HANGUL SYLLABLE IEUNG-YEO-THIEUTH
       (?$(C?7(B . "0xC606") ; HANGUL SYLLABLE IEUNG-YEO-PHIEUPH
       (?$(C?8(B . "0xC607") ; HANGUL SYLLABLE IEUNG-YEO-HIEUH
       (?$(C?9(B . "0xC608") ; HANGUL SYLLABLE IEUNG-YE
       (?$(C?:(B . "0xC60C") ; HANGUL SYLLABLE IEUNG-YE-NIEUN
       (?$(C?;(B . "0xC610") ; HANGUL SYLLABLE IEUNG-YE-RIEUL
       (?$(C?<(B . "0xC618") ; HANGUL SYLLABLE IEUNG-YE-MIEUM
       (?$(C?=(B . "0xC619") ; HANGUL SYLLABLE IEUNG-YE-PIEUP
       (?$(C?>(B . "0xC61B") ; HANGUL SYLLABLE IEUNG-YE-SIOS
       (?$(C??(B . "0xC61C") ; HANGUL SYLLABLE IEUNG-YE-SSANGSIOS
       (?$(C?@(B . "0xC624") ; HANGUL SYLLABLE IEUNG-O
       (?$(C?A(B . "0xC625") ; HANGUL SYLLABLE IEUNG-O-KIYEOK
       (?$(C?B(B . "0xC628") ; HANGUL SYLLABLE IEUNG-O-NIEUN
       (?$(C?C(B . "0xC62C") ; HANGUL SYLLABLE IEUNG-O-RIEUL
       (?$(C?D(B . "0xC62D") ; HANGUL SYLLABLE IEUNG-O-RIEULKIYEOK
       (?$(C?E(B . "0xC62E") ; HANGUL SYLLABLE IEUNG-O-RIEULMIEUM
       (?$(C?F(B . "0xC630") ; HANGUL SYLLABLE IEUNG-O-RIEULSIOS
       (?$(C?G(B . "0xC633") ; HANGUL SYLLABLE IEUNG-O-RIEULHIEUH
       (?$(C?H(B . "0xC634") ; HANGUL SYLLABLE IEUNG-O-MIEUM
       (?$(C?I(B . "0xC635") ; HANGUL SYLLABLE IEUNG-O-PIEUP
       (?$(C?J(B . "0xC637") ; HANGUL SYLLABLE IEUNG-O-SIOS
       (?$(C?K(B . "0xC639") ; HANGUL SYLLABLE IEUNG-O-IEUNG
       (?$(C?L(B . "0xC63B") ; HANGUL SYLLABLE IEUNG-O-CHIEUCH
       (?$(C?M(B . "0xC640") ; HANGUL SYLLABLE IEUNG-WA
       (?$(C?N(B . "0xC641") ; HANGUL SYLLABLE IEUNG-WA-KIYEOK
       (?$(C?O(B . "0xC644") ; HANGUL SYLLABLE IEUNG-WA-NIEUN
       (?$(C?P(B . "0xC648") ; HANGUL SYLLABLE IEUNG-WA-RIEUL
       (?$(C?Q(B . "0xC650") ; HANGUL SYLLABLE IEUNG-WA-MIEUM
       (?$(C?R(B . "0xC651") ; HANGUL SYLLABLE IEUNG-WA-PIEUP
       (?$(C?S(B . "0xC653") ; HANGUL SYLLABLE IEUNG-WA-SIOS
       (?$(C?T(B . "0xC654") ; HANGUL SYLLABLE IEUNG-WA-SSANGSIOS
       (?$(C?U(B . "0xC655") ; HANGUL SYLLABLE IEUNG-WA-IEUNG
       (?$(C?V(B . "0xC65C") ; HANGUL SYLLABLE IEUNG-WAE
       (?$(C?W(B . "0xC65D") ; HANGUL SYLLABLE IEUNG-WAE-KIYEOK
       (?$(C?X(B . "0xC660") ; HANGUL SYLLABLE IEUNG-WAE-NIEUN
       (?$(C?Y(B . "0xC66C") ; HANGUL SYLLABLE IEUNG-WAE-MIEUM
       (?$(C?Z(B . "0xC66F") ; HANGUL SYLLABLE IEUNG-WAE-SIOS
       (?$(C?[(B . "0xC671") ; HANGUL SYLLABLE IEUNG-WAE-IEUNG
       (?$(C?\(B . "0xC678") ; HANGUL SYLLABLE IEUNG-OE
       (?$(C?](B . "0xC679") ; HANGUL SYLLABLE IEUNG-OE-KIYEOK
       (?$(C?^(B . "0xC67C") ; HANGUL SYLLABLE IEUNG-OE-NIEUN
       (?$(C?_(B . "0xC680") ; HANGUL SYLLABLE IEUNG-OE-RIEUL
       (?$(C?`(B . "0xC688") ; HANGUL SYLLABLE IEUNG-OE-MIEUM
       (?$(C?a(B . "0xC689") ; HANGUL SYLLABLE IEUNG-OE-PIEUP
       (?$(C?b(B . "0xC68B") ; HANGUL SYLLABLE IEUNG-OE-SIOS
       (?$(C?c(B . "0xC68D") ; HANGUL SYLLABLE IEUNG-OE-IEUNG
       (?$(C?d(B . "0xC694") ; HANGUL SYLLABLE IEUNG-YO
       (?$(C?e(B . "0xC695") ; HANGUL SYLLABLE IEUNG-YO-KIYEOK
       (?$(C?f(B . "0xC698") ; HANGUL SYLLABLE IEUNG-YO-NIEUN
       (?$(C?g(B . "0xC69C") ; HANGUL SYLLABLE IEUNG-YO-RIEUL
       (?$(C?h(B . "0xC6A4") ; HANGUL SYLLABLE IEUNG-YO-MIEUM
       (?$(C?i(B . "0xC6A5") ; HANGUL SYLLABLE IEUNG-YO-PIEUP
       (?$(C?j(B . "0xC6A7") ; HANGUL SYLLABLE IEUNG-YO-SIOS
       (?$(C?k(B . "0xC6A9") ; HANGUL SYLLABLE IEUNG-YO-IEUNG
       (?$(C?l(B . "0xC6B0") ; HANGUL SYLLABLE IEUNG-U
       (?$(C?m(B . "0xC6B1") ; HANGUL SYLLABLE IEUNG-U-KIYEOK
       (?$(C?n(B . "0xC6B4") ; HANGUL SYLLABLE IEUNG-U-NIEUN
       (?$(C?o(B . "0xC6B8") ; HANGUL SYLLABLE IEUNG-U-RIEUL
       (?$(C?p(B . "0xC6B9") ; HANGUL SYLLABLE IEUNG-U-RIEULKIYEOK
       (?$(C?q(B . "0xC6BA") ; HANGUL SYLLABLE IEUNG-U-RIEULMIEUM
       (?$(C?r(B . "0xC6C0") ; HANGUL SYLLABLE IEUNG-U-MIEUM
       (?$(C?s(B . "0xC6C1") ; HANGUL SYLLABLE IEUNG-U-PIEUP
       (?$(C?t(B . "0xC6C3") ; HANGUL SYLLABLE IEUNG-U-SIOS
       (?$(C?u(B . "0xC6C5") ; HANGUL SYLLABLE IEUNG-U-IEUNG
       (?$(C?v(B . "0xC6CC") ; HANGUL SYLLABLE IEUNG-WEO
       (?$(C?w(B . "0xC6CD") ; HANGUL SYLLABLE IEUNG-WEO-KIYEOK
       (?$(C?x(B . "0xC6D0") ; HANGUL SYLLABLE IEUNG-WEO-NIEUN
       (?$(C?y(B . "0xC6D4") ; HANGUL SYLLABLE IEUNG-WEO-RIEUL
       (?$(C?z(B . "0xC6DC") ; HANGUL SYLLABLE IEUNG-WEO-MIEUM
       (?$(C?{(B . "0xC6DD") ; HANGUL SYLLABLE IEUNG-WEO-PIEUP
       (?$(C?|(B . "0xC6E0") ; HANGUL SYLLABLE IEUNG-WEO-SSANGSIOS
       (?$(C?}(B . "0xC6E1") ; HANGUL SYLLABLE IEUNG-WEO-IEUNG
       (?$(C?~(B . "0xC6E8") ; HANGUL SYLLABLE IEUNG-WE
       (?$(C@!(B . "0xC6E9") ; HANGUL SYLLABLE IEUNG-WE-KIYEOK
       (?$(C@"(B . "0xC6EC") ; HANGUL SYLLABLE IEUNG-WE-NIEUN
       (?$(C@#(B . "0xC6F0") ; HANGUL SYLLABLE IEUNG-WE-RIEUL
       (?$(C@$(B . "0xC6F8") ; HANGUL SYLLABLE IEUNG-WE-MIEUM
       (?$(C@%(B . "0xC6F9") ; HANGUL SYLLABLE IEUNG-WE-PIEUP
       (?$(C@&(B . "0xC6FD") ; HANGUL SYLLABLE IEUNG-WE-IEUNG
       (?$(C@'(B . "0xC704") ; HANGUL SYLLABLE IEUNG-WI
       (?$(C@((B . "0xC705") ; HANGUL SYLLABLE IEUNG-WI-KIYEOK
       (?$(C@)(B . "0xC708") ; HANGUL SYLLABLE IEUNG-WI-NIEUN
       (?$(C@*(B . "0xC70C") ; HANGUL SYLLABLE IEUNG-WI-RIEUL
       (?$(C@+(B . "0xC714") ; HANGUL SYLLABLE IEUNG-WI-MIEUM
       (?$(C@,(B . "0xC715") ; HANGUL SYLLABLE IEUNG-WI-PIEUP
       (?$(C@-(B . "0xC717") ; HANGUL SYLLABLE IEUNG-WI-SIOS
       (?$(C@.(B . "0xC719") ; HANGUL SYLLABLE IEUNG-WI-IEUNG
       (?$(C@/(B . "0xC720") ; HANGUL SYLLABLE IEUNG-YU
       (?$(C@0(B . "0xC721") ; HANGUL SYLLABLE IEUNG-YU-KIYEOK
       (?$(C@1(B . "0xC724") ; HANGUL SYLLABLE IEUNG-YU-NIEUN
       (?$(C@2(B . "0xC728") ; HANGUL SYLLABLE IEUNG-YU-RIEUL
       (?$(C@3(B . "0xC730") ; HANGUL SYLLABLE IEUNG-YU-MIEUM
       (?$(C@4(B . "0xC731") ; HANGUL SYLLABLE IEUNG-YU-PIEUP
       (?$(C@5(B . "0xC733") ; HANGUL SYLLABLE IEUNG-YU-SIOS
       (?$(C@6(B . "0xC735") ; HANGUL SYLLABLE IEUNG-YU-IEUNG
       (?$(C@7(B . "0xC737") ; HANGUL SYLLABLE IEUNG-YU-CHIEUCH
       (?$(C@8(B . "0xC73C") ; HANGUL SYLLABLE IEUNG-EU
       (?$(C@9(B . "0xC73D") ; HANGUL SYLLABLE IEUNG-EU-KIYEOK
       (?$(C@:(B . "0xC740") ; HANGUL SYLLABLE IEUNG-EU-NIEUN
       (?$(C@;(B . "0xC744") ; HANGUL SYLLABLE IEUNG-EU-RIEUL
       (?$(C@<(B . "0xC74A") ; HANGUL SYLLABLE IEUNG-EU-RIEULPHIEUPH
       (?$(C@=(B . "0xC74C") ; HANGUL SYLLABLE IEUNG-EU-MIEUM
       (?$(C@>(B . "0xC74D") ; HANGUL SYLLABLE IEUNG-EU-PIEUP
       (?$(C@?(B . "0xC74F") ; HANGUL SYLLABLE IEUNG-EU-SIOS
       (?$(C@@(B . "0xC751") ; HANGUL SYLLABLE IEUNG-EU-IEUNG
       (?$(C@A(B . "0xC752") ; HANGUL SYLLABLE IEUNG-EU-CIEUC
       (?$(C@B(B . "0xC753") ; HANGUL SYLLABLE IEUNG-EU-CHIEUCH
       (?$(C@C(B . "0xC754") ; HANGUL SYLLABLE IEUNG-EU-KHIEUKH
       (?$(C@D(B . "0xC755") ; HANGUL SYLLABLE IEUNG-EU-THIEUTH
       (?$(C@E(B . "0xC756") ; HANGUL SYLLABLE IEUNG-EU-PHIEUPH
       (?$(C@F(B . "0xC757") ; HANGUL SYLLABLE IEUNG-EU-HIEUH
       (?$(C@G(B . "0xC758") ; HANGUL SYLLABLE IEUNG-YI
       (?$(C@H(B . "0xC75C") ; HANGUL SYLLABLE IEUNG-YI-NIEUN
       (?$(C@I(B . "0xC760") ; HANGUL SYLLABLE IEUNG-YI-RIEUL
       (?$(C@J(B . "0xC768") ; HANGUL SYLLABLE IEUNG-YI-MIEUM
       (?$(C@K(B . "0xC76B") ; HANGUL SYLLABLE IEUNG-YI-SIOS
       (?$(C@L(B . "0xC774") ; HANGUL SYLLABLE IEUNG-I
       (?$(C@M(B . "0xC775") ; HANGUL SYLLABLE IEUNG-I-KIYEOK
       (?$(C@N(B . "0xC778") ; HANGUL SYLLABLE IEUNG-I-NIEUN
       (?$(C@O(B . "0xC77C") ; HANGUL SYLLABLE IEUNG-I-RIEUL
       (?$(C@P(B . "0xC77D") ; HANGUL SYLLABLE IEUNG-I-RIEULKIYEOK
       (?$(C@Q(B . "0xC77E") ; HANGUL SYLLABLE IEUNG-I-RIEULMIEUM
       (?$(C@R(B . "0xC783") ; HANGUL SYLLABLE IEUNG-I-RIEULHIEUH
       (?$(C@S(B . "0xC784") ; HANGUL SYLLABLE IEUNG-I-MIEUM
       (?$(C@T(B . "0xC785") ; HANGUL SYLLABLE IEUNG-I-PIEUP
       (?$(C@U(B . "0xC787") ; HANGUL SYLLABLE IEUNG-I-SIOS
       (?$(C@V(B . "0xC788") ; HANGUL SYLLABLE IEUNG-I-SSANGSIOS
       (?$(C@W(B . "0xC789") ; HANGUL SYLLABLE IEUNG-I-IEUNG
       (?$(C@X(B . "0xC78A") ; HANGUL SYLLABLE IEUNG-I-CIEUC
       (?$(C@Y(B . "0xC78E") ; HANGUL SYLLABLE IEUNG-I-PHIEUPH
       (?$(C@Z(B . "0xC790") ; HANGUL SYLLABLE CIEUC-A
       (?$(C@[(B . "0xC791") ; HANGUL SYLLABLE CIEUC-A-KIYEOK
       (?$(C@\(B . "0xC794") ; HANGUL SYLLABLE CIEUC-A-NIEUN
       (?$(C@](B . "0xC796") ; HANGUL SYLLABLE CIEUC-A-NIEUNHIEUH
       (?$(C@^(B . "0xC797") ; HANGUL SYLLABLE CIEUC-A-TIKEUT
       (?$(C@_(B . "0xC798") ; HANGUL SYLLABLE CIEUC-A-RIEUL
       (?$(C@`(B . "0xC79A") ; HANGUL SYLLABLE CIEUC-A-RIEULMIEUM
       (?$(C@a(B . "0xC7A0") ; HANGUL SYLLABLE CIEUC-A-MIEUM
       (?$(C@b(B . "0xC7A1") ; HANGUL SYLLABLE CIEUC-A-PIEUP
       (?$(C@c(B . "0xC7A3") ; HANGUL SYLLABLE CIEUC-A-SIOS
       (?$(C@d(B . "0xC7A4") ; HANGUL SYLLABLE CIEUC-A-SSANGSIOS
       (?$(C@e(B . "0xC7A5") ; HANGUL SYLLABLE CIEUC-A-IEUNG
       (?$(C@f(B . "0xC7A6") ; HANGUL SYLLABLE CIEUC-A-CIEUC
       (?$(C@g(B . "0xC7AC") ; HANGUL SYLLABLE CIEUC-AE
       (?$(C@h(B . "0xC7AD") ; HANGUL SYLLABLE CIEUC-AE-KIYEOK
       (?$(C@i(B . "0xC7B0") ; HANGUL SYLLABLE CIEUC-AE-NIEUN
       (?$(C@j(B . "0xC7B4") ; HANGUL SYLLABLE CIEUC-AE-RIEUL
       (?$(C@k(B . "0xC7BC") ; HANGUL SYLLABLE CIEUC-AE-MIEUM
       (?$(C@l(B . "0xC7BD") ; HANGUL SYLLABLE CIEUC-AE-PIEUP
       (?$(C@m(B . "0xC7BF") ; HANGUL SYLLABLE CIEUC-AE-SIOS
       (?$(C@n(B . "0xC7C0") ; HANGUL SYLLABLE CIEUC-AE-SSANGSIOS
       (?$(C@o(B . "0xC7C1") ; HANGUL SYLLABLE CIEUC-AE-IEUNG
       (?$(C@p(B . "0xC7C8") ; HANGUL SYLLABLE CIEUC-YA
       (?$(C@q(B . "0xC7C9") ; HANGUL SYLLABLE CIEUC-YA-KIYEOK
       (?$(C@r(B . "0xC7CC") ; HANGUL SYLLABLE CIEUC-YA-NIEUN
       (?$(C@s(B . "0xC7CE") ; HANGUL SYLLABLE CIEUC-YA-NIEUNHIEUH
       (?$(C@t(B . "0xC7D0") ; HANGUL SYLLABLE CIEUC-YA-RIEUL
       (?$(C@u(B . "0xC7D8") ; HANGUL SYLLABLE CIEUC-YA-MIEUM
       (?$(C@v(B . "0xC7DD") ; HANGUL SYLLABLE CIEUC-YA-IEUNG
       (?$(C@w(B . "0xC7E4") ; HANGUL SYLLABLE CIEUC-YAE
       (?$(C@x(B . "0xC7E8") ; HANGUL SYLLABLE CIEUC-YAE-NIEUN
       (?$(C@y(B . "0xC7EC") ; HANGUL SYLLABLE CIEUC-YAE-RIEUL
       (?$(C@z(B . "0xC800") ; HANGUL SYLLABLE CIEUC-EO
       (?$(C@{(B . "0xC801") ; HANGUL SYLLABLE CIEUC-EO-KIYEOK
       (?$(C@|(B . "0xC804") ; HANGUL SYLLABLE CIEUC-EO-NIEUN
       (?$(C@}(B . "0xC808") ; HANGUL SYLLABLE CIEUC-EO-RIEUL
       (?$(C@~(B . "0xC80A") ; HANGUL SYLLABLE CIEUC-EO-RIEULMIEUM
       (?$(CA!(B . "0xC810") ; HANGUL SYLLABLE CIEUC-EO-MIEUM
       (?$(CA"(B . "0xC811") ; HANGUL SYLLABLE CIEUC-EO-PIEUP
       (?$(CA#(B . "0xC813") ; HANGUL SYLLABLE CIEUC-EO-SIOS
       (?$(CA$(B . "0xC815") ; HANGUL SYLLABLE CIEUC-EO-IEUNG
       (?$(CA%(B . "0xC816") ; HANGUL SYLLABLE CIEUC-EO-CIEUC
       (?$(CA&(B . "0xC81C") ; HANGUL SYLLABLE CIEUC-E
       (?$(CA'(B . "0xC81D") ; HANGUL SYLLABLE CIEUC-E-KIYEOK
       (?$(CA((B . "0xC820") ; HANGUL SYLLABLE CIEUC-E-NIEUN
       (?$(CA)(B . "0xC824") ; HANGUL SYLLABLE CIEUC-E-RIEUL
       (?$(CA*(B . "0xC82C") ; HANGUL SYLLABLE CIEUC-E-MIEUM
       (?$(CA+(B . "0xC82D") ; HANGUL SYLLABLE CIEUC-E-PIEUP
       (?$(CA,(B . "0xC82F") ; HANGUL SYLLABLE CIEUC-E-SIOS
       (?$(CA-(B . "0xC831") ; HANGUL SYLLABLE CIEUC-E-IEUNG
       (?$(CA.(B . "0xC838") ; HANGUL SYLLABLE CIEUC-YEO
       (?$(CA/(B . "0xC83C") ; HANGUL SYLLABLE CIEUC-YEO-NIEUN
       (?$(CA0(B . "0xC840") ; HANGUL SYLLABLE CIEUC-YEO-RIEUL
       (?$(CA1(B . "0xC848") ; HANGUL SYLLABLE CIEUC-YEO-MIEUM
       (?$(CA2(B . "0xC849") ; HANGUL SYLLABLE CIEUC-YEO-PIEUP
       (?$(CA3(B . "0xC84C") ; HANGUL SYLLABLE CIEUC-YEO-SSANGSIOS
       (?$(CA4(B . "0xC84D") ; HANGUL SYLLABLE CIEUC-YEO-IEUNG
       (?$(CA5(B . "0xC854") ; HANGUL SYLLABLE CIEUC-YE
       (?$(CA6(B . "0xC870") ; HANGUL SYLLABLE CIEUC-O
       (?$(CA7(B . "0xC871") ; HANGUL SYLLABLE CIEUC-O-KIYEOK
       (?$(CA8(B . "0xC874") ; HANGUL SYLLABLE CIEUC-O-NIEUN
       (?$(CA9(B . "0xC878") ; HANGUL SYLLABLE CIEUC-O-RIEUL
       (?$(CA:(B . "0xC87A") ; HANGUL SYLLABLE CIEUC-O-RIEULMIEUM
       (?$(CA;(B . "0xC880") ; HANGUL SYLLABLE CIEUC-O-MIEUM
       (?$(CA<(B . "0xC881") ; HANGUL SYLLABLE CIEUC-O-PIEUP
       (?$(CA=(B . "0xC883") ; HANGUL SYLLABLE CIEUC-O-SIOS
       (?$(CA>(B . "0xC885") ; HANGUL SYLLABLE CIEUC-O-IEUNG
       (?$(CA?(B . "0xC886") ; HANGUL SYLLABLE CIEUC-O-CIEUC
       (?$(CA@(B . "0xC887") ; HANGUL SYLLABLE CIEUC-O-CHIEUCH
       (?$(CAA(B . "0xC88B") ; HANGUL SYLLABLE CIEUC-O-HIEUH
       (?$(CAB(B . "0xC88C") ; HANGUL SYLLABLE CIEUC-WA
       (?$(CAC(B . "0xC88D") ; HANGUL SYLLABLE CIEUC-WA-KIYEOK
       (?$(CAD(B . "0xC894") ; HANGUL SYLLABLE CIEUC-WA-RIEUL
       (?$(CAE(B . "0xC89D") ; HANGUL SYLLABLE CIEUC-WA-PIEUP
       (?$(CAF(B . "0xC89F") ; HANGUL SYLLABLE CIEUC-WA-SIOS
       (?$(CAG(B . "0xC8A1") ; HANGUL SYLLABLE CIEUC-WA-IEUNG
       (?$(CAH(B . "0xC8A8") ; HANGUL SYLLABLE CIEUC-WAE
       (?$(CAI(B . "0xC8BC") ; HANGUL SYLLABLE CIEUC-WAE-SSANGSIOS
       (?$(CAJ(B . "0xC8BD") ; HANGUL SYLLABLE CIEUC-WAE-IEUNG
       (?$(CAK(B . "0xC8C4") ; HANGUL SYLLABLE CIEUC-OE
       (?$(CAL(B . "0xC8C8") ; HANGUL SYLLABLE CIEUC-OE-NIEUN
       (?$(CAM(B . "0xC8CC") ; HANGUL SYLLABLE CIEUC-OE-RIEUL
       (?$(CAN(B . "0xC8D4") ; HANGUL SYLLABLE CIEUC-OE-MIEUM
       (?$(CAO(B . "0xC8D5") ; HANGUL SYLLABLE CIEUC-OE-PIEUP
       (?$(CAP(B . "0xC8D7") ; HANGUL SYLLABLE CIEUC-OE-SIOS
       (?$(CAQ(B . "0xC8D9") ; HANGUL SYLLABLE CIEUC-OE-IEUNG
       (?$(CAR(B . "0xC8E0") ; HANGUL SYLLABLE CIEUC-YO
       (?$(CAS(B . "0xC8E1") ; HANGUL SYLLABLE CIEUC-YO-KIYEOK
       (?$(CAT(B . "0xC8E4") ; HANGUL SYLLABLE CIEUC-YO-NIEUN
       (?$(CAU(B . "0xC8F5") ; HANGUL SYLLABLE CIEUC-YO-IEUNG
       (?$(CAV(B . "0xC8FC") ; HANGUL SYLLABLE CIEUC-U
       (?$(CAW(B . "0xC8FD") ; HANGUL SYLLABLE CIEUC-U-KIYEOK
       (?$(CAX(B . "0xC900") ; HANGUL SYLLABLE CIEUC-U-NIEUN
       (?$(CAY(B . "0xC904") ; HANGUL SYLLABLE CIEUC-U-RIEUL
       (?$(CAZ(B . "0xC905") ; HANGUL SYLLABLE CIEUC-U-RIEULKIYEOK
       (?$(CA[(B . "0xC906") ; HANGUL SYLLABLE CIEUC-U-RIEULMIEUM
       (?$(CA\(B . "0xC90C") ; HANGUL SYLLABLE CIEUC-U-MIEUM
       (?$(CA](B . "0xC90D") ; HANGUL SYLLABLE CIEUC-U-PIEUP
       (?$(CA^(B . "0xC90F") ; HANGUL SYLLABLE CIEUC-U-SIOS
       (?$(CA_(B . "0xC911") ; HANGUL SYLLABLE CIEUC-U-IEUNG
       (?$(CA`(B . "0xC918") ; HANGUL SYLLABLE CIEUC-WEO
       (?$(CAa(B . "0xC92C") ; HANGUL SYLLABLE CIEUC-WEO-SSANGSIOS
       (?$(CAb(B . "0xC934") ; HANGUL SYLLABLE CIEUC-WE
       (?$(CAc(B . "0xC950") ; HANGUL SYLLABLE CIEUC-WI
       (?$(CAd(B . "0xC951") ; HANGUL SYLLABLE CIEUC-WI-KIYEOK
       (?$(CAe(B . "0xC954") ; HANGUL SYLLABLE CIEUC-WI-NIEUN
       (?$(CAf(B . "0xC958") ; HANGUL SYLLABLE CIEUC-WI-RIEUL
       (?$(CAg(B . "0xC960") ; HANGUL SYLLABLE CIEUC-WI-MIEUM
       (?$(CAh(B . "0xC961") ; HANGUL SYLLABLE CIEUC-WI-PIEUP
       (?$(CAi(B . "0xC963") ; HANGUL SYLLABLE CIEUC-WI-SIOS
       (?$(CAj(B . "0xC96C") ; HANGUL SYLLABLE CIEUC-YU
       (?$(CAk(B . "0xC970") ; HANGUL SYLLABLE CIEUC-YU-NIEUN
       (?$(CAl(B . "0xC974") ; HANGUL SYLLABLE CIEUC-YU-RIEUL
       (?$(CAm(B . "0xC97C") ; HANGUL SYLLABLE CIEUC-YU-MIEUM
       (?$(CAn(B . "0xC988") ; HANGUL SYLLABLE CIEUC-EU
       (?$(CAo(B . "0xC989") ; HANGUL SYLLABLE CIEUC-EU-KIYEOK
       (?$(CAp(B . "0xC98C") ; HANGUL SYLLABLE CIEUC-EU-NIEUN
       (?$(CAq(B . "0xC990") ; HANGUL SYLLABLE CIEUC-EU-RIEUL
       (?$(CAr(B . "0xC998") ; HANGUL SYLLABLE CIEUC-EU-MIEUM
       (?$(CAs(B . "0xC999") ; HANGUL SYLLABLE CIEUC-EU-PIEUP
       (?$(CAt(B . "0xC99B") ; HANGUL SYLLABLE CIEUC-EU-SIOS
       (?$(CAu(B . "0xC99D") ; HANGUL SYLLABLE CIEUC-EU-IEUNG
       (?$(CAv(B . "0xC9C0") ; HANGUL SYLLABLE CIEUC-I
       (?$(CAw(B . "0xC9C1") ; HANGUL SYLLABLE CIEUC-I-KIYEOK
       (?$(CAx(B . "0xC9C4") ; HANGUL SYLLABLE CIEUC-I-NIEUN
       (?$(CAy(B . "0xC9C7") ; HANGUL SYLLABLE CIEUC-I-TIKEUT
       (?$(CAz(B . "0xC9C8") ; HANGUL SYLLABLE CIEUC-I-RIEUL
       (?$(CA{(B . "0xC9CA") ; HANGUL SYLLABLE CIEUC-I-RIEULMIEUM
       (?$(CA|(B . "0xC9D0") ; HANGUL SYLLABLE CIEUC-I-MIEUM
       (?$(CA}(B . "0xC9D1") ; HANGUL SYLLABLE CIEUC-I-PIEUP
       (?$(CA~(B . "0xC9D3") ; HANGUL SYLLABLE CIEUC-I-SIOS
       (?$(CB!(B . "0xC9D5") ; HANGUL SYLLABLE CIEUC-I-IEUNG
       (?$(CB"(B . "0xC9D6") ; HANGUL SYLLABLE CIEUC-I-CIEUC
       (?$(CB#(B . "0xC9D9") ; HANGUL SYLLABLE CIEUC-I-THIEUTH
       (?$(CB$(B . "0xC9DA") ; HANGUL SYLLABLE CIEUC-I-PHIEUPH
       (?$(CB%(B . "0xC9DC") ; HANGUL SYLLABLE SSANGCIEUC-A
       (?$(CB&(B . "0xC9DD") ; HANGUL SYLLABLE SSANGCIEUC-A-KIYEOK
       (?$(CB'(B . "0xC9E0") ; HANGUL SYLLABLE SSANGCIEUC-A-NIEUN
       (?$(CB((B . "0xC9E2") ; HANGUL SYLLABLE SSANGCIEUC-A-NIEUNHIEUH
       (?$(CB)(B . "0xC9E4") ; HANGUL SYLLABLE SSANGCIEUC-A-RIEUL
       (?$(CB*(B . "0xC9E7") ; HANGUL SYLLABLE SSANGCIEUC-A-RIEULPIEUP
       (?$(CB+(B . "0xC9EC") ; HANGUL SYLLABLE SSANGCIEUC-A-MIEUM
       (?$(CB,(B . "0xC9ED") ; HANGUL SYLLABLE SSANGCIEUC-A-PIEUP
       (?$(CB-(B . "0xC9EF") ; HANGUL SYLLABLE SSANGCIEUC-A-SIOS
       (?$(CB.(B . "0xC9F0") ; HANGUL SYLLABLE SSANGCIEUC-A-SSANGSIOS
       (?$(CB/(B . "0xC9F1") ; HANGUL SYLLABLE SSANGCIEUC-A-IEUNG
       (?$(CB0(B . "0xC9F8") ; HANGUL SYLLABLE SSANGCIEUC-AE
       (?$(CB1(B . "0xC9F9") ; HANGUL SYLLABLE SSANGCIEUC-AE-KIYEOK
       (?$(CB2(B . "0xC9FC") ; HANGUL SYLLABLE SSANGCIEUC-AE-NIEUN
       (?$(CB3(B . "0xCA00") ; HANGUL SYLLABLE SSANGCIEUC-AE-RIEUL
       (?$(CB4(B . "0xCA08") ; HANGUL SYLLABLE SSANGCIEUC-AE-MIEUM
       (?$(CB5(B . "0xCA09") ; HANGUL SYLLABLE SSANGCIEUC-AE-PIEUP
       (?$(CB6(B . "0xCA0B") ; HANGUL SYLLABLE SSANGCIEUC-AE-SIOS
       (?$(CB7(B . "0xCA0C") ; HANGUL SYLLABLE SSANGCIEUC-AE-SSANGSIOS
       (?$(CB8(B . "0xCA0D") ; HANGUL SYLLABLE SSANGCIEUC-AE-IEUNG
       (?$(CB9(B . "0xCA14") ; HANGUL SYLLABLE SSANGCIEUC-YA
       (?$(CB:(B . "0xCA18") ; HANGUL SYLLABLE SSANGCIEUC-YA-NIEUN
       (?$(CB;(B . "0xCA29") ; HANGUL SYLLABLE SSANGCIEUC-YA-IEUNG
       (?$(CB<(B . "0xCA4C") ; HANGUL SYLLABLE SSANGCIEUC-EO
       (?$(CB=(B . "0xCA4D") ; HANGUL SYLLABLE SSANGCIEUC-EO-KIYEOK
       (?$(CB>(B . "0xCA50") ; HANGUL SYLLABLE SSANGCIEUC-EO-NIEUN
       (?$(CB?(B . "0xCA54") ; HANGUL SYLLABLE SSANGCIEUC-EO-RIEUL
       (?$(CB@(B . "0xCA5C") ; HANGUL SYLLABLE SSANGCIEUC-EO-MIEUM
       (?$(CBA(B . "0xCA5D") ; HANGUL SYLLABLE SSANGCIEUC-EO-PIEUP
       (?$(CBB(B . "0xCA5F") ; HANGUL SYLLABLE SSANGCIEUC-EO-SIOS
       (?$(CBC(B . "0xCA60") ; HANGUL SYLLABLE SSANGCIEUC-EO-SSANGSIOS
       (?$(CBD(B . "0xCA61") ; HANGUL SYLLABLE SSANGCIEUC-EO-IEUNG
       (?$(CBE(B . "0xCA68") ; HANGUL SYLLABLE SSANGCIEUC-E
       (?$(CBF(B . "0xCA7D") ; HANGUL SYLLABLE SSANGCIEUC-E-IEUNG
       (?$(CBG(B . "0xCA84") ; HANGUL SYLLABLE SSANGCIEUC-YEO
       (?$(CBH(B . "0xCA98") ; HANGUL SYLLABLE SSANGCIEUC-YEO-SSANGSIOS
       (?$(CBI(B . "0xCABC") ; HANGUL SYLLABLE SSANGCIEUC-O
       (?$(CBJ(B . "0xCABD") ; HANGUL SYLLABLE SSANGCIEUC-O-KIYEOK
       (?$(CBK(B . "0xCAC0") ; HANGUL SYLLABLE SSANGCIEUC-O-NIEUN
       (?$(CBL(B . "0xCAC4") ; HANGUL SYLLABLE SSANGCIEUC-O-RIEUL
       (?$(CBM(B . "0xCACC") ; HANGUL SYLLABLE SSANGCIEUC-O-MIEUM
       (?$(CBN(B . "0xCACD") ; HANGUL SYLLABLE SSANGCIEUC-O-PIEUP
       (?$(CBO(B . "0xCACF") ; HANGUL SYLLABLE SSANGCIEUC-O-SIOS
       (?$(CBP(B . "0xCAD1") ; HANGUL SYLLABLE SSANGCIEUC-O-IEUNG
       (?$(CBQ(B . "0xCAD3") ; HANGUL SYLLABLE SSANGCIEUC-O-CHIEUCH
       (?$(CBR(B . "0xCAD8") ; HANGUL SYLLABLE SSANGCIEUC-WA
       (?$(CBS(B . "0xCAD9") ; HANGUL SYLLABLE SSANGCIEUC-WA-KIYEOK
       (?$(CBT(B . "0xCAE0") ; HANGUL SYLLABLE SSANGCIEUC-WA-RIEUL
       (?$(CBU(B . "0xCAEC") ; HANGUL SYLLABLE SSANGCIEUC-WA-SSANGSIOS
       (?$(CBV(B . "0xCAF4") ; HANGUL SYLLABLE SSANGCIEUC-WAE
       (?$(CBW(B . "0xCB08") ; HANGUL SYLLABLE SSANGCIEUC-WAE-SSANGSIOS
       (?$(CBX(B . "0xCB10") ; HANGUL SYLLABLE SSANGCIEUC-OE
       (?$(CBY(B . "0xCB14") ; HANGUL SYLLABLE SSANGCIEUC-OE-NIEUN
       (?$(CBZ(B . "0xCB18") ; HANGUL SYLLABLE SSANGCIEUC-OE-RIEUL
       (?$(CB[(B . "0xCB20") ; HANGUL SYLLABLE SSANGCIEUC-OE-MIEUM
       (?$(CB\(B . "0xCB21") ; HANGUL SYLLABLE SSANGCIEUC-OE-PIEUP
       (?$(CB](B . "0xCB41") ; HANGUL SYLLABLE SSANGCIEUC-YO-IEUNG
       (?$(CB^(B . "0xCB48") ; HANGUL SYLLABLE SSANGCIEUC-U
       (?$(CB_(B . "0xCB49") ; HANGUL SYLLABLE SSANGCIEUC-U-KIYEOK
       (?$(CB`(B . "0xCB4C") ; HANGUL SYLLABLE SSANGCIEUC-U-NIEUN
       (?$(CBa(B . "0xCB50") ; HANGUL SYLLABLE SSANGCIEUC-U-RIEUL
       (?$(CBb(B . "0xCB58") ; HANGUL SYLLABLE SSANGCIEUC-U-MIEUM
       (?$(CBc(B . "0xCB59") ; HANGUL SYLLABLE SSANGCIEUC-U-PIEUP
       (?$(CBd(B . "0xCB5D") ; HANGUL SYLLABLE SSANGCIEUC-U-IEUNG
       (?$(CBe(B . "0xCB64") ; HANGUL SYLLABLE SSANGCIEUC-WEO
       (?$(CBf(B . "0xCB78") ; HANGUL SYLLABLE SSANGCIEUC-WEO-SSANGSIOS
       (?$(CBg(B . "0xCB79") ; HANGUL SYLLABLE SSANGCIEUC-WEO-IEUNG
       (?$(CBh(B . "0xCB9C") ; HANGUL SYLLABLE SSANGCIEUC-WI
       (?$(CBi(B . "0xCBB8") ; HANGUL SYLLABLE SSANGCIEUC-YU
       (?$(CBj(B . "0xCBD4") ; HANGUL SYLLABLE SSANGCIEUC-EU
       (?$(CBk(B . "0xCBE4") ; HANGUL SYLLABLE SSANGCIEUC-EU-MIEUM
       (?$(CBl(B . "0xCBE7") ; HANGUL SYLLABLE SSANGCIEUC-EU-SIOS
       (?$(CBm(B . "0xCBE9") ; HANGUL SYLLABLE SSANGCIEUC-EU-IEUNG
       (?$(CBn(B . "0xCC0C") ; HANGUL SYLLABLE SSANGCIEUC-I
       (?$(CBo(B . "0xCC0D") ; HANGUL SYLLABLE SSANGCIEUC-I-KIYEOK
       (?$(CBp(B . "0xCC10") ; HANGUL SYLLABLE SSANGCIEUC-I-NIEUN
       (?$(CBq(B . "0xCC14") ; HANGUL SYLLABLE SSANGCIEUC-I-RIEUL
       (?$(CBr(B . "0xCC1C") ; HANGUL SYLLABLE SSANGCIEUC-I-MIEUM
       (?$(CBs(B . "0xCC1D") ; HANGUL SYLLABLE SSANGCIEUC-I-PIEUP
       (?$(CBt(B . "0xCC21") ; HANGUL SYLLABLE SSANGCIEUC-I-IEUNG
       (?$(CBu(B . "0xCC22") ; HANGUL SYLLABLE SSANGCIEUC-I-CIEUC
       (?$(CBv(B . "0xCC27") ; HANGUL SYLLABLE SSANGCIEUC-I-HIEUH
       (?$(CBw(B . "0xCC28") ; HANGUL SYLLABLE CHIEUCH-A
       (?$(CBx(B . "0xCC29") ; HANGUL SYLLABLE CHIEUCH-A-KIYEOK
       (?$(CBy(B . "0xCC2C") ; HANGUL SYLLABLE CHIEUCH-A-NIEUN
       (?$(CBz(B . "0xCC2E") ; HANGUL SYLLABLE CHIEUCH-A-NIEUNHIEUH
       (?$(CB{(B . "0xCC30") ; HANGUL SYLLABLE CHIEUCH-A-RIEUL
       (?$(CB|(B . "0xCC38") ; HANGUL SYLLABLE CHIEUCH-A-MIEUM
       (?$(CB}(B . "0xCC39") ; HANGUL SYLLABLE CHIEUCH-A-PIEUP
       (?$(CB~(B . "0xCC3B") ; HANGUL SYLLABLE CHIEUCH-A-SIOS
       (?$(CC!(B . "0xCC3C") ; HANGUL SYLLABLE CHIEUCH-A-SSANGSIOS
       (?$(CC"(B . "0xCC3D") ; HANGUL SYLLABLE CHIEUCH-A-IEUNG
       (?$(CC#(B . "0xCC3E") ; HANGUL SYLLABLE CHIEUCH-A-CIEUC
       (?$(CC$(B . "0xCC44") ; HANGUL SYLLABLE CHIEUCH-AE
       (?$(CC%(B . "0xCC45") ; HANGUL SYLLABLE CHIEUCH-AE-KIYEOK
       (?$(CC&(B . "0xCC48") ; HANGUL SYLLABLE CHIEUCH-AE-NIEUN
       (?$(CC'(B . "0xCC4C") ; HANGUL SYLLABLE CHIEUCH-AE-RIEUL
       (?$(CC((B . "0xCC54") ; HANGUL SYLLABLE CHIEUCH-AE-MIEUM
       (?$(CC)(B . "0xCC55") ; HANGUL SYLLABLE CHIEUCH-AE-PIEUP
       (?$(CC*(B . "0xCC57") ; HANGUL SYLLABLE CHIEUCH-AE-SIOS
       (?$(CC+(B . "0xCC58") ; HANGUL SYLLABLE CHIEUCH-AE-SSANGSIOS
       (?$(CC,(B . "0xCC59") ; HANGUL SYLLABLE CHIEUCH-AE-IEUNG
       (?$(CC-(B . "0xCC60") ; HANGUL SYLLABLE CHIEUCH-YA
       (?$(CC.(B . "0xCC64") ; HANGUL SYLLABLE CHIEUCH-YA-NIEUN
       (?$(CC/(B . "0xCC66") ; HANGUL SYLLABLE CHIEUCH-YA-NIEUNHIEUH
       (?$(CC0(B . "0xCC68") ; HANGUL SYLLABLE CHIEUCH-YA-RIEUL
       (?$(CC1(B . "0xCC70") ; HANGUL SYLLABLE CHIEUCH-YA-MIEUM
       (?$(CC2(B . "0xCC75") ; HANGUL SYLLABLE CHIEUCH-YA-IEUNG
       (?$(CC3(B . "0xCC98") ; HANGUL SYLLABLE CHIEUCH-EO
       (?$(CC4(B . "0xCC99") ; HANGUL SYLLABLE CHIEUCH-EO-KIYEOK
       (?$(CC5(B . "0xCC9C") ; HANGUL SYLLABLE CHIEUCH-EO-NIEUN
       (?$(CC6(B . "0xCCA0") ; HANGUL SYLLABLE CHIEUCH-EO-RIEUL
       (?$(CC7(B . "0xCCA8") ; HANGUL SYLLABLE CHIEUCH-EO-MIEUM
       (?$(CC8(B . "0xCCA9") ; HANGUL SYLLABLE CHIEUCH-EO-PIEUP
       (?$(CC9(B . "0xCCAB") ; HANGUL SYLLABLE CHIEUCH-EO-SIOS
       (?$(CC:(B . "0xCCAC") ; HANGUL SYLLABLE CHIEUCH-EO-SSANGSIOS
       (?$(CC;(B . "0xCCAD") ; HANGUL SYLLABLE CHIEUCH-EO-IEUNG
       (?$(CC<(B . "0xCCB4") ; HANGUL SYLLABLE CHIEUCH-E
       (?$(CC=(B . "0xCCB5") ; HANGUL SYLLABLE CHIEUCH-E-KIYEOK
       (?$(CC>(B . "0xCCB8") ; HANGUL SYLLABLE CHIEUCH-E-NIEUN
       (?$(CC?(B . "0xCCBC") ; HANGUL SYLLABLE CHIEUCH-E-RIEUL
       (?$(CC@(B . "0xCCC4") ; HANGUL SYLLABLE CHIEUCH-E-MIEUM
       (?$(CCA(B . "0xCCC5") ; HANGUL SYLLABLE CHIEUCH-E-PIEUP
       (?$(CCB(B . "0xCCC7") ; HANGUL SYLLABLE CHIEUCH-E-SIOS
       (?$(CCC(B . "0xCCC9") ; HANGUL SYLLABLE CHIEUCH-E-IEUNG
       (?$(CCD(B . "0xCCD0") ; HANGUL SYLLABLE CHIEUCH-YEO
       (?$(CCE(B . "0xCCD4") ; HANGUL SYLLABLE CHIEUCH-YEO-NIEUN
       (?$(CCF(B . "0xCCE4") ; HANGUL SYLLABLE CHIEUCH-YEO-SSANGSIOS
       (?$(CCG(B . "0xCCEC") ; HANGUL SYLLABLE CHIEUCH-YE
       (?$(CCH(B . "0xCCF0") ; HANGUL SYLLABLE CHIEUCH-YE-NIEUN
       (?$(CCI(B . "0xCD01") ; HANGUL SYLLABLE CHIEUCH-YE-IEUNG
       (?$(CCJ(B . "0xCD08") ; HANGUL SYLLABLE CHIEUCH-O
       (?$(CCK(B . "0xCD09") ; HANGUL SYLLABLE CHIEUCH-O-KIYEOK
       (?$(CCL(B . "0xCD0C") ; HANGUL SYLLABLE CHIEUCH-O-NIEUN
       (?$(CCM(B . "0xCD10") ; HANGUL SYLLABLE CHIEUCH-O-RIEUL
       (?$(CCN(B . "0xCD18") ; HANGUL SYLLABLE CHIEUCH-O-MIEUM
       (?$(CCO(B . "0xCD19") ; HANGUL SYLLABLE CHIEUCH-O-PIEUP
       (?$(CCP(B . "0xCD1B") ; HANGUL SYLLABLE CHIEUCH-O-SIOS
       (?$(CCQ(B . "0xCD1D") ; HANGUL SYLLABLE CHIEUCH-O-IEUNG
       (?$(CCR(B . "0xCD24") ; HANGUL SYLLABLE CHIEUCH-WA
       (?$(CCS(B . "0xCD28") ; HANGUL SYLLABLE CHIEUCH-WA-NIEUN
       (?$(CCT(B . "0xCD2C") ; HANGUL SYLLABLE CHIEUCH-WA-RIEUL
       (?$(CCU(B . "0xCD39") ; HANGUL SYLLABLE CHIEUCH-WA-IEUNG
       (?$(CCV(B . "0xCD5C") ; HANGUL SYLLABLE CHIEUCH-OE
       (?$(CCW(B . "0xCD60") ; HANGUL SYLLABLE CHIEUCH-OE-NIEUN
       (?$(CCX(B . "0xCD64") ; HANGUL SYLLABLE CHIEUCH-OE-RIEUL
       (?$(CCY(B . "0xCD6C") ; HANGUL SYLLABLE CHIEUCH-OE-MIEUM
       (?$(CCZ(B . "0xCD6D") ; HANGUL SYLLABLE CHIEUCH-OE-PIEUP
       (?$(CC[(B . "0xCD6F") ; HANGUL SYLLABLE CHIEUCH-OE-SIOS
       (?$(CC\(B . "0xCD71") ; HANGUL SYLLABLE CHIEUCH-OE-IEUNG
       (?$(CC](B . "0xCD78") ; HANGUL SYLLABLE CHIEUCH-YO
       (?$(CC^(B . "0xCD88") ; HANGUL SYLLABLE CHIEUCH-YO-MIEUM
       (?$(CC_(B . "0xCD94") ; HANGUL SYLLABLE CHIEUCH-U
       (?$(CC`(B . "0xCD95") ; HANGUL SYLLABLE CHIEUCH-U-KIYEOK
       (?$(CCa(B . "0xCD98") ; HANGUL SYLLABLE CHIEUCH-U-NIEUN
       (?$(CCb(B . "0xCD9C") ; HANGUL SYLLABLE CHIEUCH-U-RIEUL
       (?$(CCc(B . "0xCDA4") ; HANGUL SYLLABLE CHIEUCH-U-MIEUM
       (?$(CCd(B . "0xCDA5") ; HANGUL SYLLABLE CHIEUCH-U-PIEUP
       (?$(CCe(B . "0xCDA7") ; HANGUL SYLLABLE CHIEUCH-U-SIOS
       (?$(CCf(B . "0xCDA9") ; HANGUL SYLLABLE CHIEUCH-U-IEUNG
       (?$(CCg(B . "0xCDB0") ; HANGUL SYLLABLE CHIEUCH-WEO
       (?$(CCh(B . "0xCDC4") ; HANGUL SYLLABLE CHIEUCH-WEO-SSANGSIOS
       (?$(CCi(B . "0xCDCC") ; HANGUL SYLLABLE CHIEUCH-WE
       (?$(CCj(B . "0xCDD0") ; HANGUL SYLLABLE CHIEUCH-WE-NIEUN
       (?$(CCk(B . "0xCDE8") ; HANGUL SYLLABLE CHIEUCH-WI
       (?$(CCl(B . "0xCDEC") ; HANGUL SYLLABLE CHIEUCH-WI-NIEUN
       (?$(CCm(B . "0xCDF0") ; HANGUL SYLLABLE CHIEUCH-WI-RIEUL
       (?$(CCn(B . "0xCDF8") ; HANGUL SYLLABLE CHIEUCH-WI-MIEUM
       (?$(CCo(B . "0xCDF9") ; HANGUL SYLLABLE CHIEUCH-WI-PIEUP
       (?$(CCp(B . "0xCDFB") ; HANGUL SYLLABLE CHIEUCH-WI-SIOS
       (?$(CCq(B . "0xCDFD") ; HANGUL SYLLABLE CHIEUCH-WI-IEUNG
       (?$(CCr(B . "0xCE04") ; HANGUL SYLLABLE CHIEUCH-YU
       (?$(CCs(B . "0xCE08") ; HANGUL SYLLABLE CHIEUCH-YU-NIEUN
       (?$(CCt(B . "0xCE0C") ; HANGUL SYLLABLE CHIEUCH-YU-RIEUL
       (?$(CCu(B . "0xCE14") ; HANGUL SYLLABLE CHIEUCH-YU-MIEUM
       (?$(CCv(B . "0xCE19") ; HANGUL SYLLABLE CHIEUCH-YU-IEUNG
       (?$(CCw(B . "0xCE20") ; HANGUL SYLLABLE CHIEUCH-EU
       (?$(CCx(B . "0xCE21") ; HANGUL SYLLABLE CHIEUCH-EU-KIYEOK
       (?$(CCy(B . "0xCE24") ; HANGUL SYLLABLE CHIEUCH-EU-NIEUN
       (?$(CCz(B . "0xCE28") ; HANGUL SYLLABLE CHIEUCH-EU-RIEUL
       (?$(CC{(B . "0xCE30") ; HANGUL SYLLABLE CHIEUCH-EU-MIEUM
       (?$(CC|(B . "0xCE31") ; HANGUL SYLLABLE CHIEUCH-EU-PIEUP
       (?$(CC}(B . "0xCE33") ; HANGUL SYLLABLE CHIEUCH-EU-SIOS
       (?$(CC~(B . "0xCE35") ; HANGUL SYLLABLE CHIEUCH-EU-IEUNG
       (?$(CD!(B . "0xCE58") ; HANGUL SYLLABLE CHIEUCH-I
       (?$(CD"(B . "0xCE59") ; HANGUL SYLLABLE CHIEUCH-I-KIYEOK
       (?$(CD#(B . "0xCE5C") ; HANGUL SYLLABLE CHIEUCH-I-NIEUN
       (?$(CD$(B . "0xCE5F") ; HANGUL SYLLABLE CHIEUCH-I-TIKEUT
       (?$(CD%(B . "0xCE60") ; HANGUL SYLLABLE CHIEUCH-I-RIEUL
       (?$(CD&(B . "0xCE61") ; HANGUL SYLLABLE CHIEUCH-I-RIEULKIYEOK
       (?$(CD'(B . "0xCE68") ; HANGUL SYLLABLE CHIEUCH-I-MIEUM
       (?$(CD((B . "0xCE69") ; HANGUL SYLLABLE CHIEUCH-I-PIEUP
       (?$(CD)(B . "0xCE6B") ; HANGUL SYLLABLE CHIEUCH-I-SIOS
       (?$(CD*(B . "0xCE6D") ; HANGUL SYLLABLE CHIEUCH-I-IEUNG
       (?$(CD+(B . "0xCE74") ; HANGUL SYLLABLE KHIEUKH-A
       (?$(CD,(B . "0xCE75") ; HANGUL SYLLABLE KHIEUKH-A-KIYEOK
       (?$(CD-(B . "0xCE78") ; HANGUL SYLLABLE KHIEUKH-A-NIEUN
       (?$(CD.(B . "0xCE7C") ; HANGUL SYLLABLE KHIEUKH-A-RIEUL
       (?$(CD/(B . "0xCE84") ; HANGUL SYLLABLE KHIEUKH-A-MIEUM
       (?$(CD0(B . "0xCE85") ; HANGUL SYLLABLE KHIEUKH-A-PIEUP
       (?$(CD1(B . "0xCE87") ; HANGUL SYLLABLE KHIEUKH-A-SIOS
       (?$(CD2(B . "0xCE89") ; HANGUL SYLLABLE KHIEUKH-A-IEUNG
       (?$(CD3(B . "0xCE90") ; HANGUL SYLLABLE KHIEUKH-AE
       (?$(CD4(B . "0xCE91") ; HANGUL SYLLABLE KHIEUKH-AE-KIYEOK
       (?$(CD5(B . "0xCE94") ; HANGUL SYLLABLE KHIEUKH-AE-NIEUN
       (?$(CD6(B . "0xCE98") ; HANGUL SYLLABLE KHIEUKH-AE-RIEUL
       (?$(CD7(B . "0xCEA0") ; HANGUL SYLLABLE KHIEUKH-AE-MIEUM
       (?$(CD8(B . "0xCEA1") ; HANGUL SYLLABLE KHIEUKH-AE-PIEUP
       (?$(CD9(B . "0xCEA3") ; HANGUL SYLLABLE KHIEUKH-AE-SIOS
       (?$(CD:(B . "0xCEA4") ; HANGUL SYLLABLE KHIEUKH-AE-SSANGSIOS
       (?$(CD;(B . "0xCEA5") ; HANGUL SYLLABLE KHIEUKH-AE-IEUNG
       (?$(CD<(B . "0xCEAC") ; HANGUL SYLLABLE KHIEUKH-YA
       (?$(CD=(B . "0xCEAD") ; HANGUL SYLLABLE KHIEUKH-YA-KIYEOK
       (?$(CD>(B . "0xCEC1") ; HANGUL SYLLABLE KHIEUKH-YA-IEUNG
       (?$(CD?(B . "0xCEE4") ; HANGUL SYLLABLE KHIEUKH-EO
       (?$(CD@(B . "0xCEE5") ; HANGUL SYLLABLE KHIEUKH-EO-KIYEOK
       (?$(CDA(B . "0xCEE8") ; HANGUL SYLLABLE KHIEUKH-EO-NIEUN
       (?$(CDB(B . "0xCEEB") ; HANGUL SYLLABLE KHIEUKH-EO-TIKEUT
       (?$(CDC(B . "0xCEEC") ; HANGUL SYLLABLE KHIEUKH-EO-RIEUL
       (?$(CDD(B . "0xCEF4") ; HANGUL SYLLABLE KHIEUKH-EO-MIEUM
       (?$(CDE(B . "0xCEF5") ; HANGUL SYLLABLE KHIEUKH-EO-PIEUP
       (?$(CDF(B . "0xCEF7") ; HANGUL SYLLABLE KHIEUKH-EO-SIOS
       (?$(CDG(B . "0xCEF8") ; HANGUL SYLLABLE KHIEUKH-EO-SSANGSIOS
       (?$(CDH(B . "0xCEF9") ; HANGUL SYLLABLE KHIEUKH-EO-IEUNG
       (?$(CDI(B . "0xCF00") ; HANGUL SYLLABLE KHIEUKH-E
       (?$(CDJ(B . "0xCF01") ; HANGUL SYLLABLE KHIEUKH-E-KIYEOK
       (?$(CDK(B . "0xCF04") ; HANGUL SYLLABLE KHIEUKH-E-NIEUN
       (?$(CDL(B . "0xCF08") ; HANGUL SYLLABLE KHIEUKH-E-RIEUL
       (?$(CDM(B . "0xCF10") ; HANGUL SYLLABLE KHIEUKH-E-MIEUM
       (?$(CDN(B . "0xCF11") ; HANGUL SYLLABLE KHIEUKH-E-PIEUP
       (?$(CDO(B . "0xCF13") ; HANGUL SYLLABLE KHIEUKH-E-SIOS
       (?$(CDP(B . "0xCF15") ; HANGUL SYLLABLE KHIEUKH-E-IEUNG
       (?$(CDQ(B . "0xCF1C") ; HANGUL SYLLABLE KHIEUKH-YEO
       (?$(CDR(B . "0xCF20") ; HANGUL SYLLABLE KHIEUKH-YEO-NIEUN
       (?$(CDS(B . "0xCF24") ; HANGUL SYLLABLE KHIEUKH-YEO-RIEUL
       (?$(CDT(B . "0xCF2C") ; HANGUL SYLLABLE KHIEUKH-YEO-MIEUM
       (?$(CDU(B . "0xCF2D") ; HANGUL SYLLABLE KHIEUKH-YEO-PIEUP
       (?$(CDV(B . "0xCF2F") ; HANGUL SYLLABLE KHIEUKH-YEO-SIOS
       (?$(CDW(B . "0xCF30") ; HANGUL SYLLABLE KHIEUKH-YEO-SSANGSIOS
       (?$(CDX(B . "0xCF31") ; HANGUL SYLLABLE KHIEUKH-YEO-IEUNG
       (?$(CDY(B . "0xCF38") ; HANGUL SYLLABLE KHIEUKH-YE
       (?$(CDZ(B . "0xCF54") ; HANGUL SYLLABLE KHIEUKH-O
       (?$(CD[(B . "0xCF55") ; HANGUL SYLLABLE KHIEUKH-O-KIYEOK
       (?$(CD\(B . "0xCF58") ; HANGUL SYLLABLE KHIEUKH-O-NIEUN
       (?$(CD](B . "0xCF5C") ; HANGUL SYLLABLE KHIEUKH-O-RIEUL
       (?$(CD^(B . "0xCF64") ; HANGUL SYLLABLE KHIEUKH-O-MIEUM
       (?$(CD_(B . "0xCF65") ; HANGUL SYLLABLE KHIEUKH-O-PIEUP
       (?$(CD`(B . "0xCF67") ; HANGUL SYLLABLE KHIEUKH-O-SIOS
       (?$(CDa(B . "0xCF69") ; HANGUL SYLLABLE KHIEUKH-O-IEUNG
       (?$(CDb(B . "0xCF70") ; HANGUL SYLLABLE KHIEUKH-WA
       (?$(CDc(B . "0xCF71") ; HANGUL SYLLABLE KHIEUKH-WA-KIYEOK
       (?$(CDd(B . "0xCF74") ; HANGUL SYLLABLE KHIEUKH-WA-NIEUN
       (?$(CDe(B . "0xCF78") ; HANGUL SYLLABLE KHIEUKH-WA-RIEUL
       (?$(CDf(B . "0xCF80") ; HANGUL SYLLABLE KHIEUKH-WA-MIEUM
       (?$(CDg(B . "0xCF85") ; HANGUL SYLLABLE KHIEUKH-WA-IEUNG
       (?$(CDh(B . "0xCF8C") ; HANGUL SYLLABLE KHIEUKH-WAE
       (?$(CDi(B . "0xCFA1") ; HANGUL SYLLABLE KHIEUKH-WAE-IEUNG
       (?$(CDj(B . "0xCFA8") ; HANGUL SYLLABLE KHIEUKH-OE
       (?$(CDk(B . "0xCFB0") ; HANGUL SYLLABLE KHIEUKH-OE-RIEUL
       (?$(CDl(B . "0xCFC4") ; HANGUL SYLLABLE KHIEUKH-YO
       (?$(CDm(B . "0xCFE0") ; HANGUL SYLLABLE KHIEUKH-U
       (?$(CDn(B . "0xCFE1") ; HANGUL SYLLABLE KHIEUKH-U-KIYEOK
       (?$(CDo(B . "0xCFE4") ; HANGUL SYLLABLE KHIEUKH-U-NIEUN
       (?$(CDp(B . "0xCFE8") ; HANGUL SYLLABLE KHIEUKH-U-RIEUL
       (?$(CDq(B . "0xCFF0") ; HANGUL SYLLABLE KHIEUKH-U-MIEUM
       (?$(CDr(B . "0xCFF1") ; HANGUL SYLLABLE KHIEUKH-U-PIEUP
       (?$(CDs(B . "0xCFF3") ; HANGUL SYLLABLE KHIEUKH-U-SIOS
       (?$(CDt(B . "0xCFF5") ; HANGUL SYLLABLE KHIEUKH-U-IEUNG
       (?$(CDu(B . "0xCFFC") ; HANGUL SYLLABLE KHIEUKH-WEO
       (?$(CDv(B . "0xD000") ; HANGUL SYLLABLE KHIEUKH-WEO-NIEUN
       (?$(CDw(B . "0xD004") ; HANGUL SYLLABLE KHIEUKH-WEO-RIEUL
       (?$(CDx(B . "0xD011") ; HANGUL SYLLABLE KHIEUKH-WEO-IEUNG
       (?$(CDy(B . "0xD018") ; HANGUL SYLLABLE KHIEUKH-WE
       (?$(CDz(B . "0xD02D") ; HANGUL SYLLABLE KHIEUKH-WE-IEUNG
       (?$(CD{(B . "0xD034") ; HANGUL SYLLABLE KHIEUKH-WI
       (?$(CD|(B . "0xD035") ; HANGUL SYLLABLE KHIEUKH-WI-KIYEOK
       (?$(CD}(B . "0xD038") ; HANGUL SYLLABLE KHIEUKH-WI-NIEUN
       (?$(CD~(B . "0xD03C") ; HANGUL SYLLABLE KHIEUKH-WI-RIEUL
       (?$(CE!(B . "0xD044") ; HANGUL SYLLABLE KHIEUKH-WI-MIEUM
       (?$(CE"(B . "0xD045") ; HANGUL SYLLABLE KHIEUKH-WI-PIEUP
       (?$(CE#(B . "0xD047") ; HANGUL SYLLABLE KHIEUKH-WI-SIOS
       (?$(CE$(B . "0xD049") ; HANGUL SYLLABLE KHIEUKH-WI-IEUNG
       (?$(CE%(B . "0xD050") ; HANGUL SYLLABLE KHIEUKH-YU
       (?$(CE&(B . "0xD054") ; HANGUL SYLLABLE KHIEUKH-YU-NIEUN
       (?$(CE'(B . "0xD058") ; HANGUL SYLLABLE KHIEUKH-YU-RIEUL
       (?$(CE((B . "0xD060") ; HANGUL SYLLABLE KHIEUKH-YU-MIEUM
       (?$(CE)(B . "0xD06C") ; HANGUL SYLLABLE KHIEUKH-EU
       (?$(CE*(B . "0xD06D") ; HANGUL SYLLABLE KHIEUKH-EU-KIYEOK
       (?$(CE+(B . "0xD070") ; HANGUL SYLLABLE KHIEUKH-EU-NIEUN
       (?$(CE,(B . "0xD074") ; HANGUL SYLLABLE KHIEUKH-EU-RIEUL
       (?$(CE-(B . "0xD07C") ; HANGUL SYLLABLE KHIEUKH-EU-MIEUM
       (?$(CE.(B . "0xD07D") ; HANGUL SYLLABLE KHIEUKH-EU-PIEUP
       (?$(CE/(B . "0xD081") ; HANGUL SYLLABLE KHIEUKH-EU-IEUNG
       (?$(CE0(B . "0xD0A4") ; HANGUL SYLLABLE KHIEUKH-I
       (?$(CE1(B . "0xD0A5") ; HANGUL SYLLABLE KHIEUKH-I-KIYEOK
       (?$(CE2(B . "0xD0A8") ; HANGUL SYLLABLE KHIEUKH-I-NIEUN
       (?$(CE3(B . "0xD0AC") ; HANGUL SYLLABLE KHIEUKH-I-RIEUL
       (?$(CE4(B . "0xD0B4") ; HANGUL SYLLABLE KHIEUKH-I-MIEUM
       (?$(CE5(B . "0xD0B5") ; HANGUL SYLLABLE KHIEUKH-I-PIEUP
       (?$(CE6(B . "0xD0B7") ; HANGUL SYLLABLE KHIEUKH-I-SIOS
       (?$(CE7(B . "0xD0B9") ; HANGUL SYLLABLE KHIEUKH-I-IEUNG
       (?$(CE8(B . "0xD0C0") ; HANGUL SYLLABLE THIEUTH-A
       (?$(CE9(B . "0xD0C1") ; HANGUL SYLLABLE THIEUTH-A-KIYEOK
       (?$(CE:(B . "0xD0C4") ; HANGUL SYLLABLE THIEUTH-A-NIEUN
       (?$(CE;(B . "0xD0C8") ; HANGUL SYLLABLE THIEUTH-A-RIEUL
       (?$(CE<(B . "0xD0C9") ; HANGUL SYLLABLE THIEUTH-A-RIEULKIYEOK
       (?$(CE=(B . "0xD0D0") ; HANGUL SYLLABLE THIEUTH-A-MIEUM
       (?$(CE>(B . "0xD0D1") ; HANGUL SYLLABLE THIEUTH-A-PIEUP
       (?$(CE?(B . "0xD0D3") ; HANGUL SYLLABLE THIEUTH-A-SIOS
       (?$(CE@(B . "0xD0D4") ; HANGUL SYLLABLE THIEUTH-A-SSANGSIOS
       (?$(CEA(B . "0xD0D5") ; HANGUL SYLLABLE THIEUTH-A-IEUNG
       (?$(CEB(B . "0xD0DC") ; HANGUL SYLLABLE THIEUTH-AE
       (?$(CEC(B . "0xD0DD") ; HANGUL SYLLABLE THIEUTH-AE-KIYEOK
       (?$(CED(B . "0xD0E0") ; HANGUL SYLLABLE THIEUTH-AE-NIEUN
       (?$(CEE(B . "0xD0E4") ; HANGUL SYLLABLE THIEUTH-AE-RIEUL
       (?$(CEF(B . "0xD0EC") ; HANGUL SYLLABLE THIEUTH-AE-MIEUM
       (?$(CEG(B . "0xD0ED") ; HANGUL SYLLABLE THIEUTH-AE-PIEUP
       (?$(CEH(B . "0xD0EF") ; HANGUL SYLLABLE THIEUTH-AE-SIOS
       (?$(CEI(B . "0xD0F0") ; HANGUL SYLLABLE THIEUTH-AE-SSANGSIOS
       (?$(CEJ(B . "0xD0F1") ; HANGUL SYLLABLE THIEUTH-AE-IEUNG
       (?$(CEK(B . "0xD0F8") ; HANGUL SYLLABLE THIEUTH-YA
       (?$(CEL(B . "0xD10D") ; HANGUL SYLLABLE THIEUTH-YA-IEUNG
       (?$(CEM(B . "0xD130") ; HANGUL SYLLABLE THIEUTH-EO
       (?$(CEN(B . "0xD131") ; HANGUL SYLLABLE THIEUTH-EO-KIYEOK
       (?$(CEO(B . "0xD134") ; HANGUL SYLLABLE THIEUTH-EO-NIEUN
       (?$(CEP(B . "0xD138") ; HANGUL SYLLABLE THIEUTH-EO-RIEUL
       (?$(CEQ(B . "0xD13A") ; HANGUL SYLLABLE THIEUTH-EO-RIEULMIEUM
       (?$(CER(B . "0xD140") ; HANGUL SYLLABLE THIEUTH-EO-MIEUM
       (?$(CES(B . "0xD141") ; HANGUL SYLLABLE THIEUTH-EO-PIEUP
       (?$(CET(B . "0xD143") ; HANGUL SYLLABLE THIEUTH-EO-SIOS
       (?$(CEU(B . "0xD144") ; HANGUL SYLLABLE THIEUTH-EO-SSANGSIOS
       (?$(CEV(B . "0xD145") ; HANGUL SYLLABLE THIEUTH-EO-IEUNG
       (?$(CEW(B . "0xD14C") ; HANGUL SYLLABLE THIEUTH-E
       (?$(CEX(B . "0xD14D") ; HANGUL SYLLABLE THIEUTH-E-KIYEOK
       (?$(CEY(B . "0xD150") ; HANGUL SYLLABLE THIEUTH-E-NIEUN
       (?$(CEZ(B . "0xD154") ; HANGUL SYLLABLE THIEUTH-E-RIEUL
       (?$(CE[(B . "0xD15C") ; HANGUL SYLLABLE THIEUTH-E-MIEUM
       (?$(CE\(B . "0xD15D") ; HANGUL SYLLABLE THIEUTH-E-PIEUP
       (?$(CE](B . "0xD15F") ; HANGUL SYLLABLE THIEUTH-E-SIOS
       (?$(CE^(B . "0xD161") ; HANGUL SYLLABLE THIEUTH-E-IEUNG
       (?$(CE_(B . "0xD168") ; HANGUL SYLLABLE THIEUTH-YEO
       (?$(CE`(B . "0xD16C") ; HANGUL SYLLABLE THIEUTH-YEO-NIEUN
       (?$(CEa(B . "0xD17C") ; HANGUL SYLLABLE THIEUTH-YEO-SSANGSIOS
       (?$(CEb(B . "0xD184") ; HANGUL SYLLABLE THIEUTH-YE
       (?$(CEc(B . "0xD188") ; HANGUL SYLLABLE THIEUTH-YE-NIEUN
       (?$(CEd(B . "0xD1A0") ; HANGUL SYLLABLE THIEUTH-O
       (?$(CEe(B . "0xD1A1") ; HANGUL SYLLABLE THIEUTH-O-KIYEOK
       (?$(CEf(B . "0xD1A4") ; HANGUL SYLLABLE THIEUTH-O-NIEUN
       (?$(CEg(B . "0xD1A8") ; HANGUL SYLLABLE THIEUTH-O-RIEUL
       (?$(CEh(B . "0xD1B0") ; HANGUL SYLLABLE THIEUTH-O-MIEUM
       (?$(CEi(B . "0xD1B1") ; HANGUL SYLLABLE THIEUTH-O-PIEUP
       (?$(CEj(B . "0xD1B3") ; HANGUL SYLLABLE THIEUTH-O-SIOS
       (?$(CEk(B . "0xD1B5") ; HANGUL SYLLABLE THIEUTH-O-IEUNG
       (?$(CEl(B . "0xD1BA") ; HANGUL SYLLABLE THIEUTH-O-PHIEUPH
       (?$(CEm(B . "0xD1BC") ; HANGUL SYLLABLE THIEUTH-WA
       (?$(CEn(B . "0xD1C0") ; HANGUL SYLLABLE THIEUTH-WA-NIEUN
       (?$(CEo(B . "0xD1D8") ; HANGUL SYLLABLE THIEUTH-WAE
       (?$(CEp(B . "0xD1F4") ; HANGUL SYLLABLE THIEUTH-OE
       (?$(CEq(B . "0xD1F8") ; HANGUL SYLLABLE THIEUTH-OE-NIEUN
       (?$(CEr(B . "0xD207") ; HANGUL SYLLABLE THIEUTH-OE-SIOS
       (?$(CEs(B . "0xD209") ; HANGUL SYLLABLE THIEUTH-OE-IEUNG
       (?$(CEt(B . "0xD210") ; HANGUL SYLLABLE THIEUTH-YO
       (?$(CEu(B . "0xD22C") ; HANGUL SYLLABLE THIEUTH-U
       (?$(CEv(B . "0xD22D") ; HANGUL SYLLABLE THIEUTH-U-KIYEOK
       (?$(CEw(B . "0xD230") ; HANGUL SYLLABLE THIEUTH-U-NIEUN
       (?$(CEx(B . "0xD234") ; HANGUL SYLLABLE THIEUTH-U-RIEUL
       (?$(CEy(B . "0xD23C") ; HANGUL SYLLABLE THIEUTH-U-MIEUM
       (?$(CEz(B . "0xD23D") ; HANGUL SYLLABLE THIEUTH-U-PIEUP
       (?$(CE{(B . "0xD23F") ; HANGUL SYLLABLE THIEUTH-U-SIOS
       (?$(CE|(B . "0xD241") ; HANGUL SYLLABLE THIEUTH-U-IEUNG
       (?$(CE}(B . "0xD248") ; HANGUL SYLLABLE THIEUTH-WEO
       (?$(CE~(B . "0xD25C") ; HANGUL SYLLABLE THIEUTH-WEO-SSANGSIOS
       (?$(CF!(B . "0xD264") ; HANGUL SYLLABLE THIEUTH-WE
       (?$(CF"(B . "0xD280") ; HANGUL SYLLABLE THIEUTH-WI
       (?$(CF#(B . "0xD281") ; HANGUL SYLLABLE THIEUTH-WI-KIYEOK
       (?$(CF$(B . "0xD284") ; HANGUL SYLLABLE THIEUTH-WI-NIEUN
       (?$(CF%(B . "0xD288") ; HANGUL SYLLABLE THIEUTH-WI-RIEUL
       (?$(CF&(B . "0xD290") ; HANGUL SYLLABLE THIEUTH-WI-MIEUM
       (?$(CF'(B . "0xD291") ; HANGUL SYLLABLE THIEUTH-WI-PIEUP
       (?$(CF((B . "0xD295") ; HANGUL SYLLABLE THIEUTH-WI-IEUNG
       (?$(CF)(B . "0xD29C") ; HANGUL SYLLABLE THIEUTH-YU
       (?$(CF*(B . "0xD2A0") ; HANGUL SYLLABLE THIEUTH-YU-NIEUN
       (?$(CF+(B . "0xD2A4") ; HANGUL SYLLABLE THIEUTH-YU-RIEUL
       (?$(CF,(B . "0xD2AC") ; HANGUL SYLLABLE THIEUTH-YU-MIEUM
       (?$(CF-(B . "0xD2B1") ; HANGUL SYLLABLE THIEUTH-YU-IEUNG
       (?$(CF.(B . "0xD2B8") ; HANGUL SYLLABLE THIEUTH-EU
       (?$(CF/(B . "0xD2B9") ; HANGUL SYLLABLE THIEUTH-EU-KIYEOK
       (?$(CF0(B . "0xD2BC") ; HANGUL SYLLABLE THIEUTH-EU-NIEUN
       (?$(CF1(B . "0xD2BF") ; HANGUL SYLLABLE THIEUTH-EU-TIKEUT
       (?$(CF2(B . "0xD2C0") ; HANGUL SYLLABLE THIEUTH-EU-RIEUL
       (?$(CF3(B . "0xD2C2") ; HANGUL SYLLABLE THIEUTH-EU-RIEULMIEUM
       (?$(CF4(B . "0xD2C8") ; HANGUL SYLLABLE THIEUTH-EU-MIEUM
       (?$(CF5(B . "0xD2C9") ; HANGUL SYLLABLE THIEUTH-EU-PIEUP
       (?$(CF6(B . "0xD2CB") ; HANGUL SYLLABLE THIEUTH-EU-SIOS
       (?$(CF7(B . "0xD2D4") ; HANGUL SYLLABLE THIEUTH-YI
       (?$(CF8(B . "0xD2D8") ; HANGUL SYLLABLE THIEUTH-YI-NIEUN
       (?$(CF9(B . "0xD2DC") ; HANGUL SYLLABLE THIEUTH-YI-RIEUL
       (?$(CF:(B . "0xD2E4") ; HANGUL SYLLABLE THIEUTH-YI-MIEUM
       (?$(CF;(B . "0xD2E5") ; HANGUL SYLLABLE THIEUTH-YI-PIEUP
       (?$(CF<(B . "0xD2F0") ; HANGUL SYLLABLE THIEUTH-I
       (?$(CF=(B . "0xD2F1") ; HANGUL SYLLABLE THIEUTH-I-KIYEOK
       (?$(CF>(B . "0xD2F4") ; HANGUL SYLLABLE THIEUTH-I-NIEUN
       (?$(CF?(B . "0xD2F8") ; HANGUL SYLLABLE THIEUTH-I-RIEUL
       (?$(CF@(B . "0xD300") ; HANGUL SYLLABLE THIEUTH-I-MIEUM
       (?$(CFA(B . "0xD301") ; HANGUL SYLLABLE THIEUTH-I-PIEUP
       (?$(CFB(B . "0xD303") ; HANGUL SYLLABLE THIEUTH-I-SIOS
       (?$(CFC(B . "0xD305") ; HANGUL SYLLABLE THIEUTH-I-IEUNG
       (?$(CFD(B . "0xD30C") ; HANGUL SYLLABLE PHIEUPH-A
       (?$(CFE(B . "0xD30D") ; HANGUL SYLLABLE PHIEUPH-A-KIYEOK
       (?$(CFF(B . "0xD30E") ; HANGUL SYLLABLE PHIEUPH-A-SSANGKIYEOK
       (?$(CFG(B . "0xD310") ; HANGUL SYLLABLE PHIEUPH-A-NIEUN
       (?$(CFH(B . "0xD314") ; HANGUL SYLLABLE PHIEUPH-A-RIEUL
       (?$(CFI(B . "0xD316") ; HANGUL SYLLABLE PHIEUPH-A-RIEULMIEUM
       (?$(CFJ(B . "0xD31C") ; HANGUL SYLLABLE PHIEUPH-A-MIEUM
       (?$(CFK(B . "0xD31D") ; HANGUL SYLLABLE PHIEUPH-A-PIEUP
       (?$(CFL(B . "0xD31F") ; HANGUL SYLLABLE PHIEUPH-A-SIOS
       (?$(CFM(B . "0xD320") ; HANGUL SYLLABLE PHIEUPH-A-SSANGSIOS
       (?$(CFN(B . "0xD321") ; HANGUL SYLLABLE PHIEUPH-A-IEUNG
       (?$(CFO(B . "0xD325") ; HANGUL SYLLABLE PHIEUPH-A-THIEUTH
       (?$(CFP(B . "0xD328") ; HANGUL SYLLABLE PHIEUPH-AE
       (?$(CFQ(B . "0xD329") ; HANGUL SYLLABLE PHIEUPH-AE-KIYEOK
       (?$(CFR(B . "0xD32C") ; HANGUL SYLLABLE PHIEUPH-AE-NIEUN
       (?$(CFS(B . "0xD330") ; HANGUL SYLLABLE PHIEUPH-AE-RIEUL
       (?$(CFT(B . "0xD338") ; HANGUL SYLLABLE PHIEUPH-AE-MIEUM
       (?$(CFU(B . "0xD339") ; HANGUL SYLLABLE PHIEUPH-AE-PIEUP
       (?$(CFV(B . "0xD33B") ; HANGUL SYLLABLE PHIEUPH-AE-SIOS
       (?$(CFW(B . "0xD33C") ; HANGUL SYLLABLE PHIEUPH-AE-SSANGSIOS
       (?$(CFX(B . "0xD33D") ; HANGUL SYLLABLE PHIEUPH-AE-IEUNG
       (?$(CFY(B . "0xD344") ; HANGUL SYLLABLE PHIEUPH-YA
       (?$(CFZ(B . "0xD345") ; HANGUL SYLLABLE PHIEUPH-YA-KIYEOK
       (?$(CF[(B . "0xD37C") ; HANGUL SYLLABLE PHIEUPH-EO
       (?$(CF\(B . "0xD37D") ; HANGUL SYLLABLE PHIEUPH-EO-KIYEOK
       (?$(CF](B . "0xD380") ; HANGUL SYLLABLE PHIEUPH-EO-NIEUN
       (?$(CF^(B . "0xD384") ; HANGUL SYLLABLE PHIEUPH-EO-RIEUL
       (?$(CF_(B . "0xD38C") ; HANGUL SYLLABLE PHIEUPH-EO-MIEUM
       (?$(CF`(B . "0xD38D") ; HANGUL SYLLABLE PHIEUPH-EO-PIEUP
       (?$(CFa(B . "0xD38F") ; HANGUL SYLLABLE PHIEUPH-EO-SIOS
       (?$(CFb(B . "0xD390") ; HANGUL SYLLABLE PHIEUPH-EO-SSANGSIOS
       (?$(CFc(B . "0xD391") ; HANGUL SYLLABLE PHIEUPH-EO-IEUNG
       (?$(CFd(B . "0xD398") ; HANGUL SYLLABLE PHIEUPH-E
       (?$(CFe(B . "0xD399") ; HANGUL SYLLABLE PHIEUPH-E-KIYEOK
       (?$(CFf(B . "0xD39C") ; HANGUL SYLLABLE PHIEUPH-E-NIEUN
       (?$(CFg(B . "0xD3A0") ; HANGUL SYLLABLE PHIEUPH-E-RIEUL
       (?$(CFh(B . "0xD3A8") ; HANGUL SYLLABLE PHIEUPH-E-MIEUM
       (?$(CFi(B . "0xD3A9") ; HANGUL SYLLABLE PHIEUPH-E-PIEUP
       (?$(CFj(B . "0xD3AB") ; HANGUL SYLLABLE PHIEUPH-E-SIOS
       (?$(CFk(B . "0xD3AD") ; HANGUL SYLLABLE PHIEUPH-E-IEUNG
       (?$(CFl(B . "0xD3B4") ; HANGUL SYLLABLE PHIEUPH-YEO
       (?$(CFm(B . "0xD3B8") ; HANGUL SYLLABLE PHIEUPH-YEO-NIEUN
       (?$(CFn(B . "0xD3BC") ; HANGUL SYLLABLE PHIEUPH-YEO-RIEUL
       (?$(CFo(B . "0xD3C4") ; HANGUL SYLLABLE PHIEUPH-YEO-MIEUM
       (?$(CFp(B . "0xD3C5") ; HANGUL SYLLABLE PHIEUPH-YEO-PIEUP
       (?$(CFq(B . "0xD3C8") ; HANGUL SYLLABLE PHIEUPH-YEO-SSANGSIOS
       (?$(CFr(B . "0xD3C9") ; HANGUL SYLLABLE PHIEUPH-YEO-IEUNG
       (?$(CFs(B . "0xD3D0") ; HANGUL SYLLABLE PHIEUPH-YE
       (?$(CFt(B . "0xD3D8") ; HANGUL SYLLABLE PHIEUPH-YE-RIEUL
       (?$(CFu(B . "0xD3E1") ; HANGUL SYLLABLE PHIEUPH-YE-PIEUP
       (?$(CFv(B . "0xD3E3") ; HANGUL SYLLABLE PHIEUPH-YE-SIOS
       (?$(CFw(B . "0xD3EC") ; HANGUL SYLLABLE PHIEUPH-O
       (?$(CFx(B . "0xD3ED") ; HANGUL SYLLABLE PHIEUPH-O-KIYEOK
       (?$(CFy(B . "0xD3F0") ; HANGUL SYLLABLE PHIEUPH-O-NIEUN
       (?$(CFz(B . "0xD3F4") ; HANGUL SYLLABLE PHIEUPH-O-RIEUL
       (?$(CF{(B . "0xD3FC") ; HANGUL SYLLABLE PHIEUPH-O-MIEUM
       (?$(CF|(B . "0xD3FD") ; HANGUL SYLLABLE PHIEUPH-O-PIEUP
       (?$(CF}(B . "0xD3FF") ; HANGUL SYLLABLE PHIEUPH-O-SIOS
       (?$(CF~(B . "0xD401") ; HANGUL SYLLABLE PHIEUPH-O-IEUNG
       (?$(CG!(B . "0xD408") ; HANGUL SYLLABLE PHIEUPH-WA
       (?$(CG"(B . "0xD41D") ; HANGUL SYLLABLE PHIEUPH-WA-IEUNG
       (?$(CG#(B . "0xD440") ; HANGUL SYLLABLE PHIEUPH-OE
       (?$(CG$(B . "0xD444") ; HANGUL SYLLABLE PHIEUPH-OE-NIEUN
       (?$(CG%(B . "0xD45C") ; HANGUL SYLLABLE PHIEUPH-YO
       (?$(CG&(B . "0xD460") ; HANGUL SYLLABLE PHIEUPH-YO-NIEUN
       (?$(CG'(B . "0xD464") ; HANGUL SYLLABLE PHIEUPH-YO-RIEUL
       (?$(CG((B . "0xD46D") ; HANGUL SYLLABLE PHIEUPH-YO-PIEUP
       (?$(CG)(B . "0xD46F") ; HANGUL SYLLABLE PHIEUPH-YO-SIOS
       (?$(CG*(B . "0xD478") ; HANGUL SYLLABLE PHIEUPH-U
       (?$(CG+(B . "0xD479") ; HANGUL SYLLABLE PHIEUPH-U-KIYEOK
       (?$(CG,(B . "0xD47C") ; HANGUL SYLLABLE PHIEUPH-U-NIEUN
       (?$(CG-(B . "0xD47F") ; HANGUL SYLLABLE PHIEUPH-U-TIKEUT
       (?$(CG.(B . "0xD480") ; HANGUL SYLLABLE PHIEUPH-U-RIEUL
       (?$(CG/(B . "0xD482") ; HANGUL SYLLABLE PHIEUPH-U-RIEULMIEUM
       (?$(CG0(B . "0xD488") ; HANGUL SYLLABLE PHIEUPH-U-MIEUM
       (?$(CG1(B . "0xD489") ; HANGUL SYLLABLE PHIEUPH-U-PIEUP
       (?$(CG2(B . "0xD48B") ; HANGUL SYLLABLE PHIEUPH-U-SIOS
       (?$(CG3(B . "0xD48D") ; HANGUL SYLLABLE PHIEUPH-U-IEUNG
       (?$(CG4(B . "0xD494") ; HANGUL SYLLABLE PHIEUPH-WEO
       (?$(CG5(B . "0xD4A9") ; HANGUL SYLLABLE PHIEUPH-WEO-IEUNG
       (?$(CG6(B . "0xD4CC") ; HANGUL SYLLABLE PHIEUPH-WI
       (?$(CG7(B . "0xD4D0") ; HANGUL SYLLABLE PHIEUPH-WI-NIEUN
       (?$(CG8(B . "0xD4D4") ; HANGUL SYLLABLE PHIEUPH-WI-RIEUL
       (?$(CG9(B . "0xD4DC") ; HANGUL SYLLABLE PHIEUPH-WI-MIEUM
       (?$(CG:(B . "0xD4DF") ; HANGUL SYLLABLE PHIEUPH-WI-SIOS
       (?$(CG;(B . "0xD4E8") ; HANGUL SYLLABLE PHIEUPH-YU
       (?$(CG<(B . "0xD4EC") ; HANGUL SYLLABLE PHIEUPH-YU-NIEUN
       (?$(CG=(B . "0xD4F0") ; HANGUL SYLLABLE PHIEUPH-YU-RIEUL
       (?$(CG>(B . "0xD4F8") ; HANGUL SYLLABLE PHIEUPH-YU-MIEUM
       (?$(CG?(B . "0xD4FB") ; HANGUL SYLLABLE PHIEUPH-YU-SIOS
       (?$(CG@(B . "0xD4FD") ; HANGUL SYLLABLE PHIEUPH-YU-IEUNG
       (?$(CGA(B . "0xD504") ; HANGUL SYLLABLE PHIEUPH-EU
       (?$(CGB(B . "0xD508") ; HANGUL SYLLABLE PHIEUPH-EU-NIEUN
       (?$(CGC(B . "0xD50C") ; HANGUL SYLLABLE PHIEUPH-EU-RIEUL
       (?$(CGD(B . "0xD514") ; HANGUL SYLLABLE PHIEUPH-EU-MIEUM
       (?$(CGE(B . "0xD515") ; HANGUL SYLLABLE PHIEUPH-EU-PIEUP
       (?$(CGF(B . "0xD517") ; HANGUL SYLLABLE PHIEUPH-EU-SIOS
       (?$(CGG(B . "0xD53C") ; HANGUL SYLLABLE PHIEUPH-I
       (?$(CGH(B . "0xD53D") ; HANGUL SYLLABLE PHIEUPH-I-KIYEOK
       (?$(CGI(B . "0xD540") ; HANGUL SYLLABLE PHIEUPH-I-NIEUN
       (?$(CGJ(B . "0xD544") ; HANGUL SYLLABLE PHIEUPH-I-RIEUL
       (?$(CGK(B . "0xD54C") ; HANGUL SYLLABLE PHIEUPH-I-MIEUM
       (?$(CGL(B . "0xD54D") ; HANGUL SYLLABLE PHIEUPH-I-PIEUP
       (?$(CGM(B . "0xD54F") ; HANGUL SYLLABLE PHIEUPH-I-SIOS
       (?$(CGN(B . "0xD551") ; HANGUL SYLLABLE PHIEUPH-I-IEUNG
       (?$(CGO(B . "0xD558") ; HANGUL SYLLABLE HIEUH-A
       (?$(CGP(B . "0xD559") ; HANGUL SYLLABLE HIEUH-A-KIYEOK
       (?$(CGQ(B . "0xD55C") ; HANGUL SYLLABLE HIEUH-A-NIEUN
       (?$(CGR(B . "0xD560") ; HANGUL SYLLABLE HIEUH-A-RIEUL
       (?$(CGS(B . "0xD565") ; HANGUL SYLLABLE HIEUH-A-RIEULTHIEUTH
       (?$(CGT(B . "0xD568") ; HANGUL SYLLABLE HIEUH-A-MIEUM
       (?$(CGU(B . "0xD569") ; HANGUL SYLLABLE HIEUH-A-PIEUP
       (?$(CGV(B . "0xD56B") ; HANGUL SYLLABLE HIEUH-A-SIOS
       (?$(CGW(B . "0xD56D") ; HANGUL SYLLABLE HIEUH-A-IEUNG
       (?$(CGX(B . "0xD574") ; HANGUL SYLLABLE HIEUH-AE
       (?$(CGY(B . "0xD575") ; HANGUL SYLLABLE HIEUH-AE-KIYEOK
       (?$(CGZ(B . "0xD578") ; HANGUL SYLLABLE HIEUH-AE-NIEUN
       (?$(CG[(B . "0xD57C") ; HANGUL SYLLABLE HIEUH-AE-RIEUL
       (?$(CG\(B . "0xD584") ; HANGUL SYLLABLE HIEUH-AE-MIEUM
       (?$(CG](B . "0xD585") ; HANGUL SYLLABLE HIEUH-AE-PIEUP
       (?$(CG^(B . "0xD587") ; HANGUL SYLLABLE HIEUH-AE-SIOS
       (?$(CG_(B . "0xD588") ; HANGUL SYLLABLE HIEUH-AE-SSANGSIOS
       (?$(CG`(B . "0xD589") ; HANGUL SYLLABLE HIEUH-AE-IEUNG
       (?$(CGa(B . "0xD590") ; HANGUL SYLLABLE HIEUH-YA
       (?$(CGb(B . "0xD5A5") ; HANGUL SYLLABLE HIEUH-YA-IEUNG
       (?$(CGc(B . "0xD5C8") ; HANGUL SYLLABLE HIEUH-EO
       (?$(CGd(B . "0xD5C9") ; HANGUL SYLLABLE HIEUH-EO-KIYEOK
       (?$(CGe(B . "0xD5CC") ; HANGUL SYLLABLE HIEUH-EO-NIEUN
       (?$(CGf(B . "0xD5D0") ; HANGUL SYLLABLE HIEUH-EO-RIEUL
       (?$(CGg(B . "0xD5D2") ; HANGUL SYLLABLE HIEUH-EO-RIEULMIEUM
       (?$(CGh(B . "0xD5D8") ; HANGUL SYLLABLE HIEUH-EO-MIEUM
       (?$(CGi(B . "0xD5D9") ; HANGUL SYLLABLE HIEUH-EO-PIEUP
       (?$(CGj(B . "0xD5DB") ; HANGUL SYLLABLE HIEUH-EO-SIOS
       (?$(CGk(B . "0xD5DD") ; HANGUL SYLLABLE HIEUH-EO-IEUNG
       (?$(CGl(B . "0xD5E4") ; HANGUL SYLLABLE HIEUH-E
       (?$(CGm(B . "0xD5E5") ; HANGUL SYLLABLE HIEUH-E-KIYEOK
       (?$(CGn(B . "0xD5E8") ; HANGUL SYLLABLE HIEUH-E-NIEUN
       (?$(CGo(B . "0xD5EC") ; HANGUL SYLLABLE HIEUH-E-RIEUL
       (?$(CGp(B . "0xD5F4") ; HANGUL SYLLABLE HIEUH-E-MIEUM
       (?$(CGq(B . "0xD5F5") ; HANGUL SYLLABLE HIEUH-E-PIEUP
       (?$(CGr(B . "0xD5F7") ; HANGUL SYLLABLE HIEUH-E-SIOS
       (?$(CGs(B . "0xD5F9") ; HANGUL SYLLABLE HIEUH-E-IEUNG
       (?$(CGt(B . "0xD600") ; HANGUL SYLLABLE HIEUH-YEO
       (?$(CGu(B . "0xD601") ; HANGUL SYLLABLE HIEUH-YEO-KIYEOK
       (?$(CGv(B . "0xD604") ; HANGUL SYLLABLE HIEUH-YEO-NIEUN
       (?$(CGw(B . "0xD608") ; HANGUL SYLLABLE HIEUH-YEO-RIEUL
       (?$(CGx(B . "0xD610") ; HANGUL SYLLABLE HIEUH-YEO-MIEUM
       (?$(CGy(B . "0xD611") ; HANGUL SYLLABLE HIEUH-YEO-PIEUP
       (?$(CGz(B . "0xD613") ; HANGUL SYLLABLE HIEUH-YEO-SIOS
       (?$(CG{(B . "0xD614") ; HANGUL SYLLABLE HIEUH-YEO-SSANGSIOS
       (?$(CG|(B . "0xD615") ; HANGUL SYLLABLE HIEUH-YEO-IEUNG
       (?$(CG}(B . "0xD61C") ; HANGUL SYLLABLE HIEUH-YE
       (?$(CG~(B . "0xD620") ; HANGUL SYLLABLE HIEUH-YE-NIEUN
       (?$(CH!(B . "0xD624") ; HANGUL SYLLABLE HIEUH-YE-RIEUL
       (?$(CH"(B . "0xD62D") ; HANGUL SYLLABLE HIEUH-YE-PIEUP
       (?$(CH#(B . "0xD638") ; HANGUL SYLLABLE HIEUH-O
       (?$(CH$(B . "0xD639") ; HANGUL SYLLABLE HIEUH-O-KIYEOK
       (?$(CH%(B . "0xD63C") ; HANGUL SYLLABLE HIEUH-O-NIEUN
       (?$(CH&(B . "0xD640") ; HANGUL SYLLABLE HIEUH-O-RIEUL
       (?$(CH'(B . "0xD645") ; HANGUL SYLLABLE HIEUH-O-RIEULTHIEUTH
       (?$(CH((B . "0xD648") ; HANGUL SYLLABLE HIEUH-O-MIEUM
       (?$(CH)(B . "0xD649") ; HANGUL SYLLABLE HIEUH-O-PIEUP
       (?$(CH*(B . "0xD64B") ; HANGUL SYLLABLE HIEUH-O-SIOS
       (?$(CH+(B . "0xD64D") ; HANGUL SYLLABLE HIEUH-O-IEUNG
       (?$(CH,(B . "0xD651") ; HANGUL SYLLABLE HIEUH-O-THIEUTH
       (?$(CH-(B . "0xD654") ; HANGUL SYLLABLE HIEUH-WA
       (?$(CH.(B . "0xD655") ; HANGUL SYLLABLE HIEUH-WA-KIYEOK
       (?$(CH/(B . "0xD658") ; HANGUL SYLLABLE HIEUH-WA-NIEUN
       (?$(CH0(B . "0xD65C") ; HANGUL SYLLABLE HIEUH-WA-RIEUL
       (?$(CH1(B . "0xD667") ; HANGUL SYLLABLE HIEUH-WA-SIOS
       (?$(CH2(B . "0xD669") ; HANGUL SYLLABLE HIEUH-WA-IEUNG
       (?$(CH3(B . "0xD670") ; HANGUL SYLLABLE HIEUH-WAE
       (?$(CH4(B . "0xD671") ; HANGUL SYLLABLE HIEUH-WAE-KIYEOK
       (?$(CH5(B . "0xD674") ; HANGUL SYLLABLE HIEUH-WAE-NIEUN
       (?$(CH6(B . "0xD683") ; HANGUL SYLLABLE HIEUH-WAE-SIOS
       (?$(CH7(B . "0xD685") ; HANGUL SYLLABLE HIEUH-WAE-IEUNG
       (?$(CH8(B . "0xD68C") ; HANGUL SYLLABLE HIEUH-OE
       (?$(CH9(B . "0xD68D") ; HANGUL SYLLABLE HIEUH-OE-KIYEOK
       (?$(CH:(B . "0xD690") ; HANGUL SYLLABLE HIEUH-OE-NIEUN
       (?$(CH;(B . "0xD694") ; HANGUL SYLLABLE HIEUH-OE-RIEUL
       (?$(CH<(B . "0xD69D") ; HANGUL SYLLABLE HIEUH-OE-PIEUP
       (?$(CH=(B . "0xD69F") ; HANGUL SYLLABLE HIEUH-OE-SIOS
       (?$(CH>(B . "0xD6A1") ; HANGUL SYLLABLE HIEUH-OE-IEUNG
       (?$(CH?(B . "0xD6A8") ; HANGUL SYLLABLE HIEUH-YO
       (?$(CH@(B . "0xD6AC") ; HANGUL SYLLABLE HIEUH-YO-NIEUN
       (?$(CHA(B . "0xD6B0") ; HANGUL SYLLABLE HIEUH-YO-RIEUL
       (?$(CHB(B . "0xD6B9") ; HANGUL SYLLABLE HIEUH-YO-PIEUP
       (?$(CHC(B . "0xD6BB") ; HANGUL SYLLABLE HIEUH-YO-SIOS
       (?$(CHD(B . "0xD6C4") ; HANGUL SYLLABLE HIEUH-U
       (?$(CHE(B . "0xD6C5") ; HANGUL SYLLABLE HIEUH-U-KIYEOK
       (?$(CHF(B . "0xD6C8") ; HANGUL SYLLABLE HIEUH-U-NIEUN
       (?$(CHG(B . "0xD6CC") ; HANGUL SYLLABLE HIEUH-U-RIEUL
       (?$(CHH(B . "0xD6D1") ; HANGUL SYLLABLE HIEUH-U-RIEULTHIEUTH
       (?$(CHI(B . "0xD6D4") ; HANGUL SYLLABLE HIEUH-U-MIEUM
       (?$(CHJ(B . "0xD6D7") ; HANGUL SYLLABLE HIEUH-U-SIOS
       (?$(CHK(B . "0xD6D9") ; HANGUL SYLLABLE HIEUH-U-IEUNG
       (?$(CHL(B . "0xD6E0") ; HANGUL SYLLABLE HIEUH-WEO
       (?$(CHM(B . "0xD6E4") ; HANGUL SYLLABLE HIEUH-WEO-NIEUN
       (?$(CHN(B . "0xD6E8") ; HANGUL SYLLABLE HIEUH-WEO-RIEUL
       (?$(CHO(B . "0xD6F0") ; HANGUL SYLLABLE HIEUH-WEO-MIEUM
       (?$(CHP(B . "0xD6F5") ; HANGUL SYLLABLE HIEUH-WEO-IEUNG
       (?$(CHQ(B . "0xD6FC") ; HANGUL SYLLABLE HIEUH-WE
       (?$(CHR(B . "0xD6FD") ; HANGUL SYLLABLE HIEUH-WE-KIYEOK
       (?$(CHS(B . "0xD700") ; HANGUL SYLLABLE HIEUH-WE-NIEUN
       (?$(CHT(B . "0xD704") ; HANGUL SYLLABLE HIEUH-WE-RIEUL
       (?$(CHU(B . "0xD711") ; HANGUL SYLLABLE HIEUH-WE-IEUNG
       (?$(CHV(B . "0xD718") ; HANGUL SYLLABLE HIEUH-WI
       (?$(CHW(B . "0xD719") ; HANGUL SYLLABLE HIEUH-WI-KIYEOK
       (?$(CHX(B . "0xD71C") ; HANGUL SYLLABLE HIEUH-WI-NIEUN
       (?$(CHY(B . "0xD720") ; HANGUL SYLLABLE HIEUH-WI-RIEUL
       (?$(CHZ(B . "0xD728") ; HANGUL SYLLABLE HIEUH-WI-MIEUM
       (?$(CH[(B . "0xD729") ; HANGUL SYLLABLE HIEUH-WI-PIEUP
       (?$(CH\(B . "0xD72B") ; HANGUL SYLLABLE HIEUH-WI-SIOS
       (?$(CH](B . "0xD72D") ; HANGUL SYLLABLE HIEUH-WI-IEUNG
       (?$(CH^(B . "0xD734") ; HANGUL SYLLABLE HIEUH-YU
       (?$(CH_(B . "0xD735") ; HANGUL SYLLABLE HIEUH-YU-KIYEOK
       (?$(CH`(B . "0xD738") ; HANGUL SYLLABLE HIEUH-YU-NIEUN
       (?$(CHa(B . "0xD73C") ; HANGUL SYLLABLE HIEUH-YU-RIEUL
       (?$(CHb(B . "0xD744") ; HANGUL SYLLABLE HIEUH-YU-MIEUM
       (?$(CHc(B . "0xD747") ; HANGUL SYLLABLE HIEUH-YU-SIOS
       (?$(CHd(B . "0xD749") ; HANGUL SYLLABLE HIEUH-YU-IEUNG
       (?$(CHe(B . "0xD750") ; HANGUL SYLLABLE HIEUH-EU
       (?$(CHf(B . "0xD751") ; HANGUL SYLLABLE HIEUH-EU-KIYEOK
       (?$(CHg(B . "0xD754") ; HANGUL SYLLABLE HIEUH-EU-NIEUN
       (?$(CHh(B . "0xD756") ; HANGUL SYLLABLE HIEUH-EU-NIEUNHIEUH
       (?$(CHi(B . "0xD757") ; HANGUL SYLLABLE HIEUH-EU-TIKEUT
       (?$(CHj(B . "0xD758") ; HANGUL SYLLABLE HIEUH-EU-RIEUL
       (?$(CHk(B . "0xD759") ; HANGUL SYLLABLE HIEUH-EU-RIEULKIYEOK
       (?$(CHl(B . "0xD760") ; HANGUL SYLLABLE HIEUH-EU-MIEUM
       (?$(CHm(B . "0xD761") ; HANGUL SYLLABLE HIEUH-EU-PIEUP
       (?$(CHn(B . "0xD763") ; HANGUL SYLLABLE HIEUH-EU-SIOS
       (?$(CHo(B . "0xD765") ; HANGUL SYLLABLE HIEUH-EU-IEUNG
       (?$(CHp(B . "0xD769") ; HANGUL SYLLABLE HIEUH-EU-THIEUTH
       (?$(CHq(B . "0xD76C") ; HANGUL SYLLABLE HIEUH-YI
       (?$(CHr(B . "0xD770") ; HANGUL SYLLABLE HIEUH-YI-NIEUN
       (?$(CHs(B . "0xD774") ; HANGUL SYLLABLE HIEUH-YI-RIEUL
       (?$(CHt(B . "0xD77C") ; HANGUL SYLLABLE HIEUH-YI-MIEUM
       (?$(CHu(B . "0xD77D") ; HANGUL SYLLABLE HIEUH-YI-PIEUP
       (?$(CHv(B . "0xD781") ; HANGUL SYLLABLE HIEUH-YI-IEUNG
       (?$(CHw(B . "0xD788") ; HANGUL SYLLABLE HIEUH-I
       (?$(CHx(B . "0xD789") ; HANGUL SYLLABLE HIEUH-I-KIYEOK
       (?$(CHy(B . "0xD78C") ; HANGUL SYLLABLE HIEUH-I-NIEUN
       (?$(CHz(B . "0xD790") ; HANGUL SYLLABLE HIEUH-I-RIEUL
       (?$(CH{(B . "0xD798") ; HANGUL SYLLABLE HIEUH-I-MIEUM
       (?$(CH|(B . "0xD799") ; HANGUL SYLLABLE HIEUH-I-PIEUP
       (?$(CH}(B . "0xD79B") ; HANGUL SYLLABLE HIEUH-I-SIOS
       (?$(CH~(B . "0xD79D") ; HANGUL SYLLABLE HIEUH-I-IEUNG
       (?$(CJ!(B . "0x4F3D") ; <CJK>
       (?$(CJ"(B . "0x4F73") ; <CJK>
       (?$(CJ#(B . "0x5047") ; <CJK>
       (?$(CJ$(B . "0x50F9") ; <CJK>
       (?$(CJ%(B . "0x52A0") ; <CJK>
       (?$(CJ&(B . "0x53EF") ; <CJK>
       (?$(CJ'(B . "0x5475") ; <CJK>
       (?$(CJ((B . "0x54E5") ; <CJK>
       (?$(CJ)(B . "0x5609") ; <CJK>
       (?$(CJ*(B . "0x5AC1") ; <CJK>
       (?$(CJ+(B . "0x5BB6") ; <CJK>
       (?$(CJ,(B . "0x6687") ; <CJK>
       (?$(CJ-(B . "0x67B6") ; <CJK>
       (?$(CJ.(B . "0x67B7") ; <CJK>
       (?$(CJ/(B . "0x67EF") ; <CJK>
       (?$(CJ0(B . "0x6B4C") ; <CJK>
       (?$(CJ1(B . "0x73C2") ; <CJK>
       (?$(CJ2(B . "0x75C2") ; <CJK>
       (?$(CJ3(B . "0x7A3C") ; <CJK>
       (?$(CJ4(B . "0x82DB") ; <CJK>
       (?$(CJ5(B . "0x8304") ; <CJK>
       (?$(CJ6(B . "0x8857") ; <CJK>
       (?$(CJ7(B . "0x8888") ; <CJK>
       (?$(CJ8(B . "0x8A36") ; <CJK>
       (?$(CJ9(B . "0x8CC8") ; <CJK>
       (?$(CJ:(B . "0x8DCF") ; <CJK>
       (?$(CJ;(B . "0x8EFB") ; <CJK>
       (?$(CJ<(B . "0x8FE6") ; <CJK>
       (?$(CJ=(B . "0x99D5") ; <CJK>
       (?$(CJ>(B . "0x523B") ; <CJK>
       (?$(CJ?(B . "0x5374") ; <CJK>
       (?$(CJ@(B . "0x5404") ; <CJK>
       (?$(CJA(B . "0x606A") ; <CJK>
       (?$(CJB(B . "0x6164") ; <CJK>
       (?$(CJC(B . "0x6BBC") ; <CJK>
       (?$(CJD(B . "0x73CF") ; <CJK>
       (?$(CJE(B . "0x811A") ; <CJK>
       (?$(CJF(B . "0x89BA") ; <CJK>
       (?$(CJG(B . "0x89D2") ; <CJK>
       (?$(CJH(B . "0x95A3") ; <CJK>
       (?$(CJI(B . "0x4F83") ; <CJK>
       (?$(CJJ(B . "0x520A") ; <CJK>
       (?$(CJK(B . "0x58BE") ; <CJK>
       (?$(CJL(B . "0x5978") ; <CJK>
       (?$(CJM(B . "0x59E6") ; <CJK>
       (?$(CJN(B . "0x5E72") ; <CJK>
       (?$(CJO(B . "0x5E79") ; <CJK>
       (?$(CJP(B . "0x61C7") ; <CJK>
       (?$(CJQ(B . "0x63C0") ; <CJK>
       (?$(CJR(B . "0x6746") ; <CJK>
       (?$(CJS(B . "0x67EC") ; <CJK>
       (?$(CJT(B . "0x687F") ; <CJK>
       (?$(CJU(B . "0x6F97") ; <CJK>
       (?$(CJV(B . "0x764E") ; <CJK>
       (?$(CJW(B . "0x770B") ; <CJK>
       (?$(CJX(B . "0x78F5") ; <CJK>
       (?$(CJY(B . "0x7A08") ; <CJK>
       (?$(CJZ(B . "0x7AFF") ; <CJK>
       (?$(CJ[(B . "0x7C21") ; <CJK>
       (?$(CJ\(B . "0x809D") ; <CJK>
       (?$(CJ](B . "0x826E") ; <CJK>
       (?$(CJ^(B . "0x8271") ; <CJK>
       (?$(CJ_(B . "0x8AEB") ; <CJK>
       (?$(CJ`(B . "0x9593") ; <CJK>
       (?$(CJa(B . "0x4E6B") ; <CJK>
       (?$(CJb(B . "0x559D") ; <CJK>
       (?$(CJc(B . "0x66F7") ; <CJK>
       (?$(CJd(B . "0x6E34") ; <CJK>
       (?$(CJe(B . "0x78A3") ; <CJK>
       (?$(CJf(B . "0x7AED") ; <CJK>
       (?$(CJg(B . "0x845B") ; <CJK>
       (?$(CJh(B . "0x8910") ; <CJK>
       (?$(CJi(B . "0x874E") ; <CJK>
       (?$(CJj(B . "0x97A8") ; <CJK>
       (?$(CJk(B . "0x52D8") ; <CJK>
       (?$(CJl(B . "0x574E") ; <CJK>
       (?$(CJm(B . "0x582A") ; <CJK>
       (?$(CJn(B . "0x5D4C") ; <CJK>
       (?$(CJo(B . "0x611F") ; <CJK>
       (?$(CJp(B . "0x61BE") ; <CJK>
       (?$(CJq(B . "0x6221") ; <CJK>
       (?$(CJr(B . "0x6562") ; <CJK>
       (?$(CJs(B . "0x67D1") ; <CJK>
       (?$(CJt(B . "0x6A44") ; <CJK>
       (?$(CJu(B . "0x6E1B") ; <CJK>
       (?$(CJv(B . "0x7518") ; <CJK>
       (?$(CJw(B . "0x75B3") ; <CJK>
       (?$(CJx(B . "0x76E3") ; <CJK>
       (?$(CJy(B . "0x77B0") ; <CJK>
       (?$(CJz(B . "0x7D3A") ; <CJK>
       (?$(CJ{(B . "0x90AF") ; <CJK>
       (?$(CJ|(B . "0x9451") ; <CJK>
       (?$(CJ}(B . "0x9452") ; <CJK>
       (?$(CJ~(B . "0x9F95") ; <CJK>
       (?$(CK!(B . "0x5323") ; <CJK>
       (?$(CK"(B . "0x5CAC") ; <CJK>
       (?$(CK#(B . "0x7532") ; <CJK>
       (?$(CK$(B . "0x80DB") ; <CJK>
       (?$(CK%(B . "0x9240") ; <CJK>
       (?$(CK&(B . "0x9598") ; <CJK>
       (?$(CK'(B . "0x525B") ; <CJK>
       (?$(CK((B . "0x5808") ; <CJK>
       (?$(CK)(B . "0x59DC") ; <CJK>
       (?$(CK*(B . "0x5CA1") ; <CJK>
       (?$(CK+(B . "0x5D17") ; <CJK>
       (?$(CK,(B . "0x5EB7") ; <CJK>
       (?$(CK-(B . "0x5F3A") ; <CJK>
       (?$(CK.(B . "0x5F4A") ; <CJK>
       (?$(CK/(B . "0x6177") ; <CJK>
       (?$(CK0(B . "0x6C5F") ; <CJK>
       (?$(CK1(B . "0x757A") ; <CJK>
       (?$(CK2(B . "0x7586") ; <CJK>
       (?$(CK3(B . "0x7CE0") ; <CJK>
       (?$(CK4(B . "0x7D73") ; <CJK>
       (?$(CK5(B . "0x7DB1") ; <CJK>
       (?$(CK6(B . "0x7F8C") ; <CJK>
       (?$(CK7(B . "0x8154") ; <CJK>
       (?$(CK8(B . "0x8221") ; <CJK>
       (?$(CK9(B . "0x8591") ; <CJK>
       (?$(CK:(B . "0x8941") ; <CJK>
       (?$(CK;(B . "0x8B1B") ; <CJK>
       (?$(CK<(B . "0x92FC") ; <CJK>
       (?$(CK=(B . "0x964D") ; <CJK>
       (?$(CK>(B . "0x9C47") ; <CJK>
       (?$(CK?(B . "0x4ECB") ; <CJK>
       (?$(CK@(B . "0x4EF7") ; <CJK>
       (?$(CKA(B . "0x500B") ; <CJK>
       (?$(CKB(B . "0x51F1") ; <CJK>
       (?$(CKC(B . "0x584F") ; <CJK>
       (?$(CKD(B . "0x6137") ; <CJK>
       (?$(CKE(B . "0x613E") ; <CJK>
       (?$(CKF(B . "0x6168") ; <CJK>
       (?$(CKG(B . "0x6539") ; <CJK>
       (?$(CKH(B . "0x69EA") ; <CJK>
       (?$(CKI(B . "0x6F11") ; <CJK>
       (?$(CKJ(B . "0x75A5") ; <CJK>
       (?$(CKK(B . "0x7686") ; <CJK>
       (?$(CKL(B . "0x76D6") ; <CJK>
       (?$(CKM(B . "0x7B87") ; <CJK>
       (?$(CKN(B . "0x82A5") ; <CJK>
       (?$(CKO(B . "0x84CB") ; <CJK>
       (?$(CKP(B . "0xF900") ; <CJK>
       (?$(CKQ(B . "0x93A7") ; <CJK>
       (?$(CKR(B . "0x958B") ; <CJK>
       (?$(CKS(B . "0x5580") ; <CJK>
       (?$(CKT(B . "0x5BA2") ; <CJK>
       (?$(CKU(B . "0x5751") ; <CJK>
       (?$(CKV(B . "0xF901") ; <CJK>
       (?$(CKW(B . "0x7CB3") ; <CJK>
       (?$(CKX(B . "0x7FB9") ; <CJK>
       (?$(CKY(B . "0x91B5") ; <CJK>
       (?$(CKZ(B . "0x5028") ; <CJK>
       (?$(CK[(B . "0x53BB") ; <CJK>
       (?$(CK\(B . "0x5C45") ; <CJK>
       (?$(CK](B . "0x5DE8") ; <CJK>
       (?$(CK^(B . "0x62D2") ; <CJK>
       (?$(CK_(B . "0x636E") ; <CJK>
       (?$(CK`(B . "0x64DA") ; <CJK>
       (?$(CKa(B . "0x64E7") ; <CJK>
       (?$(CKb(B . "0x6E20") ; <CJK>
       (?$(CKc(B . "0x70AC") ; <CJK>
       (?$(CKd(B . "0x795B") ; <CJK>
       (?$(CKe(B . "0x8DDD") ; <CJK>
       (?$(CKf(B . "0x8E1E") ; <CJK>
       (?$(CKg(B . "0xF902") ; <CJK>
       (?$(CKh(B . "0x907D") ; <CJK>
       (?$(CKi(B . "0x9245") ; <CJK>
       (?$(CKj(B . "0x92F8") ; <CJK>
       (?$(CKk(B . "0x4E7E") ; <CJK>
       (?$(CKl(B . "0x4EF6") ; <CJK>
       (?$(CKm(B . "0x5065") ; <CJK>
       (?$(CKn(B . "0x5DFE") ; <CJK>
       (?$(CKo(B . "0x5EFA") ; <CJK>
       (?$(CKp(B . "0x6106") ; <CJK>
       (?$(CKq(B . "0x6957") ; <CJK>
       (?$(CKr(B . "0x8171") ; <CJK>
       (?$(CKs(B . "0x8654") ; <CJK>
       (?$(CKt(B . "0x8E47") ; <CJK>
       (?$(CKu(B . "0x9375") ; <CJK>
       (?$(CKv(B . "0x9A2B") ; <CJK>
       (?$(CKw(B . "0x4E5E") ; <CJK>
       (?$(CKx(B . "0x5091") ; <CJK>
       (?$(CKy(B . "0x6770") ; <CJK>
       (?$(CKz(B . "0x6840") ; <CJK>
       (?$(CK{(B . "0x5109") ; <CJK>
       (?$(CK|(B . "0x528D") ; <CJK>
       (?$(CK}(B . "0x5292") ; <CJK>
       (?$(CK~(B . "0x6AA2") ; <CJK>
       (?$(CL!(B . "0x77BC") ; <CJK>
       (?$(CL"(B . "0x9210") ; <CJK>
       (?$(CL#(B . "0x9ED4") ; <CJK>
       (?$(CL$(B . "0x52AB") ; <CJK>
       (?$(CL%(B . "0x602F") ; <CJK>
       (?$(CL&(B . "0x8FF2") ; <CJK>
       (?$(CL'(B . "0x5048") ; <CJK>
       (?$(CL((B . "0x61A9") ; <CJK>
       (?$(CL)(B . "0x63ED") ; <CJK>
       (?$(CL*(B . "0x64CA") ; <CJK>
       (?$(CL+(B . "0x683C") ; <CJK>
       (?$(CL,(B . "0x6A84") ; <CJK>
       (?$(CL-(B . "0x6FC0") ; <CJK>
       (?$(CL.(B . "0x8188") ; <CJK>
       (?$(CL/(B . "0x89A1") ; <CJK>
       (?$(CL0(B . "0x9694") ; <CJK>
       (?$(CL1(B . "0x5805") ; <CJK>
       (?$(CL2(B . "0x727D") ; <CJK>
       (?$(CL3(B . "0x72AC") ; <CJK>
       (?$(CL4(B . "0x7504") ; <CJK>
       (?$(CL5(B . "0x7D79") ; <CJK>
       (?$(CL6(B . "0x7E6D") ; <CJK>
       (?$(CL7(B . "0x80A9") ; <CJK>
       (?$(CL8(B . "0x898B") ; <CJK>
       (?$(CL9(B . "0x8B74") ; <CJK>
       (?$(CL:(B . "0x9063") ; <CJK>
       (?$(CL;(B . "0x9D51") ; <CJK>
       (?$(CL<(B . "0x6289") ; <CJK>
       (?$(CL=(B . "0x6C7A") ; <CJK>
       (?$(CL>(B . "0x6F54") ; <CJK>
       (?$(CL?(B . "0x7D50") ; <CJK>
       (?$(CL@(B . "0x7F3A") ; <CJK>
       (?$(CLA(B . "0x8A23") ; <CJK>
       (?$(CLB(B . "0x517C") ; <CJK>
       (?$(CLC(B . "0x614A") ; <CJK>
       (?$(CLD(B . "0x7B9D") ; <CJK>
       (?$(CLE(B . "0x8B19") ; <CJK>
       (?$(CLF(B . "0x9257") ; <CJK>
       (?$(CLG(B . "0x938C") ; <CJK>
       (?$(CLH(B . "0x4EAC") ; <CJK>
       (?$(CLI(B . "0x4FD3") ; <CJK>
       (?$(CLJ(B . "0x501E") ; <CJK>
       (?$(CLK(B . "0x50BE") ; <CJK>
       (?$(CLL(B . "0x5106") ; <CJK>
       (?$(CLM(B . "0x52C1") ; <CJK>
       (?$(CLN(B . "0x52CD") ; <CJK>
       (?$(CLO(B . "0x537F") ; <CJK>
       (?$(CLP(B . "0x5770") ; <CJK>
       (?$(CLQ(B . "0x5883") ; <CJK>
       (?$(CLR(B . "0x5E9A") ; <CJK>
       (?$(CLS(B . "0x5F91") ; <CJK>
       (?$(CLT(B . "0x6176") ; <CJK>
       (?$(CLU(B . "0x61AC") ; <CJK>
       (?$(CLV(B . "0x64CE") ; <CJK>
       (?$(CLW(B . "0x656C") ; <CJK>
       (?$(CLX(B . "0x666F") ; <CJK>
       (?$(CLY(B . "0x66BB") ; <CJK>
       (?$(CLZ(B . "0x66F4") ; <CJK>
       (?$(CL[(B . "0x6897") ; <CJK>
       (?$(CL\(B . "0x6D87") ; <CJK>
       (?$(CL](B . "0x7085") ; <CJK>
       (?$(CL^(B . "0x70F1") ; <CJK>
       (?$(CL_(B . "0x749F") ; <CJK>
       (?$(CL`(B . "0x74A5") ; <CJK>
       (?$(CLa(B . "0x74CA") ; <CJK>
       (?$(CLb(B . "0x75D9") ; <CJK>
       (?$(CLc(B . "0x786C") ; <CJK>
       (?$(CLd(B . "0x78EC") ; <CJK>
       (?$(CLe(B . "0x7ADF") ; <CJK>
       (?$(CLf(B . "0x7AF6") ; <CJK>
       (?$(CLg(B . "0x7D45") ; <CJK>
       (?$(CLh(B . "0x7D93") ; <CJK>
       (?$(CLi(B . "0x8015") ; <CJK>
       (?$(CLj(B . "0x803F") ; <CJK>
       (?$(CLk(B . "0x811B") ; <CJK>
       (?$(CLl(B . "0x8396") ; <CJK>
       (?$(CLm(B . "0x8B66") ; <CJK>
       (?$(CLn(B . "0x8F15") ; <CJK>
       (?$(CLo(B . "0x9015") ; <CJK>
       (?$(CLp(B . "0x93E1") ; <CJK>
       (?$(CLq(B . "0x9803") ; <CJK>
       (?$(CLr(B . "0x9838") ; <CJK>
       (?$(CLs(B . "0x9A5A") ; <CJK>
       (?$(CLt(B . "0x9BE8") ; <CJK>
       (?$(CLu(B . "0x4FC2") ; <CJK>
       (?$(CLv(B . "0x5553") ; <CJK>
       (?$(CLw(B . "0x583A") ; <CJK>
       (?$(CLx(B . "0x5951") ; <CJK>
       (?$(CLy(B . "0x5B63") ; <CJK>
       (?$(CLz(B . "0x5C46") ; <CJK>
       (?$(CL{(B . "0x60B8") ; <CJK>
       (?$(CL|(B . "0x6212") ; <CJK>
       (?$(CL}(B . "0x6842") ; <CJK>
       (?$(CL~(B . "0x68B0") ; <CJK>
       (?$(CM!(B . "0x68E8") ; <CJK>
       (?$(CM"(B . "0x6EAA") ; <CJK>
       (?$(CM#(B . "0x754C") ; <CJK>
       (?$(CM$(B . "0x7678") ; <CJK>
       (?$(CM%(B . "0x78CE") ; <CJK>
       (?$(CM&(B . "0x7A3D") ; <CJK>
       (?$(CM'(B . "0x7CFB") ; <CJK>
       (?$(CM((B . "0x7E6B") ; <CJK>
       (?$(CM)(B . "0x7E7C") ; <CJK>
       (?$(CM*(B . "0x8A08") ; <CJK>
       (?$(CM+(B . "0x8AA1") ; <CJK>
       (?$(CM,(B . "0x8C3F") ; <CJK>
       (?$(CM-(B . "0x968E") ; <CJK>
       (?$(CM.(B . "0x9DC4") ; <CJK>
       (?$(CM/(B . "0x53E4") ; <CJK>
       (?$(CM0(B . "0x53E9") ; <CJK>
       (?$(CM1(B . "0x544A") ; <CJK>
       (?$(CM2(B . "0x5471") ; <CJK>
       (?$(CM3(B . "0x56FA") ; <CJK>
       (?$(CM4(B . "0x59D1") ; <CJK>
       (?$(CM5(B . "0x5B64") ; <CJK>
       (?$(CM6(B . "0x5C3B") ; <CJK>
       (?$(CM7(B . "0x5EAB") ; <CJK>
       (?$(CM8(B . "0x62F7") ; <CJK>
       (?$(CM9(B . "0x6537") ; <CJK>
       (?$(CM:(B . "0x6545") ; <CJK>
       (?$(CM;(B . "0x6572") ; <CJK>
       (?$(CM<(B . "0x66A0") ; <CJK>
       (?$(CM=(B . "0x67AF") ; <CJK>
       (?$(CM>(B . "0x69C1") ; <CJK>
       (?$(CM?(B . "0x6CBD") ; <CJK>
       (?$(CM@(B . "0x75FC") ; <CJK>
       (?$(CMA(B . "0x7690") ; <CJK>
       (?$(CMB(B . "0x777E") ; <CJK>
       (?$(CMC(B . "0x7A3F") ; <CJK>
       (?$(CMD(B . "0x7F94") ; <CJK>
       (?$(CME(B . "0x8003") ; <CJK>
       (?$(CMF(B . "0x80A1") ; <CJK>
       (?$(CMG(B . "0x818F") ; <CJK>
       (?$(CMH(B . "0x82E6") ; <CJK>
       (?$(CMI(B . "0x82FD") ; <CJK>
       (?$(CMJ(B . "0x83F0") ; <CJK>
       (?$(CMK(B . "0x85C1") ; <CJK>
       (?$(CML(B . "0x8831") ; <CJK>
       (?$(CMM(B . "0x88B4") ; <CJK>
       (?$(CMN(B . "0x8AA5") ; <CJK>
       (?$(CMO(B . "0xF903") ; <CJK>
       (?$(CMP(B . "0x8F9C") ; <CJK>
       (?$(CMQ(B . "0x932E") ; <CJK>
       (?$(CMR(B . "0x96C7") ; <CJK>
       (?$(CMS(B . "0x9867") ; <CJK>
       (?$(CMT(B . "0x9AD8") ; <CJK>
       (?$(CMU(B . "0x9F13") ; <CJK>
       (?$(CMV(B . "0x54ED") ; <CJK>
       (?$(CMW(B . "0x659B") ; <CJK>
       (?$(CMX(B . "0x66F2") ; <CJK>
       (?$(CMY(B . "0x688F") ; <CJK>
       (?$(CMZ(B . "0x7A40") ; <CJK>
       (?$(CM[(B . "0x8C37") ; <CJK>
       (?$(CM\(B . "0x9D60") ; <CJK>
       (?$(CM](B . "0x56F0") ; <CJK>
       (?$(CM^(B . "0x5764") ; <CJK>
       (?$(CM_(B . "0x5D11") ; <CJK>
       (?$(CM`(B . "0x6606") ; <CJK>
       (?$(CMa(B . "0x68B1") ; <CJK>
       (?$(CMb(B . "0x68CD") ; <CJK>
       (?$(CMc(B . "0x6EFE") ; <CJK>
       (?$(CMd(B . "0x7428") ; <CJK>
       (?$(CMe(B . "0x889E") ; <CJK>
       (?$(CMf(B . "0x9BE4") ; <CJK>
       (?$(CMg(B . "0x6C68") ; <CJK>
       (?$(CMh(B . "0xF904") ; <CJK>
       (?$(CMi(B . "0x9AA8") ; <CJK>
       (?$(CMj(B . "0x4F9B") ; <CJK>
       (?$(CMk(B . "0x516C") ; <CJK>
       (?$(CMl(B . "0x5171") ; <CJK>
       (?$(CMm(B . "0x529F") ; <CJK>
       (?$(CMn(B . "0x5B54") ; <CJK>
       (?$(CMo(B . "0x5DE5") ; <CJK>
       (?$(CMp(B . "0x6050") ; <CJK>
       (?$(CMq(B . "0x606D") ; <CJK>
       (?$(CMr(B . "0x62F1") ; <CJK>
       (?$(CMs(B . "0x63A7") ; <CJK>
       (?$(CMt(B . "0x653B") ; <CJK>
       (?$(CMu(B . "0x73D9") ; <CJK>
       (?$(CMv(B . "0x7A7A") ; <CJK>
       (?$(CMw(B . "0x86A3") ; <CJK>
       (?$(CMx(B . "0x8CA2") ; <CJK>
       (?$(CMy(B . "0x978F") ; <CJK>
       (?$(CMz(B . "0x4E32") ; <CJK>
       (?$(CM{(B . "0x5BE1") ; <CJK>
       (?$(CM|(B . "0x6208") ; <CJK>
       (?$(CM}(B . "0x679C") ; <CJK>
       (?$(CM~(B . "0x74DC") ; <CJK>
       (?$(CN!(B . "0x79D1") ; <CJK>
       (?$(CN"(B . "0x83D3") ; <CJK>
       (?$(CN#(B . "0x8A87") ; <CJK>
       (?$(CN$(B . "0x8AB2") ; <CJK>
       (?$(CN%(B . "0x8DE8") ; <CJK>
       (?$(CN&(B . "0x904E") ; <CJK>
       (?$(CN'(B . "0x934B") ; <CJK>
       (?$(CN((B . "0x9846") ; <CJK>
       (?$(CN)(B . "0x5ED3") ; <CJK>
       (?$(CN*(B . "0x69E8") ; <CJK>
       (?$(CN+(B . "0x85FF") ; <CJK>
       (?$(CN,(B . "0x90ED") ; <CJK>
       (?$(CN-(B . "0xF905") ; <CJK>
       (?$(CN.(B . "0x51A0") ; <CJK>
       (?$(CN/(B . "0x5B98") ; <CJK>
       (?$(CN0(B . "0x5BEC") ; <CJK>
       (?$(CN1(B . "0x6163") ; <CJK>
       (?$(CN2(B . "0x68FA") ; <CJK>
       (?$(CN3(B . "0x6B3E") ; <CJK>
       (?$(CN4(B . "0x704C") ; <CJK>
       (?$(CN5(B . "0x742F") ; <CJK>
       (?$(CN6(B . "0x74D8") ; <CJK>
       (?$(CN7(B . "0x7BA1") ; <CJK>
       (?$(CN8(B . "0x7F50") ; <CJK>
       (?$(CN9(B . "0x83C5") ; <CJK>
       (?$(CN:(B . "0x89C0") ; <CJK>
       (?$(CN;(B . "0x8CAB") ; <CJK>
       (?$(CN<(B . "0x95DC") ; <CJK>
       (?$(CN=(B . "0x9928") ; <CJK>
       (?$(CN>(B . "0x522E") ; <CJK>
       (?$(CN?(B . "0x605D") ; <CJK>
       (?$(CN@(B . "0x62EC") ; <CJK>
       (?$(CNA(B . "0x9002") ; <CJK>
       (?$(CNB(B . "0x4F8A") ; <CJK>
       (?$(CNC(B . "0x5149") ; <CJK>
       (?$(CND(B . "0x5321") ; <CJK>
       (?$(CNE(B . "0x58D9") ; <CJK>
       (?$(CNF(B . "0x5EE3") ; <CJK>
       (?$(CNG(B . "0x66E0") ; <CJK>
       (?$(CNH(B . "0x6D38") ; <CJK>
       (?$(CNI(B . "0x709A") ; <CJK>
       (?$(CNJ(B . "0x72C2") ; <CJK>
       (?$(CNK(B . "0x73D6") ; <CJK>
       (?$(CNL(B . "0x7B50") ; <CJK>
       (?$(CNM(B . "0x80F1") ; <CJK>
       (?$(CNN(B . "0x945B") ; <CJK>
       (?$(CNO(B . "0x5366") ; <CJK>
       (?$(CNP(B . "0x639B") ; <CJK>
       (?$(CNQ(B . "0x7F6B") ; <CJK>
       (?$(CNR(B . "0x4E56") ; <CJK>
       (?$(CNS(B . "0x5080") ; <CJK>
       (?$(CNT(B . "0x584A") ; <CJK>
       (?$(CNU(B . "0x58DE") ; <CJK>
       (?$(CNV(B . "0x602A") ; <CJK>
       (?$(CNW(B . "0x6127") ; <CJK>
       (?$(CNX(B . "0x62D0") ; <CJK>
       (?$(CNY(B . "0x69D0") ; <CJK>
       (?$(CNZ(B . "0x9B41") ; <CJK>
       (?$(CN[(B . "0x5B8F") ; <CJK>
       (?$(CN\(B . "0x7D18") ; <CJK>
       (?$(CN](B . "0x80B1") ; <CJK>
       (?$(CN^(B . "0x8F5F") ; <CJK>
       (?$(CN_(B . "0x4EA4") ; <CJK>
       (?$(CN`(B . "0x50D1") ; <CJK>
       (?$(CNa(B . "0x54AC") ; <CJK>
       (?$(CNb(B . "0x55AC") ; <CJK>
       (?$(CNc(B . "0x5B0C") ; <CJK>
       (?$(CNd(B . "0x5DA0") ; <CJK>
       (?$(CNe(B . "0x5DE7") ; <CJK>
       (?$(CNf(B . "0x652A") ; <CJK>
       (?$(CNg(B . "0x654E") ; <CJK>
       (?$(CNh(B . "0x6821") ; <CJK>
       (?$(CNi(B . "0x6A4B") ; <CJK>
       (?$(CNj(B . "0x72E1") ; <CJK>
       (?$(CNk(B . "0x768E") ; <CJK>
       (?$(CNl(B . "0x77EF") ; <CJK>
       (?$(CNm(B . "0x7D5E") ; <CJK>
       (?$(CNn(B . "0x7FF9") ; <CJK>
       (?$(CNo(B . "0x81A0") ; <CJK>
       (?$(CNp(B . "0x854E") ; <CJK>
       (?$(CNq(B . "0x86DF") ; <CJK>
       (?$(CNr(B . "0x8F03") ; <CJK>
       (?$(CNs(B . "0x8F4E") ; <CJK>
       (?$(CNt(B . "0x90CA") ; <CJK>
       (?$(CNu(B . "0x9903") ; <CJK>
       (?$(CNv(B . "0x9A55") ; <CJK>
       (?$(CNw(B . "0x9BAB") ; <CJK>
       (?$(CNx(B . "0x4E18") ; <CJK>
       (?$(CNy(B . "0x4E45") ; <CJK>
       (?$(CNz(B . "0x4E5D") ; <CJK>
       (?$(CN{(B . "0x4EC7") ; <CJK>
       (?$(CN|(B . "0x4FF1") ; <CJK>
       (?$(CN}(B . "0x5177") ; <CJK>
       (?$(CN~(B . "0x52FE") ; <CJK>
       (?$(CO!(B . "0x5340") ; <CJK>
       (?$(CO"(B . "0x53E3") ; <CJK>
       (?$(CO#(B . "0x53E5") ; <CJK>
       (?$(CO$(B . "0x548E") ; <CJK>
       (?$(CO%(B . "0x5614") ; <CJK>
       (?$(CO&(B . "0x5775") ; <CJK>
       (?$(CO'(B . "0x57A2") ; <CJK>
       (?$(CO((B . "0x5BC7") ; <CJK>
       (?$(CO)(B . "0x5D87") ; <CJK>
       (?$(CO*(B . "0x5ED0") ; <CJK>
       (?$(CO+(B . "0x61FC") ; <CJK>
       (?$(CO,(B . "0x62D8") ; <CJK>
       (?$(CO-(B . "0x6551") ; <CJK>
       (?$(CO.(B . "0x67B8") ; <CJK>
       (?$(CO/(B . "0x67E9") ; <CJK>
       (?$(CO0(B . "0x69CB") ; <CJK>
       (?$(CO1(B . "0x6B50") ; <CJK>
       (?$(CO2(B . "0x6BC6") ; <CJK>
       (?$(CO3(B . "0x6BEC") ; <CJK>
       (?$(CO4(B . "0x6C42") ; <CJK>
       (?$(CO5(B . "0x6E9D") ; <CJK>
       (?$(CO6(B . "0x7078") ; <CJK>
       (?$(CO7(B . "0x72D7") ; <CJK>
       (?$(CO8(B . "0x7396") ; <CJK>
       (?$(CO9(B . "0x7403") ; <CJK>
       (?$(CO:(B . "0x77BF") ; <CJK>
       (?$(CO;(B . "0x77E9") ; <CJK>
       (?$(CO<(B . "0x7A76") ; <CJK>
       (?$(CO=(B . "0x7D7F") ; <CJK>
       (?$(CO>(B . "0x8009") ; <CJK>
       (?$(CO?(B . "0x81FC") ; <CJK>
       (?$(CO@(B . "0x8205") ; <CJK>
       (?$(COA(B . "0x820A") ; <CJK>
       (?$(COB(B . "0x82DF") ; <CJK>
       (?$(COC(B . "0x8862") ; <CJK>
       (?$(COD(B . "0x8B33") ; <CJK>
       (?$(COE(B . "0x8CFC") ; <CJK>
       (?$(COF(B . "0x8EC0") ; <CJK>
       (?$(COG(B . "0x9011") ; <CJK>
       (?$(COH(B . "0x90B1") ; <CJK>
       (?$(COI(B . "0x9264") ; <CJK>
       (?$(COJ(B . "0x92B6") ; <CJK>
       (?$(COK(B . "0x99D2") ; <CJK>
       (?$(COL(B . "0x9A45") ; <CJK>
       (?$(COM(B . "0x9CE9") ; <CJK>
       (?$(CON(B . "0x9DD7") ; <CJK>
       (?$(COO(B . "0x9F9C") ; <CJK>
       (?$(COP(B . "0x570B") ; <CJK>
       (?$(COQ(B . "0x5C40") ; <CJK>
       (?$(COR(B . "0x83CA") ; <CJK>
       (?$(COS(B . "0x97A0") ; <CJK>
       (?$(COT(B . "0x97AB") ; <CJK>
       (?$(COU(B . "0x9EB4") ; <CJK>
       (?$(COV(B . "0x541B") ; <CJK>
       (?$(COW(B . "0x7A98") ; <CJK>
       (?$(COX(B . "0x7FA4") ; <CJK>
       (?$(COY(B . "0x88D9") ; <CJK>
       (?$(COZ(B . "0x8ECD") ; <CJK>
       (?$(CO[(B . "0x90E1") ; <CJK>
       (?$(CO\(B . "0x5800") ; <CJK>
       (?$(CO](B . "0x5C48") ; <CJK>
       (?$(CO^(B . "0x6398") ; <CJK>
       (?$(CO_(B . "0x7A9F") ; <CJK>
       (?$(CO`(B . "0x5BAE") ; <CJK>
       (?$(COa(B . "0x5F13") ; <CJK>
       (?$(COb(B . "0x7A79") ; <CJK>
       (?$(COc(B . "0x7AAE") ; <CJK>
       (?$(COd(B . "0x828E") ; <CJK>
       (?$(COe(B . "0x8EAC") ; <CJK>
       (?$(COf(B . "0x5026") ; <CJK>
       (?$(COg(B . "0x5238") ; <CJK>
       (?$(COh(B . "0x52F8") ; <CJK>
       (?$(COi(B . "0x5377") ; <CJK>
       (?$(COj(B . "0x5708") ; <CJK>
       (?$(COk(B . "0x62F3") ; <CJK>
       (?$(COl(B . "0x6372") ; <CJK>
       (?$(COm(B . "0x6B0A") ; <CJK>
       (?$(COn(B . "0x6DC3") ; <CJK>
       (?$(COo(B . "0x7737") ; <CJK>
       (?$(COp(B . "0x53A5") ; <CJK>
       (?$(COq(B . "0x7357") ; <CJK>
       (?$(COr(B . "0x8568") ; <CJK>
       (?$(COs(B . "0x8E76") ; <CJK>
       (?$(COt(B . "0x95D5") ; <CJK>
       (?$(COu(B . "0x673A") ; <CJK>
       (?$(COv(B . "0x6AC3") ; <CJK>
       (?$(COw(B . "0x6F70") ; <CJK>
       (?$(COx(B . "0x8A6D") ; <CJK>
       (?$(COy(B . "0x8ECC") ; <CJK>
       (?$(COz(B . "0x994B") ; <CJK>
       (?$(CO{(B . "0xF906") ; <CJK>
       (?$(CO|(B . "0x6677") ; <CJK>
       (?$(CO}(B . "0x6B78") ; <CJK>
       (?$(CO~(B . "0x8CB4") ; <CJK>
       (?$(CP!(B . "0x9B3C") ; <CJK>
       (?$(CP"(B . "0xF907") ; <CJK>
       (?$(CP#(B . "0x53EB") ; <CJK>
       (?$(CP$(B . "0x572D") ; <CJK>
       (?$(CP%(B . "0x594E") ; <CJK>
       (?$(CP&(B . "0x63C6") ; <CJK>
       (?$(CP'(B . "0x69FB") ; <CJK>
       (?$(CP((B . "0x73EA") ; <CJK>
       (?$(CP)(B . "0x7845") ; <CJK>
       (?$(CP*(B . "0x7ABA") ; <CJK>
       (?$(CP+(B . "0x7AC5") ; <CJK>
       (?$(CP,(B . "0x7CFE") ; <CJK>
       (?$(CP-(B . "0x8475") ; <CJK>
       (?$(CP.(B . "0x898F") ; <CJK>
       (?$(CP/(B . "0x8D73") ; <CJK>
       (?$(CP0(B . "0x9035") ; <CJK>
       (?$(CP1(B . "0x95A8") ; <CJK>
       (?$(CP2(B . "0x52FB") ; <CJK>
       (?$(CP3(B . "0x5747") ; <CJK>
       (?$(CP4(B . "0x7547") ; <CJK>
       (?$(CP5(B . "0x7B60") ; <CJK>
       (?$(CP6(B . "0x83CC") ; <CJK>
       (?$(CP7(B . "0x921E") ; <CJK>
       (?$(CP8(B . "0xF908") ; <CJK>
       (?$(CP9(B . "0x6A58") ; <CJK>
       (?$(CP:(B . "0x514B") ; <CJK>
       (?$(CP;(B . "0x524B") ; <CJK>
       (?$(CP<(B . "0x5287") ; <CJK>
       (?$(CP=(B . "0x621F") ; <CJK>
       (?$(CP>(B . "0x68D8") ; <CJK>
       (?$(CP?(B . "0x6975") ; <CJK>
       (?$(CP@(B . "0x9699") ; <CJK>
       (?$(CPA(B . "0x50C5") ; <CJK>
       (?$(CPB(B . "0x52A4") ; <CJK>
       (?$(CPC(B . "0x52E4") ; <CJK>
       (?$(CPD(B . "0x61C3") ; <CJK>
       (?$(CPE(B . "0x65A4") ; <CJK>
       (?$(CPF(B . "0x6839") ; <CJK>
       (?$(CPG(B . "0x69FF") ; <CJK>
       (?$(CPH(B . "0x747E") ; <CJK>
       (?$(CPI(B . "0x7B4B") ; <CJK>
       (?$(CPJ(B . "0x82B9") ; <CJK>
       (?$(CPK(B . "0x83EB") ; <CJK>
       (?$(CPL(B . "0x89B2") ; <CJK>
       (?$(CPM(B . "0x8B39") ; <CJK>
       (?$(CPN(B . "0x8FD1") ; <CJK>
       (?$(CPO(B . "0x9949") ; <CJK>
       (?$(CPP(B . "0xF909") ; <CJK>
       (?$(CPQ(B . "0x4ECA") ; <CJK>
       (?$(CPR(B . "0x5997") ; <CJK>
       (?$(CPS(B . "0x64D2") ; <CJK>
       (?$(CPT(B . "0x6611") ; <CJK>
       (?$(CPU(B . "0x6A8E") ; <CJK>
       (?$(CPV(B . "0x7434") ; <CJK>
       (?$(CPW(B . "0x7981") ; <CJK>
       (?$(CPX(B . "0x79BD") ; <CJK>
       (?$(CPY(B . "0x82A9") ; <CJK>
       (?$(CPZ(B . "0x887E") ; <CJK>
       (?$(CP[(B . "0x887F") ; <CJK>
       (?$(CP\(B . "0x895F") ; <CJK>
       (?$(CP](B . "0xF90A") ; <CJK>
       (?$(CP^(B . "0x9326") ; <CJK>
       (?$(CP_(B . "0x4F0B") ; <CJK>
       (?$(CP`(B . "0x53CA") ; <CJK>
       (?$(CPa(B . "0x6025") ; <CJK>
       (?$(CPb(B . "0x6271") ; <CJK>
       (?$(CPc(B . "0x6C72") ; <CJK>
       (?$(CPd(B . "0x7D1A") ; <CJK>
       (?$(CPe(B . "0x7D66") ; <CJK>
       (?$(CPf(B . "0x4E98") ; <CJK>
       (?$(CPg(B . "0x5162") ; <CJK>
       (?$(CPh(B . "0x77DC") ; <CJK>
       (?$(CPi(B . "0x80AF") ; <CJK>
       (?$(CPj(B . "0x4F01") ; <CJK>
       (?$(CPk(B . "0x4F0E") ; <CJK>
       (?$(CPl(B . "0x5176") ; <CJK>
       (?$(CPm(B . "0x5180") ; <CJK>
       (?$(CPn(B . "0x55DC") ; <CJK>
       (?$(CPo(B . "0x5668") ; <CJK>
       (?$(CPp(B . "0x573B") ; <CJK>
       (?$(CPq(B . "0x57FA") ; <CJK>
       (?$(CPr(B . "0x57FC") ; <CJK>
       (?$(CPs(B . "0x5914") ; <CJK>
       (?$(CPt(B . "0x5947") ; <CJK>
       (?$(CPu(B . "0x5993") ; <CJK>
       (?$(CPv(B . "0x5BC4") ; <CJK>
       (?$(CPw(B . "0x5C90") ; <CJK>
       (?$(CPx(B . "0x5D0E") ; <CJK>
       (?$(CPy(B . "0x5DF1") ; <CJK>
       (?$(CPz(B . "0x5E7E") ; <CJK>
       (?$(CP{(B . "0x5FCC") ; <CJK>
       (?$(CP|(B . "0x6280") ; <CJK>
       (?$(CP}(B . "0x65D7") ; <CJK>
       (?$(CP~(B . "0x65E3") ; <CJK>
       (?$(CQ!(B . "0x671E") ; <CJK>
       (?$(CQ"(B . "0x671F") ; <CJK>
       (?$(CQ#(B . "0x675E") ; <CJK>
       (?$(CQ$(B . "0x68CB") ; <CJK>
       (?$(CQ%(B . "0x68C4") ; <CJK>
       (?$(CQ&(B . "0x6A5F") ; <CJK>
       (?$(CQ'(B . "0x6B3A") ; <CJK>
       (?$(CQ((B . "0x6C23") ; <CJK>
       (?$(CQ)(B . "0x6C7D") ; <CJK>
       (?$(CQ*(B . "0x6C82") ; <CJK>
       (?$(CQ+(B . "0x6DC7") ; <CJK>
       (?$(CQ,(B . "0x7398") ; <CJK>
       (?$(CQ-(B . "0x7426") ; <CJK>
       (?$(CQ.(B . "0x742A") ; <CJK>
       (?$(CQ/(B . "0x7482") ; <CJK>
       (?$(CQ0(B . "0x74A3") ; <CJK>
       (?$(CQ1(B . "0x7578") ; <CJK>
       (?$(CQ2(B . "0x757F") ; <CJK>
       (?$(CQ3(B . "0x7881") ; <CJK>
       (?$(CQ4(B . "0x78EF") ; <CJK>
       (?$(CQ5(B . "0x7941") ; <CJK>
       (?$(CQ6(B . "0x7947") ; <CJK>
       (?$(CQ7(B . "0x7948") ; <CJK>
       (?$(CQ8(B . "0x797A") ; <CJK>
       (?$(CQ9(B . "0x7B95") ; <CJK>
       (?$(CQ:(B . "0x7D00") ; <CJK>
       (?$(CQ;(B . "0x7DBA") ; <CJK>
       (?$(CQ<(B . "0x7F88") ; <CJK>
       (?$(CQ=(B . "0x8006") ; <CJK>
       (?$(CQ>(B . "0x802D") ; <CJK>
       (?$(CQ?(B . "0x808C") ; <CJK>
       (?$(CQ@(B . "0x8A18") ; <CJK>
       (?$(CQA(B . "0x8B4F") ; <CJK>
       (?$(CQB(B . "0x8C48") ; <CJK>
       (?$(CQC(B . "0x8D77") ; <CJK>
       (?$(CQD(B . "0x9321") ; <CJK>
       (?$(CQE(B . "0x9324") ; <CJK>
       (?$(CQF(B . "0x98E2") ; <CJK>
       (?$(CQG(B . "0x9951") ; <CJK>
       (?$(CQH(B . "0x9A0E") ; <CJK>
       (?$(CQI(B . "0x9A0F") ; <CJK>
       (?$(CQJ(B . "0x9A65") ; <CJK>
       (?$(CQK(B . "0x9E92") ; <CJK>
       (?$(CQL(B . "0x7DCA") ; <CJK>
       (?$(CQM(B . "0x4F76") ; <CJK>
       (?$(CQN(B . "0x5409") ; <CJK>
       (?$(CQO(B . "0x62EE") ; <CJK>
       (?$(CQP(B . "0x6854") ; <CJK>
       (?$(CQQ(B . "0x91D1") ; <CJK>
       (?$(CQR(B . "0x55AB") ; <CJK>
       (?$(CQS(B . "0x513A") ; <CJK>
       (?$(CQT(B . "0xF90B") ; <CJK>
       (?$(CQU(B . "0xF90C") ; <CJK>
       (?$(CQV(B . "0x5A1C") ; <CJK>
       (?$(CQW(B . "0x61E6") ; <CJK>
       (?$(CQX(B . "0xF90D") ; <CJK>
       (?$(CQY(B . "0x62CF") ; <CJK>
       (?$(CQZ(B . "0x62FF") ; <CJK>
       (?$(CQ[(B . "0xF90E") ; <CJK>
       (?$(CQ\(B . "0xF90F") ; <CJK>
       (?$(CQ](B . "0xF910") ; <CJK>
       (?$(CQ^(B . "0xF911") ; <CJK>
       (?$(CQ_(B . "0xF912") ; <CJK>
       (?$(CQ`(B . "0xF913") ; <CJK>
       (?$(CQa(B . "0x90A3") ; <CJK>
       (?$(CQb(B . "0xF914") ; <CJK>
       (?$(CQc(B . "0xF915") ; <CJK>
       (?$(CQd(B . "0xF916") ; <CJK>
       (?$(CQe(B . "0xF917") ; <CJK>
       (?$(CQf(B . "0xF918") ; <CJK>
       (?$(CQg(B . "0x8AFE") ; <CJK>
       (?$(CQh(B . "0xF919") ; <CJK>
       (?$(CQi(B . "0xF91A") ; <CJK>
       (?$(CQj(B . "0xF91B") ; <CJK>
       (?$(CQk(B . "0xF91C") ; <CJK>
       (?$(CQl(B . "0x6696") ; <CJK>
       (?$(CQm(B . "0xF91D") ; <CJK>
       (?$(CQn(B . "0x7156") ; <CJK>
       (?$(CQo(B . "0xF91E") ; <CJK>
       (?$(CQp(B . "0xF91F") ; <CJK>
       (?$(CQq(B . "0x96E3") ; <CJK>
       (?$(CQr(B . "0xF920") ; <CJK>
       (?$(CQs(B . "0x634F") ; <CJK>
       (?$(CQt(B . "0x637A") ; <CJK>
       (?$(CQu(B . "0x5357") ; <CJK>
       (?$(CQv(B . "0xF921") ; <CJK>
       (?$(CQw(B . "0x678F") ; <CJK>
       (?$(CQx(B . "0x6960") ; <CJK>
       (?$(CQy(B . "0x6E73") ; <CJK>
       (?$(CQz(B . "0xF922") ; <CJK>
       (?$(CQ{(B . "0x7537") ; <CJK>
       (?$(CQ|(B . "0xF923") ; <CJK>
       (?$(CQ}(B . "0xF924") ; <CJK>
       (?$(CQ~(B . "0xF925") ; <CJK>
       (?$(CR!(B . "0x7D0D") ; <CJK>
       (?$(CR"(B . "0xF926") ; <CJK>
       (?$(CR#(B . "0xF927") ; <CJK>
       (?$(CR$(B . "0x8872") ; <CJK>
       (?$(CR%(B . "0x56CA") ; <CJK>
       (?$(CR&(B . "0x5A18") ; <CJK>
       (?$(CR'(B . "0xF928") ; <CJK>
       (?$(CR((B . "0xF929") ; <CJK>
       (?$(CR)(B . "0xF92A") ; <CJK>
       (?$(CR*(B . "0xF92B") ; <CJK>
       (?$(CR+(B . "0xF92C") ; <CJK>
       (?$(CR,(B . "0x4E43") ; <CJK>
       (?$(CR-(B . "0xF92D") ; <CJK>
       (?$(CR.(B . "0x5167") ; <CJK>
       (?$(CR/(B . "0x5948") ; <CJK>
       (?$(CR0(B . "0x67F0") ; <CJK>
       (?$(CR1(B . "0x8010") ; <CJK>
       (?$(CR2(B . "0xF92E") ; <CJK>
       (?$(CR3(B . "0x5973") ; <CJK>
       (?$(CR4(B . "0x5E74") ; <CJK>
       (?$(CR5(B . "0x649A") ; <CJK>
       (?$(CR6(B . "0x79CA") ; <CJK>
       (?$(CR7(B . "0x5FF5") ; <CJK>
       (?$(CR8(B . "0x606C") ; <CJK>
       (?$(CR9(B . "0x62C8") ; <CJK>
       (?$(CR:(B . "0x637B") ; <CJK>
       (?$(CR;(B . "0x5BE7") ; <CJK>
       (?$(CR<(B . "0x5BD7") ; <CJK>
       (?$(CR=(B . "0x52AA") ; <CJK>
       (?$(CR>(B . "0xF92F") ; <CJK>
       (?$(CR?(B . "0x5974") ; <CJK>
       (?$(CR@(B . "0x5F29") ; <CJK>
       (?$(CRA(B . "0x6012") ; <CJK>
       (?$(CRB(B . "0xF930") ; <CJK>
       (?$(CRC(B . "0xF931") ; <CJK>
       (?$(CRD(B . "0xF932") ; <CJK>
       (?$(CRE(B . "0x7459") ; <CJK>
       (?$(CRF(B . "0xF933") ; <CJK>
       (?$(CRG(B . "0xF934") ; <CJK>
       (?$(CRH(B . "0xF935") ; <CJK>
       (?$(CRI(B . "0xF936") ; <CJK>
       (?$(CRJ(B . "0xF937") ; <CJK>
       (?$(CRK(B . "0xF938") ; <CJK>
       (?$(CRL(B . "0x99D1") ; <CJK>
       (?$(CRM(B . "0xF939") ; <CJK>
       (?$(CRN(B . "0xF93A") ; <CJK>
       (?$(CRO(B . "0xF93B") ; <CJK>
       (?$(CRP(B . "0xF93C") ; <CJK>
       (?$(CRQ(B . "0xF93D") ; <CJK>
       (?$(CRR(B . "0xF93E") ; <CJK>
       (?$(CRS(B . "0xF93F") ; <CJK>
       (?$(CRT(B . "0xF940") ; <CJK>
       (?$(CRU(B . "0xF941") ; <CJK>
       (?$(CRV(B . "0xF942") ; <CJK>
       (?$(CRW(B . "0xF943") ; <CJK>
       (?$(CRX(B . "0x6FC3") ; <CJK>
       (?$(CRY(B . "0xF944") ; <CJK>
       (?$(CRZ(B . "0xF945") ; <CJK>
       (?$(CR[(B . "0x81BF") ; <CJK>
       (?$(CR\(B . "0x8FB2") ; <CJK>
       (?$(CR](B . "0x60F1") ; <CJK>
       (?$(CR^(B . "0xF946") ; <CJK>
       (?$(CR_(B . "0xF947") ; <CJK>
       (?$(CR`(B . "0x8166") ; <CJK>
       (?$(CRa(B . "0xF948") ; <CJK>
       (?$(CRb(B . "0xF949") ; <CJK>
       (?$(CRc(B . "0x5C3F") ; <CJK>
       (?$(CRd(B . "0xF94A") ; <CJK>
       (?$(CRe(B . "0xF94B") ; <CJK>
       (?$(CRf(B . "0xF94C") ; <CJK>
       (?$(CRg(B . "0xF94D") ; <CJK>
       (?$(CRh(B . "0xF94E") ; <CJK>
       (?$(CRi(B . "0xF94F") ; <CJK>
       (?$(CRj(B . "0xF950") ; <CJK>
       (?$(CRk(B . "0xF951") ; <CJK>
       (?$(CRl(B . "0x5AE9") ; <CJK>
       (?$(CRm(B . "0x8A25") ; <CJK>
       (?$(CRn(B . "0x677B") ; <CJK>
       (?$(CRo(B . "0x7D10") ; <CJK>
       (?$(CRp(B . "0xF952") ; <CJK>
       (?$(CRq(B . "0xF953") ; <CJK>
       (?$(CRr(B . "0xF954") ; <CJK>
       (?$(CRs(B . "0xF955") ; <CJK>
       (?$(CRt(B . "0xF956") ; <CJK>
       (?$(CRu(B . "0xF957") ; <CJK>
       (?$(CRv(B . "0x80FD") ; <CJK>
       (?$(CRw(B . "0xF958") ; <CJK>
       (?$(CRx(B . "0xF959") ; <CJK>
       (?$(CRy(B . "0x5C3C") ; <CJK>
       (?$(CRz(B . "0x6CE5") ; <CJK>
       (?$(CR{(B . "0x533F") ; <CJK>
       (?$(CR|(B . "0x6EBA") ; <CJK>
       (?$(CR}(B . "0x591A") ; <CJK>
       (?$(CR~(B . "0x8336") ; <CJK>
       (?$(CS!(B . "0x4E39") ; <CJK>
       (?$(CS"(B . "0x4EB6") ; <CJK>
       (?$(CS#(B . "0x4F46") ; <CJK>
       (?$(CS$(B . "0x55AE") ; <CJK>
       (?$(CS%(B . "0x5718") ; <CJK>
       (?$(CS&(B . "0x58C7") ; <CJK>
       (?$(CS'(B . "0x5F56") ; <CJK>
       (?$(CS((B . "0x65B7") ; <CJK>
       (?$(CS)(B . "0x65E6") ; <CJK>
       (?$(CS*(B . "0x6A80") ; <CJK>
       (?$(CS+(B . "0x6BB5") ; <CJK>
       (?$(CS,(B . "0x6E4D") ; <CJK>
       (?$(CS-(B . "0x77ED") ; <CJK>
       (?$(CS.(B . "0x7AEF") ; <CJK>
       (?$(CS/(B . "0x7C1E") ; <CJK>
       (?$(CS0(B . "0x7DDE") ; <CJK>
       (?$(CS1(B . "0x86CB") ; <CJK>
       (?$(CS2(B . "0x8892") ; <CJK>
       (?$(CS3(B . "0x9132") ; <CJK>
       (?$(CS4(B . "0x935B") ; <CJK>
       (?$(CS5(B . "0x64BB") ; <CJK>
       (?$(CS6(B . "0x6FBE") ; <CJK>
       (?$(CS7(B . "0x737A") ; <CJK>
       (?$(CS8(B . "0x75B8") ; <CJK>
       (?$(CS9(B . "0x9054") ; <CJK>
       (?$(CS:(B . "0x5556") ; <CJK>
       (?$(CS;(B . "0x574D") ; <CJK>
       (?$(CS<(B . "0x61BA") ; <CJK>
       (?$(CS=(B . "0x64D4") ; <CJK>
       (?$(CS>(B . "0x66C7") ; <CJK>
       (?$(CS?(B . "0x6DE1") ; <CJK>
       (?$(CS@(B . "0x6E5B") ; <CJK>
       (?$(CSA(B . "0x6F6D") ; <CJK>
       (?$(CSB(B . "0x6FB9") ; <CJK>
       (?$(CSC(B . "0x75F0") ; <CJK>
       (?$(CSD(B . "0x8043") ; <CJK>
       (?$(CSE(B . "0x81BD") ; <CJK>
       (?$(CSF(B . "0x8541") ; <CJK>
       (?$(CSG(B . "0x8983") ; <CJK>
       (?$(CSH(B . "0x8AC7") ; <CJK>
       (?$(CSI(B . "0x8B5A") ; <CJK>
       (?$(CSJ(B . "0x931F") ; <CJK>
       (?$(CSK(B . "0x6C93") ; <CJK>
       (?$(CSL(B . "0x7553") ; <CJK>
       (?$(CSM(B . "0x7B54") ; <CJK>
       (?$(CSN(B . "0x8E0F") ; <CJK>
       (?$(CSO(B . "0x905D") ; <CJK>
       (?$(CSP(B . "0x5510") ; <CJK>
       (?$(CSQ(B . "0x5802") ; <CJK>
       (?$(CSR(B . "0x5858") ; <CJK>
       (?$(CSS(B . "0x5E62") ; <CJK>
       (?$(CST(B . "0x6207") ; <CJK>
       (?$(CSU(B . "0x649E") ; <CJK>
       (?$(CSV(B . "0x68E0") ; <CJK>
       (?$(CSW(B . "0x7576") ; <CJK>
       (?$(CSX(B . "0x7CD6") ; <CJK>
       (?$(CSY(B . "0x87B3") ; <CJK>
       (?$(CSZ(B . "0x9EE8") ; <CJK>
       (?$(CS[(B . "0x4EE3") ; <CJK>
       (?$(CS\(B . "0x5788") ; <CJK>
       (?$(CS](B . "0x576E") ; <CJK>
       (?$(CS^(B . "0x5927") ; <CJK>
       (?$(CS_(B . "0x5C0D") ; <CJK>
       (?$(CS`(B . "0x5CB1") ; <CJK>
       (?$(CSa(B . "0x5E36") ; <CJK>
       (?$(CSb(B . "0x5F85") ; <CJK>
       (?$(CSc(B . "0x6234") ; <CJK>
       (?$(CSd(B . "0x64E1") ; <CJK>
       (?$(CSe(B . "0x73B3") ; <CJK>
       (?$(CSf(B . "0x81FA") ; <CJK>
       (?$(CSg(B . "0x888B") ; <CJK>
       (?$(CSh(B . "0x8CB8") ; <CJK>
       (?$(CSi(B . "0x968A") ; <CJK>
       (?$(CSj(B . "0x9EDB") ; <CJK>
       (?$(CSk(B . "0x5B85") ; <CJK>
       (?$(CSl(B . "0x5FB7") ; <CJK>
       (?$(CSm(B . "0x60B3") ; <CJK>
       (?$(CSn(B . "0x5012") ; <CJK>
       (?$(CSo(B . "0x5200") ; <CJK>
       (?$(CSp(B . "0x5230") ; <CJK>
       (?$(CSq(B . "0x5716") ; <CJK>
       (?$(CSr(B . "0x5835") ; <CJK>
       (?$(CSs(B . "0x5857") ; <CJK>
       (?$(CSt(B . "0x5C0E") ; <CJK>
       (?$(CSu(B . "0x5C60") ; <CJK>
       (?$(CSv(B . "0x5CF6") ; <CJK>
       (?$(CSw(B . "0x5D8B") ; <CJK>
       (?$(CSx(B . "0x5EA6") ; <CJK>
       (?$(CSy(B . "0x5F92") ; <CJK>
       (?$(CSz(B . "0x60BC") ; <CJK>
       (?$(CS{(B . "0x6311") ; <CJK>
       (?$(CS|(B . "0x6389") ; <CJK>
       (?$(CS}(B . "0x6417") ; <CJK>
       (?$(CS~(B . "0x6843") ; <CJK>
       (?$(CT!(B . "0x68F9") ; <CJK>
       (?$(CT"(B . "0x6AC2") ; <CJK>
       (?$(CT#(B . "0x6DD8") ; <CJK>
       (?$(CT$(B . "0x6E21") ; <CJK>
       (?$(CT%(B . "0x6ED4") ; <CJK>
       (?$(CT&(B . "0x6FE4") ; <CJK>
       (?$(CT'(B . "0x71FE") ; <CJK>
       (?$(CT((B . "0x76DC") ; <CJK>
       (?$(CT)(B . "0x7779") ; <CJK>
       (?$(CT*(B . "0x79B1") ; <CJK>
       (?$(CT+(B . "0x7A3B") ; <CJK>
       (?$(CT,(B . "0x8404") ; <CJK>
       (?$(CT-(B . "0x89A9") ; <CJK>
       (?$(CT.(B . "0x8CED") ; <CJK>
       (?$(CT/(B . "0x8DF3") ; <CJK>
       (?$(CT0(B . "0x8E48") ; <CJK>
       (?$(CT1(B . "0x9003") ; <CJK>
       (?$(CT2(B . "0x9014") ; <CJK>
       (?$(CT3(B . "0x9053") ; <CJK>
       (?$(CT4(B . "0x90FD") ; <CJK>
       (?$(CT5(B . "0x934D") ; <CJK>
       (?$(CT6(B . "0x9676") ; <CJK>
       (?$(CT7(B . "0x97DC") ; <CJK>
       (?$(CT8(B . "0x6BD2") ; <CJK>
       (?$(CT9(B . "0x7006") ; <CJK>
       (?$(CT:(B . "0x7258") ; <CJK>
       (?$(CT;(B . "0x72A2") ; <CJK>
       (?$(CT<(B . "0x7368") ; <CJK>
       (?$(CT=(B . "0x7763") ; <CJK>
       (?$(CT>(B . "0x79BF") ; <CJK>
       (?$(CT?(B . "0x7BE4") ; <CJK>
       (?$(CT@(B . "0x7E9B") ; <CJK>
       (?$(CTA(B . "0x8B80") ; <CJK>
       (?$(CTB(B . "0x58A9") ; <CJK>
       (?$(CTC(B . "0x60C7") ; <CJK>
       (?$(CTD(B . "0x6566") ; <CJK>
       (?$(CTE(B . "0x65FD") ; <CJK>
       (?$(CTF(B . "0x66BE") ; <CJK>
       (?$(CTG(B . "0x6C8C") ; <CJK>
       (?$(CTH(B . "0x711E") ; <CJK>
       (?$(CTI(B . "0x71C9") ; <CJK>
       (?$(CTJ(B . "0x8C5A") ; <CJK>
       (?$(CTK(B . "0x9813") ; <CJK>
       (?$(CTL(B . "0x4E6D") ; <CJK>
       (?$(CTM(B . "0x7A81") ; <CJK>
       (?$(CTN(B . "0x4EDD") ; <CJK>
       (?$(CTO(B . "0x51AC") ; <CJK>
       (?$(CTP(B . "0x51CD") ; <CJK>
       (?$(CTQ(B . "0x52D5") ; <CJK>
       (?$(CTR(B . "0x540C") ; <CJK>
       (?$(CTS(B . "0x61A7") ; <CJK>
       (?$(CTT(B . "0x6771") ; <CJK>
       (?$(CTU(B . "0x6850") ; <CJK>
       (?$(CTV(B . "0x68DF") ; <CJK>
       (?$(CTW(B . "0x6D1E") ; <CJK>
       (?$(CTX(B . "0x6F7C") ; <CJK>
       (?$(CTY(B . "0x75BC") ; <CJK>
       (?$(CTZ(B . "0x77B3") ; <CJK>
       (?$(CT[(B . "0x7AE5") ; <CJK>
       (?$(CT\(B . "0x80F4") ; <CJK>
       (?$(CT](B . "0x8463") ; <CJK>
       (?$(CT^(B . "0x9285") ; <CJK>
       (?$(CT_(B . "0x515C") ; <CJK>
       (?$(CT`(B . "0x6597") ; <CJK>
       (?$(CTa(B . "0x675C") ; <CJK>
       (?$(CTb(B . "0x6793") ; <CJK>
       (?$(CTc(B . "0x75D8") ; <CJK>
       (?$(CTd(B . "0x7AC7") ; <CJK>
       (?$(CTe(B . "0x8373") ; <CJK>
       (?$(CTf(B . "0xF95A") ; <CJK>
       (?$(CTg(B . "0x8C46") ; <CJK>
       (?$(CTh(B . "0x9017") ; <CJK>
       (?$(CTi(B . "0x982D") ; <CJK>
       (?$(CTj(B . "0x5C6F") ; <CJK>
       (?$(CTk(B . "0x81C0") ; <CJK>
       (?$(CTl(B . "0x829A") ; <CJK>
       (?$(CTm(B . "0x9041") ; <CJK>
       (?$(CTn(B . "0x906F") ; <CJK>
       (?$(CTo(B . "0x920D") ; <CJK>
       (?$(CTp(B . "0x5F97") ; <CJK>
       (?$(CTq(B . "0x5D9D") ; <CJK>
       (?$(CTr(B . "0x6A59") ; <CJK>
       (?$(CTs(B . "0x71C8") ; <CJK>
       (?$(CTt(B . "0x767B") ; <CJK>
       (?$(CTu(B . "0x7B49") ; <CJK>
       (?$(CTv(B . "0x85E4") ; <CJK>
       (?$(CTw(B . "0x8B04") ; <CJK>
       (?$(CTx(B . "0x9127") ; <CJK>
       (?$(CTy(B . "0x9A30") ; <CJK>
       (?$(CTz(B . "0x5587") ; <CJK>
       (?$(CT{(B . "0x61F6") ; <CJK>
       (?$(CT|(B . "0xF95B") ; <CJK>
       (?$(CT}(B . "0x7669") ; <CJK>
       (?$(CT~(B . "0x7F85") ; <CJK>
       (?$(CU!(B . "0x863F") ; <CJK>
       (?$(CU"(B . "0x87BA") ; <CJK>
       (?$(CU#(B . "0x88F8") ; <CJK>
       (?$(CU$(B . "0x908F") ; <CJK>
       (?$(CU%(B . "0xF95C") ; <CJK>
       (?$(CU&(B . "0x6D1B") ; <CJK>
       (?$(CU'(B . "0x70D9") ; <CJK>
       (?$(CU((B . "0x73DE") ; <CJK>
       (?$(CU)(B . "0x7D61") ; <CJK>
       (?$(CU*(B . "0x843D") ; <CJK>
       (?$(CU+(B . "0xF95D") ; <CJK>
       (?$(CU,(B . "0x916A") ; <CJK>
       (?$(CU-(B . "0x99F1") ; <CJK>
       (?$(CU.(B . "0xF95E") ; <CJK>
       (?$(CU/(B . "0x4E82") ; <CJK>
       (?$(CU0(B . "0x5375") ; <CJK>
       (?$(CU1(B . "0x6B04") ; <CJK>
       (?$(CU2(B . "0x6B12") ; <CJK>
       (?$(CU3(B . "0x703E") ; <CJK>
       (?$(CU4(B . "0x721B") ; <CJK>
       (?$(CU5(B . "0x862D") ; <CJK>
       (?$(CU6(B . "0x9E1E") ; <CJK>
       (?$(CU7(B . "0x524C") ; <CJK>
       (?$(CU8(B . "0x8FA3") ; <CJK>
       (?$(CU9(B . "0x5D50") ; <CJK>
       (?$(CU:(B . "0x64E5") ; <CJK>
       (?$(CU;(B . "0x652C") ; <CJK>
       (?$(CU<(B . "0x6B16") ; <CJK>
       (?$(CU=(B . "0x6FEB") ; <CJK>
       (?$(CU>(B . "0x7C43") ; <CJK>
       (?$(CU?(B . "0x7E9C") ; <CJK>
       (?$(CU@(B . "0x85CD") ; <CJK>
       (?$(CUA(B . "0x8964") ; <CJK>
       (?$(CUB(B . "0x89BD") ; <CJK>
       (?$(CUC(B . "0x62C9") ; <CJK>
       (?$(CUD(B . "0x81D8") ; <CJK>
       (?$(CUE(B . "0x881F") ; <CJK>
       (?$(CUF(B . "0x5ECA") ; <CJK>
       (?$(CUG(B . "0x6717") ; <CJK>
       (?$(CUH(B . "0x6D6A") ; <CJK>
       (?$(CUI(B . "0x72FC") ; <CJK>
       (?$(CUJ(B . "0x7405") ; <CJK>
       (?$(CUK(B . "0x746F") ; <CJK>
       (?$(CUL(B . "0x8782") ; <CJK>
       (?$(CUM(B . "0x90DE") ; <CJK>
       (?$(CUN(B . "0x4F86") ; <CJK>
       (?$(CUO(B . "0x5D0D") ; <CJK>
       (?$(CUP(B . "0x5FA0") ; <CJK>
       (?$(CUQ(B . "0x840A") ; <CJK>
       (?$(CUR(B . "0x51B7") ; <CJK>
       (?$(CUS(B . "0x63A0") ; <CJK>
       (?$(CUT(B . "0x7565") ; <CJK>
       (?$(CUU(B . "0x4EAE") ; <CJK>
       (?$(CUV(B . "0x5006") ; <CJK>
       (?$(CUW(B . "0x5169") ; <CJK>
       (?$(CUX(B . "0x51C9") ; <CJK>
       (?$(CUY(B . "0x6881") ; <CJK>
       (?$(CUZ(B . "0x6A11") ; <CJK>
       (?$(CU[(B . "0x7CAE") ; <CJK>
       (?$(CU\(B . "0x7CB1") ; <CJK>
       (?$(CU](B . "0x7CE7") ; <CJK>
       (?$(CU^(B . "0x826F") ; <CJK>
       (?$(CU_(B . "0x8AD2") ; <CJK>
       (?$(CU`(B . "0x8F1B") ; <CJK>
       (?$(CUa(B . "0x91CF") ; <CJK>
       (?$(CUb(B . "0x4FB6") ; <CJK>
       (?$(CUc(B . "0x5137") ; <CJK>
       (?$(CUd(B . "0x52F5") ; <CJK>
       (?$(CUe(B . "0x5442") ; <CJK>
       (?$(CUf(B . "0x5EEC") ; <CJK>
       (?$(CUg(B . "0x616E") ; <CJK>
       (?$(CUh(B . "0x623E") ; <CJK>
       (?$(CUi(B . "0x65C5") ; <CJK>
       (?$(CUj(B . "0x6ADA") ; <CJK>
       (?$(CUk(B . "0x6FFE") ; <CJK>
       (?$(CUl(B . "0x792A") ; <CJK>
       (?$(CUm(B . "0x85DC") ; <CJK>
       (?$(CUn(B . "0x8823") ; <CJK>
       (?$(CUo(B . "0x95AD") ; <CJK>
       (?$(CUp(B . "0x9A62") ; <CJK>
       (?$(CUq(B . "0x9A6A") ; <CJK>
       (?$(CUr(B . "0x9E97") ; <CJK>
       (?$(CUs(B . "0x9ECE") ; <CJK>
       (?$(CUt(B . "0x529B") ; <CJK>
       (?$(CUu(B . "0x66C6") ; <CJK>
       (?$(CUv(B . "0x6B77") ; <CJK>
       (?$(CUw(B . "0x701D") ; <CJK>
       (?$(CUx(B . "0x792B") ; <CJK>
       (?$(CUy(B . "0x8F62") ; <CJK>
       (?$(CUz(B . "0x9742") ; <CJK>
       (?$(CU{(B . "0x6190") ; <CJK>
       (?$(CU|(B . "0x6200") ; <CJK>
       (?$(CU}(B . "0x6523") ; <CJK>
       (?$(CU~(B . "0x6F23") ; <CJK>
       (?$(CV!(B . "0x7149") ; <CJK>
       (?$(CV"(B . "0x7489") ; <CJK>
       (?$(CV#(B . "0x7DF4") ; <CJK>
       (?$(CV$(B . "0x806F") ; <CJK>
       (?$(CV%(B . "0x84EE") ; <CJK>
       (?$(CV&(B . "0x8F26") ; <CJK>
       (?$(CV'(B . "0x9023") ; <CJK>
       (?$(CV((B . "0x934A") ; <CJK>
       (?$(CV)(B . "0x51BD") ; <CJK>
       (?$(CV*(B . "0x5217") ; <CJK>
       (?$(CV+(B . "0x52A3") ; <CJK>
       (?$(CV,(B . "0x6D0C") ; <CJK>
       (?$(CV-(B . "0x70C8") ; <CJK>
       (?$(CV.(B . "0x88C2") ; <CJK>
       (?$(CV/(B . "0x5EC9") ; <CJK>
       (?$(CV0(B . "0x6582") ; <CJK>
       (?$(CV1(B . "0x6BAE") ; <CJK>
       (?$(CV2(B . "0x6FC2") ; <CJK>
       (?$(CV3(B . "0x7C3E") ; <CJK>
       (?$(CV4(B . "0x7375") ; <CJK>
       (?$(CV5(B . "0x4EE4") ; <CJK>
       (?$(CV6(B . "0x4F36") ; <CJK>
       (?$(CV7(B . "0x56F9") ; <CJK>
       (?$(CV8(B . "0xF95F") ; <CJK>
       (?$(CV9(B . "0x5CBA") ; <CJK>
       (?$(CV:(B . "0x5DBA") ; <CJK>
       (?$(CV;(B . "0x601C") ; <CJK>
       (?$(CV<(B . "0x73B2") ; <CJK>
       (?$(CV=(B . "0x7B2D") ; <CJK>
       (?$(CV>(B . "0x7F9A") ; <CJK>
       (?$(CV?(B . "0x7FCE") ; <CJK>
       (?$(CV@(B . "0x8046") ; <CJK>
       (?$(CVA(B . "0x901E") ; <CJK>
       (?$(CVB(B . "0x9234") ; <CJK>
       (?$(CVC(B . "0x96F6") ; <CJK>
       (?$(CVD(B . "0x9748") ; <CJK>
       (?$(CVE(B . "0x9818") ; <CJK>
       (?$(CVF(B . "0x9F61") ; <CJK>
       (?$(CVG(B . "0x4F8B") ; <CJK>
       (?$(CVH(B . "0x6FA7") ; <CJK>
       (?$(CVI(B . "0x79AE") ; <CJK>
       (?$(CVJ(B . "0x91B4") ; <CJK>
       (?$(CVK(B . "0x96B7") ; <CJK>
       (?$(CVL(B . "0x52DE") ; <CJK>
       (?$(CVM(B . "0xF960") ; <CJK>
       (?$(CVN(B . "0x6488") ; <CJK>
       (?$(CVO(B . "0x64C4") ; <CJK>
       (?$(CVP(B . "0x6AD3") ; <CJK>
       (?$(CVQ(B . "0x6F5E") ; <CJK>
       (?$(CVR(B . "0x7018") ; <CJK>
       (?$(CVS(B . "0x7210") ; <CJK>
       (?$(CVT(B . "0x76E7") ; <CJK>
       (?$(CVU(B . "0x8001") ; <CJK>
       (?$(CVV(B . "0x8606") ; <CJK>
       (?$(CVW(B . "0x865C") ; <CJK>
       (?$(CVX(B . "0x8DEF") ; <CJK>
       (?$(CVY(B . "0x8F05") ; <CJK>
       (?$(CVZ(B . "0x9732") ; <CJK>
       (?$(CV[(B . "0x9B6F") ; <CJK>
       (?$(CV\(B . "0x9DFA") ; <CJK>
       (?$(CV](B . "0x9E75") ; <CJK>
       (?$(CV^(B . "0x788C") ; <CJK>
       (?$(CV_(B . "0x797F") ; <CJK>
       (?$(CV`(B . "0x7DA0") ; <CJK>
       (?$(CVa(B . "0x83C9") ; <CJK>
       (?$(CVb(B . "0x9304") ; <CJK>
       (?$(CVc(B . "0x9E7F") ; <CJK>
       (?$(CVd(B . "0x9E93") ; <CJK>
       (?$(CVe(B . "0x8AD6") ; <CJK>
       (?$(CVf(B . "0x58DF") ; <CJK>
       (?$(CVg(B . "0x5F04") ; <CJK>
       (?$(CVh(B . "0x6727") ; <CJK>
       (?$(CVi(B . "0x7027") ; <CJK>
       (?$(CVj(B . "0x74CF") ; <CJK>
       (?$(CVk(B . "0x7C60") ; <CJK>
       (?$(CVl(B . "0x807E") ; <CJK>
       (?$(CVm(B . "0x5121") ; <CJK>
       (?$(CVn(B . "0x7028") ; <CJK>
       (?$(CVo(B . "0x7262") ; <CJK>
       (?$(CVp(B . "0x78CA") ; <CJK>
       (?$(CVq(B . "0x8CC2") ; <CJK>
       (?$(CVr(B . "0x8CDA") ; <CJK>
       (?$(CVs(B . "0x8CF4") ; <CJK>
       (?$(CVt(B . "0x96F7") ; <CJK>
       (?$(CVu(B . "0x4E86") ; <CJK>
       (?$(CVv(B . "0x50DA") ; <CJK>
       (?$(CVw(B . "0x5BEE") ; <CJK>
       (?$(CVx(B . "0x5ED6") ; <CJK>
       (?$(CVy(B . "0x6599") ; <CJK>
       (?$(CVz(B . "0x71CE") ; <CJK>
       (?$(CV{(B . "0x7642") ; <CJK>
       (?$(CV|(B . "0x77AD") ; <CJK>
       (?$(CV}(B . "0x804A") ; <CJK>
       (?$(CV~(B . "0x84FC") ; <CJK>
       (?$(CW!(B . "0x907C") ; <CJK>
       (?$(CW"(B . "0x9B27") ; <CJK>
       (?$(CW#(B . "0x9F8D") ; <CJK>
       (?$(CW$(B . "0x58D8") ; <CJK>
       (?$(CW%(B . "0x5A41") ; <CJK>
       (?$(CW&(B . "0x5C62") ; <CJK>
       (?$(CW'(B . "0x6A13") ; <CJK>
       (?$(CW((B . "0x6DDA") ; <CJK>
       (?$(CW)(B . "0x6F0F") ; <CJK>
       (?$(CW*(B . "0x763B") ; <CJK>
       (?$(CW+(B . "0x7D2F") ; <CJK>
       (?$(CW,(B . "0x7E37") ; <CJK>
       (?$(CW-(B . "0x851E") ; <CJK>
       (?$(CW.(B . "0x8938") ; <CJK>
       (?$(CW/(B . "0x93E4") ; <CJK>
       (?$(CW0(B . "0x964B") ; <CJK>
       (?$(CW1(B . "0x5289") ; <CJK>
       (?$(CW2(B . "0x65D2") ; <CJK>
       (?$(CW3(B . "0x67F3") ; <CJK>
       (?$(CW4(B . "0x69B4") ; <CJK>
       (?$(CW5(B . "0x6D41") ; <CJK>
       (?$(CW6(B . "0x6E9C") ; <CJK>
       (?$(CW7(B . "0x700F") ; <CJK>
       (?$(CW8(B . "0x7409") ; <CJK>
       (?$(CW9(B . "0x7460") ; <CJK>
       (?$(CW:(B . "0x7559") ; <CJK>
       (?$(CW;(B . "0x7624") ; <CJK>
       (?$(CW<(B . "0x786B") ; <CJK>
       (?$(CW=(B . "0x8B2C") ; <CJK>
       (?$(CW>(B . "0x985E") ; <CJK>
       (?$(CW?(B . "0x516D") ; <CJK>
       (?$(CW@(B . "0x622E") ; <CJK>
       (?$(CWA(B . "0x9678") ; <CJK>
       (?$(CWB(B . "0x4F96") ; <CJK>
       (?$(CWC(B . "0x502B") ; <CJK>
       (?$(CWD(B . "0x5D19") ; <CJK>
       (?$(CWE(B . "0x6DEA") ; <CJK>
       (?$(CWF(B . "0x7DB8") ; <CJK>
       (?$(CWG(B . "0x8F2A") ; <CJK>
       (?$(CWH(B . "0x5F8B") ; <CJK>
       (?$(CWI(B . "0x6144") ; <CJK>
       (?$(CWJ(B . "0x6817") ; <CJK>
       (?$(CWK(B . "0xF961") ; <CJK>
       (?$(CWL(B . "0x9686") ; <CJK>
       (?$(CWM(B . "0x52D2") ; <CJK>
       (?$(CWN(B . "0x808B") ; <CJK>
       (?$(CWO(B . "0x51DC") ; <CJK>
       (?$(CWP(B . "0x51CC") ; <CJK>
       (?$(CWQ(B . "0x695E") ; <CJK>
       (?$(CWR(B . "0x7A1C") ; <CJK>
       (?$(CWS(B . "0x7DBE") ; <CJK>
       (?$(CWT(B . "0x83F1") ; <CJK>
       (?$(CWU(B . "0x9675") ; <CJK>
       (?$(CWV(B . "0x4FDA") ; <CJK>
       (?$(CWW(B . "0x5229") ; <CJK>
       (?$(CWX(B . "0x5398") ; <CJK>
       (?$(CWY(B . "0x540F") ; <CJK>
       (?$(CWZ(B . "0x550E") ; <CJK>
       (?$(CW[(B . "0x5C65") ; <CJK>
       (?$(CW\(B . "0x60A7") ; <CJK>
       (?$(CW](B . "0x674E") ; <CJK>
       (?$(CW^(B . "0x68A8") ; <CJK>
       (?$(CW_(B . "0x6D6C") ; <CJK>
       (?$(CW`(B . "0x7281") ; <CJK>
       (?$(CWa(B . "0x72F8") ; <CJK>
       (?$(CWb(B . "0x7406") ; <CJK>
       (?$(CWc(B . "0x7483") ; <CJK>
       (?$(CWd(B . "0xF962") ; <CJK>
       (?$(CWe(B . "0x75E2") ; <CJK>
       (?$(CWf(B . "0x7C6C") ; <CJK>
       (?$(CWg(B . "0x7F79") ; <CJK>
       (?$(CWh(B . "0x7FB8") ; <CJK>
       (?$(CWi(B . "0x8389") ; <CJK>
       (?$(CWj(B . "0x88CF") ; <CJK>
       (?$(CWk(B . "0x88E1") ; <CJK>
       (?$(CWl(B . "0x91CC") ; <CJK>
       (?$(CWm(B . "0x91D0") ; <CJK>
       (?$(CWn(B . "0x96E2") ; <CJK>
       (?$(CWo(B . "0x9BC9") ; <CJK>
       (?$(CWp(B . "0x541D") ; <CJK>
       (?$(CWq(B . "0x6F7E") ; <CJK>
       (?$(CWr(B . "0x71D0") ; <CJK>
       (?$(CWs(B . "0x7498") ; <CJK>
       (?$(CWt(B . "0x85FA") ; <CJK>
       (?$(CWu(B . "0x8EAA") ; <CJK>
       (?$(CWv(B . "0x96A3") ; <CJK>
       (?$(CWw(B . "0x9C57") ; <CJK>
       (?$(CWx(B . "0x9E9F") ; <CJK>
       (?$(CWy(B . "0x6797") ; <CJK>
       (?$(CWz(B . "0x6DCB") ; <CJK>
       (?$(CW{(B . "0x7433") ; <CJK>
       (?$(CW|(B . "0x81E8") ; <CJK>
       (?$(CW}(B . "0x9716") ; <CJK>
       (?$(CW~(B . "0x782C") ; <CJK>
       (?$(CX!(B . "0x7ACB") ; <CJK>
       (?$(CX"(B . "0x7B20") ; <CJK>
       (?$(CX#(B . "0x7C92") ; <CJK>
       (?$(CX$(B . "0x6469") ; <CJK>
       (?$(CX%(B . "0x746A") ; <CJK>
       (?$(CX&(B . "0x75F2") ; <CJK>
       (?$(CX'(B . "0x78BC") ; <CJK>
       (?$(CX((B . "0x78E8") ; <CJK>
       (?$(CX)(B . "0x99AC") ; <CJK>
       (?$(CX*(B . "0x9B54") ; <CJK>
       (?$(CX+(B . "0x9EBB") ; <CJK>
       (?$(CX,(B . "0x5BDE") ; <CJK>
       (?$(CX-(B . "0x5E55") ; <CJK>
       (?$(CX.(B . "0x6F20") ; <CJK>
       (?$(CX/(B . "0x819C") ; <CJK>
       (?$(CX0(B . "0x83AB") ; <CJK>
       (?$(CX1(B . "0x9088") ; <CJK>
       (?$(CX2(B . "0x4E07") ; <CJK>
       (?$(CX3(B . "0x534D") ; <CJK>
       (?$(CX4(B . "0x5A29") ; <CJK>
       (?$(CX5(B . "0x5DD2") ; <CJK>
       (?$(CX6(B . "0x5F4E") ; <CJK>
       (?$(CX7(B . "0x6162") ; <CJK>
       (?$(CX8(B . "0x633D") ; <CJK>
       (?$(CX9(B . "0x6669") ; <CJK>
       (?$(CX:(B . "0x66FC") ; <CJK>
       (?$(CX;(B . "0x6EFF") ; <CJK>
       (?$(CX<(B . "0x6F2B") ; <CJK>
       (?$(CX=(B . "0x7063") ; <CJK>
       (?$(CX>(B . "0x779E") ; <CJK>
       (?$(CX?(B . "0x842C") ; <CJK>
       (?$(CX@(B . "0x8513") ; <CJK>
       (?$(CXA(B . "0x883B") ; <CJK>
       (?$(CXB(B . "0x8F13") ; <CJK>
       (?$(CXC(B . "0x9945") ; <CJK>
       (?$(CXD(B . "0x9C3B") ; <CJK>
       (?$(CXE(B . "0x551C") ; <CJK>
       (?$(CXF(B . "0x62B9") ; <CJK>
       (?$(CXG(B . "0x672B") ; <CJK>
       (?$(CXH(B . "0x6CAB") ; <CJK>
       (?$(CXI(B . "0x8309") ; <CJK>
       (?$(CXJ(B . "0x896A") ; <CJK>
       (?$(CXK(B . "0x977A") ; <CJK>
       (?$(CXL(B . "0x4EA1") ; <CJK>
       (?$(CXM(B . "0x5984") ; <CJK>
       (?$(CXN(B . "0x5FD8") ; <CJK>
       (?$(CXO(B . "0x5FD9") ; <CJK>
       (?$(CXP(B . "0x671B") ; <CJK>
       (?$(CXQ(B . "0x7DB2") ; <CJK>
       (?$(CXR(B . "0x7F54") ; <CJK>
       (?$(CXS(B . "0x8292") ; <CJK>
       (?$(CXT(B . "0x832B") ; <CJK>
       (?$(CXU(B . "0x83BD") ; <CJK>
       (?$(CXV(B . "0x8F1E") ; <CJK>
       (?$(CXW(B . "0x9099") ; <CJK>
       (?$(CXX(B . "0x57CB") ; <CJK>
       (?$(CXY(B . "0x59B9") ; <CJK>
       (?$(CXZ(B . "0x5A92") ; <CJK>
       (?$(CX[(B . "0x5BD0") ; <CJK>
       (?$(CX\(B . "0x6627") ; <CJK>
       (?$(CX](B . "0x679A") ; <CJK>
       (?$(CX^(B . "0x6885") ; <CJK>
       (?$(CX_(B . "0x6BCF") ; <CJK>
       (?$(CX`(B . "0x7164") ; <CJK>
       (?$(CXa(B . "0x7F75") ; <CJK>
       (?$(CXb(B . "0x8CB7") ; <CJK>
       (?$(CXc(B . "0x8CE3") ; <CJK>
       (?$(CXd(B . "0x9081") ; <CJK>
       (?$(CXe(B . "0x9B45") ; <CJK>
       (?$(CXf(B . "0x8108") ; <CJK>
       (?$(CXg(B . "0x8C8A") ; <CJK>
       (?$(CXh(B . "0x964C") ; <CJK>
       (?$(CXi(B . "0x9A40") ; <CJK>
       (?$(CXj(B . "0x9EA5") ; <CJK>
       (?$(CXk(B . "0x5B5F") ; <CJK>
       (?$(CXl(B . "0x6C13") ; <CJK>
       (?$(CXm(B . "0x731B") ; <CJK>
       (?$(CXn(B . "0x76F2") ; <CJK>
       (?$(CXo(B . "0x76DF") ; <CJK>
       (?$(CXp(B . "0x840C") ; <CJK>
       (?$(CXq(B . "0x51AA") ; <CJK>
       (?$(CXr(B . "0x8993") ; <CJK>
       (?$(CXs(B . "0x514D") ; <CJK>
       (?$(CXt(B . "0x5195") ; <CJK>
       (?$(CXu(B . "0x52C9") ; <CJK>
       (?$(CXv(B . "0x68C9") ; <CJK>
       (?$(CXw(B . "0x6C94") ; <CJK>
       (?$(CXx(B . "0x7704") ; <CJK>
       (?$(CXy(B . "0x7720") ; <CJK>
       (?$(CXz(B . "0x7DBF") ; <CJK>
       (?$(CX{(B . "0x7DEC") ; <CJK>
       (?$(CX|(B . "0x9762") ; <CJK>
       (?$(CX}(B . "0x9EB5") ; <CJK>
       (?$(CX~(B . "0x6EC5") ; <CJK>
       (?$(CY!(B . "0x8511") ; <CJK>
       (?$(CY"(B . "0x51A5") ; <CJK>
       (?$(CY#(B . "0x540D") ; <CJK>
       (?$(CY$(B . "0x547D") ; <CJK>
       (?$(CY%(B . "0x660E") ; <CJK>
       (?$(CY&(B . "0x669D") ; <CJK>
       (?$(CY'(B . "0x6927") ; <CJK>
       (?$(CY((B . "0x6E9F") ; <CJK>
       (?$(CY)(B . "0x76BF") ; <CJK>
       (?$(CY*(B . "0x7791") ; <CJK>
       (?$(CY+(B . "0x8317") ; <CJK>
       (?$(CY,(B . "0x84C2") ; <CJK>
       (?$(CY-(B . "0x879F") ; <CJK>
       (?$(CY.(B . "0x9169") ; <CJK>
       (?$(CY/(B . "0x9298") ; <CJK>
       (?$(CY0(B . "0x9CF4") ; <CJK>
       (?$(CY1(B . "0x8882") ; <CJK>
       (?$(CY2(B . "0x4FAE") ; <CJK>
       (?$(CY3(B . "0x5192") ; <CJK>
       (?$(CY4(B . "0x52DF") ; <CJK>
       (?$(CY5(B . "0x59C6") ; <CJK>
       (?$(CY6(B . "0x5E3D") ; <CJK>
       (?$(CY7(B . "0x6155") ; <CJK>
       (?$(CY8(B . "0x6478") ; <CJK>
       (?$(CY9(B . "0x6479") ; <CJK>
       (?$(CY:(B . "0x66AE") ; <CJK>
       (?$(CY;(B . "0x67D0") ; <CJK>
       (?$(CY<(B . "0x6A21") ; <CJK>
       (?$(CY=(B . "0x6BCD") ; <CJK>
       (?$(CY>(B . "0x6BDB") ; <CJK>
       (?$(CY?(B . "0x725F") ; <CJK>
       (?$(CY@(B . "0x7261") ; <CJK>
       (?$(CYA(B . "0x7441") ; <CJK>
       (?$(CYB(B . "0x7738") ; <CJK>
       (?$(CYC(B . "0x77DB") ; <CJK>
       (?$(CYD(B . "0x8017") ; <CJK>
       (?$(CYE(B . "0x82BC") ; <CJK>
       (?$(CYF(B . "0x8305") ; <CJK>
       (?$(CYG(B . "0x8B00") ; <CJK>
       (?$(CYH(B . "0x8B28") ; <CJK>
       (?$(CYI(B . "0x8C8C") ; <CJK>
       (?$(CYJ(B . "0x6728") ; <CJK>
       (?$(CYK(B . "0x6C90") ; <CJK>
       (?$(CYL(B . "0x7267") ; <CJK>
       (?$(CYM(B . "0x76EE") ; <CJK>
       (?$(CYN(B . "0x7766") ; <CJK>
       (?$(CYO(B . "0x7A46") ; <CJK>
       (?$(CYP(B . "0x9DA9") ; <CJK>
       (?$(CYQ(B . "0x6B7F") ; <CJK>
       (?$(CYR(B . "0x6C92") ; <CJK>
       (?$(CYS(B . "0x5922") ; <CJK>
       (?$(CYT(B . "0x6726") ; <CJK>
       (?$(CYU(B . "0x8499") ; <CJK>
       (?$(CYV(B . "0x536F") ; <CJK>
       (?$(CYW(B . "0x5893") ; <CJK>
       (?$(CYX(B . "0x5999") ; <CJK>
       (?$(CYY(B . "0x5EDF") ; <CJK>
       (?$(CYZ(B . "0x63CF") ; <CJK>
       (?$(CY[(B . "0x6634") ; <CJK>
       (?$(CY\(B . "0x6773") ; <CJK>
       (?$(CY](B . "0x6E3A") ; <CJK>
       (?$(CY^(B . "0x732B") ; <CJK>
       (?$(CY_(B . "0x7AD7") ; <CJK>
       (?$(CY`(B . "0x82D7") ; <CJK>
       (?$(CYa(B . "0x9328") ; <CJK>
       (?$(CYb(B . "0x52D9") ; <CJK>
       (?$(CYc(B . "0x5DEB") ; <CJK>
       (?$(CYd(B . "0x61AE") ; <CJK>
       (?$(CYe(B . "0x61CB") ; <CJK>
       (?$(CYf(B . "0x620A") ; <CJK>
       (?$(CYg(B . "0x62C7") ; <CJK>
       (?$(CYh(B . "0x64AB") ; <CJK>
       (?$(CYi(B . "0x65E0") ; <CJK>
       (?$(CYj(B . "0x6959") ; <CJK>
       (?$(CYk(B . "0x6B66") ; <CJK>
       (?$(CYl(B . "0x6BCB") ; <CJK>
       (?$(CYm(B . "0x7121") ; <CJK>
       (?$(CYn(B . "0x73F7") ; <CJK>
       (?$(CYo(B . "0x755D") ; <CJK>
       (?$(CYp(B . "0x7E46") ; <CJK>
       (?$(CYq(B . "0x821E") ; <CJK>
       (?$(CYr(B . "0x8302") ; <CJK>
       (?$(CYs(B . "0x856A") ; <CJK>
       (?$(CYt(B . "0x8AA3") ; <CJK>
       (?$(CYu(B . "0x8CBF") ; <CJK>
       (?$(CYv(B . "0x9727") ; <CJK>
       (?$(CYw(B . "0x9D61") ; <CJK>
       (?$(CYx(B . "0x58A8") ; <CJK>
       (?$(CYy(B . "0x9ED8") ; <CJK>
       (?$(CYz(B . "0x5011") ; <CJK>
       (?$(CY{(B . "0x520E") ; <CJK>
       (?$(CY|(B . "0x543B") ; <CJK>
       (?$(CY}(B . "0x554F") ; <CJK>
       (?$(CY~(B . "0x6587") ; <CJK>
       (?$(CZ!(B . "0x6C76") ; <CJK>
       (?$(CZ"(B . "0x7D0A") ; <CJK>
       (?$(CZ#(B . "0x7D0B") ; <CJK>
       (?$(CZ$(B . "0x805E") ; <CJK>
       (?$(CZ%(B . "0x868A") ; <CJK>
       (?$(CZ&(B . "0x9580") ; <CJK>
       (?$(CZ'(B . "0x96EF") ; <CJK>
       (?$(CZ((B . "0x52FF") ; <CJK>
       (?$(CZ)(B . "0x6C95") ; <CJK>
       (?$(CZ*(B . "0x7269") ; <CJK>
       (?$(CZ+(B . "0x5473") ; <CJK>
       (?$(CZ,(B . "0x5A9A") ; <CJK>
       (?$(CZ-(B . "0x5C3E") ; <CJK>
       (?$(CZ.(B . "0x5D4B") ; <CJK>
       (?$(CZ/(B . "0x5F4C") ; <CJK>
       (?$(CZ0(B . "0x5FAE") ; <CJK>
       (?$(CZ1(B . "0x672A") ; <CJK>
       (?$(CZ2(B . "0x68B6") ; <CJK>
       (?$(CZ3(B . "0x6963") ; <CJK>
       (?$(CZ4(B . "0x6E3C") ; <CJK>
       (?$(CZ5(B . "0x6E44") ; <CJK>
       (?$(CZ6(B . "0x7709") ; <CJK>
       (?$(CZ7(B . "0x7C73") ; <CJK>
       (?$(CZ8(B . "0x7F8E") ; <CJK>
       (?$(CZ9(B . "0x8587") ; <CJK>
       (?$(CZ:(B . "0x8B0E") ; <CJK>
       (?$(CZ;(B . "0x8FF7") ; <CJK>
       (?$(CZ<(B . "0x9761") ; <CJK>
       (?$(CZ=(B . "0x9EF4") ; <CJK>
       (?$(CZ>(B . "0x5CB7") ; <CJK>
       (?$(CZ?(B . "0x60B6") ; <CJK>
       (?$(CZ@(B . "0x610D") ; <CJK>
       (?$(CZA(B . "0x61AB") ; <CJK>
       (?$(CZB(B . "0x654F") ; <CJK>
       (?$(CZC(B . "0x65FB") ; <CJK>
       (?$(CZD(B . "0x65FC") ; <CJK>
       (?$(CZE(B . "0x6C11") ; <CJK>
       (?$(CZF(B . "0x6CEF") ; <CJK>
       (?$(CZG(B . "0x739F") ; <CJK>
       (?$(CZH(B . "0x73C9") ; <CJK>
       (?$(CZI(B . "0x7DE1") ; <CJK>
       (?$(CZJ(B . "0x9594") ; <CJK>
       (?$(CZK(B . "0x5BC6") ; <CJK>
       (?$(CZL(B . "0x871C") ; <CJK>
       (?$(CZM(B . "0x8B10") ; <CJK>
       (?$(CZN(B . "0x525D") ; <CJK>
       (?$(CZO(B . "0x535A") ; <CJK>
       (?$(CZP(B . "0x62CD") ; <CJK>
       (?$(CZQ(B . "0x640F") ; <CJK>
       (?$(CZR(B . "0x64B2") ; <CJK>
       (?$(CZS(B . "0x6734") ; <CJK>
       (?$(CZT(B . "0x6A38") ; <CJK>
       (?$(CZU(B . "0x6CCA") ; <CJK>
       (?$(CZV(B . "0x73C0") ; <CJK>
       (?$(CZW(B . "0x749E") ; <CJK>
       (?$(CZX(B . "0x7B94") ; <CJK>
       (?$(CZY(B . "0x7C95") ; <CJK>
       (?$(CZZ(B . "0x7E1B") ; <CJK>
       (?$(CZ[(B . "0x818A") ; <CJK>
       (?$(CZ\(B . "0x8236") ; <CJK>
       (?$(CZ](B . "0x8584") ; <CJK>
       (?$(CZ^(B . "0x8FEB") ; <CJK>
       (?$(CZ_(B . "0x96F9") ; <CJK>
       (?$(CZ`(B . "0x99C1") ; <CJK>
       (?$(CZa(B . "0x4F34") ; <CJK>
       (?$(CZb(B . "0x534A") ; <CJK>
       (?$(CZc(B . "0x53CD") ; <CJK>
       (?$(CZd(B . "0x53DB") ; <CJK>
       (?$(CZe(B . "0x62CC") ; <CJK>
       (?$(CZf(B . "0x642C") ; <CJK>
       (?$(CZg(B . "0x6500") ; <CJK>
       (?$(CZh(B . "0x6591") ; <CJK>
       (?$(CZi(B . "0x69C3") ; <CJK>
       (?$(CZj(B . "0x6CEE") ; <CJK>
       (?$(CZk(B . "0x6F58") ; <CJK>
       (?$(CZl(B . "0x73ED") ; <CJK>
       (?$(CZm(B . "0x7554") ; <CJK>
       (?$(CZn(B . "0x7622") ; <CJK>
       (?$(CZo(B . "0x76E4") ; <CJK>
       (?$(CZp(B . "0x76FC") ; <CJK>
       (?$(CZq(B . "0x78D0") ; <CJK>
       (?$(CZr(B . "0x78FB") ; <CJK>
       (?$(CZs(B . "0x792C") ; <CJK>
       (?$(CZt(B . "0x7D46") ; <CJK>
       (?$(CZu(B . "0x822C") ; <CJK>
       (?$(CZv(B . "0x87E0") ; <CJK>
       (?$(CZw(B . "0x8FD4") ; <CJK>
       (?$(CZx(B . "0x9812") ; <CJK>
       (?$(CZy(B . "0x98EF") ; <CJK>
       (?$(CZz(B . "0x52C3") ; <CJK>
       (?$(CZ{(B . "0x62D4") ; <CJK>
       (?$(CZ|(B . "0x64A5") ; <CJK>
       (?$(CZ}(B . "0x6E24") ; <CJK>
       (?$(CZ~(B . "0x6F51") ; <CJK>
       (?$(C[!(B . "0x767C") ; <CJK>
       (?$(C["(B . "0x8DCB") ; <CJK>
       (?$(C[#(B . "0x91B1") ; <CJK>
       (?$(C[$(B . "0x9262") ; <CJK>
       (?$(C[%(B . "0x9AEE") ; <CJK>
       (?$(C[&(B . "0x9B43") ; <CJK>
       (?$(C['(B . "0x5023") ; <CJK>
       (?$(C[((B . "0x508D") ; <CJK>
       (?$(C[)(B . "0x574A") ; <CJK>
       (?$(C[*(B . "0x59A8") ; <CJK>
       (?$(C[+(B . "0x5C28") ; <CJK>
       (?$(C[,(B . "0x5E47") ; <CJK>
       (?$(C[-(B . "0x5F77") ; <CJK>
       (?$(C[.(B . "0x623F") ; <CJK>
       (?$(C[/(B . "0x653E") ; <CJK>
       (?$(C[0(B . "0x65B9") ; <CJK>
       (?$(C[1(B . "0x65C1") ; <CJK>
       (?$(C[2(B . "0x6609") ; <CJK>
       (?$(C[3(B . "0x678B") ; <CJK>
       (?$(C[4(B . "0x699C") ; <CJK>
       (?$(C[5(B . "0x6EC2") ; <CJK>
       (?$(C[6(B . "0x78C5") ; <CJK>
       (?$(C[7(B . "0x7D21") ; <CJK>
       (?$(C[8(B . "0x80AA") ; <CJK>
       (?$(C[9(B . "0x8180") ; <CJK>
       (?$(C[:(B . "0x822B") ; <CJK>
       (?$(C[;(B . "0x82B3") ; <CJK>
       (?$(C[<(B . "0x84A1") ; <CJK>
       (?$(C[=(B . "0x868C") ; <CJK>
       (?$(C[>(B . "0x8A2A") ; <CJK>
       (?$(C[?(B . "0x8B17") ; <CJK>
       (?$(C[@(B . "0x90A6") ; <CJK>
       (?$(C[A(B . "0x9632") ; <CJK>
       (?$(C[B(B . "0x9F90") ; <CJK>
       (?$(C[C(B . "0x500D") ; <CJK>
       (?$(C[D(B . "0x4FF3") ; <CJK>
       (?$(C[E(B . "0xF963") ; <CJK>
       (?$(C[F(B . "0x57F9") ; <CJK>
       (?$(C[G(B . "0x5F98") ; <CJK>
       (?$(C[H(B . "0x62DC") ; <CJK>
       (?$(C[I(B . "0x6392") ; <CJK>
       (?$(C[J(B . "0x676F") ; <CJK>
       (?$(C[K(B . "0x6E43") ; <CJK>
       (?$(C[L(B . "0x7119") ; <CJK>
       (?$(C[M(B . "0x76C3") ; <CJK>
       (?$(C[N(B . "0x80CC") ; <CJK>
       (?$(C[O(B . "0x80DA") ; <CJK>
       (?$(C[P(B . "0x88F4") ; <CJK>
       (?$(C[Q(B . "0x88F5") ; <CJK>
       (?$(C[R(B . "0x8919") ; <CJK>
       (?$(C[S(B . "0x8CE0") ; <CJK>
       (?$(C[T(B . "0x8F29") ; <CJK>
       (?$(C[U(B . "0x914D") ; <CJK>
       (?$(C[V(B . "0x966A") ; <CJK>
       (?$(C[W(B . "0x4F2F") ; <CJK>
       (?$(C[X(B . "0x4F70") ; <CJK>
       (?$(C[Y(B . "0x5E1B") ; <CJK>
       (?$(C[Z(B . "0x67CF") ; <CJK>
       (?$(C[[(B . "0x6822") ; <CJK>
       (?$(C[\(B . "0x767D") ; <CJK>
       (?$(C[](B . "0x767E") ; <CJK>
       (?$(C[^(B . "0x9B44") ; <CJK>
       (?$(C[_(B . "0x5E61") ; <CJK>
       (?$(C[`(B . "0x6A0A") ; <CJK>
       (?$(C[a(B . "0x7169") ; <CJK>
       (?$(C[b(B . "0x71D4") ; <CJK>
       (?$(C[c(B . "0x756A") ; <CJK>
       (?$(C[d(B . "0xF964") ; <CJK>
       (?$(C[e(B . "0x7E41") ; <CJK>
       (?$(C[f(B . "0x8543") ; <CJK>
       (?$(C[g(B . "0x85E9") ; <CJK>
       (?$(C[h(B . "0x98DC") ; <CJK>
       (?$(C[i(B . "0x4F10") ; <CJK>
       (?$(C[j(B . "0x7B4F") ; <CJK>
       (?$(C[k(B . "0x7F70") ; <CJK>
       (?$(C[l(B . "0x95A5") ; <CJK>
       (?$(C[m(B . "0x51E1") ; <CJK>
       (?$(C[n(B . "0x5E06") ; <CJK>
       (?$(C[o(B . "0x68B5") ; <CJK>
       (?$(C[p(B . "0x6C3E") ; <CJK>
       (?$(C[q(B . "0x6C4E") ; <CJK>
       (?$(C[r(B . "0x6CDB") ; <CJK>
       (?$(C[s(B . "0x72AF") ; <CJK>
       (?$(C[t(B . "0x7BC4") ; <CJK>
       (?$(C[u(B . "0x8303") ; <CJK>
       (?$(C[v(B . "0x6CD5") ; <CJK>
       (?$(C[w(B . "0x743A") ; <CJK>
       (?$(C[x(B . "0x50FB") ; <CJK>
       (?$(C[y(B . "0x5288") ; <CJK>
       (?$(C[z(B . "0x58C1") ; <CJK>
       (?$(C[{(B . "0x64D8") ; <CJK>
       (?$(C[|(B . "0x6A97") ; <CJK>
       (?$(C[}(B . "0x74A7") ; <CJK>
       (?$(C[~(B . "0x7656") ; <CJK>
       (?$(C\!(B . "0x78A7") ; <CJK>
       (?$(C\"(B . "0x8617") ; <CJK>
       (?$(C\#(B . "0x95E2") ; <CJK>
       (?$(C\$(B . "0x9739") ; <CJK>
       (?$(C\%(B . "0xF965") ; <CJK>
       (?$(C\&(B . "0x535E") ; <CJK>
       (?$(C\'(B . "0x5F01") ; <CJK>
       (?$(C\((B . "0x8B8A") ; <CJK>
       (?$(C\)(B . "0x8FA8") ; <CJK>
       (?$(C\*(B . "0x8FAF") ; <CJK>
       (?$(C\+(B . "0x908A") ; <CJK>
       (?$(C\,(B . "0x5225") ; <CJK>
       (?$(C\-(B . "0x77A5") ; <CJK>
       (?$(C\.(B . "0x9C49") ; <CJK>
       (?$(C\/(B . "0x9F08") ; <CJK>
       (?$(C\0(B . "0x4E19") ; <CJK>
       (?$(C\1(B . "0x5002") ; <CJK>
       (?$(C\2(B . "0x5175") ; <CJK>
       (?$(C\3(B . "0x5C5B") ; <CJK>
       (?$(C\4(B . "0x5E77") ; <CJK>
       (?$(C\5(B . "0x661E") ; <CJK>
       (?$(C\6(B . "0x663A") ; <CJK>
       (?$(C\7(B . "0x67C4") ; <CJK>
       (?$(C\8(B . "0x68C5") ; <CJK>
       (?$(C\9(B . "0x70B3") ; <CJK>
       (?$(C\:(B . "0x7501") ; <CJK>
       (?$(C\;(B . "0x75C5") ; <CJK>
       (?$(C\<(B . "0x79C9") ; <CJK>
       (?$(C\=(B . "0x7ADD") ; <CJK>
       (?$(C\>(B . "0x8F27") ; <CJK>
       (?$(C\?(B . "0x9920") ; <CJK>
       (?$(C\@(B . "0x9A08") ; <CJK>
       (?$(C\A(B . "0x4FDD") ; <CJK>
       (?$(C\B(B . "0x5821") ; <CJK>
       (?$(C\C(B . "0x5831") ; <CJK>
       (?$(C\D(B . "0x5BF6") ; <CJK>
       (?$(C\E(B . "0x666E") ; <CJK>
       (?$(C\F(B . "0x6B65") ; <CJK>
       (?$(C\G(B . "0x6D11") ; <CJK>
       (?$(C\H(B . "0x6E7A") ; <CJK>
       (?$(C\I(B . "0x6F7D") ; <CJK>
       (?$(C\J(B . "0x73E4") ; <CJK>
       (?$(C\K(B . "0x752B") ; <CJK>
       (?$(C\L(B . "0x83E9") ; <CJK>
       (?$(C\M(B . "0x88DC") ; <CJK>
       (?$(C\N(B . "0x8913") ; <CJK>
       (?$(C\O(B . "0x8B5C") ; <CJK>
       (?$(C\P(B . "0x8F14") ; <CJK>
       (?$(C\Q(B . "0x4F0F") ; <CJK>
       (?$(C\R(B . "0x50D5") ; <CJK>
       (?$(C\S(B . "0x5310") ; <CJK>
       (?$(C\T(B . "0x535C") ; <CJK>
       (?$(C\U(B . "0x5B93") ; <CJK>
       (?$(C\V(B . "0x5FA9") ; <CJK>
       (?$(C\W(B . "0x670D") ; <CJK>
       (?$(C\X(B . "0x798F") ; <CJK>
       (?$(C\Y(B . "0x8179") ; <CJK>
       (?$(C\Z(B . "0x832F") ; <CJK>
       (?$(C\[(B . "0x8514") ; <CJK>
       (?$(C\\(B . "0x8907") ; <CJK>
       (?$(C\](B . "0x8986") ; <CJK>
       (?$(C\^(B . "0x8F39") ; <CJK>
       (?$(C\_(B . "0x8F3B") ; <CJK>
       (?$(C\`(B . "0x99A5") ; <CJK>
       (?$(C\a(B . "0x9C12") ; <CJK>
       (?$(C\b(B . "0x672C") ; <CJK>
       (?$(C\c(B . "0x4E76") ; <CJK>
       (?$(C\d(B . "0x4FF8") ; <CJK>
       (?$(C\e(B . "0x5949") ; <CJK>
       (?$(C\f(B . "0x5C01") ; <CJK>
       (?$(C\g(B . "0x5CEF") ; <CJK>
       (?$(C\h(B . "0x5CF0") ; <CJK>
       (?$(C\i(B . "0x6367") ; <CJK>
       (?$(C\j(B . "0x68D2") ; <CJK>
       (?$(C\k(B . "0x70FD") ; <CJK>
       (?$(C\l(B . "0x71A2") ; <CJK>
       (?$(C\m(B . "0x742B") ; <CJK>
       (?$(C\n(B . "0x7E2B") ; <CJK>
       (?$(C\o(B . "0x84EC") ; <CJK>
       (?$(C\p(B . "0x8702") ; <CJK>
       (?$(C\q(B . "0x9022") ; <CJK>
       (?$(C\r(B . "0x92D2") ; <CJK>
       (?$(C\s(B . "0x9CF3") ; <CJK>
       (?$(C\t(B . "0x4E0D") ; <CJK>
       (?$(C\u(B . "0x4ED8") ; <CJK>
       (?$(C\v(B . "0x4FEF") ; <CJK>
       (?$(C\w(B . "0x5085") ; <CJK>
       (?$(C\x(B . "0x5256") ; <CJK>
       (?$(C\y(B . "0x526F") ; <CJK>
       (?$(C\z(B . "0x5426") ; <CJK>
       (?$(C\{(B . "0x5490") ; <CJK>
       (?$(C\|(B . "0x57E0") ; <CJK>
       (?$(C\}(B . "0x592B") ; <CJK>
       (?$(C\~(B . "0x5A66") ; <CJK>
       (?$(C]!(B . "0x5B5A") ; <CJK>
       (?$(C]"(B . "0x5B75") ; <CJK>
       (?$(C]#(B . "0x5BCC") ; <CJK>
       (?$(C]$(B . "0x5E9C") ; <CJK>
       (?$(C]%(B . "0xF966") ; <CJK>
       (?$(C]&(B . "0x6276") ; <CJK>
       (?$(C]'(B . "0x6577") ; <CJK>
       (?$(C]((B . "0x65A7") ; <CJK>
       (?$(C])(B . "0x6D6E") ; <CJK>
       (?$(C]*(B . "0x6EA5") ; <CJK>
       (?$(C]+(B . "0x7236") ; <CJK>
       (?$(C],(B . "0x7B26") ; <CJK>
       (?$(C]-(B . "0x7C3F") ; <CJK>
       (?$(C].(B . "0x7F36") ; <CJK>
       (?$(C]/(B . "0x8150") ; <CJK>
       (?$(C]0(B . "0x8151") ; <CJK>
       (?$(C]1(B . "0x819A") ; <CJK>
       (?$(C]2(B . "0x8240") ; <CJK>
       (?$(C]3(B . "0x8299") ; <CJK>
       (?$(C]4(B . "0x83A9") ; <CJK>
       (?$(C]5(B . "0x8A03") ; <CJK>
       (?$(C]6(B . "0x8CA0") ; <CJK>
       (?$(C]7(B . "0x8CE6") ; <CJK>
       (?$(C]8(B . "0x8CFB") ; <CJK>
       (?$(C]9(B . "0x8D74") ; <CJK>
       (?$(C]:(B . "0x8DBA") ; <CJK>
       (?$(C];(B . "0x90E8") ; <CJK>
       (?$(C]<(B . "0x91DC") ; <CJK>
       (?$(C]=(B . "0x961C") ; <CJK>
       (?$(C]>(B . "0x9644") ; <CJK>
       (?$(C]?(B . "0x99D9") ; <CJK>
       (?$(C]@(B . "0x9CE7") ; <CJK>
       (?$(C]A(B . "0x5317") ; <CJK>
       (?$(C]B(B . "0x5206") ; <CJK>
       (?$(C]C(B . "0x5429") ; <CJK>
       (?$(C]D(B . "0x5674") ; <CJK>
       (?$(C]E(B . "0x58B3") ; <CJK>
       (?$(C]F(B . "0x5954") ; <CJK>
       (?$(C]G(B . "0x596E") ; <CJK>
       (?$(C]H(B . "0x5FFF") ; <CJK>
       (?$(C]I(B . "0x61A4") ; <CJK>
       (?$(C]J(B . "0x626E") ; <CJK>
       (?$(C]K(B . "0x6610") ; <CJK>
       (?$(C]L(B . "0x6C7E") ; <CJK>
       (?$(C]M(B . "0x711A") ; <CJK>
       (?$(C]N(B . "0x76C6") ; <CJK>
       (?$(C]O(B . "0x7C89") ; <CJK>
       (?$(C]P(B . "0x7CDE") ; <CJK>
       (?$(C]Q(B . "0x7D1B") ; <CJK>
       (?$(C]R(B . "0x82AC") ; <CJK>
       (?$(C]S(B . "0x8CC1") ; <CJK>
       (?$(C]T(B . "0x96F0") ; <CJK>
       (?$(C]U(B . "0xF967") ; <CJK>
       (?$(C]V(B . "0x4F5B") ; <CJK>
       (?$(C]W(B . "0x5F17") ; <CJK>
       (?$(C]X(B . "0x5F7F") ; <CJK>
       (?$(C]Y(B . "0x62C2") ; <CJK>
       (?$(C]Z(B . "0x5D29") ; <CJK>
       (?$(C][(B . "0x670B") ; <CJK>
       (?$(C]\(B . "0x68DA") ; <CJK>
       (?$(C]](B . "0x787C") ; <CJK>
       (?$(C]^(B . "0x7E43") ; <CJK>
       (?$(C]_(B . "0x9D6C") ; <CJK>
       (?$(C]`(B . "0x4E15") ; <CJK>
       (?$(C]a(B . "0x5099") ; <CJK>
       (?$(C]b(B . "0x5315") ; <CJK>
       (?$(C]c(B . "0x532A") ; <CJK>
       (?$(C]d(B . "0x5351") ; <CJK>
       (?$(C]e(B . "0x5983") ; <CJK>
       (?$(C]f(B . "0x5A62") ; <CJK>
       (?$(C]g(B . "0x5E87") ; <CJK>
       (?$(C]h(B . "0x60B2") ; <CJK>
       (?$(C]i(B . "0x618A") ; <CJK>
       (?$(C]j(B . "0x6249") ; <CJK>
       (?$(C]k(B . "0x6279") ; <CJK>
       (?$(C]l(B . "0x6590") ; <CJK>
       (?$(C]m(B . "0x6787") ; <CJK>
       (?$(C]n(B . "0x69A7") ; <CJK>
       (?$(C]o(B . "0x6BD4") ; <CJK>
       (?$(C]p(B . "0x6BD6") ; <CJK>
       (?$(C]q(B . "0x6BD7") ; <CJK>
       (?$(C]r(B . "0x6BD8") ; <CJK>
       (?$(C]s(B . "0x6CB8") ; <CJK>
       (?$(C]t(B . "0xF968") ; <CJK>
       (?$(C]u(B . "0x7435") ; <CJK>
       (?$(C]v(B . "0x75FA") ; <CJK>
       (?$(C]w(B . "0x7812") ; <CJK>
       (?$(C]x(B . "0x7891") ; <CJK>
       (?$(C]y(B . "0x79D5") ; <CJK>
       (?$(C]z(B . "0x79D8") ; <CJK>
       (?$(C]{(B . "0x7C83") ; <CJK>
       (?$(C]|(B . "0x7DCB") ; <CJK>
       (?$(C]}(B . "0x7FE1") ; <CJK>
       (?$(C]~(B . "0x80A5") ; <CJK>
       (?$(C^!(B . "0x813E") ; <CJK>
       (?$(C^"(B . "0x81C2") ; <CJK>
       (?$(C^#(B . "0x83F2") ; <CJK>
       (?$(C^$(B . "0x871A") ; <CJK>
       (?$(C^%(B . "0x88E8") ; <CJK>
       (?$(C^&(B . "0x8AB9") ; <CJK>
       (?$(C^'(B . "0x8B6C") ; <CJK>
       (?$(C^((B . "0x8CBB") ; <CJK>
       (?$(C^)(B . "0x9119") ; <CJK>
       (?$(C^*(B . "0x975E") ; <CJK>
       (?$(C^+(B . "0x98DB") ; <CJK>
       (?$(C^,(B . "0x9F3B") ; <CJK>
       (?$(C^-(B . "0x56AC") ; <CJK>
       (?$(C^.(B . "0x5B2A") ; <CJK>
       (?$(C^/(B . "0x5F6C") ; <CJK>
       (?$(C^0(B . "0x658C") ; <CJK>
       (?$(C^1(B . "0x6AB3") ; <CJK>
       (?$(C^2(B . "0x6BAF") ; <CJK>
       (?$(C^3(B . "0x6D5C") ; <CJK>
       (?$(C^4(B . "0x6FF1") ; <CJK>
       (?$(C^5(B . "0x7015") ; <CJK>
       (?$(C^6(B . "0x725D") ; <CJK>
       (?$(C^7(B . "0x73AD") ; <CJK>
       (?$(C^8(B . "0x8CA7") ; <CJK>
       (?$(C^9(B . "0x8CD3") ; <CJK>
       (?$(C^:(B . "0x983B") ; <CJK>
       (?$(C^;(B . "0x6191") ; <CJK>
       (?$(C^<(B . "0x6C37") ; <CJK>
       (?$(C^=(B . "0x8058") ; <CJK>
       (?$(C^>(B . "0x9A01") ; <CJK>
       (?$(C^?(B . "0x4E4D") ; <CJK>
       (?$(C^@(B . "0x4E8B") ; <CJK>
       (?$(C^A(B . "0x4E9B") ; <CJK>
       (?$(C^B(B . "0x4ED5") ; <CJK>
       (?$(C^C(B . "0x4F3A") ; <CJK>
       (?$(C^D(B . "0x4F3C") ; <CJK>
       (?$(C^E(B . "0x4F7F") ; <CJK>
       (?$(C^F(B . "0x4FDF") ; <CJK>
       (?$(C^G(B . "0x50FF") ; <CJK>
       (?$(C^H(B . "0x53F2") ; <CJK>
       (?$(C^I(B . "0x53F8") ; <CJK>
       (?$(C^J(B . "0x5506") ; <CJK>
       (?$(C^K(B . "0x55E3") ; <CJK>
       (?$(C^L(B . "0x56DB") ; <CJK>
       (?$(C^M(B . "0x58EB") ; <CJK>
       (?$(C^N(B . "0x5962") ; <CJK>
       (?$(C^O(B . "0x5A11") ; <CJK>
       (?$(C^P(B . "0x5BEB") ; <CJK>
       (?$(C^Q(B . "0x5BFA") ; <CJK>
       (?$(C^R(B . "0x5C04") ; <CJK>
       (?$(C^S(B . "0x5DF3") ; <CJK>
       (?$(C^T(B . "0x5E2B") ; <CJK>
       (?$(C^U(B . "0x5F99") ; <CJK>
       (?$(C^V(B . "0x601D") ; <CJK>
       (?$(C^W(B . "0x6368") ; <CJK>
       (?$(C^X(B . "0x659C") ; <CJK>
       (?$(C^Y(B . "0x65AF") ; <CJK>
       (?$(C^Z(B . "0x67F6") ; <CJK>
       (?$(C^[(B . "0x67FB") ; <CJK>
       (?$(C^\(B . "0x68AD") ; <CJK>
       (?$(C^](B . "0x6B7B") ; <CJK>
       (?$(C^^(B . "0x6C99") ; <CJK>
       (?$(C^_(B . "0x6CD7") ; <CJK>
       (?$(C^`(B . "0x6E23") ; <CJK>
       (?$(C^a(B . "0x7009") ; <CJK>
       (?$(C^b(B . "0x7345") ; <CJK>
       (?$(C^c(B . "0x7802") ; <CJK>
       (?$(C^d(B . "0x793E") ; <CJK>
       (?$(C^e(B . "0x7940") ; <CJK>
       (?$(C^f(B . "0x7960") ; <CJK>
       (?$(C^g(B . "0x79C1") ; <CJK>
       (?$(C^h(B . "0x7BE9") ; <CJK>
       (?$(C^i(B . "0x7D17") ; <CJK>
       (?$(C^j(B . "0x7D72") ; <CJK>
       (?$(C^k(B . "0x8086") ; <CJK>
       (?$(C^l(B . "0x820D") ; <CJK>
       (?$(C^m(B . "0x838E") ; <CJK>
       (?$(C^n(B . "0x84D1") ; <CJK>
       (?$(C^o(B . "0x86C7") ; <CJK>
       (?$(C^p(B . "0x88DF") ; <CJK>
       (?$(C^q(B . "0x8A50") ; <CJK>
       (?$(C^r(B . "0x8A5E") ; <CJK>
       (?$(C^s(B . "0x8B1D") ; <CJK>
       (?$(C^t(B . "0x8CDC") ; <CJK>
       (?$(C^u(B . "0x8D66") ; <CJK>
       (?$(C^v(B . "0x8FAD") ; <CJK>
       (?$(C^w(B . "0x90AA") ; <CJK>
       (?$(C^x(B . "0x98FC") ; <CJK>
       (?$(C^y(B . "0x99DF") ; <CJK>
       (?$(C^z(B . "0x9E9D") ; <CJK>
       (?$(C^{(B . "0x524A") ; <CJK>
       (?$(C^|(B . "0xF969") ; <CJK>
       (?$(C^}(B . "0x6714") ; <CJK>
       (?$(C^~(B . "0xF96A") ; <CJK>
       (?$(C_!(B . "0x5098") ; <CJK>
       (?$(C_"(B . "0x522A") ; <CJK>
       (?$(C_#(B . "0x5C71") ; <CJK>
       (?$(C_$(B . "0x6563") ; <CJK>
       (?$(C_%(B . "0x6C55") ; <CJK>
       (?$(C_&(B . "0x73CA") ; <CJK>
       (?$(C_'(B . "0x7523") ; <CJK>
       (?$(C_((B . "0x759D") ; <CJK>
       (?$(C_)(B . "0x7B97") ; <CJK>
       (?$(C_*(B . "0x849C") ; <CJK>
       (?$(C_+(B . "0x9178") ; <CJK>
       (?$(C_,(B . "0x9730") ; <CJK>
       (?$(C_-(B . "0x4E77") ; <CJK>
       (?$(C_.(B . "0x6492") ; <CJK>
       (?$(C_/(B . "0x6BBA") ; <CJK>
       (?$(C_0(B . "0x715E") ; <CJK>
       (?$(C_1(B . "0x85A9") ; <CJK>
       (?$(C_2(B . "0x4E09") ; <CJK>
       (?$(C_3(B . "0xF96B") ; <CJK>
       (?$(C_4(B . "0x6749") ; <CJK>
       (?$(C_5(B . "0x68EE") ; <CJK>
       (?$(C_6(B . "0x6E17") ; <CJK>
       (?$(C_7(B . "0x829F") ; <CJK>
       (?$(C_8(B . "0x8518") ; <CJK>
       (?$(C_9(B . "0x886B") ; <CJK>
       (?$(C_:(B . "0x63F7") ; <CJK>
       (?$(C_;(B . "0x6F81") ; <CJK>
       (?$(C_<(B . "0x9212") ; <CJK>
       (?$(C_=(B . "0x98AF") ; <CJK>
       (?$(C_>(B . "0x4E0A") ; <CJK>
       (?$(C_?(B . "0x50B7") ; <CJK>
       (?$(C_@(B . "0x50CF") ; <CJK>
       (?$(C_A(B . "0x511F") ; <CJK>
       (?$(C_B(B . "0x5546") ; <CJK>
       (?$(C_C(B . "0x55AA") ; <CJK>
       (?$(C_D(B . "0x5617") ; <CJK>
       (?$(C_E(B . "0x5B40") ; <CJK>
       (?$(C_F(B . "0x5C19") ; <CJK>
       (?$(C_G(B . "0x5CE0") ; <CJK>
       (?$(C_H(B . "0x5E38") ; <CJK>
       (?$(C_I(B . "0x5E8A") ; <CJK>
       (?$(C_J(B . "0x5EA0") ; <CJK>
       (?$(C_K(B . "0x5EC2") ; <CJK>
       (?$(C_L(B . "0x60F3") ; <CJK>
       (?$(C_M(B . "0x6851") ; <CJK>
       (?$(C_N(B . "0x6A61") ; <CJK>
       (?$(C_O(B . "0x6E58") ; <CJK>
       (?$(C_P(B . "0x723D") ; <CJK>
       (?$(C_Q(B . "0x7240") ; <CJK>
       (?$(C_R(B . "0x72C0") ; <CJK>
       (?$(C_S(B . "0x76F8") ; <CJK>
       (?$(C_T(B . "0x7965") ; <CJK>
       (?$(C_U(B . "0x7BB1") ; <CJK>
       (?$(C_V(B . "0x7FD4") ; <CJK>
       (?$(C_W(B . "0x88F3") ; <CJK>
       (?$(C_X(B . "0x89F4") ; <CJK>
       (?$(C_Y(B . "0x8A73") ; <CJK>
       (?$(C_Z(B . "0x8C61") ; <CJK>
       (?$(C_[(B . "0x8CDE") ; <CJK>
       (?$(C_\(B . "0x971C") ; <CJK>
       (?$(C_](B . "0x585E") ; <CJK>
       (?$(C_^(B . "0x74BD") ; <CJK>
       (?$(C__(B . "0x8CFD") ; <CJK>
       (?$(C_`(B . "0x55C7") ; <CJK>
       (?$(C_a(B . "0xF96C") ; <CJK>
       (?$(C_b(B . "0x7A61") ; <CJK>
       (?$(C_c(B . "0x7D22") ; <CJK>
       (?$(C_d(B . "0x8272") ; <CJK>
       (?$(C_e(B . "0x7272") ; <CJK>
       (?$(C_f(B . "0x751F") ; <CJK>
       (?$(C_g(B . "0x7525") ; <CJK>
       (?$(C_h(B . "0xF96D") ; <CJK>
       (?$(C_i(B . "0x7B19") ; <CJK>
       (?$(C_j(B . "0x5885") ; <CJK>
       (?$(C_k(B . "0x58FB") ; <CJK>
       (?$(C_l(B . "0x5DBC") ; <CJK>
       (?$(C_m(B . "0x5E8F") ; <CJK>
       (?$(C_n(B . "0x5EB6") ; <CJK>
       (?$(C_o(B . "0x5F90") ; <CJK>
       (?$(C_p(B . "0x6055") ; <CJK>
       (?$(C_q(B . "0x6292") ; <CJK>
       (?$(C_r(B . "0x637F") ; <CJK>
       (?$(C_s(B . "0x654D") ; <CJK>
       (?$(C_t(B . "0x6691") ; <CJK>
       (?$(C_u(B . "0x66D9") ; <CJK>
       (?$(C_v(B . "0x66F8") ; <CJK>
       (?$(C_w(B . "0x6816") ; <CJK>
       (?$(C_x(B . "0x68F2") ; <CJK>
       (?$(C_y(B . "0x7280") ; <CJK>
       (?$(C_z(B . "0x745E") ; <CJK>
       (?$(C_{(B . "0x7B6E") ; <CJK>
       (?$(C_|(B . "0x7D6E") ; <CJK>
       (?$(C_}(B . "0x7DD6") ; <CJK>
       (?$(C_~(B . "0x7F72") ; <CJK>
       (?$(C`!(B . "0x80E5") ; <CJK>
       (?$(C`"(B . "0x8212") ; <CJK>
       (?$(C`#(B . "0x85AF") ; <CJK>
       (?$(C`$(B . "0x897F") ; <CJK>
       (?$(C`%(B . "0x8A93") ; <CJK>
       (?$(C`&(B . "0x901D") ; <CJK>
       (?$(C`'(B . "0x92E4") ; <CJK>
       (?$(C`((B . "0x9ECD") ; <CJK>
       (?$(C`)(B . "0x9F20") ; <CJK>
       (?$(C`*(B . "0x5915") ; <CJK>
       (?$(C`+(B . "0x596D") ; <CJK>
       (?$(C`,(B . "0x5E2D") ; <CJK>
       (?$(C`-(B . "0x60DC") ; <CJK>
       (?$(C`.(B . "0x6614") ; <CJK>
       (?$(C`/(B . "0x6673") ; <CJK>
       (?$(C`0(B . "0x6790") ; <CJK>
       (?$(C`1(B . "0x6C50") ; <CJK>
       (?$(C`2(B . "0x6DC5") ; <CJK>
       (?$(C`3(B . "0x6F5F") ; <CJK>
       (?$(C`4(B . "0x77F3") ; <CJK>
       (?$(C`5(B . "0x78A9") ; <CJK>
       (?$(C`6(B . "0x84C6") ; <CJK>
       (?$(C`7(B . "0x91CB") ; <CJK>
       (?$(C`8(B . "0x932B") ; <CJK>
       (?$(C`9(B . "0x4ED9") ; <CJK>
       (?$(C`:(B . "0x50CA") ; <CJK>
       (?$(C`;(B . "0x5148") ; <CJK>
       (?$(C`<(B . "0x5584") ; <CJK>
       (?$(C`=(B . "0x5B0B") ; <CJK>
       (?$(C`>(B . "0x5BA3") ; <CJK>
       (?$(C`?(B . "0x6247") ; <CJK>
       (?$(C`@(B . "0x657E") ; <CJK>
       (?$(C`A(B . "0x65CB") ; <CJK>
       (?$(C`B(B . "0x6E32") ; <CJK>
       (?$(C`C(B . "0x717D") ; <CJK>
       (?$(C`D(B . "0x7401") ; <CJK>
       (?$(C`E(B . "0x7444") ; <CJK>
       (?$(C`F(B . "0x7487") ; <CJK>
       (?$(C`G(B . "0x74BF") ; <CJK>
       (?$(C`H(B . "0x766C") ; <CJK>
       (?$(C`I(B . "0x79AA") ; <CJK>
       (?$(C`J(B . "0x7DDA") ; <CJK>
       (?$(C`K(B . "0x7E55") ; <CJK>
       (?$(C`L(B . "0x7FA8") ; <CJK>
       (?$(C`M(B . "0x817A") ; <CJK>
       (?$(C`N(B . "0x81B3") ; <CJK>
       (?$(C`O(B . "0x8239") ; <CJK>
       (?$(C`P(B . "0x861A") ; <CJK>
       (?$(C`Q(B . "0x87EC") ; <CJK>
       (?$(C`R(B . "0x8A75") ; <CJK>
       (?$(C`S(B . "0x8DE3") ; <CJK>
       (?$(C`T(B . "0x9078") ; <CJK>
       (?$(C`U(B . "0x9291") ; <CJK>
       (?$(C`V(B . "0x9425") ; <CJK>
       (?$(C`W(B . "0x994D") ; <CJK>
       (?$(C`X(B . "0x9BAE") ; <CJK>
       (?$(C`Y(B . "0x5368") ; <CJK>
       (?$(C`Z(B . "0x5C51") ; <CJK>
       (?$(C`[(B . "0x6954") ; <CJK>
       (?$(C`\(B . "0x6CC4") ; <CJK>
       (?$(C`](B . "0x6D29") ; <CJK>
       (?$(C`^(B . "0x6E2B") ; <CJK>
       (?$(C`_(B . "0x820C") ; <CJK>
       (?$(C``(B . "0x859B") ; <CJK>
       (?$(C`a(B . "0x893B") ; <CJK>
       (?$(C`b(B . "0x8A2D") ; <CJK>
       (?$(C`c(B . "0x8AAA") ; <CJK>
       (?$(C`d(B . "0x96EA") ; <CJK>
       (?$(C`e(B . "0x9F67") ; <CJK>
       (?$(C`f(B . "0x5261") ; <CJK>
       (?$(C`g(B . "0x66B9") ; <CJK>
       (?$(C`h(B . "0x6BB2") ; <CJK>
       (?$(C`i(B . "0x7E96") ; <CJK>
       (?$(C`j(B . "0x87FE") ; <CJK>
       (?$(C`k(B . "0x8D0D") ; <CJK>
       (?$(C`l(B . "0x9583") ; <CJK>
       (?$(C`m(B . "0x965D") ; <CJK>
       (?$(C`n(B . "0x651D") ; <CJK>
       (?$(C`o(B . "0x6D89") ; <CJK>
       (?$(C`p(B . "0x71EE") ; <CJK>
       (?$(C`q(B . "0xF96E") ; <CJK>
       (?$(C`r(B . "0x57CE") ; <CJK>
       (?$(C`s(B . "0x59D3") ; <CJK>
       (?$(C`t(B . "0x5BAC") ; <CJK>
       (?$(C`u(B . "0x6027") ; <CJK>
       (?$(C`v(B . "0x60FA") ; <CJK>
       (?$(C`w(B . "0x6210") ; <CJK>
       (?$(C`x(B . "0x661F") ; <CJK>
       (?$(C`y(B . "0x665F") ; <CJK>
       (?$(C`z(B . "0x7329") ; <CJK>
       (?$(C`{(B . "0x73F9") ; <CJK>
       (?$(C`|(B . "0x76DB") ; <CJK>
       (?$(C`}(B . "0x7701") ; <CJK>
       (?$(C`~(B . "0x7B6C") ; <CJK>
       (?$(Ca!(B . "0x8056") ; <CJK>
       (?$(Ca"(B . "0x8072") ; <CJK>
       (?$(Ca#(B . "0x8165") ; <CJK>
       (?$(Ca$(B . "0x8AA0") ; <CJK>
       (?$(Ca%(B . "0x9192") ; <CJK>
       (?$(Ca&(B . "0x4E16") ; <CJK>
       (?$(Ca'(B . "0x52E2") ; <CJK>
       (?$(Ca((B . "0x6B72") ; <CJK>
       (?$(Ca)(B . "0x6D17") ; <CJK>
       (?$(Ca*(B . "0x7A05") ; <CJK>
       (?$(Ca+(B . "0x7B39") ; <CJK>
       (?$(Ca,(B . "0x7D30") ; <CJK>
       (?$(Ca-(B . "0xF96F") ; <CJK>
       (?$(Ca.(B . "0x8CB0") ; <CJK>
       (?$(Ca/(B . "0x53EC") ; <CJK>
       (?$(Ca0(B . "0x562F") ; <CJK>
       (?$(Ca1(B . "0x5851") ; <CJK>
       (?$(Ca2(B . "0x5BB5") ; <CJK>
       (?$(Ca3(B . "0x5C0F") ; <CJK>
       (?$(Ca4(B . "0x5C11") ; <CJK>
       (?$(Ca5(B . "0x5DE2") ; <CJK>
       (?$(Ca6(B . "0x6240") ; <CJK>
       (?$(Ca7(B . "0x6383") ; <CJK>
       (?$(Ca8(B . "0x6414") ; <CJK>
       (?$(Ca9(B . "0x662D") ; <CJK>
       (?$(Ca:(B . "0x68B3") ; <CJK>
       (?$(Ca;(B . "0x6CBC") ; <CJK>
       (?$(Ca<(B . "0x6D88") ; <CJK>
       (?$(Ca=(B . "0x6EAF") ; <CJK>
       (?$(Ca>(B . "0x701F") ; <CJK>
       (?$(Ca?(B . "0x70A4") ; <CJK>
       (?$(Ca@(B . "0x71D2") ; <CJK>
       (?$(CaA(B . "0x7526") ; <CJK>
       (?$(CaB(B . "0x758F") ; <CJK>
       (?$(CaC(B . "0x758E") ; <CJK>
       (?$(CaD(B . "0x7619") ; <CJK>
       (?$(CaE(B . "0x7B11") ; <CJK>
       (?$(CaF(B . "0x7BE0") ; <CJK>
       (?$(CaG(B . "0x7C2B") ; <CJK>
       (?$(CaH(B . "0x7D20") ; <CJK>
       (?$(CaI(B . "0x7D39") ; <CJK>
       (?$(CaJ(B . "0x852C") ; <CJK>
       (?$(CaK(B . "0x856D") ; <CJK>
       (?$(CaL(B . "0x8607") ; <CJK>
       (?$(CaM(B . "0x8A34") ; <CJK>
       (?$(CaN(B . "0x900D") ; <CJK>
       (?$(CaO(B . "0x9061") ; <CJK>
       (?$(CaP(B . "0x90B5") ; <CJK>
       (?$(CaQ(B . "0x92B7") ; <CJK>
       (?$(CaR(B . "0x97F6") ; <CJK>
       (?$(CaS(B . "0x9A37") ; <CJK>
       (?$(CaT(B . "0x4FD7") ; <CJK>
       (?$(CaU(B . "0x5C6C") ; <CJK>
       (?$(CaV(B . "0x675F") ; <CJK>
       (?$(CaW(B . "0x6D91") ; <CJK>
       (?$(CaX(B . "0x7C9F") ; <CJK>
       (?$(CaY(B . "0x7E8C") ; <CJK>
       (?$(CaZ(B . "0x8B16") ; <CJK>
       (?$(Ca[(B . "0x8D16") ; <CJK>
       (?$(Ca\(B . "0x901F") ; <CJK>
       (?$(Ca](B . "0x5B6B") ; <CJK>
       (?$(Ca^(B . "0x5DFD") ; <CJK>
       (?$(Ca_(B . "0x640D") ; <CJK>
       (?$(Ca`(B . "0x84C0") ; <CJK>
       (?$(Caa(B . "0x905C") ; <CJK>
       (?$(Cab(B . "0x98E1") ; <CJK>
       (?$(Cac(B . "0x7387") ; <CJK>
       (?$(Cad(B . "0x5B8B") ; <CJK>
       (?$(Cae(B . "0x609A") ; <CJK>
       (?$(Caf(B . "0x677E") ; <CJK>
       (?$(Cag(B . "0x6DDE") ; <CJK>
       (?$(Cah(B . "0x8A1F") ; <CJK>
       (?$(Cai(B . "0x8AA6") ; <CJK>
       (?$(Caj(B . "0x9001") ; <CJK>
       (?$(Cak(B . "0x980C") ; <CJK>
       (?$(Cal(B . "0x5237") ; <CJK>
       (?$(Cam(B . "0xF970") ; <CJK>
       (?$(Can(B . "0x7051") ; <CJK>
       (?$(Cao(B . "0x788E") ; <CJK>
       (?$(Cap(B . "0x9396") ; <CJK>
       (?$(Caq(B . "0x8870") ; <CJK>
       (?$(Car(B . "0x91D7") ; <CJK>
       (?$(Cas(B . "0x4FEE") ; <CJK>
       (?$(Cat(B . "0x53D7") ; <CJK>
       (?$(Cau(B . "0x55FD") ; <CJK>
       (?$(Cav(B . "0x56DA") ; <CJK>
       (?$(Caw(B . "0x5782") ; <CJK>
       (?$(Cax(B . "0x58FD") ; <CJK>
       (?$(Cay(B . "0x5AC2") ; <CJK>
       (?$(Caz(B . "0x5B88") ; <CJK>
       (?$(Ca{(B . "0x5CAB") ; <CJK>
       (?$(Ca|(B . "0x5CC0") ; <CJK>
       (?$(Ca}(B . "0x5E25") ; <CJK>
       (?$(Ca~(B . "0x6101") ; <CJK>
       (?$(Cb!(B . "0x620D") ; <CJK>
       (?$(Cb"(B . "0x624B") ; <CJK>
       (?$(Cb#(B . "0x6388") ; <CJK>
       (?$(Cb$(B . "0x641C") ; <CJK>
       (?$(Cb%(B . "0x6536") ; <CJK>
       (?$(Cb&(B . "0x6578") ; <CJK>
       (?$(Cb'(B . "0x6A39") ; <CJK>
       (?$(Cb((B . "0x6B8A") ; <CJK>
       (?$(Cb)(B . "0x6C34") ; <CJK>
       (?$(Cb*(B . "0x6D19") ; <CJK>
       (?$(Cb+(B . "0x6F31") ; <CJK>
       (?$(Cb,(B . "0x71E7") ; <CJK>
       (?$(Cb-(B . "0x72E9") ; <CJK>
       (?$(Cb.(B . "0x7378") ; <CJK>
       (?$(Cb/(B . "0x7407") ; <CJK>
       (?$(Cb0(B . "0x74B2") ; <CJK>
       (?$(Cb1(B . "0x7626") ; <CJK>
       (?$(Cb2(B . "0x7761") ; <CJK>
       (?$(Cb3(B . "0x79C0") ; <CJK>
       (?$(Cb4(B . "0x7A57") ; <CJK>
       (?$(Cb5(B . "0x7AEA") ; <CJK>
       (?$(Cb6(B . "0x7CB9") ; <CJK>
       (?$(Cb7(B . "0x7D8F") ; <CJK>
       (?$(Cb8(B . "0x7DAC") ; <CJK>
       (?$(Cb9(B . "0x7E61") ; <CJK>
       (?$(Cb:(B . "0x7F9E") ; <CJK>
       (?$(Cb;(B . "0x8129") ; <CJK>
       (?$(Cb<(B . "0x8331") ; <CJK>
       (?$(Cb=(B . "0x8490") ; <CJK>
       (?$(Cb>(B . "0x84DA") ; <CJK>
       (?$(Cb?(B . "0x85EA") ; <CJK>
       (?$(Cb@(B . "0x8896") ; <CJK>
       (?$(CbA(B . "0x8AB0") ; <CJK>
       (?$(CbB(B . "0x8B90") ; <CJK>
       (?$(CbC(B . "0x8F38") ; <CJK>
       (?$(CbD(B . "0x9042") ; <CJK>
       (?$(CbE(B . "0x9083") ; <CJK>
       (?$(CbF(B . "0x916C") ; <CJK>
       (?$(CbG(B . "0x9296") ; <CJK>
       (?$(CbH(B . "0x92B9") ; <CJK>
       (?$(CbI(B . "0x968B") ; <CJK>
       (?$(CbJ(B . "0x96A7") ; <CJK>
       (?$(CbK(B . "0x96A8") ; <CJK>
       (?$(CbL(B . "0x96D6") ; <CJK>
       (?$(CbM(B . "0x9700") ; <CJK>
       (?$(CbN(B . "0x9808") ; <CJK>
       (?$(CbO(B . "0x9996") ; <CJK>
       (?$(CbP(B . "0x9AD3") ; <CJK>
       (?$(CbQ(B . "0x9B1A") ; <CJK>
       (?$(CbR(B . "0x53D4") ; <CJK>
       (?$(CbS(B . "0x587E") ; <CJK>
       (?$(CbT(B . "0x5919") ; <CJK>
       (?$(CbU(B . "0x5B70") ; <CJK>
       (?$(CbV(B . "0x5BBF") ; <CJK>
       (?$(CbW(B . "0x6DD1") ; <CJK>
       (?$(CbX(B . "0x6F5A") ; <CJK>
       (?$(CbY(B . "0x719F") ; <CJK>
       (?$(CbZ(B . "0x7421") ; <CJK>
       (?$(Cb[(B . "0x74B9") ; <CJK>
       (?$(Cb\(B . "0x8085") ; <CJK>
       (?$(Cb](B . "0x83FD") ; <CJK>
       (?$(Cb^(B . "0x5DE1") ; <CJK>
       (?$(Cb_(B . "0x5F87") ; <CJK>
       (?$(Cb`(B . "0x5FAA") ; <CJK>
       (?$(Cba(B . "0x6042") ; <CJK>
       (?$(Cbb(B . "0x65EC") ; <CJK>
       (?$(Cbc(B . "0x6812") ; <CJK>
       (?$(Cbd(B . "0x696F") ; <CJK>
       (?$(Cbe(B . "0x6A53") ; <CJK>
       (?$(Cbf(B . "0x6B89") ; <CJK>
       (?$(Cbg(B . "0x6D35") ; <CJK>
       (?$(Cbh(B . "0x6DF3") ; <CJK>
       (?$(Cbi(B . "0x73E3") ; <CJK>
       (?$(Cbj(B . "0x76FE") ; <CJK>
       (?$(Cbk(B . "0x77AC") ; <CJK>
       (?$(Cbl(B . "0x7B4D") ; <CJK>
       (?$(Cbm(B . "0x7D14") ; <CJK>
       (?$(Cbn(B . "0x8123") ; <CJK>
       (?$(Cbo(B . "0x821C") ; <CJK>
       (?$(Cbp(B . "0x8340") ; <CJK>
       (?$(Cbq(B . "0x84F4") ; <CJK>
       (?$(Cbr(B . "0x8563") ; <CJK>
       (?$(Cbs(B . "0x8A62") ; <CJK>
       (?$(Cbt(B . "0x8AC4") ; <CJK>
       (?$(Cbu(B . "0x9187") ; <CJK>
       (?$(Cbv(B . "0x931E") ; <CJK>
       (?$(Cbw(B . "0x9806") ; <CJK>
       (?$(Cbx(B . "0x99B4") ; <CJK>
       (?$(Cby(B . "0x620C") ; <CJK>
       (?$(Cbz(B . "0x8853") ; <CJK>
       (?$(Cb{(B . "0x8FF0") ; <CJK>
       (?$(Cb|(B . "0x9265") ; <CJK>
       (?$(Cb}(B . "0x5D07") ; <CJK>
       (?$(Cb~(B . "0x5D27") ; <CJK>
       (?$(Cc!(B . "0x5D69") ; <CJK>
       (?$(Cc"(B . "0x745F") ; <CJK>
       (?$(Cc#(B . "0x819D") ; <CJK>
       (?$(Cc$(B . "0x8768") ; <CJK>
       (?$(Cc%(B . "0x6FD5") ; <CJK>
       (?$(Cc&(B . "0x62FE") ; <CJK>
       (?$(Cc'(B . "0x7FD2") ; <CJK>
       (?$(Cc((B . "0x8936") ; <CJK>
       (?$(Cc)(B . "0x8972") ; <CJK>
       (?$(Cc*(B . "0x4E1E") ; <CJK>
       (?$(Cc+(B . "0x4E58") ; <CJK>
       (?$(Cc,(B . "0x50E7") ; <CJK>
       (?$(Cc-(B . "0x52DD") ; <CJK>
       (?$(Cc.(B . "0x5347") ; <CJK>
       (?$(Cc/(B . "0x627F") ; <CJK>
       (?$(Cc0(B . "0x6607") ; <CJK>
       (?$(Cc1(B . "0x7E69") ; <CJK>
       (?$(Cc2(B . "0x8805") ; <CJK>
       (?$(Cc3(B . "0x965E") ; <CJK>
       (?$(Cc4(B . "0x4F8D") ; <CJK>
       (?$(Cc5(B . "0x5319") ; <CJK>
       (?$(Cc6(B . "0x5636") ; <CJK>
       (?$(Cc7(B . "0x59CB") ; <CJK>
       (?$(Cc8(B . "0x5AA4") ; <CJK>
       (?$(Cc9(B . "0x5C38") ; <CJK>
       (?$(Cc:(B . "0x5C4E") ; <CJK>
       (?$(Cc;(B . "0x5C4D") ; <CJK>
       (?$(Cc<(B . "0x5E02") ; <CJK>
       (?$(Cc=(B . "0x5F11") ; <CJK>
       (?$(Cc>(B . "0x6043") ; <CJK>
       (?$(Cc?(B . "0x65BD") ; <CJK>
       (?$(Cc@(B . "0x662F") ; <CJK>
       (?$(CcA(B . "0x6642") ; <CJK>
       (?$(CcB(B . "0x67BE") ; <CJK>
       (?$(CcC(B . "0x67F4") ; <CJK>
       (?$(CcD(B . "0x731C") ; <CJK>
       (?$(CcE(B . "0x77E2") ; <CJK>
       (?$(CcF(B . "0x793A") ; <CJK>
       (?$(CcG(B . "0x7FC5") ; <CJK>
       (?$(CcH(B . "0x8494") ; <CJK>
       (?$(CcI(B . "0x84CD") ; <CJK>
       (?$(CcJ(B . "0x8996") ; <CJK>
       (?$(CcK(B . "0x8A66") ; <CJK>
       (?$(CcL(B . "0x8A69") ; <CJK>
       (?$(CcM(B . "0x8AE1") ; <CJK>
       (?$(CcN(B . "0x8C55") ; <CJK>
       (?$(CcO(B . "0x8C7A") ; <CJK>
       (?$(CcP(B . "0x57F4") ; <CJK>
       (?$(CcQ(B . "0x5BD4") ; <CJK>
       (?$(CcR(B . "0x5F0F") ; <CJK>
       (?$(CcS(B . "0x606F") ; <CJK>
       (?$(CcT(B . "0x62ED") ; <CJK>
       (?$(CcU(B . "0x690D") ; <CJK>
       (?$(CcV(B . "0x6B96") ; <CJK>
       (?$(CcW(B . "0x6E5C") ; <CJK>
       (?$(CcX(B . "0x7184") ; <CJK>
       (?$(CcY(B . "0x7BD2") ; <CJK>
       (?$(CcZ(B . "0x8755") ; <CJK>
       (?$(Cc[(B . "0x8B58") ; <CJK>
       (?$(Cc\(B . "0x8EFE") ; <CJK>
       (?$(Cc](B . "0x98DF") ; <CJK>
       (?$(Cc^(B . "0x98FE") ; <CJK>
       (?$(Cc_(B . "0x4F38") ; <CJK>
       (?$(Cc`(B . "0x4F81") ; <CJK>
       (?$(Cca(B . "0x4FE1") ; <CJK>
       (?$(Ccb(B . "0x547B") ; <CJK>
       (?$(Ccc(B . "0x5A20") ; <CJK>
       (?$(Ccd(B . "0x5BB8") ; <CJK>
       (?$(Cce(B . "0x613C") ; <CJK>
       (?$(Ccf(B . "0x65B0") ; <CJK>
       (?$(Ccg(B . "0x6668") ; <CJK>
       (?$(Cch(B . "0x71FC") ; <CJK>
       (?$(Cci(B . "0x7533") ; <CJK>
       (?$(Ccj(B . "0x795E") ; <CJK>
       (?$(Cck(B . "0x7D33") ; <CJK>
       (?$(Ccl(B . "0x814E") ; <CJK>
       (?$(Ccm(B . "0x81E3") ; <CJK>
       (?$(Ccn(B . "0x8398") ; <CJK>
       (?$(Cco(B . "0x85AA") ; <CJK>
       (?$(Ccp(B . "0x85CE") ; <CJK>
       (?$(Ccq(B . "0x8703") ; <CJK>
       (?$(Ccr(B . "0x8A0A") ; <CJK>
       (?$(Ccs(B . "0x8EAB") ; <CJK>
       (?$(Cct(B . "0x8F9B") ; <CJK>
       (?$(Ccu(B . "0xF971") ; <CJK>
       (?$(Ccv(B . "0x8FC5") ; <CJK>
       (?$(Ccw(B . "0x5931") ; <CJK>
       (?$(Ccx(B . "0x5BA4") ; <CJK>
       (?$(Ccy(B . "0x5BE6") ; <CJK>
       (?$(Ccz(B . "0x6089") ; <CJK>
       (?$(Cc{(B . "0x5BE9") ; <CJK>
       (?$(Cc|(B . "0x5C0B") ; <CJK>
       (?$(Cc}(B . "0x5FC3") ; <CJK>
       (?$(Cc~(B . "0x6C81") ; <CJK>
       (?$(Cd!(B . "0xF972") ; <CJK>
       (?$(Cd"(B . "0x6DF1") ; <CJK>
       (?$(Cd#(B . "0x700B") ; <CJK>
       (?$(Cd$(B . "0x751A") ; <CJK>
       (?$(Cd%(B . "0x82AF") ; <CJK>
       (?$(Cd&(B . "0x8AF6") ; <CJK>
       (?$(Cd'(B . "0x4EC0") ; <CJK>
       (?$(Cd((B . "0x5341") ; <CJK>
       (?$(Cd)(B . "0xF973") ; <CJK>
       (?$(Cd*(B . "0x96D9") ; <CJK>
       (?$(Cd+(B . "0x6C0F") ; <CJK>
       (?$(Cd,(B . "0x4E9E") ; <CJK>
       (?$(Cd-(B . "0x4FC4") ; <CJK>
       (?$(Cd.(B . "0x5152") ; <CJK>
       (?$(Cd/(B . "0x555E") ; <CJK>
       (?$(Cd0(B . "0x5A25") ; <CJK>
       (?$(Cd1(B . "0x5CE8") ; <CJK>
       (?$(Cd2(B . "0x6211") ; <CJK>
       (?$(Cd3(B . "0x7259") ; <CJK>
       (?$(Cd4(B . "0x82BD") ; <CJK>
       (?$(Cd5(B . "0x83AA") ; <CJK>
       (?$(Cd6(B . "0x86FE") ; <CJK>
       (?$(Cd7(B . "0x8859") ; <CJK>
       (?$(Cd8(B . "0x8A1D") ; <CJK>
       (?$(Cd9(B . "0x963F") ; <CJK>
       (?$(Cd:(B . "0x96C5") ; <CJK>
       (?$(Cd;(B . "0x9913") ; <CJK>
       (?$(Cd<(B . "0x9D09") ; <CJK>
       (?$(Cd=(B . "0x9D5D") ; <CJK>
       (?$(Cd>(B . "0x580A") ; <CJK>
       (?$(Cd?(B . "0x5CB3") ; <CJK>
       (?$(Cd@(B . "0x5DBD") ; <CJK>
       (?$(CdA(B . "0x5E44") ; <CJK>
       (?$(CdB(B . "0x60E1") ; <CJK>
       (?$(CdC(B . "0x6115") ; <CJK>
       (?$(CdD(B . "0x63E1") ; <CJK>
       (?$(CdE(B . "0x6A02") ; <CJK>
       (?$(CdF(B . "0x6E25") ; <CJK>
       (?$(CdG(B . "0x9102") ; <CJK>
       (?$(CdH(B . "0x9354") ; <CJK>
       (?$(CdI(B . "0x984E") ; <CJK>
       (?$(CdJ(B . "0x9C10") ; <CJK>
       (?$(CdK(B . "0x9F77") ; <CJK>
       (?$(CdL(B . "0x5B89") ; <CJK>
       (?$(CdM(B . "0x5CB8") ; <CJK>
       (?$(CdN(B . "0x6309") ; <CJK>
       (?$(CdO(B . "0x664F") ; <CJK>
       (?$(CdP(B . "0x6848") ; <CJK>
       (?$(CdQ(B . "0x773C") ; <CJK>
       (?$(CdR(B . "0x96C1") ; <CJK>
       (?$(CdS(B . "0x978D") ; <CJK>
       (?$(CdT(B . "0x9854") ; <CJK>
       (?$(CdU(B . "0x9B9F") ; <CJK>
       (?$(CdV(B . "0x65A1") ; <CJK>
       (?$(CdW(B . "0x8B01") ; <CJK>
       (?$(CdX(B . "0x8ECB") ; <CJK>
       (?$(CdY(B . "0x95BC") ; <CJK>
       (?$(CdZ(B . "0x5535") ; <CJK>
       (?$(Cd[(B . "0x5CA9") ; <CJK>
       (?$(Cd\(B . "0x5DD6") ; <CJK>
       (?$(Cd](B . "0x5EB5") ; <CJK>
       (?$(Cd^(B . "0x6697") ; <CJK>
       (?$(Cd_(B . "0x764C") ; <CJK>
       (?$(Cd`(B . "0x83F4") ; <CJK>
       (?$(Cda(B . "0x95C7") ; <CJK>
       (?$(Cdb(B . "0x58D3") ; <CJK>
       (?$(Cdc(B . "0x62BC") ; <CJK>
       (?$(Cdd(B . "0x72CE") ; <CJK>
       (?$(Cde(B . "0x9D28") ; <CJK>
       (?$(Cdf(B . "0x4EF0") ; <CJK>
       (?$(Cdg(B . "0x592E") ; <CJK>
       (?$(Cdh(B . "0x600F") ; <CJK>
       (?$(Cdi(B . "0x663B") ; <CJK>
       (?$(Cdj(B . "0x6B83") ; <CJK>
       (?$(Cdk(B . "0x79E7") ; <CJK>
       (?$(Cdl(B . "0x9D26") ; <CJK>
       (?$(Cdm(B . "0x5393") ; <CJK>
       (?$(Cdn(B . "0x54C0") ; <CJK>
       (?$(Cdo(B . "0x57C3") ; <CJK>
       (?$(Cdp(B . "0x5D16") ; <CJK>
       (?$(Cdq(B . "0x611B") ; <CJK>
       (?$(Cdr(B . "0x66D6") ; <CJK>
       (?$(Cds(B . "0x6DAF") ; <CJK>
       (?$(Cdt(B . "0x788D") ; <CJK>
       (?$(Cdu(B . "0x827E") ; <CJK>
       (?$(Cdv(B . "0x9698") ; <CJK>
       (?$(Cdw(B . "0x9744") ; <CJK>
       (?$(Cdx(B . "0x5384") ; <CJK>
       (?$(Cdy(B . "0x627C") ; <CJK>
       (?$(Cdz(B . "0x6396") ; <CJK>
       (?$(Cd{(B . "0x6DB2") ; <CJK>
       (?$(Cd|(B . "0x7E0A") ; <CJK>
       (?$(Cd}(B . "0x814B") ; <CJK>
       (?$(Cd~(B . "0x984D") ; <CJK>
       (?$(Ce!(B . "0x6AFB") ; <CJK>
       (?$(Ce"(B . "0x7F4C") ; <CJK>
       (?$(Ce#(B . "0x9DAF") ; <CJK>
       (?$(Ce$(B . "0x9E1A") ; <CJK>
       (?$(Ce%(B . "0x4E5F") ; <CJK>
       (?$(Ce&(B . "0x503B") ; <CJK>
       (?$(Ce'(B . "0x51B6") ; <CJK>
       (?$(Ce((B . "0x591C") ; <CJK>
       (?$(Ce)(B . "0x60F9") ; <CJK>
       (?$(Ce*(B . "0x63F6") ; <CJK>
       (?$(Ce+(B . "0x6930") ; <CJK>
       (?$(Ce,(B . "0x723A") ; <CJK>
       (?$(Ce-(B . "0x8036") ; <CJK>
       (?$(Ce.(B . "0xF974") ; <CJK>
       (?$(Ce/(B . "0x91CE") ; <CJK>
       (?$(Ce0(B . "0x5F31") ; <CJK>
       (?$(Ce1(B . "0xF975") ; <CJK>
       (?$(Ce2(B . "0xF976") ; <CJK>
       (?$(Ce3(B . "0x7D04") ; <CJK>
       (?$(Ce4(B . "0x82E5") ; <CJK>
       (?$(Ce5(B . "0x846F") ; <CJK>
       (?$(Ce6(B . "0x84BB") ; <CJK>
       (?$(Ce7(B . "0x85E5") ; <CJK>
       (?$(Ce8(B . "0x8E8D") ; <CJK>
       (?$(Ce9(B . "0xF977") ; <CJK>
       (?$(Ce:(B . "0x4F6F") ; <CJK>
       (?$(Ce;(B . "0xF978") ; <CJK>
       (?$(Ce<(B . "0xF979") ; <CJK>
       (?$(Ce=(B . "0x58E4") ; <CJK>
       (?$(Ce>(B . "0x5B43") ; <CJK>
       (?$(Ce?(B . "0x6059") ; <CJK>
       (?$(Ce@(B . "0x63DA") ; <CJK>
       (?$(CeA(B . "0x6518") ; <CJK>
       (?$(CeB(B . "0x656D") ; <CJK>
       (?$(CeC(B . "0x6698") ; <CJK>
       (?$(CeD(B . "0xF97A") ; <CJK>
       (?$(CeE(B . "0x694A") ; <CJK>
       (?$(CeF(B . "0x6A23") ; <CJK>
       (?$(CeG(B . "0x6D0B") ; <CJK>
       (?$(CeH(B . "0x7001") ; <CJK>
       (?$(CeI(B . "0x716C") ; <CJK>
       (?$(CeJ(B . "0x75D2") ; <CJK>
       (?$(CeK(B . "0x760D") ; <CJK>
       (?$(CeL(B . "0x79B3") ; <CJK>
       (?$(CeM(B . "0x7A70") ; <CJK>
       (?$(CeN(B . "0xF97B") ; <CJK>
       (?$(CeO(B . "0x7F8A") ; <CJK>
       (?$(CeP(B . "0xF97C") ; <CJK>
       (?$(CeQ(B . "0x8944") ; <CJK>
       (?$(CeR(B . "0xF97D") ; <CJK>
       (?$(CeS(B . "0x8B93") ; <CJK>
       (?$(CeT(B . "0x91C0") ; <CJK>
       (?$(CeU(B . "0x967D") ; <CJK>
       (?$(CeV(B . "0xF97E") ; <CJK>
       (?$(CeW(B . "0x990A") ; <CJK>
       (?$(CeX(B . "0x5704") ; <CJK>
       (?$(CeY(B . "0x5FA1") ; <CJK>
       (?$(CeZ(B . "0x65BC") ; <CJK>
       (?$(Ce[(B . "0x6F01") ; <CJK>
       (?$(Ce\(B . "0x7600") ; <CJK>
       (?$(Ce](B . "0x79A6") ; <CJK>
       (?$(Ce^(B . "0x8A9E") ; <CJK>
       (?$(Ce_(B . "0x99AD") ; <CJK>
       (?$(Ce`(B . "0x9B5A") ; <CJK>
       (?$(Cea(B . "0x9F6C") ; <CJK>
       (?$(Ceb(B . "0x5104") ; <CJK>
       (?$(Cec(B . "0x61B6") ; <CJK>
       (?$(Ced(B . "0x6291") ; <CJK>
       (?$(Cee(B . "0x6A8D") ; <CJK>
       (?$(Cef(B . "0x81C6") ; <CJK>
       (?$(Ceg(B . "0x5043") ; <CJK>
       (?$(Ceh(B . "0x5830") ; <CJK>
       (?$(Cei(B . "0x5F66") ; <CJK>
       (?$(Cej(B . "0x7109") ; <CJK>
       (?$(Cek(B . "0x8A00") ; <CJK>
       (?$(Cel(B . "0x8AFA") ; <CJK>
       (?$(Cem(B . "0x5B7C") ; <CJK>
       (?$(Cen(B . "0x8616") ; <CJK>
       (?$(Ceo(B . "0x4FFA") ; <CJK>
       (?$(Cep(B . "0x513C") ; <CJK>
       (?$(Ceq(B . "0x56B4") ; <CJK>
       (?$(Cer(B . "0x5944") ; <CJK>
       (?$(Ces(B . "0x63A9") ; <CJK>
       (?$(Cet(B . "0x6DF9") ; <CJK>
       (?$(Ceu(B . "0x5DAA") ; <CJK>
       (?$(Cev(B . "0x696D") ; <CJK>
       (?$(Cew(B . "0x5186") ; <CJK>
       (?$(Cex(B . "0x4E88") ; <CJK>
       (?$(Cey(B . "0x4F59") ; <CJK>
       (?$(Cez(B . "0xF97F") ; <CJK>
       (?$(Ce{(B . "0xF980") ; <CJK>
       (?$(Ce|(B . "0xF981") ; <CJK>
       (?$(Ce}(B . "0x5982") ; <CJK>
       (?$(Ce~(B . "0xF982") ; <CJK>
       (?$(Cf!(B . "0xF983") ; <CJK>
       (?$(Cf"(B . "0x6B5F") ; <CJK>
       (?$(Cf#(B . "0x6C5D") ; <CJK>
       (?$(Cf$(B . "0xF984") ; <CJK>
       (?$(Cf%(B . "0x74B5") ; <CJK>
       (?$(Cf&(B . "0x7916") ; <CJK>
       (?$(Cf'(B . "0xF985") ; <CJK>
       (?$(Cf((B . "0x8207") ; <CJK>
       (?$(Cf)(B . "0x8245") ; <CJK>
       (?$(Cf*(B . "0x8339") ; <CJK>
       (?$(Cf+(B . "0x8F3F") ; <CJK>
       (?$(Cf,(B . "0x8F5D") ; <CJK>
       (?$(Cf-(B . "0xF986") ; <CJK>
       (?$(Cf.(B . "0x9918") ; <CJK>
       (?$(Cf/(B . "0xF987") ; <CJK>
       (?$(Cf0(B . "0xF988") ; <CJK>
       (?$(Cf1(B . "0xF989") ; <CJK>
       (?$(Cf2(B . "0x4EA6") ; <CJK>
       (?$(Cf3(B . "0xF98A") ; <CJK>
       (?$(Cf4(B . "0x57DF") ; <CJK>
       (?$(Cf5(B . "0x5F79") ; <CJK>
       (?$(Cf6(B . "0x6613") ; <CJK>
       (?$(Cf7(B . "0xF98B") ; <CJK>
       (?$(Cf8(B . "0xF98C") ; <CJK>
       (?$(Cf9(B . "0x75AB") ; <CJK>
       (?$(Cf:(B . "0x7E79") ; <CJK>
       (?$(Cf;(B . "0x8B6F") ; <CJK>
       (?$(Cf<(B . "0xF98D") ; <CJK>
       (?$(Cf=(B . "0x9006") ; <CJK>
       (?$(Cf>(B . "0x9A5B") ; <CJK>
       (?$(Cf?(B . "0x56A5") ; <CJK>
       (?$(Cf@(B . "0x5827") ; <CJK>
       (?$(CfA(B . "0x59F8") ; <CJK>
       (?$(CfB(B . "0x5A1F") ; <CJK>
       (?$(CfC(B . "0x5BB4") ; <CJK>
       (?$(CfD(B . "0xF98E") ; <CJK>
       (?$(CfE(B . "0x5EF6") ; <CJK>
       (?$(CfF(B . "0xF98F") ; <CJK>
       (?$(CfG(B . "0xF990") ; <CJK>
       (?$(CfH(B . "0x6350") ; <CJK>
       (?$(CfI(B . "0x633B") ; <CJK>
       (?$(CfJ(B . "0xF991") ; <CJK>
       (?$(CfK(B . "0x693D") ; <CJK>
       (?$(CfL(B . "0x6C87") ; <CJK>
       (?$(CfM(B . "0x6CBF") ; <CJK>
       (?$(CfN(B . "0x6D8E") ; <CJK>
       (?$(CfO(B . "0x6D93") ; <CJK>
       (?$(CfP(B . "0x6DF5") ; <CJK>
       (?$(CfQ(B . "0x6F14") ; <CJK>
       (?$(CfR(B . "0xF992") ; <CJK>
       (?$(CfS(B . "0x70DF") ; <CJK>
       (?$(CfT(B . "0x7136") ; <CJK>
       (?$(CfU(B . "0x7159") ; <CJK>
       (?$(CfV(B . "0xF993") ; <CJK>
       (?$(CfW(B . "0x71C3") ; <CJK>
       (?$(CfX(B . "0x71D5") ; <CJK>
       (?$(CfY(B . "0xF994") ; <CJK>
       (?$(CfZ(B . "0x784F") ; <CJK>
       (?$(Cf[(B . "0x786F") ; <CJK>
       (?$(Cf\(B . "0xF995") ; <CJK>
       (?$(Cf](B . "0x7B75") ; <CJK>
       (?$(Cf^(B . "0x7DE3") ; <CJK>
       (?$(Cf_(B . "0xF996") ; <CJK>
       (?$(Cf`(B . "0x7E2F") ; <CJK>
       (?$(Cfa(B . "0xF997") ; <CJK>
       (?$(Cfb(B . "0x884D") ; <CJK>
       (?$(Cfc(B . "0x8EDF") ; <CJK>
       (?$(Cfd(B . "0xF998") ; <CJK>
       (?$(Cfe(B . "0xF999") ; <CJK>
       (?$(Cff(B . "0xF99A") ; <CJK>
       (?$(Cfg(B . "0x925B") ; <CJK>
       (?$(Cfh(B . "0xF99B") ; <CJK>
       (?$(Cfi(B . "0x9CF6") ; <CJK>
       (?$(Cfj(B . "0xF99C") ; <CJK>
       (?$(Cfk(B . "0xF99D") ; <CJK>
       (?$(Cfl(B . "0xF99E") ; <CJK>
       (?$(Cfm(B . "0x6085") ; <CJK>
       (?$(Cfn(B . "0x6D85") ; <CJK>
       (?$(Cfo(B . "0xF99F") ; <CJK>
       (?$(Cfp(B . "0x71B1") ; <CJK>
       (?$(Cfq(B . "0xF9A0") ; <CJK>
       (?$(Cfr(B . "0xF9A1") ; <CJK>
       (?$(Cfs(B . "0x95B1") ; <CJK>
       (?$(Cft(B . "0x53AD") ; <CJK>
       (?$(Cfu(B . "0xF9A2") ; <CJK>
       (?$(Cfv(B . "0xF9A3") ; <CJK>
       (?$(Cfw(B . "0xF9A4") ; <CJK>
       (?$(Cfx(B . "0x67D3") ; <CJK>
       (?$(Cfy(B . "0xF9A5") ; <CJK>
       (?$(Cfz(B . "0x708E") ; <CJK>
       (?$(Cf{(B . "0x7130") ; <CJK>
       (?$(Cf|(B . "0x7430") ; <CJK>
       (?$(Cf}(B . "0x8276") ; <CJK>
       (?$(Cf~(B . "0x82D2") ; <CJK>
       (?$(Cg!(B . "0xF9A6") ; <CJK>
       (?$(Cg"(B . "0x95BB") ; <CJK>
       (?$(Cg#(B . "0x9AE5") ; <CJK>
       (?$(Cg$(B . "0x9E7D") ; <CJK>
       (?$(Cg%(B . "0x66C4") ; <CJK>
       (?$(Cg&(B . "0xF9A7") ; <CJK>
       (?$(Cg'(B . "0x71C1") ; <CJK>
       (?$(Cg((B . "0x8449") ; <CJK>
       (?$(Cg)(B . "0xF9A8") ; <CJK>
       (?$(Cg*(B . "0xF9A9") ; <CJK>
       (?$(Cg+(B . "0x584B") ; <CJK>
       (?$(Cg,(B . "0xF9AA") ; <CJK>
       (?$(Cg-(B . "0xF9AB") ; <CJK>
       (?$(Cg.(B . "0x5DB8") ; <CJK>
       (?$(Cg/(B . "0x5F71") ; <CJK>
       (?$(Cg0(B . "0xF9AC") ; <CJK>
       (?$(Cg1(B . "0x6620") ; <CJK>
       (?$(Cg2(B . "0x668E") ; <CJK>
       (?$(Cg3(B . "0x6979") ; <CJK>
       (?$(Cg4(B . "0x69AE") ; <CJK>
       (?$(Cg5(B . "0x6C38") ; <CJK>
       (?$(Cg6(B . "0x6CF3") ; <CJK>
       (?$(Cg7(B . "0x6E36") ; <CJK>
       (?$(Cg8(B . "0x6F41") ; <CJK>
       (?$(Cg9(B . "0x6FDA") ; <CJK>
       (?$(Cg:(B . "0x701B") ; <CJK>
       (?$(Cg;(B . "0x702F") ; <CJK>
       (?$(Cg<(B . "0x7150") ; <CJK>
       (?$(Cg=(B . "0x71DF") ; <CJK>
       (?$(Cg>(B . "0x7370") ; <CJK>
       (?$(Cg?(B . "0xF9AD") ; <CJK>
       (?$(Cg@(B . "0x745B") ; <CJK>
       (?$(CgA(B . "0xF9AE") ; <CJK>
       (?$(CgB(B . "0x74D4") ; <CJK>
       (?$(CgC(B . "0x76C8") ; <CJK>
       (?$(CgD(B . "0x7A4E") ; <CJK>
       (?$(CgE(B . "0x7E93") ; <CJK>
       (?$(CgF(B . "0xF9AF") ; <CJK>
       (?$(CgG(B . "0xF9B0") ; <CJK>
       (?$(CgH(B . "0x82F1") ; <CJK>
       (?$(CgI(B . "0x8A60") ; <CJK>
       (?$(CgJ(B . "0x8FCE") ; <CJK>
       (?$(CgK(B . "0xF9B1") ; <CJK>
       (?$(CgL(B . "0x9348") ; <CJK>
       (?$(CgM(B . "0xF9B2") ; <CJK>
       (?$(CgN(B . "0x9719") ; <CJK>
       (?$(CgO(B . "0xF9B3") ; <CJK>
       (?$(CgP(B . "0xF9B4") ; <CJK>
       (?$(CgQ(B . "0x4E42") ; <CJK>
       (?$(CgR(B . "0x502A") ; <CJK>
       (?$(CgS(B . "0xF9B5") ; <CJK>
       (?$(CgT(B . "0x5208") ; <CJK>
       (?$(CgU(B . "0x53E1") ; <CJK>
       (?$(CgV(B . "0x66F3") ; <CJK>
       (?$(CgW(B . "0x6C6D") ; <CJK>
       (?$(CgX(B . "0x6FCA") ; <CJK>
       (?$(CgY(B . "0x730A") ; <CJK>
       (?$(CgZ(B . "0x777F") ; <CJK>
       (?$(Cg[(B . "0x7A62") ; <CJK>
       (?$(Cg\(B . "0x82AE") ; <CJK>
       (?$(Cg](B . "0x85DD") ; <CJK>
       (?$(Cg^(B . "0x8602") ; <CJK>
       (?$(Cg_(B . "0xF9B6") ; <CJK>
       (?$(Cg`(B . "0x88D4") ; <CJK>
       (?$(Cga(B . "0x8A63") ; <CJK>
       (?$(Cgb(B . "0x8B7D") ; <CJK>
       (?$(Cgc(B . "0x8C6B") ; <CJK>
       (?$(Cgd(B . "0xF9B7") ; <CJK>
       (?$(Cge(B . "0x92B3") ; <CJK>
       (?$(Cgf(B . "0xF9B8") ; <CJK>
       (?$(Cgg(B . "0x9713") ; <CJK>
       (?$(Cgh(B . "0x9810") ; <CJK>
       (?$(Cgi(B . "0x4E94") ; <CJK>
       (?$(Cgj(B . "0x4F0D") ; <CJK>
       (?$(Cgk(B . "0x4FC9") ; <CJK>
       (?$(Cgl(B . "0x50B2") ; <CJK>
       (?$(Cgm(B . "0x5348") ; <CJK>
       (?$(Cgn(B . "0x543E") ; <CJK>
       (?$(Cgo(B . "0x5433") ; <CJK>
       (?$(Cgp(B . "0x55DA") ; <CJK>
       (?$(Cgq(B . "0x5862") ; <CJK>
       (?$(Cgr(B . "0x58BA") ; <CJK>
       (?$(Cgs(B . "0x5967") ; <CJK>
       (?$(Cgt(B . "0x5A1B") ; <CJK>
       (?$(Cgu(B . "0x5BE4") ; <CJK>
       (?$(Cgv(B . "0x609F") ; <CJK>
       (?$(Cgw(B . "0xF9B9") ; <CJK>
       (?$(Cgx(B . "0x61CA") ; <CJK>
       (?$(Cgy(B . "0x6556") ; <CJK>
       (?$(Cgz(B . "0x65FF") ; <CJK>
       (?$(Cg{(B . "0x6664") ; <CJK>
       (?$(Cg|(B . "0x68A7") ; <CJK>
       (?$(Cg}(B . "0x6C5A") ; <CJK>
       (?$(Cg~(B . "0x6FB3") ; <CJK>
       (?$(Ch!(B . "0x70CF") ; <CJK>
       (?$(Ch"(B . "0x71AC") ; <CJK>
       (?$(Ch#(B . "0x7352") ; <CJK>
       (?$(Ch$(B . "0x7B7D") ; <CJK>
       (?$(Ch%(B . "0x8708") ; <CJK>
       (?$(Ch&(B . "0x8AA4") ; <CJK>
       (?$(Ch'(B . "0x9C32") ; <CJK>
       (?$(Ch((B . "0x9F07") ; <CJK>
       (?$(Ch)(B . "0x5C4B") ; <CJK>
       (?$(Ch*(B . "0x6C83") ; <CJK>
       (?$(Ch+(B . "0x7344") ; <CJK>
       (?$(Ch,(B . "0x7389") ; <CJK>
       (?$(Ch-(B . "0x923A") ; <CJK>
       (?$(Ch.(B . "0x6EAB") ; <CJK>
       (?$(Ch/(B . "0x7465") ; <CJK>
       (?$(Ch0(B . "0x761F") ; <CJK>
       (?$(Ch1(B . "0x7A69") ; <CJK>
       (?$(Ch2(B . "0x7E15") ; <CJK>
       (?$(Ch3(B . "0x860A") ; <CJK>
       (?$(Ch4(B . "0x5140") ; <CJK>
       (?$(Ch5(B . "0x58C5") ; <CJK>
       (?$(Ch6(B . "0x64C1") ; <CJK>
       (?$(Ch7(B . "0x74EE") ; <CJK>
       (?$(Ch8(B . "0x7515") ; <CJK>
       (?$(Ch9(B . "0x7670") ; <CJK>
       (?$(Ch:(B . "0x7FC1") ; <CJK>
       (?$(Ch;(B . "0x9095") ; <CJK>
       (?$(Ch<(B . "0x96CD") ; <CJK>
       (?$(Ch=(B . "0x9954") ; <CJK>
       (?$(Ch>(B . "0x6E26") ; <CJK>
       (?$(Ch?(B . "0x74E6") ; <CJK>
       (?$(Ch@(B . "0x7AA9") ; <CJK>
       (?$(ChA(B . "0x7AAA") ; <CJK>
       (?$(ChB(B . "0x81E5") ; <CJK>
       (?$(ChC(B . "0x86D9") ; <CJK>
       (?$(ChD(B . "0x8778") ; <CJK>
       (?$(ChE(B . "0x8A1B") ; <CJK>
       (?$(ChF(B . "0x5A49") ; <CJK>
       (?$(ChG(B . "0x5B8C") ; <CJK>
       (?$(ChH(B . "0x5B9B") ; <CJK>
       (?$(ChI(B . "0x68A1") ; <CJK>
       (?$(ChJ(B . "0x6900") ; <CJK>
       (?$(ChK(B . "0x6D63") ; <CJK>
       (?$(ChL(B . "0x73A9") ; <CJK>
       (?$(ChM(B . "0x7413") ; <CJK>
       (?$(ChN(B . "0x742C") ; <CJK>
       (?$(ChO(B . "0x7897") ; <CJK>
       (?$(ChP(B . "0x7DE9") ; <CJK>
       (?$(ChQ(B . "0x7FEB") ; <CJK>
       (?$(ChR(B . "0x8118") ; <CJK>
       (?$(ChS(B . "0x8155") ; <CJK>
       (?$(ChT(B . "0x839E") ; <CJK>
       (?$(ChU(B . "0x8C4C") ; <CJK>
       (?$(ChV(B . "0x962E") ; <CJK>
       (?$(ChW(B . "0x9811") ; <CJK>
       (?$(ChX(B . "0x66F0") ; <CJK>
       (?$(ChY(B . "0x5F80") ; <CJK>
       (?$(ChZ(B . "0x65FA") ; <CJK>
       (?$(Ch[(B . "0x6789") ; <CJK>
       (?$(Ch\(B . "0x6C6A") ; <CJK>
       (?$(Ch](B . "0x738B") ; <CJK>
       (?$(Ch^(B . "0x502D") ; <CJK>
       (?$(Ch_(B . "0x5A03") ; <CJK>
       (?$(Ch`(B . "0x6B6A") ; <CJK>
       (?$(Cha(B . "0x77EE") ; <CJK>
       (?$(Chb(B . "0x5916") ; <CJK>
       (?$(Chc(B . "0x5D6C") ; <CJK>
       (?$(Chd(B . "0x5DCD") ; <CJK>
       (?$(Che(B . "0x7325") ; <CJK>
       (?$(Chf(B . "0x754F") ; <CJK>
       (?$(Chg(B . "0xF9BA") ; <CJK>
       (?$(Chh(B . "0xF9BB") ; <CJK>
       (?$(Chi(B . "0x50E5") ; <CJK>
       (?$(Chj(B . "0x51F9") ; <CJK>
       (?$(Chk(B . "0x582F") ; <CJK>
       (?$(Chl(B . "0x592D") ; <CJK>
       (?$(Chm(B . "0x5996") ; <CJK>
       (?$(Chn(B . "0x59DA") ; <CJK>
       (?$(Cho(B . "0x5BE5") ; <CJK>
       (?$(Chp(B . "0xF9BC") ; <CJK>
       (?$(Chq(B . "0xF9BD") ; <CJK>
       (?$(Chr(B . "0x5DA2") ; <CJK>
       (?$(Chs(B . "0x62D7") ; <CJK>
       (?$(Cht(B . "0x6416") ; <CJK>
       (?$(Chu(B . "0x6493") ; <CJK>
       (?$(Chv(B . "0x64FE") ; <CJK>
       (?$(Chw(B . "0xF9BE") ; <CJK>
       (?$(Chx(B . "0x66DC") ; <CJK>
       (?$(Chy(B . "0xF9BF") ; <CJK>
       (?$(Chz(B . "0x6A48") ; <CJK>
       (?$(Ch{(B . "0xF9C0") ; <CJK>
       (?$(Ch|(B . "0x71FF") ; <CJK>
       (?$(Ch}(B . "0x7464") ; <CJK>
       (?$(Ch~(B . "0xF9C1") ; <CJK>
       (?$(Ci!(B . "0x7A88") ; <CJK>
       (?$(Ci"(B . "0x7AAF") ; <CJK>
       (?$(Ci#(B . "0x7E47") ; <CJK>
       (?$(Ci$(B . "0x7E5E") ; <CJK>
       (?$(Ci%(B . "0x8000") ; <CJK>
       (?$(Ci&(B . "0x8170") ; <CJK>
       (?$(Ci'(B . "0xF9C2") ; <CJK>
       (?$(Ci((B . "0x87EF") ; <CJK>
       (?$(Ci)(B . "0x8981") ; <CJK>
       (?$(Ci*(B . "0x8B20") ; <CJK>
       (?$(Ci+(B . "0x9059") ; <CJK>
       (?$(Ci,(B . "0xF9C3") ; <CJK>
       (?$(Ci-(B . "0x9080") ; <CJK>
       (?$(Ci.(B . "0x9952") ; <CJK>
       (?$(Ci/(B . "0x617E") ; <CJK>
       (?$(Ci0(B . "0x6B32") ; <CJK>
       (?$(Ci1(B . "0x6D74") ; <CJK>
       (?$(Ci2(B . "0x7E1F") ; <CJK>
       (?$(Ci3(B . "0x8925") ; <CJK>
       (?$(Ci4(B . "0x8FB1") ; <CJK>
       (?$(Ci5(B . "0x4FD1") ; <CJK>
       (?$(Ci6(B . "0x50AD") ; <CJK>
       (?$(Ci7(B . "0x5197") ; <CJK>
       (?$(Ci8(B . "0x52C7") ; <CJK>
       (?$(Ci9(B . "0x57C7") ; <CJK>
       (?$(Ci:(B . "0x5889") ; <CJK>
       (?$(Ci;(B . "0x5BB9") ; <CJK>
       (?$(Ci<(B . "0x5EB8") ; <CJK>
       (?$(Ci=(B . "0x6142") ; <CJK>
       (?$(Ci>(B . "0x6995") ; <CJK>
       (?$(Ci?(B . "0x6D8C") ; <CJK>
       (?$(Ci@(B . "0x6E67") ; <CJK>
       (?$(CiA(B . "0x6EB6") ; <CJK>
       (?$(CiB(B . "0x7194") ; <CJK>
       (?$(CiC(B . "0x7462") ; <CJK>
       (?$(CiD(B . "0x7528") ; <CJK>
       (?$(CiE(B . "0x752C") ; <CJK>
       (?$(CiF(B . "0x8073") ; <CJK>
       (?$(CiG(B . "0x8338") ; <CJK>
       (?$(CiH(B . "0x84C9") ; <CJK>
       (?$(CiI(B . "0x8E0A") ; <CJK>
       (?$(CiJ(B . "0x9394") ; <CJK>
       (?$(CiK(B . "0x93DE") ; <CJK>
       (?$(CiL(B . "0xF9C4") ; <CJK>
       (?$(CiM(B . "0x4E8E") ; <CJK>
       (?$(CiN(B . "0x4F51") ; <CJK>
       (?$(CiO(B . "0x5076") ; <CJK>
       (?$(CiP(B . "0x512A") ; <CJK>
       (?$(CiQ(B . "0x53C8") ; <CJK>
       (?$(CiR(B . "0x53CB") ; <CJK>
       (?$(CiS(B . "0x53F3") ; <CJK>
       (?$(CiT(B . "0x5B87") ; <CJK>
       (?$(CiU(B . "0x5BD3") ; <CJK>
       (?$(CiV(B . "0x5C24") ; <CJK>
       (?$(CiW(B . "0x611A") ; <CJK>
       (?$(CiX(B . "0x6182") ; <CJK>
       (?$(CiY(B . "0x65F4") ; <CJK>
       (?$(CiZ(B . "0x725B") ; <CJK>
       (?$(Ci[(B . "0x7397") ; <CJK>
       (?$(Ci\(B . "0x7440") ; <CJK>
       (?$(Ci](B . "0x76C2") ; <CJK>
       (?$(Ci^(B . "0x7950") ; <CJK>
       (?$(Ci_(B . "0x7991") ; <CJK>
       (?$(Ci`(B . "0x79B9") ; <CJK>
       (?$(Cia(B . "0x7D06") ; <CJK>
       (?$(Cib(B . "0x7FBD") ; <CJK>
       (?$(Cic(B . "0x828B") ; <CJK>
       (?$(Cid(B . "0x85D5") ; <CJK>
       (?$(Cie(B . "0x865E") ; <CJK>
       (?$(Cif(B . "0x8FC2") ; <CJK>
       (?$(Cig(B . "0x9047") ; <CJK>
       (?$(Cih(B . "0x90F5") ; <CJK>
       (?$(Cii(B . "0x91EA") ; <CJK>
       (?$(Cij(B . "0x9685") ; <CJK>
       (?$(Cik(B . "0x96E8") ; <CJK>
       (?$(Cil(B . "0x96E9") ; <CJK>
       (?$(Cim(B . "0x52D6") ; <CJK>
       (?$(Cin(B . "0x5F67") ; <CJK>
       (?$(Cio(B . "0x65ED") ; <CJK>
       (?$(Cip(B . "0x6631") ; <CJK>
       (?$(Ciq(B . "0x682F") ; <CJK>
       (?$(Cir(B . "0x715C") ; <CJK>
       (?$(Cis(B . "0x7A36") ; <CJK>
       (?$(Cit(B . "0x90C1") ; <CJK>
       (?$(Ciu(B . "0x980A") ; <CJK>
       (?$(Civ(B . "0x4E91") ; <CJK>
       (?$(Ciw(B . "0xF9C5") ; <CJK>
       (?$(Cix(B . "0x6A52") ; <CJK>
       (?$(Ciy(B . "0x6B9E") ; <CJK>
       (?$(Ciz(B . "0x6F90") ; <CJK>
       (?$(Ci{(B . "0x7189") ; <CJK>
       (?$(Ci|(B . "0x8018") ; <CJK>
       (?$(Ci}(B . "0x82B8") ; <CJK>
       (?$(Ci~(B . "0x8553") ; <CJK>
       (?$(Cj!(B . "0x904B") ; <CJK>
       (?$(Cj"(B . "0x9695") ; <CJK>
       (?$(Cj#(B . "0x96F2") ; <CJK>
       (?$(Cj$(B . "0x97FB") ; <CJK>
       (?$(Cj%(B . "0x851A") ; <CJK>
       (?$(Cj&(B . "0x9B31") ; <CJK>
       (?$(Cj'(B . "0x4E90") ; <CJK>
       (?$(Cj((B . "0x718A") ; <CJK>
       (?$(Cj)(B . "0x96C4") ; <CJK>
       (?$(Cj*(B . "0x5143") ; <CJK>
       (?$(Cj+(B . "0x539F") ; <CJK>
       (?$(Cj,(B . "0x54E1") ; <CJK>
       (?$(Cj-(B . "0x5713") ; <CJK>
       (?$(Cj.(B . "0x5712") ; <CJK>
       (?$(Cj/(B . "0x57A3") ; <CJK>
       (?$(Cj0(B . "0x5A9B") ; <CJK>
       (?$(Cj1(B . "0x5AC4") ; <CJK>
       (?$(Cj2(B . "0x5BC3") ; <CJK>
       (?$(Cj3(B . "0x6028") ; <CJK>
       (?$(Cj4(B . "0x613F") ; <CJK>
       (?$(Cj5(B . "0x63F4") ; <CJK>
       (?$(Cj6(B . "0x6C85") ; <CJK>
       (?$(Cj7(B . "0x6D39") ; <CJK>
       (?$(Cj8(B . "0x6E72") ; <CJK>
       (?$(Cj9(B . "0x6E90") ; <CJK>
       (?$(Cj:(B . "0x7230") ; <CJK>
       (?$(Cj;(B . "0x733F") ; <CJK>
       (?$(Cj<(B . "0x7457") ; <CJK>
       (?$(Cj=(B . "0x82D1") ; <CJK>
       (?$(Cj>(B . "0x8881") ; <CJK>
       (?$(Cj?(B . "0x8F45") ; <CJK>
       (?$(Cj@(B . "0x9060") ; <CJK>
       (?$(CjA(B . "0xF9C6") ; <CJK>
       (?$(CjB(B . "0x9662") ; <CJK>
       (?$(CjC(B . "0x9858") ; <CJK>
       (?$(CjD(B . "0x9D1B") ; <CJK>
       (?$(CjE(B . "0x6708") ; <CJK>
       (?$(CjF(B . "0x8D8A") ; <CJK>
       (?$(CjG(B . "0x925E") ; <CJK>
       (?$(CjH(B . "0x4F4D") ; <CJK>
       (?$(CjI(B . "0x5049") ; <CJK>
       (?$(CjJ(B . "0x50DE") ; <CJK>
       (?$(CjK(B . "0x5371") ; <CJK>
       (?$(CjL(B . "0x570D") ; <CJK>
       (?$(CjM(B . "0x59D4") ; <CJK>
       (?$(CjN(B . "0x5A01") ; <CJK>
       (?$(CjO(B . "0x5C09") ; <CJK>
       (?$(CjP(B . "0x6170") ; <CJK>
       (?$(CjQ(B . "0x6690") ; <CJK>
       (?$(CjR(B . "0x6E2D") ; <CJK>
       (?$(CjS(B . "0x7232") ; <CJK>
       (?$(CjT(B . "0x744B") ; <CJK>
       (?$(CjU(B . "0x7DEF") ; <CJK>
       (?$(CjV(B . "0x80C3") ; <CJK>
       (?$(CjW(B . "0x840E") ; <CJK>
       (?$(CjX(B . "0x8466") ; <CJK>
       (?$(CjY(B . "0x853F") ; <CJK>
       (?$(CjZ(B . "0x875F") ; <CJK>
       (?$(Cj[(B . "0x885B") ; <CJK>
       (?$(Cj\(B . "0x8918") ; <CJK>
       (?$(Cj](B . "0x8B02") ; <CJK>
       (?$(Cj^(B . "0x9055") ; <CJK>
       (?$(Cj_(B . "0x97CB") ; <CJK>
       (?$(Cj`(B . "0x9B4F") ; <CJK>
       (?$(Cja(B . "0x4E73") ; <CJK>
       (?$(Cjb(B . "0x4F91") ; <CJK>
       (?$(Cjc(B . "0x5112") ; <CJK>
       (?$(Cjd(B . "0x516A") ; <CJK>
       (?$(Cje(B . "0xF9C7") ; <CJK>
       (?$(Cjf(B . "0x552F") ; <CJK>
       (?$(Cjg(B . "0x55A9") ; <CJK>
       (?$(Cjh(B . "0x5B7A") ; <CJK>
       (?$(Cji(B . "0x5BA5") ; <CJK>
       (?$(Cjj(B . "0x5E7C") ; <CJK>
       (?$(Cjk(B . "0x5E7D") ; <CJK>
       (?$(Cjl(B . "0x5EBE") ; <CJK>
       (?$(Cjm(B . "0x60A0") ; <CJK>
       (?$(Cjn(B . "0x60DF") ; <CJK>
       (?$(Cjo(B . "0x6108") ; <CJK>
       (?$(Cjp(B . "0x6109") ; <CJK>
       (?$(Cjq(B . "0x63C4") ; <CJK>
       (?$(Cjr(B . "0x6538") ; <CJK>
       (?$(Cjs(B . "0x6709") ; <CJK>
       (?$(Cjt(B . "0xF9C8") ; <CJK>
       (?$(Cju(B . "0x67D4") ; <CJK>
       (?$(Cjv(B . "0x67DA") ; <CJK>
       (?$(Cjw(B . "0xF9C9") ; <CJK>
       (?$(Cjx(B . "0x6961") ; <CJK>
       (?$(Cjy(B . "0x6962") ; <CJK>
       (?$(Cjz(B . "0x6CB9") ; <CJK>
       (?$(Cj{(B . "0x6D27") ; <CJK>
       (?$(Cj|(B . "0xF9CA") ; <CJK>
       (?$(Cj}(B . "0x6E38") ; <CJK>
       (?$(Cj~(B . "0xF9CB") ; <CJK>
       (?$(Ck!(B . "0x6FE1") ; <CJK>
       (?$(Ck"(B . "0x7336") ; <CJK>
       (?$(Ck#(B . "0x7337") ; <CJK>
       (?$(Ck$(B . "0xF9CC") ; <CJK>
       (?$(Ck%(B . "0x745C") ; <CJK>
       (?$(Ck&(B . "0x7531") ; <CJK>
       (?$(Ck'(B . "0xF9CD") ; <CJK>
       (?$(Ck((B . "0x7652") ; <CJK>
       (?$(Ck)(B . "0xF9CE") ; <CJK>
       (?$(Ck*(B . "0xF9CF") ; <CJK>
       (?$(Ck+(B . "0x7DAD") ; <CJK>
       (?$(Ck,(B . "0x81FE") ; <CJK>
       (?$(Ck-(B . "0x8438") ; <CJK>
       (?$(Ck.(B . "0x88D5") ; <CJK>
       (?$(Ck/(B . "0x8A98") ; <CJK>
       (?$(Ck0(B . "0x8ADB") ; <CJK>
       (?$(Ck1(B . "0x8AED") ; <CJK>
       (?$(Ck2(B . "0x8E30") ; <CJK>
       (?$(Ck3(B . "0x8E42") ; <CJK>
       (?$(Ck4(B . "0x904A") ; <CJK>
       (?$(Ck5(B . "0x903E") ; <CJK>
       (?$(Ck6(B . "0x907A") ; <CJK>
       (?$(Ck7(B . "0x9149") ; <CJK>
       (?$(Ck8(B . "0x91C9") ; <CJK>
       (?$(Ck9(B . "0x936E") ; <CJK>
       (?$(Ck:(B . "0xF9D0") ; <CJK>
       (?$(Ck;(B . "0xF9D1") ; <CJK>
       (?$(Ck<(B . "0x5809") ; <CJK>
       (?$(Ck=(B . "0xF9D2") ; <CJK>
       (?$(Ck>(B . "0x6BD3") ; <CJK>
       (?$(Ck?(B . "0x8089") ; <CJK>
       (?$(Ck@(B . "0x80B2") ; <CJK>
       (?$(CkA(B . "0xF9D3") ; <CJK>
       (?$(CkB(B . "0xF9D4") ; <CJK>
       (?$(CkC(B . "0x5141") ; <CJK>
       (?$(CkD(B . "0x596B") ; <CJK>
       (?$(CkE(B . "0x5C39") ; <CJK>
       (?$(CkF(B . "0xF9D5") ; <CJK>
       (?$(CkG(B . "0xF9D6") ; <CJK>
       (?$(CkH(B . "0x6F64") ; <CJK>
       (?$(CkI(B . "0x73A7") ; <CJK>
       (?$(CkJ(B . "0x80E4") ; <CJK>
       (?$(CkK(B . "0x8D07") ; <CJK>
       (?$(CkL(B . "0xF9D7") ; <CJK>
       (?$(CkM(B . "0x9217") ; <CJK>
       (?$(CkN(B . "0x958F") ; <CJK>
       (?$(CkO(B . "0xF9D8") ; <CJK>
       (?$(CkP(B . "0xF9D9") ; <CJK>
       (?$(CkQ(B . "0xF9DA") ; <CJK>
       (?$(CkR(B . "0xF9DB") ; <CJK>
       (?$(CkS(B . "0x807F") ; <CJK>
       (?$(CkT(B . "0x620E") ; <CJK>
       (?$(CkU(B . "0x701C") ; <CJK>
       (?$(CkV(B . "0x7D68") ; <CJK>
       (?$(CkW(B . "0x878D") ; <CJK>
       (?$(CkX(B . "0xF9DC") ; <CJK>
       (?$(CkY(B . "0x57A0") ; <CJK>
       (?$(CkZ(B . "0x6069") ; <CJK>
       (?$(Ck[(B . "0x6147") ; <CJK>
       (?$(Ck\(B . "0x6BB7") ; <CJK>
       (?$(Ck](B . "0x8ABE") ; <CJK>
       (?$(Ck^(B . "0x9280") ; <CJK>
       (?$(Ck_(B . "0x96B1") ; <CJK>
       (?$(Ck`(B . "0x4E59") ; <CJK>
       (?$(Cka(B . "0x541F") ; <CJK>
       (?$(Ckb(B . "0x6DEB") ; <CJK>
       (?$(Ckc(B . "0x852D") ; <CJK>
       (?$(Ckd(B . "0x9670") ; <CJK>
       (?$(Cke(B . "0x97F3") ; <CJK>
       (?$(Ckf(B . "0x98EE") ; <CJK>
       (?$(Ckg(B . "0x63D6") ; <CJK>
       (?$(Ckh(B . "0x6CE3") ; <CJK>
       (?$(Cki(B . "0x9091") ; <CJK>
       (?$(Ckj(B . "0x51DD") ; <CJK>
       (?$(Ckk(B . "0x61C9") ; <CJK>
       (?$(Ckl(B . "0x81BA") ; <CJK>
       (?$(Ckm(B . "0x9DF9") ; <CJK>
       (?$(Ckn(B . "0x4F9D") ; <CJK>
       (?$(Cko(B . "0x501A") ; <CJK>
       (?$(Ckp(B . "0x5100") ; <CJK>
       (?$(Ckq(B . "0x5B9C") ; <CJK>
       (?$(Ckr(B . "0x610F") ; <CJK>
       (?$(Cks(B . "0x61FF") ; <CJK>
       (?$(Ckt(B . "0x64EC") ; <CJK>
       (?$(Cku(B . "0x6905") ; <CJK>
       (?$(Ckv(B . "0x6BC5") ; <CJK>
       (?$(Ckw(B . "0x7591") ; <CJK>
       (?$(Ckx(B . "0x77E3") ; <CJK>
       (?$(Cky(B . "0x7FA9") ; <CJK>
       (?$(Ckz(B . "0x8264") ; <CJK>
       (?$(Ck{(B . "0x858F") ; <CJK>
       (?$(Ck|(B . "0x87FB") ; <CJK>
       (?$(Ck}(B . "0x8863") ; <CJK>
       (?$(Ck~(B . "0x8ABC") ; <CJK>
       (?$(Cl!(B . "0x8B70") ; <CJK>
       (?$(Cl"(B . "0x91AB") ; <CJK>
       (?$(Cl#(B . "0x4E8C") ; <CJK>
       (?$(Cl$(B . "0x4EE5") ; <CJK>
       (?$(Cl%(B . "0x4F0A") ; <CJK>
       (?$(Cl&(B . "0xF9DD") ; <CJK>
       (?$(Cl'(B . "0xF9DE") ; <CJK>
       (?$(Cl((B . "0x5937") ; <CJK>
       (?$(Cl)(B . "0x59E8") ; <CJK>
       (?$(Cl*(B . "0xF9DF") ; <CJK>
       (?$(Cl+(B . "0x5DF2") ; <CJK>
       (?$(Cl,(B . "0x5F1B") ; <CJK>
       (?$(Cl-(B . "0x5F5B") ; <CJK>
       (?$(Cl.(B . "0x6021") ; <CJK>
       (?$(Cl/(B . "0xF9E0") ; <CJK>
       (?$(Cl0(B . "0xF9E1") ; <CJK>
       (?$(Cl1(B . "0xF9E2") ; <CJK>
       (?$(Cl2(B . "0xF9E3") ; <CJK>
       (?$(Cl3(B . "0x723E") ; <CJK>
       (?$(Cl4(B . "0x73E5") ; <CJK>
       (?$(Cl5(B . "0xF9E4") ; <CJK>
       (?$(Cl6(B . "0x7570") ; <CJK>
       (?$(Cl7(B . "0x75CD") ; <CJK>
       (?$(Cl8(B . "0xF9E5") ; <CJK>
       (?$(Cl9(B . "0x79FB") ; <CJK>
       (?$(Cl:(B . "0xF9E6") ; <CJK>
       (?$(Cl;(B . "0x800C") ; <CJK>
       (?$(Cl<(B . "0x8033") ; <CJK>
       (?$(Cl=(B . "0x8084") ; <CJK>
       (?$(Cl>(B . "0x82E1") ; <CJK>
       (?$(Cl?(B . "0x8351") ; <CJK>
       (?$(Cl@(B . "0xF9E7") ; <CJK>
       (?$(ClA(B . "0xF9E8") ; <CJK>
       (?$(ClB(B . "0x8CBD") ; <CJK>
       (?$(ClC(B . "0x8CB3") ; <CJK>
       (?$(ClD(B . "0x9087") ; <CJK>
       (?$(ClE(B . "0xF9E9") ; <CJK>
       (?$(ClF(B . "0xF9EA") ; <CJK>
       (?$(ClG(B . "0x98F4") ; <CJK>
       (?$(ClH(B . "0x990C") ; <CJK>
       (?$(ClI(B . "0xF9EB") ; <CJK>
       (?$(ClJ(B . "0xF9EC") ; <CJK>
       (?$(ClK(B . "0x7037") ; <CJK>
       (?$(ClL(B . "0x76CA") ; <CJK>
       (?$(ClM(B . "0x7FCA") ; <CJK>
       (?$(ClN(B . "0x7FCC") ; <CJK>
       (?$(ClO(B . "0x7FFC") ; <CJK>
       (?$(ClP(B . "0x8B1A") ; <CJK>
       (?$(ClQ(B . "0x4EBA") ; <CJK>
       (?$(ClR(B . "0x4EC1") ; <CJK>
       (?$(ClS(B . "0x5203") ; <CJK>
       (?$(ClT(B . "0x5370") ; <CJK>
       (?$(ClU(B . "0xF9ED") ; <CJK>
       (?$(ClV(B . "0x54BD") ; <CJK>
       (?$(ClW(B . "0x56E0") ; <CJK>
       (?$(ClX(B . "0x59FB") ; <CJK>
       (?$(ClY(B . "0x5BC5") ; <CJK>
       (?$(ClZ(B . "0x5F15") ; <CJK>
       (?$(Cl[(B . "0x5FCD") ; <CJK>
       (?$(Cl\(B . "0x6E6E") ; <CJK>
       (?$(Cl](B . "0xF9EE") ; <CJK>
       (?$(Cl^(B . "0xF9EF") ; <CJK>
       (?$(Cl_(B . "0x7D6A") ; <CJK>
       (?$(Cl`(B . "0x8335") ; <CJK>
       (?$(Cla(B . "0xF9F0") ; <CJK>
       (?$(Clb(B . "0x8693") ; <CJK>
       (?$(Clc(B . "0x8A8D") ; <CJK>
       (?$(Cld(B . "0xF9F1") ; <CJK>
       (?$(Cle(B . "0x976D") ; <CJK>
       (?$(Clf(B . "0x9777") ; <CJK>
       (?$(Clg(B . "0xF9F2") ; <CJK>
       (?$(Clh(B . "0xF9F3") ; <CJK>
       (?$(Cli(B . "0x4E00") ; <CJK>
       (?$(Clj(B . "0x4F5A") ; <CJK>
       (?$(Clk(B . "0x4F7E") ; <CJK>
       (?$(Cll(B . "0x58F9") ; <CJK>
       (?$(Clm(B . "0x65E5") ; <CJK>
       (?$(Cln(B . "0x6EA2") ; <CJK>
       (?$(Clo(B . "0x9038") ; <CJK>
       (?$(Clp(B . "0x93B0") ; <CJK>
       (?$(Clq(B . "0x99B9") ; <CJK>
       (?$(Clr(B . "0x4EFB") ; <CJK>
       (?$(Cls(B . "0x58EC") ; <CJK>
       (?$(Clt(B . "0x598A") ; <CJK>
       (?$(Clu(B . "0x59D9") ; <CJK>
       (?$(Clv(B . "0x6041") ; <CJK>
       (?$(Clw(B . "0xF9F4") ; <CJK>
       (?$(Clx(B . "0xF9F5") ; <CJK>
       (?$(Cly(B . "0x7A14") ; <CJK>
       (?$(Clz(B . "0xF9F6") ; <CJK>
       (?$(Cl{(B . "0x834F") ; <CJK>
       (?$(Cl|(B . "0x8CC3") ; <CJK>
       (?$(Cl}(B . "0x5165") ; <CJK>
       (?$(Cl~(B . "0x5344") ; <CJK>
       (?$(Cm!(B . "0xF9F7") ; <CJK>
       (?$(Cm"(B . "0xF9F8") ; <CJK>
       (?$(Cm#(B . "0xF9F9") ; <CJK>
       (?$(Cm$(B . "0x4ECD") ; <CJK>
       (?$(Cm%(B . "0x5269") ; <CJK>
       (?$(Cm&(B . "0x5B55") ; <CJK>
       (?$(Cm'(B . "0x82BF") ; <CJK>
       (?$(Cm((B . "0x4ED4") ; <CJK>
       (?$(Cm)(B . "0x523A") ; <CJK>
       (?$(Cm*(B . "0x54A8") ; <CJK>
       (?$(Cm+(B . "0x59C9") ; <CJK>
       (?$(Cm,(B . "0x59FF") ; <CJK>
       (?$(Cm-(B . "0x5B50") ; <CJK>
       (?$(Cm.(B . "0x5B57") ; <CJK>
       (?$(Cm/(B . "0x5B5C") ; <CJK>
       (?$(Cm0(B . "0x6063") ; <CJK>
       (?$(Cm1(B . "0x6148") ; <CJK>
       (?$(Cm2(B . "0x6ECB") ; <CJK>
       (?$(Cm3(B . "0x7099") ; <CJK>
       (?$(Cm4(B . "0x716E") ; <CJK>
       (?$(Cm5(B . "0x7386") ; <CJK>
       (?$(Cm6(B . "0x74F7") ; <CJK>
       (?$(Cm7(B . "0x75B5") ; <CJK>
       (?$(Cm8(B . "0x78C1") ; <CJK>
       (?$(Cm9(B . "0x7D2B") ; <CJK>
       (?$(Cm:(B . "0x8005") ; <CJK>
       (?$(Cm;(B . "0x81EA") ; <CJK>
       (?$(Cm<(B . "0x8328") ; <CJK>
       (?$(Cm=(B . "0x8517") ; <CJK>
       (?$(Cm>(B . "0x85C9") ; <CJK>
       (?$(Cm?(B . "0x8AEE") ; <CJK>
       (?$(Cm@(B . "0x8CC7") ; <CJK>
       (?$(CmA(B . "0x96CC") ; <CJK>
       (?$(CmB(B . "0x4F5C") ; <CJK>
       (?$(CmC(B . "0x52FA") ; <CJK>
       (?$(CmD(B . "0x56BC") ; <CJK>
       (?$(CmE(B . "0x65AB") ; <CJK>
       (?$(CmF(B . "0x6628") ; <CJK>
       (?$(CmG(B . "0x707C") ; <CJK>
       (?$(CmH(B . "0x70B8") ; <CJK>
       (?$(CmI(B . "0x7235") ; <CJK>
       (?$(CmJ(B . "0x7DBD") ; <CJK>
       (?$(CmK(B . "0x828D") ; <CJK>
       (?$(CmL(B . "0x914C") ; <CJK>
       (?$(CmM(B . "0x96C0") ; <CJK>
       (?$(CmN(B . "0x9D72") ; <CJK>
       (?$(CmO(B . "0x5B71") ; <CJK>
       (?$(CmP(B . "0x68E7") ; <CJK>
       (?$(CmQ(B . "0x6B98") ; <CJK>
       (?$(CmR(B . "0x6F7A") ; <CJK>
       (?$(CmS(B . "0x76DE") ; <CJK>
       (?$(CmT(B . "0x5C91") ; <CJK>
       (?$(CmU(B . "0x66AB") ; <CJK>
       (?$(CmV(B . "0x6F5B") ; <CJK>
       (?$(CmW(B . "0x7BB4") ; <CJK>
       (?$(CmX(B . "0x7C2A") ; <CJK>
       (?$(CmY(B . "0x8836") ; <CJK>
       (?$(CmZ(B . "0x96DC") ; <CJK>
       (?$(Cm[(B . "0x4E08") ; <CJK>
       (?$(Cm\(B . "0x4ED7") ; <CJK>
       (?$(Cm](B . "0x5320") ; <CJK>
       (?$(Cm^(B . "0x5834") ; <CJK>
       (?$(Cm_(B . "0x58BB") ; <CJK>
       (?$(Cm`(B . "0x58EF") ; <CJK>
       (?$(Cma(B . "0x596C") ; <CJK>
       (?$(Cmb(B . "0x5C07") ; <CJK>
       (?$(Cmc(B . "0x5E33") ; <CJK>
       (?$(Cmd(B . "0x5E84") ; <CJK>
       (?$(Cme(B . "0x5F35") ; <CJK>
       (?$(Cmf(B . "0x638C") ; <CJK>
       (?$(Cmg(B . "0x66B2") ; <CJK>
       (?$(Cmh(B . "0x6756") ; <CJK>
       (?$(Cmi(B . "0x6A1F") ; <CJK>
       (?$(Cmj(B . "0x6AA3") ; <CJK>
       (?$(Cmk(B . "0x6B0C") ; <CJK>
       (?$(Cml(B . "0x6F3F") ; <CJK>
       (?$(Cmm(B . "0x7246") ; <CJK>
       (?$(Cmn(B . "0xF9FA") ; <CJK>
       (?$(Cmo(B . "0x7350") ; <CJK>
       (?$(Cmp(B . "0x748B") ; <CJK>
       (?$(Cmq(B . "0x7AE0") ; <CJK>
       (?$(Cmr(B . "0x7CA7") ; <CJK>
       (?$(Cms(B . "0x8178") ; <CJK>
       (?$(Cmt(B . "0x81DF") ; <CJK>
       (?$(Cmu(B . "0x81E7") ; <CJK>
       (?$(Cmv(B . "0x838A") ; <CJK>
       (?$(Cmw(B . "0x846C") ; <CJK>
       (?$(Cmx(B . "0x8523") ; <CJK>
       (?$(Cmy(B . "0x8594") ; <CJK>
       (?$(Cmz(B . "0x85CF") ; <CJK>
       (?$(Cm{(B . "0x88DD") ; <CJK>
       (?$(Cm|(B . "0x8D13") ; <CJK>
       (?$(Cm}(B . "0x91AC") ; <CJK>
       (?$(Cm~(B . "0x9577") ; <CJK>
       (?$(Cn!(B . "0x969C") ; <CJK>
       (?$(Cn"(B . "0x518D") ; <CJK>
       (?$(Cn#(B . "0x54C9") ; <CJK>
       (?$(Cn$(B . "0x5728") ; <CJK>
       (?$(Cn%(B . "0x5BB0") ; <CJK>
       (?$(Cn&(B . "0x624D") ; <CJK>
       (?$(Cn'(B . "0x6750") ; <CJK>
       (?$(Cn((B . "0x683D") ; <CJK>
       (?$(Cn)(B . "0x6893") ; <CJK>
       (?$(Cn*(B . "0x6E3D") ; <CJK>
       (?$(Cn+(B . "0x6ED3") ; <CJK>
       (?$(Cn,(B . "0x707D") ; <CJK>
       (?$(Cn-(B . "0x7E21") ; <CJK>
       (?$(Cn.(B . "0x88C1") ; <CJK>
       (?$(Cn/(B . "0x8CA1") ; <CJK>
       (?$(Cn0(B . "0x8F09") ; <CJK>
       (?$(Cn1(B . "0x9F4B") ; <CJK>
       (?$(Cn2(B . "0x9F4E") ; <CJK>
       (?$(Cn3(B . "0x722D") ; <CJK>
       (?$(Cn4(B . "0x7B8F") ; <CJK>
       (?$(Cn5(B . "0x8ACD") ; <CJK>
       (?$(Cn6(B . "0x931A") ; <CJK>
       (?$(Cn7(B . "0x4F47") ; <CJK>
       (?$(Cn8(B . "0x4F4E") ; <CJK>
       (?$(Cn9(B . "0x5132") ; <CJK>
       (?$(Cn:(B . "0x5480") ; <CJK>
       (?$(Cn;(B . "0x59D0") ; <CJK>
       (?$(Cn<(B . "0x5E95") ; <CJK>
       (?$(Cn=(B . "0x62B5") ; <CJK>
       (?$(Cn>(B . "0x6775") ; <CJK>
       (?$(Cn?(B . "0x696E") ; <CJK>
       (?$(Cn@(B . "0x6A17") ; <CJK>
       (?$(CnA(B . "0x6CAE") ; <CJK>
       (?$(CnB(B . "0x6E1A") ; <CJK>
       (?$(CnC(B . "0x72D9") ; <CJK>
       (?$(CnD(B . "0x732A") ; <CJK>
       (?$(CnE(B . "0x75BD") ; <CJK>
       (?$(CnF(B . "0x7BB8") ; <CJK>
       (?$(CnG(B . "0x7D35") ; <CJK>
       (?$(CnH(B . "0x82E7") ; <CJK>
       (?$(CnI(B . "0x83F9") ; <CJK>
       (?$(CnJ(B . "0x8457") ; <CJK>
       (?$(CnK(B . "0x85F7") ; <CJK>
       (?$(CnL(B . "0x8A5B") ; <CJK>
       (?$(CnM(B . "0x8CAF") ; <CJK>
       (?$(CnN(B . "0x8E87") ; <CJK>
       (?$(CnO(B . "0x9019") ; <CJK>
       (?$(CnP(B . "0x90B8") ; <CJK>
       (?$(CnQ(B . "0x96CE") ; <CJK>
       (?$(CnR(B . "0x9F5F") ; <CJK>
       (?$(CnS(B . "0x52E3") ; <CJK>
       (?$(CnT(B . "0x540A") ; <CJK>
       (?$(CnU(B . "0x5AE1") ; <CJK>
       (?$(CnV(B . "0x5BC2") ; <CJK>
       (?$(CnW(B . "0x6458") ; <CJK>
       (?$(CnX(B . "0x6575") ; <CJK>
       (?$(CnY(B . "0x6EF4") ; <CJK>
       (?$(CnZ(B . "0x72C4") ; <CJK>
       (?$(Cn[(B . "0xF9FB") ; <CJK>
       (?$(Cn\(B . "0x7684") ; <CJK>
       (?$(Cn](B . "0x7A4D") ; <CJK>
       (?$(Cn^(B . "0x7B1B") ; <CJK>
       (?$(Cn_(B . "0x7C4D") ; <CJK>
       (?$(Cn`(B . "0x7E3E") ; <CJK>
       (?$(Cna(B . "0x7FDF") ; <CJK>
       (?$(Cnb(B . "0x837B") ; <CJK>
       (?$(Cnc(B . "0x8B2B") ; <CJK>
       (?$(Cnd(B . "0x8CCA") ; <CJK>
       (?$(Cne(B . "0x8D64") ; <CJK>
       (?$(Cnf(B . "0x8DE1") ; <CJK>
       (?$(Cng(B . "0x8E5F") ; <CJK>
       (?$(Cnh(B . "0x8FEA") ; <CJK>
       (?$(Cni(B . "0x8FF9") ; <CJK>
       (?$(Cnj(B . "0x9069") ; <CJK>
       (?$(Cnk(B . "0x93D1") ; <CJK>
       (?$(Cnl(B . "0x4F43") ; <CJK>
       (?$(Cnm(B . "0x4F7A") ; <CJK>
       (?$(Cnn(B . "0x50B3") ; <CJK>
       (?$(Cno(B . "0x5168") ; <CJK>
       (?$(Cnp(B . "0x5178") ; <CJK>
       (?$(Cnq(B . "0x524D") ; <CJK>
       (?$(Cnr(B . "0x526A") ; <CJK>
       (?$(Cns(B . "0x5861") ; <CJK>
       (?$(Cnt(B . "0x587C") ; <CJK>
       (?$(Cnu(B . "0x5960") ; <CJK>
       (?$(Cnv(B . "0x5C08") ; <CJK>
       (?$(Cnw(B . "0x5C55") ; <CJK>
       (?$(Cnx(B . "0x5EDB") ; <CJK>
       (?$(Cny(B . "0x609B") ; <CJK>
       (?$(Cnz(B . "0x6230") ; <CJK>
       (?$(Cn{(B . "0x6813") ; <CJK>
       (?$(Cn|(B . "0x6BBF") ; <CJK>
       (?$(Cn}(B . "0x6C08") ; <CJK>
       (?$(Cn~(B . "0x6FB1") ; <CJK>
       (?$(Co!(B . "0x714E") ; <CJK>
       (?$(Co"(B . "0x7420") ; <CJK>
       (?$(Co#(B . "0x7530") ; <CJK>
       (?$(Co$(B . "0x7538") ; <CJK>
       (?$(Co%(B . "0x7551") ; <CJK>
       (?$(Co&(B . "0x7672") ; <CJK>
       (?$(Co'(B . "0x7B4C") ; <CJK>
       (?$(Co((B . "0x7B8B") ; <CJK>
       (?$(Co)(B . "0x7BAD") ; <CJK>
       (?$(Co*(B . "0x7BC6") ; <CJK>
       (?$(Co+(B . "0x7E8F") ; <CJK>
       (?$(Co,(B . "0x8A6E") ; <CJK>
       (?$(Co-(B . "0x8F3E") ; <CJK>
       (?$(Co.(B . "0x8F49") ; <CJK>
       (?$(Co/(B . "0x923F") ; <CJK>
       (?$(Co0(B . "0x9293") ; <CJK>
       (?$(Co1(B . "0x9322") ; <CJK>
       (?$(Co2(B . "0x942B") ; <CJK>
       (?$(Co3(B . "0x96FB") ; <CJK>
       (?$(Co4(B . "0x985A") ; <CJK>
       (?$(Co5(B . "0x986B") ; <CJK>
       (?$(Co6(B . "0x991E") ; <CJK>
       (?$(Co7(B . "0x5207") ; <CJK>
       (?$(Co8(B . "0x622A") ; <CJK>
       (?$(Co9(B . "0x6298") ; <CJK>
       (?$(Co:(B . "0x6D59") ; <CJK>
       (?$(Co;(B . "0x7664") ; <CJK>
       (?$(Co<(B . "0x7ACA") ; <CJK>
       (?$(Co=(B . "0x7BC0") ; <CJK>
       (?$(Co>(B . "0x7D76") ; <CJK>
       (?$(Co?(B . "0x5360") ; <CJK>
       (?$(Co@(B . "0x5CBE") ; <CJK>
       (?$(CoA(B . "0x5E97") ; <CJK>
       (?$(CoB(B . "0x6F38") ; <CJK>
       (?$(CoC(B . "0x70B9") ; <CJK>
       (?$(CoD(B . "0x7C98") ; <CJK>
       (?$(CoE(B . "0x9711") ; <CJK>
       (?$(CoF(B . "0x9B8E") ; <CJK>
       (?$(CoG(B . "0x9EDE") ; <CJK>
       (?$(CoH(B . "0x63A5") ; <CJK>
       (?$(CoI(B . "0x647A") ; <CJK>
       (?$(CoJ(B . "0x8776") ; <CJK>
       (?$(CoK(B . "0x4E01") ; <CJK>
       (?$(CoL(B . "0x4E95") ; <CJK>
       (?$(CoM(B . "0x4EAD") ; <CJK>
       (?$(CoN(B . "0x505C") ; <CJK>
       (?$(CoO(B . "0x5075") ; <CJK>
       (?$(CoP(B . "0x5448") ; <CJK>
       (?$(CoQ(B . "0x59C3") ; <CJK>
       (?$(CoR(B . "0x5B9A") ; <CJK>
       (?$(CoS(B . "0x5E40") ; <CJK>
       (?$(CoT(B . "0x5EAD") ; <CJK>
       (?$(CoU(B . "0x5EF7") ; <CJK>
       (?$(CoV(B . "0x5F81") ; <CJK>
       (?$(CoW(B . "0x60C5") ; <CJK>
       (?$(CoX(B . "0x633A") ; <CJK>
       (?$(CoY(B . "0x653F") ; <CJK>
       (?$(CoZ(B . "0x6574") ; <CJK>
       (?$(Co[(B . "0x65CC") ; <CJK>
       (?$(Co\(B . "0x6676") ; <CJK>
       (?$(Co](B . "0x6678") ; <CJK>
       (?$(Co^(B . "0x67FE") ; <CJK>
       (?$(Co_(B . "0x6968") ; <CJK>
       (?$(Co`(B . "0x6A89") ; <CJK>
       (?$(Coa(B . "0x6B63") ; <CJK>
       (?$(Cob(B . "0x6C40") ; <CJK>
       (?$(Coc(B . "0x6DC0") ; <CJK>
       (?$(Cod(B . "0x6DE8") ; <CJK>
       (?$(Coe(B . "0x6E1F") ; <CJK>
       (?$(Cof(B . "0x6E5E") ; <CJK>
       (?$(Cog(B . "0x701E") ; <CJK>
       (?$(Coh(B . "0x70A1") ; <CJK>
       (?$(Coi(B . "0x738E") ; <CJK>
       (?$(Coj(B . "0x73FD") ; <CJK>
       (?$(Cok(B . "0x753A") ; <CJK>
       (?$(Col(B . "0x775B") ; <CJK>
       (?$(Com(B . "0x7887") ; <CJK>
       (?$(Con(B . "0x798E") ; <CJK>
       (?$(Coo(B . "0x7A0B") ; <CJK>
       (?$(Cop(B . "0x7A7D") ; <CJK>
       (?$(Coq(B . "0x7CBE") ; <CJK>
       (?$(Cor(B . "0x7D8E") ; <CJK>
       (?$(Cos(B . "0x8247") ; <CJK>
       (?$(Cot(B . "0x8A02") ; <CJK>
       (?$(Cou(B . "0x8AEA") ; <CJK>
       (?$(Cov(B . "0x8C9E") ; <CJK>
       (?$(Cow(B . "0x912D") ; <CJK>
       (?$(Cox(B . "0x914A") ; <CJK>
       (?$(Coy(B . "0x91D8") ; <CJK>
       (?$(Coz(B . "0x9266") ; <CJK>
       (?$(Co{(B . "0x92CC") ; <CJK>
       (?$(Co|(B . "0x9320") ; <CJK>
       (?$(Co}(B . "0x9706") ; <CJK>
       (?$(Co~(B . "0x9756") ; <CJK>
       (?$(Cp!(B . "0x975C") ; <CJK>
       (?$(Cp"(B . "0x9802") ; <CJK>
       (?$(Cp#(B . "0x9F0E") ; <CJK>
       (?$(Cp$(B . "0x5236") ; <CJK>
       (?$(Cp%(B . "0x5291") ; <CJK>
       (?$(Cp&(B . "0x557C") ; <CJK>
       (?$(Cp'(B . "0x5824") ; <CJK>
       (?$(Cp((B . "0x5E1D") ; <CJK>
       (?$(Cp)(B . "0x5F1F") ; <CJK>
       (?$(Cp*(B . "0x608C") ; <CJK>
       (?$(Cp+(B . "0x63D0") ; <CJK>
       (?$(Cp,(B . "0x68AF") ; <CJK>
       (?$(Cp-(B . "0x6FDF") ; <CJK>
       (?$(Cp.(B . "0x796D") ; <CJK>
       (?$(Cp/(B . "0x7B2C") ; <CJK>
       (?$(Cp0(B . "0x81CD") ; <CJK>
       (?$(Cp1(B . "0x85BA") ; <CJK>
       (?$(Cp2(B . "0x88FD") ; <CJK>
       (?$(Cp3(B . "0x8AF8") ; <CJK>
       (?$(Cp4(B . "0x8E44") ; <CJK>
       (?$(Cp5(B . "0x918D") ; <CJK>
       (?$(Cp6(B . "0x9664") ; <CJK>
       (?$(Cp7(B . "0x969B") ; <CJK>
       (?$(Cp8(B . "0x973D") ; <CJK>
       (?$(Cp9(B . "0x984C") ; <CJK>
       (?$(Cp:(B . "0x9F4A") ; <CJK>
       (?$(Cp;(B . "0x4FCE") ; <CJK>
       (?$(Cp<(B . "0x5146") ; <CJK>
       (?$(Cp=(B . "0x51CB") ; <CJK>
       (?$(Cp>(B . "0x52A9") ; <CJK>
       (?$(Cp?(B . "0x5632") ; <CJK>
       (?$(Cp@(B . "0x5F14") ; <CJK>
       (?$(CpA(B . "0x5F6B") ; <CJK>
       (?$(CpB(B . "0x63AA") ; <CJK>
       (?$(CpC(B . "0x64CD") ; <CJK>
       (?$(CpD(B . "0x65E9") ; <CJK>
       (?$(CpE(B . "0x6641") ; <CJK>
       (?$(CpF(B . "0x66FA") ; <CJK>
       (?$(CpG(B . "0x66F9") ; <CJK>
       (?$(CpH(B . "0x671D") ; <CJK>
       (?$(CpI(B . "0x689D") ; <CJK>
       (?$(CpJ(B . "0x68D7") ; <CJK>
       (?$(CpK(B . "0x69FD") ; <CJK>
       (?$(CpL(B . "0x6F15") ; <CJK>
       (?$(CpM(B . "0x6F6E") ; <CJK>
       (?$(CpN(B . "0x7167") ; <CJK>
       (?$(CpO(B . "0x71E5") ; <CJK>
       (?$(CpP(B . "0x722A") ; <CJK>
       (?$(CpQ(B . "0x74AA") ; <CJK>
       (?$(CpR(B . "0x773A") ; <CJK>
       (?$(CpS(B . "0x7956") ; <CJK>
       (?$(CpT(B . "0x795A") ; <CJK>
       (?$(CpU(B . "0x79DF") ; <CJK>
       (?$(CpV(B . "0x7A20") ; <CJK>
       (?$(CpW(B . "0x7A95") ; <CJK>
       (?$(CpX(B . "0x7C97") ; <CJK>
       (?$(CpY(B . "0x7CDF") ; <CJK>
       (?$(CpZ(B . "0x7D44") ; <CJK>
       (?$(Cp[(B . "0x7E70") ; <CJK>
       (?$(Cp\(B . "0x8087") ; <CJK>
       (?$(Cp](B . "0x85FB") ; <CJK>
       (?$(Cp^(B . "0x86A4") ; <CJK>
       (?$(Cp_(B . "0x8A54") ; <CJK>
       (?$(Cp`(B . "0x8ABF") ; <CJK>
       (?$(Cpa(B . "0x8D99") ; <CJK>
       (?$(Cpb(B . "0x8E81") ; <CJK>
       (?$(Cpc(B . "0x9020") ; <CJK>
       (?$(Cpd(B . "0x906D") ; <CJK>
       (?$(Cpe(B . "0x91E3") ; <CJK>
       (?$(Cpf(B . "0x963B") ; <CJK>
       (?$(Cpg(B . "0x96D5") ; <CJK>
       (?$(Cph(B . "0x9CE5") ; <CJK>
       (?$(Cpi(B . "0x65CF") ; <CJK>
       (?$(Cpj(B . "0x7C07") ; <CJK>
       (?$(Cpk(B . "0x8DB3") ; <CJK>
       (?$(Cpl(B . "0x93C3") ; <CJK>
       (?$(Cpm(B . "0x5B58") ; <CJK>
       (?$(Cpn(B . "0x5C0A") ; <CJK>
       (?$(Cpo(B . "0x5352") ; <CJK>
       (?$(Cpp(B . "0x62D9") ; <CJK>
       (?$(Cpq(B . "0x731D") ; <CJK>
       (?$(Cpr(B . "0x5027") ; <CJK>
       (?$(Cps(B . "0x5B97") ; <CJK>
       (?$(Cpt(B . "0x5F9E") ; <CJK>
       (?$(Cpu(B . "0x60B0") ; <CJK>
       (?$(Cpv(B . "0x616B") ; <CJK>
       (?$(Cpw(B . "0x68D5") ; <CJK>
       (?$(Cpx(B . "0x6DD9") ; <CJK>
       (?$(Cpy(B . "0x742E") ; <CJK>
       (?$(Cpz(B . "0x7A2E") ; <CJK>
       (?$(Cp{(B . "0x7D42") ; <CJK>
       (?$(Cp|(B . "0x7D9C") ; <CJK>
       (?$(Cp}(B . "0x7E31") ; <CJK>
       (?$(Cp~(B . "0x816B") ; <CJK>
       (?$(Cq!(B . "0x8E2A") ; <CJK>
       (?$(Cq"(B . "0x8E35") ; <CJK>
       (?$(Cq#(B . "0x937E") ; <CJK>
       (?$(Cq$(B . "0x9418") ; <CJK>
       (?$(Cq%(B . "0x4F50") ; <CJK>
       (?$(Cq&(B . "0x5750") ; <CJK>
       (?$(Cq'(B . "0x5DE6") ; <CJK>
       (?$(Cq((B . "0x5EA7") ; <CJK>
       (?$(Cq)(B . "0x632B") ; <CJK>
       (?$(Cq*(B . "0x7F6A") ; <CJK>
       (?$(Cq+(B . "0x4E3B") ; <CJK>
       (?$(Cq,(B . "0x4F4F") ; <CJK>
       (?$(Cq-(B . "0x4F8F") ; <CJK>
       (?$(Cq.(B . "0x505A") ; <CJK>
       (?$(Cq/(B . "0x59DD") ; <CJK>
       (?$(Cq0(B . "0x80C4") ; <CJK>
       (?$(Cq1(B . "0x546A") ; <CJK>
       (?$(Cq2(B . "0x5468") ; <CJK>
       (?$(Cq3(B . "0x55FE") ; <CJK>
       (?$(Cq4(B . "0x594F") ; <CJK>
       (?$(Cq5(B . "0x5B99") ; <CJK>
       (?$(Cq6(B . "0x5DDE") ; <CJK>
       (?$(Cq7(B . "0x5EDA") ; <CJK>
       (?$(Cq8(B . "0x665D") ; <CJK>
       (?$(Cq9(B . "0x6731") ; <CJK>
       (?$(Cq:(B . "0x67F1") ; <CJK>
       (?$(Cq;(B . "0x682A") ; <CJK>
       (?$(Cq<(B . "0x6CE8") ; <CJK>
       (?$(Cq=(B . "0x6D32") ; <CJK>
       (?$(Cq>(B . "0x6E4A") ; <CJK>
       (?$(Cq?(B . "0x6F8D") ; <CJK>
       (?$(Cq@(B . "0x70B7") ; <CJK>
       (?$(CqA(B . "0x73E0") ; <CJK>
       (?$(CqB(B . "0x7587") ; <CJK>
       (?$(CqC(B . "0x7C4C") ; <CJK>
       (?$(CqD(B . "0x7D02") ; <CJK>
       (?$(CqE(B . "0x7D2C") ; <CJK>
       (?$(CqF(B . "0x7DA2") ; <CJK>
       (?$(CqG(B . "0x821F") ; <CJK>
       (?$(CqH(B . "0x86DB") ; <CJK>
       (?$(CqI(B . "0x8A3B") ; <CJK>
       (?$(CqJ(B . "0x8A85") ; <CJK>
       (?$(CqK(B . "0x8D70") ; <CJK>
       (?$(CqL(B . "0x8E8A") ; <CJK>
       (?$(CqM(B . "0x8F33") ; <CJK>
       (?$(CqN(B . "0x9031") ; <CJK>
       (?$(CqO(B . "0x914E") ; <CJK>
       (?$(CqP(B . "0x9152") ; <CJK>
       (?$(CqQ(B . "0x9444") ; <CJK>
       (?$(CqR(B . "0x99D0") ; <CJK>
       (?$(CqS(B . "0x7AF9") ; <CJK>
       (?$(CqT(B . "0x7CA5") ; <CJK>
       (?$(CqU(B . "0x4FCA") ; <CJK>
       (?$(CqV(B . "0x5101") ; <CJK>
       (?$(CqW(B . "0x51C6") ; <CJK>
       (?$(CqX(B . "0x57C8") ; <CJK>
       (?$(CqY(B . "0x5BEF") ; <CJK>
       (?$(CqZ(B . "0x5CFB") ; <CJK>
       (?$(Cq[(B . "0x6659") ; <CJK>
       (?$(Cq\(B . "0x6A3D") ; <CJK>
       (?$(Cq](B . "0x6D5A") ; <CJK>
       (?$(Cq^(B . "0x6E96") ; <CJK>
       (?$(Cq_(B . "0x6FEC") ; <CJK>
       (?$(Cq`(B . "0x710C") ; <CJK>
       (?$(Cqa(B . "0x756F") ; <CJK>
       (?$(Cqb(B . "0x7AE3") ; <CJK>
       (?$(Cqc(B . "0x8822") ; <CJK>
       (?$(Cqd(B . "0x9021") ; <CJK>
       (?$(Cqe(B . "0x9075") ; <CJK>
       (?$(Cqf(B . "0x96CB") ; <CJK>
       (?$(Cqg(B . "0x99FF") ; <CJK>
       (?$(Cqh(B . "0x8301") ; <CJK>
       (?$(Cqi(B . "0x4E2D") ; <CJK>
       (?$(Cqj(B . "0x4EF2") ; <CJK>
       (?$(Cqk(B . "0x8846") ; <CJK>
       (?$(Cql(B . "0x91CD") ; <CJK>
       (?$(Cqm(B . "0x537D") ; <CJK>
       (?$(Cqn(B . "0x6ADB") ; <CJK>
       (?$(Cqo(B . "0x696B") ; <CJK>
       (?$(Cqp(B . "0x6C41") ; <CJK>
       (?$(Cqq(B . "0x847A") ; <CJK>
       (?$(Cqr(B . "0x589E") ; <CJK>
       (?$(Cqs(B . "0x618E") ; <CJK>
       (?$(Cqt(B . "0x66FE") ; <CJK>
       (?$(Cqu(B . "0x62EF") ; <CJK>
       (?$(Cqv(B . "0x70DD") ; <CJK>
       (?$(Cqw(B . "0x7511") ; <CJK>
       (?$(Cqx(B . "0x75C7") ; <CJK>
       (?$(Cqy(B . "0x7E52") ; <CJK>
       (?$(Cqz(B . "0x84B8") ; <CJK>
       (?$(Cq{(B . "0x8B49") ; <CJK>
       (?$(Cq|(B . "0x8D08") ; <CJK>
       (?$(Cq}(B . "0x4E4B") ; <CJK>
       (?$(Cq~(B . "0x53EA") ; <CJK>
       (?$(Cr!(B . "0x54AB") ; <CJK>
       (?$(Cr"(B . "0x5730") ; <CJK>
       (?$(Cr#(B . "0x5740") ; <CJK>
       (?$(Cr$(B . "0x5FD7") ; <CJK>
       (?$(Cr%(B . "0x6301") ; <CJK>
       (?$(Cr&(B . "0x6307") ; <CJK>
       (?$(Cr'(B . "0x646F") ; <CJK>
       (?$(Cr((B . "0x652F") ; <CJK>
       (?$(Cr)(B . "0x65E8") ; <CJK>
       (?$(Cr*(B . "0x667A") ; <CJK>
       (?$(Cr+(B . "0x679D") ; <CJK>
       (?$(Cr,(B . "0x67B3") ; <CJK>
       (?$(Cr-(B . "0x6B62") ; <CJK>
       (?$(Cr.(B . "0x6C60") ; <CJK>
       (?$(Cr/(B . "0x6C9A") ; <CJK>
       (?$(Cr0(B . "0x6F2C") ; <CJK>
       (?$(Cr1(B . "0x77E5") ; <CJK>
       (?$(Cr2(B . "0x7825") ; <CJK>
       (?$(Cr3(B . "0x7949") ; <CJK>
       (?$(Cr4(B . "0x7957") ; <CJK>
       (?$(Cr5(B . "0x7D19") ; <CJK>
       (?$(Cr6(B . "0x80A2") ; <CJK>
       (?$(Cr7(B . "0x8102") ; <CJK>
       (?$(Cr8(B . "0x81F3") ; <CJK>
       (?$(Cr9(B . "0x829D") ; <CJK>
       (?$(Cr:(B . "0x82B7") ; <CJK>
       (?$(Cr;(B . "0x8718") ; <CJK>
       (?$(Cr<(B . "0x8A8C") ; <CJK>
       (?$(Cr=(B . "0xF9FC") ; <CJK>
       (?$(Cr>(B . "0x8D04") ; <CJK>
       (?$(Cr?(B . "0x8DBE") ; <CJK>
       (?$(Cr@(B . "0x9072") ; <CJK>
       (?$(CrA(B . "0x76F4") ; <CJK>
       (?$(CrB(B . "0x7A19") ; <CJK>
       (?$(CrC(B . "0x7A37") ; <CJK>
       (?$(CrD(B . "0x7E54") ; <CJK>
       (?$(CrE(B . "0x8077") ; <CJK>
       (?$(CrF(B . "0x5507") ; <CJK>
       (?$(CrG(B . "0x55D4") ; <CJK>
       (?$(CrH(B . "0x5875") ; <CJK>
       (?$(CrI(B . "0x632F") ; <CJK>
       (?$(CrJ(B . "0x6422") ; <CJK>
       (?$(CrK(B . "0x6649") ; <CJK>
       (?$(CrL(B . "0x664B") ; <CJK>
       (?$(CrM(B . "0x686D") ; <CJK>
       (?$(CrN(B . "0x699B") ; <CJK>
       (?$(CrO(B . "0x6B84") ; <CJK>
       (?$(CrP(B . "0x6D25") ; <CJK>
       (?$(CrQ(B . "0x6EB1") ; <CJK>
       (?$(CrR(B . "0x73CD") ; <CJK>
       (?$(CrS(B . "0x7468") ; <CJK>
       (?$(CrT(B . "0x74A1") ; <CJK>
       (?$(CrU(B . "0x755B") ; <CJK>
       (?$(CrV(B . "0x75B9") ; <CJK>
       (?$(CrW(B . "0x76E1") ; <CJK>
       (?$(CrX(B . "0x771E") ; <CJK>
       (?$(CrY(B . "0x778B") ; <CJK>
       (?$(CrZ(B . "0x79E6") ; <CJK>
       (?$(Cr[(B . "0x7E09") ; <CJK>
       (?$(Cr\(B . "0x7E1D") ; <CJK>
       (?$(Cr](B . "0x81FB") ; <CJK>
       (?$(Cr^(B . "0x852F") ; <CJK>
       (?$(Cr_(B . "0x8897") ; <CJK>
       (?$(Cr`(B . "0x8A3A") ; <CJK>
       (?$(Cra(B . "0x8CD1") ; <CJK>
       (?$(Crb(B . "0x8EEB") ; <CJK>
       (?$(Crc(B . "0x8FB0") ; <CJK>
       (?$(Crd(B . "0x9032") ; <CJK>
       (?$(Cre(B . "0x93AD") ; <CJK>
       (?$(Crf(B . "0x9663") ; <CJK>
       (?$(Crg(B . "0x9673") ; <CJK>
       (?$(Crh(B . "0x9707") ; <CJK>
       (?$(Cri(B . "0x4F84") ; <CJK>
       (?$(Crj(B . "0x53F1") ; <CJK>
       (?$(Crk(B . "0x59EA") ; <CJK>
       (?$(Crl(B . "0x5AC9") ; <CJK>
       (?$(Crm(B . "0x5E19") ; <CJK>
       (?$(Crn(B . "0x684E") ; <CJK>
       (?$(Cro(B . "0x74C6") ; <CJK>
       (?$(Crp(B . "0x75BE") ; <CJK>
       (?$(Crq(B . "0x79E9") ; <CJK>
       (?$(Crr(B . "0x7A92") ; <CJK>
       (?$(Crs(B . "0x81A3") ; <CJK>
       (?$(Crt(B . "0x86ED") ; <CJK>
       (?$(Cru(B . "0x8CEA") ; <CJK>
       (?$(Crv(B . "0x8DCC") ; <CJK>
       (?$(Crw(B . "0x8FED") ; <CJK>
       (?$(Crx(B . "0x659F") ; <CJK>
       (?$(Cry(B . "0x6715") ; <CJK>
       (?$(Crz(B . "0xF9FD") ; <CJK>
       (?$(Cr{(B . "0x57F7") ; <CJK>
       (?$(Cr|(B . "0x6F57") ; <CJK>
       (?$(Cr}(B . "0x7DDD") ; <CJK>
       (?$(Cr~(B . "0x8F2F") ; <CJK>
       (?$(Cs!(B . "0x93F6") ; <CJK>
       (?$(Cs"(B . "0x96C6") ; <CJK>
       (?$(Cs#(B . "0x5FB5") ; <CJK>
       (?$(Cs$(B . "0x61F2") ; <CJK>
       (?$(Cs%(B . "0x6F84") ; <CJK>
       (?$(Cs&(B . "0x4E14") ; <CJK>
       (?$(Cs'(B . "0x4F98") ; <CJK>
       (?$(Cs((B . "0x501F") ; <CJK>
       (?$(Cs)(B . "0x53C9") ; <CJK>
       (?$(Cs*(B . "0x55DF") ; <CJK>
       (?$(Cs+(B . "0x5D6F") ; <CJK>
       (?$(Cs,(B . "0x5DEE") ; <CJK>
       (?$(Cs-(B . "0x6B21") ; <CJK>
       (?$(Cs.(B . "0x6B64") ; <CJK>
       (?$(Cs/(B . "0x78CB") ; <CJK>
       (?$(Cs0(B . "0x7B9A") ; <CJK>
       (?$(Cs1(B . "0xF9FE") ; <CJK>
       (?$(Cs2(B . "0x8E49") ; <CJK>
       (?$(Cs3(B . "0x8ECA") ; <CJK>
       (?$(Cs4(B . "0x906E") ; <CJK>
       (?$(Cs5(B . "0x6349") ; <CJK>
       (?$(Cs6(B . "0x643E") ; <CJK>
       (?$(Cs7(B . "0x7740") ; <CJK>
       (?$(Cs8(B . "0x7A84") ; <CJK>
       (?$(Cs9(B . "0x932F") ; <CJK>
       (?$(Cs:(B . "0x947F") ; <CJK>
       (?$(Cs;(B . "0x9F6A") ; <CJK>
       (?$(Cs<(B . "0x64B0") ; <CJK>
       (?$(Cs=(B . "0x6FAF") ; <CJK>
       (?$(Cs>(B . "0x71E6") ; <CJK>
       (?$(Cs?(B . "0x74A8") ; <CJK>
       (?$(Cs@(B . "0x74DA") ; <CJK>
       (?$(CsA(B . "0x7AC4") ; <CJK>
       (?$(CsB(B . "0x7C12") ; <CJK>
       (?$(CsC(B . "0x7E82") ; <CJK>
       (?$(CsD(B . "0x7CB2") ; <CJK>
       (?$(CsE(B . "0x7E98") ; <CJK>
       (?$(CsF(B . "0x8B9A") ; <CJK>
       (?$(CsG(B . "0x8D0A") ; <CJK>
       (?$(CsH(B . "0x947D") ; <CJK>
       (?$(CsI(B . "0x9910") ; <CJK>
       (?$(CsJ(B . "0x994C") ; <CJK>
       (?$(CsK(B . "0x5239") ; <CJK>
       (?$(CsL(B . "0x5BDF") ; <CJK>
       (?$(CsM(B . "0x64E6") ; <CJK>
       (?$(CsN(B . "0x672D") ; <CJK>
       (?$(CsO(B . "0x7D2E") ; <CJK>
       (?$(CsP(B . "0x50ED") ; <CJK>
       (?$(CsQ(B . "0x53C3") ; <CJK>
       (?$(CsR(B . "0x5879") ; <CJK>
       (?$(CsS(B . "0x6158") ; <CJK>
       (?$(CsT(B . "0x6159") ; <CJK>
       (?$(CsU(B . "0x61FA") ; <CJK>
       (?$(CsV(B . "0x65AC") ; <CJK>
       (?$(CsW(B . "0x7AD9") ; <CJK>
       (?$(CsX(B . "0x8B92") ; <CJK>
       (?$(CsY(B . "0x8B96") ; <CJK>
       (?$(CsZ(B . "0x5009") ; <CJK>
       (?$(Cs[(B . "0x5021") ; <CJK>
       (?$(Cs\(B . "0x5275") ; <CJK>
       (?$(Cs](B . "0x5531") ; <CJK>
       (?$(Cs^(B . "0x5A3C") ; <CJK>
       (?$(Cs_(B . "0x5EE0") ; <CJK>
       (?$(Cs`(B . "0x5F70") ; <CJK>
       (?$(Csa(B . "0x6134") ; <CJK>
       (?$(Csb(B . "0x655E") ; <CJK>
       (?$(Csc(B . "0x660C") ; <CJK>
       (?$(Csd(B . "0x6636") ; <CJK>
       (?$(Cse(B . "0x66A2") ; <CJK>
       (?$(Csf(B . "0x69CD") ; <CJK>
       (?$(Csg(B . "0x6EC4") ; <CJK>
       (?$(Csh(B . "0x6F32") ; <CJK>
       (?$(Csi(B . "0x7316") ; <CJK>
       (?$(Csj(B . "0x7621") ; <CJK>
       (?$(Csk(B . "0x7A93") ; <CJK>
       (?$(Csl(B . "0x8139") ; <CJK>
       (?$(Csm(B . "0x8259") ; <CJK>
       (?$(Csn(B . "0x83D6") ; <CJK>
       (?$(Cso(B . "0x84BC") ; <CJK>
       (?$(Csp(B . "0x50B5") ; <CJK>
       (?$(Csq(B . "0x57F0") ; <CJK>
       (?$(Csr(B . "0x5BC0") ; <CJK>
       (?$(Css(B . "0x5BE8") ; <CJK>
       (?$(Cst(B . "0x5F69") ; <CJK>
       (?$(Csu(B . "0x63A1") ; <CJK>
       (?$(Csv(B . "0x7826") ; <CJK>
       (?$(Csw(B . "0x7DB5") ; <CJK>
       (?$(Csx(B . "0x83DC") ; <CJK>
       (?$(Csy(B . "0x8521") ; <CJK>
       (?$(Csz(B . "0x91C7") ; <CJK>
       (?$(Cs{(B . "0x91F5") ; <CJK>
       (?$(Cs|(B . "0x518A") ; <CJK>
       (?$(Cs}(B . "0x67F5") ; <CJK>
       (?$(Cs~(B . "0x7B56") ; <CJK>
       (?$(Ct!(B . "0x8CAC") ; <CJK>
       (?$(Ct"(B . "0x51C4") ; <CJK>
       (?$(Ct#(B . "0x59BB") ; <CJK>
       (?$(Ct$(B . "0x60BD") ; <CJK>
       (?$(Ct%(B . "0x8655") ; <CJK>
       (?$(Ct&(B . "0x501C") ; <CJK>
       (?$(Ct'(B . "0xF9FF") ; <CJK>
       (?$(Ct((B . "0x5254") ; <CJK>
       (?$(Ct)(B . "0x5C3A") ; <CJK>
       (?$(Ct*(B . "0x617D") ; <CJK>
       (?$(Ct+(B . "0x621A") ; <CJK>
       (?$(Ct,(B . "0x62D3") ; <CJK>
       (?$(Ct-(B . "0x64F2") ; <CJK>
       (?$(Ct.(B . "0x65A5") ; <CJK>
       (?$(Ct/(B . "0x6ECC") ; <CJK>
       (?$(Ct0(B . "0x7620") ; <CJK>
       (?$(Ct1(B . "0x810A") ; <CJK>
       (?$(Ct2(B . "0x8E60") ; <CJK>
       (?$(Ct3(B . "0x965F") ; <CJK>
       (?$(Ct4(B . "0x96BB") ; <CJK>
       (?$(Ct5(B . "0x4EDF") ; <CJK>
       (?$(Ct6(B . "0x5343") ; <CJK>
       (?$(Ct7(B . "0x5598") ; <CJK>
       (?$(Ct8(B . "0x5929") ; <CJK>
       (?$(Ct9(B . "0x5DDD") ; <CJK>
       (?$(Ct:(B . "0x64C5") ; <CJK>
       (?$(Ct;(B . "0x6CC9") ; <CJK>
       (?$(Ct<(B . "0x6DFA") ; <CJK>
       (?$(Ct=(B . "0x7394") ; <CJK>
       (?$(Ct>(B . "0x7A7F") ; <CJK>
       (?$(Ct?(B . "0x821B") ; <CJK>
       (?$(Ct@(B . "0x85A6") ; <CJK>
       (?$(CtA(B . "0x8CE4") ; <CJK>
       (?$(CtB(B . "0x8E10") ; <CJK>
       (?$(CtC(B . "0x9077") ; <CJK>
       (?$(CtD(B . "0x91E7") ; <CJK>
       (?$(CtE(B . "0x95E1") ; <CJK>
       (?$(CtF(B . "0x9621") ; <CJK>
       (?$(CtG(B . "0x97C6") ; <CJK>
       (?$(CtH(B . "0x51F8") ; <CJK>
       (?$(CtI(B . "0x54F2") ; <CJK>
       (?$(CtJ(B . "0x5586") ; <CJK>
       (?$(CtK(B . "0x5FB9") ; <CJK>
       (?$(CtL(B . "0x64A4") ; <CJK>
       (?$(CtM(B . "0x6F88") ; <CJK>
       (?$(CtN(B . "0x7DB4") ; <CJK>
       (?$(CtO(B . "0x8F1F") ; <CJK>
       (?$(CtP(B . "0x8F4D") ; <CJK>
       (?$(CtQ(B . "0x9435") ; <CJK>
       (?$(CtR(B . "0x50C9") ; <CJK>
       (?$(CtS(B . "0x5C16") ; <CJK>
       (?$(CtT(B . "0x6CBE") ; <CJK>
       (?$(CtU(B . "0x6DFB") ; <CJK>
       (?$(CtV(B . "0x751B") ; <CJK>
       (?$(CtW(B . "0x77BB") ; <CJK>
       (?$(CtX(B . "0x7C3D") ; <CJK>
       (?$(CtY(B . "0x7C64") ; <CJK>
       (?$(CtZ(B . "0x8A79") ; <CJK>
       (?$(Ct[(B . "0x8AC2") ; <CJK>
       (?$(Ct\(B . "0x581E") ; <CJK>
       (?$(Ct](B . "0x59BE") ; <CJK>
       (?$(Ct^(B . "0x5E16") ; <CJK>
       (?$(Ct_(B . "0x6377") ; <CJK>
       (?$(Ct`(B . "0x7252") ; <CJK>
       (?$(Cta(B . "0x758A") ; <CJK>
       (?$(Ctb(B . "0x776B") ; <CJK>
       (?$(Ctc(B . "0x8ADC") ; <CJK>
       (?$(Ctd(B . "0x8CBC") ; <CJK>
       (?$(Cte(B . "0x8F12") ; <CJK>
       (?$(Ctf(B . "0x5EF3") ; <CJK>
       (?$(Ctg(B . "0x6674") ; <CJK>
       (?$(Cth(B . "0x6DF8") ; <CJK>
       (?$(Cti(B . "0x807D") ; <CJK>
       (?$(Ctj(B . "0x83C1") ; <CJK>
       (?$(Ctk(B . "0x8ACB") ; <CJK>
       (?$(Ctl(B . "0x9751") ; <CJK>
       (?$(Ctm(B . "0x9BD6") ; <CJK>
       (?$(Ctn(B . "0xFA00") ; <CJK>
       (?$(Cto(B . "0x5243") ; <CJK>
       (?$(Ctp(B . "0x66FF") ; <CJK>
       (?$(Ctq(B . "0x6D95") ; <CJK>
       (?$(Ctr(B . "0x6EEF") ; <CJK>
       (?$(Cts(B . "0x7DE0") ; <CJK>
       (?$(Ctt(B . "0x8AE6") ; <CJK>
       (?$(Ctu(B . "0x902E") ; <CJK>
       (?$(Ctv(B . "0x905E") ; <CJK>
       (?$(Ctw(B . "0x9AD4") ; <CJK>
       (?$(Ctx(B . "0x521D") ; <CJK>
       (?$(Cty(B . "0x527F") ; <CJK>
       (?$(Ctz(B . "0x54E8") ; <CJK>
       (?$(Ct{(B . "0x6194") ; <CJK>
       (?$(Ct|(B . "0x6284") ; <CJK>
       (?$(Ct}(B . "0x62DB") ; <CJK>
       (?$(Ct~(B . "0x68A2") ; <CJK>
       (?$(Cu!(B . "0x6912") ; <CJK>
       (?$(Cu"(B . "0x695A") ; <CJK>
       (?$(Cu#(B . "0x6A35") ; <CJK>
       (?$(Cu$(B . "0x7092") ; <CJK>
       (?$(Cu%(B . "0x7126") ; <CJK>
       (?$(Cu&(B . "0x785D") ; <CJK>
       (?$(Cu'(B . "0x7901") ; <CJK>
       (?$(Cu((B . "0x790E") ; <CJK>
       (?$(Cu)(B . "0x79D2") ; <CJK>
       (?$(Cu*(B . "0x7A0D") ; <CJK>
       (?$(Cu+(B . "0x8096") ; <CJK>
       (?$(Cu,(B . "0x8278") ; <CJK>
       (?$(Cu-(B . "0x82D5") ; <CJK>
       (?$(Cu.(B . "0x8349") ; <CJK>
       (?$(Cu/(B . "0x8549") ; <CJK>
       (?$(Cu0(B . "0x8C82") ; <CJK>
       (?$(Cu1(B . "0x8D85") ; <CJK>
       (?$(Cu2(B . "0x9162") ; <CJK>
       (?$(Cu3(B . "0x918B") ; <CJK>
       (?$(Cu4(B . "0x91AE") ; <CJK>
       (?$(Cu5(B . "0x4FC3") ; <CJK>
       (?$(Cu6(B . "0x56D1") ; <CJK>
       (?$(Cu7(B . "0x71ED") ; <CJK>
       (?$(Cu8(B . "0x77D7") ; <CJK>
       (?$(Cu9(B . "0x8700") ; <CJK>
       (?$(Cu:(B . "0x89F8") ; <CJK>
       (?$(Cu;(B . "0x5BF8") ; <CJK>
       (?$(Cu<(B . "0x5FD6") ; <CJK>
       (?$(Cu=(B . "0x6751") ; <CJK>
       (?$(Cu>(B . "0x90A8") ; <CJK>
       (?$(Cu?(B . "0x53E2") ; <CJK>
       (?$(Cu@(B . "0x585A") ; <CJK>
       (?$(CuA(B . "0x5BF5") ; <CJK>
       (?$(CuB(B . "0x60A4") ; <CJK>
       (?$(CuC(B . "0x6181") ; <CJK>
       (?$(CuD(B . "0x6460") ; <CJK>
       (?$(CuE(B . "0x7E3D") ; <CJK>
       (?$(CuF(B . "0x8070") ; <CJK>
       (?$(CuG(B . "0x8525") ; <CJK>
       (?$(CuH(B . "0x9283") ; <CJK>
       (?$(CuI(B . "0x64AE") ; <CJK>
       (?$(CuJ(B . "0x50AC") ; <CJK>
       (?$(CuK(B . "0x5D14") ; <CJK>
       (?$(CuL(B . "0x6700") ; <CJK>
       (?$(CuM(B . "0x589C") ; <CJK>
       (?$(CuN(B . "0x62BD") ; <CJK>
       (?$(CuO(B . "0x63A8") ; <CJK>
       (?$(CuP(B . "0x690E") ; <CJK>
       (?$(CuQ(B . "0x6978") ; <CJK>
       (?$(CuR(B . "0x6A1E") ; <CJK>
       (?$(CuS(B . "0x6E6B") ; <CJK>
       (?$(CuT(B . "0x76BA") ; <CJK>
       (?$(CuU(B . "0x79CB") ; <CJK>
       (?$(CuV(B . "0x82BB") ; <CJK>
       (?$(CuW(B . "0x8429") ; <CJK>
       (?$(CuX(B . "0x8ACF") ; <CJK>
       (?$(CuY(B . "0x8DA8") ; <CJK>
       (?$(CuZ(B . "0x8FFD") ; <CJK>
       (?$(Cu[(B . "0x9112") ; <CJK>
       (?$(Cu\(B . "0x914B") ; <CJK>
       (?$(Cu](B . "0x919C") ; <CJK>
       (?$(Cu^(B . "0x9310") ; <CJK>
       (?$(Cu_(B . "0x9318") ; <CJK>
       (?$(Cu`(B . "0x939A") ; <CJK>
       (?$(Cua(B . "0x96DB") ; <CJK>
       (?$(Cub(B . "0x9A36") ; <CJK>
       (?$(Cuc(B . "0x9C0D") ; <CJK>
       (?$(Cud(B . "0x4E11") ; <CJK>
       (?$(Cue(B . "0x755C") ; <CJK>
       (?$(Cuf(B . "0x795D") ; <CJK>
       (?$(Cug(B . "0x7AFA") ; <CJK>
       (?$(Cuh(B . "0x7B51") ; <CJK>
       (?$(Cui(B . "0x7BC9") ; <CJK>
       (?$(Cuj(B . "0x7E2E") ; <CJK>
       (?$(Cuk(B . "0x84C4") ; <CJK>
       (?$(Cul(B . "0x8E59") ; <CJK>
       (?$(Cum(B . "0x8E74") ; <CJK>
       (?$(Cun(B . "0x8EF8") ; <CJK>
       (?$(Cuo(B . "0x9010") ; <CJK>
       (?$(Cup(B . "0x6625") ; <CJK>
       (?$(Cuq(B . "0x693F") ; <CJK>
       (?$(Cur(B . "0x7443") ; <CJK>
       (?$(Cus(B . "0x51FA") ; <CJK>
       (?$(Cut(B . "0x672E") ; <CJK>
       (?$(Cuu(B . "0x9EDC") ; <CJK>
       (?$(Cuv(B . "0x5145") ; <CJK>
       (?$(Cuw(B . "0x5FE0") ; <CJK>
       (?$(Cux(B . "0x6C96") ; <CJK>
       (?$(Cuy(B . "0x87F2") ; <CJK>
       (?$(Cuz(B . "0x885D") ; <CJK>
       (?$(Cu{(B . "0x8877") ; <CJK>
       (?$(Cu|(B . "0x60B4") ; <CJK>
       (?$(Cu}(B . "0x81B5") ; <CJK>
       (?$(Cu~(B . "0x8403") ; <CJK>
       (?$(Cv!(B . "0x8D05") ; <CJK>
       (?$(Cv"(B . "0x53D6") ; <CJK>
       (?$(Cv#(B . "0x5439") ; <CJK>
       (?$(Cv$(B . "0x5634") ; <CJK>
       (?$(Cv%(B . "0x5A36") ; <CJK>
       (?$(Cv&(B . "0x5C31") ; <CJK>
       (?$(Cv'(B . "0x708A") ; <CJK>
       (?$(Cv((B . "0x7FE0") ; <CJK>
       (?$(Cv)(B . "0x805A") ; <CJK>
       (?$(Cv*(B . "0x8106") ; <CJK>
       (?$(Cv+(B . "0x81ED") ; <CJK>
       (?$(Cv,(B . "0x8DA3") ; <CJK>
       (?$(Cv-(B . "0x9189") ; <CJK>
       (?$(Cv.(B . "0x9A5F") ; <CJK>
       (?$(Cv/(B . "0x9DF2") ; <CJK>
       (?$(Cv0(B . "0x5074") ; <CJK>
       (?$(Cv1(B . "0x4EC4") ; <CJK>
       (?$(Cv2(B . "0x53A0") ; <CJK>
       (?$(Cv3(B . "0x60FB") ; <CJK>
       (?$(Cv4(B . "0x6E2C") ; <CJK>
       (?$(Cv5(B . "0x5C64") ; <CJK>
       (?$(Cv6(B . "0x4F88") ; <CJK>
       (?$(Cv7(B . "0x5024") ; <CJK>
       (?$(Cv8(B . "0x55E4") ; <CJK>
       (?$(Cv9(B . "0x5CD9") ; <CJK>
       (?$(Cv:(B . "0x5E5F") ; <CJK>
       (?$(Cv;(B . "0x6065") ; <CJK>
       (?$(Cv<(B . "0x6894") ; <CJK>
       (?$(Cv=(B . "0x6CBB") ; <CJK>
       (?$(Cv>(B . "0x6DC4") ; <CJK>
       (?$(Cv?(B . "0x71BE") ; <CJK>
       (?$(Cv@(B . "0x75D4") ; <CJK>
       (?$(CvA(B . "0x75F4") ; <CJK>
       (?$(CvB(B . "0x7661") ; <CJK>
       (?$(CvC(B . "0x7A1A") ; <CJK>
       (?$(CvD(B . "0x7A49") ; <CJK>
       (?$(CvE(B . "0x7DC7") ; <CJK>
       (?$(CvF(B . "0x7DFB") ; <CJK>
       (?$(CvG(B . "0x7F6E") ; <CJK>
       (?$(CvH(B . "0x81F4") ; <CJK>
       (?$(CvI(B . "0x86A9") ; <CJK>
       (?$(CvJ(B . "0x8F1C") ; <CJK>
       (?$(CvK(B . "0x96C9") ; <CJK>
       (?$(CvL(B . "0x99B3") ; <CJK>
       (?$(CvM(B . "0x9F52") ; <CJK>
       (?$(CvN(B . "0x5247") ; <CJK>
       (?$(CvO(B . "0x52C5") ; <CJK>
       (?$(CvP(B . "0x98ED") ; <CJK>
       (?$(CvQ(B . "0x89AA") ; <CJK>
       (?$(CvR(B . "0x4E03") ; <CJK>
       (?$(CvS(B . "0x67D2") ; <CJK>
       (?$(CvT(B . "0x6F06") ; <CJK>
       (?$(CvU(B . "0x4FB5") ; <CJK>
       (?$(CvV(B . "0x5BE2") ; <CJK>
       (?$(CvW(B . "0x6795") ; <CJK>
       (?$(CvX(B . "0x6C88") ; <CJK>
       (?$(CvY(B . "0x6D78") ; <CJK>
       (?$(CvZ(B . "0x741B") ; <CJK>
       (?$(Cv[(B . "0x7827") ; <CJK>
       (?$(Cv\(B . "0x91DD") ; <CJK>
       (?$(Cv](B . "0x937C") ; <CJK>
       (?$(Cv^(B . "0x87C4") ; <CJK>
       (?$(Cv_(B . "0x79E4") ; <CJK>
       (?$(Cv`(B . "0x7A31") ; <CJK>
       (?$(Cva(B . "0x5FEB") ; <CJK>
       (?$(Cvb(B . "0x4ED6") ; <CJK>
       (?$(Cvc(B . "0x54A4") ; <CJK>
       (?$(Cvd(B . "0x553E") ; <CJK>
       (?$(Cve(B . "0x58AE") ; <CJK>
       (?$(Cvf(B . "0x59A5") ; <CJK>
       (?$(Cvg(B . "0x60F0") ; <CJK>
       (?$(Cvh(B . "0x6253") ; <CJK>
       (?$(Cvi(B . "0x62D6") ; <CJK>
       (?$(Cvj(B . "0x6736") ; <CJK>
       (?$(Cvk(B . "0x6955") ; <CJK>
       (?$(Cvl(B . "0x8235") ; <CJK>
       (?$(Cvm(B . "0x9640") ; <CJK>
       (?$(Cvn(B . "0x99B1") ; <CJK>
       (?$(Cvo(B . "0x99DD") ; <CJK>
       (?$(Cvp(B . "0x502C") ; <CJK>
       (?$(Cvq(B . "0x5353") ; <CJK>
       (?$(Cvr(B . "0x5544") ; <CJK>
       (?$(Cvs(B . "0x577C") ; <CJK>
       (?$(Cvt(B . "0xFA01") ; <CJK>
       (?$(Cvu(B . "0x6258") ; <CJK>
       (?$(Cvv(B . "0xFA02") ; <CJK>
       (?$(Cvw(B . "0x64E2") ; <CJK>
       (?$(Cvx(B . "0x666B") ; <CJK>
       (?$(Cvy(B . "0x67DD") ; <CJK>
       (?$(Cvz(B . "0x6FC1") ; <CJK>
       (?$(Cv{(B . "0x6FEF") ; <CJK>
       (?$(Cv|(B . "0x7422") ; <CJK>
       (?$(Cv}(B . "0x7438") ; <CJK>
       (?$(Cv~(B . "0x8A17") ; <CJK>
       (?$(Cw!(B . "0x9438") ; <CJK>
       (?$(Cw"(B . "0x5451") ; <CJK>
       (?$(Cw#(B . "0x5606") ; <CJK>
       (?$(Cw$(B . "0x5766") ; <CJK>
       (?$(Cw%(B . "0x5F48") ; <CJK>
       (?$(Cw&(B . "0x619A") ; <CJK>
       (?$(Cw'(B . "0x6B4E") ; <CJK>
       (?$(Cw((B . "0x7058") ; <CJK>
       (?$(Cw)(B . "0x70AD") ; <CJK>
       (?$(Cw*(B . "0x7DBB") ; <CJK>
       (?$(Cw+(B . "0x8A95") ; <CJK>
       (?$(Cw,(B . "0x596A") ; <CJK>
       (?$(Cw-(B . "0x812B") ; <CJK>
       (?$(Cw.(B . "0x63A2") ; <CJK>
       (?$(Cw/(B . "0x7708") ; <CJK>
       (?$(Cw0(B . "0x803D") ; <CJK>
       (?$(Cw1(B . "0x8CAA") ; <CJK>
       (?$(Cw2(B . "0x5854") ; <CJK>
       (?$(Cw3(B . "0x642D") ; <CJK>
       (?$(Cw4(B . "0x69BB") ; <CJK>
       (?$(Cw5(B . "0x5B95") ; <CJK>
       (?$(Cw6(B . "0x5E11") ; <CJK>
       (?$(Cw7(B . "0x6E6F") ; <CJK>
       (?$(Cw8(B . "0xFA03") ; <CJK>
       (?$(Cw9(B . "0x8569") ; <CJK>
       (?$(Cw:(B . "0x514C") ; <CJK>
       (?$(Cw;(B . "0x53F0") ; <CJK>
       (?$(Cw<(B . "0x592A") ; <CJK>
       (?$(Cw=(B . "0x6020") ; <CJK>
       (?$(Cw>(B . "0x614B") ; <CJK>
       (?$(Cw?(B . "0x6B86") ; <CJK>
       (?$(Cw@(B . "0x6C70") ; <CJK>
       (?$(CwA(B . "0x6CF0") ; <CJK>
       (?$(CwB(B . "0x7B1E") ; <CJK>
       (?$(CwC(B . "0x80CE") ; <CJK>
       (?$(CwD(B . "0x82D4") ; <CJK>
       (?$(CwE(B . "0x8DC6") ; <CJK>
       (?$(CwF(B . "0x90B0") ; <CJK>
       (?$(CwG(B . "0x98B1") ; <CJK>
       (?$(CwH(B . "0xFA04") ; <CJK>
       (?$(CwI(B . "0x64C7") ; <CJK>
       (?$(CwJ(B . "0x6FA4") ; <CJK>
       (?$(CwK(B . "0x6491") ; <CJK>
       (?$(CwL(B . "0x6504") ; <CJK>
       (?$(CwM(B . "0x514E") ; <CJK>
       (?$(CwN(B . "0x5410") ; <CJK>
       (?$(CwO(B . "0x571F") ; <CJK>
       (?$(CwP(B . "0x8A0E") ; <CJK>
       (?$(CwQ(B . "0x615F") ; <CJK>
       (?$(CwR(B . "0x6876") ; <CJK>
       (?$(CwS(B . "0xFA05") ; <CJK>
       (?$(CwT(B . "0x75DB") ; <CJK>
       (?$(CwU(B . "0x7B52") ; <CJK>
       (?$(CwV(B . "0x7D71") ; <CJK>
       (?$(CwW(B . "0x901A") ; <CJK>
       (?$(CwX(B . "0x5806") ; <CJK>
       (?$(CwY(B . "0x69CC") ; <CJK>
       (?$(CwZ(B . "0x817F") ; <CJK>
       (?$(Cw[(B . "0x892A") ; <CJK>
       (?$(Cw\(B . "0x9000") ; <CJK>
       (?$(Cw](B . "0x9839") ; <CJK>
       (?$(Cw^(B . "0x5078") ; <CJK>
       (?$(Cw_(B . "0x5957") ; <CJK>
       (?$(Cw`(B . "0x59AC") ; <CJK>
       (?$(Cwa(B . "0x6295") ; <CJK>
       (?$(Cwb(B . "0x900F") ; <CJK>
       (?$(Cwc(B . "0x9B2A") ; <CJK>
       (?$(Cwd(B . "0x615D") ; <CJK>
       (?$(Cwe(B . "0x7279") ; <CJK>
       (?$(Cwf(B . "0x95D6") ; <CJK>
       (?$(Cwg(B . "0x5761") ; <CJK>
       (?$(Cwh(B . "0x5A46") ; <CJK>
       (?$(Cwi(B . "0x5DF4") ; <CJK>
       (?$(Cwj(B . "0x628A") ; <CJK>
       (?$(Cwk(B . "0x64AD") ; <CJK>
       (?$(Cwl(B . "0x64FA") ; <CJK>
       (?$(Cwm(B . "0x6777") ; <CJK>
       (?$(Cwn(B . "0x6CE2") ; <CJK>
       (?$(Cwo(B . "0x6D3E") ; <CJK>
       (?$(Cwp(B . "0x722C") ; <CJK>
       (?$(Cwq(B . "0x7436") ; <CJK>
       (?$(Cwr(B . "0x7834") ; <CJK>
       (?$(Cws(B . "0x7F77") ; <CJK>
       (?$(Cwt(B . "0x82AD") ; <CJK>
       (?$(Cwu(B . "0x8DDB") ; <CJK>
       (?$(Cwv(B . "0x9817") ; <CJK>
       (?$(Cww(B . "0x5224") ; <CJK>
       (?$(Cwx(B . "0x5742") ; <CJK>
       (?$(Cwy(B . "0x677F") ; <CJK>
       (?$(Cwz(B . "0x7248") ; <CJK>
       (?$(Cw{(B . "0x74E3") ; <CJK>
       (?$(Cw|(B . "0x8CA9") ; <CJK>
       (?$(Cw}(B . "0x8FA6") ; <CJK>
       (?$(Cw~(B . "0x9211") ; <CJK>
       (?$(Cx!(B . "0x962A") ; <CJK>
       (?$(Cx"(B . "0x516B") ; <CJK>
       (?$(Cx#(B . "0x53ED") ; <CJK>
       (?$(Cx$(B . "0x634C") ; <CJK>
       (?$(Cx%(B . "0x4F69") ; <CJK>
       (?$(Cx&(B . "0x5504") ; <CJK>
       (?$(Cx'(B . "0x6096") ; <CJK>
       (?$(Cx((B . "0x6557") ; <CJK>
       (?$(Cx)(B . "0x6C9B") ; <CJK>
       (?$(Cx*(B . "0x6D7F") ; <CJK>
       (?$(Cx+(B . "0x724C") ; <CJK>
       (?$(Cx,(B . "0x72FD") ; <CJK>
       (?$(Cx-(B . "0x7A17") ; <CJK>
       (?$(Cx.(B . "0x8987") ; <CJK>
       (?$(Cx/(B . "0x8C9D") ; <CJK>
       (?$(Cx0(B . "0x5F6D") ; <CJK>
       (?$(Cx1(B . "0x6F8E") ; <CJK>
       (?$(Cx2(B . "0x70F9") ; <CJK>
       (?$(Cx3(B . "0x81A8") ; <CJK>
       (?$(Cx4(B . "0x610E") ; <CJK>
       (?$(Cx5(B . "0x4FBF") ; <CJK>
       (?$(Cx6(B . "0x504F") ; <CJK>
       (?$(Cx7(B . "0x6241") ; <CJK>
       (?$(Cx8(B . "0x7247") ; <CJK>
       (?$(Cx9(B . "0x7BC7") ; <CJK>
       (?$(Cx:(B . "0x7DE8") ; <CJK>
       (?$(Cx;(B . "0x7FE9") ; <CJK>
       (?$(Cx<(B . "0x904D") ; <CJK>
       (?$(Cx=(B . "0x97AD") ; <CJK>
       (?$(Cx>(B . "0x9A19") ; <CJK>
       (?$(Cx?(B . "0x8CB6") ; <CJK>
       (?$(Cx@(B . "0x576A") ; <CJK>
       (?$(CxA(B . "0x5E73") ; <CJK>
       (?$(CxB(B . "0x67B0") ; <CJK>
       (?$(CxC(B . "0x840D") ; <CJK>
       (?$(CxD(B . "0x8A55") ; <CJK>
       (?$(CxE(B . "0x5420") ; <CJK>
       (?$(CxF(B . "0x5B16") ; <CJK>
       (?$(CxG(B . "0x5E63") ; <CJK>
       (?$(CxH(B . "0x5EE2") ; <CJK>
       (?$(CxI(B . "0x5F0A") ; <CJK>
       (?$(CxJ(B . "0x6583") ; <CJK>
       (?$(CxK(B . "0x80BA") ; <CJK>
       (?$(CxL(B . "0x853D") ; <CJK>
       (?$(CxM(B . "0x9589") ; <CJK>
       (?$(CxN(B . "0x965B") ; <CJK>
       (?$(CxO(B . "0x4F48") ; <CJK>
       (?$(CxP(B . "0x5305") ; <CJK>
       (?$(CxQ(B . "0x530D") ; <CJK>
       (?$(CxR(B . "0x530F") ; <CJK>
       (?$(CxS(B . "0x5486") ; <CJK>
       (?$(CxT(B . "0x54FA") ; <CJK>
       (?$(CxU(B . "0x5703") ; <CJK>
       (?$(CxV(B . "0x5E03") ; <CJK>
       (?$(CxW(B . "0x6016") ; <CJK>
       (?$(CxX(B . "0x629B") ; <CJK>
       (?$(CxY(B . "0x62B1") ; <CJK>
       (?$(CxZ(B . "0x6355") ; <CJK>
       (?$(Cx[(B . "0xFA06") ; <CJK>
       (?$(Cx\(B . "0x6CE1") ; <CJK>
       (?$(Cx](B . "0x6D66") ; <CJK>
       (?$(Cx^(B . "0x75B1") ; <CJK>
       (?$(Cx_(B . "0x7832") ; <CJK>
       (?$(Cx`(B . "0x80DE") ; <CJK>
       (?$(Cxa(B . "0x812F") ; <CJK>
       (?$(Cxb(B . "0x82DE") ; <CJK>
       (?$(Cxc(B . "0x8461") ; <CJK>
       (?$(Cxd(B . "0x84B2") ; <CJK>
       (?$(Cxe(B . "0x888D") ; <CJK>
       (?$(Cxf(B . "0x8912") ; <CJK>
       (?$(Cxg(B . "0x900B") ; <CJK>
       (?$(Cxh(B . "0x92EA") ; <CJK>
       (?$(Cxi(B . "0x98FD") ; <CJK>
       (?$(Cxj(B . "0x9B91") ; <CJK>
       (?$(Cxk(B . "0x5E45") ; <CJK>
       (?$(Cxl(B . "0x66B4") ; <CJK>
       (?$(Cxm(B . "0x66DD") ; <CJK>
       (?$(Cxn(B . "0x7011") ; <CJK>
       (?$(Cxo(B . "0x7206") ; <CJK>
       (?$(Cxp(B . "0xFA07") ; <CJK>
       (?$(Cxq(B . "0x4FF5") ; <CJK>
       (?$(Cxr(B . "0x527D") ; <CJK>
       (?$(Cxs(B . "0x5F6A") ; <CJK>
       (?$(Cxt(B . "0x6153") ; <CJK>
       (?$(Cxu(B . "0x6753") ; <CJK>
       (?$(Cxv(B . "0x6A19") ; <CJK>
       (?$(Cxw(B . "0x6F02") ; <CJK>
       (?$(Cxx(B . "0x74E2") ; <CJK>
       (?$(Cxy(B . "0x7968") ; <CJK>
       (?$(Cxz(B . "0x8868") ; <CJK>
       (?$(Cx{(B . "0x8C79") ; <CJK>
       (?$(Cx|(B . "0x98C7") ; <CJK>
       (?$(Cx}(B . "0x98C4") ; <CJK>
       (?$(Cx~(B . "0x9A43") ; <CJK>
       (?$(Cy!(B . "0x54C1") ; <CJK>
       (?$(Cy"(B . "0x7A1F") ; <CJK>
       (?$(Cy#(B . "0x6953") ; <CJK>
       (?$(Cy$(B . "0x8AF7") ; <CJK>
       (?$(Cy%(B . "0x8C4A") ; <CJK>
       (?$(Cy&(B . "0x98A8") ; <CJK>
       (?$(Cy'(B . "0x99AE") ; <CJK>
       (?$(Cy((B . "0x5F7C") ; <CJK>
       (?$(Cy)(B . "0x62AB") ; <CJK>
       (?$(Cy*(B . "0x75B2") ; <CJK>
       (?$(Cy+(B . "0x76AE") ; <CJK>
       (?$(Cy,(B . "0x88AB") ; <CJK>
       (?$(Cy-(B . "0x907F") ; <CJK>
       (?$(Cy.(B . "0x9642") ; <CJK>
       (?$(Cy/(B . "0x5339") ; <CJK>
       (?$(Cy0(B . "0x5F3C") ; <CJK>
       (?$(Cy1(B . "0x5FC5") ; <CJK>
       (?$(Cy2(B . "0x6CCC") ; <CJK>
       (?$(Cy3(B . "0x73CC") ; <CJK>
       (?$(Cy4(B . "0x7562") ; <CJK>
       (?$(Cy5(B . "0x758B") ; <CJK>
       (?$(Cy6(B . "0x7B46") ; <CJK>
       (?$(Cy7(B . "0x82FE") ; <CJK>
       (?$(Cy8(B . "0x999D") ; <CJK>
       (?$(Cy9(B . "0x4E4F") ; <CJK>
       (?$(Cy:(B . "0x903C") ; <CJK>
       (?$(Cy;(B . "0x4E0B") ; <CJK>
       (?$(Cy<(B . "0x4F55") ; <CJK>
       (?$(Cy=(B . "0x53A6") ; <CJK>
       (?$(Cy>(B . "0x590F") ; <CJK>
       (?$(Cy?(B . "0x5EC8") ; <CJK>
       (?$(Cy@(B . "0x6630") ; <CJK>
       (?$(CyA(B . "0x6CB3") ; <CJK>
       (?$(CyB(B . "0x7455") ; <CJK>
       (?$(CyC(B . "0x8377") ; <CJK>
       (?$(CyD(B . "0x8766") ; <CJK>
       (?$(CyE(B . "0x8CC0") ; <CJK>
       (?$(CyF(B . "0x9050") ; <CJK>
       (?$(CyG(B . "0x971E") ; <CJK>
       (?$(CyH(B . "0x9C15") ; <CJK>
       (?$(CyI(B . "0x58D1") ; <CJK>
       (?$(CyJ(B . "0x5B78") ; <CJK>
       (?$(CyK(B . "0x8650") ; <CJK>
       (?$(CyL(B . "0x8B14") ; <CJK>
       (?$(CyM(B . "0x9DB4") ; <CJK>
       (?$(CyN(B . "0x5BD2") ; <CJK>
       (?$(CyO(B . "0x6068") ; <CJK>
       (?$(CyP(B . "0x608D") ; <CJK>
       (?$(CyQ(B . "0x65F1") ; <CJK>
       (?$(CyR(B . "0x6C57") ; <CJK>
       (?$(CyS(B . "0x6F22") ; <CJK>
       (?$(CyT(B . "0x6FA3") ; <CJK>
       (?$(CyU(B . "0x701A") ; <CJK>
       (?$(CyV(B . "0x7F55") ; <CJK>
       (?$(CyW(B . "0x7FF0") ; <CJK>
       (?$(CyX(B . "0x9591") ; <CJK>
       (?$(CyY(B . "0x9592") ; <CJK>
       (?$(CyZ(B . "0x9650") ; <CJK>
       (?$(Cy[(B . "0x97D3") ; <CJK>
       (?$(Cy\(B . "0x5272") ; <CJK>
       (?$(Cy](B . "0x8F44") ; <CJK>
       (?$(Cy^(B . "0x51FD") ; <CJK>
       (?$(Cy_(B . "0x542B") ; <CJK>
       (?$(Cy`(B . "0x54B8") ; <CJK>
       (?$(Cya(B . "0x5563") ; <CJK>
       (?$(Cyb(B . "0x558A") ; <CJK>
       (?$(Cyc(B . "0x6ABB") ; <CJK>
       (?$(Cyd(B . "0x6DB5") ; <CJK>
       (?$(Cye(B . "0x7DD8") ; <CJK>
       (?$(Cyf(B . "0x8266") ; <CJK>
       (?$(Cyg(B . "0x929C") ; <CJK>
       (?$(Cyh(B . "0x9677") ; <CJK>
       (?$(Cyi(B . "0x9E79") ; <CJK>
       (?$(Cyj(B . "0x5408") ; <CJK>
       (?$(Cyk(B . "0x54C8") ; <CJK>
       (?$(Cyl(B . "0x76D2") ; <CJK>
       (?$(Cym(B . "0x86E4") ; <CJK>
       (?$(Cyn(B . "0x95A4") ; <CJK>
       (?$(Cyo(B . "0x95D4") ; <CJK>
       (?$(Cyp(B . "0x965C") ; <CJK>
       (?$(Cyq(B . "0x4EA2") ; <CJK>
       (?$(Cyr(B . "0x4F09") ; <CJK>
       (?$(Cys(B . "0x59EE") ; <CJK>
       (?$(Cyt(B . "0x5AE6") ; <CJK>
       (?$(Cyu(B . "0x5DF7") ; <CJK>
       (?$(Cyv(B . "0x6052") ; <CJK>
       (?$(Cyw(B . "0x6297") ; <CJK>
       (?$(Cyx(B . "0x676D") ; <CJK>
       (?$(Cyy(B . "0x6841") ; <CJK>
       (?$(Cyz(B . "0x6C86") ; <CJK>
       (?$(Cy{(B . "0x6E2F") ; <CJK>
       (?$(Cy|(B . "0x7F38") ; <CJK>
       (?$(Cy}(B . "0x809B") ; <CJK>
       (?$(Cy~(B . "0x822A") ; <CJK>
       (?$(Cz!(B . "0xFA08") ; <CJK>
       (?$(Cz"(B . "0xFA09") ; <CJK>
       (?$(Cz#(B . "0x9805") ; <CJK>
       (?$(Cz$(B . "0x4EA5") ; <CJK>
       (?$(Cz%(B . "0x5055") ; <CJK>
       (?$(Cz&(B . "0x54B3") ; <CJK>
       (?$(Cz'(B . "0x5793") ; <CJK>
       (?$(Cz((B . "0x595A") ; <CJK>
       (?$(Cz)(B . "0x5B69") ; <CJK>
       (?$(Cz*(B . "0x5BB3") ; <CJK>
       (?$(Cz+(B . "0x61C8") ; <CJK>
       (?$(Cz,(B . "0x6977") ; <CJK>
       (?$(Cz-(B . "0x6D77") ; <CJK>
       (?$(Cz.(B . "0x7023") ; <CJK>
       (?$(Cz/(B . "0x87F9") ; <CJK>
       (?$(Cz0(B . "0x89E3") ; <CJK>
       (?$(Cz1(B . "0x8A72") ; <CJK>
       (?$(Cz2(B . "0x8AE7") ; <CJK>
       (?$(Cz3(B . "0x9082") ; <CJK>
       (?$(Cz4(B . "0x99ED") ; <CJK>
       (?$(Cz5(B . "0x9AB8") ; <CJK>
       (?$(Cz6(B . "0x52BE") ; <CJK>
       (?$(Cz7(B . "0x6838") ; <CJK>
       (?$(Cz8(B . "0x5016") ; <CJK>
       (?$(Cz9(B . "0x5E78") ; <CJK>
       (?$(Cz:(B . "0x674F") ; <CJK>
       (?$(Cz;(B . "0x8347") ; <CJK>
       (?$(Cz<(B . "0x884C") ; <CJK>
       (?$(Cz=(B . "0x4EAB") ; <CJK>
       (?$(Cz>(B . "0x5411") ; <CJK>
       (?$(Cz?(B . "0x56AE") ; <CJK>
       (?$(Cz@(B . "0x73E6") ; <CJK>
       (?$(CzA(B . "0x9115") ; <CJK>
       (?$(CzB(B . "0x97FF") ; <CJK>
       (?$(CzC(B . "0x9909") ; <CJK>
       (?$(CzD(B . "0x9957") ; <CJK>
       (?$(CzE(B . "0x9999") ; <CJK>
       (?$(CzF(B . "0x5653") ; <CJK>
       (?$(CzG(B . "0x589F") ; <CJK>
       (?$(CzH(B . "0x865B") ; <CJK>
       (?$(CzI(B . "0x8A31") ; <CJK>
       (?$(CzJ(B . "0x61B2") ; <CJK>
       (?$(CzK(B . "0x6AF6") ; <CJK>
       (?$(CzL(B . "0x737B") ; <CJK>
       (?$(CzM(B . "0x8ED2") ; <CJK>
       (?$(CzN(B . "0x6B47") ; <CJK>
       (?$(CzO(B . "0x96AA") ; <CJK>
       (?$(CzP(B . "0x9A57") ; <CJK>
       (?$(CzQ(B . "0x5955") ; <CJK>
       (?$(CzR(B . "0x7200") ; <CJK>
       (?$(CzS(B . "0x8D6B") ; <CJK>
       (?$(CzT(B . "0x9769") ; <CJK>
       (?$(CzU(B . "0x4FD4") ; <CJK>
       (?$(CzV(B . "0x5CF4") ; <CJK>
       (?$(CzW(B . "0x5F26") ; <CJK>
       (?$(CzX(B . "0x61F8") ; <CJK>
       (?$(CzY(B . "0x665B") ; <CJK>
       (?$(CzZ(B . "0x6CEB") ; <CJK>
       (?$(Cz[(B . "0x70AB") ; <CJK>
       (?$(Cz\(B . "0x7384") ; <CJK>
       (?$(Cz](B . "0x73B9") ; <CJK>
       (?$(Cz^(B . "0x73FE") ; <CJK>
       (?$(Cz_(B . "0x7729") ; <CJK>
       (?$(Cz`(B . "0x774D") ; <CJK>
       (?$(Cza(B . "0x7D43") ; <CJK>
       (?$(Czb(B . "0x7D62") ; <CJK>
       (?$(Czc(B . "0x7E23") ; <CJK>
       (?$(Czd(B . "0x8237") ; <CJK>
       (?$(Cze(B . "0x8852") ; <CJK>
       (?$(Czf(B . "0xFA0A") ; <CJK>
       (?$(Czg(B . "0x8CE2") ; <CJK>
       (?$(Czh(B . "0x9249") ; <CJK>
       (?$(Czi(B . "0x986F") ; <CJK>
       (?$(Czj(B . "0x5B51") ; <CJK>
       (?$(Czk(B . "0x7A74") ; <CJK>
       (?$(Czl(B . "0x8840") ; <CJK>
       (?$(Czm(B . "0x9801") ; <CJK>
       (?$(Czn(B . "0x5ACC") ; <CJK>
       (?$(Czo(B . "0x4FE0") ; <CJK>
       (?$(Czp(B . "0x5354") ; <CJK>
       (?$(Czq(B . "0x593E") ; <CJK>
       (?$(Czr(B . "0x5CFD") ; <CJK>
       (?$(Czs(B . "0x633E") ; <CJK>
       (?$(Czt(B . "0x6D79") ; <CJK>
       (?$(Czu(B . "0x72F9") ; <CJK>
       (?$(Czv(B . "0x8105") ; <CJK>
       (?$(Czw(B . "0x8107") ; <CJK>
       (?$(Czx(B . "0x83A2") ; <CJK>
       (?$(Czy(B . "0x92CF") ; <CJK>
       (?$(Czz(B . "0x9830") ; <CJK>
       (?$(Cz{(B . "0x4EA8") ; <CJK>
       (?$(Cz|(B . "0x5144") ; <CJK>
       (?$(Cz}(B . "0x5211") ; <CJK>
       (?$(Cz~(B . "0x578B") ; <CJK>
       (?$(C{!(B . "0x5F62") ; <CJK>
       (?$(C{"(B . "0x6CC2") ; <CJK>
       (?$(C{#(B . "0x6ECE") ; <CJK>
       (?$(C{$(B . "0x7005") ; <CJK>
       (?$(C{%(B . "0x7050") ; <CJK>
       (?$(C{&(B . "0x70AF") ; <CJK>
       (?$(C{'(B . "0x7192") ; <CJK>
       (?$(C{((B . "0x73E9") ; <CJK>
       (?$(C{)(B . "0x7469") ; <CJK>
       (?$(C{*(B . "0x834A") ; <CJK>
       (?$(C{+(B . "0x87A2") ; <CJK>
       (?$(C{,(B . "0x8861") ; <CJK>
       (?$(C{-(B . "0x9008") ; <CJK>
       (?$(C{.(B . "0x90A2") ; <CJK>
       (?$(C{/(B . "0x93A3") ; <CJK>
       (?$(C{0(B . "0x99A8") ; <CJK>
       (?$(C{1(B . "0x516E") ; <CJK>
       (?$(C{2(B . "0x5F57") ; <CJK>
       (?$(C{3(B . "0x60E0") ; <CJK>
       (?$(C{4(B . "0x6167") ; <CJK>
       (?$(C{5(B . "0x66B3") ; <CJK>
       (?$(C{6(B . "0x8559") ; <CJK>
       (?$(C{7(B . "0x8E4A") ; <CJK>
       (?$(C{8(B . "0x91AF") ; <CJK>
       (?$(C{9(B . "0x978B") ; <CJK>
       (?$(C{:(B . "0x4E4E") ; <CJK>
       (?$(C{;(B . "0x4E92") ; <CJK>
       (?$(C{<(B . "0x547C") ; <CJK>
       (?$(C{=(B . "0x58D5") ; <CJK>
       (?$(C{>(B . "0x58FA") ; <CJK>
       (?$(C{?(B . "0x597D") ; <CJK>
       (?$(C{@(B . "0x5CB5") ; <CJK>
       (?$(C{A(B . "0x5F27") ; <CJK>
       (?$(C{B(B . "0x6236") ; <CJK>
       (?$(C{C(B . "0x6248") ; <CJK>
       (?$(C{D(B . "0x660A") ; <CJK>
       (?$(C{E(B . "0x6667") ; <CJK>
       (?$(C{F(B . "0x6BEB") ; <CJK>
       (?$(C{G(B . "0x6D69") ; <CJK>
       (?$(C{H(B . "0x6DCF") ; <CJK>
       (?$(C{I(B . "0x6E56") ; <CJK>
       (?$(C{J(B . "0x6EF8") ; <CJK>
       (?$(C{K(B . "0x6F94") ; <CJK>
       (?$(C{L(B . "0x6FE0") ; <CJK>
       (?$(C{M(B . "0x6FE9") ; <CJK>
       (?$(C{N(B . "0x705D") ; <CJK>
       (?$(C{O(B . "0x72D0") ; <CJK>
       (?$(C{P(B . "0x7425") ; <CJK>
       (?$(C{Q(B . "0x745A") ; <CJK>
       (?$(C{R(B . "0x74E0") ; <CJK>
       (?$(C{S(B . "0x7693") ; <CJK>
       (?$(C{T(B . "0x795C") ; <CJK>
       (?$(C{U(B . "0x7CCA") ; <CJK>
       (?$(C{V(B . "0x7E1E") ; <CJK>
       (?$(C{W(B . "0x80E1") ; <CJK>
       (?$(C{X(B . "0x82A6") ; <CJK>
       (?$(C{Y(B . "0x846B") ; <CJK>
       (?$(C{Z(B . "0x84BF") ; <CJK>
       (?$(C{[(B . "0x864E") ; <CJK>
       (?$(C{\(B . "0x865F") ; <CJK>
       (?$(C{](B . "0x8774") ; <CJK>
       (?$(C{^(B . "0x8B77") ; <CJK>
       (?$(C{_(B . "0x8C6A") ; <CJK>
       (?$(C{`(B . "0x93AC") ; <CJK>
       (?$(C{a(B . "0x9800") ; <CJK>
       (?$(C{b(B . "0x9865") ; <CJK>
       (?$(C{c(B . "0x60D1") ; <CJK>
       (?$(C{d(B . "0x6216") ; <CJK>
       (?$(C{e(B . "0x9177") ; <CJK>
       (?$(C{f(B . "0x5A5A") ; <CJK>
       (?$(C{g(B . "0x660F") ; <CJK>
       (?$(C{h(B . "0x6DF7") ; <CJK>
       (?$(C{i(B . "0x6E3E") ; <CJK>
       (?$(C{j(B . "0x743F") ; <CJK>
       (?$(C{k(B . "0x9B42") ; <CJK>
       (?$(C{l(B . "0x5FFD") ; <CJK>
       (?$(C{m(B . "0x60DA") ; <CJK>
       (?$(C{n(B . "0x7B0F") ; <CJK>
       (?$(C{o(B . "0x54C4") ; <CJK>
       (?$(C{p(B . "0x5F18") ; <CJK>
       (?$(C{q(B . "0x6C5E") ; <CJK>
       (?$(C{r(B . "0x6CD3") ; <CJK>
       (?$(C{s(B . "0x6D2A") ; <CJK>
       (?$(C{t(B . "0x70D8") ; <CJK>
       (?$(C{u(B . "0x7D05") ; <CJK>
       (?$(C{v(B . "0x8679") ; <CJK>
       (?$(C{w(B . "0x8A0C") ; <CJK>
       (?$(C{x(B . "0x9D3B") ; <CJK>
       (?$(C{y(B . "0x5316") ; <CJK>
       (?$(C{z(B . "0x548C") ; <CJK>
       (?$(C{{(B . "0x5B05") ; <CJK>
       (?$(C{|(B . "0x6A3A") ; <CJK>
       (?$(C{}(B . "0x706B") ; <CJK>
       (?$(C{~(B . "0x7575") ; <CJK>
       (?$(C|!(B . "0x798D") ; <CJK>
       (?$(C|"(B . "0x79BE") ; <CJK>
       (?$(C|#(B . "0x82B1") ; <CJK>
       (?$(C|$(B . "0x83EF") ; <CJK>
       (?$(C|%(B . "0x8A71") ; <CJK>
       (?$(C|&(B . "0x8B41") ; <CJK>
       (?$(C|'(B . "0x8CA8") ; <CJK>
       (?$(C|((B . "0x9774") ; <CJK>
       (?$(C|)(B . "0xFA0B") ; <CJK>
       (?$(C|*(B . "0x64F4") ; <CJK>
       (?$(C|+(B . "0x652B") ; <CJK>
       (?$(C|,(B . "0x78BA") ; <CJK>
       (?$(C|-(B . "0x78BB") ; <CJK>
       (?$(C|.(B . "0x7A6B") ; <CJK>
       (?$(C|/(B . "0x4E38") ; <CJK>
       (?$(C|0(B . "0x559A") ; <CJK>
       (?$(C|1(B . "0x5950") ; <CJK>
       (?$(C|2(B . "0x5BA6") ; <CJK>
       (?$(C|3(B . "0x5E7B") ; <CJK>
       (?$(C|4(B . "0x60A3") ; <CJK>
       (?$(C|5(B . "0x63DB") ; <CJK>
       (?$(C|6(B . "0x6B61") ; <CJK>
       (?$(C|7(B . "0x6665") ; <CJK>
       (?$(C|8(B . "0x6853") ; <CJK>
       (?$(C|9(B . "0x6E19") ; <CJK>
       (?$(C|:(B . "0x7165") ; <CJK>
       (?$(C|;(B . "0x74B0") ; <CJK>
       (?$(C|<(B . "0x7D08") ; <CJK>
       (?$(C|=(B . "0x9084") ; <CJK>
       (?$(C|>(B . "0x9A69") ; <CJK>
       (?$(C|?(B . "0x9C25") ; <CJK>
       (?$(C|@(B . "0x6D3B") ; <CJK>
       (?$(C|A(B . "0x6ED1") ; <CJK>
       (?$(C|B(B . "0x733E") ; <CJK>
       (?$(C|C(B . "0x8C41") ; <CJK>
       (?$(C|D(B . "0x95CA") ; <CJK>
       (?$(C|E(B . "0x51F0") ; <CJK>
       (?$(C|F(B . "0x5E4C") ; <CJK>
       (?$(C|G(B . "0x5FA8") ; <CJK>
       (?$(C|H(B . "0x604D") ; <CJK>
       (?$(C|I(B . "0x60F6") ; <CJK>
       (?$(C|J(B . "0x6130") ; <CJK>
       (?$(C|K(B . "0x614C") ; <CJK>
       (?$(C|L(B . "0x6643") ; <CJK>
       (?$(C|M(B . "0x6644") ; <CJK>
       (?$(C|N(B . "0x69A5") ; <CJK>
       (?$(C|O(B . "0x6CC1") ; <CJK>
       (?$(C|P(B . "0x6E5F") ; <CJK>
       (?$(C|Q(B . "0x6EC9") ; <CJK>
       (?$(C|R(B . "0x6F62") ; <CJK>
       (?$(C|S(B . "0x714C") ; <CJK>
       (?$(C|T(B . "0x749C") ; <CJK>
       (?$(C|U(B . "0x7687") ; <CJK>
       (?$(C|V(B . "0x7BC1") ; <CJK>
       (?$(C|W(B . "0x7C27") ; <CJK>
       (?$(C|X(B . "0x8352") ; <CJK>
       (?$(C|Y(B . "0x8757") ; <CJK>
       (?$(C|Z(B . "0x9051") ; <CJK>
       (?$(C|[(B . "0x968D") ; <CJK>
       (?$(C|\(B . "0x9EC3") ; <CJK>
       (?$(C|](B . "0x532F") ; <CJK>
       (?$(C|^(B . "0x56DE") ; <CJK>
       (?$(C|_(B . "0x5EFB") ; <CJK>
       (?$(C|`(B . "0x5F8A") ; <CJK>
       (?$(C|a(B . "0x6062") ; <CJK>
       (?$(C|b(B . "0x6094") ; <CJK>
       (?$(C|c(B . "0x61F7") ; <CJK>
       (?$(C|d(B . "0x6666") ; <CJK>
       (?$(C|e(B . "0x6703") ; <CJK>
       (?$(C|f(B . "0x6A9C") ; <CJK>
       (?$(C|g(B . "0x6DEE") ; <CJK>
       (?$(C|h(B . "0x6FAE") ; <CJK>
       (?$(C|i(B . "0x7070") ; <CJK>
       (?$(C|j(B . "0x736A") ; <CJK>
       (?$(C|k(B . "0x7E6A") ; <CJK>
       (?$(C|l(B . "0x81BE") ; <CJK>
       (?$(C|m(B . "0x8334") ; <CJK>
       (?$(C|n(B . "0x86D4") ; <CJK>
       (?$(C|o(B . "0x8AA8") ; <CJK>
       (?$(C|p(B . "0x8CC4") ; <CJK>
       (?$(C|q(B . "0x5283") ; <CJK>
       (?$(C|r(B . "0x7372") ; <CJK>
       (?$(C|s(B . "0x5B96") ; <CJK>
       (?$(C|t(B . "0x6A6B") ; <CJK>
       (?$(C|u(B . "0x9404") ; <CJK>
       (?$(C|v(B . "0x54EE") ; <CJK>
       (?$(C|w(B . "0x5686") ; <CJK>
       (?$(C|x(B . "0x5B5D") ; <CJK>
       (?$(C|y(B . "0x6548") ; <CJK>
       (?$(C|z(B . "0x6585") ; <CJK>
       (?$(C|{(B . "0x66C9") ; <CJK>
       (?$(C||(B . "0x689F") ; <CJK>
       (?$(C|}(B . "0x6D8D") ; <CJK>
       (?$(C|~(B . "0x6DC6") ; <CJK>
       (?$(C}!(B . "0x723B") ; <CJK>
       (?$(C}"(B . "0x80B4") ; <CJK>
       (?$(C}#(B . "0x9175") ; <CJK>
       (?$(C}$(B . "0x9A4D") ; <CJK>
       (?$(C}%(B . "0x4FAF") ; <CJK>
       (?$(C}&(B . "0x5019") ; <CJK>
       (?$(C}'(B . "0x539A") ; <CJK>
       (?$(C}((B . "0x540E") ; <CJK>
       (?$(C})(B . "0x543C") ; <CJK>
       (?$(C}*(B . "0x5589") ; <CJK>
       (?$(C}+(B . "0x55C5") ; <CJK>
       (?$(C},(B . "0x5E3F") ; <CJK>
       (?$(C}-(B . "0x5F8C") ; <CJK>
       (?$(C}.(B . "0x673D") ; <CJK>
       (?$(C}/(B . "0x7166") ; <CJK>
       (?$(C}0(B . "0x73DD") ; <CJK>
       (?$(C}1(B . "0x9005") ; <CJK>
       (?$(C}2(B . "0x52DB") ; <CJK>
       (?$(C}3(B . "0x52F3") ; <CJK>
       (?$(C}4(B . "0x5864") ; <CJK>
       (?$(C}5(B . "0x58CE") ; <CJK>
       (?$(C}6(B . "0x7104") ; <CJK>
       (?$(C}7(B . "0x718F") ; <CJK>
       (?$(C}8(B . "0x71FB") ; <CJK>
       (?$(C}9(B . "0x85B0") ; <CJK>
       (?$(C}:(B . "0x8A13") ; <CJK>
       (?$(C};(B . "0x6688") ; <CJK>
       (?$(C}<(B . "0x85A8") ; <CJK>
       (?$(C}=(B . "0x55A7") ; <CJK>
       (?$(C}>(B . "0x6684") ; <CJK>
       (?$(C}?(B . "0x714A") ; <CJK>
       (?$(C}@(B . "0x8431") ; <CJK>
       (?$(C}A(B . "0x5349") ; <CJK>
       (?$(C}B(B . "0x5599") ; <CJK>
       (?$(C}C(B . "0x6BC1") ; <CJK>
       (?$(C}D(B . "0x5F59") ; <CJK>
       (?$(C}E(B . "0x5FBD") ; <CJK>
       (?$(C}F(B . "0x63EE") ; <CJK>
       (?$(C}G(B . "0x6689") ; <CJK>
       (?$(C}H(B . "0x7147") ; <CJK>
       (?$(C}I(B . "0x8AF1") ; <CJK>
       (?$(C}J(B . "0x8F1D") ; <CJK>
       (?$(C}K(B . "0x9EBE") ; <CJK>
       (?$(C}L(B . "0x4F11") ; <CJK>
       (?$(C}M(B . "0x643A") ; <CJK>
       (?$(C}N(B . "0x70CB") ; <CJK>
       (?$(C}O(B . "0x7566") ; <CJK>
       (?$(C}P(B . "0x8667") ; <CJK>
       (?$(C}Q(B . "0x6064") ; <CJK>
       (?$(C}R(B . "0x8B4E") ; <CJK>
       (?$(C}S(B . "0x9DF8") ; <CJK>
       (?$(C}T(B . "0x5147") ; <CJK>
       (?$(C}U(B . "0x51F6") ; <CJK>
       (?$(C}V(B . "0x5308") ; <CJK>
       (?$(C}W(B . "0x6D36") ; <CJK>
       (?$(C}X(B . "0x80F8") ; <CJK>
       (?$(C}Y(B . "0x9ED1") ; <CJK>
       (?$(C}Z(B . "0x6615") ; <CJK>
       (?$(C}[(B . "0x6B23") ; <CJK>
       (?$(C}\(B . "0x7098") ; <CJK>
       (?$(C}](B . "0x75D5") ; <CJK>
       (?$(C}^(B . "0x5403") ; <CJK>
       (?$(C}_(B . "0x5C79") ; <CJK>
       (?$(C}`(B . "0x7D07") ; <CJK>
       (?$(C}a(B . "0x8A16") ; <CJK>
       (?$(C}b(B . "0x6B20") ; <CJK>
       (?$(C}c(B . "0x6B3D") ; <CJK>
       (?$(C}d(B . "0x6B46") ; <CJK>
       (?$(C}e(B . "0x5438") ; <CJK>
       (?$(C}f(B . "0x6070") ; <CJK>
       (?$(C}g(B . "0x6D3D") ; <CJK>
       (?$(C}h(B . "0x7FD5") ; <CJK>
       (?$(C}i(B . "0x8208") ; <CJK>
       (?$(C}j(B . "0x50D6") ; <CJK>
       (?$(C}k(B . "0x51DE") ; <CJK>
       (?$(C}l(B . "0x559C") ; <CJK>
       (?$(C}m(B . "0x566B") ; <CJK>
       (?$(C}n(B . "0x56CD") ; <CJK>
       (?$(C}o(B . "0x59EC") ; <CJK>
       (?$(C}p(B . "0x5B09") ; <CJK>
       (?$(C}q(B . "0x5E0C") ; <CJK>
       (?$(C}r(B . "0x6199") ; <CJK>
       (?$(C}s(B . "0x6198") ; <CJK>
       (?$(C}t(B . "0x6231") ; <CJK>
       (?$(C}u(B . "0x665E") ; <CJK>
       (?$(C}v(B . "0x66E6") ; <CJK>
       (?$(C}w(B . "0x7199") ; <CJK>
       (?$(C}x(B . "0x71B9") ; <CJK>
       (?$(C}y(B . "0x71BA") ; <CJK>
       (?$(C}z(B . "0x72A7") ; <CJK>
       (?$(C}{(B . "0x79A7") ; <CJK>
       (?$(C}|(B . "0x7A00") ; <CJK>
       (?$(C}}(B . "0x7FB2") ; <CJK>
       (?$(C}~(B . "0x8A70") ; <CJK>
       ))))

(provide 'uksc5601)

;;; uksc5601.el ends here
 
