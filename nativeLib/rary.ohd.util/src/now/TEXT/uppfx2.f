C  =====================================================================
C  pgm: UPPFX2 .. Small rtn used by "UPPFIX" to concatenate strings
C
C  use:      CALL UPPFX2( str1, lnstr1, str2, lnstr2, str3, lnstr3 )
C
C   in: str1 .......... first string - CHAR*128
C   in: lnstr1 ........ last char in "str1" to be used - INT
C   in: str2 .......... second string - CHAR*32
C   in: lnstr2 ........ last char in "str2" to be concatenanted - INT
C  out: str3 .......... output concatenated string - CHAR*160
C  out: lnstr3 ........ number of chars in the output string - INT
C
C  cmt: Note, both input strings are assumed to start at position 1.
C  =====================================================================
      SUBROUTINE UPPFX2(STR1,LNSTR1,STR2,LNSTR2,STR3,LNSTR3)

      CHARACTER*128 STR1
      CHARACTER*32  STR2
      CHARACTER*160 STR3
      INTEGER       LNSTR1,LNSTR2,LNSTR3,II,JJ,KK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uppfx2.f,v $
     . $',                                                             '
     .$Id: uppfx2.f,v 1.1 1996/12/10 16:00:32 dws Exp $
     . $' /
C    ===================================================================
C

        STR3 = STR1(1:LNSTR1)

        II = LNSTR1
        IF (II .LT.   0) II = 0
        IF (II .GT. 128) II = 128
        JJ = LNSTR2
        IF (JJ .LT.   0) JJ = 0
        IF (JJ .GT.  32) JJ = 32

        KK = 0
  120   IF (KK .GE. JJ) GOTO 130
          II = II+1
          KK = KK+1
          STR3(II:II) = STR2(KK:KK)
            GOTO 120
  130   CONTINUE
        LNSTR3 = II

      RETURN
      END
