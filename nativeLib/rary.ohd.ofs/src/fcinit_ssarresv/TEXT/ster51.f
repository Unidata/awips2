C MEMBER STER51
C----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE STER51(NUMER,IPOS)
C
C DESC ADDS TO A LIST OF ERRORS OCCURRING IN PIN51 ROUTINE
C......................................................................
C
C  INCREMENTS THE NUMBER OF ERRORS IN /ERR51/ BY ONE IF THERE ARE LESS
C  THAN 50 ERRORS HELD IN COMMON BLOCK. THE 50-TH ERROR IS THE ERROR
C  SAYING THAT 50 ERRORS HAVE OCCURRED.
C.......................................................................
C
C  KUANG HSU - HRL - APRIL 1994
C................................................................
      INCLUDE 'common/err51'
      INCLUDE 'common/read51'
      INCLUDE 'common/fld51'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/ster51.f,v $
     . $',                                                             '
     .$Id: ster51.f,v 1.1 1996/03/21 14:39:54 page Exp $
     . $' /
C    ===================================================================
C
      DATA IBLNK/4H    /
C
      IF (NUMERR.LT.49) GO TO 10
C
      NUMERR = 50
      IERTYP(50) = 1000
      IERLN(50) = 0
      IERFLD(50) = 0
      IERPOS(50) = 0
      DO 7 I=1,3
      IERNME(I,50) = IBLNK
    7 CONTINUE
      RETURN
C
   10 CONTINUE
C
C  INCREMENT THE NUMBER OF ERRORS
C  STORE THE ERROR TYPE (NUMER),
C        THE LINE IT OCCURRED ON,
C        THE FIELD ON THE LINE WHERE IT HAPPENED,
C         THE POSITION IN THE STRING IT OCCURRED.
C   SET THE NAME IN THE COMMON BLOCK TO ALL BLANKS
C
      NUMERR = NUMERR + 1
      IERTYP(NUMERR) = NUMER
      IERLN(NUMERR) = NCARD
      IERFLD(NUMERR) = NFLD
      IERPOS(NUMERR) = IPOS
C
      DO 13 I=1,3
      IERNME(I,NUMERR) = IBLNK
   13 CONTINUE
C
      RETURN
      END
