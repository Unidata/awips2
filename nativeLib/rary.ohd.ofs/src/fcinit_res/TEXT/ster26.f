C MEMBER STER26
C  (from old member FCSTER26)
C
C DESC ADDS TO A LIST OF ERRORS OCCURRING IN PIN26 ROUTINE
C
C.....................................................................
C
      SUBROUTINE STER26(NUM,IPOS)
C
C......................................................................
C
C  INCREMENTS THE NUMBER OF ERRORS IN /ERR26/ BY ONE IF THERE ARE LESS
C  THAN 50 ERRORS HELD IN COMMON BLOCK. THE 50-TH ERROR IS THE ERROR
C  SAYING THAT 50 ERRORS HAVE OCCURRED.
C.......................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      INCLUDE 'common/err26'
      INCLUDE 'common/read26'
      INCLUDE 'common/fld26'
      DIMENSION LOC(3)
      EQUIVALENCE (LOC(1),LGENL),(LOC(2),LSPEC),(LOC(3),LRCL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ster26.f,v $
     . $',                                                             '
     .$Id: ster26.f,v 1.1 1995/09/17 18:52:44 dws Exp $
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
C  STORE THE ERROR TYPE (NUM),
C        THE LINE IT OCCURRED ON,
C        THE FIELD ON THE LINE WHERE IT HAPPENED,
C         THE POSITION IN THE STRING IT OCCURRED.
C   SET THE NAME IN THE COMMON BLOCK TO ALL BLANKS
C
      NUMERR = NUMERR + 1
      IERTYP(NUMERR) = NUM
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
