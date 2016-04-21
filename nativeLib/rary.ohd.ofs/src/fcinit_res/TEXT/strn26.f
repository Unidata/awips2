C MEMBER STRN26
C  (from old member FCSTRN26)
C
C DESC ADDS TO A LIST OF ERRORS OCCURRING IN PIN26 ROUTINE
C
C.....................................................................
C
      SUBROUTINE STRN26(NUM,IPOS,NAME,LNAME)
C
C......................................................................
C
C  INCREMENTS THE NUMBER OF ERRORS IN /ERR26/ BY ONE IF THERE ARE LESS
C  THAN 50 ERRORS HELD IN COMMON BLOCK. THE 50-TH ERROR IS THE ERROR
C  SAYING THAT 50 ERRORS HAVE OCCURRED.
C.......................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/err26'
      INCLUDE 'common/read26'
      INCLUDE 'common/fld26'
      DIMENSION LOC(3),NAME(1)
      EQUIVALENCE (LOC(1),LGENL),(LOC(2),LSPEC),(LOC(3),LRCL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/strn26.f,v $
     . $',                                                             '
     .$Id: strn26.f,v 1.1 1995/09/17 18:52:46 dws Exp $
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
C        THE POSITION IN THE STRING IT OCCURRED,
C        AND THE NAME NEEDED TO FURTHER CLARIFY THE ERROR MESSAGE
C
      NUMERR = NUMERR + 1
      IERTYP(NUMERR) = NUM
      IERLN(NUMERR) = NCARD
      IERFLD(NUMERR) = NFLD
      IERPOS(NUMERR) = IPOS
C
      NLT = LNAME
      IF (NLT.GT.3) NLT = 3
      DO 13 I=1,NLT
      IERNME(I,NUMERR) = NAME(I)
   13 CONTINUE
C
      IF (NLT.EQ.3) RETURN
C
      NEXT = NLT + 1
      DO 23 I=NEXT,3
      IERNME(I,NUMERR) = IBLNK
   23 CONTINUE
C
      RETURN
      END
