C MEMBER STWN26
C  (from old member FCSTWN26)
C
C DESC ADDS TO A LIST OF WARNINGS OCCURRING IN PIN26 ROUTINE
C
C.....................................................................
C
      SUBROUTINE STWN26(NUM,IPOS)
C
C......................................................................
C
C  INCREMENTS NUMBER OF WARNINGS IN /WARN26/ BY ONE IF THERE ARE LESS
C  THAN 50 WARNINGS HELD IN COMMON BLOCK. THE 50-TH TIME WILL SET AN
C  ERROR SAYING THAT 50 WARNINGS HAVE OCCURRED.
C.......................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      INCLUDE 'common/warn26'
      INCLUDE 'common/read26'
      INCLUDE 'common/fld26'
      DIMENSION LOC(3)
      EQUIVALENCE (LOC(1),LGENL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/stwn26.f,v $
     . $',                                                             '
     .$Id: stwn26.f,v 1.1 1995/09/17 18:52:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUMWRN.LT.49) GO TO 10
C
      NUMWRN = NUMWRN + 1
      IWNTYP(50) = 1001
      IWNLN(50) = 0
      IWNFLD(50) = 0
      IWNPOS(50) = 0
      RETURN
C
   10 CONTINUE
C
C  INCREMENT THE NUMBER OF WARNINGS
C  STORE THE WARNING TYPE (NUM),
C        THE LINE IT OCCURRED ON,
C        THE FIELD ON THE LINE WHERE IT HAPPENED,
C         THE POSITION IN THE STRING IT OCCURRED.
C
      NUMWRN = NUMWRN + 1
      IWNTYP(NUMWRN) = NUM
      IWNLN(NUMWRN) = NCARD - LOC(ISECT) + 1
      IWNFLD(NUMWRN) = NFLD
      IWNPOS(NUMWRN) = IPOS
C
      RETURN
      END
