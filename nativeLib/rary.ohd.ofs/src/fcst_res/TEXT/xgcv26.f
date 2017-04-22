C MEMBER XGCV26
C  (from old member FCXGCV26)
C
C DESC DETERMINE VALUE FOR USER COMPARISON VARIABLE IN A.GT.B RELATION
C-----------------------------------------------------------------------
      SUBROUTINE XGCV26(PO,LOCCV,W,LOCOWS,VALUE,IVAL)
C-----------------------------------------------------------------------
C  SUBROUTINE TO RETURN THE USER SPECIFIED SIDE OF AN (A.EQ.B) TYPE
C  IF GROUP COMPARISON.
C-----------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - AUGUST 1983
C-----------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),W(1),LOCOWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xgcv26.f,v $
     . $',                                                             '
     .$Id: xgcv26.f,v 1.2 1996/05/07 11:28:17 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XGCV26 ***')
C
C---------------------------
C  THERE ARE THREE TYPES OF USER-SPECIFIED COMPARISON VARIABLES.
C     1) CONSTANT
C     2) ASSIGNED VALUE
C     3) SYSTEM FUNCTION
C
C  THE FIRST TWO VALUES ARE OBTAINED FROM THE PO ARRAY DIRECTLY.
C  THE LAST ONE MUST BE COMPUTED AT RUN TIME.
C
      ICTYPE = PO(LOCCV)
      GO TO (100,200,300), ICTYPE
C
C-----------------------------------
C  CONSTANT VALUE
C
  100 CONTINUE
      VALUE = PO(LOCCV+1)
      IVAL = VALUE
      GO TO 9000
C
C-----------------------------------
C  ASSIGNED VALUE (I.E. 'SET' VARAIBLE)
C
  200 CONTINUE
      VALUE = PO(LOCCV+4)
      IVAL = VALUE
      GO TO 9000
C
C----------------------------------
C  SYSTEM FUNCTION (EITHER RULE CURVE OR MAXIMUM DISCHARGE)
C
  300 CONTINUE
      IF (PO(LOCCV+1).GT.620.0) GO TO 350
C
C  WE NEED TO RETURN THE RULE CURVE ELEVATION AT RUN TIME
C
      VALUE = RULEL2
      IF (NSCHEX.EQ.0) VALUE = RULEL1
C
C  NOW SEE IF WE NEED TO WORK WITH AN OPERATOR AND FACTOR (E.G. +2.00)
C
      IOP = PO(LOCCV+2)
      IF (IOP.EQ.0) GO TO 320
C
      FACTOR = PO(LOCCV+2)
      CALL MATH26(VALUE,IOP,FACTOR)
C
  320 CONTINUE
      IVAL = VALUE
      GO TO 9000
C
C-------------------
C  WE NEED TO COMPUTE THE MAXIMUM DISCHARGE AT THE CURRENT POOL
C  ELEVATION.
C
  350 CONTINUE
C
      ELCHEK = ELEV2
      IF (NSCHEX.EQ.0) ELCHEK = ELEV1
C
C  GET THE MAX Q AT ELEVATION
C
      CALL XU2026(PO(LOCCV+1),PO,W,LOCOWS,ELCHEK,QM)
C
C  NOW SEE IF WE NEED TO WORK WITH AN OPERATOR AND FACTOR (E.G. +2.00)
C
      IOP = PO(LOCCV+2)
      IF (IOP.EQ.0) GO TO 370
C
      CALL MATH26(QM,IOP,PO(LOCCV+2))
C
  370 CONTINUE
      VALUE = QM
      IVAL = VALUE
C
 9000 CONTINUE
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XGCV26 ***')
C
      RETURN
      END
