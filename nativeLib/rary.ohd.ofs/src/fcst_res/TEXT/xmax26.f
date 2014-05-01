C MEMBER XMAX26
C  (from old member FCXMAX26)
C
C DESC DETERMINE MAX NO. OF POINTS IN LARGEST SPILLWAY CURVE
C--------------------------------------------------------------------
      SUBROUTINE XMAX26(PO,MAX)
C--------------------------------------------------------------------
C  SUBROUTINE TO RETURN THE MAXIMUM NO. OF POINTS USED TO STORE
C  A SPILLWAY RATING CURVE IN ANY DEFINITION OF THE FOLLOWING S/U'S:
C
C     1. FILL 'N' SPILL - S/U #5 IBASE #105
C     2. SPILLWAY       - S/U #6 IBASE #106
C     3. MINQ           - S/U #9 IBASE #109
C
C  THIS ROUTINE IS USED WITH EX26 ONLY.
C----------------------------------------------------------------------
C  WRITTEN BY - JTOSTROWSKI - MARCH 1985
C----------------------------------------------------------------------
C
      INCLUDE 'common/exg26'
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xmax26.f,v $
     . $',                                                             '
     .$Id: xmax26.f,v 1.2 1999/04/23 16:55:31 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1600)
 1600 FORMAT('    *** ENTER XMAX26 ***')
C
C  INITIALIZE MAX. NO. OF POINTS
C
      MAX = 0
C
      DO 550 I=1,NSUDEF
      LOCSUN = LOCPTR + (I-1)*4 + 1
      ISUNUM = PO(LOCSUN)
      LOCPM = PO(LOCSUN+1)
      IBASE=ISUNUM/10
      IF(IBASE.EQ.105) GO TO 1050
      IF(IBASE.EQ.106) GO TO 1060
      IF(IBASE.EQ.109) GO TO 1090
      GO TO 550
C
C   S/U #5 FOR SPILLWAY RATING DEF'S.
C
 1050 CONTINUE
C
C  NEGATIVE VALUES INDICATE REFERENCES TO ORIGINAL DEFS OF ITEMS
C
      LOCTYP = LOCPM + 1
      IF (PO(LOCTYP) .LT. 0.00) LOCTYP = -PO(LOCTYP)
      LOCSPW = LOCTYP + 3
      IF (PO(LOCSPW) .LT. 0.00) LOCSPW = -PO(LOCSPW)
      NEVSQ = PO(LOCSPW)
      MAX = MAX0(MAX,NEVSQ)
C
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) I,NEVSQ,MAX
 1610 FORMAT('      * AFTER PASS #',I1,' NEVSQ = ',I2,' MAX = ',I2)
      GO TO 550
C
C  S/U #6 FOR SPILLWAY RATING DEF'S.
C
 1060 CONTINUE
      LOCTYP = LOCPM
      IF (PO(LOCTYP) .LT. 0.00) LOCTYP = -PO(LOCTYP)
      LOCSPW = LOCTYP + 3
      IF (PO(LOCSPW) .LT. 0.00) LOCSPW = -PO(LOCSPW)
      NEVSQ = PO(LOCSPW)
      MAX = MAX0(MAX,NEVSQ)
C
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) I,NEVSQ,MAX
      GO TO 550
C
C  S/U #9 MINQ TO FIND ELEV VS MAXQ DEFS.
C
 1090 CONTINUE
      LOCMQ = LOCPM + 4
      ITYPE = PO(LOCPM)
C
C  CAN HAVE THREE KINDS OF CURVES FOR MINQ. OTHER THAN TYPE = -1,
C  THE NO. OF POINTS IN CURVE IS NSE( # IN ELEV. VS. STORAGE CURVE).
C
      IF (ITYPE .GE. 0) GO TO 550
C
C  A NEGATIVE VALUE FOR THE NO. OF VALUES INDICATES THAT CURVE IS
C  DEFINED ELSEWHERE, SO GO TO ELSEWHERE.
C
      IF (PO(LOCMQ) .LT. 0.00) LOCMQ = -PO(LOCMQ)
      NEVSQ = PO(LOCMQ)
      MAX = MAX0(MAX,NEVSQ)
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) I,NEVSQ,MAX
C
  550 CONTINUE
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1699)
 1699 FORMAT('       *** EXIT XMAX26 ***')
      RETURN
      END

