C MEMBER MAXV26
C  (from old member FCMAXV26)
C
C DESC DETERMINE MAX NO. OF VALUES USED TO DEFINE ANY SPILLWAY CURVE
C--------------------------------------------------------------------
      SUBROUTINE MAXV26(PO,MAX)
C--------------------------------------------------------------------
C  SUBROUTINE TO RETURN THE MAXIMUM NO. OF POINTS USED TO STORE
C  A SPILLWAY RATING CURVE IN ANY DEFINITION OF THE FOLLOWING S/U'S:
C
C     1. FILL 'N' SPILL - S/U 1050
C     2. SPILLWAY       - S/U 1060
C     3. MINQ           - S/U 1090
C
C  THIS ROUTINE IS USED WITH PIN26 ONLY.
C----------------------------------------------------------------------
C  WRITTEN BY - JTOSTROWSKI - MARCH 1985
C----------------------------------------------------------------------
C
      INCLUDE 'common/comn26'
      INCLUDE 'common/suin26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/maxv26.f,v $
     . $',                                                             '
     .$Id: maxv26.f,v 1.2 1999/04/22 14:45:14 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1600)
 1600 FORMAT('    *** ENTER MAXV26 ***')
C
C  INITIALIZE MAX. NO. OF POINTS
C
      MAX = 0
      LOCPTR = PO(9)
C
C  LOOP THROUGH ALL DEFINED S/U'S LOOKING FOR ALL DEFS. OF THE
C  THREE SPILLWAY USING S/U'S.
C
      IF (NSUIN .EQ. 0) GO TO 9000
C
      DO 1000 IORD = 1,NSUIN
ccc      CALL XBLV26(DEFCDE(IORD),IBASE,LEVEL)
C
      IBASE=DEFCDE(IORD)/10
      LEVEL=DEFCDE(IORD)-IBASE*10
      IF (IBASE .EQ. 105) GO TO 100
      IF (IBASE .EQ. 106) GO TO 200
      IF (IBASE .EQ. 109) GO TO 300
      GO TO 1000
C
C  LOOK THROUGH DEFINITION OF S/U #5 FOR SPILLWAY RATING DEF.
C
  100 CONTINUE
C
      LOCSU = LOCPTR + (IORD-1)*4 + 1
      LOCPM = PO(LOCSU+1)
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
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) IBASE,LEVEL,NEVSQ,MAX
 1610 FORMAT('      * BASE NO.',I3,' LEVEL NO.',I2,' NEVSQ = ',I2,
     .      ' MAX = ',I2)
      GO TO 1000
C
C  LOOK AT SPILLWAY ROUTING NEXT (S/U #6)
C
  200 CONTINUE
C
C  LOOK THROUGH DEFINITION OF S/U #6 FOR SPILLWAY RATING DEF.
C
      LOCSU = LOCPTR + (IORD-1)*4 + 1
      LOCPM = PO(LOCSU+1)
C
      LOCTYP = LOCPM
      IF (PO(LOCTYP) .LT. 0.00) LOCTYP = -PO(LOCTYP)
      LOCSPW = LOCTYP + 3
      IF (PO(LOCSPW) .LT. 0.00) LOCSPW = -PO(LOCSPW)
      NEVSQ = PO(LOCSPW)
      MAX = MAX0(MAX,NEVSQ)
C
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) IBASE,LEVEL,NEVSQ,MAX
      GO TO 1000
C
C  LOOK AT MINQ NEXT (S/U #9)
C
  300 CONTINUE
C
C  GO THROUGH DEFINITION OF MINQ TO FIND ELEV VS MAXQ DEF.
C
      LOCSU = LOCPTR + (IORD-1)*4 + 1
      LOCPM = PO(LOCSU+1)
      LOCMQ = LOCPM + 4
      ITYPE = PO(LOCPM)
C
C  CAN HAVE THREE KINDS OF CURVES FOR MINQ. OTHER THAN TYPE = -1,
C  THE NO. OF POINTS IN CURVE IS NSE( # IN ELEV. VS. STORAGE CURVE).
C
      IF (ITYPE .GE. 0) GO TO 1000
C
C  A NEGATIVE VALUE FOR THE NO. OF VALUES INDICATES THAT CURVE IS
C  DEFINED ELSEWHERE, SO GO TO ELSEWHERE.
C
      IF (PO(LOCMQ) .LT. 0.00) LOCMQ = -PO(LOCMQ)
      NEVSQ = PO(LOCMQ)
      MAX = MAX0(MAX,NEVSQ)
      IF (IBUG .GE. 2) WRITE(IODBUG,1610) IBASE,LEVEL,NEVSQ,MAX
      GO TO 1000
C
 1000 CONTINUE
C
 9000 CONTINUE
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1699)
 1699 FORMAT('       *** EXIT MAXV26 ***')
      RETURN
      END
