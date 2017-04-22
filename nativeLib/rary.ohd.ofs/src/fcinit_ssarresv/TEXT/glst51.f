C MEMBER GLST51
C---------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE GLST51(LTYPE,LENC,IVAL,RVAL,CVAL,NVAL,LISTOK)
C
C DESC GET LIST OF ITEMS (INTEGER, REAL OR CHARACTER)
C----------------------------------------------------------------------
C  ROUTINE TO GET LIST OF ITEMS, EITHER INTEGER, REAL, OR CHARACTER
C----------------------------------------------------------------------
C
C  KUANG HSU - HRL - OCTOBER 1994
C----------------------------------------------------------------
      INCLUDE 'common/fld51'
C
      DIMENSION IVAL(1),RVAL(1),CVAL(1,1)
C
      LOGICAL LISTOK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/glst51.f,v $
     . $',                                                             '
     .$Id: glst51.f,v 1.1 1996/03/21 14:28:07 page Exp $
     . $' /
C    ===================================================================
C
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      LISTOK = .TRUE.
      MVAL=NVAL
      NVAL = 0
      USEDUP = .FALSE.
C
C  GET THE NEXT ITEM ON THE LINE, BUT DO NOT DROP TO NEXT LINE
C
   10 CONTINUE
      NUMFLD = -2
C
   15 CONTINUE
      CALL UFLD51(NUMFLD,IERF)
      IF (NVAL .LE. MVAL) GO TO 17
      IF (IERF.NE.1) GO TO 16
      LISTOK=.FALSE.
      GO TO 9999
   16 IF (LAMPS .NE. 1) GO TO 10
      NUMFLD=0
      GO TO 15
   17 CONTINUE
      IF (IERF.GT.1) GO TO 9000
C
C  IF NEXT FIELD IS NULL (IERF=1), WE ARE DONE WITH THE LIST
C
      IF (IERF.EQ.1) GO TO 9999
C
C  CHECK FIRST FOR CONTINUATION CHARACTER
C
      IF (LAMPS.NE.1) GO TO 30
C
C  IF CONTINUATION FOUND, GO TO NEXT LINE FOR NEXT ITEM
C
      NUMFLD = 0
      GO TO 15
C
C  CHECK FOR CORRECT TYPE OF FIELD
C
   30 CONTINUE
      IF (ITYPE.LE.LTYPE) GO TO 40
C
      IF (LTYPE.EQ.0) CALL STER51(5,1)
      IF (LTYPE.EQ.1) CALL STER51(4,1)
      LISTOK = .FALSE.
      GO TO 10
C
C  CHECK FOR REPEAT FACTOR
C
   40 CONTINUE
      IF (NREP.EQ.-1) GO TO 100
C
C  FILL PROPER ARRAY WITH VALUES
C
      IDEST = LTYPE + 1
      GO TO (50,60,70), IDEST
C
C  INTEGER TO BE STORED
C
   50 CONTINUE
      DO 55 I=1,NREP
      NVAL = NVAL+1
      IF (NVAL .LE. MVAL) GO TO 54
      CALL STER51(119,1)
      GO TO 10
   54 CONTINUE
      IVAL(NVAL) = INTEGR
   55 CONTINUE
      GO TO 10
C
C  REAL VALUE TO BE STORED
C
   60 CONTINUE
      DO 65 I=1,NREP
      NVAL = NVAL + 1
      IF (NVAL .LE. MVAL) GO TO 64
      CALL STER51(119,1)
      GO TO 10
   64 CONTINUE
      RVAL(NVAL) = REAL
   65 CONTINUE
C
C  CHARACTER FIELDS TO BE STORED. MAKE SURE LENGTH OF FIELD WILL FIT IN
C  CVAL ARRAY.
C
   70 CONTINUE
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.LE.LENC) GO TO 75
C
      CALL STER51(96,1)
      LISTOK = .FALSE.
      GO TO 10
C
   75 CONTINUE
      DO 80 I=1,NREP
      NVAL = NVAL + 1
      IF (NVAL .LE. MVAL) GO TO 84
      CALL STER51(119,1)
      GO TO 10
   84 CONTINUE
      DO 85 J=1,LENC
      CVAL(J,NVAL) = CHAR(J)
   85 CONTINUE
   80 CONTINUE
C
      GO TO 10
C
C-----------------------------------------------------------------
C  NO REPEAT FACTOR FOUND, JUST STORE VALUE.
C
  100 CONTINUE
C
      NVAL = NVAL + 1
      IF (NVAL .LE. MVAL) GO TO 110
      CALL STER51(119,1)
      GO TO 10
  110 CONTINUE
      IF (LTYPE.EQ.2) GO TO 150
C
      IF (LTYPE.EQ.0) IVAL(NVAL) = INTEGR
      IF (LTYPE.EQ.1) RVAL(NVAL) = REAL
      GO TO 10
C
C  CHARACTERS TO BE STORED, SEE IF THEY'LL FIT
C
  150 CONTINUE
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.LE.LENC) GO TO 160
C
      CALL STER51(96,1)
      LISTOK = .FALSE.
      GO TO 10
C
  160 CONTINUE
      DO 170 J=1,LENC
      CVAL(J,NVAL) = CHAR(J)
  170 CONTINUE
      GO TO 10
C
C---------------------------------------------------------------------
C  ERROR IN UFLD51
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER51(19,1)
      IF (IERF.EQ.2) CALL STER51(20,1)
      IF (IERF.EQ.3) CALL STER51(21,1)
      IF (IERF.EQ.4) CALL STER51(1,1)
C
      IF (IERF.NE.3) GO TO 10
      USEDUP = .TRUE.
C
 9999 CONTINUE
      RETURN
      END
