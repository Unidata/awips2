C MEMBER ASCN26
C  (from old member FCASCN26)
C
C DESC CHECK ON ASCENDING VALUES IN AN ARRAY
C--------------------------------------------------------------------
      SUBROUTINE ASCN26(VALUE,NVAL,ITYPE,IER)
C---------------------------------------------------------------------
C  ROUTINE TO CHECK ON ASCENDING VALUES IN AN ARRAY. EACH VALUE MUST
C   BE GREATER THAN THE PREVIOUS ONE IN THE ARRAY IF ITYPE .NE. 0.
C   IF ITYPE = 1, EACH VALUE CAN BE GREATER THAN OR EQUAL TO
C   THE PREVIOUS VALUE.
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------------
C
      DIMENSION VALUE(*)
      INCLUDE 'common/errv26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ascn26.f,v $
     . $',                                                             '
     .$Id: ascn26.f,v 1.2 1996/07/12 13:12:31 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NVAL .LE. 1) GO TO 35
      IER = 1
      NALM = NVAL - 1
C
      IF (ITYPE .EQ. 1) GO TO 20
      DO 10 I=1,NALM
      IF (IFMSNG(VALUE(I+1)) .EQ. 1) GO TO 10
      IF (VALUE(I).LT.VALUE(I+1)) GO TO 10
C
      CALL STER26(41,1)
      IERR=41
      IPT=I
      VALUE1=VALUE(I)
      VALUE2=VALUE(I+1)
      GO TO 9999
C
   10 CONTINUE
      IER = 0
      GO TO 9999
C
   20 CONTINUE
      DO 30 I=1,NALM
      IF (IFMSNG(VALUE(I+1)) .EQ. 1) GO TO 30
      IF (VALUE(I).LE.VALUE(I+1)) GO TO 30
C
      CALL STER26(41,1)
      IERR=41
      IPT=I
      VALUE1=VALUE(I)
      VALUE2=VALUE(I+1)
      GO TO 9999
C
   30 CONTINUE
   35 CONTINUE
      IER = 0
C
 9999 CONTINUE
      RETURN
      END
