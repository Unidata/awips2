C MEMBER UCMPAR
C-----------------------------------------------------------------------
C
      SUBROUTINE UCMPAR (IRAY,JRAY,NUM,ISTAT)
C
C  ROUTINE TO COMPARE TWO ARRAYS.
C
C     IF ARRAYS SAME, ISTAT=0
C
CZ    DIMENSION IRAY(1),JRAY(1)
      INTEGER*2 IRAY(1),JRAY(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/ucmpar.f,v $
     . $',                                                             '
     .$Id: ucmpar.f,v 1.1 1995/09/17 19:02:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=1
C
      DO 100 I=1,2*NUM
         IF (IRAY(I).NE.JRAY(I)) GO TO 999
  100    CONTINUE
C
      ISTAT=0
C
  999 RETURN
C
      END
