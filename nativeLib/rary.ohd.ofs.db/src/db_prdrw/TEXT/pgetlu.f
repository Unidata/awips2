C MEMBER PGETLU
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      SUBROUTINE PGETLU (LUNIT,INDEX)
C
C  THIS ROUTINE WILL ASSOCIATE A LOGICAL UNIT WITH THE INDEX
C  FOR THE TIME SERIES CONTROL COMMON BLOCK.
C
      DIMENSION IUNIT(5)
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/punits'
C
      EQUIVALENCE (IUNIT(1),KMAPTS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pgetlu.f,v $
     . $',                                                             '
     .$Id: pgetlu.f,v 1.1 1995/09/17 18:45:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,40) LUNIT
C
      DO 10 I=1,5
         IF (LUNIT.EQ.IUNIT(I)) GO TO 20
10       CONTINUE
C
      INDEX=0
      GO TO 30
C
20    INDEX=I
C
30    IF (IPRTR.GT.0) WRITE (IOGDB,50) INDEX
C
      RETURN
C
40    FORMAT (' *** ENTER PGETLU - LUNIT=',I3)
50    FORMAT (' *** EXIT PGETLU - INDEX=',I2)
C
      END
