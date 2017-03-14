C MEMBER SUDMDD
C-----------------------------------------------------------------------
C
C DESC CONVERT LATITUDE/LONGITUDE FROM DECIMAL DEGREES TO DEGREES AND
C DESC MINUTES OR VISA VERSA
C
      SUBROUTINE SUDMDD (CONV,NVAL,DECDEG,MINDEG,ISTAT)
C
      REAL DM/4HDM  /,DD/4HDD  /
C
      DIMENSION DECDEG(1),MINDEG(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sudmdd.f,v $
     . $',                                                             '
     .$Id: sudmdd.f,v 1.1 1995/09/17 19:15:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,200)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSYS )
C
      ISTAT=0
      NUMERR=0
C
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (IOSDBG,210) CONV
         CALL SULINE (IOSDBG,1)
C
C  CHECK FOR VALID CONVERSION REQUEST
C
C     CONV='DM' TO CONVERT FROM DECIMAL DEGREES TO DEGREES AND MINUTES
C     CONV='DD' TO CONVERT FROM DEGREES AND MINUTES TO DECIMAL DEGREES
C
10    IF (CONV.EQ.DM.OR.CONV.EQ.DD) GO TO 20
         WRITE (LP,240) CONV
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 100
C
20    IF (CONV.EQ.DD) GO TO 40
C
C  CONVERT FROM DECIMAL DEGREES TO DEGREES AND MINUTES
      DO 70 I=1,NVAL
         MINDEG(I*2-1)=DECDEG(I)
         DEG=MINDEG(I*2-1)
         MINDEG(I*2)=(DECDEG(I)*100.-DEG*100.)*.6+.5
         IF (LDEBUG.GT.0) WRITE (IOSDBG,250) DECDEG(I),
     *      MINDEG(I*2-1),MINDEG(I*2)
         IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
70       CONTINUE
      GO TO 100
C
C  CONVERT FROM DEGREES AND MINUTES TO DECIMAL DEGREES
40    DO 80 I=1,NVAL
         DECDEG(I)=MINDEG(I*2-1)+MINDEG(I*2)/60.
80       CONTINUE
         IF (LDEBUG.GT.0) WRITE (IOSDBG,255) DECDEG,MINDEG
         IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
100   IF (ISTRCE.GT.0) WRITE (IOSDBG,530) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
200   FORMAT (' *** ENTER SUDMDD')
210   FORMAT (' CONV=',A4)
240   FORMAT ('0*** ERROR - IN SUDMDD - INVALID CONVERSION CODE: ',I2)
250   FORMAT (' DECDEG=',F7.2,3X,'MINDEG=',I3,'-',I2)
255   FORMAT (' MINDEG=',I3,'-',I2,3X,'DEGDEG=',F7.2)
530   FORMAT (' *** EXIT SUDMDD - STATUS CODE=',I2)
C
      END
