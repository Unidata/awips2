C MEMBER SUGTRR
C-----------------------------------------------------------------------
C
C DESC ROUTINE TO FILL RAINFALL-RUNOFF RELATIONSHIP ARRAY
C
      SUBROUTINE SUGTRR (LARRAY,ARRAY,MRFRO,IRFRO,NRFRO,ISTAT)
C
C
      REAL*8 BLNK8/8H        /
C
      DIMENSION ARRAY(1)
      DIMENSION UNUSED(2)
      DIMENSION IRFRO(1)
      INCLUDE 'scommon/dimrfro'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtrr.f,v $
     . $',                                                             '
     .$Id: sugtrr.f,v 1.1 1995/09/17 19:15:36 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,70)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HRFRO)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,80) MRFRO
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
      NRFRO=0
C
      DO 10 I=1,MRFRO
         IRFRO(I)=0
10       CONTINUE
C
C  READ RFRO PARAMETERS
      IPTR=0
      IPRERR=0
20    CALL SUBSTR (BLNK8,1,8,RFROID,1)
      INCLUDE 'scommon/callsrrfro'
      IF (IERR.EQ.2.AND.NRFRO.EQ.0) GO TO 50
      IF (IERR.EQ.6) GO TO 50
      IF (IERR.GT.0)
     *   CALL SRPPST (RFROID,4HRFRO,IPTR,LARRAY,NFILL,IPTRNX,IERR)
      IRRFRO=IERR
      IF (IRRFRO.GT.0) GO TO 35
      IF (LDEBUG.LE.1) GO TO 30
      INCLUDE 'scommon/callsprfro'
30    NRFRO=NRFRO+1
      IF (NRFRO.LE.MRFRO) GO TO 33
         WRITE (LP,85) MRFRO
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 50
33    IRFRO(NRFRO)=NURFRO
35    IF (IPTRNX.EQ.0) GO TO 50
         IPTR=IPTRNX
         GO TO 20
C
50    IF (LDEBUG.EQ.0) GO TO 60
         WRITE (IOSDBG,90) NRFRO
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,100) (IRFRO(I),I=1,NRFRO)
         NLINES=(NRFRO+20)/20
         CALL SULINE (IOSDBG,NLINES)
C
60    IF (ISTRCE.GT.0) WRITE (IOSDBG,110)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SUGTRR')
80    FORMAT (' MRFRO=',I5)
85    FORMAT ('0*** ERROR - IN SUGTRR - MAXUMUM NUMBER OF ',
     *   'RAINFALL-RUNOFF RELATIONSHIPS THAT CAN BE PROCESSED ',I4,
     *   'EXCEEDED.')
90    FORMAT (' NRFRO=',I2,3X,'IRFRO:')
100   FORMAT (20(1X,I2))
110   FORMAT (' *** EXIT SUGTRR')
C
      END
