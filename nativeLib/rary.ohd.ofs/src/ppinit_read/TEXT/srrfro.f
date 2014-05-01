C MEMBER SRRFRO
C-----------------------------------------------------------------------
C
C DESC READ RAINFALL-RUNOFF RELATION PARAMETERS
C
      SUBROUTINE SRRFRO (IVRFRO,UNUSED,
     *   RFROID,NURFRO,RFROPM,
     *   LARRAY,ARRAY,IPRERR,IPTR,IPTRNX,
     *   ISTAT)
C
C
      REAL XRFRO/4HRFRO/
      DIMENSION ARRAY(1)
      DIMENSION UNUSED(1)
      INCLUDE 'scommon/dimrfro'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srrfro.f,v $
     . $',                                                             '
     .$Id: srrfro.f,v 1.1 1995/09/17 19:15:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,80)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(XRFRO)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,90) LARRAY
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
      NUMERR=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,4HPPP ,IERR)
      IF (IERR.EQ.0) GO TO 10
         ISTAT=1
         GO TO 70
10    CALL RPPREC (RFROID,XRFRO,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.EQ.0) GO TO 20
         ISTAT=IERR
         IF (ISTAT.EQ.6) GO TO 70
            IF (ISTAT.EQ.2.AND.IPRERR.EQ.0) GO TO 70
               CALL SRPPST (RFROID,XRFRO,IPTR,LARRAY,NFILL,IPTRNX,IERR)
               WRITE (LP,100) IERR,IPTR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 70
C
C  SET PARAMETER ARRAY VERSION NUMBER
20    IVRFRO=ARRAY(1)
C
C  SET RAINFALL-RUNOFF RELATION IDENTIFIER
      RFROID(1)=ARRAY(2)
      RFROID(2)=ARRAY(3)
C
C  SET RAINFALL-RUNOFF RELATION NUMBER
      NURFRO=ARRAY(4)
C
      NPOS=4
C
C  NEXT POSITIONS ARE UNUSED
      DO 30 I=1,2
         NPOS=NPOS+1
         UNUSED(I)=ARRAY(NPOS)
30       CONTINUE
C
C  SET RAINFALL-RUNOFF RELATION PARAMETERS
      DO 40 I=1,10
         NPOS=NPOS+1
         RFROPM(I)=ARRAY(NPOS)
40       CONTINUE
C
60    IF (LDEBUG.EQ.0) GO TO 70
         WRITE (IOSDBG,110) NPOS,NFILL,IPTRNX,IVRFRO
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP (XRFRO,4HBOTH,0,NFILL,ARRAY,ARRAY)
C
70    IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) WRITE (IOSDBG,120) RFROID
      IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) CALL SULINE (IOSDBG,1)
      IF (LDEBUG.GT.0.AND.ISTAT.GT.0) WRITE (IOSDBG,130) RFROID
      IF (LDEBUG.GT.0.AND.ISTAT.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,140)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SRRFRO')
90    FORMAT (' LARRAY=',I5)
100   FORMAT ('0*** ERROR - IN SRRFRO - UNSUCCESSFUL CALL TO RPPREC : ',
     *   'STATUS CODE=',I2,3X,'IPTR=',I5)
110   FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVRFRO=',I3)
120   FORMAT ('0*** NOTE - RFRO PARAMETERS FOR IDENTIFIER ',2A4,
     *   ' SUCCESSFULLY READ.')
130   FORMAT ('0*** NOTE - RFRO PARAMETERS FOR IDENTIFIER ',2A4,
     *   ' NOT SUCCESSFULLY READ.')
140   FORMAT (' *** EXIT SRRFRO')
C
      END
