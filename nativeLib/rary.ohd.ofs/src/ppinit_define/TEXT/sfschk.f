C MODULE SFSCHK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF A STATION CAN BE USED IN AN AREA DEFINITION.
C
      SUBROUTINE SFSCHK (LARRAY,ARRAY,STAID,TYPE,STACOR,
     *   IPPP24,IPPPVR,IPEA24,IPTM24,NUMERR,ISTAT)
C
      CHARACTER*4 TYPE,XTYPE,UNITS,RDISP
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(10)
C
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimpcpn'
      INCLUDE 'scommon/dimtemp'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfschk.f,v $
     . $',                                                             '
     .$Id: sfschk.f,v 1.3 1998/04/10 16:15:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      IPPP24=0
      IPPPVR=0
      IPEA24=0
      IPTM24=0
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
C  READ GENL PARAMETER RECORD
      IPRERR=0
      IPTR=0
      RDISP='OLD'
      INCLUDE 'scommon/callsrstan'
      IF (IERR.GT.0) THEN
         WRITE (LP,70) STAID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 50
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,90) (GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         IPRNT=1
         UNITS='ENGL'
         INCLUDE 'scommon/callspstan'
         ENDIF
C
C  CHECK IF STATION IS COMPLETE
      IF (ICSTAN.EQ.1) THEN
         WRITE (LP,80) STAID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 50
         ENDIF
C
C  CHECK IF STATION HAS SPECIFIED DATA TYPE DEFINED
      XTYPE=TYPE
      IF (XTYPE.EQ.'PP24') XTYPE='PCPN'
      IF (XTYPE.EQ.'PPVR') XTYPE='PCPN'
      DO 10 I=1,NGPS
         IF (GPS(I).EQ.XTYPE) GO TO 20
10       CONTINUE
      WRITE (LP,110) STAID,TYPE
      CALL SUERRS (LP,2,NUMERR)
      ISTAT=1
      GO TO 50
C
C  CHECK IF TYPE IS POTENTIAL EVAPORATION
20    IF (TYPE.EQ.'PE') GO TO 40
C
C  READ PARAMETERS
      IPTR=0
      IPRERR=0
C
C  CHECK IF TYPE TO BE CHECKED IS PRECIPITATION
      IF (TYPE.EQ.'PCPN'.OR.TYPE.EQ.'PP24'.OR.TYPE.EQ.'PPVR') THEN
         IREAD=1
         INCLUDE 'scommon/callsrpcpn'
         IF (IERR.GT.0) THEN
            CALL SRPPST (STAID,TYPE,IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ISTAT=1
            GO TO 50
            ENDIF
         IF (LDEBUG.GT.0) THEN
            IPRNT=1
            LEVEL=1
            IPREST=0
            UNITS='ENGL'
            INCLUDE 'scommon/callsppcpn'
            ENDIF
         IF (TYPE.EQ.'PPVR') THEN
C        CHECK TIME INTERVAL
            IF (IPTIME.EQ.24) THEN
               WRITE (LP,120) STAID,IPTIME
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 50
               ENDIF
            ENDIF
C     CHECK IF NETWORK HAS BEEN RUN
         IF (IPNTWK.EQ.2.OR.IPNTWK.EQ.3) THEN
            ELSE
               WRITE (LP,130) STAID,TYPE
               CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 40
         ENDIF
C
C  CHECK IF TYPE TO BE CHECKED IS TEMPERATURE
      IF (TYPE.EQ.'TEMP') THEN
         IREAD=1
         INCLUDE 'scommon/callsrtemp'
         IF (IERR.GT.0) THEN
            CALL SRPPST (STAID,TYPE,IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ISTAT=1
            GO TO 50
            ENDIF
         IF (LDEBUG.GT.0) THEN
            IPRNT=1
            LEVEL=1
            UNITS='ENGL'
            INCLUDE 'scommon/callsptemp'
            ENDIF
C     CHECK IF NETWORK HAS BEEN RUN
         IF (ITNTWK.EQ.2) THEN
            ELSE
               WRITE (LP,130) STAID,TYPE
               CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 40
         ENDIF
C
C  INVALID DATA TYPE
      WRITE (LP,100) STAID,TYPE
      CALL SUERRS (LP,2,NUMERR)
      ISTAT=1
      GO TO 50
C
40    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,140) STAID,TYPE,IPPP24,IPPPVR,IPEA24,IPTM24
         CALL SULINE (IOSDBG,1)
         ENDIF
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SFSCHK')
70    FORMAT ('0*** ERROR - STATION ',A,' IS NOT DEFINED.')
80    FORMAT ('0*** ERROR - STATION ',A,' EXISTS BUT ',
     *   'HAS A STATUS OF INCOMPLETE.')
90    FORMAT (' GPS=',5(A4,1X))
100   FORMAT ('0*** ERROR - STATION ',A,' EXISTS BUT ',
     *   'DATA TYPE ',A4,' IS INVALID.')
110   FORMAT ('0*** ERROR - STATION ',A,' EXISTS BUT ',
     *   'DOES NOT HAVE ',A4,' DATA DEFINED.')
120   FORMAT ('0*** ERROR - STATION ',A,' EXISTS BUT ',
     *   'TIME INTERVAL (',I2,') IS 24 HOURS.')
130   FORMAT ('0*** ERROR - STATION ',A,' EXISTS AND HAS ',A4,
     *   ' DATA DEFINED BUT NETWORK HAS NOT BEEN RUN.')
140   FORMAT (' STAID=',A,3X,'TYPE=',A4,3X,'IPPP24=',I5,3X,
     *   'IPPPVR=',I5,3X,'IPEA24=',I5,3X,'IPTM24=',I5)
150   FORMAT (' *** EXIT SFSCHK : STATUS CODE=',I2)
C
      END
