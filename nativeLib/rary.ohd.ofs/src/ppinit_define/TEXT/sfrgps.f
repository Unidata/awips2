C MODULE SFRGPS
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ PARAMETERS FOR GROUPS NOT REDEFINED AND UPDATE
C  STATION DESCRIPTION AND STATE DESIGNATOR IF NECESSARY.
C
      SUBROUTINE SFRGPS (DISP,PRPARM,STAID,NBRSTA,NGPS,GPS,
     *   NGPSN,GPSN,IPPARM,IDESCN,ISTATN,DESCRP,STATE,
     *   INPCPN,IPPROC,PCPNCF,ITPPVR,
     *   ITYOBS,TEMPCF,ITTAVR,ITFMM,
     *   RRSTYP,NRRSTP,IRTIME,NVLPOB,NUMOBS,MNODAY,
     *   LARRAY,ARRAY,ISTAT)
C
C
      CHARACTER*4 DISP,WDISP,PRPARM
      CHARACTER*4 GPSN(NGPSN)
      CHARACTER*20 XDESCRP
C      
      DIMENSION ARRAY(LARRAY)
      DIMENSION IPPARM(1),IPPPTR(3)
      DIMENSION UNUSED(10)
C
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimpcpn'
      INCLUDE 'scommon/dimtemp'
      INCLUDE 'scommon/dimpe'
      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfrgps.f,v $
     . $',                                                             '
     .$Id: sfrgps.f,v 1.2 1998/04/07 15:09:41 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,220)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,230) STAID,DISP,NGPSN,LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUMERR=0
      UNSD=-999.
C
C  CHECK FOR VALID DISPOSITION
      IF (DISP.EQ.'OLD'.OR.DISP.EQ.'NEW') GO TO 10
         WRITE (LP,240) DISP
         CALL SUERRS (LP,2,NUMERR)
         GO TO 210
C
10    IF (DISP.EQ.'NEW') GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IPRNT=1
      IPREST=0
      LEVEL=1
C      
C  READ PARAMETERS FOR GROUPS NOT REDEFINED - UPDATE STATION
C  DESCRIPTION AND STATE DESIGNATOR IF NECESSARY
C
      DO 200 I=1,NGPS
         IF (NGPSN.EQ.0) GO TO 30
         DO 20 J=1,NGPSN
            IF (GPS(I).EQ.GPSN(J)) GO TO 200
20          CONTINUE
30       IF (GPS(I).EQ.'PCPN') THEN
            IPRERR=1
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrpcpn'
            IF (IERR.NE.0) THEN
               WRITE (LP,270) 'PCPN',STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 200
               ENDIF
            INPCPN=1
            IF (IDESCN.EQ.0.AND.ISTATN.EQ.0) GO TO 50
               WRITE (LP,290) 'PCPN',STAID
               CALL SULINE (LP,2)
               WDISP='OLD'
               NSPACE=1
               INCLUDE 'scommon/callswpcpn'
               IF (IERR.NE.0) THEN
                  WRITE (LP,280) 'PCPN',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 200
                  ENDIF
50          IF (LDEBUG.GT.0) GO TO 60
            IF (PRPARM.EQ.'YES'.AND.
     *          (IDESCN.GT.0.OR.ISTATN.GT.0)) GO TO 60
            GO TO 200
60          IF (IDESCN.EQ.0) CALL SUBSTR (XDESCRP,1,20,DESCRP,1)
            IF (ISTATN.EQ.0) STATE=XSTATE
            INCLUDE 'scommon/callsppcpn'
            GO TO 200
            ENDIF
         IF (GPS(I).EQ.'TEMP') THEN
            IPRERR=1
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrtemp'
            IF (IERR.NE.0) THEN
               WRITE (LP,270) 'TEMP',STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 200
               ENDIF
            IF (IDESCN.EQ.0.AND.ISTATN.EQ.0) GO TO 90
               WRITE (LP,290) 'TEMP',STAID
               CALL SULINE (LP,2)
               WDISP='OLD'
               NSPACE=1
               IF (IERR.NE.0) THEN
                  WRITE (LP,280) 'TEMP',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 200
                  ENDIF
90          IF (LDEBUG.GT.0) GO TO 100
            IF (PRPARM.EQ.'YES'.AND.
     *          (IDESCN.GT.0.OR.ISTATN.GT.0)) GO TO 100
            GO TO 200
100         IF (IDESCN.EQ.0) CALL SUBSTR (XDESCRP,1,20,DESCRP,1)
            IF (ISTATN.EQ.0) STATE=XSTATE
            INCLUDE 'scommon/callsrpcpn'
            GO TO 200
            ENDIF
         IF (GPS(I).EQ.'PE') THEN
            IPRERR=1
            IPTR=0
            INCLUDE 'scommon/callsrpcpn'
            IF (IERR.NE.0) THEN
               WRITE (LP,270) 'PE',STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 200
               ENDIF
            IF (IDESCN.EQ.0.AND.ISTATN.EQ.0) GO TO 130
               WRITE (LP,290) 'PE',STAID
               CALL SULINE (LP,2)
               WDISP='OLD'
               INCLUDE 'scommon/callswpe'
               IF (IERR.NE.0) THEN
                  WRITE (LP,280) 'PE',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 200
                  ENDIF
130         IF (LDEBUG.GT.0) GO TO 140
            IF (PRPARM.EQ.'YES'.AND.
     *         (IDESCN.GT.0.OR.ISTATN.GT.0)) GO TO 140
            GO TO 200
140         IF (IDESCN.EQ.0) CALL SUBSTR (XDESCRP,1,20,DESCRP,1)
            IF (ISTATN.EQ.0) STATE=XSTATE
            INCLUDE 'scommon/callsppe'
            GO TO 200
            ENDIF
         IF (GPS(I).EQ.'RRS') THEN
            IPRERR=1
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrrrs'
            IF (IERR.NE.0) THEN
               WRITE (LP,270) 'RRS',STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 200
               ENDIF
            IF (IDESCN.EQ.0.AND.ISTATN.EQ.0) GO TO 170
               WRITE (LP,290) 'RRS',STAID
               CALL SULINE (LP,2)
               WDISP='OLD'
               INCLUDE 'scommon/callswrrs'
               IF (IERR.NE.0) THEN
                  WRITE (LP,280) 'RRS',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 200
                  ENDIF
170         IF (LDEBUG.GT.0) GO TO 180
            IF (PRPARM.EQ.'YES'.AND.
     *         (IDESCN.GT.0.OR.ISTATN.GT.0)) GO TO 180
            GO TO 200
180         IF (IDESCN.EQ.0) CALL SUBSTR (XDESCRP,1,20,DESCRP,1)
            IF (ISTATN.EQ.0) STATE=XSTATE
            INCLUDE 'scommon/callsprrs'
            GO TO 200
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,260) STAID,GPS(I)
            CALL SULINE (IOSDBG,1)
            ENDIF
200      CONTINUE
C
C  CHECK IF ERRORS ENCOUNTERED
210   IF (NUMERR.GT.0) ISTAT=1
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,300)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
220   FORMAT (' *** ENTER SFRGPS')
230   FORMAT (' STAID=',A,3X,'DISP=',A4,3X,'NGPSN=',I2,3X,
     *   'LARRAY=',I5)
240   FORMAT ('0*** ERROR - IN SFRGPS - INVALID DISPOSITION : ',A4)
260   FORMAT (' STATION ',A,' DOES NOT HAVE ',A4,' PARAMETERS')
270   FORMAT ('0*** ERROR - IN SFRGPS - ',A4,' PARAMETER ',
     *   'FOR STATION ',A,' NOT SUCCESSFULLY READ.')
280   FORMAT ('0*** ERROR - IN SFRGPS - ',A4,' PARAMETER ',
     *   'FOR STATION ',A,' NOT SUCCESSFULLY WRITTEN.')
290   FORMAT ('0*** NOTE - ',A4,' PARAMETERS FOR STATION ',A,
     *   ' WILL BE UPDATED BECAUSE STATION DESCRIPTION OR STATE ',
     *   'DESIGNATOR CHANGED.')
300   FORMAT (' *** EXIT SFRGPS')
C
      END
