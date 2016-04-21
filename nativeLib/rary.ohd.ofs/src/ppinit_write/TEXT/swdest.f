C MODULE SWDEST
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES BASED ON STATUS CODE RETURNED
C  FROM DATA ENTRY CONTROL FILE DATA BASE READ/WRITE ROUTINES.
C
      SUBROUTINE SWDEST (CALLER,PTYPE,USERID,STAID,
     *   IDTYPE,SRCID1,SRCID2,
     *   NDLYTP,DLYTYP,NRRSTP,RRSTYP,IDTIME,
     *   NUMWRN,NUMERR,ISTAT)
C
      CHARACTER*(*) CALLER,PTYPE,USERID,STAID
      CHARACTER*4 FLTYPE
      CHARACTER*4 DLYTYP(NDLYTP),RRSTYP(NRRSTP)
      CHARACTER*6 ROUTINE,ACTION
      CHARACTER*8 SRCID1,SRCID2
      CHARACTER*8 XID1,XID2
C
      DIMENSION IDTIME(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swdest.f,v $
     . $',                                                             '
     .$Id: swdest.f,v 1.3 1998/04/07 18:33:05 page Exp $
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
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,70) CALLER,ISTAT,STAID,IDTYPE
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,80) NDLYTP,(DLYTYP(I),I=1,NDLYTP)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,90) NRRSTP,(RRSTYP(I),I=1,NRRSTP)
         CALL SULINE (IOSDBG,1)
         NDTIME=NDLYTP+NRRSTP
         WRITE (IOSDBG,100) NDTIME,(IDTIME(I),I=1,NDTIME)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF STATUS CODE IS ZERO
       IF (ISTAT.EQ.0) THEN
         WRITE (LP,110) CALLER
         CALL SULINE (LP,2)
         GO TO 50
         ENDIF
C
C  CHECK FOR VALID CALLING ROUTINE
      IF (CALLER.EQ.'WDGCR') GO TO 10
      IF (CALLER.EQ.'WDGCH') GO TO 20
      IF (CALLER.EQ.'WDSCR') GO TO 30
      IF (CALLER.EQ.'WDSCH') GO TO 40
      WRITE (LP,120) CALLER
      CALL SUERRS (LP,2,NUMERR)
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  'WDGCR' STATUS CODES
C
C  CHECK FOR VALID STATUS CODE
10     IF (ISTAT.GE.1.OR.ISTAT.LE.3) THEN
          ELSE
            WRITE (LP,130) CALLER,ISTAT
            CALL SUERRS (LP,2,NUMERR)
            GO TO 50
         ENDIF
C
C  CHECK FOR BLANK IDENTIFIER
      XID1=SRCID1
      CALL SUBLID (XID1,IERR)
C
      ROUTINE='WDGCR'
      ACTION='CREATE'
      FLTYPE='GOES'
C
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,140) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,160) ROUTINE,STAID,FLTYPE
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (LP,180) ROUTINE,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      WRITE (LP,130) CALLER,ISTAT
      CALL SUERRS (LP,2,NUMERR)
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  'WDGCH' STATUS CODES
C
C  CHECK FOR VALID STATUS CODE
20    IF (ISTAT.GE.1.OR.ISTAT.LE.4) THEN
         ELSE
            WRITE (LP,130) CALLER,ISTAT
            CALL SUERRS (LP,2,NUMERR)
            GO TO 50
         ENDIF
C
C  CHECK FOR BLANK IDENTIFIER
      XID1=SRCID1
      CALL SUBLID (XID1,IERR)
C
      ROUTINE='WDGCH'
      ACTION='CHANGE'
      FLTYPE='GOES'
C
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,140) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,150) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (LP,170) ROUTINE,STAID,FLTYPE
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.4) THEN
         WRITE (LP,180) ROUTINE,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      WRITE (LP,130) CALLER,ISTAT
      CALL SUERRS (LP,2,NUMERR)
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  'WDSCR' STATUS CODES
C
C  CHECK FOR VALID STATUS CODE
30    IF (ISTAT.GE.1.OR.ISTAT.LE.2) THEN
         ELSE
            WRITE (LP,130) CALLER,ISTAT
            CALL SUERRS (LP,2,NUMERR)
            GO TO 50
         ENDIF
C
      XID1=SRCID1
      XID2=SRCID2
      CALL SUBLID (XID1,IERR)
      CALL SUBLID (XID2,IERR)
C
      ROUTINE='WDSCR'
      ACTION='CREATE'
      FLTYPE='SASM'
C
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,140) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,160) ROUTINE,STAID,FLTYPE
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      WRITE (LP,130) CALLER,ISTAT
      CALL SUERRS (LP,2,NUMERR)
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  'WDSCH' STATUS CODES
C
C  CHECK FOR VALID STATUS CODE
40    IF (ISTAT.GE.1.OR.ISTAT.LE.3) THEN
         ELSE
            WRITE (LP,130) CALLER,ISTAT
            CALL SUERRS (LP,2,NUMERR)
            GO TO 50
         ENDIF
C
      XID1=SRCID1
      XID2=SRCID2
      CALL SUBLID (XID1,IERR)
      CALL SUBLID (XID2,IERR)
C
      ROUTINE='WDSCH'
      ACTION='CHANGE'
      FLTYPE='SASM'
C
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,140) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,150) ROUTINE,FLTYPE,ACTION,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (LP,170) ROUTINE,STAID,FLTYPE
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 50
         ENDIF
C
      WRITE (LP,130) CALLER,ISTAT
      CALL SUERRS (LP,2,NUMERR)
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SWDEST')
70    FORMAT (' CALLER=',A,3X,'ISTAT=',I2,3X,'STAID=',A,3X,
     *   'IDTYPE=',I2)
80    FORMAT (' NDLYTP=',I2,3X,'DLYTYP=',20(A,1X))
90    FORMAT (' NRRSTP=',I2,3X,'RRSTYP=',20(A,1X))
100   FORMAT (' NDTIME=',I2,3X,'IDTIME=',20(I6,1X))
110   FORMAT ('0*** NOTE - IN SWDEST - STATUS CODE IS ZERO FOR CALLER ',
     *   A,'.')
120   FORMAT ('0*** ERROR - IN SWDEST - INVALID CALLER CODE : ',A)
130   FORMAT ('0*** ERROR - IN SWDEST - ',A,' STATUS CODE NOT ',
     *   'RECOGNIZED : ',I3)
140   FORMAT ('0*** ERROR - IN ',A,' - SYSTEM ERROR ACCESSING ',
     *   'THE ',A,' CONTROL FILE WHILE TRYING TO ',A,' STATION ',A,'.')
150   FORMAT ('0*** ERROR - IN ',A,' - NO ENTRIES FOUND IN ',
     *   'THE ',A,' CONTROL FILE WHILE TRYING TO ',A,' STATION ',A,'.')
160   FORMAT ('0*** ERROR - IN ',A,' - STATION ',A,' ',
     *   'ALREADY EXISTS IN ',
     *   'THE ',A,' CONTROL FILE.')
170   FORMAT ('0*** WARNING - IN ',A,' - STATION ',A,' ',
     *   'NOT FOUND IN ',
     *   'THE ', A,' CONTROL FILE AND WILL BE CREATED.')
180   FORMAT ('0*** ERROR - IN ',A,' - INVALID DAILY DATA TYPE FOUND ',
     *   'FOR STATION ',A,'.')
190   FORMAT (' *** EXIT SWDEST')
C
      END
