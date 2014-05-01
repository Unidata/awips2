C MODULE SUDCLS
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE PREPROCESSOR DATA BASE, PREPROCESSOR
C  PARAMETRIC DATA BASE, PROCESSED DATA BASE AND DATA ENTRY
C  CONTROL INFORMATION TO FILE AND CLOSE ALL FILES.
C
      SUBROUTINE SUDCLS (ISTAT)
C
      INCLUDE 'uiox'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudcls.f,v $
     . $',                                                             '
     .$Id: sudcls.f,v 1.2 2001/06/13 13:32:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUDCLS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
C  SET INDICATORS IF NO CONTROL RECORDS TO BE UPDATED
      IDBWRT(7)=0
      IDBWRT(8)=0
      IDBWRT(9)=0
      IDBWRT(10)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IPOS=4
      IF (IDBWRT(IPOS).EQ.0) GO TO 10
C
C  UPDATE PREPROCESSOR DATA BASE FILE CONTROL INFORMATION
C
      CALL WPPDCO (IERR)
      IDXTYP=0
      CALL WPDHSH (IDXTYP,IERR2)
      IF (IERR.GT.0.OR.IERR2.GT.0) THEN
         WRITE (LP,100) IERR,IERR2
         CALL SUERRS (LP,2,-1)
         GO TO 10
         ENDIF
      WRITE (LP,110)
      CALL SULINE (LP,2)
      IDBWRT(IPOS)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    IPOS=5
      IF (IDBWRT(IPOS).EQ.0) GO TO 20
C
C  UPDATE PREPROCESSOR PARAMETRIC DATA BASE FILE CONTROL INFORMATION
C
      CALL WPPPCO (IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,120) IERR
         CALL SUERRS (LP,2,-1)
         GO TO 20
         ENDIF
      WRITE (LP,130)
      CALL SULINE (LP,2)
      IDBWRT(5)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    IPOS=6
      IF (IDBWRT(IPOS).EQ.0) GO TO 30
C
C  UPDATE PROCESSED DATA BASE FILE CONTROL INFORMATION
C
      CALL WPDBCO (IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,140) IERR
         CALL SUERRS (LP,2,-1)
         GO TO 30
         ENDIF
      WRITE (LP,150)
      CALL SULINE (LP,2)
      IDBWRT(IPOS)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    IPOS=7
      IF (IDBWRT(IPOS).EQ.0) GO TO 40
C
C  NO FC FILE CONTROL INFORMATION TO BE UPDATED
C
      IDBWRT(IPOS)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    IPOS=8
      IF (IDBWRT(IPOS).EQ.0) GO TO 50
C
C  NO SASM FILE CONTROL INFORMATION TO BE UPDATED
C
      IDBWRT(IPOS)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    IPOS=9
      IF (IDBWRT(IPOS).EQ.0) GO TO 60
C
C  NO GOES FILE CONTROL INFORMATION TO BE UPDATED
C
      IDBWRT(IPOS)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    IPOS=10
      IF (IDBWRT(10).EQ.0) GO TO 70
C
C  NOT USED
C
      IDBWRT(10)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF REORDER PROGRAM
70    IF (IORDER.EQ.0) GO TO 80
C
      IAMORD=1
C
C  UPDATE NEW PREPROCESSOR DATA BASE FILE CONTROL INFORMATION
      IDXTYP=0
      CALL WPDHSH (IDXTYP,IERR)
      CALL WPPDCO (IERR)
      CALL SULINE (LP,2)
      WRITE (LP,160)
C
C  UPDATE NEW PARAMETRIC DATA BASE FILE CONTROL INFORMATION
      CALL WPPPCO (IERR)
      CALL SULINE (LP,2)
      WRITE (LP,170)
C
C  UPDATE NEW PROCESSED DATA BASE FILE CONTROL INFORMATION
      CALL WPDBCO (IERR)
      CALL SULINE (LP,2)
      WRITE (LP,180)
C
C  CLOSE ALL FILES TO WRITE FROM BUFFER TO DATA FILE
80    CALL SUFCLS
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUDCLS'
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('0*** ERROR - IN SUDCLS - PREPROCESSOR DATA BASE ',
     *   'FILE INFORMATION NOT SUCCESSFULLY WRITTEN. IERR=',I2,3X,
     *   'IERR2=',I2)
110   FORMAT ('0*** NOTE - PREPROCESSOR ',
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
120   FORMAT ('0*** ERROR - IN SUDCLS - PARAMETRIC ',2X,
     *   'DATA BASE FILE CONTROL INFORMATION NOT SUCCESSFULLY ',
     *   'WRITTEN. IERR=',I2)
130   FORMAT ('0*** NOTE - PARAMETRIC ',2X,
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
140   FORMAT ('0*** ERROR - IN SUDCLS - PROCESSED ',3X,
     *   'DATA BASE FILE CONTROL INFORMATION NOT SUCCESSFULLY ',
     *   'UPDATED. IERR=',I2)
150   FORMAT ('0*** NOTE - PROCESSED ',3X,
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
160   FORMAT ('0*** NOTE - NEW PREPROCESSOR ',
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
170   FORMAT ('0*** NOTE - NEW PARAMETRIC ',2X,
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
180   FORMAT ('0*** NOTE - NEW PROCESSED ',3X,
     *   'DATA BASE FILE CONTROL INFORMATION SUCCESSFULLY UPDATED.')
C
      END
