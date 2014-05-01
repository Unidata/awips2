C MODULE SWPLST
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES BASED ON STATUS CODE FROM WPPDEL.
C
      SUBROUTINE SWPLST (TYPERR,PARMID,TYPE,ISTAT)
C
C
      CHARACTER*(*) TYPERR,PARMID,TYPE
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swplst.f,v $
     . $',                                                             '
     .$Id: swplst.f,v 1.2 1998/04/07 18:36:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR STATUS CODE OR ZERO
      IF (ISTAT.EQ.0) THEN
         WRITE (LP,80) ISTAT
         CALL SULINE (LP,2)
         GO TO 60
         ENDIF
C
C  CHECK FOR INVALID STATUS CODE
      IF (ISTAT.LT.1.OR.ISTAT.GT.5) THEN
         WRITE (LP,90) ISTAT
         CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
C  CHECK FOR BLANK IDENTIFIER
      CALL SUBLID (PARMID,IERR)
C
C  PARAMETER RECORD NOT FOUND
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,100) TYPERR(1:LENSTR(TYPERR)),TYPE,PARMID
         IF (TYPERR.EQ.'NOTE') CALL SULINE (LP,2)
         IF (TYPERR.EQ.'WARNING') CALL SUWRNS (LP,2,-1)
         IF (TYPERR.EQ.'ERROR') CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
C  PARAMETER TYPE NOT DEFINED IN FILES
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,120) TYPERR(1:LENSTR(TYPERR)),TYPE,PARMID
         IF (TYPERR.EQ.'NOTE') CALL SULINE (LP,2)
         IF (TYPERR.EQ.'WARNING') CALL SUWRNS (LP,2,-1)
         IF (TYPERR.EQ.'ERROR') CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
C  SYSTEM ERROR ACCESSING PARAMETER FILE
      IF (ISTAT.EQ.3) THEN
         WRITE (LP,140) TYPERR(1:LENSTR(TYPERR)),PARMID,TYPE
         IF (TYPERR.EQ.'NOTE') CALL SULINE (LP,2)
         IF (TYPERR.EQ.'WARNING') CALL SUWRNS (LP,2,-1)
         IF (TYPERR.EQ.'ERROR') CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
C  DATA TYPES OR IDENTIFIERS DO NOT MATCH
      IF (ISTAT.EQ.4) THEN
         WRITE (LP,150) TYPERR(1:LENSTR(TYPERR)),PARMID,TYPE
         IF (TYPERR.EQ.'NOTE') CALL SULINE (LP,2)
         IF (TYPERR.EQ.'WARNING') CALL SUWRNS (LP,2,-1)
         IF (TYPERR.EQ.'ERROR') CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
C  DATA TYPES OR IDENTIFIERS DO NOT MATCH
      IF (ISTAT.EQ.5) THEN
         WRITE (LP,160) PARMID,TYPE
         CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
C
60     IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,170)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SWPLST')
80    FORMAT ('0*** NOTE - IN SWPLST - WPPDEL STATUS CODE=',I2)
90    FORMAT ('0*** ERROR - IN SWPLST - STATUS CODE NOT RECOGNIZED : ',
     *   I2)
100   FORMAT ('0*** ',A,' - ',A,' PARAMETERS NOT FOUND ',
     *   'FOR IDENTIFIER ',A,'.')
120   FORMAT ('0*** ',A,' - IN WPPDEL - PARAMETER TYPE ',A,
     *   ' NOT DEFINED IN DATA BASE FOR IDENTIFIER ',A,'.')
140   FORMAT ('0*** ',A,' - IN WPPDEL - SYSTEM ERROR ACCESSING ',
     *   'PARAMETER FILE FOR IDENTIFIER ',A,' AND TYPE ',A,'.')
150   FORMAT ('0*** ',A,' - IN WPPDEL - SYSTEM ERROR ACCESSING ',
     *   'INDEX FOR IDENTIFIER ',A,' AND TYPE ',A,'.')
160   FORMAT ('0*** ERROR - IN WPPDEL - PARAMETER TYPE TO BE WRITTEN ',
     *   'FOR STATION ',A,' (',A,') DOES NOT MATCH PARAMETER TYPE ',
     *   'IN THE ' /
     *   T15,'EXISTING PARAMETER RECORD.')
170   FORMAT (' *** EXIT SWPLST')
C
      END
