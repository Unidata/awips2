C MODULE SWPPST
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES FOR THE STATUS CODE FROM WPPREC.
C
      SUBROUTINE SWPPST (PARMID,PARMTP,LARRAY,IPTR,ISTAT)
C
      CHARACTER*4 PARMTP
      CHARACTER*8 PARMID
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swppst.f,v $
     . $',                                                             '
     .$Id: swppst.f,v 1.3 2002/02/11 21:08:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'ENTER SWPPST'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR STATUS CODE=0
      IF (ISTAT.EQ.0) THEN
         WRITE (LP,30)
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
C  CHECK FOR STATUS CODE
      IF (ISTAT.GE.1.OR.ISTAT.LE.6) THEN
         ELSE
            WRITE (LP,40) ISTAT
            CALL SUERRS (LP,2,-1)
            GO TO 10
          ENDIF
C
C  CHECK FOR BLANK IDENTIFIER
      CALL SUBLID (PARMID,IERR)
C
C  SYSTEM ERROR ACCESSING FILE
      IF (ISTAT.EQ.1) THEN
         WRITE (LP,50) PARMTP,PARMID
         CALL SUERRS (LP,2,-1)
         GO TO 10
         ENDIF
C
C  PARAMETER FILE IS FULL
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,60) PARMTP,PARMID
         CALL SUERRS (LP,2,-1)
         WRITE (LP,70) LARRAY
         CALL SULINE (LP,1)
         GO TO 10
         ENDIF
C
C  INVALID IDENTIFIER
      IF (ISTAT.EQ.3) THEN
         WRITE (LP,80) PARMID
         CALL SUERRS (LP,2,-1)
         WRITE (LP,90) PARMTP
         CALL SULINE (LP,1)
         GO TO 10
         ENDIF
C
C  PARAMETER TYPE OR IDENTIFIER DO NOT MATCH
      IF (ISTAT.EQ.4) THEN
         WRITE (LP,100) PARMTP,PARMID
         CALL SUERRS (LP,2,-1)
         WRITE (LP,110)
         CALL SULINE (LP,1)
         GO TO 10
         ENDIF
C
C  PARAMETER TYPE NOT IN DIRECTORY
      IF (ISTAT.EQ.5) THEN
         WRITE (LP,120) PARMTP,PARMID
         CALL SUERRS (LP,2,-1)
         GO TO 10
         ENDIF
C
C  RECORD NUMBER OUT OF RANGE
      IF (ISTAT.EQ.6) THEN
         WRITE (LP,130) IPTR,PARMTP,PARMID
         CALL SUERRS (LP,2,-1)
         GO TO 10
         ENDIF
C
10    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'EXIT SWPPST'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0*** NOTE - IN SWPPST - WPPREC STATUS CODE IS ZERO.')
40    FORMAT ('0*** ERROR - IN SWPPST - STATUS CODE ',I2,
     *   ' NOT RECOGNIZED.')
50    FORMAT ('0*** ERROR - IN WPPREC - SYSTEM ERROR WHILE WRITING ',
     *   'PARAMETER TYPE ',A,' FOR IDENTIFIER ',A,' TO DATA FILE.')
60    FORMAT ('0*** ERROR - IN WPPREC - FILE INTO WHICH ',A,
     *   ' PARAMETER RECORD FOR IDENTIFIER ',A,' IS TO BE ',
     *   'WRITTEN IS FULL.')
70    FORMAT (T26,'RECORD CANNOT BE ADDED OR UPDATED. ',
     *   'NUMBER OF WORDS IN ARRAY IS ',I5,'.')
80    FORMAT ('0*** ERROR - IN WPPREC - ',A,' IS AN INVALID ',
     *   'IDENTIFIER BECAUSE IT IS ONE OF THE RESERVED WORDS. ')
90    FORMAT (T26,'THE TYPE OF PARAMETER ARRAY BEING WRITTEN WAS ',
     *     A)
100   FORMAT ('0*** ERROR - IN WPPREC - PARAMETER TYPE (',A,') AND ',
     *   'IDENTIFIER (',A,') DO NOT MATCH THAT FOUND IN PARAMETER ',
     *   'RECORD.')
110   FORMAT (T26,'EXISTING PARAMETER RECORD.')
120   FORMAT ('0*** ERROR - IN WPPREC - PARAMETER TYPE ',A,
     *   ' SPECIFIED FOR IDENTIFIER ',A,' IS NOT DEFINED.')
130   FORMAT ('0*** ERROR - IN WPPREC - RECORD NUMBER (',I5,
     *   ') IS OUT OF RANGE ',
     *   'FOR PARAMETER TYPE ',A,' AND IDENTIFIER ',A,'.')
C
      END
