C MODULE SPORRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT RRS STATION ALPHABETICAL ORDER.
C
      SUBROUTINE SPORRS (IVORRS,UNUSED,ISORT,IPNTRS,NSTA,LARRAY,ARRAY,
     *   ISTAT)
C
      CHARACTER*8 TYPMSG
      INTEGER*2 IPNTRS(1)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(1)
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sporrs.f,v $
     . $',                                                             '
     .$Id: sporrs.f,v 1.2 1998/04/07 18:00:52 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('ORRS')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,50)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER AND NUMBER OF STATIONS
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,70) IVORRS
         CALL SULINE (LP,2)
         WRITE (LP,80) NSTA
         CALL SULINE (LP,2)
         ENDIF
C
      IF (NSTA.EQ.0) THEN
         WRITE (LP,80) NSTA
         CALL SULINE (LP,2)
         GO TO 20
         ENDIF
C
C  PRINT HOW LIST WAS SORTED
      IF (ISORT.EQ.1) THEN
         WRITE (LP,90)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISORT.EQ.2) THEN
         WRITE (LP,100)
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT RRS PPPDB POINTER, STATION NAME AND DESCRIPTION
      WRITE (LP,110)
      CALL SULINE (LP,3)
      DO 10 I=1,NSTA
         STAID=' '
         IPRERR=1
         IPTR=IPNTRS(I)
         IREAD=1
         INCLUDE 'scommon/callsrrrs'
         IF (IERR.NE.0) THEN
            ISTAT=1
            GO TO 10
            ENDIF
         TYPMSG='ERROR'
         CALL SUGTDS (STAID,DESCRP,STATE,STACOR,STAELV,LARRAY,ARRAY,
     *      TYPMSG,IERR)
         IF (IERR.NE.0) THEN
            ISTAT=1
            GO TO 10
            ENDIF
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,50)
            CALL SULINE (LP,2)
            WRITE (LP,60)
            CALL SULINE (LP,2)
            WRITE (LP,110)
            CALL SULINE (LP,3)
            ENDIF
         WRITE (LP,120) I,IPTR,STATE,STAID,DESCRP
         CALL SULINE (LP,1)
10    CONTINUE
      WRITE (LP,130)
      CALL SULINE (LP,2)
C
C  PRINT NUMBER OF UNUSED POSITIONS
20    IF (LDEBUG.GT.0) THEN
         NUNUSD=2
         WRITE (LP,140) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SPORRS')
40    FORMAT ('0',132('-'))
50    FORMAT ('0*--> ORRS PARAMETERS ',
     *   '(RRS STATION ALPHABETICAL ORDER)')
60    FORMAT (' ')
70    FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
80    FORMAT ('0NUMBER OF RRS STATIONS = ',I4)
90    FORMAT ('0LIST ORDERED BY STATE AND STATION IDENTIFIER')
100   FORMAT ('0LIST ORDERED BY STATE AND STATION DESCRIPTION')
110   FORMAT ('0',8X,'PNTR ',3X,
     *      'STATE',3X,
     *      'IDENTIFIER',3X,
     *      'DESCRIPTION' /
     *   9X,5('-'),3X,
     *      5('-'),3X,
     *      10('-'),3X,
     *      20('-'))
120   FORMAT (3X,I4,2X,I5,5X,A2,4X,A,5X,A)
130   FORMAT ('0',8X,'PNTR = RECORD NUMBER OF THE RRS PARAMETER ',
     *   'ARRAY IN THE PREPROCESSOR PARAMETRIC DATA BASE.')
140   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
150   FORMAT (' *** EXIT SPORRS')
C
      END
