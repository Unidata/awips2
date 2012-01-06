C MODULE SWORRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE RRS STATION ALPHABETICAL ORDER PARAMETERS.
C
      SUBROUTINE SWORRS (IVORRS,UNSD,IORDER,IPNTRS,NUMSTA,
     *   LARRAY,ARRAY,ISTAT)
C
C
      INTEGER*2 IWORK(1)
      CHARACTER*8 BLNK8/' '/
C      
      DIMENSION ARRAY(LARRAY)
      DIMENSION IPNTRS(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
      EQUIVALENCE (SWORK(1),IWORK(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/sworrs.f,v $
     . $',                                                             '
     .$Id: sworrs.f,v 1.2 1998/04/07 18:33:58 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('ORRS')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'IVORRS=',IVORRS,
     *      'UNSD=',UNSD,
     *      'NUMSTA=',NUMSTA,
     *      'LARRAY=',LARRAY,
     *      ''
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  STORE RRS POINTERS IN WORK ARRAY
      NSTA=0
      MAXSTA=LSWORK*2
      DO 10 I=1,NUMSTA
         IF (IPNTRS(I).EQ.0) GO TO 10
            NSTA=NSTA+1
            IF (NSTA.GT.MAXSTA) THEN
               WRITE (LP,50) MAXSTA
               CALL SUERRS (LP,2,-1)
               ISTAT=1
               GO TO 20
               ENDIF
             IWORK(NSTA)=IPNTRS(I)
10       CONTINUE
C
      IF (NSTA.EQ.0) THEN
         WRITE (LP,40) NUMSTA
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 20
         ENDIF
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=5+NSTA/2-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,60) LARRAY,MINLEN
         CALL SULINE (LP,2)
         ISTAT=1
         CALL SUERRS (LP,2,-1)
         GO TO 20
         ENDIF
C
      NPOS=0
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      ARRAY(NPOS)=IVORRS+.01
C
C  STORE INDICATOR HOW LIST WAS ORDERED
      NPOS=NPOS+1
      ARRAY(NPOS)=IORDER+.01
C
C  NEXT 2 POSITIONS ARE UNSD
      NPOS=NPOS+1
      ARRAY(NPOS)=UNSD
      NPOS=NPOS+1
      ARRAY(NPOS)=UNSD
C
C  STORE NUMBER STATIONS IN LIST
      NPOS=NPOS+1
      ARRAY(NPOS)=NSTA+.01
C
C  STORE RECORD LOCATION OF PARAMETERS IN PARAMETRIC DATA BASE
      IF (NSTA.GT.0) THEN
         IPOS=NPOS*4+1
         CALL SUBSTR (IWORK(1),1,NSTA*2,ARRAY,IPOS)
         NPOS=NPOS+(NSTA+1)/2
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 20
         ENDIF
C
C  WRITE PARAMETER RECORD
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPTR=0
      CALL WPPREC (BLNK8,'ORRS',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.NE.0) THEN
         CALL SWPPST (BLNK8,'ORRS',NPOS,IPTR,IERR)
         WRITE (LP,70)
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTAT.EQ.0) THEN
         WRITE (LP,80)
         CALL SULINE (LP,2)
         CALL SUDWRT (1,'PPP ',IERR)
         IF (LDEBUG.GT.0) THEN
            CALL SUPDMP ('ORRS','REAL',0,NPOS,ARRAY,ARRAY)
            ENDIF
         ENDIF
      IF (ISTAT.GT.0) THEN
         WRITE (LP,90)
         CALL SULINE (LP,2)
         ENDIF
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SWORRS')
40    FORMAT ('0*** ERROR - IN SWORRS - NONE OF THE ',I4,
     *   ' RRS STATIONS HAVE POSITIVE RRS PARAMETER RECORD NUMBERS.')
50    FORMAT ('0*** ERROR - IN SWORRS - NUMBER OF STATIONS TO BE ',
     *   'PROCESSED EXCEEDS MAXIMUM (',I5,').')
60    FORMAT ('0*** ERROR - IN SWORRS - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I6,3X,
     *   'NUMBER OF WORDS NEEDED=',I6)
70    FORMAT ('0*** ERROR - IN SWORRS - UNSUCCESSFUL CALL TO WPPREC.')
80    FORMAT ('0*** NOTE - ORRS PARAMETERS SUCCESSFULLY WRITTEN.')
90    FORMAT ('0*** NOTE - ORRS PARAMETERS NOT SUCCESSFULLY WRITTEN.')
100   FORMAT (' *** EXIT SWORRS')
C
      END
