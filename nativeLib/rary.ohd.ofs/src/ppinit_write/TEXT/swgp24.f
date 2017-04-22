C MEMBER SWGP24
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/09/94.14:20:39 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC WRITE GP24 PARAMETERS
C
      SUBROUTINE SWGP24 (IVGP24,NGP24,IGP24,NSGP24,ISGP24,
     *     UNUSED,LARRAY,ARRAY,IPTR,DISP,ISTAT)
C
C
      CHARACTER*4 DISP
      CHARACTER*8 BLNK8/' '/
      INTEGER*2 IGP24(1),ISGP24(1)
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swgp24.f,v $
     . $',                                                             '
     .$Id: swgp24.f,v 1.1 1995/09/17 19:16:09 dws Exp $
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
      LDEBUG=ISBUG('GP24')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,40) IVGP24,UNUSED,LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=5+NGP24+(NSGP24+1)/2
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,50) LARRAY,MINLEN
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 10
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVGP24+.01
C
C  STORE NUMBER OF STATIONS IN LIST
      ARRAY(2)=NGP24+.01
C
C  STORE NUMBER OF STATIONS THAT SHARE SAME GRID POINT ADDRESS
      ARRAY(3)=NSGP24+.01
C
C  POSITIONS 4 AND 5 ARE UNUSED
      ARRAY(4)=UNUSED
      ARRAY(5)=UNUSED
C
      NPOS=5
C
C  STORE STATION GRID-POINT ADDRESS
      CALL SUBSTR (IGP24(1),1,NGP24*4,ARRAY(NPOS+1),1)
      NPOS=NPOS+NGP24
C
C  STORE STATION GRID-POINT ADDRESS
      CALL SUBSTR (ISGP24(1),1,NSGP24*2,ARRAY(NPOS+1),1)
      NPOS=NPOS+(NSGP24+1)/2
C
C  WRITE PARAMETER RECORD TO FILE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (BLNK8,'GP24',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (BLNK8,'GP24',NPOS,IPTR,IERR)
         WRITE (LP,60)
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         GO TO 10
         ENDIF
C
C  PARAMETERS SUCCESSFULLY WRITTEN
      CALL SUDWRT (1,'PPP ',IERR)
      IF (DISP.EQ.'NEW') WRITE (LP,70) 'WRITTEN'
      IF (DISP.EQ.'OLD') WRITE (LP,70) 'UPDATED'
      CALL SULINE (LP,2)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('GP24','BOTH',0,NPOS,ARRAY,ARRAY)
         CALL SUPDMP ('GP24','INT2',0,NPOS,ARRAY,ARRAY)
         ENDIF
      GO TO 20
C
C  PARAMETERS NOT SUCCESSFULLY WRITTEN
10    IF (DISP.EQ.'NEW') WRITE (LP,90) 'WRITTEN'
      IF (DISP.EQ.'OLD') WRITE (LP,90) 'UPDATED'
      CALL SULINE (LP,2)
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SWGP24')
40    FORMAT (' IVGP24=',I2,3X,'UNUSED=',F7.2,3X,'LARRAY=',I5)
50    FORMAT ('0*** ERROR - IN SWGP24 - NOT ENOUGH SPACE IN PARAMETER ',
     *     'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *     'NUMBER OF WORDS NEEDED=',I5,'.')
60    FORMAT ('0*** ERROR - IN SWGP24 - UNSUCCESSFUL CALL TO WPPREC.')
70    FORMAT ('0*** NOTE - GP24 PARAMETERS SUCCESSFULLY ',A,'.')
90    FORMAT ('0*** NOTE - GP24 PARAMETERS NOT SUCCESSFULLY ',A,'.')
100   FORMAT (' *** EXIT SWGP24')
C
      END
