C MEMBER SWFMPO
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C DESC WRITE FUTURE MAP COMPUTATIONAL ORDER
C
      SUBROUTINE SWFMPO (IVFMPO,UNUSED,NFMPID,FMPID,LARRAY,ARRAY,NUMERR,
     *   ISTAT)
C
C
      CHARACTER*8 BLNK8/' '/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION FMPID(2,1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swfmpo.f,v $
     . $',                                                             '
     .$Id: swfmpo.f,v 1.1 1995/09/17 19:22:16 dws Exp $
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
      LDEBUG=ISBUG('FMPO')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,40) IVFMPO,UNUSED,NFMPID,LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=5+2*NFMPID-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,50) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 20
         ENDIF
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVFMPO+.01
C
C  POSITIONS 2 AND 3 ARE UNUSED
      ARRAY(2)=UNUSED
      ARRAY(3)=UNUSED
C
C  STORE NUMBER OF FUTURE MAP AREAS
      ARRAY(4)=NFMPID+.01
C
      NPOS=4
C
C  CHECK NUMBER OF FUTURE MAP AREAS SPECIFIED
      IF (NFMPID.EQ.0) THEN
         WRITE (LP,60)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 20
         ENDIF
C
C  STORE FUTURE MAP IDENTIFIERS
      DO 10 I=1,NFMPID
         NPOS=NPOS+1
         ARRAY(NPOS)=FMPID(1,I)
         NPOS=NPOS+1
         ARRAY(NPOS)=FMPID(2,I)
10       CONTINUE
C
C  WRITE PARAMTER RECORD TO FILE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (BLNK8,'FMPO',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (BLNK8,'FMPO',NPOS,IPTR,IERR)
         ISTAT=1
         GO TO 20
         ENDIF
C
      WRITE (LP,70)
      CALL SULINE (LP,2)
      CALL SUDWRT (1,'PPP ',IERR)
C
      IF (LDEBUG.GT.0) CALL SUPDMP ('FMPO','BOTH',0,NPOS,ARRAY,ARRAY)
C
20    IF (ISTRCE.GT.0) WRITE (IOSDBG,80)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SWFMPO')
40    FORMAT (' IVMPCO=',I2,3X,'UNUSED=',F7.2,3X,'NFMPID=',I5,3X,
     *   'LARRAY=',I5)
50    FORMAT ('0*** ERROR - IN SWFMPO - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5,'.')
60    FORMAT ('0*** ERROR - IN SWFMPO - NO FUTURE MAP AREA ',
     *   'IDENTIFIERS SPECIFIED.')
70    FORMAT ('0*** NOTE - FMPO PARAMETERS SUCCESSFULLY WRITTEN.')
80    FORMAT (' *** EXIT SWFMPO')
C
      END
