C MODULE SWURRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE URRS RRS PARAMETERS.
C
      SUBROUTINE SWURRS (IVURRS,UNSD,NTYPE,TYPE,MNDAY,NMOBS,
     *   LARRAY,ARRAY,DISP,ISTAT)
C
      CHARACTER*4 DISP
      CHARACTER*8 BLNKID/' '/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION TYPE(NTYPE),MNDAY(NTYPE),NMOBS(NTYPE)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swurrs.f,v $
     . $',                                                             '
     .$Id: swurrs.f,v 1.2 1998/04/07 18:38:35 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('URRS')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,90) IVURRS,UNSD,LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=4+NTYPE*3
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,120) LARRAY,MINLEN
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
C
      NPOS=0
C      
C  STORE PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      ARRAY(NPOS)=IVURRS+.01
C
C  POSITIONS 2 AND 3 UNSD
      NPOS=NPOS+1
      ARRAY(NPOS)=UNSD
      NPOS=NPOS+1
      ARRAY(NPOS)=UNSD
C
C  STORE NUMBER OF RRS DATA TYPES
      NPOS=NPOS+1
      ARRAY(NPOS)=NTYPE+.01
C
C  STORE RRS DATA TYPE CODES
      DO 20 I=1,NTYPE
         NPOS=NPOS+1
         ARRAY(NPOS)=TYPE(I)
20       CONTINUE
C
C  STORE MINIMUM NUMBER OF DAYS OF DATA TO BE RETAINED ON PPDB
      DO 30 I=1,NTYPE
         NPOS=NPOS+1
         ARRAY(NPOS)=MNDAY(I)+.01
30       CONTINUE
C
C  STORE TYPICAL NUMBER OF OBSERVATIONS STORED FOR TYPE ON PPDB
      DO 40 I=1,NTYPE
         NPOS=NPOS+1
         ARRAY(NPOS)=NMOBS(I)+.01
40       CONTINUE
C
C  WRITE PARAMTER RECORD TO FILE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (BLNKID,'URRS',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.NE.0) THEN
         CALL SWPPST (BLNKID,'URRS',NPOS,IPTR,IERR)
         WRITE (LP,140) IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         GO TO 70
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         CALL SUPDMP ('URRS','BOTH',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
70    IF (ISTAT.EQ.0) THEN
         IF (DISP.EQ.'NEW') THEN
            WRITE (LP,150)
            CALL SULINE (LP,2)
            ENDIF
         IF (DISP.EQ.'OLD') THEN
            WRITE (LP,160)
            CALL SULINE (LP,2)
            ENDIF
         CALL SUDWRT (1,'PPP ',IERR)
         ENDIF
      IF (ISTAT.GT.0) THEN
         IF (DISP.EQ.'NEW') THEN
            WRITE (LP,170)
            CALL SULINE (LP,2)
            ENDIF
         IF (DISP.EQ.'OLD') THEN
            WRITE (LP,180)
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SWURRS')
90    FORMAT (' IVURRS=',I2,3X,'UNSD=',F7.2,3X,'LARRAY=',I5)
120   FORMAT ('0*** ERROR - IN SWURRS - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5)
140   FORMAT ('0*** ERROR - IN SWURRS - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I3)
150   FORMAT ('0*** NOTE - URRS PARAMETERS SUCCESSFULLY WRITTEN.')
160   FORMAT ('0*** NOTE - URRS PARAMETERS SUCCESSFULLY UPDATED.')
170   FORMAT ('0*** NOTE - URRS PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN.')
180   FORMAT ('0*** NOTE - URRS PARAMETERS NOT SUCCESSFULLY ',
     *   'UPDATED.')
190   FORMAT (' *** EXIT SWURRS')
C
      END
