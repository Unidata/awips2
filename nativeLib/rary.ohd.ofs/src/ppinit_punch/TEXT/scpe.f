C MODULE SCPE
C-----------------------------------------------------------------------
C
C  ROUTINE TO PUNCH STATION PE PARAMETERS.
C
      SUBROUTINE SCPE (IPNCH,UNITS,IVPE,STAID,ANEMHT,ITYRAD,PECOR,PEB3,
     *   ISTAT)
C
      CHARACTER*4 UNITS
      CHARACTER*8 CHAR
      CHARACTER*80 CARD/' '/
C
      INCLUDE 'scommon/dimsta'      
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scpe.f,v $
     . $',                                                             '
     .$Id: scpe.f,v 1.2 1998/04/07 14:58:52 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('PE  ')
C
      ISTAT=0
C      
      MCHAR=LEN(CHAR)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130) IVPE
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH PE STATION IDENTIFIER
      IF (IPNCH.GT.0) THEN
         NPOS=1
         CALL UTOCRD (ICDPUN,NPOS,'STAN',4,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         CALL UTOCRD (ICDPUN,NPOS,STAID,8,1,CARD,3,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         CALL UPNCRD (ICDPUN,CARD)
         ENDIF
C
C  PUNCH 'PE' STARTING IN COLUMN 1
      NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,'PE',2,3,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  PUNCH ANEMOMETER HEIGHT
      VALUE=ANEMHT
      IF (UNITS.EQ.'ENGL') THEN      
         CALL UDUCNV ('M   ','FT  ',1,1,ANEMHT,VALUE,IERR)
         ENDIF
      NUMDEC=1
      CALL URELCH (VALUE,MCHAR,CHAR,NUMDEC,NFILL,IERR)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  PUNCH B3 PARAMETER
      NUMDEC=2
      CALL URELCH (PEB3,MCHAR,CHAR,NUMDEC,NFILL,IERR)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  PUNCH PRIMARY TYPE OF RADIATION DATA TO BE USED
      IF (ITYRAD.EQ.1) THEN
         CALL UTOCRD (ICDPUN,NPOS,'SKY',3,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         ENDIF   
      IF (ITYRAD.EQ.2) THEN   
         CALL UTOCRD (ICDPUN,NPOS,'SUN',3,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         ENDIF   
      IF (ITYRAD.EQ.3) THEN 
         CALL UTOCRD (ICDPUN,NPOS,'RAD',3,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         ENDIF
C
      IF (PECOR.NE.1.0) THEN
C     PUNCH CORRECTION FACTOR
         CALL UTOCRD (ICDPUN,NPOS,'CF(',3,0,CARD,0,9,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         NUMDEC=2
         CALL URELCH (PECOR,MCHAR,CHAR,NUMDEC,NFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 80
         ENDIF
C
      CALL UPNCRD (ICDPUN,CARD)
      GO TO 90
C
80    ISTAT=1
C
90    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,120)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' *** ENTER SCPE')
130   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
120   FORMAT(' *** EXIT SCPE')
C
      END
