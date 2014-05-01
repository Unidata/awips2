C MODULE SCTEMP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PUNCH STATION TEMP PARAMETER.
C
      SUBROUTINE SCTEMP (IPNCH,UNITS,IVTEMP,STAID,ITYOBS,
     *   TEMPCF,ITFMM,TEMPFE,IPMMMT,ITTAVR,ISTAT)
C
      CHARACTER*4 UNITS
      CHARACTER*4 OBSTYP,PFMM
      CHARACTER*8 CHAR
      CHARACTER*80 CARD/' '/
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimtemp'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/sctemp.f,v $
     . $',                                                             '
     .$Id: sctemp.f,v 1.2 1998/04/07 15:00:09 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('TEMP')
C
      ISTAT=0
C
      MCHAR=LEN(CHAR)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,80) IVTEMP
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH PCPN STATION IDENTIFIER
      IF (IPNCH.GT.0) THEN
         NPOS=1
         CALL UTOCRD (ICDPUN,NPOS,'STAN',4,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         CALL UTOCRD (ICDPUN,NPOS,STAID,8,1,CARD,3,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         CALL UPNCRD (ICDPUN,CARD)
         ENDIF
C
C  PUNCH 'TEMP' STARTING IN COLUMN 1
      NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,'TEMP',4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  READ MEAN MONTHLY MAX/MIN TEMPERATURES
      CALL RPP1MT (IPMMMT,TMPMAX,TMPMIN,IERR)
      IF (IERR.EQ.1) THEN
         WRITE (LP,90)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 50
         ENDIF
      IF (IERR.EQ.2) THEN
         WRITE (LP,100)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 50
         ENDIF
      IF (IERR.EQ.3) THEN
         WRITE (LP,110) IPMMMT
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 50
         ENDIF
C
C  PUNCH MEAN MONTHLY MAXIMUM TEMPERATURES
      CALL UTOCRD (ICDPUN,NPOS,'MX(',3,0,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
      NSPACE=1
      DO 10 I=1,12
         IF (I.EQ.12) NSPACE=0
         VALUE=TMPMAX(I)
         IF (UNITS.EQ.'METR') THEN
            CALL UDUCNV ('DEGF','DEGC',1,1,TMPMAX(I),VALUE,IERR)
            ENDIF
         NUMDEC=1
         CALL URELCH (VALUE,MCHAR,CHAR,NUMDEC,NFILL,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,120) VALUE,NUMDEC,CHAR
            CALL SULINE (IOSDBG,1)
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
10       CONTINUE
      CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  START NEW CARD
      CALL UPNCRD (ICDPUN,CARD)
      NPOS=6
C
C  PUNCH MEAN MONTHLY MINIMUM TEMPERATURE
      CALL UTOCRD (ICDPUN,NPOS,'MN(',3,0,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
      NSPACE=1
      DO 20 I=1,12
         IF (I.EQ.12) NSPACE=0
         VALUE=TMPMIN(I)
         IF (UNITS.EQ.'METR') THEN
            CALL UDUCNV ('DEGF','DEGC',1,1,TMPMIN(I),VALUE,IERR)
            ENDIF
         NUMDEC=1
         CALL URELCH (VALUE,MCHAR,CHAR,NUMDEC,NFILL,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,120) VALUE,NUMDEC,CHAR
            CALL SULINE (IOSDBG,1)
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
20       CONTINUE
      CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  START NEW CARD
      CALL UPNCRD (ICDPUN,CARD)
      NPOS=6
C
C  PUNCH TYPE OF DATA OBSERVED
      OBSTYP='????'
      IF (ITYOBS.EQ.1) THEN
         OBSTYP='MXMN'
         GO TO 30
         ENDIF
      IF (ITYOBS.EQ.2) THEN
         OBSTYP='INST'
         GO TO 30
         ENDIF
      IF (ITYOBS.EQ.3) THEN
         OBSTYP='BOTH'
         GO TO 30
         ENDIF
      IF (ITYOBS.EQ.4) THEN
         OBSTYP='SYN'
         GO TO 30
         ENDIF
30    CALL UTOCRD (ICDPUN,NPOS,OBSTYP,4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  PUNCH TIME INTERVAL OF INSTANTANEOUS DATA
      IF (ITYOBS.EQ.2.OR.ITYOBS.EQ.3) THEN
         IF (ITTAVR.GT.0) THEN
            CALL UINTCH (ITTAVR,MCHAR,CHAR,NFILL,IERR)
            CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,1,CARD,1,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 50
            ENDIF
         ENDIF
C
C  PUNCH CORRECTION FACTORS
      IF (TEMPCF(1).GT.0.0) THEN
         CALL UTOCRD (ICDPUN,NPOS,'CF(',3,0,CARD,0,14,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         NSPACE=1
         DO 40 I=1,2
            IF (I.EQ.2) NSPACE=0
            VALUE=TEMPCF(I)
            IF (UNITS.EQ.'METR') THEN
               CALL UDUCNV ('DEGF','DEGC',1,1,TEMPCF(I),VALUE,IERR)
               ENDIF
            NUMDEC=2
            CALL URELCH (VALUE,MCHAR,CHAR,NUMDEC,NFILL,IERR)
            CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 50
40          CONTINUE
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         ENDIF
C
C  PUNCH FORECAST MAX/MIN INDICATOR
      PFMM='FMM'
      IF (ITFMM.EQ.0) PFMM='NFMM'
      CALL UTOCRD (ICDPUN,NPOS,PFMM,4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  PUNCH ELEVATION WEIGHTING FACTOR
      IF (TEMPFE.GT.0.0) THEN
         CALL UTOCRD (ICDPUN,NPOS,'FE(',3,0,CARD,0,9,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         VALUE=TEMPFE
         IF (UNITS.EQ.'ENGL') THEN
C        GET CONVERSION FACTORS
            ICONV=2
            NVAL=1
            CALL UDUCNV ('KM  ','MI  ',ICONV,NVAL,CONV1,TCONV,IERR)
            CALL UDUCNV ('M   ','FT  ',ICONV,NVAL,CONV2,TCONV,IERR)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*)
     *            ' CONV1=',CONV1,
     *            ' CONV2=',CONV2,
     *            ' CONV1/CONV2=',CONV1/CONV2,
     *            ' '
               ENDIF
C        CONVERT VALUE
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*)
     *            ' CONV1=',CONV1,
     *            ' CONV2=',CONV2,
     *            ' CONV1/CONV2=',CONV1/CONV2,
     *            ' '
               CALL SULINE (IOSDBG,1)
               ENDIF
            VALUE=TEMPFE*(CONV1/CONV2)
            ENDIF
         NUMDEC=2
         CALL URELCH (VALUE,MCHAR,CHAR,NUMDEC,NFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 50
         ENDIF
C
      CALL UPNCRD (ICDPUN,CARD)
      GO TO 60
C
50    ISTAT=1
C
60    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,130)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SCTEMP')
80    FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
90    FORMAT ('0*** ERROR - IN SCTEMP - SYSTEM ERROR ACCESSING ',
     *   'MEAN MONTHLY MAX/MIN TEMPERATURES.')
100   FORMAT ('0*** ERROR - IN SCTEMP - ',
     *   'MEAN MONTHLY MAX/MIN TEMPERATURES ',
     *   'NOT DEFINED IN THE PREPROCESSOR PARAMETRIC DATA BASE.')
110   FORMAT ('0*** ERROR - IN SCTEMP - INVALID VALUE OF POINTER TO ',
     *   'MEAN TEMPERATURES : IPMMMT = ',I5)
120   FORMAT (' VALUE=',F7.2,3X,'NUMDEC=',I2,3X,'CHAR=',A)
130   FORMAT (' *** EXIT SCTEMP')
C
      END
