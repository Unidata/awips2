C MODULE SMPDFN
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT INDEX RECORD FOR SPECIFIED IDENTIFIER.
C
      SUBROUTINE SMPDFN (NFLD,ISTAT)
C
      CHARACTER*8 STAID
      CHARACTER*20 CHAR,CHK
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smpdfn.f,v $
     . $',                                                             '
     .$Id: smpdfn.f,v 1.2 1999/07/07 11:21:07 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' NFLD=',NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      ISTRT=0
      LCHAR=-LEN(CHAR)
      LCHK=-LEN(CHAR)
      ILPFND=0
      IRPFND=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
        ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,60) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 40
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 40
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,70) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DUMP',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 40
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET IDENTIFIER
      CALL SUBSTR (CHAR,1,8,STAID,1)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOGDB,*) ' STAID=',STAID
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) GO TO 10
C
C  CHECK IF STATION IDENTIFIER IS IN INDEX
      CALL PDFNDR (STAID,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,IERR)
      IF (IERR.EQ.0.AND.IFIND.GT.0) GO TO 20
         IF (IFIND.EQ.0) THEN
            WRITE (LP,80) STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.GT.0) THEN
            WRITE (LP,90) STAID,'PDFNDR',IERR
            CALL SUERRS (LP,2,-1)
            ENDIF
         GO TO 10
20    WRITE (LP,110) STAID
      CALL SULINE (LP,2)
      WRITE (LP,120) IFIND,ISIREC
      CALL SULINE (LP,1)
C
C  CHECK IF STATION HAS INTEGER NUMBER
      IDTYPE=0
      CALL RPDID (STAID,NBRSTA,IDTYPE,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,90) STAID,'PDFNDI',IERR
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
      IF (NBRSTA.EQ.0) THEN
         WRITE (LP,100) STAID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
C  CHECK IF STATION NUMBER IS IN INDEX
      CALL PDFNDI (NBRSTA,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,IERR)
      IF (IERR.EQ.0.AND.IFIND.GT.0) GO TO 30
         IF (IFIND.EQ.0) THEN
            WRITE (LP,80) STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.GT.0) THEN
            WRITE (LP,90) STAID,'PDFNDI',IERR
            CALL SUERRS (LP,2,-1)
            ENDIF
         GO TO 10
30    WRITE (LP,130) STAID
      CALL SULINE (LP,2)
      WRITE (LP,140) IFIND,ISIREC
      CALL SULINE (LP,1)
      GO TO 10
C
40    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER SMPDFN')
60    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
70    FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
80    FORMAT ('0*** WARNING - STATION ',A,' NOT FOUND IN ',
     *   'PREPROCESSOR DATA BASE.')
90    FORMAT ('0*** ERROR - ACCESSING STATION ',A,'. PDFNDR CODE=',
     *   I3,'.')
100   FORMAT ('0*** NOTE - STATION ',A,' DOES NOT HAVE AN ',
     *   'INTEGER STATION NUMBER.')
110   FORMAT ('0*** NOTE - STATION ',A,' FOUND IN PREPROCESSOR ',
     *   'DATA BASE CHARACTER INDEX.')
120   FORMAT (T13,'LOCATION IN CHARACTER INDEX IS ',I5,
     *   '. SIF RECORD IS AT RECORD ',I5,'.')
130   FORMAT ('0*** NOTE - STATION ',A,' FOUND IN PREPROCESSOR ',
     *   'DATA BASE INTEGER INDEX.')
140   FORMAT (T13,'LOCATION IN INTEGER INDEX IS ',I5,
     *   '. SIF RECORD IS AT RECORD ',I5,'.')
150   FORMAT (' *** EXIT SMPDFN')
C
      END
