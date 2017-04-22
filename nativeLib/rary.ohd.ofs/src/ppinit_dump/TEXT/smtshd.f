C MEMBER SMTSHD
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/04/94.13:12:31 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC ROUTINE FOR PRINTING TIME SERIES HEADERS
C DESC PPINIT COMMAND :  @DUMP TSHDR
C
      SUBROUTINE SMTSHD (NFLD,ISTAT)
C
      REAL TSHD(2)/4HTSHD,4HR   /
C
      DIMENSION CHAR(5),CHK(5)
      DIMENSION TSID(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smtshd.f,v $
     . $',                                                             '
     .$Id: smtshd.f,v 1.1 1995/09/17 19:13:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,180)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,190) NFLD
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
C
      ISTRT=-1
      LCHAR=5
      LCHK=5
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NXTFLD=1
      NPRINT=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0)
     *   CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) WRITE (IOSDBG,220) NFLD
         IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 160
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 160
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 30
         GO TO 40
30    IF (NFLD.EQ.1) CALL SUPCRD
      WRITE (LP,230) NFLD
      CALL SULINE (LP,2)
      ILPFND=0
      IRPFND=0
40    IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
      IF (NUMFLD.GT.1) GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK DUMP KEYWORD
      IF (CHK(1).EQ.TSHD(1).AND.CHK(2).EQ.TSHD(2)) GO TO 50
         WRITE (LP,200) TSHD,NFLD,(CHK(I),I=1,2)
         CALL SUERRS (LP,2,NUMERR)
50    IF (NFLD.EQ.1) CALL SUPCRD
      GO TO 10
C
C  CHECK FOR KEYWORD
90    CALL SUIDCK ('DMPG',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 160
         IF (LDEBUG.GT.0) WRITE (IOSDBG,210) CHK
         IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      GO TO (100,110),NXTFLD
C
C  SET IDENTIFIER
100   CALL SUBSTR (CHAR,1,8,TSID,1)
      NXTFLD=2
      GO TO 10
C
C  SET TIME SERIES TYPE
110   TYPE=CHAR(1)
      IF (LENGTH.LE.4) GO TO 130
         WRITE (LP,270) LENGTH,CHAR(1),CHAR(2)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 150
C
C  PRINT TIME SERIES HEADER
130   IPRERR=1
      CALL SPTSHD (TSID,TYPE,IPRERR,IPTRNX,IERR)
      NPRINT=NPRINT+1
C
150   NXTFLD=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF TIME SERIES PRINTED
160   IF (NPRINT.EQ.0) WRITE (LP,290)
      IF (NPRINT.GT.0) WRITE (LP,300) NPRINT
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,310)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SMTSHD')
190   FORMAT (' NFLD=',I2)
200   FORMAT ('0*** ERROR - IN SMTSHD - ',2A4,' WAS EXPECTED IN FIELD ',
     *   I2,3X,' BUT ',2A4,' WAS FOUND.')
210   FORMAT (' KEYWORD IS ',5A4)
220   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
230   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
270   FORMAT ('0*** ERROR - NUMBER OF CHARACTERS (',I2,
     *   ') IN TIME SERIES TYPE SPECIFIED (',2A4,
     *   ') EXCEEDS 4.')
290   FORMAT ('0*** NOTE - NO  TIME SERIES SUCCESSFULLY PRINTED.')
300   FORMAT ('0*** NOTE - ',I3,' TIME SERIES ',
     *   'SUCCESSFULLY PRINTED.')
310   FORMAT (' *** EXIT SMTSHD')
C
      END
