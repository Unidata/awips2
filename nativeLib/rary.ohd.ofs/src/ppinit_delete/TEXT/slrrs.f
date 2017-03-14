C MODULE SLRRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE STATION RRS PARAMETERS.
C
      SUBROUTINE SLRRS (LARRAY,ARRAY,NFLD,IRUNCK,ISTAT)
C
      CHARACTER*4 TYPE,RDISP,WDISP
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slrrs.f,v $
     . $',                                                             '
     .$Id: slrrs.f,v 1.4 1999/04/26 11:38:51 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,220)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,240) LARRAY,NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NDELTE=0
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) 	THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,270) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 200
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 200
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
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
      NUMFLD=NUMFLD+1
      IF (NUMFLD.GT.1) GO TO 60
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK PARAMETER TYPE
      TYPE=CHK
      IF (TYPE.NE.'RRS') THEN
         WRITE (LP,250) 'RRS',NFLD,CHK(1:LEN(CHK))
         CALL SUERRS (LP,2,-1)
         ENDIF
      GO TO 10
C
C  CHECK FOR KEYWORD
60    CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IRIDCK)
      IF (IRIDCK.EQ.2) GO TO 200
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'CHK=',CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
C
      IF (NUMERR.GT.0) GO TO 10
C      
      IF (NFLD.EQ.1) CALL SUPCRD
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      STAID=CHAR
C
C  UPDATE GENERAL STATION PARAMETERS
      IPTR=0
      IPRERR=0
      RDISP='OLD'
      INCLUDE 'scommon/callsrstan'
      IF (IERR.NE.0) THEN
         WRITE (LP,300) STAID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
C  CHECK IF STATION HAS RRS PARAMETERS
      DO 90 IGP=1,NGPS
         IF (GPS(IGP).EQ.'RRS') GO TO 100
90       CONTINUE
         WRITE (LP,310) 'RRS',STAID
         CALL SUWRNS (LP,2,-1)
         GO TO 10
C
C  UPDATE DATA GROUP CODES
100   IF (NGPS.GT.1) GO TO 110
         NGPS=0
         GO TO 130
110   GPS(IGP)=' '
      DO 120 I=1,NGPS
         IF (GPS(I).NE.' ') GO TO 120
         IF (I.EQ.NGPS) GO TO 120
            GPS(I)=GPS(I+1)
            GPS(I+1)=' '
120      CONTINUE
      NGPS=NGPS-1
C
C  CHECK IF RUNCHECK OPTION SPEFICIED
      IF (IRUNCK.EQ.1) GO TO 10
C
130   WDISP='OLD'
      IWRITE=1
      INCLUDE 'scommon/callswstan'
      IF (IERR.NE.0) THEN
         WRITE (LP,330) STAID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,340) STAID
         CALL SULINE (IOSDBG,1)
         IPRNT=1
         INCLUDE 'scommon/callspstan'
         ENDIF
C
C  READ RRS PARAMETERS
      IPTR=0
      IREAD=1
      INCLUDE 'scommon/callsrrrs'
      IF (IERR.NE.0) THEN
         WRITE (LP,320) STAID,IERR
         CALL SUERRS (LP,2,-1)
         GO TO 10
         ENDIF
      IF (LDEBUG.GT.0) THEN
         INCLUDE 'scommon/callsprrs'
         ENDIF
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      CALL SUDOPN (1,'PRD ',IERR)
      DO 180 I=1,NRRSTP
         IFUT=0
         ICKREF=0
         IPRERR=0
         CALL WPRDEL (STAID,RRSTYP(I),IFUT,ICKREF,IPRERR,IERR)
         IF (IERR.EQ.0) GO TO 170
            IF (IERR.EQ.1) THEN
               WRITE (LP,360) RRSTYP(I),STAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IERR.EQ.2) THEN
               WRITE (LP,290) RRSTYP(I),STAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IERR.GT.2) THEN
               WRITE (LP,380) IERR,RRSTYP(I),STAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            GO TO 180
170      CALL SUDWRT (1,'PRD ',IERR)
         WRITE (IOSDBG,390) RRSTYP(I),STAID
         CALL SULINE (LP,1)
180      CONTINUE
C
C  DELETE PARAMETERS FROM PREPROCESSOR PARAMETRIC DATA BASE
      TYPE='RRS'
      CALL WPPDEL (STAID,TYPE,IERR)
      IF (IERR.EQ.0) GO TO 190
         IF (IERR.EQ.1)THEN
            WRITE (LP,350) STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.GT.1) THEN
            WRITE (LP,400) IERR,STAID           
            CALL SUWRNS (LP,2,-1)
            ENDIF
         GO TO 10
C
190   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,410) STAID
         CALL SULINE (LP,1)
         ENDIF
      CALL SUDWRT (1,'PPP ',IERR)
C
C  STATION SUCCESSFULLY DELETED
      WRITE (LP,420) STAID
      CALL SULINE (LP,1)
      WRITE (LP,230)
      CALL SULINE (LP,1)
      NDELTE=NDELTE+1
      GO TO 10
C
C  PRINT NUMBER OF RRS STATIONS DELETED
200   IF (NDELTE.EQ.0) THEN
         WRITE (LP,430)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,440) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,450)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
220   FORMAT (' *** ENTER SLRRS')
230   FORMAT (' ')
240   FORMAT (' LARRAY=',I4,3X,'NFLD=',I2)
250   FORMAT ('0*** ERROR - IN SLRRS - THE CHARACTERS ''',A,
     *   ' ''WERE EXPECTED IN FIELD ',I2,' BUT ''',A,''' WERE FOUND.')
270   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
290   FORMAT ('0*** WARNING - ',A,' TIME SERIES FOR STATION ',A,
     *   'CANNOT BE DELETED BECAUSE IT IS STILL REFERENCED.')
300   FORMAT ('0*** WARNING - STAN PARAMETERS NOT FOUND ',
     *   'FOR STATION ',A,'.')
310   FORMAT ('0*** WARNING - ',A4,' PARAMETERS NOT DEFINED ',
     *   'FOR STATION ',A,'.')
320   FORMAT ('0*** ERROR - READING RRS PARAMETERS ',
     *   'FOR STATION ',A,'.')
330   FORMAT ('0*** ERROR - STATION GENERAL PARAMETERS ',
     *   'FOR STATION ',A,' NOT SUCCESSFULLY UPDATED.')
340   FORMAT (' GENL PARAMETERS FOR STATION ',A,' SUCCESSFULLY UPDATED')
350   FORMAT ('0*** WARNING - RRS  PARAMETERS  NOT FOUND ',
     *   'FOR STATION ',A,'.')
360   FORMAT ('0*** WARNING - ',A,' TIME SERIES NOT FOUND ',
     *   'FOR STATION ',A,'.')
380   FORMAT ('0*** WARNING - STATUS CODE (',I3,') FROM WPRDEL ',
     *   'ENCOUNTERED WHILE DELETING ',A,' TIME SERIES ',
     *   'FOR STATION ',A,'.')
390   FORMAT (' *** NOTE - ',A,' TIME SERIES DELETED ',
     *   'FOR STATION ',A,'.')
400   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WPPDEL ',
     *   'ENCOUNTERED WHILE DELETING RRS  STATION ',A,'.')
410   FORMAT (' RRS  PARAMETERS DELETED FOR STATION ',A)
420   FORMAT (' *** NOTE - RRS  PARAMETERS ',
     *   'FOR STATION ',A,' SUCCESSFULLY DELETED.')
430   FORMAT ('0*** NOTE - NO STATIONS WITH RRS  PARAMETERS ',
     *   'SUCCESSFULLY PROCESSED.')
440   FORMAT ('0*** NOTE - ',I3,' STATION RRS  PARAMETERS ',
     *   'SUCCESSFULLY DELETED.')
450   FORMAT (' *** EXIT SLRRS')
C
      END
