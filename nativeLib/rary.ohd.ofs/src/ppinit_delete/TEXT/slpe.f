C MODULE SLPE
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE STATION PE PARAMETERS.
C
      SUBROUTINE SLPE (LARRAY,ARRAY,NFLD,IRUNCK,ISTAT)
C
      CHARACTER*4 TYPE,RDISP,WDISP
      CHARACTER*20 CHAR/' '/,CHK/' '/
C      
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'scommon/dimstan'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slpe.f,v $
     . $',                                                             '
     .$Id: slpe.f,v 1.2 1998/04/07 15:18:19 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,200) LARRAY,NFLD
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
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,230) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 170
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 170
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 30
         GO TO 40
30    IF (NFLD.EQ.1) CALL SUPCRD
      WRITE (LP,240) NFLD
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
      IF (NUMFLD.GT.1) GO TO 60
C
      IF (NFLD.EQ.1) CALL SUPCRD
C      
C  CHECK PARAMETER TYPE
      TYPE=CHK
      IF (TYPE.NE.'PE') THEN
         WRITE (LP,210) 'PE',NFLD,CHK(1:LENSTR(CHK))
         CALL SUERRS (LP,2,-1)
         ENDIF
      GO TO 10
C
C  CHECK FOR KEYWORD
60    CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IRIDCK)
      IF (IRIDCK.EQ.2) GO TO 170
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'CHK=',CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
C
      IF (NUMERR.GT.0) GO TO 10
      IF (NFLD.EQ.1) CALL SUPCRD
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CALL SUBSTR (CHAR,1,8,STAID,1)
C
C  UPDATE GENERAL STATION PARAMETERS
      IPTR=0
      IPRERR=0
      RDISP='OLD'
      INCLUDE 'scommon/callsrstan'
      IF (IERR.NE.0) THEN
         WRITE (LP,260) STAID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
C  CHECK IF STATION HAS PE PARAMETERS
      DO 90 IGP=1,NGPS
         IF (GPS(IGP).EQ.TYPE) GO TO 100
90       CONTINUE
      WRITE (LP,270) TYPE,STAID
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
      IF (IERR.EQ.0) GO TO 140
         WRITE (LP,280) STAID
         CALL SULINE (LP,2)
         GO TO 10
C
140   WRITE (LP,290) STAID
      CALL SULINE (LP,2)
C
      IF (LDEBUG.GT.0) THEN
         IPRNT=1
         INCLUDE 'scommon/callspstan'
         ENDIF
C
C  DELETE PARAMETERS FROM PREPROCESSOR PARAMETRIC DATA BASE
      TYPE='PE'
      CALL WPPDEL (STAID,TYPE,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,300) STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.GT.1) THEN
            WRITE (LP,310) IERR,STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         GO TO 10
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,320) STAID
         CALL SULINE (LP,1)
         ENDIF
      CALL SUDWRT (1,'PPP ',IERR)
C
C  STATION SUCCESSFULLY DELETED
      WRITE (LP,330) STAID
      CALL SULINE (LP,2)
      NDELTE=NDELTE+1
      GO TO 10
C
C  PRINT NUMBER OF PE STATIONS DELETED
170   IF (NDELTE.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,350) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,360)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT (' *** ENTER SLPE')
200   FORMAT (' LARRAY=',I5,3X,'NFLD=',I2)
210   FORMAT ('0*** ERROR - IN SLPE - THE CHARACTERS ''',A,
     *   ' ''WERE EXPECTED IN FIELD ',I2,' BUT ''',A,''' WERE FOUND.')
230   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
240   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
260   FORMAT ('0*** WARNING - STAN PARAMETERS NOT FOUND ',
     *   'FOR STATION ',A,'.')
270   FORMAT ('0*** WARNING - ',A4,' PARAMETERS NOT DEFINED ',
     *   'FOR STATION ',A,'.')
280   FORMAT ('0*** ERROR - STATION GENERAL PARAMETERS ',
     *   'FOR STATION ',A,' NOT SUCCESSFULLY UPDATED.')
290   FORMAT ('0*** NOTE - STATION GENERAL PARAMETERS FOR ',
     *   'FOR STATION ',A,' SUCCESSFULLY UPDATED.')
300   FORMAT ('0*** WARNING - PE PARAMETERS NOT FOUND ',
     *   'FOR STATION ',A,'.')
310   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WPPDEL ',
     *   'ENCOUNTERED WHILE DELETING PE STATION ',A,'.')
320   FORMAT (' PARAMETERS DELETED FOR PE STATION ',A)
330   FORMAT ('0*** NOTE - PE PARAMETERS FOR STATION  ',
     *   'FOR STATION ',A,' SUCCESSFULLY DELETED.')
340   FORMAT ('0*** NOTE - NO STATIONS WITH PE PARAMETERS ',
     *   'SUCCESSFULLY PROCESSED.')
350   FORMAT ('0*** NOTE - ',I3,' STATION PE PARAMETERS ',
     *   'SUCCESSFULLY DELETED.')
360   FORMAT (' *** EXIT SLPE')
C
      END
