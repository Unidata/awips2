C MODULE SETUNT
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET OUTPUT UNIT NUMBERS.
C
      SUBROUTINE SETUNT (NFLD,ISTAT)
C
      CHARACTER*8 DDN/'FT??F001'/
      PARAMETER (MOPTN=8)
      CHARACTER*8 OPTN(MOPTN)/
     *   'PRINT   ','PUNCH   ','DEBUG   ','ERROR   ',
     *   'CPUTIME ','CMDLOG  ','OPTLOG  ','        '/
      CHARACTER*20 STRNG,STRNG2
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sutmrx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/setunt.f,v $
     . $',                                                             '
     .$Id: setunt.f,v 1.3 2000/12/18 22:53:32 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SETUNT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SOPT')
C
      ISTAT=0
      NUMERR=0
      NUMWRN=0
C
      STRNG=' '
      STRNG2=' '
      LSTRNG=-LEN(STRNG)
      LSTRNG2=-LEN(STRNG2)
      ISTRT=1
      ILPFND=0
      IRPFND=0
C
C  SET DEFAULT OPTIONS
      IUPRNT=6
      IUPNCH=7
      IUDBUG=6
      IUEROR=6
      IUTIMR=6
      IUCLOG=1
      IUOLOG=8
C
C  PRINT CARD
      CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DEFINE OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,210) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 190
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR OPTION
      DO 20 IOPTN=1,MOPTN
         IF (STRNG2.EQ.OPTN(IOPTN)) GO TO 30
20       CONTINUE
C
C  INVALID OPTION
      WRITE (LP,230) STRNG2
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
30    ILPFND=0
      IRPFND=0
      GO TO (50,70,90,110,130,150,170,40),IOPTN
40    WRITE (LP,240) OPTN(IOPTN)
      CALL SULINE (LP,2)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT UNIT OUTPUT OPTION
C
50    IF (LLPAR.EQ.0) THEN
         INTEGR=IUPRNT
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 60
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
60    CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      LP=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),LP
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCH UNIT OUTPUT OPTION
C
70    IF (LLPAR.EQ.0) THEN
         INTEGR=IUPNCH
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 80
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.7.OR.
     *    INTEGR.EQ.8) THEN
         ELSE
            WRITE (LP,290) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
80    CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      ICDPUN=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),ICDPUN
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEBUG UNIT OUTPUT OPTION
C
90    IF (LLPAR.EQ.0) THEN
         INTEGR=IUDBUG
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 100
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
100   CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      IOSDBG=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),IOSDBG
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ERROR UNIT OUTPUT OPTION
C
110   IF (LLPAR.EQ.0) THEN
         INTEGR=IUEROR
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 120
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
120   CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      LPE=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),LPE
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CPUTIME UNIT OUTPUT OPTION
C
130   IF (LLPAR.EQ.0) THEN
         INTEGR=IUTIMR
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 140
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
140   CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      ITMUNT=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),ITMUNT
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CMDLOG UNIT OUTPUT OPTION
C
150   IF (LLPAR.EQ.0) THEN
         INTEGR=IUCLOG
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 160
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
160   CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      IOPCLG(2)=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),IOPCLG(2)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  OPTLOG UNIT OUTPUT OPTION
C
170   IF (LLPAR.EQ.0) THEN
         INTEGR=IUOLOG
         WRITE (LP,260) OPTN(IOPTN),INTEGR
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 180
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,220) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFINFX (INTEGR,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (INTEGR.EQ.1.OR.
     *    INTEGR.EQ.6.OR.
     *    INTEGR.EQ.8.OR.
     *    INTEGR.EQ.9) THEN
         ELSE
            WRITE (LP,280) INTEGR,OPTN(IOPTN)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
C
C  CHECK IF UNIT IS ALLOCATED
180   CALL UFXDDN (DDN,INTEGR,IERR)
      IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,250) DDN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      IOPOLG(2)=INTEGR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) OPTN(IOPTN),IOPOLG(2)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT OPTIONS IN EFFECT
C
190   WRITE (LP,300)
      CALL SULINE (LP,2)
      WRITE (LP,310) IOPCLG(2),IOPOLG(2),ITMUNT,IOSDBG,LPE,LP,ICDPUN
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SETUNT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
220   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
230   FORMAT ('0*** ERROR - INVALID SETUNT OPTION : ',5A4)
240   FORMAT ('0*** ERROR - ERROR PROCESSING OPTION : ',5A4)
250   FORMAT ('0*** ERROR - DDNAME ',A,' IS NOT ALLOCATED.')
260   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. ',A,
     *   ' OPTION SET TO ',I2,'.')
270   FORMAT (1H ,A,' OUTPUT UNIT OPTION SET TO ',I2)
280   FORMAT ('0*** ERROR - ',I2,' IS AN INVALID ',A,' UNIT VALUE. ',
     *   'VALID VALUES ARE 1, 6, 8 OR 9.')
290   FORMAT ('0*** ERROR - ',I2,' IS AN INVALID ',A,' UNIT VALUE. ',
     *   'VALID VALUES ARE 1, 6, 7 OR 8.')
300   FORMAT ('0- OUTPUT UNIT OPTIONS IN EFFECT -')
310   FORMAT ('0  CMDLOG=',I2,5X,'OPTLOG=',I2,5X,
     *   'CPUTIME=',I2,5X,'DEBUG=',I2,5X,
     *   'ERROR=',I2,5X,'PRINT=',I2,5X,'PUNCH=',I2)
C
      END
