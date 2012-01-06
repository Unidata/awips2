C MODULE SMUSER
C-----------------------------------------------------------------------
C
C  ROUTINE TO OUTPUT USER PARAMETERS.
C
      SUBROUTINE SMUSER (LARRAY,ARRAY,OUTPUT,UNITS,NFLD,ISTAT)
C
      CHARACTER*(*) OUTPUT,UNITS
      CHARACTER*4 PARMTP
      CHARACTER*20 CHAR/' '/,CHK/' '/
      CHARACTER*8 OPTN(5)
     *      /'NEWPAGE','ALL','UGNL','URRS','STBN'/
C
      DIMENSION ARRAY(LARRAY)
C
C  ARRAYS FOR USER STBN PARAMETERS
      DIMENSION MDRBND(4),ISTBNP(89,42)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/surrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smuser.f,v $
     . $',                                                             '
     .$Id: smuser.f,v 1.3 1998/07/06 12:42:45 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,130)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
C
      MOPTN=5
C
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      IALL=0
      NUMERR=0
      NUMWRN=0
      NUMOPT=0
      LPUNCH=NPUCRD
C
      IENDIN=0
      ISTRT=1
C
C  PRINT CARD
      CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DUMP USER OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (NFLD.EQ.-1) THEN
         IENDIN=1
         GO TO 120
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,160) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) THEN
         IENDIN=1
         GO TO 120
         ENDIF
C
C  CHECK FOR PARENTHESES
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK FOR OPTION
      DO 20 IOPTN=1,MOPTN
         CALL SUCOMP (2,CHK,OPTN(IOPTN),IMATCH)
         IF (IMATCH.EQ.1) GO TO 30
20       CONTINUE
C
C  CHECK FOR GROUP
      CALL SUIDCK ('DUMP',CHK,NFLD,0,IKEYWD,IRETRN)
      IF (IRETRN.GT.0) THEN
         IENDIN=1
         GO TO 120
         ENDIF
C
C  INVALID OPTION
      WRITE (LP,140) CHK
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    GO TO (40,50,60,80,100),IOPTN
         WRITE (LP,150) IOPTN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
C
C  NEWPAGE OPTION
40    NEWPAG=0
      CALL SUNEWP (NFLD,ISTRT,CHK,LCHK,LLPAR,LRPAR,LENGTH,IRPFND,
     *   OPTN(IOPTN),NUMERR,NUMWRN,NEWPAG,IERR)
      GO TO 10
C
C  ALL OPTION
50    IALL=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS USER GENERAL PARAMETERS
C
60    PARMTP='UGNL'
C
C  READ PARAMETERS
      CALL SUGTUG (LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,180) PARMTP(1:LENSTR(PARMTP))
         CALL SULINE (LP,2)
         GO TO 70
         ENDIF
C
C  CHECK IF TO PRINT PARAMETERS
      IF (OUTPUT.EQ.'PRNT'.OR.OUTPUT.EQ.'BOTH') THEN
         INCLUDE 'scommon/callspugnl'
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PRINTING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
C  CHECK IF TO PUNCH PARAMETERS
      IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH') THEN
         CALL SCUGNL (UNITS,IVUGNL,UGNLID,ISEASN,MDRSUB,ULLMTS,
     *      ELLMTS,NBLEND,DPOWER,DPCNMN,STMNWT,SORTBY,NHPSUB,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PUNCHING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
70    IF (IALL.EQ.0) GO TO 110
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS USER RRS PARAMETERS
C
80    PARMTP='URRS'
C
C  READ PARAMETERS
      IPRERR=0
      CALL SUGTUR (LARRAY,ARRAY,IPRERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,180) PARMTP(1:LENSTR(PARMTP))
         CALL SULINE (LP,2)
         GO TO 90
         ENDIF
C
C  CHECK IF TO PRINT PARAMETERS
      IF (OUTPUT.EQ.'PRNT'.OR.OUTPUT.EQ.'BOTH') THEN
         INCLUDE 'scommon/callspurrs'
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PRINTING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
C  CHECK IF TO PUNCH PARAMETERS
      IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH') THEN
         CALL SCURRS (IVURRS,NTYPCD,TYPCD,MNDAY,NMOBS,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PUNCHING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
90    IF (IALL.EQ.0) GO TO 110
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATE BOUNDARY PARAMETERS
C
100   PARMTP='STBN'
C
C  READ PARAMETERS
      IPRERR=0
      CALL SRSTBN (IVSTBN,UNUSED,MDRBND,ISTBNP,LARRAY,ARRAY,IPRERR,
     *   IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,180) PARMTP(1:LENSTR(PARMTP))
         CALL SULINE (LP,2)
         GO TO 110
         ENDIF
C
C  CHECK IF TO PRINT PARAMETERS
      IF (OUTPUT.EQ.'PRNT'.OR.OUTPUT.EQ.'BOTH') THEN
         CALL SPSTBN (IVSTBN,UNUSED,MDRBND,ISTBNP,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PRINTING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
C  CHECK IF TO PUNCH PARAMETERS
      IF (OUTPUT.EQ.'PNCH'.OR.OUTPUT.EQ.'BOTH') THEN
         CALL SCSTBN (IVSTBN,MDRBND,ISTBNP,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,190) 'PUNCHING',PARMTP(1:LENSTR(PARMTP))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR END OF INPUT
110   IF (IENDIN.EQ.1) GO TO 120
C
      IALL=0
      NUMOPT=NUMOPT+1
      GO TO 10
C
C  CHECK NUMBER OF OPTIONS FOUND
120   IF (NUMOPT.EQ.0) THEN
         WRITE (LP,170) (OPTN(I)(1:LENSTR(OPTN(I))),I=3,5),
     *     OPTN(2)(1:LENSTR(OPTN(2)))
         CALL SULINE (LP,2)
         NUMOPT=NUMOPT+1
         GO TO 50
         ENDIF
C
C  PRINT NUMBER OF CARD IMAGES OUTPUT
      NPUNCH=NPUCRD-LPUNCH
      IF (NPUNCH.GT.0) THEN
         WRITE (LP,200) NPUNCH
         CALL SULINE (LP,2)
         ENDIF
C
C  CHECK NUMBER OF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) THEN
         WRITE (LP,210) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,220)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER SMUSER')
140   FORMAT ('0*** ERROR - INVALID DUMP USER OPTION : ',A)
150   FORMAT ('0*** ERROR - PROCESSING DUMP USER OPTION NUMBER ',I2,
     *   '.')
160   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
170   FORMAT ('0*** NOTE - NO PARAMETER TYPE KEYWORD (',A,', ',A,
     *   ' OR ',A,') WAS FOUND. ',A,' IS ASSUMED.')
180   FORMAT ('0*** NOTE - ',A,' PARAMETERS ARE NOT DEFINED.')
190   FORMAT ('0*** ERROR - IN SMUSER - ',A,1X,A,' PARAMETERS.')
200   FORMAT ('0*** NOTE - ',I4,' CARD IMAGES OUTPUT.')
210   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED IN DUMP USER ',
     *   'COMMAND.')
220   FORMAT (' *** EXIT SMUSER')
C
      END
