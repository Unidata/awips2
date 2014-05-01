C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE URMAIN
C-----------------------------------------------------------------------
C
      SUBROUTINE URMAIN_MAIN
C
C   MAIN ROUTINE FOR PROGRAM REORDER.
C
      CHARACTER*8 TYPERR
      CHARACTER*8 DCBDDN/' '/
      CHARACTER*8 DCBMBR/' '/
      CHARACTER*8 SEQNUM/' '/
      CHARACTER*9 XCMND
      CHARACTER*20 ENVVAR,USROLD,USRNEW
      CHARACTER*40 TXMLOG,TXSLOG
      CHARACTER*44 DSNAME
      CHARACTER*72 XOPTN,XOPTN2
      CHARACTER*128 FILNAM/' '/
C
cfan      PARAMETER (LWORK=125000)
      PARAMETER (LWORK=250000)            !cfan10052006 DR18073
cfan
      DIMENSION WORK(LWORK)
      PARAMETER (MFUNIT=10)
      DIMENSION KFUNIT(MFUNIT),LFUNIT(MFUNIT),KFUNITO(MFUNIT)
      DIMENSION KFUNITT(MFUNIT)
      EQUIVALENCE (KFUNIT(1),KFCGD),(LFUNIT(1),LFCGD)
      DIMENSION UNUSED(10)
C
      INCLUDE 'uiox'
      common /CMREORDER/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'upagex'
      INCLUDE 'udebug'
      INCLUDE 'uoptnx'
      INCLUDE 'udsatx'
      INCLUDE 'ufreei'
      INCLUDE 'ufstcd'
      INCLUDE 'udatas'
      INCLUDE 'ustopx'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/toterz'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/sntwfx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urmain.f,v $
     . $',                                                             '
     .$Id: urmain.f,v 1.16 2004/07/21 18:09:29 dsa Exp $
     . $' /
C    ===================================================================
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C

      CALL ARGVER()
C
      ISTAT=0
C
      LDEBUG=0
C
      LPE=LP
      LULOG=8
C
      IORPP=0
      IORFC=0
      IORESP=0
      IORFCC=0
      ICUPRM=0
      ICLHCL=0
      ICESP=0
C
      IORDER=1
C
      INDERR=0
      NOVPRT=2
      NOVPRTO=NOVPRT
      IINIT=0
      IFSLVL=0
      IPAGE=0
      IRVCMP=1
C
      INCLUDE 'cluprimo'
C
C  BE SURE UTIL_TO_REORDER ROUTINE LOADED BEFORE UTIL ROUTINES
      CALL UTIL_TO_REORDER
C
C  SET OPTIONS FOR UTILITY ROUTINES
      CALL USETO1 ('NOPAGHDR',IERR)
C
C  SET CPU TIMER
      ITMELA=0
      ITMTOT=0
      IUNIT=-1
      CALL SUTIMR (IUNIT,ITMELA,ITMTOT)
C
C  GET CURRENT DATE AND TIME IN INTEGER FORM AND STORE IN COMMON BLOCK
      CALL UDATEI (NMO,NDA,NYR,NHRMIN,NSEC,JULDAY,IERR)
      ISTDAY=JULDAY
      IELDAY=ISTDAY
      NHR=NHRMIN/100
      NMIN=NHRMIN-NHR*100
      ISTHMS=(NHR*10000+NMIN*100)+(NSEC/100)
      IELHMS=ISTHMS
C
C  READ USER PARAMETERS AND SET USER NAME
      CALL HGTUSR (PUSRID,IERR)
      IF (IERR.NE.0) THEN
         GO TO 330
         ENDIF
C
C  PRINT PROGRAM HEADER
      IOPTLE=2
      CALL SUPAGE
C
C  GET NAME OF FILE FROM WHICH PROGRAM IS BEING EXECUTED
      NUNIT=0
      IPRERR=-1
      CALL SULINE (LP,2)
      CALL UPRDSN ('STEPLIB ',NUNIT,DSNAME,IPRERR,LP,IERR)
C
C  PRINT FILE ATTRIBUTES
      NUNIT=ICD
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) INDERR=1
      NUNIT=LP
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (LPE.NE.LP) THEN
         NUNIT=LPE
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) LPE=LP
         ENDIF
      IF (LULOG.NE.LP) THEN
         NUNIT=LULOG
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) LULOG=0
         ENDIF
C
      TXMLOG='PROGRAM EXECUTION'
      CALL URWLOG (' ',TXMLOG,'STARTED  ',' ',LULOG)
C
      IF (INDERR.GT.0) GO TO 310
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET INDICATORS TO NOT CHECK IF SASM, GOES AND FMM DATA BASES
C  ALLOCATED
      IDBALC(8)=-1
      IDBALC(9)=-1
      IDBALC(10)=-1
      CALL UMEMOV (IDBALC,IDBSAV,10)
C
C  CHECK FOR SUFFICIENT REGION AND CPU TIME AVAILABLE
      ICKRGN=1
      MINRGN=MPGMRG
      ICKCPU=1
      MINCPU=30
      IPRERR=-1
      IPUNIT=LP
      TYPERR='WARNING'
      INCLUDE 'clugtres'
      IF (IERR.GT.0) THEN
         CALL SUFATL
         CALL SULINE (LP,2)
         WRITE (LP,360) '0'
         DO 20 I=1,NOVPRT
            CALL SULINE (LP,0)
            WRITE (LP,360) '+'
20          CONTINUE
         GO TO 310
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET OLD AND NEW USER NAME
      ENVVAR='ofs_level'
      USROLD=' '
      CALL GET_APPS_DEFAULTS(ENVVAR,LENSTR(ENVVAR),USROLD,LUSROLD)
      ENVVAR='ofs_reor_lvl'
      USRNEW=' '
      CALL GET_APPS_DEFAULTS(ENVVAR,LENSTR(ENVVAR),USRNEW,LUSRNEW)
      CALL SULINE (LP,2)
      WRITE (LP,410) USROLD(1:LUSROLD),USRNEW(1:LUSRNEW)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INITIALIZE ERROR FLAG
      IWURFL=0
C
C  SET OPTION TO NOT PRINT ALL WORK ARRAY USAGE INFORMATION
      ISWORK=0
C
C  SET INDICATOR TO CONTROL PRINTING DATA BASE STATUS
      ISTATS=3
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      INDERR=0
C
C  READ COMMAND
40    CALL SULINE (LP,1)
      WRITE (LP,*)
      IFSTCD=1
      CALL RWCARD (ISTAT)
      CALL SULINE (LP,1)
      IF (ISTAT.NE.0) GO TO 280
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 40
C
C  GET CARD SEQUENCE NUMBER
      NCHAR=LEN(SEQNUM)
      CALL UPACK1 (IBUF(73),SEQNUM,NCHAR)
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MXCMND=LEN(XCMND)
      IF (NCHAR.GT.MXCMND) NCHAR=MXCMND
      XCMND=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XCMND,NCHAR)
C
      XOPTN=' '
      MXOPTN=LEN(XOPTN)
      IF (NFIELD.GT.1) THEN
         NFLD=2
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
         XOPTN=' '
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' XCMND=',XCMND,
     *      ' XOPTN=',XOPTN,
     *      ' '
         ENDIF
C
C  CHECK FOR STOP COMMAND
      IF (XCMND.EQ.'@STOP') GO TO 130
C
C  CHECK FOR COMMENT
      IF (XCMND.EQ.'$') GO TO 40
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR OVERPRNT COMMAND
C
      IF (XCMND.EQ.'@OVERPRNT') THEN
         IF (XOPTN.NE.' ') THEN
            IF (XOPTN.EQ.'YES') THEN
               NOVPRT=NOVPRTO
               IOPOVP=1
               ENDIF
            IF (XOPTN.EQ.'NO') THEN
               NOVPRT=0
               IOPOVP=0
               ENDIF
            GO TO 40
            ENDIF
         NOVPRT=NOVPRTO
         IOPOVP=1
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR WORKUSAG COMMAND
C
      IF (XCMND.EQ.'@WORKUSAG') THEN
         IF (XOPTN.NE.' ') THEN
            IF (XOPTN.EQ.'YES') ISWORK=1
            IF (XOPTN.EQ.'NO') ISWORK=0
            GO TO 40
            ENDIF
         ISWORK=1
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR STATUS COMMAND
C
      IF (XCMND.EQ.'@STATUS') THEN
         NFLD=2
70       IF (NFLD.LE.NFIELD) THEN
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
            XOPTN=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
            NSCAN=1
            CALL USCAN2 (XOPTN,'=()',NSCAN,XOPTN2,LXOPTN2,IERR)
            IF (XOPTN2.EQ.'LEVEL') THEN
               NSCAN=NSCAN+1
               CALL USCAN2 (XOPTN,'=()',NSCAN,XOPTN2,LXOPTN2,IERR)
               IPRERR=1
               CALL UFA2I (XOPTN2,1,LXOPTN2,IFSLVL,IPRERR,LP,IERR)
               GO TO 80
               ENDIF
            ISTATS=-1
            IF (XOPTN2.EQ.'NONE') ISTATS=0
            IF (XOPTN2.EQ.'BEFORE') ISTATS=1
            IF (XOPTN2.EQ.'AFTER') ISTATS=2
            IF (XOPTN2.EQ.'BOTH') ISTATS=3
            IF (ISTATS.GE.0) GO TO 40
               WRITE (LP,460) XOPTN(1:LENSTR(XOPTN))
               CALL SUERRS (LP,2,-1)
               GO TO 40
80          NFLD=NFLD+1
            GO TO 70
            ENDIF
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR DEBUG COMMAND
C
      IF (XCMND.EQ.'@DEBUG') THEN
         CALL SULINE (LP,10)
         CALL USETDB
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR MAXERROR COMMAND
C
      IF (XCMND.EQ.'@MAXERROR') THEN
         NFLD=2
         IF (NFLD.LE.NFIELD) THEN
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
            XOPTN=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
            IPRERR=1
            CALL UFA2I (XOPTN,1,LENSTR(XOPTN),MAXERR,IPRERR,LP,IERR)
            ICKMIN=0
            ICKMAX=9999
            IF (MAXERR.GT.0.AND.MAXERR.LE.9999) THEN
               ELSE
                  WRITE (LP,450) XOPTN(1:LENSTR(MAXWRN)),
     *               MAXERR,ICKMIN,ICKMAX
                  CALL SUERRS (LP,2,-1)
                  GO TO 40
              ENDIF
            IOPMXE=MAXERR
            ENDIF
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR MAXWARN COMMAND
C
      IF (XCMND.EQ.'@MAXWARN') THEN
         NFLD=2
         IF (NFLD.LE.NFIELD) THEN
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
            XOPTN=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
            IPRERR=1
            CALL UFA2I (XOPTN,1,LENSTR(XOPTN),MAXWRN,IPRERR,LP,IERR)
            ICKMIN=0
            ICKMAX=9999
            IF (MAXWRN.GT.0.AND.MAXWRN.LE.9999) THEN
               ELSE
                  WRITE (LP,450) XOPTN(1:LENSTR(MAXWRN)),
     *               MAXWRN,ICKMIN,ICKMAX
                  CALL SUERRS (LP,2,-1)
                  GO TO 40
              ENDIF
            IOPMXW=MAXWRN
            ENDIF
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR INITFILE COMMAND
C
      IF (XCMND(1:5).EQ.'@INIT') THEN
C     CHECK OPTION SPECIFYING TYPE OF INITIALIZATION
         IF (XOPTN.NE.'COPY'.AND.XOPTN.NE.'ZERO') THEN
            WRITE (LP,460) XOPTN(1:LENSTR(XOPTN))
            CALL SUERRS (LP,2,-1)
            GO TO 40
            ENDIF
         IINIT=1
         IF (XOPTN.EQ.'COPY') IINIT=-1
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR ORDER COMMAND
C
      IF (XCMND.EQ.'@ORDER') THEN
90       IF (XOPTN.EQ.'FC') THEN
            IORFC=1
            GO TO 100
            ENDIF
         IF (XOPTN.EQ.'PP') THEN
            IORPP=1
C        CHECK FOR PPVRCMPR OPTION
            IF (NFLD.LT.NFIELD) THEN
               NFLD=NFLD+1
               NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
               IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
               XOPTN=' '
               CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
               NSCAN=1
               CALL USCAN2 (XOPTN,'=()',NSCAN,XOPTN2,LXOPTN2,IERR)
               IF (XOPTN2.EQ.'PPVRCMPR') THEN
                  NSCAN=NSCAN+1
                  CALL USCAN2 (XOPTN,'=()',NSCAN,XOPTN2,LXOPTN2,IERR)
                  IRVCMP=-1
                  IF (XOPTN2.EQ.'NO') IRVCMP=0
                  IF (XOPTN2.EQ.'YES') IRVCMP=1
                  IF (IRVCMP.EQ.-1) THEN
                     WRITE (LP,460) XOPTN2(1:LENSTR(XOPTN2))
                     CALL SUERRS (LP,2,-1)
                     IRVCMP=1
                     ENDIF
                  ELSE
                     NFLD=NFLD-1
                  ENDIF
               ENDIF
            GO TO 100
            ENDIF
         IF (XOPTN.EQ.'HCL') THEN
            IF (IPAGE.EQ.1) CALL SUPAGE
            IPAGE=1
            TXMLOG='COMMAND @ORDER HCL'
            CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
            CALL URHORD
            CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
            CALL UCLOSL
            GO TO 100
            ENDIF
         IF (XOPTN.EQ.'ESP') THEN
            IORESP=1
            GO TO 100
            ENDIF
         WRITE (LP,460) XOPTN(1:LENSTR(XOPTN))
         CALL SUERRS (LP,2,-1)
C     GET NEXT FIELD
100      IF (NFLD.LT.NFIELD) THEN
            NFLD=NFLD+1
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
            XOPTN=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
            GO TO 90
            ENDIF
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHECK FOR COPY COMMAND
C
      IF (XCMND.EQ.'@COPY') THEN
110      IF (XOPTN.EQ.'UPRM') THEN
            ICUPRM=1
            GO TO 120
            ENDIF
         IF (XOPTN.EQ.'LHCL') THEN
            ICLHCL=1
            GO TO 120
            ENDIF
         IF (XOPTN.EQ.'ESP') THEN
            ICESP=1
            GO TO 120
            ENDIF
         WRITE (LP,460) XOPTN(1:LENSTR(XOPTN))
         CALL SUERRS (LP,2,-1)
C     GET NEXT FIELD
120      IF (NFLD.LT.NFIELD) THEN
            NFLD=NFLD+1
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            IF (NCHAR.GT.MXOPTN) NCHAR=MXOPTN
            XOPTN=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XOPTN,NCHAR)
            GO TO 110
            ENDIF
         GO TO 40
         ENDIF
C
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  INVALID COMMAND
      WRITE (LP,480) XCMND(1:LENSTR(XCMND))
      CALL SUERRS (LP,2,-1)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF FC FILES AND ESP PARAMETER FILE TO BE REORDERED
130   IF (IORFC.EQ.0.AND.IORESP.EQ.1) THEN
         WRITE (LP,490)
         CALL SUWRNS (LP,2,-1)
         IORFC=1
         ENDIF
      IF (IORFC.EQ.1.AND.IORESP.EQ.0) THEN
C     GET ESP PARAMETER FILE NAME
         IUNIT=KUESPP
         CALL UPFNCU ('NORM',' ',IUNIT,FILNAM,IUNITO,LRECL,LBLOCK,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,500)
            CALL SUERRS (LP,2,-1)
            INDERR=1
            ELSE
C           CHECK IF ESP PARAMETER FILE EXISTS
               IUNIT=0
               CALL UPEXIS (IUNIT,FILNAM,IERR)
               IF (IERR.EQ.0) THEN
                  WRITE (LP,510)
                  CALL SUWRNS (LP,2,-1)
                  IORESP=1
                  ENDIF
            ENDIF
         ENDIF
C
C  CHECK IF ESP PARAMETER FILE TO BE REORDERED
      IF (IORESP.EQ.1) THEN
C     GET ESP PARAMETER FILE NAME
         IUNIT=KUESPP
         CALL UPFNCU ('NORM',' ',IUNIT,FILNAM,IUNITO,LRECL,LBLOCK,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,500)
            CALL SUERRS (LP,2,-1)
            INDERR=1
            ELSE
C           CHECK IF ESP PARAMETER FILE EXISTS
               IUNIT=0
               CALL UPEXIS (IUNIT,FILNAM,IERR)
               IF (IERR.NE.0) THEN
                  WRITE (LP,515)
                  CALL SUWRNS (LP,2,-1)
                  IORESP=0
                  ENDIF
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF PARAMETRIC DATA BASE ALLOCATED
      IDPPP=1
      CALL SUDALC (0,0,0,0,IDPPP,0,0,0,0,0,-1,IERR)
      IF (IERR.GT.0) GO TO 170
C
C  READ NETWORK PARAMETERS
      IPRERR=1
      CALL SRNTWK (LWORK,WORK,IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,
     *   IPRERR,IERR)
      IF (IERR.GT.0) GO TO 310
C
C  CHECK IF ANY NEWTWORK INDICATORS SET
      DO 140 I=1,NNWFLG
         IF (INWFLG(I).NE.0) GO TO 150
140      CONTINUE
      GO TO 170
150   WRITE (LP,430)
      CALL SUERRS (LP,2,-1)
C
C  PRINT NTWK PARAMETERS
      CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
      CALL SUFATL
      CALL SULINE (LP,2)
      WRITE (LP,360) '0'
      DO 160 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,360) '+'
160      CONTINUE
      GO TO 310
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ERRORS ENCOUNTERED
C
170   IF (INDERR.EQ.1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,520)
         GO TO 310
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY FILES THAT ARE NOT REORDERED
C
      IPAGE=1
C
C  CHECK IF UPRM COPY COMMAND SPECIFIED
      IF (ICUPRM.EQ.1) THEN
         IF (IPAGE.EQ.1) CALL SUPAGE
         IPAGE=0
         TXMLOG='USERPARM FILE COPY'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL URUCPY (LWORK,WORK,NOVPRT,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  REORDER FILES
C
      IPAGE=0
C
C  CHECK IF FC ORDER COMMAND SPECIFIED
      IF (IORFC.EQ.1) THEN
         IPAGE=1
         TXMLOG='COMMAND @ORDER FC'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL URFORD (IINIT,
     *      MC,C,MD,D,MP,P,MT,T,MTS,TS,
     *      DCBDDN,DCBMBR,
     *      ISTATS,NOVPRT,TXSLOG,LULOG,
     *      MFUNIT,KFUNIT,LFUNIT,KFUNITO,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         IDBFIL=0
         CALL UMEMOV (IDBSAV,IDBALC,10)
         IF (IERR.GT.0) GO TO 260
         IORFCC=1
         ENDIF
C
C  CHECK IF PP ORDER COMMAND SPECIFIED
      IF (IORPP.EQ.1) THEN
         IF (IPAGE.EQ.1) CALL SUPAGE
         IPAGE=1
         TXMLOG='COMMAND @ORDER PP'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL URPORD (IINIT,IRVCMP,IFSLVL,
     *      MP,P,MT,T,MTS,TS,
     *      LWORK,WORK,ISWORK,
     *      ISTATS,NOVPRT,TXSLOG,LULOG,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         ENDIF
C
C  CHECK IF ESP ORDER COMMAND SPECIFIED
      IOESPP=0
      IF (IORESP.EQ.1) THEN
         IF (IPAGE.EQ.1) CALL SUPAGE
         IPAGE=1
         TXMLOG='COMMAND @ORDER ESP'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL UREORD (IINIT,IORFCC,
     *      MD,D,MP,P,MT,T,MTS,TS,
     *      IOESPP,NOVPRT,
     *      MFUNIT,KFUNIT,LFUNIT,KFUNITO,KFUNITT,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY FILES THAT CAN BE REORDERD
C
C  CHECK IF LHCL COPY COMMAND SPECIFIED
      IF (ICLHCL.EQ.1) THEN
         IF (IPAGE.EQ.1) CALL SUPAGE
         IPAGE=0
         TXMLOG='COMMAND @COPY HCL'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL URHCPY (LWORK,WORK,NOVPRT,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         ENDIF
C
C  CHECK IF ESP COPY COMMAND SPECIFIED
      IF (ICESP.EQ.1) THEN
         IF (IPAGE.EQ.1) CALL SUPAGE
         IPAGE=0
         TXMLOG='COMMAND @COPY ESP'
         CALL URWLOG ('0',TXMLOG,'STARTED  ',' ',LULOG)
         CALL URECPY (IOESPP,LWORK,WORK,NOVPRT,IERR)
         CALL URWLOG (' ',TXMLOG,'COMPLETED',' ',LULOG)
         CALL UCLOSL
         ENDIF
C
      GO TO 310
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESSING ERROR - FILES WILL NOT BE CLOSED
260   WRITE (LP,270)
      CALL SULINE (LP,2)
270   FORMAT ('0*** NOTE - FILES WILL NOT BE CLOSED BECAUSE ERRORS ',
     *   'ENCOUNTERED.')
      GO TO 310
C
C  END OF FILE
280   WRITE (LP,290)
      CALL SULINE (LP,2)
290   FORMAT ('0*** NOTE - END OF COMMAND INPUT ENCOUNTERED. ',
     *   'STOP ASSUMED.')
      GO TO 310
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CLOSE ALL FILES
310   CALL UCLOSL
C
C  ADD NUMBER OF FC ERRORS AND WARNINGS TO SUERRX COMMON BLOCK
      IF (LDEBUG.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*)
     *      ' NWARNT=',NWARNT,' NERRST=',NERRST,
     *      ' NTWARN=',NTWARN,' NTERR=',NTERR,
     *      ' NRWARN=',NRWARN,' NRERR=',NRERR,
     *      ' '
         ENDIF
      NTWARN=NTWARN+NWARNT
      NTERR=NTERR+NERRST
      NRWARN=NRWARN+NWARNT
      NRERR=NRERR+NERRST
C
      CALL SULINE (LP,2)
      WRITE (LP,560) '0',PGMNAM,PGMNAM,PGMNAM,PGMNAM
      DO 320 I=1,NOVPRT
         CALL SULINE (LP,0)
         WRITE (LP,560) '+',PGMNAM,PGMNAM,PGMNAM,PGMNAM
320      CONTINUE
C
      TXMLOG='PROGRAM EXECUTION'
      CALL URWLOG ('0',TXMLOG,'COMPLETED',' ',LULOG)
C
C  STOP PROGRAM EXECUTION
330   ISTOP=-2
      CALL SUSTOP (ISTOP)
      INCLUDE 'cluprimc'
      ISTOPX=ISTOP
      CALL USTOP2
C
      STOP
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
360   FORMAT (A,7('***  FATAL ERROR  '),'***')
410   FORMAT ('0*** NOTE - USER NAME FOR OLD FILES IS ',A,'. ',
     *   'USER NAME FOR NEW FILES IS ',A,'.')
430   FORMAT ('0*** ERROR - ONE OR MORE NETWORK FLAGS ARE SET.')
450   FORMAT ('0*** ERROR - ',A,' VALUE (',I5,') MUST BE GREATER ',
     *   'THAN ',I2,' OR LESS THAN OR EQUAL TO ',I4,'.')
460   FORMAT ('0*** ERROR - OPTION ',A,' IS INVALID.')
480   FORMAT ('0*** ERROR - COMMAND ',A,' IS INVALID.')
490   FORMAT ('0*** WARNING - ESP PARAMETER FILE WAS SPECIFIED TO BE ',
     *   'REORDERED BUT FC FILES WERE NOT. ',
     *   'FC FILES WILL BE REORDERED.')
500   FORMAT ('0*** ERROR - CANNOT OBTAIN FILE NAME FOR ',
     *   'ESP PARAMETER FILE.')
510   FORMAT ('0*** WARNING - ESP PARAMETER FILE EXISTS BUT WAS NOT ',
     *   'SPECIFIED TO BE REORDERED. ',
     *   'ESP PARAMETER FILE WILL BE REORDERED.')
515   FORMAT ('0*** WARNING - ESP PARAMETER FILE DOES EXIST BUT WAS ',
     *   'SPECIFIED TO BE REORDERED. ',
     *   'ESP PARAMETER FILE WILL NOT BE REORDERED.')
520   FORMAT ('0*** NOTE - PROGRAM EXECUTION TERMINATED BECAUSE ',
     *   'ERRORS WERE ENCOUNTERED.')
560   FORMAT (A,4('***  ',A8,' RUN COMPLETED  '),'***')
C
      END
