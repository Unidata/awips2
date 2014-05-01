C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (start_jvm,close_jvm)
C MODULE FCSTMAIN
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR PROGRAM FCST.
C
CCC     PROGRAM MAIN
C
C		Added call START_JVM and CLOSE_JVM - Open/Close Java Virtual
C                                          Machine
C  CHANGE NAME IN ORDER TO LINK WITH C++ COMPILER (NEEDED WHEN ADDED
C  OPERATION RES-J )
      SUBROUTINE FCSTMAIN_MAIN
C      EXTERNAL MAIN
C
      CHARACTER*8 USERN
      CHARACTER*8 DDNAME
      CHARACTER*10 XDLIM,XCHR1,XCHR2
      CHARACTER*44 DSNAME
      CHARACTER*100 VAR_NAME,VAR_VALUE,PMFLD/' '/
      CHARACTER UNIXCMD*300,PATHO*150,PATHN*150

      INTEGER JVMTOKEN
C
      INCLUDE 'updaio'
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'upagex'
      INCLUDE 'uoptnx'
      INCLUDE 'ufiles'
      INCLUDE 'ufreei'
      INCLUDE 'ufstcd'
      INCLUDE 'common/toterz'
      INCLUDE 'common/where'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/killcd'
      INCLUDE 'clbcommon/crwctl'
      INCLUDE 'hclcommon/hcomnd'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hsvcrd'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hprflg'
      INCLUDE 'hclcommon/hprocc'
      INCLUDE 'prdcommon/punits'
      COMMON /HCPUCK/ ICPTOT,IBGSEC,ILASEC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_main/RCS/fcstmain.f,v $
     . $',                                                             '
     .$Id: fcstmain.f,v 1.11 2004/07/21 13:52:14 hank Exp $
     . $' /
C    ===================================================================
C
C
C      CALL MAIN()
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
CHDH  This line calls uprimo.  It has been here for a while, but I
CHDH  decided to add this comment on 6/23/2004 so that I could remember
CHDH  what it did!  (Hank Herr)
CHDH
CHDH  This call creates the initial HCLUSER read lock (calls hlockfiles
CHDH  with a program name of FCST).
      INCLUDE 'cluprimo'
C
C  BE SURE UTIL_TO_FCST ROUTINE LOADED BEFORE UTIL ROUTINES
      CALL UTIL_TO_FCST
C
      CALL FCSTM2
cc Added option to not enable JVM
cc token "turn_off_jvm"=1 to disable JVM
      CALL GET_APPS_DEFAULTS ('turn_off_jvm',12,JVMTOKEN,LJVMTOKEN)
      IF(JVMTOKEN .EQ. 0)THEN
C  Start J Virtual Memory for DHM OPERATION - start this JVM once
         CALL START_JVM
      END IF
C
      ICPUTM=0
      ICLKTM=0
C
C  GET USER PARAMETERS AND SET USER NAME
      CALL HGTUSR (PUSRID,IERR)
      IF (IERR.NE.0) GO TO 470
C
C  SET OVERPRINT OPTION
      NOVPRT=-1
C
C  SET OPTIONS FOR UTILITY ROUTINES
      IPAGE=1
      IERPRT=0
      ICDPRT=0
      ITMPRT=1
      CALL USETOP (IPAGE,IERPRT,ICDPRT,ITMPRT)
      CALL USETO1 ('LINCNT',IERR)
      CALL USETO1 ('NOPAGNUM',IERR)
C
C  PRINT PAGE HEADER
      CALL UPAGE (LP)
C
C  GET DATASET FROM WHICH PROGRAM IS BEING EXECUTED
      NUNIT=0
      DDNAME='STEPLIB'
      IPRERR=-1
      CALL UPRDSN (DDNAME,NUNIT,DSNAME,IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 470
C
C  PRINT DATASET ATTRIBUTES
      DDNAME=' '
      NUNIT=ICD
      IPRERR=1
      CALL UPRDSA (DDNAME,NUNIT,DSNAME,IPRERR,LP,IERR)
      NUNIT=LP
      CALL UPRDSA (DDNAME,NUNIT,DSNAME,IPRERR,LP,IERR)
C
      IGTPRM=1
      IF (IGTPRM.EQ.1) THEN
C  GET PARM FIELD
      CALL UGTPRM (LPMFLD,PMFLD)
      IF (LPMFLD.GT.0) THEN
C     PROCESS PARM FIELD
         CALL ULINE (LP,2)
         WRITE (LP,480) PMFLD(1:LPMFLD)
         IDBSET=0
         XDLIM=',=()/'
         NSCAN=0
C     GET NEXT FIELD
10       NSCAN=NSCAN+1
         CALL USCAN2 (PMFLD,XDLIM,-NSCAN,XCHR1,LXCHR1,IERR)
         IF (NOBUG.GT.0) THEN
            WRITE (LPD,*)
     *         ' NSCAN=',NSCAN,
     *         ' XCHR1=',XCHR1,
     *         ' IERR=',IERR,
     *         ' '
         CALL ULEFTC (XCHR1,LEN(XCHR1),LXCHR1)
            ENDIF
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.4) GO TO 10
            ELSE
               IF (NSCAN.LT.2) GO TO 10
               IF (XCHR1.EQ.' ') GO TO 10
               NSCAN=NSCAN+1
               CALL USCAN2 (PMFLD,XDLIM,-NSCAN,XCHR2,LXCHR2,IERR)
               IF (NOBUG.GT.0) THEN
                  WRITE (LPD,*)
     *               ' NSCAN=',NSCAN,
     *               ' XCHR2=',XCHR2,
     *               ' IERR=',IERR,
     *               ' '
                  ENDIF
               CALL ULEFTC (XCHR2,LEN(XCHR2),LXCHR2)
C           CHECK FOR DBUG OPTION
               IF (XCHR1.EQ.'DBUG') THEN
                  IF (XCHR2.EQ.' ') THEN
                     NOBUG=1
                     IHCLDB=1
                     ELSE
                        IPRERR=1
                        CALL UFA2I (XCHR2,1,LXCHR2,INTEGR,IPRERR,LP,
     *                     IERR)
                        NOBUG=INTEGR
                        IHCLDB=INTEGR
                     ENDIF
                  IDBSET=1
                  CALL ULINE (LP,2)
                  WRITE (LP,490) XCHR1(1:LENSTR(XCHR1))
                  GO TO 10
                  ENDIF
C           CHECK FOR TRCE OPTION
               IF (XCHR1.EQ.'TRCE') THEN
                  IF (XCHR2.EQ.' ') THEN
                     NOBUG=1
                     IHCLTR=1
                     ELSE
                        IPRERR=1
                        CALL UFA2I (XCHR2,1,LXCHR2,INTEGR,IPRERR,LP,
     *                     IERR)
                        NOBUG=INTEGR
                        IHCLTR=INTEGR
                     ENDIF
                  IDBSET=1
                  CALL ULINE (LP,2)
                  WRITE (LP,490) XCHR1(1:LENSTR(XCHR1))
                  GO TO 10
                  ENDIF
C           OPTION NOT RECOGNIZED
               IPRWRN=0
               IF (IPRWRN.EQ.1) THEN
                  CALL ULINE (LP,2)
                  WRITE (LP,500) XCHR1(1:LENSTR(XCHR1))
                  ENDIF
               GO TO 10
            ENDIF
         IF (IDBSET.EQ.1) THEN
            CALL ULINE (LP,2)
            WRITE (LP,540) NOBUG,IHCLTR,IHCLDB
            ENDIF
         ENDIF
      ENDIF
C
      IOPNUM=0
      CALL UREPET (' ',ISEG,8)
      CALL UREPET (' ',OPNAME,8)
C
      IPROCF=0
      IFCFLG=0
      INONFC=0
      ICDSAV=ICD
CCC      IOERR=LP
CCC      IODBUG=LPD
CCC      IPR=LP
      NONFCT=0
      IWTCTL=0
      ISTOP=0
      IERROR=0
      ICSAV=0
      MAXOPT=0
      LRECOP=16
C
CCC      WRITE (UE,*) 'IN FCSTMAIN - IPR=',IPR,' IOERR=',IOERR
C
C  INITIALIZE INCREMENTAL AND TOTAL CPU TIME
      NUNIT=-1
      ICPTOT=0
      CALL UCPUTM (NUNIT,ICPELA,ICPTOT)
C
C  SET BEGINNING WALL CLOCK TIME
      CALL UDATEI (NMON,NDAY,NYEAR,NHRMIN,NSEC,JULDAT,IERR)
      IBGSEC=(NHRMIN/100)*3600+MOD(NHRMIN,100)*60+(NSEC+50)/100
      ILASEC=IBGSEC
C
C  STORE CURRENT HYDROLOGIC DATES IN COMMON
      CALL HSYSDA (JULDAT)
C
C  READ INPUT CARDS, CHECK FOR INCLUDES AND WRITE TO TEMPORARY DATASET
      IUNIT=ICD
      CALL FCRCDS (IUNIT,IERR)
C
C  READ FIRST CARD
      CALL RPCARD (ICBUF,IERR)
      IF (IERR.NE.0) GO TO 470
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ HCL CONTROL INFORMATION
20    CALL HRDCTL (IERR)
      IF (IERR.NE.0) GO TO 470
      CALL HRIDXC (IERR)
      IF (IERR.NE.0) GO TO 470
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    IOPNUM=0
      MAINUM=1
      CALL UREPET (' ',ISEG,8)
      CALL UREPET (' ',OPNAME,8)
C
C  READ COMMAND CARD
      CALL HCARDI (IERR)
      IF (IERR.EQ.-1) GO TO 270
      IF (IERR.NE.0) GO TO 470
C
C  SET FIRST COMMAND INDICATOR
      IF (INONFC.EQ.1.AND.IFCFLG.EQ.0) THEN
         IF (IHCMND.EQ.20.OR.IHCMND.EQ.30.OR.IHCMND.EQ.38) THEN
            ELSE
               IFCFLG=1
            ENDIF
         ENDIF
C
C  CHECK IF IMPLIED STOP
      IF (IHCMND.EQ.-28) GO TO 290
C
C  SET INCREMENTAL AND TOTAL CPU TIME
      NUNIT=-1
      CALL UCPUTM (NUNIT,ICPELA,ICPTOT)
C
C  SET WALL CLOCK TIME
      CALL UDATEI (NMON,NDAY,NYEAR,NHRMIN,NSEC,JULDAT,IERR)
      ITEMP=(NHRMIN/100)*3600+MOD(NHRMIN,100)*60+(NSEC+50)/100
C
      IF (ICPUTM.GT.0) THEN
         ELAPSE=ICPELA/100.
         WRITE (LPD,510) ELAPSE
         ENDIF
C
      IF (ICLKTM.GT.0) THEN
         INCCLK=ITEMP-ILASEC
         IF (INCCLK.LT.0) INCCLK=INCCLK+86400
         INCMIN=INCCLK/60
         INCSEC=MOD(INCCLK,60)
         WRITE (LPD,520) INCMIN,INCSEC
         ENDIF
C
      ILASEC=ITEMP
C
C  CHECK NONFCST FLAG
      IF (NONFCT.EQ.1.AND.IHCMND.EQ.4) THEN
C     TRYING TO DO COMPUTE ON NONFCST RUN
         CALL ULINE (LP,2)
         WRITE (LP,600)
         CALL ERROR
         GO TO 450
         ENDIF

      IF (NONFCT.EQ.0) THEN
         IF (IHCMND.EQ.01) GO TO 440
         IF (IHCMND.EQ.06) GO TO 440
         IF (IHCMND.EQ.09) GO TO 440
         IF (IHCMND.EQ.10) GO TO 440
         IF (IHCMND.EQ.11) GO TO 440
         IF (IHCMND.EQ.25) GO TO 440
         IF (IHCMND.EQ.26) GO TO 440
         IF (IHCMND.EQ.32) GO TO 440
         IF (IHCMND.EQ.33) GO TO 440
         IF (IHCMND.EQ.34) GO TO 440
         IF (IHCMND.EQ.35) GO TO 440
         IF (IHCMND.EQ.36) GO TO 440
         IF (IHCMND.EQ.37) GO TO 440
         ENDIF
      IF (IHCMND.LT.12.OR.IHCMND.GT.18) IPRFLG=1
C
      IF (NOBUG.GT.0) WRITE (LPD,*) 'IHCMND=',IHCMND
C
      GO TO (50,40,60,70,40,
     *       80,40,40,90,140,
     *       40,40,40,190,40,
     *       40,200,40,40,210,
     *       40,40,230,40,240,
     *       250,260,270,310,320,
     *       330,340,350,360,40,
     *       370,380,40,400,410,
     *       420,430,435),IHCMND
C
      NC=1
      CALL HCARDR (NC,IERR)
      CALL ULINE (LP,2)
      WRITE (LP,550) IBUF
      CALL ERROR
      GO TO 30
C
40    CALL ULINE (LP,2)
      WRITE (LP,560)
      CALL WARN
      NC=0
      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  ADD TECHNIQUE COMMAND
C
50    CALL HADTEC
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CLEAR COMMAND
C
60    CALL HCLEAR
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  COMPUTE COMMAND
C
cav move this call to hcompt.f
cav 70      CALL RPDBCI (IERR)
CHDH  Change made by Hank Herr (2004-06-15).
CHDH  I have added pieces of code that will unlock the files currently
CHDH  locked before calling hcompt, and then lock those files again
CHDH  after.
70    IF (NONFCT.EQ.0) THEN
          CALL HUNLOCKFILES('FCST',KOND)
          IF (KOND.GT.0) THEN
              STOP 16
          ENDIF
      ELSE
          CALL HUNLOCKFILES('NONFCST',KOND)
          IF (KOND.GT.0) THEN
              STOP 16
          ENDIF
      ENDIF

      CALL HCOMPT (IERR)
C
C  CLOSE ALL FILES
      CALL UCLOSL

      IF (NONFCT.EQ.0) THEN
          CALL HLOCKFILES('FCST',KOND)
          IF (KOND.GT.0) THEN
              STOP 16
          ENDIF
      ELSE
          CALL HLOCKFILES('NONFCST',KOND)
          IF (KOND.GT.0) THEN
              STOP 16
          ENDIF
      ENDIF
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  CHANGE PROC COMMAND
C
80    CALL HCNGPR (IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DEFINE LOCAL
C
90    GO TO (100,110,120),KEYWRD
      GO TO 130
C
C  LOCAL PROCEDURE
C
100   CALL HDPROC (1,IERR)
      GO TO 30
C
C  LOCAL FUNCTION
C
110   CALL HDFUNC (1,IERR)
      GO TO 30
C
C  LOCAL TECHNIQUE
C
120   CALL HDTECH (1,IERR)
      GO TO 30
C
C  LOCAL OPTION
C
130   CALL HDOPTN (IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DEFINE GLOBAL
C
140   GO TO (150,160,170),KEYWRD
      GO TO 180
C
C  GLOBAL PROCEDURE
C
150   CALL HDPROC (-1,IERR)
      GO TO 30
C
C  GLOBAL FUNCTION
C
160   CALL HDFUNC (-1,IERR)
      GO TO 30
C
C  GLOBAL TECHNIQUE
C
170   CALL HDTECH (-1,IERR)
      GO TO 30
C
C  NO GLOBAL OPTION
C
180   CALL ULINE (LP,2)
      WRITE (LP,530)
      NC=0
      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DUMPOPT COMMAND
C
190   CALL HPRTOS
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DUMPSYS COMMAND
C
200   CALL HPRNTS
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  EXECUTE COMMAND
C
210   CALL HEXECP (IERR)
      IF (IERR.EQ.0) GO TO 30
      GO TO 470
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  SETOPT COMMAND
C
230   CALL HROPTN (IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  REPLACE PROC COMMAND
C
240   CALL HREPPR (IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  SETGDFLT COMMAND
C
250   IGL=-1
      CALL HDDFLT (IGL)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  SETTODAY COMMAND
C
260   ICARDR=1
      CALL HTODAY (ICARDR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  STOP COMMAND
C
270   IFSTCD=1
      NC=1
      CALL HCARDR (NC,IERR)
C
280   NC=NC+1
      IFSTCD=1
      IF (NC.GT.NCARD) GO TO 290
      CALL HCARDR (NC,IERR)
      GO TO 280
C
290   IF (NONFCT.EQ.0.AND.IWTCTL.EQ.0) GO TO 300
C
C  WRITE CONTROL INFORMATION
      CALL HWIDXC (IERR)
      CALL HWDCTL (IERR)
C
C  CLOSE ALL FILES
300   CALL UCLOSL
      GO TO 470
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DEBUG COMMAND
C
310   NC=1
      CALL HCARDR (NC,IERR)
      NOBUG=0
      NFLD=2
      IF (NFIELD.GE.NFLD) THEN
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NOBUG)
         ENDIF
      IHCLTR=0
      NFLD=3
      IF (NFIELD.GE.NFLD) THEN
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IHCLTR)
         ENDIF
      IHCLDB=0
      NFLD=4
      IF (NFIELD.GE.NFLD) THEN
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IHCLDB)
         ENDIF
      CALL ULINE (LP,2)
      WRITE (LP,540) NOBUG,IHCLTR,IHCLDB
      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  NONFCST COMMAND
C
320   NC=1
      CALL HCARDR (NC,IERR)
      IF (NONFCT.EQ.0.AND.IFCFLG.EQ.1) THEN
         CALL ULINE (LP,2)
         WRITE (LP,610)
         CALL ERROR
         GO TO 290
         ENDIF
      NONFCT=1

CHDH  Changes made by Hank Herr (2004-06-14).
CHDH  Change the HCLUSER lock to a write lock.  To do so, free the old
CHDH  lock and create the new lock.
      CALL HUNLOCKFILES('FCST',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      CALL HLOCKFILES('NONFCST',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF

      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  FCST COMMAND
C
330   IF (NONFCT.EQ.1) IWTCTL=1
      NONFCT=0
      NC=0

CHDH  Changes made by Hank Herr (2004-06-15).
CHDH  Change the HCLUSER lock to a read lock.  To do so, free the old
CHDH  lock and create the new lock.
      CALL HUNLOCKFILES('NONFCST',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      CALL HLOCKFILES('FCST',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF

      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  SETLDFLT
C
340   IGL=1
      CALL HDDFLT (IGL)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DELETE LOCAL HCL DEFINITIONS
C
350   IGL=1
      CALL HDELDF (IGL)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DELETE GLOBAL HCL DEFINITIONS
C
360   IGL=-1
      CALL HDELDF (IGL)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  DELETE LOCAL DEFAULTS COMMAND
C
370   CALL HDELLD
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  SETUPARM COMMAND
C
380   CALL HSETPM
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  RUN COMMAND
C
400   NC=1
      CALL HCARDR (NC,IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  NEWUSER COMMAND
C
410   NC=1
      CALL HCARDR (NC,IERR)
C
C  SET NEW USER NAME
      NFLD=2
      IF (NOBUG.GT.0) WRITE (LPD,*) 'NFIELD=',NFIELD
      IF (NFIELD.LT.NFLD) GO TO 30
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(USERN)) NCHAR=LEN(USERN)
      USERN=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),USERN,NCHAR)
      IF (NOBUG.GT.0) WRITE (LPD,*) 'USERN=',USERN
C
C  CLOSE ALL FILES
      IFILES(KHCARD)=0
      CALL UCLOSL
C
C  GET OLD PATHNAME AND USERNAME
      VAR_NAME='ofs_fs5files'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *   VAR_VALUE,LENGTH)
      PATHO=VAR_VALUE(1:LENGTH)
C
C  CHANGE FILE SET TO BE PROCESSED
      CALL USRNEW (USERN,PATHN,IERR)
C
C  GET USER PARAMETERS
      CALL HGTUSR (PUSRID,IERR)
C
      CALL UPAGE (LP)
C
C  RESET NUMBER OF OPTIONS
      MAXOPT=0
C
C  COPY HCL FILES
      UNIXCMD='cp -p '
     *        //PATHO(1:LENSTR(PATHO))
     *        //'/TEMP.HCL* '
     *        //PATHN(1:LENSTR(PATHN))
      CALL ULINE (LP,1)
      WRITE (LP,'(1H0,A,A)') '**NOTE** RUNNING UNIX COMMAND: ',
     *   UNIXCMD(1:LENSTR(UNIXCMD))
      CALL SYSTEM (UNIXCMD)
C
      GO TO 20
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  PAGESIZE COMMAND
C
420   NC=1
      CALL HCARDR (NC,IERR)
C
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL ULINE (LP,1)
         WRITE (LP,570) NFLD
         GO TO 30
         ENDIF
      IF (NFIELD.GE.NFLD) THEN
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
         MINVALUE=50
         MAXVALUE=80
         IF (INTEGR.GE.MINVALUE.AND.INTEGR.LE.MAXVALUE) THEN
            NPAGE=INTEGR
            ELSE
               NPAGE=80
               CALL ULINE (LP,1)
               WRITE (LP,580) INTEGR,MINVALUE,MAXVALUE,NPAGE
            ENDIF
         CALL USETPS (NPAGE,MINVALUE,MAXVALUE,IERR)
         ENDIF
C
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  STATUS COMMAND
C
430   NC=1
      CALL HCARDR (NC,IERR)
C
      CALL HSTATS (IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  INCLUDE COMMAND
C
435   GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  NONFCST NOT SPECIFIED
440   CALL WPCARD (IBUF)
      CALL ULINE (LP,1)
      WRITE (LP,590)
C
450   NC=1
      CALL HCARDR (NC,IERR)
      GO TO 30
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
460   IF (NC.GE.NCARD) GO TO 30
      NC=NC+1
      CALL HCARDR (NC,IERR)
      GO TO 460
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
470   IPR=LP
C
      KLSTOP=1

CHDH  NOTE: By calling STOP, all locks will be freed.
      CALL STOP
C
      INCLUDE 'cluprimc'
      IF(JVMTOKEN .EQ. 0)THEN
cc Destroy JVM When program finished
          CALL CLOSE_JVM
      END IF
C
      STOP
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
480   FORMAT ('0PARM FIELD = ',A)
490   FORMAT ('0**NOTE** ',A,' OPTION',
     *   ' FOUND IN PARM FIELD',
     *   '.')
500   FORMAT ('0**WARNING** OPTION ',A,
     *   ' FOUND IN PARM FIELD',
     *   ' NOT RECOGNIZED.')
510   FORMAT ('0**NOTE** INCREMENTAL ',
     *   'CPU TIME = ',F6.2,' SECONDS.')
520   FORMAT ('0**NOTE** INCREMENTAL ',
     *  'CLOCK TIME = ',I4,' MINUTES',I3,' SECONDS.')
530   FORMAT ('0**ERROR** NO DEFINE GLOBAL OPTION COMMAND AVAILABLE.')
540   FORMAT ('0DEBUG VARIABLES SET : ',
     *  'NOBUG=',I2,3X,
     *  'IHCLTR=',I2,3X,
     *  'IHCLDB=',I2)
550   FORMAT ('0**ERROR** INVALID COMMAND : ',80A1)
560   FORMAT ('0**WARNING** COMMAND NOT IMPLEMENTED. CARD IGNORED.')
570   FORMAT ('0**ERROR** FIELD ',I2,' ON PAGESIZE COMMAND IS NOT AN ',
     *   'INTEGER. CARD IGNORED.')
580   FORMAT ('0**WARNING** PAGESIZE VALUE ',I3,' IS NOT WITHIN ',
     *    'THE RANGE ',I2,' THRU ',I2,' AND WILL BE SET TO ',I2,'.')
590   FORMAT ('0**ERROR** THIS COMMAND CANNOT BE EXECUTED DURING A ',
     *  'FORECAST RUN. USE @NONFCST FIRST.')
600   FORMAT ('0**ERROR** COMPUTE COMMAND NOT VALID DURING A NONFCST ',
     *   'RUN.')
610   FORMAT ('0**ERROR** NONFCST MUST BE FIRST COMMAND. ',
     *   'RUN TERMINATED.')
C
      END
