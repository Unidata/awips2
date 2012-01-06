C MODULE PDUTMAIN
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR PROGRAM PPDUTIL.
C
      SUBROUTINE PDUTMAIN_MAIN
C
      CHARACTER*4 PDTYPE
      CHARACTER*8 DDNAME,USERN
      CHARACTER*8 RUNTYP,RDTYPE
      CHARACTER*8 OLDOPN,TYPERR
      CHARACTER*8 STAID
      PARAMETER (MCMNDS=33)
      CHARACTER*8 CMNDS(MCMNDS)
     *   /'DEBUG   ','STATION ','UNITS   ',
     *    'DUMPSTAT','DUMPOBS ','RSETSTAT',
     *    'CHNGSPEC','DEFTYPE ','DUMPUTIL',
     *    'DELTYPE ','DELRRS  ','EDITDLY ',
     *    'EDITRRS ','PAGESIZE','STOP    ',
     *    'RRSFREE ','RRSDUMP ','ZERODATE',
     *    'RLSEFREE','UNSDFREE','DUMPSHEF',
     *    'DUMPPP24','RESET   ','NEWUSER ',
     *    'ZERODLY ','$       ','DSATTR  ',
     *    'STATUS  ','DUMPSIF ','DELSTA  ',
     *    'CHECKRRS','PDFNDR  ','SETTODAY'/
      CHARACTER*12 CHAR12
      CHARACTER*150 PATHN
C
      DIMENSION IUSRID(2)
      DIMENSION IUNTS(2),ITYP(2),NUMSTA(2)
      DIMENSION LENG(2),METC(2),ISTYP(3)
      PARAMETER (LARRAY=25000)
      DIMENSION ARRAY(LARRAY)
      PARAMETER (LISIBUF=128)
      INTEGER*2 ISIBUF(LISIBUF)
      PARAMETER (LMNRRS=750)
      PARAMETER (LOBRRS=LMNRRS*2)
      DIMENSION MNRRS(LMNRRS)
      DIMENSION OBRRS(LOBRRS)
      DIMENSION IOBRRS(1)
      EQUIVALENCE (IOBRRS,OBRRS)
C
      INCLUDE 'uiox'
      common /CMPPDUTIL/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'uerorx'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'upagex'
      INCLUDE 'ustopx'
      INCLUDE 'ufreei'
      INCLUDE 'uunits'
      INCLUDE 'ufstcd'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdnxrd'
      INCLUDE 'pdbcommon/pdrrsc'
      COMMON /FCTIME/ IFCD1(11),LOCALF,NOUTZ,NOUTDS,NLSTZF,IFCD2(5)
C
      DATA ID/4hID  /
      DATA NUMSTA/4hNUMB,4hER  /
      DATA LENG/4hENGL,4hISH /
      DATA METC/4hMETR,4hIC  /
      DATA LALL/4hALL /
      DATA ISTYP/4hPP24,4hRRS ,4hPPSR/
      DATA IBLNK/4hBOTH/
C
      EQUIVALENCE (IUSRID,PUSRID)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdutmain.f,v $
     . $',                                                             '
     .$Id: pdutmain.f,v 1.14 2004/07/21 18:28:10 dsa Exp $
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
      LDEBUG=0
C
      INCLUDE 'cluprimo'
C
      LPE=LP
C
      IPDENQ=0
      IPRMPT=0
      IUSTOP=0
      INDERR=0
      INWUSR=0
      IWPPDC=0
C
C  SET INFORMATION IN WHERE COMMON BLOCK
      IOPNUM=-3
      CALL FSTWHR ('**NONE**',IOPNUM,OLDOPN,IOLDOP)
C
C  GET USER PARAMETERS
      CALL HGTUSR (PUSRID,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,1020) 'HGTUSR',IERR
         GO TO 800
         ENDIF
C
C  SET OPTIONS FOR UTILITY ROUTINES
      IPAGE=0
      IERPRT=0
      ICDPRT=0
      ITMPRT=1
      CALL USETOP (IPAGE,IERPRT,ICDPRT,ITMPRT)
      CALL USETO1 ('NOOVERPRINT',IERR)
      MAXERR=0
C
C  PRINT HEADER
      CALL UPAGE (LP)
C
C  GET DATASET FROM WHICH PROGRAM IS BEING EXECUTED
      NUNIT=0
      IPRERR=-1
      CALL UPRDSN ('STEPLIB ',NUNIT,'NONE',IPRERR,LP,IERR)
C
C  CHECK REGION AND CPU TIME
      ICKRGN=1
      MINRGN=MPGMRG
      ICKCPU=0
      MINCPU=0
      IPRERR=-1
      IPUNIT=LP
      TYPERR='WARNING'
      LDEBUG=0
      INCLUDE 'clugtres'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT DATASET ATTRIBUTES
      NUNIT=ICD
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) INDERR=1
      CALL UPRMPT (IPRMPT,IERR)
      NUNIT=LP
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      NUNIT=ICDPUN
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) ICDPUN=0
      IF (LPE.NE.LP) THEN
         NUNIT=LPE
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) LPE=LP
         ENDIF
C
      IF (INDERR.GT.0) GO TO 780
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ FILE CONTROL INFORMATION
10    CALL RPPDCO (IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1020) 'RPPDCO',IERR
         GO TO 800
         ENDIF
      IKEY=0
      CALL RPDHSH (IKEY,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1020) 'RPDHSH',IERR
         GO TO 800
         ENDIF
C
C  CHECK USER NAME FROM USERPARM FILE AND PDB FILE
      IF (IUSRID(1).NE.IPUSER(1).AND.IUSRID(2).NE.IPUSER(2)) THEN
         CALL ULINE (LP,2)
         WRITE (LP,810) IUSRID,IPUSER
         ENDIF
C
      IF (IPRMPT.EQ.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,820)
         ENDIF
C
C  READ HCL CONTROLS
      CALL HGETPM (IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1020) 'HGETPM',IERR
         GO TO 800
         ENDIF
C
C  GET TODAYS DATE
      CALL HSYSDA (JULDAT)
C
      LOCALF=LOCAL
      NLSTZF=NLSTZ
C
      ITRACE=0
      IODBUG=LP
      IOERR=LP
      IN=ICD
      IPR=LP
C
      ISTAFL=0
      IUNFLG=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    INWUSR=0
C
25    IF (IPRMPT.EQ.1) THEN
         CALL ULINE (LP,1)
         WRITE (LP,830)
         ENDIF
C
C  READ CARD
      CALL RPCARD (IBUF,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1030)
         GO TO 780
         ENDIF
C
C  PRINT CARD
      IFSTCD=1
      CALL WPCARD (IBUF)
C
C  FIND FIELDS ON CARD
      ICDBEG=1
      ICDEND=72
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK IF NO FIELDS ON CARD
      IF (NFIELD.EQ.0) GO TO 20
C
C  GET FIRST FIELD
27    CHAR12=' '
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(CHAR12)) NCHAR=LEN(CHAR12)
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NCHAR)
      IF (CHAR12(1:1).EQ.'@') CHAR12=CHAR12(2:LEN(CHAR12))
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LPD,1)
         WRITE (LPD,*) 'CHAR12=',CHAR12
         ENDIF
C
C  CHECK FOR VALID COMMAND
      DO 30 ICMNDS=1,MCMNDS
         IF (CHAR12.EQ.CMNDS(ICMNDS)) GO TO 40
30       CONTINUE
      GO TO 50
C
C  SET INDICATOR TO ENQ PREPROCESSOR DATA BASE
40    IF (IPDENQ.EQ.0) THEN
         IF (CHAR12.EQ.'RSETSTAT'.OR.
     *       CHAR12.EQ.'CHNGSPEC'.OR.
     *       CHAR12.EQ.'DEFTYPE'.OR.
     *       CHAR12.EQ.'DELTYPE'.OR.
     *       CHAR12.EQ.'DELRRS'.OR.
     *       CHAR12.EQ.'EDITDLY'.OR.
     *       CHAR12.EQ.'EDITRRS'.OR.
     *       CHAR12.EQ.'ZERODATE'.OR.
     *       CHAR12.EQ.'RESET'.OR.
     *       CHAR12.EQ.'ZERODLY')
     *      IPDENQ=1
         ENDIF
C
C  CHECK IF TO ENQ PPDB
      IF (IPDENQ.EQ.1) THEN
         CALL ULINE (LP,1)
         CALL PDENDQ ('ENQ ',IERR)
         IF (IERR.EQ.0) IPDENQ=-IPDENQ
         ENDIF
C
      GO TO (70,90,140,
     *       190,280,330,
     *       390,400,410,
     *       430,440,450,
     *       460,470,780,
     *       480,500,520,
     *       550,560,570,
     *       580,590,610,
     *       510,60,80,
     *       420,650,680,
     *       700,771,777,
     *       50),ICMNDS
C
C  INVALID COMMAND
50    WRITE (LP,840)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COMMENT
C
60    GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEBUG COMMAND
C
70    IF (NFIELD.GT.1)
     *   CALL UNUMIC (IBUF,IFSTRT(2),IFSTOP(2),IPDTR)
      IF (NFIELD.GT.2)
     *   CALL UNUMIC (IBUF,IFSTRT(3),IFSTOP(3),IPDDB)
      IF (NFIELD.GT.3)
     *   CALL UNUMIC (IBUF,IFSTRT(4),IFSTOP(4),NOBUG)
      IF (NFIELD.GT.4)
     *   CALL UNUMIC (IBUF,IFSTRT(5),IFSTOP(5),IHCLTR)
      IF (NFIELD.GT.5)
     *   CALL UNUMIC (IBUF,IFSTRT(6),IFSTOP(6),IHCLDB)
      WRITE (LP,850) IPDTR,IPDDB,NOBUG,IHCLTR,IHCLDB
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DSATTR COMMAND
C
80    NUNIT1=1
      NUNIT2=99
      DDNAME=' '
      IPRHDR=1
      IROUND=0
      CALL UDDTBL (NUNIT1,NUNIT2,DDNAME,IPRHDR,IROUND,LBUFRS,LP,IERR)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATION COMMAND
C
90    IF (NFIELD.EQ.2) GO TO 100
         WRITE (LP,860)
         GO TO 20
100   NUM=IFSTOP(2)-IFSTRT(2)+1
      IF (NUM.EQ.2.OR.NUM.EQ.6) GO TO 110
         WRITE (LP,860)
         GO TO 20
110   ITYP(2)=IBLNK
      CALL UPACK1 (IBUF(IFSTRT(2)),ITYP,NUM)
      IF (ITYP(1).NE.ID) GO TO 120
C
C  SET FLAG FOR STATION ID
      ISTAFL=0
      GO TO 20
C
C  CHECK IF STATION NUMBER
120   CALL UNAMCP (ITYP,NUMSTA,IERR)
      IF (IERR.EQ.0) GO TO 130
      WRITE (LP,860)
      GO TO 20
C
C  SET FLAG FOR STATION NUMBER
130   ISTAFL=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UNITS COMMAND
C
140   IF (NFIELD.EQ.2) GO TO 150
      WRITE (LP,870)
      GO TO 20
150   NUM=IFSTOP(2)-IFSTRT(2)+1
      IF (NUM.EQ.6.OR.NUM.EQ.7) GO TO 160
         WRITE (LP,870)
         GO TO 20
160   IUNTS(2)=IBLNK
      CALL UPACK1 (IBUF(IFSTRT(2)),IUNTS,NUM)
      CALL UNAMCP (LENG,IUNTS,IERR)
      IF (IERR.NE.0) GO TO 170
C
C  SET FLAG FOR ENGLISH UNITS
      IUNFLG=0
      GO TO 20
C
170   CALL UNAMCP (METC,IUNTS,IERR)
      IF (IERR.EQ.0) GO TO 180
         WRITE (LP,870)
         GO TO 20
C
C  SET FLAG FOR METRIC UNITS
180   IUNFLG=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPSTAT COMMAND
C
190   NFLD=2
      IF (NFIELD.LT.2) GO TO 210
         NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),LSTYP,NUM)
         DO 200 I=1,3
            IF (LSTYP.EQ.ISTYP(I)) GO TO (220,230,260),I
200         CONTINUE
         WRITE (LP,880)
         GO TO 20
210   WRITE (LP,890) CMNDS(ICMNDS)
      GO TO 20
C
C  DISPLAY STATISTICS FOR 24 HOUR PRECIPITATION
220   CALL PD24ST (ISTAFL,IUNFLG)
      GO TO 20
C
C  DISPLAY RRS STATISTICS
230   IX=-1
      NFLD=3
      IRSTYP=LALL
      IF (NFIELD.LT.3) GO TO 240
         IRSTYP=IBLNK
         NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),IRSTYP,NUM)
         IF (IRSTYP.NE.LALL) IX=IPDCKR(IRSTYP)
         IF (IX.EQ.0) GO TO 250
240   CALL PDRRST (IRSTYP,ISTAFL,IUNFLG)
      GO TO 20
250   WRITE (LP,900) IRSTYP
      GO TO 20
C
C  DISPLAY STRANGER STATION STATISTICS
260   IF (NFIELD.LT.3) GO TO 270
         WRITE (LP,890) CMNDS(ICMNDS)
         GO TO 20
270   CALL PDSRST (IUNFLG)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPOBS COMMAND
C
C  CHECK DATES
280   NFLD=2
      CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),0,0,1,JULBEG,IHRBEG,JULHRB,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'JULBEG=',JULBEG
      NFLD=3
      CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),0,0,1,JULEND,IHREND,JULHRE,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'JULEND=',JULEND
C
C  READ NEXT CARD
290   CALL RWCARD (IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1020) 'RWCARD',IERR
         GO TO 800
         ENDIF
C
C  GET RUN TYPE
      RUNTYP=' '
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),RUNTYP,NCHAR)
C
C  CHECK FOR COMMENT
      IF (RUNTYP.EQ.'$') GO TO 290
C
C  CHECK RUN TYPE
      IF (RUNTYP.EQ.'STAID') GO TO 300
      IF (RUNTYP.EQ.'DTYPE') GO TO 310
      WRITE (LP,920) 'REPORT',NFLD,RUNTYP
      GO TO 20
C
C  DISPLAY BY STATION
300   IF (NFIELD.EQ.1) THEN
         RDTYPE='BOTH'
         WRITE (LP,910) RDTYPE(1:LENSTR(RDTYPE))
         GO TO 320
         ENDIF
C
C  GET DAILY AND/OR RRS TYPE
      RDTYPE=' '
      NFLD=2
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),RDTYPE,NCHAR)
C
C  CHECK REPORT TYPE
      IF (RDTYPE.EQ.'RRS') GO TO 320
      IF (RDTYPE.EQ.'DAILY') GO TO 320
      IF (RDTYPE.EQ.'BOTH') GO TO 320
      WRITE (LP,920) 'DATA',NFLD,RDTYPE
      GO TO 20
C
C  DISPLAY BY DATA TYPE
310   RDTYPE=' '
      NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) GO TO 20
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),RDTYPE,NCHAR)
C
C  CHECK FOR COMMENT OR CONTINUATION INDICATOR
      IF (RDTYPE.EQ.'$'.OR.RDTYPE.EQ.'&') THEN
C     READ NEXT CARD
         CALL RWCARD (IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,1020) 'RWCARD',IERR
            GO TO 800
            ENDIF
         NFLD=0
         GO TO 310
         ENDIF
C
C  PRINT DAILY AND/OR RRS DATA
320   CALL PRTOBS (JULBEG,IHRBEG,JULEND,RUNTYP,RDTYPE,ISTAFL,
     *   IUNFLG,JULHRB,JULHRE,LARRAY,ARRAY)
      IF (RUNTYP.EQ.'DTYPE') GO TO 310
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RSETSTAT COMMAND
C
330   NFLD=2
      IF (NFIELD.LT.2) GO TO 360
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),ISET,NUM)
      IF (ISET.EQ.ISTYP(1)) GO TO 350
      IF (NFIELD.LT.3) GO TO 370
      IF (ISET.EQ.ISTYP(2)) GO TO 340
         WRITE (LP,930) ISET
         GO TO 20
340   NFLD=3
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),IRSTYP,NUM)
      IF (IRSTYP.EQ.LALL) GO TO 350
         IX=IPDCKR(IRSTYP)
         IF (IX.EQ.0) GO TO 380
C
350   CALL PDRSET (ISET,IRSTYP,ISTAFL)
      IWPPDC=1
      GO TO 20
C
360   WRITE (LP,890) CMNDS(ICMNDS)
      GO TO 20
370   WRITE (LP,890) CMNDS(ICMNDS)
      GO TO 20
380   WRITE (LP,900) IRSTYP
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHNGSPEC COMMAND
C
390   CALL PDCHSP (ISTAFL,IWPPDC,IERR)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEFTYPE COMMAND
C
400   CALL PDEFTP (IWPPDC)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATUS COMMAND
C
410   WRITE (LP,940)
C
420   CALL PDSTAT
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELTYPE COMMAND
C
430   CALL PDELTP
      IWPPDC=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELRRS COMMAND
C
440   CALL PDLRRS (ISTAFL,LARRAY,ARRAY)
      IWPPDC=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  EDITDLY COMMAND
C
450   CALL PDEDLY (ISTAFL,IUNFLG)
      IWPPDC=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  EDITRRS COMMAND
C
460   CALL PDERRS (ISTAFL,IUNFLG,LARRAY,ARRAY)
      IWPPDC=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PAGESIZE COMMAND
C
470   NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         WRITE (LP,950) NFLD
         GO TO 20
         ENDIF
      IF (NFIELD.GE.NFLD) THEN
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
         MINVAL=50
         MAXVAL=80
         IF (INTEGR.GE.MINVAL.AND.INTEGR.LE.MAXVAL) THEN
            NPAGE=INTEGR
            ELSE
               NPAGE=80
               WRITE (LP,960) INTEGR,MINVAL,MAXVAL,NPAGE
            ENDIF
         CALL USETPS (NPAGE,MINVAL,MAXVAL,IERR)
         ENDIF
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RRSFREE COMMAND
C
480   IDTAIL=0
      IRLSE=0
C
C  CHECK FOR OPTIONS
      IF (NFIELD.GT.1) THEN
         DO 490 NFLD=2,NFIELD
            IFOUND=0
            NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CHAR12=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NUM)
            IF (CHAR12.EQ.'$') GO TO 490
            IF (CHAR12.EQ.'DETAIL') THEN
               IDTAIL=1
               IFOUND=1
               ENDIF
            IF (CHAR12.EQ.'RLSE') THEN
               IRLSE=1
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,970) CMNDS(ICMNDS),CHAR12
               ENDIF
490         CONTINUE
         ENDIF
C
      CALL PDRRSF (IDTAIL,IRLSE,ARRAY,LARRAY)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RRSDUMP COMMAND
C
500   CALL PDRRSD
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ZERODLY COMMAND
C
510   IZDATA=1
      GO TO 530
C
C  ZERODATE COMMAND
C
520   IZDATA=0
C
C  ZERO DATES
530   DO 540 I=1,NMDTYP
         IDDTDR(8,I)=0
         IDDTDR(9,I)=0
         IDDTDR(10,I)=0
         IDDTDR(11,I)=0
         IDDTDR(12,I)=0
         IDDTDR(13,I)=0
540      CONTINUE
      CALL ULINE (LP,2)
      WRITE (LP,980)
      IWPPDC=1
      IF (IZDATA.EQ.0) GO TO 20
C
C  ZERO DATA
      JULDAY=-1
      CALL PDDAY1 (JULDAY)
      CALL ULINE (LP,2)
      WRITE (LP,990)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RLSEFREE COMMAND
C
550   CALL PDRLSF
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UNSDFREE COMMAND
C
560   CALL PDUNSF
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPSHEF COMMAND
C
570   CALL PVCNTL (ICD,LP,ICDPUN,0,IPDTR,IUNFLG)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPPP24 COMMAND
C
580   CALL PWCNTL (ICD,LP,LP,0,IPDTR,IUNFLG)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET COMMAND
C
590   IF (INUSEF.NE.0) GO TO 600
         CALL ULINE (LP,2)
         WRITE (LP,1000)
         GO TO 20
600   CALL ULINE (LP,2)
      WRITE (LP,1010)
      INUSEF=0
      IWPPDC=1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NEWUSER COMMAND
C
610   INWUSR=1
      GO TO 780
C
C  SET NEW USER NAME
620   NFLD=2
      USERN=' '
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),USERN,NUM)
C
      CALL USRNEW (USERN,PATHN,IERR)
      IF (IERR.NE.0) INWUSR=-1
C
C  GET USER PARAMETERS
      CALL HGTUSR (PUSRID,IERR)
      IF (IERR.GT.0) THEN
         INWUSR=-1
         GO TO 25
         ENDIF
C
C  PRINT HEADER
      CALL UPAGE (LP)
C
C  RESET NEXT RRS RECORD TO BE READ
      NEXTRD=2
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPSIF COMMAND
C
650   IRESET=0
C
C  CHECK FOR OPTIONS
      IF (NFIELD.GT.1) THEN
         DO 655 NFLD=2,NFIELD
            IFOUND=0
            NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CHAR12=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NUM)
            IF (CHAR12.EQ.'$') GO TO 655
            IF (CHAR12.EQ.'RESET') THEN
               IRESET=1
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,970) CMNDS(ICMNDS),CHAR12
               ENDIF
655         CONTINUE
         ENDIF
C
C  READ PREPROCESSOR PARAMETRIC DATA BASE CONTROL RECORDS
      CALL RPPPCO (IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,657) IERR
657   FORMAT ('0**ERROR** PREPROCESSOR PARAMETRIC DATA BASE ',
     *   'CONTROLS NOT SUCCESSFULLY READ. RPPPCO STATUS CODE IS ',I2,
     *   '.')
         ENDIF
C
      CALL PDDSIF (LISIBUF,ISIBUF,IRESET,LARRAY,ARRAY)
C
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELSTA COMMAND
C
680   IF (NFIELD.GT.1) THEN
         DO 690 NFLD=2,NFIELD
            NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CHAR12=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NUM)
            IDTYPE=0
            CALL WPDD (CHAR12,IDTYPE,ISTAT)
            WRITE (LP,*) 'WPDD CALLED : ISTAT=',ISTAT
690         CONTINUE
         ENDIF
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECKRRS COMMAND
C
C  CHECK DATES
700   NFLD=2
      CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),0,0,1,JULBEG,IHRBEG,JULHRB,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'JULBEG=',JULBEG
      NFLD=3
      CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),0,0,1,JULEND,IHREND,JULHRE,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'JULEND=',JULEND
      JULBEH=JULHRB+NHOPDB
      JULENH=JULHRE+NHOPDB
C
C  CHECK FOR OPTIONS
      IDTAIL=0
      IF (NFIELD.GT.3) THEN
         DO 710 NFLD=4,NFIELD
            IFOUND=0
            NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CHAR12=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NUM)
            IF (CHAR12.EQ.'$') GO TO 710
            IF (CHAR12.EQ.'DETAIL') THEN
               IDTAIL=1
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,970) CMNDS(ICMNDS),CHAR12
               ENDIF
710         CONTINUE
         ENDIF
C
      NUMSIF=0
      NUMCHK=0
      IREC=INFREC+1
C
C  READ SIF RECORD
720   CALL PDRSIF (IREC,NXREC,LISIBUF,ISIBUF,IERR)
      CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*)
     *      ' NUMSIF=',NUMSIF+1,
     *      ' STAID=',STAID,
     *      ' IREC=',IREC,
     *      ' NXREC=',NXREC,
     *      ' IERR=',IERR,
     *      ' '
         ENDIF
      IF (IERR.EQ.0) THEN
         NUMSIF=NUMSIF+1
         NADDTP=ISIBUF(10)
         IF (LDEBUG.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,*)
     *         ' NUMSIF=',NUMSIF,
     *         ' NADDTP=',NADDTP,
     *         ' '
            ENDIF
         IF (NADDTP.GT.0) THEN
            NUMCHK=NUMCHK+1
            IF (LDEBUG.GT.0) THEN
               CALL ULINE (LP,1)
               WRITE (LP,'(1H ,A,10(2A2,1H-,I5.5,1H ))')
     *            ' (ISIBUF(I),I=11,11+NADDTP*3-1)=',
     *              (ISIBUF(I),I=11,11+NADDTP*3-1),
     *            ' '
               ENDIF
            JSIBUF=11
            DO 760 IADTYP=1,NADDTP
               CALL UMEMOV (ISIBUF(JSIBUF),PDTYPE,1)
C           CHECK IF RRS DATA TYPE
               IRX=IPDCKR(PDTYPE)
               IF (IRX.NE.0) THEN
                  ISTTYP=0
                  IHRBEG=JULBEH
                  IHREND=JULENH
                  CALL RPDRRS (STAID,ISTTYP,PDTYPE,NVLPOB,
     *               IHRBEG,IHREND,LOBRRS,OBRRS,NOBRRS,LMNRRS,MNRRS,
     *               LARRAY,ARRAY,LSTHR,ISTAT)
                  CALL ULINE (LP,1)
                  WRITE (LP,*) 'RPDRRS CALLED FOR STATION ',STAID,
     *               ' AND DATA TYPE ',PDTYPE,
     *               ' : ISTAT=',ISTAT
                  IF (ISTAT.EQ.0) THEN
                     IJHOUR=0
                     NJHOUR=0
                     NDTA=NOBRRS*NVLPOB
                     IMNRRS=0
                     DO 740 N=1,NDTA,NVLPOB
                        IF (IJHOUR.EQ.0) IJHOUR=IOBRRS(N)
                        IMNRRS=IMNRRS+1
                        IF (IOBRRS(N).LT.IJHOUR) THEN
                           NJHOUR=NJHOUR+1
                           IF (LDEBUG.GT.0) THEN
                              CALL ULINE (LP,1)
                              WRITE (LP,*)
     *                           ' IOBRRS(N)=',IOBRRS(N),
     *                           ' IJHOUR=',IJHOUR,
     *                           ' '
                              ENDIF
                           IF (IDTAIL.EQ.1) THEN
                              IF (MNRRS(IMNRRS).GT.30) THEN
                                 IOBRRS(N)=IOBRRS(N)-1
                                 ENDIF
                              IFDAY=(IOBRRS(N)-NHOPDB)/24+1
                              IFD=IFDAY*24
                              IH=IOBRRS(N)-IFD+NHOPDB
                              CALL MDYH2 (IFDAY,IH,IMO,IDAY,IYR,IHR,
     *                           ITZ,IDSAV,TIME(3))
                              CALL ULINE (LP,1)
                              WRITE (LP,730) N,IMO,IDAY,IYR,IHR,
     *                           MNRRS(IMNRRS),TIME(3)
730   FORMAT (' **WARNING** OBSERVATION NUMBER ',I4,' ',
     *   'DATED ',I2.2,'/',I2.2,'/',I4,'-',I2.2,':',I2.2,A4,' ',
     *   'IS NOT IN CHRONOLOGICAL ORDER.')
                              ENDIF
                           ENDIF
740                     CONTINUE
                     IF (NJHOUR.GT.0) THEN
                        CALL ULINE (LP,1)
                        WRITE (LP,750) NJHOUR,STAID,PDTYPE
750   FORMAT (' **WARNING** ',I4,' ',
     *   'RRS OBSERVATIONS ARE NOT IN ASCENDING CHRONOLOGICAL ORDER ',
     *   'FOR STATION ',A,' AND DATA TYPE ',A,'.')
                        ENDIF
                     IF (IOBRRS(N).GT.IJHOUR) IJHOUR=IOBRRS(N)
                     ENDIF
                  ENDIF
               JSIBUF=JSIBUF+3
760            CONTINUE
            ENDIF
         ENDIF
      IF (NXREC.LE.LSTSIF) THEN
         IREC=NXREC
         GO TO 720
         ENDIF
      WRITE (LP,770) NUMCHK
770   FORMAT ('0**NOTE** ',I4,' RRS DATA PERIODS CHECKED.')
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PDFNDR COMMAND
C
771   NFLD=1
772   IF (NFLD.LT.NFIELD) THEN
         NFLD=NFLD+1
         NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         STAID=' '
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),STAID,NUM)
         IF (STAID(1:1).EQ.'@') GO TO 27
         IF (STAID.EQ.'$'.OR.STAID.EQ.'&') THEN
            CALL RPCARD (IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,1030)
               GO TO 780
               ENDIF
            IFSTCD=1
            CALL WPCARD (IBUF)
C        FIND FIELDS ON CARD
            CALL UFREE (ICDBEG,ICDEND)
C        CHECK IF NO FIELDS ON CARD
            IF (NFIELD.EQ.0) GO TO 20
            NFLD=0
            GO TO 772
            ENDIF
         CALL PDFNDR (STAID,LISIBUF,IFIND,ISIREC,ISIBUF,IFREE,IERR)
         WRITE (LP,*) 'STAID=',STAID,' ISIREC=',ISIREC
         IF (ISIREC.EQ.0) THEN
            CALL UWARN (LP,0,-1)
            WRITE (LP,775) STAID(1:LENSTR(STAID))
775   FORMAT ('0**WARNING** STATION ',A,' NOT FOUND IN PREPROCESSOR ',
     *   'DATA BASE.')
            ENDIF
         GO TO 772
         ENDIF
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SETTODAY COMMAND
C
777   ICARDR=0
      CALL HTODAY (ICARDR)
      TDATES(3)=TDATES(3)-1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF TO DEQ PPDB
780   IF (IPDENQ.EQ.-1) THEN
         CALL ULINE (LP,1)
         CALL PDENDQ ('DEQ ',IERR)
         IPDENQ=-IPDENQ
         ENDIF
C
C  CHECK IF NEED TO UPDATE FILE CONTROL RECORDS
      IF (IWPPDC.EQ.0) GO TO 790
C
C  UPDATE FILE CONTROL RECORDS
      IF (IPDENQ.EQ.-1) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) ' '
         ENDIF
      CALL WPPDCO (IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,1020) 'WPPDCO',IERR
         GO TO 800
         ENDIF
      WRITE (LP,1040)
C
C  CLOSE FILES
790   CALL UCLOSL
C
      IF (INWUSR.EQ.1) GO TO 620
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STOP EXECUTION
C
800   IF ((LPE.NE.LP).AND.(NPSNLT(LPE).NE.0)) THEN
         NSTOP=-1
         CALL USTOP (LPE,NSTOP)
         ENDIF
      NSTOP=-IUSTOP
      IF (NSTOP.EQ.0.AND.ISTOPX.GT.0) NSTOP=-ISTOPX
      IF (NSTOP.EQ.0) NSTOP=-1
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LPD,1)
         WRITE (LPD,*) 'ISTOPX=',ISTOPX,
     *     ' NSTOP=',NSTOP
         ENDIF
      CALL USTOP (LP,NSTOP)
C
      INCLUDE 'cluprimc'
C
      CALL USTOP2
C
      STOP
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
810   FORMAT ('0**WARNING** USER NAME FOUND IN USERPARM FILE (',2A4,
     *   ') IS DIFFERENT FROM THAT FOUND IN ',
     *   'PREPROCESSOR DATA BASE (',2A4,').')
820   FORMAT (' ')
830   FORMAT (' ?')
840   FORMAT ('0**ERROR** INVALID COMMAND. CARD IGNORED.')
850   FORMAT ('0DEBUG OPTIONS SET : IPDTR=',I2,3X,'IPDDB=',I2,3X,
     *   'NOBUG=',I2,3X,
     *   'IHCLTR=',I2,3X,'IHCLDB=',I2)
860   FORMAT ('0**ERROR** INVALID CARD FOR STATION COMMAND. ',
     *       ' CARD IGNORED.')
870   FORMAT ('0**ERROR** INVALID CARD FOR UNITS COMMAND. ',
     *       ' CARD IGNORED.')
880   FORMAT ('0**ERROR** THE DATA TYPE IN FIELD 2 IS INVALID.',
     *       ' CARD IGNORED.')
890   FORMAT ('0**ERROR** INVALID NUMBER OF FIELDS ON ',A8,
     *       ' COMMAND CARD.')
900   FORMAT ('0**ERROR** INVALID RRS TYPE ',A4,' ENTERED.')
910   FORMAT ('0**WARNING** ONLY ONE FIELD FOUND ON CARD. ',A,
     *   ' ASSUMED.')
920   FORMAT ('0**ERROR** INVALID ',A,' TYPE IN FIELD ',I2,' : ',A)
930   FORMAT ('0**ERROR** INVALID TYPE ',A4,' IN FIELD 2. ',
     *   'CARD IGNORED.')
940   FORMAT ('0**NOTE** COMMAND DUMPUTIL HAS BEEN REPLACED BY STATUS.')
950   FORMAT ('0**ERROR** FIELD ',I2,' ON PAGESIZE COMMAND IS NOT AN ',
     *      'INTEGER. CARD IGNORED.')
960   FORMAT ('0**WARNING** PAGESIZE VALUE ',I3,' IS NOT WITHIN ',
     *       'THE RANGE ',I2,' THRU ',I2,' AND WILL BE SET TO ',I2,'.')
970   FORMAT ('0**ERROR** ',A,' OPTION ',A,' IS INVALID.')
980   FORMAT ('0**NOTE** FIRST AND LAST DATE OF DATA FOR EACH ',
     *   'DAILY DATA TYPE SET TO ZERO.')
990   FORMAT ('0**NOTE** DATA RECORDS INITIALIZED FOR ALL DAILY ',
     *   'DATA TYPES.')
1000  FORMAT ('0**NOTE** INUSE FLAG IS NOT CURRENTLY SET.')
1010  FORMAT ('0**NOTE** INUSE FLAG SUCCESSFULLY RESET.')
1020  FORMAT ('0**ERROR** CALLING ROUTINE ',A,'. STATUS CODE = ',I2)
1030  FORMAT ('0**NOTE** STOP COMMAND ASSUMED.')
1040  FORMAT ('0**NOTE** PREPROCESSOR DATA FILE CONTROL ',
     *   'INFORMATION SUCCESSFULLY UPDATED.')
C
      END
