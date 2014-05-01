C MODULE PRUTMAIN
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR PROCESSED DATA BASE UTILITY PROGRAM.
C
      SUBROUTINE PRUTMAIN_MAIN
C
      CHARACTER*4 DTYPE,DUNITS,TSTYPE
      CHARACTER*4 XEND/'END'/
      CHARACTER*8 DDNAME,USERN
      CHARACTER*8 TSID,TSIDF,FUTOPT
      PARAMETER (MCMNDS=27)
      CHARACTER*8 CMNDS(MCMNDS)/
     *   'DEFTYPE ','CHNGTYPE','DELTYPE ',
     *   'DEFTS   ','DEFTSF  ','CHNGTS  ',
     *   'CHNGTSF ','DEBUG   ','NEWUSER ',
     *   'TSHDRS  ','TSDATA  ','DUMPDFIX',
     *   'DUMPTSIX','DUMPTS  ','DUMPSTAT',
     *   'MINDAY  ','STOP    ','TSEDIT  ',
     *   'PAGESIZE','$       ','DSATTR  ',
     *   'STATUS  ','DUMPSHEF','DELTS   ',
     *   'CHKNUMTS','DUMPFILE','        '/
      CHARACTER*9 CMND
      CHARACTER*12 CHAR12
      CHARACTER*150 PATHN
C
      DIMENSION USERID(2)
      DIMENSION IHEAD(22)
      PARAMETER (LXBUF=100)
      DIMENSION XBUF(LXBUF)
      PARAMETER (LTSDAT=1000)
      DIMENSION TSDAT(LTSDAT)
      PARAMETER (LIWORK=20000)
      DIMENSION IWORK(LIWORK)
      DIMENSION ITBUF(32)
C
      INCLUDE 'uio'
      INCLUDE 'uoptnx'
      common /CMPRDUTIL/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'upagex'
      INCLUDE 'ustopx'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'uunits'
      INCLUDE 'ufstcd'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/killcd'
      INCLUDE 'common/toterz'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'ucommon/uppint'
C
      EQUIVALENCE (USERID,PUSRID)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/prutmain.f,v $
     . $',                                                             '
     .$Id: prutmain.f,v 1.11 2004/08/10 14:36:31 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      LDEBUG=0
C
      IUSTOP=0
      IPRMPT=0
      NOVPRT=0
      NOBUG=0
      INDERR=0
      NWARNT=0
      NERRST=0
      IOERR=LPE
      IPPFLG=0
      INWUSR=0
C
      IPR=LP
      IODBUG=LPD
C
      CALL UPRIMO_PRDU()
C
C  GET USER PARAMETERS
      CALL HGTUSR (PUSRID,IERR)
      IF (IERR.NE.0) GO TO 640
C
C  SET OPTIONS FOR UTILITY ROUTINES
      IPAGE=0
      IERPRT=0
      ICDPRT=0
      ITMPRT=1
      CALL USETOP (IPAGE,IERPRT,ICDPRT,ITMPRT)
      CALL USETO1 ('NOOVERPRINT',IERR)
C
C  PRINT PAGE HEADER
      CALL UPAGE (LP)
C
C  GET DATA SET FROM WHICH PROGRAM IS BEING EXECUTED
      NUNIT=0
      IPRERR=-1
      CALL UPRDSN ('STEPLIB ',NUNIT,'NONE',IPRERR,LP,IERR)
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
      IF (LPE.NE.LP) THEN
         NUNIT=LPE
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) LPE=LP
         ENDIF
C
      IF (INDERR.GT.0) GO TO 650
C
C  STORE CURRENT HYDROLOGIC DATES IN COMMON BLOCK
      CALL HSYSDA (JULDAT)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ PROCESSED DATA BASE CONTROL INFORMATION
10    CALL RPDBCI (IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,20)
20    FORMAT ('0**ERROR** ERROR ENCOUNTERED CALLING ROUTINE RPDBCI.')
         GO TO 640
         ENDIF
C
C  CHECK USER NAME FROM USERPARM FILE AND PDB FILE
      IF (USERID(1).NE.USERPR(1).AND.USERID(2).NE.USERPR(2)) THEN
         CALL UWARN (LP,0,-1)
         WRITE (LP,30) USERID,USERPR
30    FORMAT ('0**WARNING** USER NAME FOUND IN USERPARM FILE (',2A4,
     *   ') IS DIFFERENT FROM THAT FOUND IN ',
     *   'PROCESSED DATA BASE (',2A4,',).')
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET FLAG TO INDICATE IF CONTROLS NEED TO BE UPDATED
      IWPRDC=0
C
40    INWUSR=0
C
50    IF (IPRMPT.EQ.1) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) '?'
         ENDIF
C
C  READ CARD
      CALL RPCARD (IBUF,IERR)
      IF (IERR.NE.0) GO TO 590
C
      IFSTCD=1
      CALL WPCARD (IBUF)
C
C  FIND FIELD ON CARD
      IBCOL=1
      IECOL=72
      CALL UFREE (IBCOL,IECOL)
C
C  CHECK IF NO FIELDS ON CARD
      IF (NFIELD.EQ.0) GO TO 40
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=LEN(CMND)
      IF (NCHAR.GT.MCHAR) NCHAR=MCHAR
      CMND=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CMND,NCHAR)
      IF (CMND(1:1).EQ.'@') CMND=CMND(2:LEN(CMND))
C
      DO 60 ICMNDS=1,MCMNDS
         IF (CMND.EQ.CMNDS(ICMNDS)) GO TO 70
60       CONTINUE
      GO TO 80
C
70    GO TO (160,160,160,
     *       170,170,170,
     *       170,110,140,
     *         220,220,180,
     *         210,230,300,
     *         310,610,360,
     *         370,100,130,
     *           200,410,430,
     *           490,500,80),ICMNDS
C
C  INVALID COMMAND
80    CALL UEROR (LP,0,-1)
      WRITE (LP,90)
90    FORMAT ('0**ERROR** INVALID COMMAND.')
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COMMENT CARD
C
100   GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEBUG COMMAND
C
110   IF (NFIELD.GE.2) THEN
         NFLD=2
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NOBUG)
         ENDIF
      IF (NFIELD.GE.3) THEN
         NFLD=3
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IUTLTR)
         ENDIF
      IF (NFIELD.GE.4) THEN
         NFLD=4
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IUTLDB)
         ENDIF
      IF (NFIELD.GE.5) THEN
         NFLD=5
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IPRTR)
         ENDIF
      IF (NFIELD.GE.6) THEN
         NFLD=6
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IPRDB)
         ENDIF
      WRITE (LP,120) NOBUG,
     *   IUTLTR,IUTLDB,
     *   IPRTR,IPRDB
120   FORMAT ('0DEBUG OPTIONS SET : ',
     *   'NOBUG=',I2,3X,
     *   'IUTLTR=',I2,3X,'IUTLDB=',I2,3X,
     *   'IPRTR=',I2,3X,'IPRDB=',I2,3X)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DSATTR COMMAND
C
130   NUNIT1=1
      NUNIT2=99
      DDNAME=' '
      IPRHDR=1
      IROUND=0
      CALL UDDTBL (NUNIT1,NUNIT2,DDNAME,IPRHDR,IROUND,LBUFRS,LP,IERR)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NEWUSER COMMAND
C
140   INWUSR=1
      GO TO 610
C
C  SET NEW USER NAME
150   NFLD=2
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
         GO TO 50
         ENDIF
C
C  PRINT PAGE HEADER
      CALL UPAGE (LP)
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEFTYPE, CHNGTYPE AND DELTYPE COMMANDS
C
160   ICMD=0
      IF (CMND.EQ.'DEFTYPE') ICMD=1
      IF (CMND.EQ.'CHNGTYPE') ICMD=2
      IF (CMND.EQ.'DELTYPE') ICMD=3
      CALL PDFTCR (ICMD)
      IWPRDC=1
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEFTS, DEFTSF, CHNGTS AND CHNGTSF COMMANDS
C
170   ICMD=0
      IF (CMND.EQ.'DEFTS') ICMD=1
      IF (CMND.EQ.'DEFTSF') ICMD=2
      IF (CMND.EQ.'CHNGTS') ICMD=3
      IF (CMND.EQ.'CHNGTSF') ICMD=4
      CALL PDTSCR (ICMD,LIWORK,IWORK)
      IWPRDC=1
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATUS COMMAND
C
180   CALL ULINE (LP,2)
      WRITE (LP,190)
190   FORMAT ('0**NOTE** COMMAND DUMPDFIX HAS BEEN REPLACED BY STATUS.')
C
200   CALL PTDATF
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPTSIX COMMAND
C
210   IRESET=0
C
C  CHECK FOR OPTIONS
      IF (NFIELD.GT.1) THEN
         DO 215 NFLD=2,NFIELD
            IFOUND=0
            NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CHAR12=' '
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR12,NUM)
            IF (CHAR12.EQ.'$') GO TO 215
            IF (CHAR12.EQ.'RESET') THEN
               IRESET=1
               IFOUND=1
               ENDIF
            IF (IFOUND.EQ.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,213) CMNDS(ICMNDS),CHAR12
213   FORMAT ('0**ERROR** ',A,' OPTION ',A,' IS INVALID.')
               ENDIF
215         CONTINUE
         ENDIF
C
      CALL PTINDX (IRESET,IWPRDC)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  TSDATA AND TSHDRS COMMANDS
C
C  CHECK IF EXECUTED A PREVIOUS COMMAND THAT PRINTED LINES
220   IF (IWPRDC.EQ.1) CALL UPAGE (LP)
C
      ITYPE=0
      IF (CMND.EQ.'TSHDRS') ITYPE=0
      IF (CMND.EQ.'TSDATA') ITYPE=1
      CALL PTSALL (ITYPE,LIWORK,IWORK,IERR)
      IF (IERR.EQ.0) GO TO 40
      GO TO 590
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPTS COMMAND
C
230   CALL RCOMND (XEND,1,IND)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
      IF (IND) 590,240,40
C
C  GET ID AND DATA TYPE
240   TSID=' '
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=LEN(TSID)
      IF (NCHAR.GT.MCHAR) NCHAR=MCHAR
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),TSID,NCHAR)
      NFLD=NFLD+1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=LEN(DTYPE)
      IF (NCHAR.GT.MCHAR) NCHAR=MCHAR
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),DTYPE,NCHAR)
C
      IF (NFIELD.EQ.2.OR.NFIELD.EQ.3) GO TO 290
      NFLD=3
      CALL UINTFX (JHOUR,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      NFLD=NFLD+1
      CALL UINTFX (ITSTEP,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      NFLD=NFLD+1
      CALL UINTFX (NUMVAL,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      NFLD=NFLD+1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),DUNITS,NCHAR)
      NFLD=NFLD+1
      CALL URELFX (RMISS,IFSTRT(NFLD),IFSTOP(NFLD),IERR,0)
C
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL ULINE (LP,1)
      WRITE (LP,*)
     *   'TSID=',TSID,
     *   ' DTYPE=',DTYPE,
     *   ' JHOUR=',JHOUR,
     *   ' ITSTEP=',ITSTEP,
     *   ' NUMVAL=',NUMVAL,
     *   ' DUNITS=',DUNITS,
     *   ' RMISS=',RMISS,
     *   ' '
C
      IFUT=0
      IF (NFIELD.EQ.8) IFUT=1
C
      IF (JHOUR.EQ.0.OR.
     *    ITSTEP.EQ.0.OR.
     *    NUMVAL.EQ.0) THEN
         IF (IFUT.EQ.0) THEN
            CALL RPRDH (TSID,DTYPE,LXBUF,IHEAD,NUMX,XBUF,TSIDF,IERR)
            WRITE (LP,*) 'RPRDH CALLED : IERR=',IERR
            IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
            ENDIF
         IF (IFUT.EQ.1) THEN
            CALL RPRDFH (TSID,DTYPE,LXBUF,IHEAD,NUMX,XBUF,IERR)
            WRITE (LP,*) 'RPRDFH CALLED : IERR=',IERR
            IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
            ENDIF
         IF (JHOUR.EQ.0) THEN
            JHOUR=IHEAD(14)
            CALL ULINE (LP,1)
            WRITE (LP,*) '**NOTE** JHOUR SET TO ',JHOUR
            ENDIF
         IF (ITSTEP.EQ.0) THEN
            ITSTEP=IHEAD(2)
            CALL ULINE (LP,1)
            WRITE (LP,*) '**NOTE** ITSTEP SET TO ',ITSTEP
            ENDIF
         IF (NUMVAL.EQ.0) THEN
            NUMVAL=IHEAD(5)
            CALL ULINE (LP,1)
            WRITE (LP,*) '**NOTE** NUMVAL SET TO ',NUMVAL
            ENDIF
         ENDIF
C
      IF (IFUT.EQ.1) GO TO 280
C
C  READ REGULAR DATA
      IFPTR=0
      CALL RPRD (TSID,DTYPE,JHOUR,ITSTEP,NUMVAL,DUNITS,RMISS,
     *   TSDAT,IFPTR,LIWORK,IWORK,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) 'RPRD CALLED : IERR=',IERR
      CALL RPRDD (TSID,DTYPE,JHOUR,ITSTEP,NUMVAL,DUNITS,RMISS,
     *   LTSDAT,TSDAT,IFPTR,LIWORK,IWORK,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) 'RPRDD CALLED : IERR=',IERR
      IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
      CALL ULINE (LP,1)
      WRITE (LP,*) 'IFPTR=',IFPTR
      NTSDAT=NUMVAL*IWORK(3)
      CALL ULINE (LP,1)
      WRITE (LP,250) (IWORK(I),I=1,LENHED)
250   FORMAT (' HEADER=',
c
c     DR17865
cfan
c     Integer array can't be printed out in F format in Fortran 90
cfan
cfan *   7(I4,1X),2A4,2(1X,A4),2F8.2,2(1X,I7,1X,I4),1X,5A4)
     *   7(I4,1X),2A4,2(1X,A4),2F8.2,2(1X,I7,1X,I4),1X,5A4)   !cfan10/2006
      IXLEN=IWORK(1)-LENHED
      IXSTRT=LENHED+1
      IXEND=LENHED+IXLEN
      IF (IXLEN.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,260) (IWORK(I),I=IXSTRT,IXEND)
260   FORMAT (' XBUF=',(1X,20A4))
         ENDIF
      NLINES=1+((NTSDAT+11)/12)
      CALL ULINE (LP,NLINES)
      WRITE (LP,270) (TSDAT(I),I=1,NTSDAT)
270   FORMAT (' DATA=',12F9.3)
      GO TO 230
C
C  READ FUTURE DATA
280   CALL RPRDF (TSID,DTYPE,JHOUR,ITSTEP,NUMVAL,DUNITS,RMISS,
     *   TSDAT,LIWORK,IWORK,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) 'RPRDF CALLED : IERR=',IERR
      CALL RPRDDF (TSID,DTYPE,JHOUR,ITSTEP,NUMVAL,DUNITS,RMISS,
     *   LTSDAT,TSDAT,LIWORK,IWORK,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) 'RPRDDF CALLED : IERR=',IERR
      IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
      NTSDAT=NUMVAL*IWORK(3)
      CALL ULINE (LP,1)
      WRITE (LP,250) (IWORK(I),I=1,LENHED)
      IXLEN=IWORK(1)-LENHED
      IXSTRT=LENHED+1
      IXEND=LENHED+IXLEN
      IF (IXLEN.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,260) (IWORK(I),I=IXSTRT,IXEND)
         ENDIF
      NLINES=1+((NTSDAT+11)/12)
      CALL ULINE (LP,NLINES)
      WRITE (LP,270) (TSDAT(I),I=1,NTSDAT)
      GO TO 230
C
C  READ REGULAR HEADER
290   CALL ULINE (LP,1)
      WRITE (LP,*) 'TSID=',TSID,' DTYPE=',DTYPE
      IF (NFIELD.EQ.2) THEN
         CALL RPRDH (TSID,DTYPE,LXBUF,IHEAD,NUMX,XBUF,TSIDF,IERR)
         WRITE (LP,*) 'RPRDH CALLED : IERR=',IERR
         IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
         CALL ULINE (LP,1)
         WRITE (LP,250) IHEAD
         WRITE (LP,*) 'TSIDF=',TSIDF
         IF (NUMX.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,260) (XBUF(I),I=1,NUMX)
            ENDIF
         GO TO 230
         ENDIF
C
C  READ FUTURE HEADER
      CALL RPRDFH (TSID,DTYPE,LXBUF,IHEAD,NUMX,XBUF,IERR)
      WRITE (LP,*) 'RPRDFH CALLED : IERR=',IERR
      IF (IERR.NE.0.AND.IERR.NE.2) GO TO 230
      CALL ULINE (LP,1)
      WRITE (LP,250) IHEAD
      IF (NUMX.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,260) (XBUF(I),I=1,NUMX)
         ENDIF
      GO TO 230
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPSTAT COMMAND
C
300   CALL PIXACC
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  MINDAY COMMAND
C
310   NFLD=2
      IF (NFIELD.EQ.NFLD) GO TO 330
         CALL UEROR (LP,0,-1)
         WRITE (LP,320) NFIELD,NFLD
320   FORMAT ('0**ERROR** NUMBER OF FIELDS FOUND (',I2,
     *   ' DOES NOT EQUAL NUMBER OF FIELDS EXPECTED (',I2,').')
         GO TO 40
330   CALL UINTFX (N,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.EQ.0.AND.(N.GE.1.AND.N.LE.31)) GO TO 350
         CALL UEROR (LP,0,-1)
         WRITE (LP,340) N
340   FORMAT ('0**ERROR** ',I4,' IS AN INVALID VALUE FOR MINDAY.')
         GO TO 40
350   MINDAY=N
      IWPRDC=1
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  TSEDIT COMMAND
C
360   IPRINT=1
      CALL PEDITS (IPRINT,LIWORK,IWORK,IERR)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PAGESIZE COMMAND
C
370   MINVAL=50
      MAXVAL=80
C
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         NPAGE=80
         WRITE (LP,380) NPAGE
380   FORMAT ('0**ERROR** FIELD 2 IS NOT AN INTEGER. PAGESIZE SET ',
     *   'TO ',I2,'.')
         GO TO 400
         ENDIF
C
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
      IF (INTEGR.GE.MINVAL.AND.INTEGR.LE.MAXVAL) THEN
         NPAGE=INTEGR
         ELSE
            NPAGE=80
            CALL UWARN (LP,0,-1)
            WRITE (LP,390) INTEGR,MINVAL,MAXVAL,NPAGE
390   FORMAT ('0**WARNING** PAGESIZE VALUE ',I3,' IS NOT WITHIN ',
     *       'THE RANGE ',I2,' THRU ',I2,' AND WILL BE SET TO ',I2,'.')
         ENDIF
C
400   CALL USETPS (NPAGE,MINVAL,MAXVAL,IERR)
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPSHEF COMMAND
C
410   INDERR=0
      NFLD=1
C
      IF (NFIELD.GT.1) THEN
         NHRADD=0
         NHSWCH=0
C     GET DATES
         IPRERR=0
         NFLD=NFLD+1
         CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),NHRADD,NHSWCH,IPRERR,
     *      JULBEG,INTHRB,JULHRB,IERR)
         IF (IERR.NE.0) THEN
            CALL UEROR (LP,0,-1)
            WRITE (LP,420) 'DATE',NFLD
420   FORMAT ('0**ERROR** INVALID ',A,' FOUND IN FIELD ',I2,'.')
            INDERR=1
            ENDIF
         IF (IPRDB.GT.0) WRITE (IOGDB,*) 'JULBEG=',JULBEG
         IF (NFIELD.GT.2) THEN
            NFLD=NFLD+1
            CALL HDATEC (IFSTRT(NFLD),IFSTOP(NFLD),NHRADD,NHSWCH,IPRERR,
     *         JULEND,INTHRE,JULHRE,IERR)
            IF (IERR.NE.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,420) 'DATE',NFLD
               INDERR=1
               ENDIF
            IF (IPRDB.GT.0) WRITE (IOGDB,*) 'JULEND=',JULEND
            ENDIF
         ENDIF
C
      CALL PRDSHF (JULBEG,INTHRB,JULHRB,JULEND,INTHRE,JULHRE,
     *   NFLD,INDERR,IENDIN)
      IF (IENDIN.EQ.1) GO TO 590
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELTS COMMAND
C
C  READ CARD
430   CALL RCOMND (XEND,1,IND)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IND) 590,440,40
C
C  GET ID AND DATA TYPE
440   TSID=' '
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(TSID)) NCHAR=LEN(TSID)
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),TSID,NCHAR)
      NFLD=NFLD+1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(DTYPE)) NCHAR=LEN(DTYPE)
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),DTYPE,NCHAR)
      IF (NFIELD.GT.2) THEN
         NFLD=NFLD+1
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         IF (NCHAR.GT.LEN(FUTOPT)) NCHAR=LEN(FUTOPT)
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),FUTOPT,NCHAR)
         IF (FUTOPT.EQ.'FUTURE') THEN
            IFUT=1
            ELSE
               CALL UEROR (LP,0,-1)
               WRITE (LP,420) 'FUTURE KEYWORD',NFLD
               GO TO 430
            ENDIF
         ELSE
            IFUT=0
         ENDIF
C
      ICKREF=1
      IPRERR=0
      CALL WPRDEL (TSID,DTYPE,IFUT,ICKREF,IPRERR,IERR)
      IF (IERR.EQ.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,450) TSID,DTYPE
450   FORMAT ('0**NOTE** TIME SERIES FOR IDENTIFIER ',A,
     *   ' AND DATA TYPE ',A,' SUCCESSFULLY DELETED.')
         IWPRDC=1
         ELSE
            IF (IERR.EQ.1) THEN
               CALL UWARN (LP,0,-1)
               WRITE (LP,460) TSID,DTYPE
460   FORMAT ('0**WARNING** TIME SERIES FOR IDENTIFIER ',A,
     *   ' AND DATA TYPE ',A,' NOT FOUND.')
               ENDIF
            IF (IERR.EQ.2) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,470) TSID,DTYPE
470   FORMAT ('0**ERROR** TIME SERIES FOR IDENTIFIER ',A,
     *   ' AND DATA TYPE ',A,' IS USED BY ANOTHER TIME SERIES.')
               ENDIF
            IF (IERR.EQ.3) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,480) DTYPE
480   FORMAT ('0**ERROR** ',A,' IS AN INVALID DATA TYPE.')
               ENDIF
         ENDIF
      GO TO 430
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHKNUMTS COMMAND
C
490   IF (INWUSR.EQ.-1) GO TO 40
C
      CALL PRCNTS
C
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DUMPFILE COMMAND
C
500   NCHK1=2
      NCHK2=6
      IF (NFIELD.LT.NCHK1.OR.NFIELD.GT.NCHK2) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,510) NCHK1,NCHK2,NFIELD
510   FORMAT ('0**ERROR** A MINIMUM OF ',I2,
     *   ' AND A MAXIMUM OF ',I2,' FIELDS WERE EXPECTED BUT ',I2,
     *   ' WERE FOUND.')
         GO TO 40
         ENDIF
C
C  CHECK SIZE OF WORK ARRAY
      IF (LIWORK.LT.LENHED) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,520) LIWORK,LENHED
520   FORMAT ('0**ERROR** SIZE OF WORK ARRAY (',I5,
     *   ') TOO SMALL TO READ TIME SERIES RECORDS. ',
     *   I5,' WORDS NEEDED.')
         GO TO 40
         ENDIF
C
      NFLD=1
C
C  GET FILE NUMBER
530   NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) GO TO 40
      CALL UINTFX (NFILE,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
C
      IF (NFILE.GT.NMPRDF) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,540) NFILE,NMPRDF
540   FORMAT ('0**ERROR** FILE NUMBER (',I2,') EXCEEDS NUMBER OF ',
     *   'PROCESSED DATA BASE FILES (',I2,').')
         GO TO 530
         ENDIF
C
      IF (TSCNTR(3,NFILE).EQ.2) THEN
         WRITE (LP,550) NFILE,IUNIT
550   FORMAT ('0FILE ',I2,' (UNIT ',I2,') IS EMPTY')
         GO TO 530
         ENDIF
C
C  GET UNIT NUMBER
      IUNIT=KPRDTU(NFILE)
C
      WRITE (LP,560) NFILE,IUNIT
560   FORMAT ('0CONTENTS OF FILE ',I2,' (UNIT ',I2,'):')
C
      IREC=2
      NUMTS=0
C
C  READ TIME SERIES HEADER
570   NREC=2
      CALL RVLRCD (IUNIT,IREC,NREC,ITBUF,LRECLT,IERR)
      IF (IERR.NE.0) GO TO 40
C
      NUMTS=NUMTS+1
C
C  EXPAND HEADER
      CALL PEXPBT (ITBUF,IWORK)
C
      CALL UMEMOV (IWORK(8),TSID,2)
      CALL UMEMOV (IWORK(10),TSTYPE,1)
      WRITE (LP,580) NUMTS,IREC,TSID,TSTYPE
580   FORMAT (' NUMTS=',I4,' IREC=',I5,' TSID=',A,' TSTYPE=',A)
C
      IXSIZ=IWORK(1)-LENHED
      NUM=LRECLT*2-LENHDC
      NREC=(IXSIZ+IWORK(4)-NUM+LRECLT-1)/LRECLT
      IREC=IREC+NREC+2
      IF (IREC.LT.TSCNTR(3,NFILE)) GO TO 570
C
      GO TO 530
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
590   CALL ULINE (LP,2)
      WRITE (LP,600)
600   FORMAT ('0**NOTE** STOP COMMAND ASSUMED.')
C
C  UPDATE CONTROL RECORDS
610   IF (IWPRDC.EQ.1) THEN
         CALL WPDBCO (IERR)
         IF (IERR.EQ.0) THEN
            CALL ULINE (LP,2)
            WRITE (LP,620)
620   FORMAT ('0**NOTE** PROCESSED DATA FILE CONTROL ',
     *   'INFORMATION SUCCESSFULLY UPDATED.')
            ELSE
               CALL UEROR (LP,0,-1)
               WRITE (LP,630) IERR
630   FORMAT ('0**ERROR** IN WRITING CONTROL RECORDS. STATUS=',I4)
            ENDIF
         ENDIF
C
640   CALL UCLOSL
C
      IF (INWUSR.EQ.1) GO TO 150
C
C  STOP EXECUTION
C
650   IF ((LPE.NE.LP).AND.(NPSNLT(LP).NE.0)) THEN
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
      END
