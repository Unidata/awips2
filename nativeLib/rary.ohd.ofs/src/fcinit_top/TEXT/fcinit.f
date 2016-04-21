C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE FCINIT
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR FCINIT COMMANDS.
C
      SUBROUTINE FCINIT (MC,C,MOLDC,OLDC,MRSTC,RESETC,MP,P,MOLDP,OLDP,
     *   MT,T,MOLDT,OLDT,MTS,TS,MOLDTS,OLDTS)
C
      CHARACTER*8 OLDOPN,DDNAME,TYPMSG,USERN,QLFNEW
      CHARACTER*10 CLARG,CLCHK1,CLCHK2
      CHARACTER*10 APPSVAR,APPSVAL
      PARAMETER (MCOMND=23)
      CHARACTER*10 XCOMND(MCOMND)/
     *     'STATUS',
     *     'SETBUG',
     *     'DEBUG',
     *     'SEGDEF',
     *     'RESEGDEF',
     *     'FGDEF',
     *     'SPECIALFG',
     *     'CGDEF',
     *     'DELETE',
     *     'PRINTSEGS',
     *     'PUNCHSEGS',
     *     'SEGDATE',
     *     'DEF-RC',
     *     'PRINTRC',
     *     'PRINTOPS',
     *     'PUNCHRC',
     *     'PUNCHFG',
     *     'PUNCHCG',
     *     'STOP',
     *     'NEWUSER',
     *     'DSATTR',
     *     'PUNCHCHR',
     *     'INCLUDE'/
      CHARACTER*50 XFLSTA(5)
     *    /'DEFINING A SEGMENT',
     *     'DEFINING A FGROUP',
     *     'DEFINING A CGROUP',
     *     'DATEING SEGMENT',
     *     'DEFINING A RATING CURVE'/
      CHARACTER*72 XCHAR
      CHARACTER*80 XCARD
      CHARACTER*150 PATHN
C
      DIMENSION C(MC),OLDC(MOLDC),RESETC(MRSTC)
      DIMENSION P(MP),OLDP(MOLDP)
      DIMENSION T(MT),OLDT(MOLDT)
      DIMENSION TS(MTS),OLDTS(MOLDTS)
      INTEGER T,OLDT
C
      common /CMFCINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'upagex'
      INCLUDE 'uunits'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdispl'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/fcdflt'
      INCLUDE 'common/where'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/errdat'
      INCLUDE 'common/toterz'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fclfls'
      INCLUDE 'hclcommon/huprm2'
      COMMON /HCPUCK/ ITOTCP,IBGSEC,ILASEC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcinit.f,v $
     . $',                                                             '
     .$Id: fcinit.f,v 1.9 2002/02/11 20:26:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

      IOPNUM=-1
      CALL FSTWHR ('FCINIT  ',IOPNUM,OLDOPN,IOLDOP)
C
      LDEBUG=0
C
      LXCARD=72
      IENDIN=0
C
C  INITIALIZE TOTAL CPU TIME AND INCREMENTAL COUNTER FOR CPU
      ITOTCP=0
      CALL URTIMR (ITMELA,ITOTCP)
C
C  SET BEGINNING WALL CLOCK TIME
      CALL UDATEI (IMON,IDAY,IYEAR,IHRMIN,ISEC,JULDAT,IERR)
      IBGSEC=(IHRMIN/100)*3600+MOD(IHRMIN,100)*60+(ISEC+50)/100
      ILASEC=IBGSEC
C
      INOSTOP=0
C
C  GET NUMBER OF UNIX COMMAND LINE ARGUMENTS
      NCLARG=IARGC()
      IF (NCLARG.GE.1) THEN
C     PROCESS EACH COMMAND LINE ARGUMENT
         DO 5 ICLARG=1,NCLARG
            CALL GETARG (ICLARG,CLARG)
            CLCHK1='NOSTOP'
            NCHAR=0
            NCONVT=0
            CALL UCP2LC (CLCHK1,CLCHK2,IERR)
            IF (CLARG.EQ.CLCHK1.OR.CLARG.EQ.CLCHK2) THEN
               INOSTOP=1
               WRITE (IPR,*) '**NOTE** ',CLARG(1:LENSTR(CLARG)),
     *            ' OPTION FOUND AT COMMAND LINE ARGUMENT NUMBER ',
     *            ICLARG,'.'
               GO TO 5
               ENDIF
5           CONTINUE
         ENDIF
C
C  READ INPUT CARDS, CHECK FOR INCLUDES AND WRITE TO TEMPORARY DATASET
      IUNIT=IN
      CALL FCRCDS (IN,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    CALL INSTRT
C
C  GET DEFAULT HCL VALUES
      CALL HLDCBS ('FCEXEC  ',IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,330) 'HYDROLOGIC COMMAND LANGUAGE'
         CALL KILLPM
         GO TO 300
         ENDIF
C
C  GET HCL AND USER DEFAULT VALUES
      CALL FIDFLT
C
C  CHECK TO SEE IF LAST RUN ENDED ABNORMALLY
      IF (IFLSTA.GT.0) THEN
         APPSVAR='LOGNAME'
         LAPPSVAR=LENSTR(APPSVAR)
         CALL GET_APPS_DEFAULTS (APPSVAR,LAPPSVAR,APPSVAL,LAPPSVAL)
         IF (APPSVAL(1:LAPPSVAL).EQ.'scv'.AND.INOSTOP.EQ.0) THEN
            WRITE (IPR,*) '**NOTE** NOSTOP OPTION SET FOR LOGNAME ',
     *         APPSVAL(1:LAPPSVAL),'.'
            INOSTOP=1
            ENDIF
         IF (INOSTOP.EQ.0) THEN
            WRITE (IPR,310)
            WRITE (IPR,360)
            WRITE (IPR,360)
            WRITE (IPR,360)
            ENDIF
         WRITE (IPR,370) PGMNAM
         WRITE (IPR,380) XFLSTA(IFLSTA)(1:LENSTR(XFLSTA(IFLSTA)))
         WRITE (IPR,390)
         WRITE (IPR,400)
         WRITE (IPR,410) PGMNAM
         WRITE (IPR,420) PGMNAM
         IFLSTA=0
         IREC=2
         CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
         KLCODE=16
         IF (INOSTOP.EQ.0) GO TO 300
         ENDIF
C
C  READ PROCESSED DATA BASE CONTROLS
      CALL RPDBCI (IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,330) 'PROCESSED'
         CALL KILLPM
         GO TO 300
         ENDIF
C
C  READ PREPROCESSOR PARAMETRIC DATA BASE CONTROLS
      IWRPPP=0
      CALL RPPPCO (IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,330) 'PREPROCESSOR PARAMETRIC'
         CALL KILLPM
         GO TO 300
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET ERROR COUNT IF PREVIOUS COMMAND PRODUCED ERRORS
20    IF (NERRS+NWARN.GT.0) CALL FRESTR
      IOPNUM=-1
      CALL UMEMOV ('FCINIT  ',OPNAME,2)
      CALL UMEMOV ('**NONE**',ISEG,2)
C
C  SET METRIC FLAG
      METRIC=-1
C
C  READ CARD
30    READ (IN,320,END=290) XCARD
C
C  PRINT CARD
      NBLINE=1
      IFORM=1
      NXCARD=0
      CALL UPRCR2 (NBLINE,IFORM,IPR,XCARD,NXCARD)
C
C  GET FIRST FIELD
      NSCAN=1
      CALL USCAN2 (XCARD(1:LXCARD),' =',NSCAN,XCHAR,LXCHAR,IERR)
C
C  CHECK IF BLANK CARD OR COMMENT
      CALL UCKCMT (XCHAR(1:1),'$',IRETRN)
      IF (IRETRN.NE.0) GO TO 280
C
C  CHECK FOR '@' BEFORE COMMAND
      IF (XCHAR(1:1).EQ.'@') XCHAR=XCHAR(2:LENSTR(XCHAR))
C
C  CHECK FOR COMMAND
      DO 50 IXCOMD=1,MCOMND
         IF (LDEBUG.GT.0) WRITE (IPR,*) 'MCOMND=',MCOMND,
     *      ' IXCOMD=',IXCOMD,
     *      ' XCHAR=',XCHAR,
     *      ' XCOMND(IXCOMD)=',XCOMND(IXCOMD)
         IF (XCHAR.EQ.XCOMND(IXCOMD)) GO TO 60
50       CONTINUE
      GO TO 70
60    IGOTO=IXCOMD
C
      GO TO (80,90,90,100,110,120,130,140,150,160,
     *       170,180,190,200,210,220,230,240,300,250,
     *       260,270,275),IGOTO
C
70    WRITE (IPR,340) XCARD
      CALL ERROR
      GO TO 30
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATUS
C
80    IPRALL=0
      CALL FSTATS (IPRALL)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEBUG OR SETBUG
C
90    CALL FSETBG
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SEGDEF
C
100   NEWSEG=1
      GO TO 115
C
C  RESEGDEF
C
110   NEWSEG=0
C
C  CHECK NEXT FIELD FOR 'NOPRINT' OPTION
C
115   NOPRT=0
      NSCAN=NSCAN+1
      CALL USCAN2 (XCARD(1:LXCARD),' ',NSCAN,XCHAR,LXCHAR,IERR)
      IF (XCHAR.EQ.'NOPRINT') NOPRT=1
C
      IFLSTA=1
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL FSGDEF (NEWSEG,MC,C,MOLDC,OLDC,MRSTC,RESETC,MP,P,MOLDP,OLDP,
     *   MT,T,MOLDT,OLDT,MTS,TS,MOLDTS,OLDTS,IENDIN)
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  FGDEF
C
120   ISPECL=0
      IFLSTA=2
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL FGDEF (ISPECL)
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SPECIALFG
C
130   ISPECL=1
      IFLSTA=2
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL FGDEF (ISPECL)
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CGDEF
C
140   IFLSTA=3
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL CGDEF
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE
C
150   CALL FCDELT
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINTSEGS
C
160   CALL FPPSEG(1)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCHSEGS
C
170   CALL FPPSEG (0)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SEGDATE
C
180   IFLSTA=4
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL FSGDAT
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DEF-RC
C
190   IFLSTA=5
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL FDEFRC
      IFLSTA=0
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINTRC
C
200   CALL FPRTRC
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINTOPS
C
C  SET METRIC DISPLAY DEFAULT
C
210   METRIC=IDEFLT(7)
      CALL FPRTOP
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCHRC
C
220   CALL FPUNRC
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCHFG
C
230   CALL FPUNFG
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCHCG
C
240   CALL FPUNCG
      CALL UCLOSL
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NEWUSER
C
C  SET NEW USER NAME
250   NSCAN=NSCAN+1
      CALL USCAN2 (XCARD(1:LXCARD),' ',NSCAN,XCHAR,LXCHAR,IERR)
      IF (XCHAR.EQ.' ') GO TO 280
      USERN=XCHAR
C
      CALL USRNEW (USERN,PATHN,IERR)
C
C  GET USER PARAMETERS
      CALL HGTUSR (PUSRID,IERR)
C
C  PRINT PAGE HEADER
      CALL UPAGE (IPR)
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DSATTR COMMAND
C
260   NUNIT1=1
      NUNIT2=99
      DDNAME=' '
      IPRHDR=1
      IROUND=0
      CALL UDDTBL (NUNIT1,NUNIT2,DDNAME,IPRHDR,IROUND,LBUFRS,IPR,IERR)
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCHCHR COMMAND
C
270   NSCAN=NSCAN+1
      CALL USCAN2 (XCARD(1:LXCARD),'=',NSCAN,XCHAR,LXCHAR,IERR)
      WRITE (IPU,'(A)') XCHAR
      WRITE (IPR,430) IPU,XCHAR
      GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INCLUDE COMMAND
C
275   GO TO 280
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
280   IF (IENDIN.EQ.1) GO TO 290
      GO TO 20
C
290   WRITE (IPR,350)
C
300   CALL UCLOSL
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
310   FORMAT ('0')
320   FORMAT (A80)
330   FORMAT ('0**ERROR** READING ',A,' ',
     *   'DATA BASE CONTROL RECORDS.')
340   FORMAT ('0**ERROR** THE FOLLOWING INPUT RECORD ',
     *      'DOES NOT CONTAIN A VALID COMMAND:' /
     *   ' ',A)
350   FORMAT ('0**NOTE** STOP COMMAND ASSUMED.')
360   FORMAT ('+',7('***  FATAL ERROR  '),'***')
370   FORMAT ('0*** FATAL ERROR - LAST RUN OF ',A,' DID NOT ',
     *   'COMPLETE SUCCESSFULLY.')
380   FORMAT ('0',T20,'THE COMMAND BEING EXECUTED WAS ',A,'.')
390   FORMAT ('0',T20,'PLEASE CHECK OUTPUT FOR THE JOB THAT ',
     *   'TERMINATED ABNORMALLY.')
400   FORMAT ('0',T20,'THE DATA FILES PROBABLY WILL HAVE TO BE ',
     *   'RESTORED.')
410   FORMAT ('0',T20,'NO COMMANDS WILL BE PROCESSED DURING THE ',
     *   'CURRENT EXECUTION OF PROGRAM ',A,'.')
420   FORMAT ('0',T20,'FUTURE EXECUTION OF PROGRAM ',A,' WILL NOT ',
     *   'BE PREVENTED.')
430   FORMAT ('0**NOTE** THE FOLLOWING CHARACTER STRING WAS WRITTEN ',
     *   'TO UNIT ',I2.2,':' /
     *   ' ',A)
C
      END
