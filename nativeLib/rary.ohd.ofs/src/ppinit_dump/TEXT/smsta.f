C MODULE SMSTA
C-----------------------------------------------------------------------
C
C  ROUTINE TO OUTPUT STATION PARAMETERS.
C
      SUBROUTINE SMSTA (LARRAY,ARRAY,OTYPE,XUNITS,SUMARY,SORT,PREST,
     *   LEVEL,DEGMIN,NSPACE,NFLD,ISTAT)
C
      CHARACTER*(*) OTYPE,XUNITS,SUMARY,SORT,PREST,DEGMIN
      CHARACTER*2 PTIME,TTIME
      CHARACTER*4 UNITS,RDISP,XPCMPL,XPFORM,XPURRS
      CHARACTER*4 SEP1,SEP2,XNBRSTA,XNUGPA
      PARAMETER (MOPTN=16)
      CHARACTER*8 OPTN(MOPTN)
     *         /'ALL     ','ALLPARM ','STAN    ','PCPN    ',
     *          'TEMP    ','PE      ','RRS     ','STATE   ',
     *          'RRSPUNCH','INCOMPL ','IDRANGE ','PCPNFORM',
     *          'NEWPAGE ','        ','        ','        '/
      CHARACTER*20 CHAR/' '/,CHK/' '/
      INTEGER*2 I2BLNK/2H  /
      INTEGER*2 NSTATE,ISTATE(1)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION ISRTBY(1),IPNTRS(1)
      CHARACTER*8 SRNGE1,SRNGE2
      PARAMETER (MAXINC=500)
      CHARACTER*8 STAINC(MAXINC)
      DIMENSION MINDEG(4)
      LOGICAL CFTYPE,PFTYPE
C
      INCLUDE 'scommon/dimstan'
      CHARACTER*4 GROUPS(MGPS),SOURCS(MSRCCD)
      INCLUDE 'scommon/dimpcpn'
      INCLUDE 'scommon/dimtemp'
      INCLUDE 'scommon/dimpe'
      INCLUDE 'scommon/dimrrs'
C
      DIMENSION UNUSED(10)
C
      EQUIVALENCE (ISTATE(1),STATNW(1))
      EQUIVALENCE (IPNTRS(1),PP24NW(1))
      EQUIVALENCE (ISRTBY(1),STIDNW(1,1))
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/surrsx'
      INCLUDE 'scommon/sntwkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smsta.f,v $
     . $',                                                             '
     .$Id: smsta.f,v 1.9 2000/12/18 23:00:47 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('DUMP')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SMSTA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
C      
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      LSTAID=LEN(STAID)
      ILPFND=0
      IRPFND=0
      NUMERR=0
      NUMWRN=0
      IPASS=0
      IOPTN=0
      LOPTN=0
      IALLST=1
      IALLPM=0
      IDLIST=0
      IDRNGE=0
      LCOLN1=0
      LCOLN2=0
      NSTATE=I2BLNK
      INDST=0
      IPURRS=0
      INCMPL=0
      IPFORM=0
      IPSTA=1
      LPUNCH=NPUCRD
      CFTYPE=.FALSE.
      IDEFLT=0
      IENDIN=0
      NUMINC=0
      IMXSTA=0
      IFILL=0
      IPROPT=1
C
      NSTAN=0
      NCOMPL=0
      NPCPN=0
      NTEMP=0
      NPE=0
      NRRS=0
      IFPRNT=0
C
C  SET INDICATOR TO REREAD CURRENT FIELD
      ISTRT=-1
C
C  SET SORT OPTION
      ISORT=1
      IF (SORT.EQ.'ID') ISORT=1
      IF (SORT.EQ.'DESC') ISORT=2
      IF (SORT.EQ.'NUM') ISORT=3
C
C  SET OPTIONS
      IPREST=0
      IF (PREST.EQ.'YES') IPREST=1
      IENGL=1
      IF (XUNITS.EQ.'METR') IENGL=0
      UNITS=XUNITS
      IF (DEGMIN.EQ.'YES') CALL SUBSTR ('DM',1,2,UNITS,3)
      IF (DEGMIN.EQ.'NO') CALL SUBSTR ('DD',1,2,UNITS,3)
C
C  PRINT HEADER LINE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,1050)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DUMP STATION OPTIONS
C
10    PFTYPE=CFTYPE
C
20    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 90
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,1130) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
         ENDIF
C
C  CHECK FOR DEBUG
      CALL SUIDCK ('CMDS',CHAR,NFLD,0,ICMD,IERR)
      IF (ICMD.EQ.1) THEN
         CALL SBDBUG (NFLD,ISTRT,IERR)
         LDEBUG=ISBUG('DUMP')
         GO TO 20
         ENDIF
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
C
      IF (IENDIN.EQ.1.AND..NOT.PFTYPE) GO TO 1030
      CFTYPE=.FALSE.
C
C  CHECK FOR OPTION
      DO 70 IOPTN=1,MOPTN
         IF (CHK.EQ.OPTN(IOPTN)) THEN
            IF ((IOPTN.GE.8.AND.IOPTN.LE.11).AND.
     *         IPASS.GT.0.AND.IPSTA.EQ.1) GO TO 50
               GO TO 60
50          CALL SMSTA2 (MOPTN,OPTN,LOPTN,NSTAN,NPCPN,NTEMP,NPE,NRRS,
     *         NUMINC,IMXSTA,STAINC,NCOMPL,INCMPL,IPSTA)
60          IF (IOPTN.EQ.8) GO TO 170
            IF (IOPTN.EQ.9) GO TO 220
            IF (IOPTN.EQ.10) GO TO 260
            IF (IOPTN.EQ.11) GO TO 300
            IF (IOPTN.EQ.12) GO TO 380
            IF (IOPTN.EQ.13) GO TO 420
            IF (IOPTN.GE.3.AND.IOPTN.LE.8) CFTYPE=.TRUE.
            GO TO 100
            ENDIF
70       CONTINUE
C
C  CHECK FOR GROUP
      CALL SUIDCK ('DUMP',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.NE.0) GO TO 90
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.0) GO TO 430
C
90    IENDIN=1
      IF (IALLST.EQ.1.AND.IPASS.GT.0) GO TO 1020
      IPSTA=1
      IF (IDLIST.EQ.1) GO TO 1020
      GO TO 130
C
C  VALID OPTION FOUND
100   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1120) PFTYPE,CFTYPE,IOPTN,IALLST
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IDLIST.EQ.0) GO TO 110
         CALL SMSTA2 (MOPTN,OPTN,LOPTN,NSTAN,NPCPN,NTEMP,NPE,NRRS,
     *      NUMINC,IMXSTA,STAINC,NCOMPL,INCMPL,IPSTA)
110   IF (PFTYPE.AND.CFTYPE) GO TO 120
         GO TO 140
120   IF (IALLST.EQ.1) GO TO 1030
130      IALLST=1
         IDLIST=0
         IDEFLT=1
         GO TO 450
140   IF (IOPTN.EQ.1) THEN
         IALLST=1
         IDLIST=0
         IDEFLT=0
         IF (NFLD.EQ.1) CALL SUPCRD
         GO TO 450
         ENDIF
      IF (IOPTN.GE.1.AND.IOPTN.LE.7) LOPTN=IOPTN
      IPSTA=1
      IALLPM=0
      IF (LOPTN.EQ.2) THEN
         IALLPM=1
         LOPTN=0
         ENDIF
      IF (LOPTN.GE.3.AND.LOPTN.LE.MOPTN) IALLST=0
      IF (NFLD.EQ.1) CALL SUPCRD
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATE OPTION
C
170   IF (NFLD.EQ.1) CALL SUPCRD
      IF (LLPAR.EQ.0) THEN
         NSTATE=I2BLNK
         WRITE (LP,1170) OPTN(IOPTN)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 210
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (CHK.EQ.'ALL') THEN
         NSTATE=I2BLNK
         IF (IPFORM.EQ.1) GO TO 210
            WRITE (LP,1180)
            CALL SULINE (LP,2)
            GO TO 210
         ENDIF
      CALL SUBSTR (CHK,1,2,NSTATE,1)
      WRITE (LP,1190) NSTATE
      CALL SULINE (LP,2)
210   IPSTA=1
      INDST=1
      GO TO 450
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PUNCH RRS MIN DAYS AND TYPICAL OBS OPTION
C
220   IF (NFLD.EQ.1) CALL SUPCRD
      IF (LLPAR.EQ.0) THEN
         CHK='NO'
         WRITE (LP,1200) OPTN(IOPTN),CHK(1:LENSTR(CHK))
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 250
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (CHK.EQ.'YES'.OR.CHK.EQ.'NO') GO TO 250
         WRITE (LP,1210) OPTN(IOPTN),CHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
250   IF (CHK.EQ.'YES') IPURRS=1
      IF (CHK.EQ.'NO') IPURRS=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1220) OPTN(IOPTN),'IPURRS',IPURRS
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPROPT=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS INCOMPLETE STATIONS OPTION
C
260   IF (NFLD.EQ.1) CALL SUPCRD
      IF (LLPAR.EQ.0) THEN
         CHK='YES'
         WRITE (LP,1200) OPTN(IOPTN),CHK(1:LENSTR(CHK))
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 290
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (CHK.EQ.'YES'.OR.CHK.EQ.'NO'.OR.CHK.EQ.'ONLY')
     *   GO TO 290
         WRITE (LP,1210) OPTN(IOPTN),CHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
290   IF (CHK.EQ.'YES') INCMPL=1
      IF (CHK.EQ.'NO') INCMPL=0
      IF (CHK.EQ.'ONLY') INCMPL=-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1220) OPTN(IOPTN),'INCMPL',INCMPL
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPSTA=1
      IPROPT=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RANGE OF STATION IDENTIFIERS TO BE PROCESSED OPTION
C
300   IF (NFLD.EQ.1) CALL SUPCRD
      IDRNGE=0
      LDASH=0
      LCOLN1=0
      LCOLN2=0
      IF (LLPAR.EQ.0) THEN
         WRITE (LP,1250) OPTN(IOPTN)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 370
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      CALL ULENTH (CHK,LEN(CHK),LENCHK)
C
C  CHECK IF RANGE SPECIFIED
      CALL UINDEX (CHK,LEN(CHK),'-',1,LDASH)
      IF (LDASH.GT.0) THEN
         SRNGE1=' '
         NCHAR=LDASH-1
         IF (NCHAR.GT.LSTAID) THEN
            WRITE (LP,1260) NCHAR
            CALL SUWRNS (LP,2,NUMWRN)
            WRITE (LP,1270) LSTAID
            CALL SULINE (LP,1)
            NCHAR=LSTAID
            ENDIF
         SRNGE1=CHK(1:NCHAR)
         SRNGE2=' '
         NCHAR=LENCHK-LDASH
         IF (NCHAR.GT.LSTAID) THEN
            WRITE (LP,1280) NCHAR
            CALL SUWRNS (LP,2,NUMWRN)
            WRITE (LP,1270) LSTAID
            CALL SULINE (LP,1)
            NCHAR=LSTAID
            ENDIF
         SRNGE2=CHK(LDASH+1:LENCHK)
         WRITE (LP,1290) SRNGE1(1:LENSTR(SRNGE1)),
     *      SRNGE2(1:LENSTR(SRNGE2))
         CALL SULINE (LP,2)
         CALL UINDEX (SRNGE1,LEN(SRNGE1),':',1,ICOL)
         IF (ICOL.GT.0) THEN
            LCOLN1=ICOL
            SRNGE1(LCOLN1:LCOLN1)=' '
            ENDIF
         CALL UINDEX (SRNGE2,LEN(SRNGE2),':',1,ICOL)
         IF (ICOL.GT.0) THEN
            LCOLN2=ICOL
            SRNGE2(LCOLN2:LCOLN2)=' '
            ENDIF
         GO TO 360
         ENDIF
      CALL UINDEX (CHK,LEN(CHK),':',1,ICOL)
      IF (ICOL.GT.0) THEN
         SRNGE1=' '
         SRNGE2=' '
         IF (ICOL.EQ.1) THEN
            SRNGE1=CHK
            ELSE
               SRNGE1=CHK(1:ICOL)
            ENDIF
         WRITE (LP,1300) SRNGE1(1:LENSTR(SRNGE1))
         CALL SULINE (LP,2)
         LCOLN1=ICOL
         SRNGE1(LCOLN1:LCOLN1)=' '
         ENDIF
360   CALL ULEFTC (SRNGE1,LEN(SRNGE1),LENGTH)
      CALL ULEFTC (SRNGE2,LEN(SRNGE2),LENGTH)
      IALLST=1
      IDLIST=0
      IPSTA=1
      IDRNGE=1
370   IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,*) 'IDRNGE=',IDRNGE,
     *      ' LDASH=',LDASH,
     *      ' LCOLN1=',LCOLN1,
     *      ' LCOLN2=',LCOLN2
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  OPTION TO PRINT PCPN STATION FORM
C
380   IF (NFLD.EQ.1) CALL SUPCRD
      IF (LLPAR.EQ.0) THEN
         CHK='YES'
         WRITE (LP,1200) OPTN(IOPTN),CHK(1:LENSTR(CHK))
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 410
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,1110) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (CHK.EQ.'YES'.OR.CHK.EQ.'NO') GO TO 410
         WRITE (LP,1210) OPTN(IOPTN),CHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
410   IF (CHK.EQ.'YES') IPFORM=1
      IF (CHK.EQ.'NO') IPFORM=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1220) OPTN(IOPTN),'IPFORM',IPFORM
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPROPT=1
      INCMPL=2
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NEWPAGE OPTION
C
420   NEWPAG=0
      CALL SUNEWP (NFLD,ISTRT,CHK,LCHK,LLPAR,LRPAR,LENGTH,IRPFND,
     *   OPTN(IOPTN),NUMERR,NUMWRN,NEWPAG,IERR)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STATION IDENTIFIER
C
430   CFTYPE=.FALSE.
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK LENGTH OF IDENTIFIER
      IF (LENGTH.GT.LSTAID) THEN
         WRITE (LP,1310) LENGTH,CHAR(1:LENSTR(CHAR)),LSTAID,LSTAID
         CALL SULINE (LP,2)
         ENDIF
      STAID=CHAR
      IALLST=0
      IDLIST=1
      IPSTA=0
      GO TO 450
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
450   IF (IPROPT.EQ.0) GO TO 460
C
C  PRINT OPTIONS IN EFFECT
      XPCMPL='NO'
      IF (INCMPL.EQ.1) XPCMPL='YES'
      IF (INCMPL.EQ.-1) XPCMPL='ONLY'
      XPFORM='NO'
      IF (IPFORM.EQ.1) XPFORM='YES'
      XPURRS='NO'
      IF (IPURRS.EQ.1) XPURRS='YES'
      WRITE (LP,1060) XPCMPL,XPFORM,XPURRS
      CALL SULINE (LP,2)
      WRITE (LP,1070)
      CALL SULINE (LP,1)
      IPROPT=0
C
460   IISTAN=0
      IPTRNS=0
      IF (IFILL.EQ.0) NUMID=0
      IPOS=0
C
      IF (IALLST.EQ.0) GO TO 510
C
C  CHECK IF SORT OPTION SPECIFIED
      IF (ISORT.NE.0) THEN
         IF (IFILL.EQ.0) THEN
            ISTATE(1)=1
            IPNTRS(1)=1
            IF (ISORT.EQ.1) NWORDS=2
            IF (ISORT.EQ.2) NWORDS=5
            IF (ISORT.EQ.3) NWORDS=3
            IPRMSG=1
            IPRERR=0
            CALL SURIDS ('STAN',ISORT,MAXSNW,ISTATE,NWORDS,ISRTBY,
     *         IPNTRS,NUMID,LARRAY,ARRAY,IPRMSG,IPRERR,IERR)
            IF (IERR.GT.0) THEN
               IF (IERR.EQ.2) THEN
                  IF (NUMID.EQ.0) THEN
                     WRITE (LP,1160)
                     CALL SULINE (LP,2)
                     IPSTA=0
                     IPASS=1
                     GO TO 1020
                     ENDIF
                  GO TO 470
                  ENDIF
               WRITE (LP,1150)
               CALL SUWRNS (LP,2,NUMWRN)
               ISORT=0
               ENDIF
470         INWFIL=0
            IFILL=1
            ENDIF
         ENDIF
C
      IPOS=1
C
C  CHECK IF STATE OPTION SPECIFIED
480   IF (NSTATE.EQ.I2BLNK) GO TO 500
C
C  CHECK IF STATION IS IN SPECIFIED STATE
490   IF (ISTATE(IPOS).NE.NSTATE) THEN
         IF (IPOS.GT.NUMID) GO TO 910
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,1350) IPFORM,IPOS,IPNTRS(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IPFORM.EQ.1.AND.IPNTRS(IPOS).GT.0)
     *         IPNTRS(IPOS)=-IPNTRS(IPOS)
            IPOS=IPOS+1
            GO TO 490
         ENDIF
C
C  SET STATION IDENTIFIER TO BLANK
500   STAID=' '
C
C  SET POINTER TO STATION GENERAL PARAMETERS
      IF (ISORT.EQ.0) IISTAN=IPTRNS
      IF (ISORT.GT.0) IISTAN=IABS(IPNTRS(IPOS))
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATION GENERAL PARAMETERS
C
510   RDISP='OLD'
      IPTR=IISTAN
      IPRERR=0
      INCLUDE 'scommon/callsrstan'
      IPTRNS=IPTRNX
      IF (IERR.GT.0) THEN
         IF (IALLST.EQ.0) GO TO 520
            IF (IERR.EQ.6) GO TO 1020
            IF (NUMID.GT.0) GO TO 1020
               WRITE (LP,1160)
               CALL SULINE (LP,2)
               IPSTA=0
               IPASS=1
               GO TO 1020
520      IF (IERR.EQ.2) GO TO 530
            CALL SRPPST (STAID,'STAN',IPTR,LARRAY,0,IPTRNX,IERR)
            WRITE (LP,1320) 'STAN',IERR
            CALL SUERRS (LP,2,NUMERR)
            GO TO 1020
530      WRITE (LP,1330) STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 1020
         ENDIF
C
C  CHECK IF RANGE OF STATIONS SPECIFIED
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,*) 'IDRNGE=',IDRNGE
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IDRNGE.EQ.1) THEN
         IF (LDEBUG.GT.1) THEN
            WRITE (IOSDBG,*) 'LCOLN1=',LCOLN1,
     *         ' LCOLN2=',LCOLN2
            CALL SULINE (IOSDBG,1)
            ENDIF
         CALL SURGCK (SRNGE1,SRNGE2,LCOLN1,LCOLN2,STAID,LSTAID,
     *      IMATCH,ISTAT)
         IF (IMATCH.EQ.0) GO TO 900
         ENDIF
C
C  CHECK IF PARAMETER ARRAY IS COMPLETE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'ICSTAN=',ICSTAN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (ICSTAN.EQ.1) THEN
         IF (SUMARY.EQ.'NO'.AND.IALLST.EQ.0.AND.INCMPL.NE.-1) THEN
            WRITE (LP,1340) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,1350) IPFORM,IPOS,IPNTRS(IPOS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IPFORM.EQ.1.AND.IPNTRS(IPOS).GT.0)
     *      IPNTRS(IPOS)=-IPNTRS(IPOS)
         IF (NUMINC.EQ.0) GO TO 550
            DO 540 I=1,NUMINC
               IF (STAINC(I).EQ.STAID) GO TO 560
540            CONTINUE
550      NUMINC=NUMINC+1
         IF (NUMINC.GT.MAXINC) THEN
            IMXSTA=IMXSTA+1
            IF (IMXSTA.EQ.1) THEN
               WRITE (LP,1420) MAXINC
               CALL SUERRS (LP,2,NUMERR)
               GO TO 900
               ENDIF
            ENDIF
         CALL SUBSTR (STAID,1,LSTAID,STAINC(NUMINC),1)
         IF (INCMPL.NE.0) GO TO 560
            NSTAN=NSTAN+1
            GO TO 900
         ENDIF
C
C  CHECK IF ONLY INCOMPLETE STATIONS TO BE PROCESSED
560   IF (INCMPL.EQ.-1.AND.ICSTAN.EQ.0) GO TO 900
C
      NSTAN=NSTAN+1
C
C  SET INDICATORS FOR PARAMETER TYPES DEFINED
      IPCPN=0
      ITEMP=0
      IPE=0
      IRRS=0
      DO 570 I=1,NGPS
         IF (GPS(I).EQ.'PCPN') IPCPN=1
         IF (GPS(I).EQ.'TEMP') ITEMP=1
         IF (GPS(I).EQ.'PE') IPE=1
         IF (GPS(I).EQ.'RRS') IRRS=1
570      CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1100) NSTAN,STAID,STATE,
     *      IPCPN,ITEMP,IPE,IRRS,IPTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF PCPNFORM OPTION SPECIFIED
      IF (IPFORM.EQ.0) GO TO 580
         IF (IPCPN.EQ.0.AND.IPNTRS(IPOS).GT.0)
     *      IPNTRS(IPOS)=-IPNTRS(IPOS)
         GO TO 900
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
580   IF ((OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH').AND.
     *   (IALLPM.EQ.1.OR.(IALLST.EQ.1.AND.LOPTN.EQ.0)).AND.
     *   SUMARY.EQ.'NO') THEN
C     PRINT SEPARATOR LINE
         IF (NCOMPL.EQ.0) THEN
            WRITE (LP,1070)
            CALL SULINE (LP,1)
            ENDIF
         WRITE (LP,1090)
         CALL SULINE (LP,1)
         ENDIF
C
C  CHECK IF DATA TYPE SPECIFIED
      IF (LOPTN.GE.4.AND.LOPTN.LE.7) THEN
C     CHECK IF STATION HAS DATA TYPE
         DO 585 I=1,NGPS
            IF (GPS(I).EQ.OPTN(LOPTN)) GO TO 587
585         CONTINUE
         GO TO 900
         ENDIF
C
587   IF (SUMARY.EQ.'NO') GO TO 740
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT SUMMARY
C
C  CHECK IF STATION HAS ANY GROUPS DEFINED
      IF (NGPS.EQ.0) THEN
         WRITE (IOSDBG,1400) STAID
         CALL SULINE (IOSDBG,1)
         GO TO 760
         ENDIF
C
      DO 630 I=1,4
         GROUPS(I)=' '
630      CONTINUE
      IPTIME=0
      ITTIME=0
      DO 660 I=1,NGPS
         IF (GPS(I).EQ.'PCPN') THEN
            GROUPS(1)='PCPN'
            IPTIME=24
            IF (ITPPVR.GT.0) IPTIME=ITPPVR
            GO TO 660
            ENDIF
         IF (GPS(I).EQ.'TEMP') THEN
            GROUPS(2)='TEMP'
            ITTIME=24
            IF (ITTAVR.GT.0) ITTIME=ITTAVR
            GO TO 660
            ENDIF
         IF (GPS(I).EQ.'PE') GROUPS(3)='PE'
         IF (GPS(I).EQ.'RRS') GROUPS(4)='RRS'
660      CONTINUE
C
C  CHECK IF SOURCE CODES DEFINED
      DO 670 I=1,MSRCCD
         SOURCS(I)=' '
670      CONTINUE
      IF (NSRCCD.GT.0) THEN
         DO 680 I=1,NSRCCD
            SOURCS(I)=SRCCD(I)
680         CONTINUE
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1410) STAID,(GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  FORMAT DATA TYPES AND TIME INTERVALS
      VALUE=STAELV
      IF (IENGL.EQ.1)
     *   CALL UDUCNV ('M   ','FT  ' ,1,1,STAELV,VALUE,IERR)
      SEP1=' '
      PTIME=' '
      IF (IPTIME.GT.0) THEN
         SEP1='-'
         CALL UINTCH (IPTIME,2,PTIME,NFILL,IERR)
         IF (PTIME(1:1).EQ.' ') PTIME(1:1)='0'
         ENDIF
      SEP2=' '
      TTIME=' '
      IF (ITTIME.GT.0) THEN
         SEP2='-'
         CALL UINTCH (ITTIME,2,TTIME,NFILL,IERR)
         IF (TTIME(1:1).EQ.' ') TTIME(1:1)='0'
         ENDIF
C
      IF (NCOMPL.EQ.0.AND.ISLEFT(10).GT.0) CALL SUPAGE
      IF (NCOMPL.EQ.0.OR.ISNWPG(LP).EQ.1) THEN
         WRITE (LP,1430)
         CALL SULINE (LP,3)
         ENDIF
C
      IF (NSPACE.EQ.1) THEN
         WRITE (LP,1070)
         CALL SULINE (LP,1)
         ENDIF
C
C  CONVERT STATION NUMBER TO CHARACTER
      XNBRSTA=' '
      IF (NBRSTA.GT.0) CALL UINTCH (NBRSTA,4,XNBRSTA,NFILL,IERR)
C
C  CONVERT GRID POINT ADDRESS TO CHARACTER
      XNUGPA=' '
      IF (NUGPA.GT.0) CALL UINTCH (NUGPA,4,XNUGPA,NFILL,IERR)
C
      NCOMPL=NCOMPL+1
C
C  CHECK OF LAT/LON TO BE PRINTED AS DECIMAL DEGREES OR DEGREES AND
C  MINUTES
      IF (DEGMIN.EQ.'YES') GO TO 720
         WRITE (LP,1440)
     *      NCOMPL,STAID,DESCRP,STATE,XNBRSTA,STALOC,VALUE,GROUPS(1),
     *      SEP1,PTIME,GROUPS(2),SEP2,TTIME,GROUPS(3),GROUPS(4),SOURCS,
     *      XNUGPA
         GO TO 730
720   CALL SUDMDD ('DM  ',2,STALOC,MINDEG,IERR)
      WRITE (LP,1450)
     *   NCOMPL,STAID,DESCRP,STATE,XNBRSTA,MINDEG,VALUE,GROUPS(1),
     *   SEP1,PTIME,GROUPS(2),SEP2,TTIME,GROUPS(3),GROUPS(4),SOURCS,
     *   XNUGPA
730   CALL SULINE (LP,1)
C
C  CHECK IF STATION HAS GRID POINT ADDRESS BUT NO PCPN DATA TYPE
      IF (NUGPA.GT.0.AND.IPCPN.EQ.0) THEN
         WRITE (LP,1460) STAID
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
      IF (IPCPN.EQ.1) NPCPN=NPCPN+1
      IF (ITEMP.EQ.1) NTEMP=NTEMP+1
      IF (IPE.EQ.1) NPE=NPE+1
      IF (IRRS.EQ.1) NRRS=NRRS+1
      GO TO 750
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT PARAMETERS

740   NCOMPL=NCOMPL+1
C
      IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         IPRNT=ISORT
         INCLUDE 'scommon/callspstan'
         ENDIF
C
750   IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
         INCLUDE 'scommon/callscstan'
         ENDIF
C
760   IFPRNT=1
C
      IF (SUMARY.EQ.'YES') GO TO 900
C
C  CHECK IF STATION IS COMPLETE
      IF (ICSTAN.EQ.0) GO TO 770
C
C  CHECK IF INCOMPLETE STATIONS TO BE PROCESSED
      IF (INCMPL.EQ.-1.OR.INCMPL.EQ.1) GO TO 770
         WRITE (LP,1070)
         CALL SULINE (LP,1)
         GO TO 900
C
770   IPRNT=ISORT
      IPNCH=1
      IF (IALLPM.EQ.1.OR.(IALLST.EQ.1.AND.LOPTN.EQ.0)) THEN
         IPRNT=0
         IPNCH=0
         GO TO 780
         ENDIF
      IF (LOPTN.EQ.4) GO TO 780
      GO TO 800
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATION PRECIPITATION PARAMETERS
C
780   IF (IPCPN.EQ.0) THEN
         IF (IALLPM.EQ.1.OR.IALLST.EQ.1) GO TO 800
            WRITE (LP,1360) STAID,'PCPN'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 800
         ENDIF
C
      IPTR=0
      IPRERR=0
      IREAD=1
      INCLUDE 'scommon/callsrpcpn'
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,1370) STAID,'PCPN'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 800
            ENDIF
         CALL SRPPST (STAID,'PCPN',IPTR,LARRAY,0,IPTRNX,IERR)
         WRITE (LP,1320) 'PCPN',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 800
         ENDIF
C
C  CHECK DATA TIME INTERVAL
      IF (IPTIME.LT.24.AND.IPTIME.NE.ITPPVR) THEN
         WRITE (LP,1380) 'PCPN',IPTIME,'STAN',ITPPVR,STAID
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
      IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         INCLUDE 'scommon/callsppcpn'
         ENDIF
C
      IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
         CALL SCPCPN (IPNCH,XUNITS,IVPCPN,STAID,IPPROC,IPTIME,MDRBOX,
     *      PCPNCF,IPTWGT,IPSWGT,IPCHAR,STASID,STASWT,IERR)
          ENDIF
C
      NPCPN=NPCPN+1
      IFPRNT=1
C
800   IF (IALLPM.EQ.1.OR.(IALLST.EQ.1.AND.LOPTN.EQ.0)) GO TO 810
         IF (LOPTN.EQ.5) GO TO 810
         GO TO 830
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATION TEMPERATURE PARAMETERS
C
810   IF (ITEMP.EQ.0) THEN
         IF (IALLPM.EQ.1.OR.IALLST.EQ.1) GO TO 830
            WRITE (LP,1360) STAID,'TEMP'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 830
         ENDIF
C
      NTTAVR=ITTAVR
C
      IPTR=0
      IPRERR=0
      IREAD=1
      INCLUDE 'scommon/callsrtemp'
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,1370) STAID,'TEMP'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 830
            ENDIF
         CALL SRPPST (STAID,'TEMP',IPTR,LARRAY,0,IPTRNX,IERR)
         WRITE (LP,1320) 'TEMP',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 830
         ENDIF
C
C  CHECK DATA TIME INTERVAL
      IF (NTTAVR.LT.24.AND.NTTAVR.NE.ITTAVR) THEN
         WRITE (LP,1380) 'STAN',NTTAVR,'TEMP',ITTAVR,STAID
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
      IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         INCLUDE 'scommon/callsptemp'
         ENDIF
C
      IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
         CALL SCTEMP (IPNCH,XUNITS,IVTEMP,STAID,ITYOBS,TEMPCF,ITFMM,
     *      TEMPFE,IPMMMT,ITTAVR,IERR)
          ENDIF
C
      NTEMP=NTEMP+1
      IFPRNT=1
C
830   IF (IALLPM.EQ.1.OR.(IALLST.EQ.1.AND.LOPTN.EQ.0)) GO TO 840
         IF (LOPTN.EQ.6) GO TO 840
         GO TO 860
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATION POTENTIAL EVAPORATION PARAMETERS
C
840   IF (IPE.EQ.0) THEN
         IF (IALLPM.EQ.1.OR.IALLST.EQ.1) GO TO 860
            WRITE (LP,1360) STAID,'PE'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 860
         ENDIF
C
      IPTR=0
      IPRERR=0
      INCLUDE 'scommon/callsrpe'
C
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,1370) STAID,'PE'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 860
            ENDIF
         CALL SRPPST (STAID,'PE',IPTR,LARRAY,0,IPTRNX,IERR)
         WRITE (LP,1320) 'PE',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 860
         ENDIF
C
      IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         INCLUDE 'scommon/callsppe'
         ENDIF
C
      IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
         CALL SCPE (IPNCH,XUNITS,IVPE,STAID,ANEMHT,ITYRAD,PECOR,PEB3,
     *      IERR)
          ENDIF
C
      NPE=NPE+1
      IFPRNT=1
C
860   IF (IALLPM.EQ.1.OR.(IALLST.EQ.1.AND.LOPTN.EQ.0)) GO TO 870
         IF (LOPTN.EQ.7) GO TO 870
         GO TO 890
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATION RIVER, RESERVOIR AND SNOW PARAMETERS
C
870   IF (IRRS.EQ.0) THEN
         IF (IALLPM.EQ.1.OR.IALLST.EQ.1) GO TO 890
            WRITE (LP,1360) STAID,'RRS'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 890
         ENDIF
C
      IPTR=0
      IPRERR=0      
      IREAD=1
      INCLUDE 'scommon/callsrrrs'
C
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,1370) STAID,'RRS'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 890
            ENDIF
         CALL SRPPST (STAID,'RRS',IPTR,LARRAY,0,IPTRNX,IERR)
         WRITE (LP,1320) 'RRS',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 890
         ENDIF       
C
      IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         INCLUDE 'scommon/callsprrs'
         ENDIF
C
      IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
C     CHECK IF NEED TO READ URRS PARAMETERS
         IF (IURRSX.EQ.0) THEN
            IPRERR=0
            CALL SUGTUR (LARRAY,ARRAY,IPRERR,IERR)
            ENDIF
         CALL SCRRS (IPNCH,IPURRS,IVRRS,STAID,
     *      NRRSTP,NDIST,RRSTYP,URMISS,IRTIME,MNODAY,NUMOBS,
     *      INTERP,EXTRAP,FLOMIN,FRACT,IERR)
         ENDIF
C
      NRRS=NRRS+1
      IFPRNT=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
890   IF (OTYPE.EQ.'PRINT'.OR.OTYPE.EQ.'BOTH') THEN
         IF (IFPRNT.EQ.1.AND.IALLPM.EQ.1.OR.
     *      (IALLST.EQ.1.AND.LOPTN.EQ.0)) THEN
            WRITE (LP,1070)
            CALL SULINE (LP,1)
            ENDIF
         ENDIF
C
900   IPASS=1
C
      IPOS=IPOS+1
C
C  CHECK IF ALL STATIONS OR ALL OF PARAMETER TYPE PROCESSED
      IF (IALLST.EQ.0) GO TO 910
      IF (ISORT.GT.0.AND.IPOS-1.EQ.NUMID) GO TO 910
      IF (ISORT.EQ.0.AND.IPTRNS.EQ.0) GO TO 910
C
C  PROCESS NEXT STATION
      IFPRNT=0
      GO TO 480
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF PCPNFORM OPTION SPECIFIED
910   IF (IPFORM.EQ.0) GO TO 1000
C
C  CHECK IF SORT OPTION SPECIFIED
      IF (ISORT.EQ.0) THEN
         WRITE (LP,1470) OPTN(12)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 1000
         ENDIF
C
C  CHECK NUMBER OF STATIONS
      IF (NSTAN.EQ.0) THEN
         WRITE (LP,1590) NSTATE
         CALL SULINE (LP,2)
         GO TO 1000
         ENDIF
C
C  CHECK NUMBER OF STATIONS WITH PCPN PARAMETERS
      NCOUNT=0
      IPOS1=0
      DO 920 I=1,NUMID
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'I=',I,
     *         ' IPNTRS(I)=',IPNTRS(I)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IPNTRS(I).GT.0) NCOUNT=NCOUNT+1
         IF (NCOUNT.EQ.1.AND.IPOS1.EQ.0) IPOS1=I
920      CONTINUE
      IF (NCOUNT.EQ.0) THEN
         IF (NSTATE.EQ.I2BLNK) THEN
            WRITE (LP,1480)
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         IF (NSTATE.NE.I2BLNK) THEN
            WRITE (LP,1490) NSTATE
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         GO TO 1000
         ENDIF
C
C  SET LOCATION FOR SECOND HALF OF STATIONS
      NPER=2
      IPOS2=0
      IF (NCOUNT.GT.1) THEN
         IPOS2=IPOS1-1
         NPOS2=0
         DO 930 I=IPOS1,NUMID
            IPOS2=IPOS2+1
            IF (IPNTRS(I).LE.0) GO TO 930
               NPOS2=NPOS2+1
               IF (NPOS2.GT.NCOUNT/NPER) GO TO 940
930         CONTINUE
940      IF (MOD(NCOUNT,NPER).NE.0) IPOS2=IPOS2+1
         ENDIF
C
C  SET NUMBER OF TIMES TO LOOP
      NTIME=NCOUNT/NPER
      IF (MOD(NCOUNT,NPER).NE.0) NTIME=NTIME+1
      IPOS1=IPOS1-1
      IF (IPOS2.GT.0) IPOS2=IPOS2-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1500) NUMID,NCOUNT,NPER,IPOS1,IPOS2,NTIME
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT FORM
      RDISP='OLD'
      NCOUNT=0
      IF (ISLEFT(15).GT.0) CALL SUPAGE
      WRITE (LP,1080)
      CALL SULINE (LP,2)
      WRITE (LP,1510)
      CALL SULINE (LP,2)
      WRITE (LP,1080)
      CALL SULINE (LP,2)
      WRITE (LP,1520)
      CALL SULINE (LP,3)
      DO 980 I=1,NTIME
950      IPOS1=IPOS1+1
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'IPOS1=',IPOS1,
     *         ' IPNTRS(IPOS1)=',IPNTRS(IPOS1)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IPNTRS(IPOS1).LE.0) GO TO 950
            STAID=' '
            IPTR=IPNTRS(IPOS1)
            IPRERR=1
            INCLUDE 'scommon/callsrstan'
            IF (IERR.GT.0) GO TO 960
            NCOUNT=NCOUNT+1
            IF (DEGMIN.EQ.'YES')
     *         CALL SUDMDD ('DM  ',2,STALOC,MINDEG,IERR)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,1520)
               CALL SULINE (LP,3)
               ENDIF
            IF (DEGMIN.EQ.'NO')
     *         WRITE (LP,1530) STAID,DESCRP,STATE,NBRSTA,STALOC
            IF (DEGMIN.EQ.'YES')
     *         WRITE (LP,1540) STAID,DESCRP,STATE,NBRSTA,MINDEG
960      IF (IPOS2.EQ.0) GO TO 970
         IPOS2=IPOS2+1
         IF (IPOS2.GT.NUMID) GO TO 970
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'IPOS2=',IPOS2,
     *         ' IPNTRS(IPOS2)=',IPNTRS(IPOS2)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IPNTRS(IPOS2).LE.0) GO TO 960
            STAID=' '
            IPTR=IPNTRS(IPOS2)
            IPRERR=0
            INCLUDE 'scommon/callsrstan'
            IF (IERR.GT.0) GO TO 970
            NCOUNT=NCOUNT+1
            IF (DEGMIN.EQ.'YES')
     *         CALL SUDMDD ('DM  ',2,STALOC,MINDEG,IERR)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,1520)
               CALL SULINE (LP,3)
               ENDIF
            IF (DEGMIN.EQ.'NO')
     *         WRITE (LP,1550) STAID,DESCRP,STATE,NBRSTA,STALOC
            IF (DEGMIN.EQ.'YES')
     *         WRITE (LP,1560) STAID,DESCRP,STATE,NBRSTA,MINDEG
970      CALL SULINE (LP,2)
980      CONTINUE
C
      WRITE (LP,1570) NCOUNT
      CALL SULINE (LP,2)
C
      DO 990 I=1,NUMID
         IF (IPNTRS(I).LT.0) IPNTRS(I)=-IPNTRS(I)
990      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
1000  IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1580) IPASS,IENDIN,IDLIST,IALLST,
     *      IDEFLT,NFLD,IPSTA,INDST
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF A STATE HAS BEEN SPECIFIED
      IF (INDST.EQ.1) THEN
         INDST=0
         IF (IPFORM.EQ.1) GO TO 1010
         IF (NSTAN.EQ.0) THEN
            WRITE (LP,1590) NSTATE
            CALL SULINE (LP,2)
            IPSTA=0
            IPASS=1
            ENDIF
         GO TO 1010
         ENDIF
C
      IF (IPASS.EQ.0) GO TO 1030
C
C  CHECK IF END OF INPUT ENCOUNTERED
1010  IF (IENDIN.EQ.1) GO TO 1020
C
      NSTATE=I2BLNK
C
C  CHECK IF STATION IDENTIFIER SPECIFIED
      IF (IDLIST.EQ.1) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF STATIONS PROCESSED
1020  IF (IPSTA.EQ.1) THEN
         CALL SMSTA2 (MOPTN,OPTN,LOPTN,NSTAN,NPCPN,NTEMP,
     *      NPE,NRRS,NUMINC,IMXSTA,STAINC,NCOMPL,INCMPL,IPSTA)
         ENDIF
C
C  CHECK IF 'ALL' DEFAULTED
      IF (IENDIN.EQ.0.AND.IDEFLT.EQ.1.AND.NFLD.EQ.1) CALL SUPCRD
      IF (IDEFLT.EQ.1) IALLST=0
      LOPTN=IOPTN
C
C  CHECK IF END OF INPUT FOR COMMAND
      IF (NFLD.EQ.-1.OR.IENDIN.EQ.1) GO TO 1030
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK NUMBER OF OPTIONS PROCESSED
1030  IF (IPASS.EQ.0) THEN
         WRITE (LP,1600)
         CALL SULINE (LP,2)
         ENDIF
C
      IF (OTYPE.EQ.'PUNCH'.OR.OTYPE.EQ.'BOTH') THEN
C     PRINT NUMBER OF CARD IMAGES OUTPUT
         NPUNCH=NPUCRD-LPUNCH
         IF (NPUNCH.GT.0) THEN
            WRITE (LP,1610) NPUNCH
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  CHECK NUMBER OF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) THEN
         WRITE (LP,1620) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SMSTA : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
1050  FORMAT ('0*--> DUMP STATION PARAMETERS')
1060  FORMAT ('0DUMP STATION OPTIONS IN EFFECT : ',
     *   'INCOMPL=',A,3X,
     *   'PCPNFORM=',A,3X,
     *   'RRSPUNCH=',A,3X)
1070  FORMAT (' ')
1080  FORMAT ('0')
1090  FORMAT (' ',132('#'))
1100  FORMAT (' NSTAN=',I4,3X,'STAID=',A,3X,'STATE=',A2,3X,
     *   'IPCPN=',I1,3X,'ITEMP=',I1,3X,'IPE=',I1,3X,'IRRS=',I1,3X,
     *   'IPTR=',I5)
1110  FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,'.')
1120  FORMAT (' PFTYPE=',L1,3X,'CFTYPE=',L1,3X,'IOPTN=',I2,3X,
     *   'IALLST=',I2)
1130  FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
1150  FORMAT ('0*** WARNING - SORTED LIST OF STATIONS NOT ',
     *   'SUCCESSFULLY OBTAINED. SORT OPTION CANCELED.')
1160  FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
1170  FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. ',A,
     *   ' OPTION SET TO ''ALL''.')
1180  FORMAT ('0*** NOTE - OPTION TO PRINT STATION PARAMETERS BY ',
     *   'STATE HAS BEEN TURNED OFF.')
1190  FORMAT ('0*** NOTE - STATIONS FOR STATE ',A2,' WILL BE ',
     *   'PROCESSED.')
1200  FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. ',A,
     *   ' OPTION SET TO ''',A,'''.')
1210  FORMAT ('0*** ERROR - INVALID ',A,' OPTION SPECIFIED : ',A)
1220  FORMAT (' ',A,' OPTION SET : ',A,'=',I2)
1250  FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. ',A,
     *   'WILL BE IGNORED.')
1260   FORMAT ('0*** WARNING - LENGTH OF FIRST IDENTIFIER IN ',
     *   'SPECIFIED STATION RANGE (',I2,') EXCEEDS MAXIMUM ALLOWED.')
1270  FORMAT (T16,'ONLY THE FIRST ',I2,' CHARACTERS WILL BE USED.')
1280   FORMAT ('0*** WARNING - LENGTH OF SECOND IDENTIFIER IN ',
     *   'SPECIFIED STATION RANGE (',I2,') EXCEEDS MAXIMUM ALLOWED.')
1290   FORMAT ('0*** NOTE - STATIONS IDENTIFIERS IN THE RANGE OF ',A,
     *   ' THROUGH ',A,' WILL BE PROCESSED.')
1300   FORMAT ('0*** NOTE - STATIONS IDENTIFIERS IN THE RANGE OF ',A,
     *   ' WILL BE PROCESSED.')
1310  FORMAT ('0*** WARNING - NUMBER OF CHARACTERS (',I2,') IN ',
     *   'STATION IDENTIFIER ',A,' EXCEEDS ',I2,'. ',
     *   'THE FIRST ',I2,' CHARACTERS WILL BE USED.')
1320  FORMAT ('0*** ERROR - IN SMSTA - UNSUCCESSFUL CALL TO SR',A,
     *   ' : STATUS CODE=',I2)
1330  FORMAT ('0*** ERROR - STATION ',A,' IS NOT DEFINED.')
1340  FORMAT ('0*** NOTE - STATUS FOR STATION ',A,' IS INCOMPLETE.')
1350  FORMAT (' IPFORM=',I2,3X,'IPOS=',I4,3X,'IPNTRS(IPOS)=',I5)
1360  FORMAT ('0*** WARNING - STATION ',A,' DOES NOT HAVE ',A,' ',
     *   'PARAMETERS DEFINED.')
1370  FORMAT ('0*** WARNING - STAN PARAMETERS FOR STATION ',A,' ',
     *   'INDICATE STATION HAS ',A,' PARAMETERS HOWEVER THE ',
     *   'PARAMETERS ARE NOT DEFINED.')
1380  FORMAT ('0*** WARNING - DATA TIME INTERVAL IN THE ',A,' ',
     *   'PARAMETERS (',I2,') ',
     *   'IS NOT THE SAME AS IN THE ',A,
     *   ' PARAMETERS (',I2,') FOR STATION ',A,'.')
1400  FORMAT ('0*** NOTE - STATION ',A,' DOES NOT HAVE PCPN, TEMP ',
     *   'PE OR RRS PARAMETERS DEFINED.')
1410  FORMAT (' STATION ',A,' HAS FOLLOWING DATA TYPES : ',5(A,2X))
1420  FORMAT ('0*** ERROR - MAXIMUM NUMBER OF INCOMPLETE STATIONS ',
     *   'THAT CAN BE PROCESSED ',I4,' EXCEEDED.')
1430  FORMAT ('0',
     *   T7,'STA  ID ',2X,
     *      'DESCRIPTION',9X,2X,
     *      'ST',2X,
     *      'NUM ',2X,
     *      'LAT  ',2X,
     *      'LON   ',2X,
     *      'ELEV   ',2X,
     *      'DATA GROUPS',17X,2X,
     *      'SOURCE CODES',14X,
     *      'GPA' /
     *   T7,8('-'),2X,
     *      20('-'),2X,
     *      2('-'),2X,
     *      4('-'),2X,
     *      5('-'),2X,
     *      6('-'),2X,
     *      7('-'),2X,
     *      28('-'),2X,
     *      24('-'),2X,
     *      4('-'))
1440  FORMAT (' ',I4,1X,
     *   A,2X,
     *   A,2X,
     *   A2,2X,
     *   A,2X,
     *   F5.2,2X,
     *   F6.2,2X,
     *   F7.1,2X,
     *   2(A,A1,A2,2X),2(A,2X),
     *   4(A,1X),A,2X,
     *   A)
1450  FORMAT (' ',I4,1X,
     *   A,2X,
     *   A,2X,
     *   A2,2X,
     *   A,2X,
     *   I2,'-',I2,2X,
     *   I3,'-',I2,2X,
     *   F7.2,2X,
     *   2(A,A1,A2,2X),2(A,2X),
     *   4(A,1X),A,2X,
     *   A)
1460  FORMAT ('0*** WARNING - STATION ',A,' ',
     *   'HAS A GRID POINT ADDRESS DEFINED ',
     *   'BUT DOES NOT HAVE PCPN DATA.')
1470  FORMAT ('0*** WARNING - SORT OPTION NOT SET. ',
     *   A,' OPTION IGNORED.')
1480  FORMAT ('0*** WARNING - NO STATIONS HAVE PCPN DATA DEFINED.')
1490  FORMAT ('0*** WARNING - NO STATIONS HAVE PCPN DATA DEFINED ',
     *   'FOR STATE ',A2,'.')
1500  FORMAT (' NUMID=',I5,3X,'NCOUNT=',I2,3X,'NPER=',I2,3X,
     *   'IPOS1=',I4,3X,'IPOS2=',I4,3X,'NTIME=',I4)
1510  FORMAT ('0DATE: ____/____/________')
1520  FORMAT ('0',
     *      'STA ID  ',1X,'DESCRIPTION',9X,1X,'ST',1X,
     *      'NUM ',1X,'PCPN AMOUNT',1X,1X,'LAT  ',1X,'LON   ',
     *      T70,
     *      'STA ID  ',1X,'DESCRIPTION',9X,1X,'ST',1X,
     *      'NUM ',1X,'PCPN AMOUNT',1X,1X,'LAT  ',1X,'LON   ',
     *   / ' ',
     *      8('-'),1X,20('-'),1X,2('-'),1X,
     *      4('-'),1X,12('-'),1X,5('-'),1X,6('-'),
     *      T70,
     *      8('-'),1X,20('-'),1X,2('-'),1X,
     *      4('-'),1X,12('-'),1X,5('-'),1X,6('-')
     *   )
1530  FORMAT ('0',A,1X,A,1X,A2,1X,I4,1X,12('_'),1X,
     *   F5.2,1X,F6.2)
1540  FORMAT ('0',A,1X,A,1X,A2,1X,I4,1X,12('_'),1X,
     *   I2,'-',I2,1X,I3,'-',I2)
1550  FORMAT ('+',T70,A,1X,A,1X,A2,1X,I4,1X,12('_'),1X,
     *   F5.2,1X,F6.2)
1560  FORMAT ('+',T70,A,1X,A,1X,A2,1X,I4,1X,12('_'),1X,
     *   I2,'-',I2,1X,I3,'-',I2)
1570  FORMAT ('0*** NOTE - ',I4,' STATIONS WITH PCPN DATA PROCESSED.')
1580  FORMAT (' IPASS=',I1,3X,'IENDIN=',I1,3X,'IDLIST=',I1,3X,
     *   'IALLST=',I1,3X,'IDEFLT=',I1,3X,'NFLD=',I2,3X,'IPSTA=',I1,3X,
     *   'INDST=',I1)
1590  FORMAT ('0*** NOTE - NO STATIONS FOUND FOR STATE ',A2,'.')
1600  FORMAT ('0*** NOTE - NO DUMP STATION OPTIONS SUCCESSFULLY ',
     *   'PROCESSED.')
1610  FORMAT ('0*** NOTE - ',I4,' CARD IMAGES OUTPUT.')
1620  FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED IN DUMP STATION ',
     *   'COMMAND.')
C
      END
