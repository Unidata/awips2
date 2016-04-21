C MEMBER SBDBUG
C  (from old member SBDBUG)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO SET DEBUG CODES
C DESC: PPINIT COMMAND :  @DEBUG
C
      SUBROUTINE SBDBUG (NFLD,ISTRT,ISTAT)
C
C
      CHARACTER*8 OPTNS(12)
C
      CHARACTER*8 CODES(64),XCODES(64)
      CHARACTER*8 ROUTNE
      CHARACTER*20 CHK,CHAR,CODE
C
      DIMENSION ICODES(64)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sbdbug.f,v $
     . $',                                                             '
     .$Id: sbdbug.f,v 1.1 1995/09/17 19:20:21 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NOPTNS/12/
      DATA OPTNS/'ON      ','OFF     ','TRACELVL','CODE    ',
     *           'UNIT    ','        ','PRINT   ','DEBUGLVL',
     *           'LISTCODE','ROUTINE ','        ','        '/
      DATA MCODES/64/
      DATA CODES/'ALL     ','SYS     ','UTIL    ','DERW    ',
     *           'PPDRW   ','PPPRW   ','PRDRW   ','FCRW    ',
     *           '        ','ORD1    ','ORD2    ','NETW    ',
     *           'STAT    ','DEFN    ','DELT    ','DEBG    ',
     *           'MPCO    ','MPFO    ','FMPO    ','ORDR    ',
     *           'BASN    ','SOPT    ','WGHT    ','SNSP    ',
     *           'PDMP    ','DESC    ','GRID    ','STA     ',
     *           'MAP     ','MAPE    ','MAPS    ','MAT     ',
     *           'FMAP    ','MPCO    ','NTWK    ','STAN    ',
     *           'PCPN    ','PE      ','RRS     ','STBN    ',
     *           'TEMP    ','URRS    ','UGNL    ','PNCH    ',
     *           'MDR     ','DUMP    ','TIMR    ','GBOX    ',
     *           'RFRO    ','MARO    ','GMDR    ','OG24    ',
     *           'GP24    ','HCLRW   ','CHNG    ','INIT    ',
     *           'MAPX    ','MXCO    ','XGRD    ','COMNSRCE',
     *           '        ','        ','        ','        '/
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,620)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEBG')
C
      ISTAT=0
C
      LCHK=LEN(CHK)/4
      LCHAR=LEN(CHAR)/4
      LCODE=LEN(CODE)/5
      LRPAR=0
      ILPFND=0
      IRPFND=0
      ICODE=0
      LTRC=-1
      LDBG=-1
      IPRINT=1
      ILSTCD=0
      IDBGON=1
      IUNIT=0
      NUMERR=0
      NUMWRN=0
      IEROPT=0
      NEROPT=0
      IERCDS=0
      NUMCDS=0
      NUMOPT=0
      NCODES=0
      DO 10 I=1,MCODES
         ICODES(I)=0
         IF (CODES(I).EQ.' ') GO TO 10
            NCODES=NCODES+1
            XCODES(NCODES)=CODES(I)
10       CONTINUE
      ROUTNE=' '
C
C  PRINT CARD
      IF (NFLD.GT.0) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DEBUG OPTIONS
C
20    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,CHAR,
     *   LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 480
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,640) CHAR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C     CHECK FOR NULL FIELD
         IF (IERR.NE.1) GO TO 30
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,630) NFLD
               CALL SULINE (IOSDBG,1)
               ENDIF
            GO TO 20
C
C     CHECK IF CODES OPTION SPECIFIED
30       IF (ICODE.EQ.1) GO TO 220
C
C     CHECK FOR PAIRED PARENTHESES
         IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 40
            GO TO 50
40          WRITE (LP,720) NFLD
            CALL SULINE (LP,2)
            LRPAR=0
C
C     CHECK FOR COMMAND
50       IF (LATSGN.EQ.0) GO TO 60
            IF (CHAR.NE.'@RUN') ISTRT=-1
            GO TO 480
C
C     CHECK FOR LEFT PARENTHESES
60       IF (LLPAR.GT.0) ILPFND=1
         IF (LLPAR.LE.1) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
         IF (LLPAR.GT.1) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
         DO 70 IOPTN=1,NOPTNS
            CALL SUCOMP (2,CHK,OPTNS(IOPTN),IERR)
            IF (IERR.EQ.1) GO TO 90
70          CONTINUE
C
         NEROPT=NEROPT+1
         IF (IEROPT.EQ.1) GO TO 20
         IF (NEROPT.LE.10) GO TO 80
            IEROPT=1
            WRITE (LP,660)
            CALL SULINE (LP,2)
            GO TO 20
80       WRITE (LP,650) CHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 20
C
90       ICODE=0
         ILPFND=0
         IRPFND=0
         IF (IOPTN.EQ.4) NUMCDS=0
         NUMOPT=NUMOPT+1
         IF (NFLD.EQ.1) CALL SUPCRD
         GO TO (110,120,180,220,330,100,400,360,130,440,100,100),IOPTN
100         WRITE (LP,710) NFLD
            CALL SULINE (LP,2)
            GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     DEBUG ON
C
110      IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,730)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IDBGON=1
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     DEBUG OFF
C
120      IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,740)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IDBGON=0
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     OPTION TO LIST DEBUG CODES
C
130      IF (LLPAR.GT.0) GO TO 140
            WRITE (LP,820) OPTNS(IOPTN),'YES'
            CALL SUWRNS (LP,2,NUMWRN)
            CHK='YES'
            GO TO 160
140      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 150
            WRITE (LP,720) NFLD
            CALL SULINE (LP,2)
            LRPAR=LENGTH+1
150      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (CHK.EQ.'YES'.OR.CHK.EQ.'NO') GO TO 160
            WRITE (LP,700) CHK
            CALL SUERRS (LP,2,NUMERR)
            GO TO 20
160      IF (CHK.EQ.'YES') ILSTCD=1
         IF (CHK.EQ.'NO') ILSTCD=0
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,790) OPTNS(IOPTN),CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ILSTCD.EQ.0) GO TO 20
            IF (ISLEFT(5).GT.0) CALL SUPAGE
            WRITE (LP,870)
            CALL SULINE (LP,2)
            ISPTR=0
            CALL SUSORT (2,NCODES,XCODES,XCODES,ISPTR,IERR)
            NPER=5
            NTIME=NCODES/NPER
            IF (MOD(NCODES,NPER).NE.0) NTIME=NTIME+1
            NUM1=1
            NUM2=NCODES
            DO 170 I=1,NTIME
               WRITE (LP,880) I,(XCODES(N),N=NUM1,NUM2,NTIME)
               CALL SULINE (LP,1)
               NUM1=NUM1+1
170            CONTINUE
               IERCDS=IERCDS+1
               GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     SET TRACE LEVEL
C
180      IF (LLPAR.GT.0) GO TO 190
            LTRC=0
            WRITE (LP,830) LTRC
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 20
190      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 200
            WRITE (LP,720) NFLD
            CALL SULINE (LP,1)
            LRPAR=0
            LRPAR=LENGTH
200      CALL UFINFX (LTRC,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (LTRC.GE.0) GO TO 210
            WRITE (LP,770) LTRC
            CALL SUWRNS (LP,2,NUMWRN)
            LTRC=-1
210      IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,840) LTRC
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     SET DEBUG CODES
C
220      ICODE=1
         IF (NUMCDS.GT.0.OR.LLPAR.GT.0) GO TO 230
            ICODE=0
            WRITE (LP,850)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 20
230      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) ICODE=0
         IF (LLPAR.EQ.0) IBEG=1
         IF (LLPAR.GT.0) IBEG=LLPAR+1
         IF (LRPAR.EQ.0) IEND=LENGTH
         IF (LRPAR.GT.0) IEND=LRPAR-1
C
C     GET DEBUG CODE
         CALL UFPACK (LCODE,CODE,ISTRT,IBEG,IEND,IERR)
C
C     GET LENGTH OF DEBUG CODE
         CALL ULENTH (CODE,LCODE*4,LENTH)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,800) LENTH,CODE
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (LENTH.LE.8) GO TO 240
            WRITE (LP,670) LENTH
            CALL SUERRS (LP,2,NUMERR)
            GO TO 20
C
C     CHECK IF 'NONE' SPECIFIED
240      IF (CODE.NE.'NONE') GO TO 260
            DO 250 I=1,NSDBUG
               SDBUG(I)=' '
               SDBUG2(I)=' '
250            CONTINUE
            NSDBUG=0
            IDBGON=0
            WRITE (LP,860)
            CALL SULINE (LP,2)
            GO TO 20
C
C     CHECK FOR DEBUG OPTION
260      DO 270 IOPTN=1,NOPTNS
            CALL SUCOMP (2,CODE,OPTNS(IOPTN),IERR)
            IF (IERR.EQ.1) GO TO 90
270         CONTINUE
C
C     CHECK FOR DEBUG CODE - CHECK ONLY FIRST 4 CHARACTERS
         NCMPR=1
         DO 280 ICDE=1,MCODES
            CALL SUCOMP (NCMPR,CODE,CODES(ICDE),IERR)
            IF (IERR.EQ.1) GO TO 300
280         CONTINUE
            WRITE (LP,680) CODE
            CALL SUERRS (LP,2,NUMERR)
            IF (IERCDS.GT.0) GO TO 20
               WRITE (LP,870)
               CALL SULINE (LP,1)
               ISPTR=0
               CALL SUSORT (2,NCODES,XCODES,XCODES,ISPTR,IERR)
               NPER=5
               NTIME=NCODES/NPER
               IF (MOD(NCODES,NPER).NE.0) NTIME=NTIME+1
               NUM1=1
               NUM2=NCODES
               DO 290 I=1,NTIME
                  WRITE (LP,880) I,(XCODES(N),N=NUM1,NUM2,NTIME)
                  CALL SULINE (LP,1)
                  NUM1=NUM1+1
290            CONTINUE
            IERCDS=IERCDS+1
            GO TO 20
C
C      VALID CODE FOUND
300      ICODES(ICDE)=1
         IF ((ICDE.GE.2.AND.ICDE.LE.3).OR.
     *      (ICDE.GE.8.AND.ICDE.LE.MCODES)) GO TO 310
            GO TO 320
310         IF (LENTH.LE.4)
     *         CALL SBSDBG (CODE,ROUTNE,IDBGON,LTRC,LDBG,IERR)
            IF (LENTH.GT.4)
     *         CALL SBSDB2 (CODE,ROUTNE,IDBGON,LTRC,LDBG,IERR)
320      IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,810) CODE
            CALL SULINE (IOSDBG,1)
            ENDIF
         LDEBUG=ISBUG('DEBG')
         NUMCDS=NUMCDS+1
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     SET DEBUG OUTPUT UNIT NUMBER
C
330      IF (LLPAR.GT.0) GO TO 340
            WRITE (LP,890) LP
            CALL SUWRNS (LP,2,NUMWRN)
            IUNIT=LP
            GO TO 20
340      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 350
            WRITE (LP,720) NFLD
            CALL SULINE (LP,1)
            LRPAR=LENGTH
350      CALL UFINFX (IUNIT,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,750) IUNIT
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     SET DEBUG LEVEL
C
360      IF (LLPAR.GT.0) GO TO 370
            WRITE (LP,900) LDBG
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 20
370      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 380
            WRITE (LP,720) NFLD
            CALL SULINE (LP,1)
            LRPAR=LENGTH
380      CALL UFINFX (LDBG,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (LDBG.GE.0) GO TO 390
            WRITE (LP,760) LDBG
            CALL SUWRNS (LP,2,NUMWRN)
            LDBG=-1
390      IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,780) LDBG
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
C
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     OPTION TO PRINT DEBUG OPTIONS IN EFFECT
C
400      IF (LLPAR.GT.0) GO TO 410
            WRITE (LP,910) 'YES'
            CALL SUWRNS (LP,2,NUMWRN)
            CHK='YES'
            GO TO 160
410      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 420
            WRITE (LP,720) NFLD
            CALL SULINE (LP,1)
            LRPAR=LENGTH
420      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (CHK.EQ.'YES'.OR.CHK.EQ.'NO') GO TO 430
            WRITE (LP,690) CHK
            CALL SUERRS (LP,2,NUMERR)
            GO TO 20
430      IF (CHK.EQ.'YES') IPRINT=1
         IF (CHK.EQ.'NO') IPRINT=0
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,790) OPTNS(IOPTN),CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
C
C    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     OPTION TO SET ROUTINE NAME AT WHICH DEBUG TO BE ENABLED
C
440      IF (LLPAR.GT.0) GO TO 450
            ROUTNE=' '
            WRITE (LP,920) OPTNS(IOPTN)
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 20
450      IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 460
            WRITE (LP,720) NFLD
            CALL SULINE (LP,2)
            LRPAR=LENGTH+1
460      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
         CALL ULENTH (CHK,LCHK*4,LENTH)
         IF (LENTH.LE.8) GO TO 470
            WRITE (LP,930) LENTH,NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 20
470      CALL SUBSTR (CHK,1,8,ROUTNE,1)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,940) ROUTNE
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
480   IF (LDEBUG.EQ.0) GO TO 490
         WRITE (IOSDBG,950) NUMOPT,IDBGON,NSDBUG
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,960)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,970) ICODES
         NPER=20
         NLINES=(MCODES+NPER)/NPER
         CALL SULINE (IOSDBG,NLINES)
C
C  SET DEBUG OPTIONS
C
C  CHECK IF ONLY 'DEBUG OFF' SPECIFIED
490   IF (NUMOPT.EQ.1.AND.IDBGON.EQ.0) GO TO 500
      GO TO 530
C
500   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,980)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET ALL DEBUG OPTIONS TO OFF
      DO 510 I=2,MCODES
         ICODES(I)=0
510      CONTINUE
      DO 520 I=1,NSDBUG
         SDBUG(I)=' '
         SDBUG2(I)=' '
520      CONTINUE
      NSDBUG=0
      ISTRCE=0
      ISDBGL=0
      ISDBUG=0
      ISALL=0
      ICMTRC=0
      ICMDBG=0
      NOBUG=0
      IHCLTR=0
      IHCLDB=0
      IUTLTR=0
      IUTLDB=0
      IDEDB=0
      IDETR=0
      IPPDB=0
      IPPTR=0
      IPDDB=0
      IPDTR=0
      IPRDB=0
      IPRTR=0
      IALL=0
      IDBALL=0
      ITRACE=0
      GO TO 600
C
C  CHECK IF DEBUG OPTION SET TO OFF
530   IF (IDBGON.EQ.0) GO TO 540
C
C  CHECK IF ALL OPTION SPECIFIED
      IF (ICODES(1).EQ.0) GO TO 540
      ICMTRC=LTRC
      ICMDBG=LDBG
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,990)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET ALL DEBUG CODES ON
      IF (LTRC.LT.0) LTRC=1
      IF (LDBG.LT.0) LDBG=1
      ISALL=1
      NOBUG=1
      IHCLTR=LTRC
      IHCLDB=LDBG
      IUTLTR=LTRC
      IUTLDB=LDBG
      NOBUG=LDBG
      IDETR=LTRC
      IDEDB=LDBG
      IPPDB=LDBG
      IPPTR=LTRC
      IPDDB=LDBG
      IPDTR=LTRC
      IPRDB=LDBG
      IPRTR=LTRC
      IALL=LDBG
      IDBALL=LDBG
      ISTRCE=LTRC
      ISDBGL=LDBG
      ISDBUG=LDBG
      ITRACE=LTRC
      GO TO 600
C
C  SET SPECIFIC DEBUG OPTIONS
C
C  CHECK IF DEBUG OPTION SPECIFIED
540   IF (IDBGON.EQ.0) LDBG=0
      IF (LDBG.GT.0) GO TO 550
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1000)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C     DEBUG OFF SPECIFIED
         IF (ICODES(1).EQ.1) ISALL=0
         IF (ICODES(1).EQ.1) NOBUG=0
         IF (ICODES(3).EQ.1) THEN
            IUTLDB=0
            IUTLTR=0
            ENDIF
         IF (ICODES(4).EQ.1) THEN
            IDEDB=0
            IDETR=0
            ENDIF
         IF (ICODES(5).EQ.1) THEN
            IPDDB=0
            IPDTR=0
            ENDIF
         IF (ICODES(6).EQ.1) THEN
            IPPDB=0
            IPPTR=0
            ENDIF
         IF (ICODES(7).EQ.1) THEN
            IPRDB=0
            IPRTR=0
            ENDIF
         IF (ICODES(8).EQ.1) IALL=0
         IF (ICODES(8).EQ.1) IDBALL=0
         IF (ICODES(60).EQ.1) THEN
            ICMTRC=0
            ICMDBG=0
            ENDIF
         ISDBGL=0
         ISDBUG=0
         GO TO 560
C
550   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1010)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C     DEBUG ON SPECIFIED
         IF (ICODES(1).EQ.1) ISALL=1
         IF (ICODES(1).EQ.1.AND.LDBG.GE.0) NOBUG=LDBG
         IF (ICODES(1).EQ.1.AND.LDBG.LT.0) NOBUG=1
         IF (ICODES(3).EQ.1.AND.LDBG.GE.0) IUTLDB=LDBG
         IF (ICODES(3).EQ.1.AND.LDBG.LT.0) IUTLDB=1
         IF (ICODES(4).EQ.1.AND.LDBG.GE.0) IDEDB=LDBG
         IF (ICODES(4).EQ.1.AND.LDBG.LT.0) IDEDB=1
         IF (ICODES(5).EQ.1.AND.LDBG.GE.0) IPDDB=LDBG
         IF (ICODES(5).EQ.1.AND.LDBG.LT.0) IPDDB=1
         IF (ICODES(6).EQ.1.AND.LDBG.GE.0) IPPDB=LDBG
         IF (ICODES(6).EQ.1.AND.LDBG.LT.0) IPPDB=1
         IF (ICODES(7).EQ.1.AND.LDBG.GE.0) IPRDB=LDBG
         IF (ICODES(7).EQ.1.AND.LDBG.LT.0) IPRDB=1
         IF (ICODES(8).EQ.1) IALL=1
         IF (ICODES(8).EQ.1) IDBALL=1
         IF (ICODES(60).EQ.1.AND.LDBG.GE.0) ICMDBG=LDBG
         IF (ICODES(60).EQ.1.AND.LDBG.LT.0) ICMDBG=1
         ISDBGL=LDBG
         ISDBUG=LDBG
C
C  CHECK IF TRACE OPTION SPECIFIED
560   IF (IDBGON.EQ.0) LTRC=0
      IF (LTRC.GT.0) GO TO 570
C
C     TRACE OFF
         IF (ICODES(3).EQ.1) IUTLTR=0
         IF (ICODES(4).EQ.1) IDETR=0
         IF (ICODES(5).EQ.1) IPDTR=0
         IF (ICODES(6).EQ.1) IPPTR=0
         IF (ICODES(7).EQ.1) IPRTR=0
         IF (ICODES(8).EQ.1) ITRACE=0
         ISTRCE=0
         GO TO 580
C
C     TRACE ON
570      IF (ICODES(1).EQ.1) NOBUG=LTRC
         IF (ICODES(3).EQ.1) IUTLTR=LTRC
         IF (ICODES(4).EQ.1) IDETR=LTRC
         IF (ICODES(5).EQ.1) IPDTR=LTRC
         IF (ICODES(6).EQ.1) IPPTR=LTRC
         IF (ICODES(7).EQ.1) IPRTR=LTRC
         IF (ICODES(8).EQ.1) ITRACE=LTRC
         IF (ICODES(60).EQ.1) ICMTRC=LTRC
         ISTRCE=LTRC
C
C  CHECK IF UNIT OPTION SPECIFIED
580   IF (IUNIT.EQ.0) GO TO 600
         IF (ICODES(1).EQ.1) LPD=IUNIT
         IF (ICODES(2).EQ.1) IOSDBG=IUNIT
         IF (ICODES(3).EQ.1) IOSDBG=IUNIT
         IF (ICODES(4).EQ.1) IOGDB=IUNIT
         IF (ICODES(5).EQ.1) IOGDB=IUNIT
         IF (ICODES(6).EQ.1) IOGDB=IUNIT
         IF (ICODES(7).EQ.1) IOGDB=IUNIT
         IF (ICODES(8).EQ.1) IODBUG=IUNIT
         IF (ICODES(9).EQ.1) LPD=IUNIT
         IF (ICODES(60).EQ.1) ICMPRU=IUNIT
         DO 590 I=10,MCODES
            IF (ICODES(I).EQ.1) IOSDBG=IUNIT
590         CONTINUE
C
600   IF (IPRINT.EQ.0) GO TO 610
C
C  PRINT DEBUG OPTIONS IN EFFECT
      CALL SBPDBG
C
610   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,1020)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
620   FORMAT (' *** ENTER SBDBUG')
630   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
640   FORMAT (' INPUT FIELD = ',A)
650   FORMAT ('0*** ERROR - INVALID DEBUG OPTION : ',A)
660   FORMAT ('0*** NOTE - MORE THAN 15 INVALID OPTIONS FOUND. NO ',
     *   'MESSAGES WILL BE PRINTED.')
670   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF CHARACTERS IN DEBUG ',
     *   'CODE (8) EXCEEDED. LENGTH OF CODE IS ',I2,' CHARACTERS.')
680   FORMAT ('0*** ERROR - INVALID DEBUG CODE : ',A)
690   FORMAT ('0*** ERROR - INVALID PRINT OPTION : ',A)
700   FORMAT ('0*** ERROR - INVALID LISTCODE OPTION : ',A)
710   FORMAT ('0*** ERROR - ERROR PROCESSING FIELD ',I2)
720   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,'.')
730   FORMAT (' DEBUG OPTION SET TO ON')
740   FORMAT (' DEBUG OPTION SET TO OFF')
750   FORMAT (' DEBUG OUTPUT UNIT SET TO ',I3)
760   FORMAT ('0*** WARNING - ',I4,' IS AN INVALID DEBUG LEVEL. ',
     *   'DEFAULT VALUE WILL BE USED.')
770   FORMAT ('0*** WARNING - ',I4,' IS AN INVALID TRACE LEVEL. ',
     *   'DEFAULT VALUE WILL BE USED.')
780   FORMAT (' DEBUG LEVEL UNIT SET TO ',I3)
790   FORMAT (' ',A,' OPTION SET TO ',A)
800   FORMAT (' LENTH=',I2,3X,'CODE=',A)
810   FORMAT (' DEBUG CODE SET TO ',A)
820   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. ',A,
     *   ' OPTION SET TO ',A,'.')
830   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. TRACE LEVEL ',
     *   'SET TO ',I2,'.')
840   FORMAT (' TRACE OPTION SET TO ',I2)
850   FORMAT ('0*** ERROR - NO LEFT PARENTHESIS FOUND FOR DEBUG CODES.')
860   FORMAT ('0*** NOTE - ALL DEBUG CODES SET TO OFF.')
870   FORMAT ('0',T14,'- VALID DEBUG CODES ARE -')
880   FORMAT (5X,'(',I2,')',3X,10(A,2X))
890   FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. OUTPUT UNIT ',
     *   'SET TO ',I2,'.')
900   FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. DEBUG LEVEL ',
     *   'SET TO ',I2,'.')
910   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. PRINT ',
     *   'OPTION SET TO ',A,'.')
920   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. ',A,
     *   ' OPTION RESET.')
930   FORMAT ('0*** ERROR - LENGTH OF ROUTINE NAME (',I2,
     *   ') FOUND IN FIELD (',I2,') EXCEEDS 8 CHARACTERS.')
940   FORMAT (' ROUTINE NAME SET TO : ',A)
950   FORMAT (' NUMOPT=',I2,3X,'IDBGON=',I2,3X,'NSDBUG=',I2)
960   FORMAT (' ICODES:')
970   FORMAT (20(1X,I2))
980   FORMAT (' SET ALL DEBUG OPTIONS OFF')
990   FORMAT (' SET ALL DEBUG OPTIONS ON')
1000  FORMAT (' SET SPECIFIED DEBUG OPTIONS OFF')
1010  FORMAT (' SET SPECIFIED DEBUG OPTIONS ON')
1020  FORMAT (' *** EXIT SBDBUG')
C
      END
