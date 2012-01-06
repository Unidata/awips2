C MODULE SFSTA
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE A STATION.
C
      SUBROUTINE SFSTA (LARRAY,ARRAY,DISP,PRPARM,PRNOTE,NFLD,
     *   IRUNCK,ICKREF,ISTAT)
C
      CHARACTER*4 DISP,PRPARM,PRNOTE
      CHARACTER*4 STSTAN,STPCPN,STTEMP,STPE,STRRS,PDDISP,WDISP,RDISP
      CHARACTER*4 TYPE,DTYPE,DUNITS,SORTBY,SORTBYX
      CHARACTER*8 TYPERR
      CHARACTER*20 STRNG/' '/,STRNG2/' '/
      CHARACTER*50 STRING
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION ISWRK2(1)
C
C  STAN PARAMETER ARRAYS
      INCLUDE 'scommon/dimstan'
      CHARACTER*4 GPSN
      PARAMETER (MGPSN=4)
      DIMENSION GPSN(MGPSN),IPARMN(MGPSN)
      DIMENSION INWSRC(MSRCCD)
      CHARACTER*4 SRCCDO,SRCIDO
      PARAMETER (MSRCCDO=5,MSRCIDO=2)
      DIMENSION SRCCDO(MSRCCDO),SRCIDO(MSRCIDO,MSRCCDO)
      CHARACTER*4 TSRCCD,SAID
      CHARACTER*8 GOESID,SMID
C
C  PCPN PARAMETER ARRAYS
      INCLUDE 'scommon/dimpcpn'
C
C  TEMP PARAMETER ARRAYS
      INCLUDE 'scommon/dimtemp'
C
C  PE PARAMETER ARRAYS
      INCLUDE 'scommon/dimpe'
C
C  RRS PARAMETER ARRAYS
      INCLUDE 'scommon/dimrrs'
      CHARACTER*4 RTYPE
      CHARACTER*4 RRSTYPO(MRRSTP),URMISSO(MRRSTP),RUNITS(MRRSTP)
      DIMENSION IRTIMEO(MRRSTP),IRSTAT(MRRSTP),ICVRRS(MRRSTP)
      DIMENSION IRTIMEA(MRRSTP)
C
C  PREPROCESSOR DATA BASE READ/WRITE ARRAYS
      PARAMETER (MDLYTP=20)
      CHARACTER*4 DLYTYP(MDLYTP)
      DIMENSION IPPPTR(3)
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
C  PROCESSED DATA BASE READ/WRITE ARRAYS
      PARAMETER (LWKBUF=2000)
      DIMENSION IWKBUF(LWKBUF)
C
C  TEMPORARY DESCRIPTION VARIABLE
      CHARACTER*20 descrpTmp
C
      INCLUDE 'uiox'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'scommon/sntwfx'
      INCLUDE 'scommon/sntwkx'
      INCLUDE 'scommon/stypsx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/suerrx'
C
      EQUIVALENCE (ISWRK2(1),SWRK2(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob6/rfc/ofs/src/ppinit_define/RCS/sfsta.f,v $
     . $',                                                             '
     .$Id: sfsta.f,v 1.9 2002/02/11 21:02:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('STA ')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFSTA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STA ')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' LARRAY=',LARRAY,
     *      ' DISP=',DISP,
     *      ' PRPARM=',PRPARM,
     *      ' PRNOTE=',PRNOTE,
     *      ' NFLD=',NFLD,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LSTRNG=LEN(STRNG)/4
      LSTRNG2=LEN(STRNG2)/4
C
C  SET ARRAY SIZE FOR STORING PARAMETERS
      NARRAY=5+1
      L1ARRAY=LARRAY/NARRAY
C
C  SET VALUES FOR UNDEFINED VARIABLES
      UNSD=-999.
C
      ILPFND=0
      IRPFND=0
C
      LRUNCK=IRUNCK
      ISYERR=0
C
      TYPE=' '
      NUMKEY=0
      IENDIN=0
C
      CALL UREPET ('?',STAID,8)
      NUMSTA=0
      NUIDF1=0
      NUIDFX=0
      NPRSTA=4
      NUMERR=0
      NUMWRN=0
      NERR=0
      NWARN=0
      NSTAN=0
      NDFNEW=0
      NDFOLD=0
      NSTERR=0
      NSTWRN=0
      NPP24=0
      NPPVR=0
      NTA24=0
      NTAIN=0
      NTF24=0
      NEA24=0
      NRRS=0
C
C  SET NUMBER OF WORDS IN PROCESSED DATA BASE XBUF ARRAY
      NXBUF=0
C
C  SET DEFAULT OPTIONS
      DUNITS='ENGL'
C
C  SET OPTION WHETHER ALL DATA GROUPS MUST BE READ FROM INPUT
      IREADA=0
C
C  SET OPTION HOW GENL PARAMETERS TO BE SET IF STATION STATUS IS
C  NOT COMPLETE
C     0 = SET GROUPS, SOURCE CODES, ETC. TO ZERO
C     1 = DO NOT SET GROUPS, SOURCE CODES, ETC. TO ZERO
      INDCMP=1
C
C  SET NUMBER OF BLANK LINES TO PRINT BEFORE MESSAGE
      NSPACE=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (DISP.EQ.'NEW') GO TO 10
C
C  CHECK IF DISP OF OLD CURRENTLY ALLOWED
      ISUPT=ISUPRT('OLDS')
      IF (ISUPT.EQ.0) THEN
         WRITE (LP,680)
         CALL SULINE (LP,1)
         WRITE (LP,680)
         CALL SULINE (LP,1)
         WRITE (LP,690)
         IF (IOPOVP.EQ.1) THEN
            WRITE (LP,690)
            WRITE (LP,690)
            ENDIF
         CALL SUERRS (LP,0,NUMERR)
         ISYERR=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF NETWORK FLAG COMMON BLOCK FILLED
C
10    IF (INFFIL.EQ.0) THEN
         RDISP='OLD'
         CALL SUGTNF (LARRAY,ARRAY,RDISP,NUMERR,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,750)
            CALL SULINE (LP,2)
            ISYERR=1
            ELSE
            IF (LDEBUG.GT.1) THEN
               CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
               ENDIF
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET INDICATOR TO REPROCESS FIELD
      ISTRT=-1
C
C  GET FIELD
20    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
C
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
C
C  CHECK IF END OF INPUT
      IF (NFLD.EQ.-1) GO TO 250
C
C  CHECK IF NULL FIELD
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,730) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 250
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,740) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DEFN',STRNG2,NFLD,0,IKEYWD,IRETRN)
      IF (IRETRN.EQ.0) GO TO 250
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,720) STRNG2
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK PARAMETER TYPE
      IF (STRNG2.EQ.'STAN') THEN
         IF (NSTAN.EQ.0) GO TO 80
         IF (NGPSN.GT.0.OR.INSTAN.GT.0) GO TO 260
            IF (STSTAN.EQ.'NEW'.AND.INSTAN.EQ.1) THEN
               WRITE (LP,850) STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            IF (STSTAN.EQ.'OLD'.AND.INSTAN.EQ.1) THEN
               WRITE (LP,860) STAID
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            IF (INDERR.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (LP,830) STAID
                  CALL SULINE (LP,2)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (LP,840) STAID
                  CALL SULINE (LP,2)
                  ENDIF
               IPOS=NUIDF1*NPRSTA
C           CHECK IF ERRORS ENCOUNTERED
               IF (NUMERR.EQ.0.AND.INDERR.EQ.0) THEN
                  ELSE
                     NSTERR=NSTERR+1
                  IF (IUIDF1.EQ.0) CALL SUBSTR ('E',1,1,ISWRK2(IPOS),-1)
                  ENDIF
C           CHECK IF WARNINGS ENCOUNTERED
               IF (NUMWRN.GT.0) THEN
                  NSTWRN=NSTWRN+1
                  IF (IUIDF1.EQ.1) CALL SUBSTR ('W',1,1,ISWRK2(IPOS),-3)
                  ENDIF
               ENDIF
            IF (IFSTAN.GT.0) GO TO 80
               IPTR=0
               IPRERR=0
               RDISP='OLD'
               INCLUDE 'scommon/callsrstan'
               IF (NUMERR.EQ.1) GO TO 40
                  IF (NGPSN.EQ.0) THEN
                     NGPSN=NGPS
                     DO 30 I=1,NGPSN
                        GPSN(I)=GPS(I)
                        IPARMN(I)=IPARM(I)
30                      CONTINUE
                     GO TO 50
                  ENDIF
40             IF (STSTAN.EQ.'OLD'.AND.ICSTAN.EQ.0) GO TO 80
                  WDISP='OLD'
                  NGPSN=0
                  INCLUDE 'scommon/callswstan'
               IF (IERR.EQ.0) GO TO 50
                  WRITE (LP,1190) STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 80
50             IF (PRPARM.EQ.'YES') THEN
                  IPRNT=1
                  UNITS=SUNITS
                  INCLUDE 'scommon/callspstan'
                  GO TO 80
                  ENDIF
         ENDIF
C
C  CHECK INPUT FIELD FOR DATA TYPE
60    DO 70 I=1,MSTYPE
         IF (STRNG2.EQ.STYPE(I)) GO TO 80
70       CONTINUE
         GO TO 250
C
C  SET DATA TYPE
80    TYPE=STRNG2
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'TYPE=',TYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK OF NEED TO RESET VARIABLES FOR NEW STATION
      IF (TYPE.EQ.'STAN') THEN
         LRUNCK=IRUNCK
         IF (LRUNCK.EQ.0.AND.ISYERR.EQ.1) LRUNCK=1
         INDERR=0
         NUMERR=0
         NUMWRN=0
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         INTWKI=0
         ICSTAN=1
         STSTAN=DISP
         CALL UREPET ('?',STAID,8)
         ILTLNN=0
         IELEVN=0
         NGPS=0
         NGPSN=0
         NSRCCD=0
         NSRCCDO=0
         NGOESN=0
         NCDASN=0
         INSTAN=0
         INPCPN=0
         INTEMP=0
         INPE=0
         INRRS=0
         LPSTAN=0
         LPPCPN=0
         LPTEMP=0
         LPPE=0
         LPRRS=0
         NPPCPN=0
         NPTEMP=0
         NPPE=0
         NPRRSO=0
         NPRRSN=0
         ITPPVR=0
         ITPPVRO=0
         IPTWGT=-1
         IPTWGTO=-1
         IPCHAR=0
         IPCHARO=0
         IPCHARN=0
         MDRBOX=-999
         ITTAVR=0
         ITTAVRO=0
         ITYOBS=0
         ITFMM=0
         ITFMMO=0
         IPMMMT=0
         IPMMMTO=0
         IPMMMTN=0
         TEMPFEO=-999.
         TEMPFE=-999.
         DO 90 I=1,MAXCF
            TEMPCF(I)=-999.
90          CONTINUE
         NRRSTP=0
         NRRSTPO=0
         CALL UMEMST (0,IPARMN,MGPSN)
C     CHECK IF UNITS SPECIFIED
         IF (LLPAR.GT.0) THEN
            IF (LRPAR.EQ.0) THEN
               WRITE (LP,740) NFLD
               CALL SULINE (LP,2)
               LRPAR=LENGTH+1
               ENDIF
            CALL UFPACK (LSTRNG2,STRNG2,ISTRT,LLPAR+1,LRPAR-1,IERR)
            IF (STRNG2.EQ.'ENGL'.OR.STRNG2.EQ.'METR') THEN
               DUNITS=STRNG2
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*) 'DUNITS=',DUNITS
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               ELSE
                  WRITE (LP,700) NFLD,STRNG2
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
         ENDIF
C
      NUMKEY=NUMKEY+1
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
      ICKRGN=0
      ICKCPU=1
      MINCPU=5
      IPRERR=1
      IPUNIT=LP
      TYPERR='ERROR'
      INCLUDE 'clugtres'
      IF (IERR.NE.0) THEN
         CALL SUFATL
         CALL SUEND
         ENDIF
C
C  SET OPTION TO FILL AND NOT WRITE PARAMETER ARRAYS TO FILE
      IWRITE=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GENERAL STATION PARAMETERS
C
      IF (TYPE.EQ.'STAN') THEN
         NRUNCK=LRUNCK
         IF (INSTAN.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,710) 'STAN',STAID
            IF (IOPOVP.EQ.1) THEN
               WRITE (LP,710) 'STAN',STAID
               WRITE (LP,710) 'STAN',STAID
               ENDIF
            CALL SUERRS (LP,0,NUMERR)
            NRUNCK=1
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'BEFORE CALL TO SFSTAN - NRUNCK=',NRUNCK
            CALL SULINE (LP,1)
            ENDIF
         CALL SFSTAN (L1ARRAY,ARRAY(L1ARRAY*1+1),DISP,STSTAN,NRUNCK,
     *      PRPARM,PRNOTE,DUNITS,SUNITS,NFLD,MGPSN,
     *      STAID,NBRSTA,DESCRP,STATE,STALOCO,STALOC,STACOR,ICSTANO,
     *      MSRCCDO,SRCCDO,NSRCCDO,MSRCIDO,SRCIDO,NSRCIDO,
     *      NSRCCD,SRCCD,SRCID,
     *      GOESN,NGOESN,CDASN,NCDASN,
     *      IDESCN,ISTATN,ILTLNN,IELEVN,NPSTAN,ISSTAN,
     *      ITPPVRO,ITTAVRO,
     *      NUGPAO,NUGPA,
     *      NUMSTA,NPRSTA,LSWRK2,ISWRK2,IUIDF1,NUIDF1,NUIDFX,
     *      NUMERR,NUMWRN,IWRITE,LPSTAN,IFSTAN)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'AFTER CALL TO SFSTAN - NRUNCK=',NRUNCK
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IFSTAN.EQ.0.OR.IFSTAN.EQ.1) GO TO 100
            IF (LDEBUG.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (IOSDBG,780) 'STAN',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (IOSDBG,790) 'STAN',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               ENDIF
            INDERR=1
            GO TO 110
100      IF (LRUNCK.EQ.1) GO TO 110
         IF (IWRITE.EQ.1) THEN
            IF (STSTAN.EQ.'OLD'.OR.NPSTAN.GT.0) THEN
               ELSE
                  WRITE (LP,760) 'STAN',STAID
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
110      NSTAN=NSTAN+1
         IF (IFSTAN.EQ.0) INSTAN=1
         IF (NRUNCK.EQ.1) LRUNCK=NRUNCK
         GO TO 240
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PCPN PARAMETERS
C
      IF (TYPE.EQ.'PCPN') THEN
         NRUNCK=LRUNCK
         IF (INPCPN.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,710) 'PCPN',STAID
            IF (IOPOVP.EQ.1) THEN
               WRITE (LP,710) 'PCPN',STAID
               WRITE (LP,710) 'PCPN',STAID
               ENDIF
            CALL SUERRS (LP,0,NUMERR)
            NRUNCK=1
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'BEFORE CALL TO SFPCPN - NRUNCK=',NRUNCK
            CALL SULINE (LP,1)
            ENDIF
         CALL SFPCPN (L1ARRAY,ARRAY(L1ARRAY*2+1),STSTAN,STPCPN,NRUNCK,
     *      PRPARM,PRNOTE,SUNITS,NFLD,
     *      STAID,DESCRP,STATE,STALOC,STACOR,NBRSTA,
     *      ICSTANO,ILTLNN,IFSTAN,
     *      ITPPVR,IPPROC,MDRBOX,PCPNCF,IPTWGT,IPTWGTO,
     *      NPPCPN,IPCHARO,IPCHARN,PXCHR,
     *      NUMERR,NUMWRN,IWRITE,LPPCPN,NSPACE,IFPCPN)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'AFTER CALL TO SFPCPN - NRUNCK=',NRUNCK
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IFPCPN.NE.0) THEN
            IF (LDEBUG.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (IOSDBG,780) 'PCPN',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (IOSDBG,790) 'PCPN',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               ENDIF
            INDERR=1
            GO TO 140
            ENDIF
         IF (LRUNCK.EQ.1) GO TO 140
         IF (IWRITE.EQ.1) THEN
            IF (NPPCPN.EQ.0) THEN
               WRITE (LP,760) 'PCPN',STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
         IF (NGPSN.GT.0) THEN
            DO 120 I=1,NGPSN
               IF (GPSN(I).EQ.'PCPN') GO TO 130
120            CONTINUE
            ENDIF
         IF (NGPSN+1.GT.MGPSN) THEN
            WRITE (LP,770) MGPSN
            CALL SUERRS (LP,2,NUMERR)
            GO TO 140
            ENDIF
         NGPSN=NGPSN+1
         GPSN(NGPSN)='PCPN'
130      IF (IWRITE.EQ.1) IPARMN(NGPSN)=NPPCPN
         INPCPN=1
140      IF (NRUNCK.EQ.1) LRUNCK=NRUNCK
         GO TO 240
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  TEMP PARAMETERS
C
      IF (TYPE.EQ.'TEMP') THEN
         NRUNCK=LRUNCK
         IF (INTEMP.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,710) 'TEMP',STAID
            IF (IOPOVP.EQ.1) THEN
               WRITE (LP,710) 'TEMP',STAID
               WRITE (LP,710) 'TEMP',STAID
               ENDIF
            CALL SUERRS (LP,0,NUMERR)
            NRUNCK=1
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'BEFORE CALL TO SFTEMP - NRUNCK=',NRUNCK
            CALL SULINE (LP,1)
            ENDIF
         CALL SFTEMP (L1ARRAY,ARRAY(L1ARRAY*3+1),STSTAN,STTEMP,NRUNCK,
     *      PRPARM,PRNOTE,DUNITS,NFLD,
     *      STAID,DESCRP,STATE,NBRSTA,
     *      ICSTANO,ILTLNN,IELEVN,IFSTAN,
     *      ITTAVR,ITYOBS,IPMMMT,TEMPCF,ITFMMO,ITFMM,TEMPFEO,TEMPFE,
     *      NPTEMP,IPMMMTO,IPMMMTN,TMPMAX,TMPMIN,
     *      NUMERR,NUMWRN,IWRITE,LPTEMP,NSPACE,IFTEMP)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'AFTER CALL TO SFTEMP - NRUNCK=',NRUNCK
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IFTEMP.NE.0) THEN
            IF (LDEBUG.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (IOSDBG,780) 'TEMP',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (IOSDBG,790) 'TEMP',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               ENDIF
            INDERR=1
            GO TO 170
            ENDIF
         IF (LRUNCK.EQ.1) GO TO 170
         IF (IWRITE.EQ.1) THEN
            IF (NPTEMP.EQ.0) THEN
               WRITE (LP,760) 'TEMP',STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
         IF (NGPSN.GT.0) THEN
            DO 150 I=1,NGPSN
               IF (GPSN(I).EQ.'TEMP') GO TO 160
150            CONTINUE
            ENDIF
         IF (NGPSN+1.GT.MGPSN) THEN
            WRITE (LP,770) MGPSN
            CALL SUERRS (LP,2,NUMERR)
            GO TO 170
            ENDIF
         NGPSN=NGPSN+1
         GPSN(NGPSN)='TEMP'
160      IF (IWRITE.EQ.1) IPARMN(NGPSN)=NPTEMP
         INTEMP=1
170      IF (NRUNCK.EQ.1) LRUNCK=NRUNCK
         GO TO 240
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PE PARAMETERS
C
      IF (TYPE.EQ.'PE') THEN
         NRUNCK=LRUNCK
         IF (INPE.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,710) 'PE',STAID
            IF (IOPOVP.EQ.1) THEN
               WRITE (LP,710) 'PE',STAID
               WRITE (LP,710) 'PE',STAID
               ENDIF
            CALL SUERRS (LP,0,NUMERR)
            NRUNCK=1
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'BEFORE CALL TO SFPE - NRUNCK=',NRUNCK
            CALL SULINE (LP,1)
            ENDIF
         CALL SFPE (L1ARRAY,ARRAY(L1ARRAY*4+1),STSTAN,STPE,NRUNCK,
     *      PRPARM,PRNOTE,SUNITS,NFLD,
     *      STAID,DESCRP,STATE,STALOC,NBRSTA,
     *      IFSTAN,
     *      NPPE,
     *      NUMERR,NUMWRN,IWRITE,LPPE,IFPE)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'AFTER CALL TO SFPE - NRUNCK=',NRUNCK
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IFPE.NE.0) THEN
            IF (LDEBUG.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (IOSDBG,780) 'PE',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (IOSDBG,790) 'PE',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               ENDIF
            INDERR=1
            GO TO 200
            ENDIF
         IF (LRUNCK.EQ.1) GO TO 200
         IF (IWRITE.EQ.1) THEN
            IF (NPPE.EQ.0) THEN
               WRITE (LP,760) 'PE',STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
         IF (NGPSN.GT.0) THEN
            DO 180 I=1,NGPSN
               IF (GPSN(I).EQ.'PE') GO TO 190
180            CONTINUE
            ENDIF
         IF (NGPSN+1.GT.MGPSN) THEN
            WRITE (LP,770) MGPSN
            CALL SUERRS (LP,2,NUMERR)
            GO TO 200
            ENDIF
         NGPSN=NGPSN+1
         GPSN(NGPSN)='PE'
190      IF (IWRITE.EQ.1) IPARMN(NGPSN)=NPPE
         INPE=1
200      IF (NRUNCK.EQ.1) LRUNCK=NRUNCK
         GO TO 240
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RRS PARAMETERS
C
      IF (TYPE.EQ.'RRS') THEN
         NRUNCK=LRUNCK
         IF (INRRS.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,680)
            CALL SULINE (LP,1)
            WRITE (LP,710) 'RRS',STAID
            IF (IOPOVP.EQ.1) THEN
               WRITE (LP,710) 'RRS',STAID
               WRITE (LP,710) 'RRS',STAID
               ENDIF
            CALL SUERRS (LP,0,NUMERR)
            NRUNCK=1
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'BEFORE CALL TO SFRRS - NRUNCK=',NRUNCK
            CALL SULINE (LP,1)
            ENDIF
         CALL SFRRS (L1ARRAY,ARRAY(L1ARRAY*5+1),STSTAN,STRRS,NRUNCK,
     *      PRPARM,PRNOTE,DUNITS,NFLD,
     *      STAID,DESCRP,STATE,STALOC,NBRSTA,NSRCCD,SRCCD,
     *      IFSTAN,
     *      RRSTYPO,NRRSTPO,IRTIMEO,URMISSO,
     *      RRSTYP,NRRSTP,IRTIME,NVLPOB,MNODAY,NUMOBS,
     *      RUNITS,URMISS,ICVRRS,IRSTAT,
     *      NXBUF,LWKBUF,IWKBUF,
     *      NPRRSO,NPRRSN,
     *      NUMERR,NUMWRN,IWRITE,LPRRS,IFRRS)
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,*) 'AFTER CALL TO SFRRS - NRUNCK=',NRUNCK
            WRITE (IOSDBG,880) NUMWRN,NUMERR,IFRRS
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IFRRS.NE.0) THEN
            IF (LDEBUG.GT.0) THEN
               IF (STSTAN.EQ.'NEW') THEN
                  WRITE (IOSDBG,780) 'RRS',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               IF (STSTAN.EQ.'OLD') THEN
                  WRITE (IOSDBG,790) 'RRS',STAID
                  CALL SULINE (LP,1)
                  ENDIF
               CALL SULINE (LP,1)
               ENDIF
            INDERR=1
            GO TO 230
            ENDIF
         IF (LRUNCK.EQ.1) GO TO 230
         IF (IWRITE.EQ.1) THEN
            IF (NPRRSN.EQ.0) THEN
               WRITE (LP,760) 'RRS',STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ENDIF
         IF (NGPSN.GT.0) THEN
            DO 210 I=1,NGPSN
               IF (GPSN(I).EQ.'RRS') GO TO 220
210            CONTINUE
            ENDIF
         IF (NGPSN+1.GT.MGPSN) THEN
            WRITE (LP,770) MGPSN
            CALL SUERRS (LP,2,NUMERR)
            GO TO 230
            ENDIF
         NGPSN=NGPSN+1
         GPSN(NGPSN)='RRS'
220      IF (IWRITE.EQ.1) IPARMN(NGPSN)=NPRRSN
         INRRS=1
230      IF (NRUNCK.EQ.1) LRUNCK=NRUNCK
         GO TO 240
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INVALID PARAMETER TYPE
C
      WRITE (LP,900) TYPE
      CALL SUERRS (LP,2,NUMERR)
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR END OF INPUT
240   IF (NFLD.EQ.-1) GO TO 250
C
C  SET INDICATOR TO REPROCESS FIELD
      ISTRT=-1
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  END OF INPUT FOR STATION DEFINITION
C
250   IENDIN=1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK NUMBER OF KEYWORDS FOUND
      IF (NUMKEY.GT.0) GO TO 260
C
C  NO PARAMETER TYPE KEYWORDS FOUND
      WRITE (LP,890)
      CALL SUWRNS (LP,2,NUMWRN)
      GO TO 650
C
C  CHECK IF RUNCHECK OPTION SPECIFIED
260   IF (LRUNCK.EQ.1) GO TO 650
C
C  CHECK NUMBER OF DATA GROUPS SPECIFIED FOR STATION
      IF (NGPSN.GT.0) GO TO 270
C
C  CHECK IF NEW STATUS SPECIFIED
      IF (ISSTAN.GT.0) GO TO 270
         IF (INSTAN.EQ.1) THEN
            IF (STSTAN.EQ.'NEW') THEN
               WRITE (LP,850) STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            IF (STSTAN.EQ.'OLD') THEN
               WRITE (LP,860) STAID
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            ENDIF
C
270   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' ICSTAN=',ICSTAN,
     *      ' NUGPA=',NUGPA,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,1010) NGPSN,(GPSN(I),I=1,NGPSN)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,1020) NGPS,(GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF GRID-POINT ADDRESS SPECIFIED AND STATION HAS PCPN DATA
      IF (NUGPA.GT.0) THEN
         DO 280 I=1,NGPSN
            IF (GPSN(I).EQ.'PCPN') GO TO 310
280         CONTINUE
         IF (ICSTAN.EQ.1) GO TO 300
         IF (NGPS.EQ.0) GO TO 300
            DO 290 I=1,NGPS
               IF (GPS(I).EQ.'PCPN') GO TO 310
290            CONTINUE
300      WRITE (LP,1030) NUGPA,STAID
         CALL SUERRS (LP,2,NUMERR)
310      ENDIF
C
C  SET STATION STATUS TO INCOMPLETE
      ICOMPL=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK AVAILABLE SPACE IN PREPROCESSOR DATA BASE
      CALL SFPDCK (STAID,ICSTAN,
     *   NGPS,GPS,NGPSN,GPSN,
     *   MDLYTP,DLYTYP,NDLYTP,
     *   ITPPVR,
     *   ITYOBS,ITTAVR,ITFMM,
     *   RRSTYP,NRRSTP,IRSTAT,NVLPOB,NUMOBS,
     *   NUMERR,NUMWRN,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK AVAILABLE SPACE IN PREPROCESSOR PARAMETRIC DATA BASE
      CALL SFPPCK (STAID,NGPSN,GPSN,
     *   STSTAN,STPCPN,STTEMP,STPE,STRRS,
     *   LPSTAN,LPPCPN,LPTEMP,LPPE,LPRRS,
     *   IPCHARN,IPCHARO,IPMMMTN,IPMMMTO,
     *   NUMERR,NUMWRN,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK AVAILABLE SPACE IN PROCESSED DATA BASE
      CALL SFPRCK (STAID,NGPSN,GPSN,
     *   NRRSTP,RRSTYP,URMISS,IRTIME,IRSTAT,
     *   NUMERR,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STORE VALUES OF PCPN AND TEMP TIME INTERVAL AND PCPN CHAR POINTER
      ITPPVRN=ITPPVR
      ITTAVRN=ITTAVR
      IF (IWRITE.EQ.1) IPCHARN=IPCHAR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' ITPPVR=',ITPPVR,
     *      ' ITTAVR=',ITTAVR,
     *      ' IPCHAR=',IPCHAR,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' ITPPVRN=',ITPPVRN,
     *      ' ITTAVRN=',ITTAVRN,
     *      ' IPCHARN=',IPCHARN,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (STSTAN.EQ.'OLD') THEN
C     READ OLD STATION GENERAL PARAMETERS
         IPTR=0
         IPRERR=1
         RDISP='OLD'
         INCLUDE 'scommon/callsrstan'
         IF (IERR.GT.0) GO TO 650
         IF (LDEBUG.GT.0) THEN
            IPRNT=1
            UNITS=SUNITS
            INCLUDE 'scommon/callspstan'
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ANY DATA GROUPS OR RRS TYPES TO BE DELETED ARE REFERENCED
      IF (ICKREF.EQ.1.AND.NGPS.GT.0.AND.NGPSN.GT.0) THEN
         IFIRST=1
         IDIM=LSWORK/3
         ID1=1
         ID2=IDIM*1+1
         ID3=IDIM*2+1
         DO 340 IGPS=1,NGPS
            DO 320 IGPSN=1,NGPSN
               IF (GPS(IGPS).EQ.GPSN(IGPSN)) GO TO 340
320            CONTINUE
C        CHECK PCPN, TEMP AND PE PARAMETERS
            IF (GPS(IGPS).EQ.'PCPN'.OR.
     *          GPS(IGPS).EQ.'TEMP'.OR.
     *          GPS(IGPS).EQ.'PE') THEN
               WRITE (LP,800) GPS(IGPS),'PARAMETERS'
               CALL SULINE (LP,1)
               CALL SLCHK (GPS(IGPS),STAID,
     *            IDIM,SWORK(ID1),IDIM,SWORK(ID2),IDIM,SWORK(ID3),
     *            IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,IERR)
               IF (IERR.NE.0) INDERR=1
               ENDIF
C        CHECK RRS PARAMETERS AND TIME SERIES
            IF (GPS(IGPS).EQ.'RRS') THEN
               IPTR=0
               IREAD=1
               INCLUDE 'scommon/callsrrrs'
               IF (IERR.NE.0) THEN
                  WRITE (LP,810) GPS(IGPS),STAID
                  CALL SUWRNS (LP,2,NUMWRN)
                  GO TO 340
                  ENDIF
               IF (LDEBUG.GT.0) THEN
                  INCLUDE 'scommon/callsprrs'
                  ENDIF
               DO 330 I=1,NRRSTP
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,820) STAID,IRTIME(I),
     *                  RRSTYP(I),URMISS(I)
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  IF (IRTIME(I).GT.0) THEN
                     RTYPE=RRSTYP(I)
                     IF (URMISS(I).NE.'SAME') RTYPE=URMISS(I)
                     WRITE (LP,800) 'RRS DATA TYPE',RTYPE
                     CALL SULINE (LP,1)
                     CALL SLCHK (RTYPE,STAID,
     *                  IDIM,SWORK(ID1),IDIM,SWORK(ID2),IDIM,SWORK(ID3),
     *                  IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *                  IERR)
                     IF (IERR.NE.0) INDERR=1
                     ENDIF
330               CONTINUE
               NRRSTP=0
               ENDIF
340         CONTINUE
C     CHECK FOR RRS TYPES IN OLD DEFINITION BUT NOT IN NEW
         IF (NRRSTPO.GT.0.AND.NRRSTP.GT.0) THEN
            DO 360 I=1,NRRSTPO
               DO 350 J=1,NRRSTP
                  IF (RRSTYPO(I).EQ.RRSTYP(J)) GO TO 360
350               CONTINUE
               IF (IRTIME(I).GT.0) THEN
                  RTYPE=RRSTYP(I)
                  IF (URMISS(I).NE.'SAME') RTYPE=URMISS(I)
                  CALL SLCHK (RTYPE,STAID,
     *               IDIM,SWORK(ID1),IDIM,SWORK(ID2),IDIM,SWORK(ID3),
     *               IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *               IERR)
                  IF (IERR.NE.0) INDERR=1
                  ENDIF
360            CONTINUE
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
370   IF (NUMERR.GT.0) INDERR=1
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (INDERR.EQ.0.AND.(IFSTAN.EQ.0.OR.IFSTAN.EQ.1)) THEN
         ELSE
            IF (STSTAN.EQ.'NEW') THEN
               WRITE (LP,830) STAID
               CALL SULINE (LP,2)
               ENDIF
            IF (STSTAN.EQ.'OLD') THEN
               WRITE (LP,840) STAID
               CALL SULINE (LP,2)
               ENDIF
            ICOMPL=0
            IF (IFSTAN.EQ.0) GO TO 650
            GO TO 590
         ENDIF
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF NEED TO WRITE PARAMETERS TO FILE
C
      IF (IWRITE.EQ.0) THEN
         NUMERRO=NUMERR
C     CHECK SIZE OF ARRAYS
         IF (INPCPN.EQ.1.AND.LPPCPN.GT.LARRAY) THEN
            WRITE (LP,910) 'PCPN',LPPCPN,LARRAY
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (INTEMP.EQ.1.AND.LPTEMP.GT.LARRAY) THEN
            WRITE (LP,910) 'TEMP',LPTEMP,LARRAY
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (INPE.EQ.1.AND.LPPE.GT.LARRAY) THEN
            WRITE (LP,910) 'PE',LPPE,LARRAY
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (INRRS.EQ.1.AND.LPRRS.GT.LARRAY) THEN
            WRITE (LP,910) 'RRS',LPRRS,LARRAY
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (INRRS.EQ.1) THEN
            IF (NRRSTPO.GT.0) THEN
C           CHECK SIZE OF PROCESSED DATA BASE WORK ARRAY FOR READING OLD
C           TIME SERIES
               DO 380 I=1,NRRSTPO
                  IF (IRTIMEO(I).GT.0) THEN
C                 SET DATA TYPE
                     DTYPE=URMISSO(I)
                     IF (DTYPE.EQ.'SAME') DTYPE=RRSTYPO(I)
C                 FIND TYPE IN DIRECTORY
                     CALL PFDTYP (DTYPE,INDEXD)
                     IF (INDEXD.EQ.0) THEN
                        WRITE (LP,920) DTYPE,STAID
                        CALL SUERRS (LP,2,NUMERR)
                        GO TO 380
                        ENDIF
C                 SET NUMBER OF VALUES PER TIME INTERVAL
                     NPERIT=DATFIL(13,INDEXD)
                     IPRERR=1
                     CALL SUPRDW (DTYPE,LWKBUF,IRTIMEO(I),NPERIT,NXBUF,
     *                  IPRERR,LWNEED,NUMERR,IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (LP,930) 'READ OLD',DTYPE,STAID
                        CALL SUERRS (LP,2,NUMERR)
                        ENDIF
                     ENDIF
380               CONTINUE
               ENDIF
C        CHECK SIZE OF PROCESSED DATA BASE WORK ARRAY FOR WRITING NEW
C        TIME SERIES
            DO 390 I=1,NRRSTP
C           CHECK IF TIME SERIES NOT TO BE CREATED
               IF (IRSTAT(I).EQ.-1) GO TO 390
C           SET DATA TYPE
               DTYPE=URMISS(I)
               IF (DTYPE.EQ.'SAME') DTYPE=RRSTYP(I)
C           FIND TYPE IN DIRECTORY
               CALL PFDTYP (DTYPE,INDEXD)
               IF (INDEXD.EQ.0) THEN
                  WRITE (LP,920) DTYPE,STAID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 390
                  ENDIF
C           SET NUMBER OF VALUES PER TIME INTERVAL
               NPERIT=DATFIL(13,INDEXD)
               IPRERR=1
               CALL SUPRDW (DTYPE,LWKBUF,IRTIME(I),NPERIT,NXBUF,
     *            IPRERR,LWNEED,NUMERR,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (LP,930) 'WRITE NEW',DTYPE,STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
390            CONTINUE
            ENDIF
         IF (NUMERR.NE.NUMERRO) GO TO 370
         NUMERRO=NUMERR
         IF (STSTAN.EQ.'OLD') THEN
            WRITE (LP,940) STAID
            CALL SULINE (LP,2)
            ENDIF
C     SET OPTION TO NOT FILL AND TO WRITE PARAMETER ARRAYS TO FILE
         IWRITE=-1
C     WRITE STAN PARAMETERS
         NPOS=LPSTAN
         CALL SUBSTR (ARRAY(L1ARRAY*1+1),1,NPOS*4,ARRAY,1)
         WDISP=STSTAN
         INCLUDE 'scommon/callswstan'
         IF (NPSTAN.GT.0) THEN
            IPTR=0
            IPRERR=1
            RDISP='OLD'
            INCLUDE 'scommon/callsrstan'
            IF (LDEBUG.GT.0) THEN
               INCLUDE 'scommon/callspstan'
               ENDIF
C        RESET VALUES
            ITPPVR=ITPPVRN
            ITTAVR=ITTAVRN
            IF (IPCHARN.EQ.0) IPCHAR=0
            IF (IPCHARN.GT.0) IPCHAR=IPCHARN
            ELSE
               WRITE (LP,760) 'STAN',STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 370
            ENDIF
         UNITS=SUNITS
         IPRNT=1
         DO 400 IGPSN=1,NGPSN
            IF (GPSN(IGPSN).EQ.'PCPN') THEN
               NPOS=LPPCPN
               CALL SUBSTR (ARRAY(L1ARRAY*2+1),1,NPOS*4,ARRAY,1)
C           GET PARAMETER VALUES FROM PARAMETER ARRAY
               IREAD=0
               INCLUDE 'scommon/callsrpcpn'
C           RESET VALUES
               IF (IPCHARN.EQ.0) IPCHAR=0
               IF (IPCHARN.GT.0) IPCHAR=IPCHARN
               IF (IPCHARN.EQ.-1) THEN
C              WRITE CHARACTERISTICS
                  IF (IPCHARO.EQ.0) THEN
                     IPCHAR=0
                     STRING='WRITTEN'
                     ELSE
                        IPCHAR=IPCHARO
                        STRING='UPDATED'
                     ENDIF
                  CALL WPP1CH (PXCHR,IPCHAR,IERR)
                  IF (IERR.EQ.0) THEN
                     WRITE (LP,960) 'PCPN CHAR',
     *                  STRING(1:LENSTR(STRING)),
     *                  STAID
                     CALL SULINE (LP,2)
                     IPCHARN=IPCHAR
                     ELSE
                        STRING='PCPN CHARACTERISTICS'
                        IF (IERR.EQ.1) THEN
                           WRITE (LP,970) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.EQ.2) THEN
                           WRITE (LP,980) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.EQ.3) THEN
                           WRITE (LP,990) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.GT.3) THEN
                           WRITE (LP,1000) 'WPP1CH',IERR
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                     ENDIF
                  ENDIF
C           WRITE PCPN PARAMETERS
               WDISP=STPCPN
               IWRITEO=IWRITE
               IWRITE=1
               INCLUDE 'scommon/callswpcpn'
               IWRITE=IWRITEO
               IF (NPPCPN.EQ.0) THEN
                  WRITE (LP,760) 'PCPN',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ELSE
                     IPARMN(IGPSN)=NPPCPN
                     IF (PRPARM.EQ.'YES') THEN
                        IPTR=0
                        IREAD=1
                        INCLUDE 'scommon/callsrpcpn'
                        LEVEL=1
                        IPREST=0
                        INCLUDE 'scommon/callsppcpn'
                        ENDIF
                  ENDIF
               ENDIF
            IF (GPSN(IGPSN).EQ.'TEMP') THEN
               NPOS=LPTEMP
               CALL SUBSTR (ARRAY(L1ARRAY*3+1),1,NPOS*4,ARRAY,1)
C           GET PARAMETER VALUES FROM PARAMETER ARRAY
               IREAD=0
               INCLUDE 'scommon/callsrtemp'
               IF (IPMMMTN.EQ.-1) THEN
C              WRITE MEAN MONTHLY MXMN TEMPERATURES
                  IF (IPMMMTO.EQ.0) THEN
                     IPMMMT=0
                     STRING='WRITTEN'
                     ELSE
                        IPMMMT=IPMMMTO
                        STRING='UPDATED'
                     ENDIF
                  CALL WPP1MT (TMPMAX,TMPMIN,IPMMMT,IERR)
                  IF (IERR.EQ.0) THEN
                     WRITE (LP,960) 'TEMP MMMT',
     *                  STRING(1:LENSTR(STRING)),
     *                  STAID
                     CALL SULINE (LP,2)
                     IPMMMTN=IPMMMT
                     ELSE
                        STRING='MEAN TEMPERATURES'
                        IF (IERR.EQ.1) THEN
                           WRITE (LP,970) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.EQ.2) THEN
                           WRITE (LP,980) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.EQ.3) THEN
                           WRITE (LP,990) STRING(1:LENSTR(STRING))
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                        IF (IERR.GT.3) THEN
                           WRITE (LP,1000)  'WPP1MT',IERR
                           CALL SUERRS (LP,2,NUMERR)
                           ENDIF
                    ENDIF
                  ENDIF
C           WRITE TEMP PARAMETERS
               WDISP=STTEMP
               IWRITEO=IWRITE
               IWRITE=1
               INCLUDE 'scommon/callswtemp'
               IWRITE=IWRITEO
               IF (NPTEMP.EQ.0) THEN
                  WRITE (LP,760) 'TEMP',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ELSE
                     IPARMN(IGPSN)=NPTEMP
                     IF (PRPARM.EQ.'YES') THEN
                        IPTR=0
                        IREAD=1
                        INCLUDE 'scommon/callsrtemp'
                        LEVEL=1
                        INCLUDE 'scommon/callsptemp'
                        ENDIF
                  ENDIF
               ENDIF
            IF (GPSN(IGPSN).EQ.'PE') THEN
C           WRITE PE PARAMETERS
               NPOS=LPPE
               CALL SUBSTR (ARRAY(L1ARRAY*4+1),1,NPOS*4,ARRAY,1)
               WDISP=STPE
               INCLUDE 'scommon/callswpe'
               IF (NPPE.EQ.0) THEN
                  WRITE (LP,760) 'PE',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ELSE
                     IPARMN(IGPSN)=NPPE
                     IF (PRPARM.EQ.'YES') THEN
                        IPTR=0
                        INCLUDE 'scommon/callsrpe'
                        INCLUDE 'scommon/callsppe'
                        ENDIF
                  ENDIF
               ENDIF
            IF (GPSN(IGPSN).EQ.'RRS') THEN
               NPOS=LPRRS
               CALL SUBSTR (ARRAY(L1ARRAY*5+1),1,NPOS*4,ARRAY,1)
C           GET PARAMETER VALUES FROM PARAMETER ARRAY
               IREAD=0
               INCLUDE 'scommon/callsrrrs'
C           DEFINE TIME SERIES AND FILL VARIABLE ISTREC
               CALL SFRRS2 (STAID,DESCRP,STALOC,
     *            RRSTYP,NRRSTP,IRTIME,NVLPOB,MNODAY,
     *            NUMOBS,RUNITS,URMISS,IRSTAT,ITSREC,NMISS,
     *            NXBUF,LWKBUF,IWKBUF,
     *            NUMERR,NUMWRN,IERR)
C           FILL AND WRITE RRS PARAMETERS
               WDISP=STRRS
               IWRITEO=IWRITE
               IWRITE=1
               INCLUDE 'scommon/callswrrs'
               IWRITE=IWRITEO
               NPRRSN=NPRRS
               IF (NPRRSN.EQ.0) THEN
                  WRITE (LP,760) 'RRS',STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ELSE
                     IPARMN(IGPSN)=NPRRSN
                     IF (PRPARM.EQ.'YES') THEN
                        IPTR=0
                        IREAD=1
                        INCLUDE 'scommon/callsrrrs'
                        INCLUDE 'scommon/callsprrs'
                        ENDIF
                  ENDIF
               ENDIF
400         CONTINUE
         IF (STSTAN.EQ.'OLD'.AND.NUMERR-NUMERRO.GT.0) THEN
            WRITE (LP,950) STAID
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
C     SET OPTION TO FILL AND WRITE PARAMETER ARRAYS TO FILE
         IWRITE=1
         ENDIF
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ANY PARAMETER GROUPS DEFINED
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' INPCPN=',INPCPN,
     *      ' INTEMP=',INTEMP,
     *      ' INPE=',INPE,
     *      ' INRRS=',INRRS,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (INPCPN.EQ.1.OR.INTEMP.EQ.1.OR.INPE.EQ.1.OR.INRRS.EQ.1) THEN
         GO TO 410
         ENDIF
C
C  CHECK IF ANY STAN PARAMETERS CHANGED
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' INSTAN=',INSTAN,
     *      ' IDESCN=',IDESCN,
     *      ' ISTATN=',ISTATN,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (INSTAN.GT.0.OR.IDESCN.GT.0.OR.ISTATN.GT.0) GO TO 410
      GO TO 630
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS DATA GROUP CODES
C
410   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1040) NGPS,GPS(1),NGPSN,ICSTAN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NGPS.GT.0) THEN
         IF (GPS(1).NE.'????') GO TO 430
         ENDIF
C
C  STORE DATA GROUP CODES - NONE DEFINED
      NGPS=NGPSN
      DO 420 I=1,NGPS
         GPS(I)=GPSN(I)
         IPARM(I)=IPARMN(I)
420      CONTINUE
      ITPPVR=ITPPVRN
      ITTAVR=ITTAVRN
      IF (IWRITE.EQ.1) IPCHAR=IPCHARN
      GO TO 550
C
430   IPRSPC=1
C
C  DATA GROUPS ALREADY DEFINED - CHECK IF ANY NEW GROUPS DEFINED
      IF (NGPSN.EQ.0) GO TO 550
C
C  CHECK IF STATION HAD CHARACTERISTICS BEFORE BUT DOES NOT NOW
      IF (IPCHARO.GT.0.AND.IPCHARN.EQ.0) THEN
C     DELETE CHARACTERISTICS
         CALL WPPDCH (IPCHARO,IERR)
         IF (IERR.EQ.0) THEN
            IF (IPRSPC.EQ.1) THEN
               WRITE (LP,680)
               CALL SULINE (LP,1)
               IPRSPC=0
               ENDIF
            WRITE (LP,1050) STAID
            CALL SULINE (LP,1)
            ELSE
               IF (IERR.EQ.1) THEN
                  WRITE (LP,1060) 'PCPN','CHAR',STAID,IPCHAR
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
               IF (IERR.EQ.2) THEN
                  WRITE (LP,1070) 'PCPN','CHAR',IPCHAR,STAID
                  CALL SUWRNS (LP,2,NUMWRN)
                  ENDIF
               IF (IERR.EQ.3) THEN
                  WRITE (LP,1080) 'PCPN','CHAR',IPCHAR,STAID
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
            ENDIF
         ENDIF
C
ckwz.r22-63.save DESCRP before goto delete parameters ...
      write(descrpTmp,'(A)')DESCRP

C  DELETE PARAMETERS FOR GROUPS NOT SPECIFIED
      INPCPNO=0
      INTEMPO=0
      INPEO=0
      INRRSO=0
      IDPCPNO=0
      IDTEMPO=0
      IDPEO=0
      IDRRSO=0
      INTWK=0
      DO 460 I=1,NGPS
         IF (GPS(I).EQ.'PCPN') INPCPNO=1
         IF (GPS(I).EQ.'TEMP') INTEMPO=1
         IF (GPS(I).EQ.'PE') INPEO=1
         IF (GPS(I).EQ.'RRS') INRRSO=1
         DO 440 J=1,NGPSN
            IF (GPS(I).EQ.GPSN(J)) GO TO 460
440         CONTINUE
         INDFLD=-2
         IF (GPS(I).EQ.'RRS') THEN
            IF (IPRSPC.EQ.1) THEN
               WRITE (LP,680)
               CALL SULINE (LP,1)
               IPRSPC=0
               ENDIF
C        DELETE RRS TIME SERIES
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrrrs'
            DO 450 N=1,NRRSTP
               IF (IRTIME(N).GT.0) THEN
                  RTYPE=RRSTYP(N)
                  IF (URMISS(N).NE.'SAME') RTYPE=URMISS(N)
                  CALL SLPRD (INDFLD,STAID,RTYPE,IERR)
                  ENDIF
450            CONTINUE
            NRRSTP=0
            ENDIF
         IF (IPRSPC.EQ.1) THEN
            WRITE (LP,680)
            CALL SULINE (LP,2)
            IPRSPC=0
            ENDIF
         IF (GPS(I).EQ.'PCPN') THEN
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrpcpn'
            IDPCPNO=1
            ITPPVRO=0
            IF (IPTIME.LT.24) ITPPVRO=IPTIME
            ENDIF
         IF (GPS(I).EQ.'TEMP') THEN
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrtemp'
            IDTEMPO=1
            ITTAVRO=ITTAVR
            ITFMMO=ITFMM
            TEMPFEO=TEMPFE
            ENDIF
         IF (GPS(I).EQ.'PE') THEN
            IDPEO=1
            ENDIF
         IF (GPS(I).EQ.'RRS') THEN
            IDRRSO=1
            ENDIF
C     DELETE PARAMETERS
         CALL SLPPP (INDFLD,STAID,GPS(I),NUMERR,NUMWRN,LARRAY,ARRAY,
     *      IERR)
         IF (IERR.EQ.0) THEN
            GPS(I)=' '
            ITPPVRI=0
            ITTAVRI=0
            ITFMMI=0
            NUGPAI=0
            IF (IDPCPNO.EQ.1) THEN
               IF (ITPPVRO.GT.0) ITPPVRI=1
               ITPPVR=0
               INTWK=1
               ENDIF
            IF (IDTEMPO.EQ.1) THEN
               IF (ITTAVRO.GT.0) ITTAVRI=1
               IF (ITFMMO.GT.0) ITFMMI=1
               ITTAVR=0
               ITFMM=0
               INTWK=1
               ENDIF
            IF (IDPEO.EQ.1) THEN
               INTWK=1
               ENDIF
            IF (IDRRSO.EQ.1) THEN
               INTWK=1
               ENDIF
            ENDIF
460      CONTINUE

ckwz.r22-63.restore DESCRP.4/5/05
      write(DESCRP,'(A)'),descrpTmp
	  
      IF (INTWK.EQ.1) THEN
         IPTWGTI=1
         IPSORT=1
         ITSORT=1
         IESORT=1
         SORTBYX=' '
         IAREA=0
         ISORTBY=0
C     SET NTWK INDICATORS
         CALL SNTWKI (IDPCPNO,ITPPVRI,IPTWGTI,IPSORT,
     *      IDTEMPO,ITTAVRI,ITFMMI,ITSORT,
     *      IDPEO,IESORT,
     *      IDRRSO,
     *      NUGPAI,
     *      SORTBYX,
     *      IAREA,
     *      ISORTBY,
     *      INTWKI,IERR)
         ENDIF
C
C  DELETE TIME SERIES FOR RRS TYPES NOT SPECIFIED
      IF (NRRSTPO.GT.0.AND.NRRSTP.GT.0) THEN
         DO 480 I=1,NRRSTPO
            DO 470 J=1,NRRSTP
               IF (RRSTYPO(I).EQ.RRSTYP(J)) GO TO 480
470            CONTINUE
               IF (IRTIMEO(I).GT.0) THEN
                  IF (IPRSPC.EQ.1) THEN
                     WRITE (LP,680)
                     CALL SULINE (LP,1)
                     IPRSPC=0
                  ENDIF
                  INDFLD=-2
                  RTYPE=RRSTYPO(I)
                  IF (URMISSO(I).NE.'SAME') RTYPE=URMISSO(I)
                  CALL SLPRD (INDFLD,STAID,RTYPE,IERR)
                  ENDIF
480         CONTINUE
         ENDIF
C
C  CHECK IF NEED TO SET AUTOMATIC NETWORK RUN INDICATOR
      IF (INTWK.EQ.1.AND.INAUTO.EQ.0) THEN
         INAUTO=1
         WRITE (LP,1090)
         CALL SULINE (LP,2)
         ENDIF
C
C  CHECK FOR ANY DELETED DATA GROUPS
      NDELT=0
      DO 490 I=1,NGPS
         IF (GPS(I).NE.' '.AND.GPS(I).NE.'????') GO TO 490
            NDELT=NDELT+1
            J=I+1
            IF (J.GT.NGPS) GO TO 490
               GPS(I)=GPS(J)
               GPS(J)='????'
490      CONTINUE
      IF (NDELT.GT.0) NGPS=NGPS-NDELT
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1100) NDELT,NGPS,(GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  ADD NEW GROUPS
      DO 520 I=1,NGPSN
         DO 500 J=1,NGPS
            IF (GPSN(I).EQ.GPS(J)) GO TO 510
500         CONTINUE
            NGPS=NGPS+1
            GPS(NGPS)=GPSN(I)
            IPARM(NGPS)=IPARMN(I)
            GO TO 520
510      IPARM(J)=IPARMN(I)
520      CONTINUE
      DO 540 I=1,NGPS
         DO 530 J=1,NGPSN
            IF (GPS(I).EQ.GPSN(J)) GO TO 530
               IF (GPS(I).EQ.'PCPN') THEN
                  ITPPVR=ITPPVRN
                  IF (IWRITE.EQ.1) IPCHAR=IPCHARN
                  ENDIF
               IF (GPS(I).EQ.'TEMP') THEN
                  ITTAVR=ITTAVRN
                  ENDIF
530         CONTINUE
540      CONTINUE
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS SOURCE CODES
C
550   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'NSRCCD=',NSRCCD,
     *      'NSRCCDO=',NSRCCDO,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF SOURCE CODES ALREADY DEFINED
      IF (NSRCCDO.EQ.0) GO TO 580
C
C  DELETE PARAMETERS FOR SOURCE CODES NOT SPECIFIED
      SAID=' '
      SMID=' '
      DO 570 I=1,NSRCCDO
         IF (NSRCCD.GT.0) THEN
            DO 560 J=1,NSRCCD
               IF (SRCCDO(I).EQ.SRCCD(J)) GO TO 570
560            CONTINUE
            ENDIF
         INDFLD=-2
         CALL SUBSTR (SRCCDO(I),1,LEN(TSRCCD),TSRCCD,1)
         IF (TSRCCD.EQ.'GHB5'.OR.
     *       TSRCCD.EQ.'GPLT'.OR.
     *       TSRCCD.EQ.'CDAS') THEN
            GOESID=' '
            CALL SUBSTR (SRCIDO(1,I),1,LEN(GOESID),GOESID,1)
            WRITE (LP,1120) TSRCCD,GOESID
            CALL SULINE (LP,1)
            CALL SLGOES (INDFLD,STAID,TSRCCD,GOESID,TYPMSG,IERR)
            ENDIF
         IF (TSRCCD.EQ.'SA') THEN
            CALL SUBSTR (SRCIDO(1,I),1,LEN(SAID),SAID,1)
            ENDIF
         IF (TSRCCD.EQ.'SM') THEN
            CALL SUBSTR (SRCIDO(1,I),1,LEN(SMID),SMID,1)
            ENDIF
570      CONTINUE
      IF (SAID.NE.' '.OR.SMID.NE.' ') THEN
         STRING='SA AND SM SOURCE CODES'
         IF (SAID.NE.' '.AND.SMID.EQ.' ') STRING='SA SOURCE CODE'
         IF (SAID.EQ.' '.AND.SMID.NE.' ') STRING='SM SOURCE CODE'
         WRITE (LP,1130) STRING(1:LENSTR(STRING))
         CALL SULINE (LP,1)
         CALL SLSASM (INDFLD,STAID,TYPMSG,IERR)
         ENDIF
C
C          - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF STATUS IS COMPLETE
580   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'ICOMPL=',ICOMPL
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (ICOMPL.EQ.0) GO TO 590
C
C  CHECK IF STAN PARAMETERS WERE WRITTEN
      IF (IFSTAN.GT.1) GO TO 650
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) GO TO 590
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' ITPPVR=',ITPPVR,
     *      ' IPCHAR=',IPCHAR,
     *      ' ITTAVR=',ITTAVR,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISSTAN.GT.0.AND.NGPSN.EQ.0) GO TO 600
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IREADA.EQ.1) THEN
C     READ PARAMETERS FOR GROUPS NOT REDEFINED - UPDATE STATION
C     DESCRIPTION AND STATE DESIGNATOR IF NECESSARY
         CALL SFRGPS (DISP,PRPARM,STAID,NBRSTA,NGPS,GPS,
     *      NGPSN,GPSN,IPARM,IDESCN,ISTATN,DESCRP,STATE,
     *      INPCPN,IPPROC,PCPNCF,ITPPVR,
     *      ITYOBS,TEMPCF,ITTAVR,ITFMM,
     *      RRSTYP,NRRSTP,IRTIME,NVLPOB,NUMOBS,MNODAY,
     *      LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,1110) STAID
            CALL SULINE (LP,2)
            GO TO 650
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CREATE OR CHANGE STATION IN DATA ENTRY CONTROL FILES
C
      IDEERR=0
C
      IF (NSRCCD.GT.0) THEN
         CALL SFDECR (STSTAN,STAID,DESCRP,STALOC,ICSTAN,NGPS,GPS,
     *      NSRCCD,SRCCD,SRCID,INWSRC,
     *      ITPPVR,ITTAVR,IPPROC,ITYOBS,
     *      RRSTYP,NRRSTP,IRTIME,IRTIMEA,
     *      NGOESN,GOESN,NCDASN,CDASN,
     *      NUMERR,NUMWRN,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,870) NUMWRN,NUMERR
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IERR.NE.0) THEN
            IF (STSTAN.EQ.'NEW') THEN
               WRITE (LP,1140) STAID
               CALL SULINE (LP,2)
               ENDIF
            IF (STSTAN.EQ.'OLD') THEN
               WRITE (LP,1150) STAID
               CALL SULINE (LP,2)
               ENDIF
            IDEERR=1
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CREATE OR CHANGE STATION IN PREPROCESSOR DATA BASE
C
      CALL SFPDCR (STSTAN,PDDISP,PRPARM,STAID,NBRSTA,NPSTAN,
     *   ICSTANO,NGPS,GPS,NGPSN,
     *   DLYTYP,NDLYTP,
     *   INPCPN,IPPROC,PCPNCF,MDRBOX,IPCHAR,
     *   ITYOBS,TEMPCF,IPMMMT,
     *   RRSTYP,NRRSTP,NVLPOB,NUMOBS,MNODAY,
     *   IPARM,IPPPTR,
     *   IPPP24,IPPPVR,IPTM24,IPTAVR,IPTF24,IPEA24,
     *   LARRAY,ARRAY,NUMERR,NUMWRN,IDEERR,IPDERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IDEERR.EQ.0.AND.IPDERR.EQ.0) THEN
         IF (LDEBUG.GT.0) THEN
C        PRINT PREPROCESSOR DATA BASE STATION INFORMATION FILE RECORDS
            IRESET=0
            CALL PDDSIF (LSIBUF,ISIBUF,IRESET,LARRAY,ARRAY)
            ENDIF
         GO TO 600
         ENDIF
      IF (IDEERR.EQ.0) THEN
         IF (PDDISP.EQ.'NEW') THEN
            WRITE (LP,1160) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (PDDISP.EQ.'OLD') THEN
            WRITE (LP,1170) STAID
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ERRORS ENCOUNTERED IN CREATING OR CHANGING STATION
590   ICSTAN=1
      IF (STSTAN.EQ.'OLD'.AND.ICSTANO.EQ.0.AND.ISSTAN.NE.1) ICSTAN=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,1180) STSTAN,ICSTANO,ISSTAN,ICSTAN,INDCMP
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (INDCMP.EQ.1) GO TO 620
      GO TO 610
C
C  SET STATION STATUS INDICATOR TO COMPLETE
600   ICSTAN=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF STATUS OF INCOMPLETE SPECIFIED
      IF (ISSTAN.NE.1) GO TO 620
      ICSTAN=1
C
610   IF (INDCMP.EQ.0) THEN
C     SET GROUPS, SOURCE CODES, ETC. TO ZERO
         NGPS=0
         NSRCCD=0
         NSRCID=0
         NGOESN=0
         NCDASN=0
         ENDIF
C
C  UPDATE STAN PARAMETERS
620   WDISP='OLD'
      INCLUDE 'scommon/callswstan'
      IF (IERR.NE.0) THEN
         WRITE (LP,1190) STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 650
         ENDIF
C
      IF (ICSTAN.EQ.0) THEN
         IF (STSTAN.EQ.'NEW') NDFNEW=NDFNEW+1
         IF (STSTAN.EQ.'OLD') NDFOLD=NDFOLD+1
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' STAID=',STAID,
     *      ' NGPS=',NGPS,
     *      ' NSRCCD=',NSRCCD,
     *      ' '
         CALL SULINE (IOSDBG,2)
         ENDIF
C
630   IF (IFSTAN.EQ.0.AND.PRPARM.EQ.'YES') THEN
         IPTR=0
         IPRERR=0
         RDISP='OLD'
         INCLUDE 'scommon/callsrstan'
         IF (IERR.GT.0) GO TO 650
         IPRNT=1
         UNITS=SUNITS
         INCLUDE 'scommon/callspstan'
         ENDIF
C
      IF (ICSTAN.EQ.1) GO TO 640
C
C  COUNT NUMBER OF DATA TYPES
      IF (INPCPN.EQ.1) THEN
         NPP24=NPP24+1
         IF (ITPPVR.GT.0) NPPVR=NPPVR+1
         ENDIF
      IF (INTEMP.EQ.1) THEN
         NTA24=NTA24+1
         IF (ITTAVR.GT.0.AND.ITTAVR.LT.24) NTAIN=NTAIN+1
         IF (ITFMM.EQ.1) NTF24=NTF24+1
         ENDIF
      IF (INPE.EQ.1) THEN
         NEA24=NEA24+1
         ENDIF
      IF (INRRS.EQ.1) THEN
         NRRS=NRRS+1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET NTWK INDICATORS
C
C  CHECK IF NEW STATION OR OLD BUT INCOMPLETE STATION
      IF (STSTAN.EQ.'NEW'.OR.ICSTANO.EQ.1) THEN
         IPTWGTI=1
         IPSORT=1
         ITSORT=1
         IESORT=1
         SORTBYX=' '
         IAREA=0
         ISORTBY=0
         CALL SNTWKI (INPCPN,ITPPVR,IPTWGTI,IPSORT,
     *      INTEMP,ITTAVR,ITFMM,ITSORT,
     *      INPE,IESORT,
     *      INRRS,
     *      NUGPA,
     *      SORTBYX,
     *      IAREA,
     *      ISORTBY,
     *      INTWKI,IERR)
         ENDIF
C
C  CHECK IF OLD STATION
      IF (STSTAN.EQ.'OLD') THEN
         IPCPN=0
         ITEMP=0
         IPE=0
         IRRS=0
         INUGPA=0
         ITPPVRI=0
         IPTWGTI=1
         IPSORT=0
         ITTAVRI=0
         ITFMMI=0
         ITSORT=0
         IESORT=0
C     CHECK IF DATA GROUP ADDED
         IF (INPCPNO.EQ.0.AND.INPCPN.EQ.1) THEN
            ITPPVRI=ITPPVR
            IPCPN=1
            IPSORT=1
            ENDIF
         IF (INTEMPO.EQ.0.AND.INTEMP.EQ.1) THEN
            ITTAVRI=ITTAVR
            ITFMMI=ITFMM
            ITEMP=1
            ITSORT=1
            ENDIF
         IF (INPEO.EQ.0.AND.INPE.EQ.1) THEN
            IPE=1
            IESORT=1
            ENDIF
         IF (INRRSO.EQ.0.AND.INRRS.EQ.1) THEN
            IRRS=1
            ENDIF
C     CHECK IF DATA GROUP REMOVED
         IF (INPCPNO.EQ.1.AND.INPCPN.EQ.0) THEN
            ITPPVRI=ITPPVRO
            IPCPN=1
            IPSORT=1
            ENDIF
         IF (INTEMPO.EQ.1.AND.INTEMP.EQ.0) THEN
            ITTAVRI=ITTAVRO
            ITFMMI=ITFMMO
            ITEMP=1
            ITSORT=1
            ENDIF
         IF (INPEO.EQ.1.AND.INPE.EQ.0) THEN
            IPE=1
            IESORT=1
            ENDIF
         IF (INRRSO.EQ.1.AND.INRRS.EQ.0) THEN
            IRRS=1
            ENDIF
C     CHECK IF PARAMETERS CHANGED
         IF (INPCPN.EQ.1.AND.INPCPNO.EQ.1) THEN
            IF (ITPPVR.NE.ITPPVRO) THEN
               ITPPVRI=1
               IPSORT=1
               IPCPN=1
               ENDIF
            IF (ILTLNN.EQ.1) THEN
               ITPPVRI=1
               IPCPN=1
               ENDIF
            IF (IPTWGT.NE.-1.AND.IPTWGTO.NE.-1) THEN
               IF (IPCPN.EQ.0.AND.IPTWGT.NE.IPTWGTO) THEN
                  IPTWGTI=0
                  IPCPN=1
                  ENDIF
               ENDIF
            ENDIF
         IF (INTEMP.EQ.1.AND.INTEMPO.EQ.1) THEN
            IF (ITTAVR.NE.ITTAVRO) THEN
               ITSORT=1
               ITTAVRI=1
               ITEMP=1
               ENDIF
            IF (ITFMM.NE.ITFMMO) THEN
               ITFMMI=1
               ITEMP=1
               ENDIF
            IF (ILTLNN.EQ.1) THEN
               ITTAVRI=1
               ITFMMI=1
               ITEMP=1
               ENDIF
            IF (IELEVN.EQ.1) THEN
               ITTAVRI=1
               ITFMMI=1
               ITEMP=1
               ENDIF
C        CHECK DIFFERENCE IN NEW AND OLD TEMP FE VALUES
            DIFF=TEMPFE-TEMPFEO
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*)
     *            ' TEMPFE=',TEMPFE,
     *            ' TEMPFEO=',TEMPFEO,
     *            ' DIFF=',DIFF,
     *            ' '
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (TEMPFE.LE.99.9.AND.DIFF.GT.0.01.OR.
     *          TEMPFE.GT.99.9.AND.DIFF.GT.0.1) THEN
               ITTAVRI=1
               ITFMMI=1
               ITEMP=1
               ENDIF
            ENDIF
         IF (INPE.EQ.1.AND.INPEO.EQ.1) THEN
            IF (ILTLNN.EQ.1) THEN
               IPE=1
               ENDIF
            ENDIF
         IF (NUGPA.NE.NUGPAO) THEN
            INUGPA=1
            ENDIF
C     CHECK IF RRS PARAMETER RECORD NUMBER CHANGED
         IF (NPRRSO.NE.NPRRSN) THEN
            IRRS=1
            ENDIF
         SORTBYX=' '
         IAREA=0
         ISORTBY=0
         CALL SNTWKI (IPCPN,ITPPVRI,IPTWGTI,IPSORT,
     *      ITEMP,ITTAVRI,ITFMMI,ITSORT,
     *      IPE,IESORT,
     *      IRRS,
     *      INUGPA,
     *      SORTBYX,
     *      IAREA,
     *      ISORTBY,
     *      INTWKI,IERR)
C     CHECK IF DESCRIPTION CHANGED AND SORT BY DESCRIPTION SPECIFIED
         IF (IDESCN.EQ.1.AND.SORTBY.EQ.'DESC') THEN
            IPTWGTI=1
            IPSORT=1
            ITSORT=1
            IESORT=1
            IAREA=0
            ISORTBY=0
            CALL SNTWKI (INPCPN,ITPPVR,IPTWGTI,IPSORT,
     *         INTEMP,ITTAVR,ITFMM,ITSORT,
     *         INPE,IESORT,
     *         INRRS,
     *         NUGPA,
     *         SORTBY,
     *         IAREA,
     *         ISORTBY,
     *         INTWKI,IERR)
            ENDIF
C     CHECK IF NEED TO SET AUTOMATIC NETWORK RUN INDICATOR
         IF (INTWKI.EQ.1.AND.INAUTO.EQ.0) THEN
            INAUTO=1
            WRITE (LP,1090)
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  CHECK IF NEED TO UPDATE NTWK PARAMETERS
640   IF (INTWKI.EQ.1) THEN
C     UPDATE NTWK PARAMETERS
         WDISP='OLD'
         CALL SWNTWK (IVNTWK,UNSD,NNWFLG,INWFLG,INWDTE,
     *      LARRAY,ARRAY,WDISP,IERR)
         IF (LDEBUG.GT.1) THEN
            CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
            ENDIF
C     RESET INDICATOR WHETHER NTWK COMMON BLOCK HAS BEEN FILLED
         INWFIL=0
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
650   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,870) NUMWRN,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IPOS=NUIDF1*NPRSTA
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0.OR.INDERR.GT.0) THEN
         NSTERR=NSTERR+1
         IF (IUIDF1.EQ.0) CALL SUBSTR ('E',1,1,ISWRK2(IPOS),-1)
         ENDIF
C
C  CHECK IF WARNINGS ENCOUNTERED
      IF (NUMWRN.GT.0) THEN
         NSTWRN=NSTWRN+1
         IF (IUIDF1.EQ.0) CALL SUBSTR ('W',1,1,ISWRK2(IPOS),-3)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IENDIN.EQ.0) GO TO 60
C
C  PRINT NUMBER OF STATIONS PROCESSED
      IF (NUMSTA.GT.0) THEN
         WRITE (LP,1210)
         CALL SULINE (LP,2)
         WRITE (LP,1230) NDFNEW,NDFOLD
         CALL SULINE (LP,2)
         WRITE (LP,1240) NPP24,'24-HR PCPN'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NPPVR,'<24-HR PCPN'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NTA24,'MAX/MIN TEMP'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NTAIN,'INST TEMP'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NTF24,'FORECAST TEMP'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NEA24,'PE'
         CALL SULINE (LP,1)
         WRITE (LP,1240) NRRS,'RRS'
         CALL SULINE (LP,1)
         GO TO 660
         ENDIF
      WRITE (LP,1220)
      CALL SULINE (LP,2)
C
660   WRITE (LP,1250) NSTERR,'ERRORS'
      CALL SULINE (LP,2)
      WRITE (LP,1250) NSTWRN,'WARNINGS'
      CALL SULINE (LP,2)
C
C  PRINT STATION IDENTIFIER AND PAGE ON WHICH DEFINITION STARTS
      CALL SUIDF2 (NUIDF1,NUIDFX,LSWRK2,ISWRK2,NSTERR,NSTWRN,IERR)
C
C  PRINT NUMBER OF ERRORS AND WARNINGS
      IF (NERR.GT.0) THEN
         WRITE (LP,1260) NERR,'ERRORS'
         CALL SULINE (LP,2)
         ENDIF
      IF (NWARN.GT.0) THEN
         WRITE (LP,1260) NWARN,'WARNINGS'
         CALL SULINE (LP,2)
         ENDIF
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFSTA : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
680   FORMAT (' ')
690   FORMAT ('+*** ERROR - DISPOSITION OF ''OLD'' NOT CURRENTLY ',
     *   'ALLOWED. ',
     *   'RUNCHECK OPTION SET. INPUT WILL BE CHECKED FOR ERRORS.')
700   FORMAT ('0*** ERROR - CARD FIELD ',I2,' HAS AN INVALID UNITS ',
     *   'CODE : ',A)
710   FORMAT ('+*** ERROR - ',A4,' PARAMETERS ALREADY SPECIFIED ',
     *   'FOR STATION ',A,'. ',
     *   'RUNCHECK OPTION SET. INPUT WILL BE CHECKED FOR ERRORS.')
720   FORMAT (' STRNG2=',A)
730   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
740   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
750   FORMAT ('0*** NOTE - NTWK PARAMETERS NOT SUCCESSFULLY READ. ',
     *   'RUNCHECK OPTION SET. INPUT WILL BE CHECKED FOR ERRORS.')
760   FORMAT ('0*** ERROR - IN SFSTA - RECORD NUMBER OF ',A4,
     *   ' PARAMETERS IS ZERO FOR STATION ',A,'.')
770   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF STATION DATA GROUPS (',
     *   I2,') EXCEEDED.')
780   FORMAT (' ',A4,' PARAMETERS FOR STATION ',A,
     *   ' NOT SUCCESSFULLY DEFINED.')
790   FORMAT (' ',A4,' PARAMETERS FOR STATION ',A,
     *   ' NOT SUCCESSFULLY REDEFINED.')
800   FORMAT (' *** NOTE - ',A4,' ',A,' FOUND IN OLD DEFINITION ',
     *   'BUT NOT IN NEW AND WILL BE DELETED.')
810   FORMAT ('0*** WARNING - ',A4,' PARAMETERS FOR STATION ',A,
     *   'NOT SUCCESSFULLY READ.')
820   FORMAT (' STAID=',A,3X,'IRTIME=',I3,3X,'RRSTYP=',A4,3X,
     *   'URMISS=',A4)
830   FORMAT ('0*** NOTE - STATION ',A,' NOT SUCCESSFULLY DEFINED ',
     *   'BECAUSE ERRORS ENCOUNTERED.')
840   FORMAT ('0*** NOTE - STATION ',A,' NOT SUCCESSFULLY REDEFINED ',
     *   'BECAUSE ERRORS ENCOUNTERED.')
850   FORMAT ('0*** ERROR - STATION ',A,' HAS NO DATA ',
     *   'GROUPS (PCPN, TEMP, PE OR RRS) SUCCESSFULLY DEFINED.')
860   FORMAT ('0*** WARNING - STATION ',A,' HAS NO DATA ',
     *   'GROUPS (PCPN, TEMP, PE OR RRS) SUCCESSFULLY REDEFINED.')
870   FORMAT (' NUMWRN=',I4,3X,'NUMERR=',I4)
880   FORMAT (' NUMWRN=',I4,3X,'NUMERR=',I4,3X,'IFRRS=',I2)
890   FORMAT ('0*** WARNING - NO PARAMETER TYPE KEYWORD (PCPN, TEMP ',
     *   'PE OR RRS) WAS FOUND.')
900   FORMAT ('0*** ERROR - IN SFSTA - PARAMETER TYPE ',A4,' CANNOT ',
     *   'BE PROCESSED.')
910   FORMAT ('0*** ERROR - IN SFSTA - NUMBER OF WORDS IN ',A,' ',
     *   'PARAMETER ARRAY (',I5,') EXCEEDS SIZE OF WORK ARRAY (',I5,'.')
920    FORMAT ('0*** ERROR - IN SFSTA - DATA TYPE ',A,' ',
     *   'NOT FOUND IN PROCESSED DATA BASE DIRECTORY ',
     *   'FOR STATION ',A,'.')
930   FORMAT ('0*** ERROR - IN SFSTA - ',
     *   'SIZE OF WORK ARRAY IS TOO SMALL TO ',A,' TIME SERIES ',
     *   'FOR DATA TYPE ',A,' ',
     *   'FOR STATION ',A,'.')
940   FORMAT ('0*** NOTE - STARTING FILE UPDATES TO REDEFINE ',
     *   'STATION ',A,'. ',
     *   'IF ANY ERRORS OCCUR, ',
     *   'STATION MAY NEED TO BE DELETED AND DEFINED.')
950   FORMAT ('0*** WARNING - ERRORS ENCOUNTERED REDEFINING ',
     *   'STATION ',A,'. ',
     *   'STATION MAY NEED TO BE DELETED AND DEFINED.')
960   FORMAT ('0*** NOTE - ',A,' VALUES SUCCESSFULLY ',A,' ',
     *   'FOR STATION ',A,'.')
970   FORMAT ('0*** ERROR - SYSTEM ERROR WRITING ',A,' ',
     *   'TO THE PPPDB.')
980   FORMAT ('0*** ERROR - ',A,' FILE IS FULL.')
990   FORMAT ('0*** ERROR - ',A,' NOT ',
     *   'DEFINED IN PREPROCESSOR PARAMETRIC DATA BASE.')
1000  FORMAT ('0*** ERROR - IN SFSTA - STATUS CODE RETURNED FROM ',A,
     *   ' (',I2,') NOT RECOGNIZED.')
1010  FORMAT (' NGPSN=',I2,3X,'GPSN(1...NGPSN)=',5(A4,1X))
1020  FORMAT (' NGPS=',I2,3X,'GPS(1...NGPS)=',5(A4,1X))
1030  FORMAT ('0*** ERROR - GRID-POINT ADDRESS ',I4,' WAS SPECIFIED ',
     *   'FOR STATION ',A,' BUT STATION DOES NOT HAVE PCPN ',
     *   'PARAMETERS.')
1040  FORMAT (' NGPS=',I2,3X,'GPS(1)=',A4,3X,'NGPSN=',I2,3X,
     *  'ICSTAN=',I2)
1050  FORMAT (' *** NOTE - PCPN CHAR VALUES SUCCESSFULLY DELETED ',
     *   'FOR STATION ',A,'.')
1060  FORMAT ('0*** ERROR - SYSTEM ERROR ACCESSING ',A,' ',
     *   A,' FOR STATION ',A,'. IPCHAR=',I3)
1070  FORMAT ('0*** WARNING - ',A,' ',A,' AT LOCATION ',I3,' ',
     *   'FOR STATION ',A,' ARE ALREADY MARKED AS DELETED.')
1080  FORMAT ('0*** ERROR - INVALID VALUE OF ',A,' ',A,' ',
     *   'POINTER (',I3,') FOR STATION ',A,'.')
1090  FORMAT ('0*** NOTE - ONE OR MORE NETWORK INDICATORS HAVE BEEN ',
     *   'SET. ',
     *   'OPTION SET TO RUN NETWORK COMMAND.')
1100  FORMAT (' NDELT=',I2,3X,'NGPS=',I2,3X,
     *   'GPS(1...NGPS)=',5(A4,1X))
1110  FORMAT ('0*** NOTE - STATION ',A,' NOT SUCCESSFULLY READ ',
     *   'TO OBTAIN THOSE PARAMETERS NOT REDEFINED.')
1120  FORMAT (' *** NOTE - ',A,' IDENTIFIER ',A,' ',
     *   'FOUND IN OLD DEFINITION BUT NOT IN NEW AND WILL BE DELETED.')
1130  FORMAT (' *** NOTE - ',A,' ',
     *   'FOUND IN OLD DEFINITION BUT NOT IN NEW AND WILL BE DELETED.')
1140  FORMAT ('0*** NOTE - STATION ',A,' NOT SUCCESSFULLY CREATED ',
     *   'IN ALL DATA ENTRY CONTROL FILES BECAUSE ERRORS ENCOUNTERED.')
1150  FORMAT ('0*** NOTE - STATION ',A,' NOT SUCCESSFULLY CHANGED ',
     *   'IN ALL DATA ENTRY CONTROL FILES BECAUSE ERRORS ENCOUNTERED.')
1160  FORMAT ('0*** NOTE - STATION ',A,' NOT CREATED IN ',
     *   'PREPROCESSOR DATA BASE BECAUSE ERRORS ENCOUNTERED.')
1170  FORMAT ('0*** NOTE - STATION ',A,' NOT CHANGED IN ',
     *   'PREPROCESSOR DATA BASE BECAUSE ERRORS ENCOUNTERED.')
1180  FORMAT (' STSTAN=',A4,3X,'ICSTANO=',I2,3X,'ISSTAN=',I2,3X,
     *   'ICSTAN=',I2,3X,'INDCMP=',I2)
1190  FORMAT ('0*** ERROR - STATION GENERAL PARAMETERS ',
     *   'FOR STATION ',A,' NOT SUCCESSFULLY UPDATED.')
1210  FORMAT ('0',132('#'))
1220  FORMAT ('0*** NOTE - NO STATIONS DEFINED OR REDEFINED.')
1230  FORMAT ('0*** NOTE - ',I4,' STATIONS SUCCESSFULLY DEFINED. ',
     *   I4,' STATIONS SUCCESSFULLY REDEFINED.')
1240  FORMAT (T13,I4,' STATIONS HAVE ',A,' DATA')
1250  FORMAT ('0*** NOTE - ',I4,' STATIONS HAD ',A,'.')
1260  FORMAT ('0*** NOTE - ',I4,' TOTAL ',A,' ENCOUNTERED BY ',
     *   'DEFINE STATION COMMAND.')
C
      END
