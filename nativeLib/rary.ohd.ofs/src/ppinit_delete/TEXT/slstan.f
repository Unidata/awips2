C MODULE SLSTAN
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE ALL STATION PARAMETERS.
C
      SUBROUTINE SLSTAN (LARRAY,ARRAY,NFLD,IRUNCK,
     *   ZCKREF,XCKREF,ICKREF,ZRNTWK,XRNTWK,IRNTWK,ISTAT)
C
      CHARACTER*(*) ZCKREF,ZRNTWK,XCKREF,XRNTWK
      CHARACTER*4 DISP,RDISP,WDISP
      CHARACTER*4 TYPE,RTYPE,UNITS,SORTBYX
      CHARACTER*4 SAID
      CHARACTER*8 SMID,GOESID
      CHARACTER*8 TYPERR
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      DIMENSION ARRAY(LARRAY)

      DIMENSION UNUSED(10)
      DIMENSION ISWRK2(1)
C
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimtemp'
      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwfx'
      INCLUDE 'scommon/sntwkx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
C
      EQUIVALENCE (ISWRK2(1),SWRK2(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slstan.f,v $
     . $',                                                             '
     .$Id: slstan.f,v 1.6 2001/06/13 14:02:31 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLSTAN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'LARRAY=',LARRAY,
     *      'NFLD=',NFLD,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      ISTRT=-1
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NDELTE=0
      ISRNCK=IRUNCK
      UNSD=-999.
      NPRSTA=4
      NUMSTA=0
      NUMEXC=0
      LCKREF=0
      LRNTWK=0
C
C  SET INDICATOR HOW INCOMPLETE STATION TO BE PROCESSED
C     0 = DO NOT DELETE GROUPS IF INCOMPLETE
C     1 = DELETE GROUPS IF INCOMPLETE
      INDCMP=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF NETWORK FLAG COMMON BLOCK FILLED
C
      IF (INFFIL.EQ.0) THEN
         DISP='OLD'
         CALL SUGTNF (LARRAY,ARRAY,DISP,NUMERR,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,210)
            CALL SULINE (LP,2)
            ISRNCK=1
            GO TO 10
            ENDIF
         ENDIF
C
C  PRINT NETWORK PARAMETERS
       CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,140) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 90
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 90
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,150) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
C
      IF (NUMFLD.EQ.1) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
C     CHECK PARAMETER TYPE
         IF (CHK.NE.'ALL') THEN
            WRITE (LP,120) CHK(1:LENSTR(CHK))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 90
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
      ICKRGN=0
      ICKCPU=1
      MINRGN=0
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
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CALL SUBSTR (CHAR,1,8,STAID,1)
C
C  STORE STATION NAME AND PAGE NUMBER
      CALL SUIDF1 (STAID,NUMSTA,NUMEXC,NPRSTA,LSWRK2,ISWRK2,IERR)
      IUIDF1=0
      IF (IERR.GT.0) IUIDF1=1
C
C  READ STATION GENERAL PARAMETERS
      IPTR=0
      IPRERR=0
      RDISP='OLD'
      INCLUDE 'scommon/callsrstan'
      IF (IERR.NE.0) THEN
         WRITE (LP,170) STAID
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 80
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         IPRNT=1
         UNITS='ENGL'
         INCLUDE 'scommon/callspstan'
         ENDIF
C
C  CHECK IF STATION STATUS IS INCOMPLETE
      IF (ICSTAN.NE.0) THEN
         WRITE (LP,180) STAID
         CALL SULINE (LP,2)
         IF (INDCMP.EQ.0) GO TO 50
         ENDIF
C
C  CHECK IF ANY DATA GROUPS DEFINED
      IF (NGPS.EQ.0) THEN
         WRITE (LP,190) STAID
         CALL SULINE (LP,2)
         GO TO 50
         ENDIF
C
C  CHECK WHICH DATA GROUPS DEFINED
      IPCPN=0
      ITEMP=0
      IPE=0
      IRRS=0
      DO 20 I=1,NGPS
         CALL SUBSTR (GPS(I),1,4,TYPE,1)
         IF (TYPE.EQ.'PCPN') IPCPN=1
         IF (TYPE.EQ.'TEMP') ITEMP=1
         IF (TYPE.EQ.'PE') IPE=1
         IF (TYPE.EQ.'RRS') IRRS=1
20       CONTINUE
C
      WRITE (LP,220)
      CALL SULINE (LP,1)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IFIRST=1
      IPRERR=1
      INDERR=0
      NUMERR=0
      NUMWRN=0
      LARAY2=1000
      LARAY1=LARRAY-LARAY2
      LARAY3=0
C
C  -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  CHECK IF CHECKS TO BE MADE IF STATION REFERENCED
C
      IF (ICKREF.LE.0) THEN
         IF (LCKREF.EQ.0) THEN
            IF (ICKREF.EQ.-1) THEN
               WRITE (LP,200) '.'
               CALL SULINE (LP,2)
               ENDIF
            IF (ICKREF.EQ.0) THEN
               WRITE (LP,200) ' BECAUSE ',ZCKREF(1:LENSTR(ZCKREF)),
     *            '(',XCKREF(1:LENSTR(XCKREF)),') SPECIFIED.'
               CALL SULINE (LP,2)
               ENDIF
            LCKREF=1
            ENDIF
         ENDIF
C
      IF (ICKREF.EQ.1) THEN
C     CHECK PCPN PARAMETERS
         IF (IPCPN.EQ.1) THEN
            TYPE='PCPN'
            CALL SLCHK (TYPE,STAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *         LARAY3,ARAY3,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *         IERR)
            IF (IERR.NE.0) INDERR=1
            ENDIF
C     CHECK TEMP PARAMETERS
         IF (ITEMP.EQ.1) THEN
            TYPE='TEMP'
            CALL SLCHK (TYPE,STAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *         LARAY3,ARAY3,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *         IERR)
            IF (IERR.NE.0) INDERR=1
            ENDIF
C     CHECK PE PARAMETERS
         IF (IPE.EQ.1) THEN
            TYPE='PE'
            CALL SLCHK (TYPE,STAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *         LARAY3,ARAY3,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *         IERR)
            IF (IERR.NE.0) INDERR=1
            ENDIF
C     CHECK RRS PARAMETERS
         IF (IRRS.EQ.1) THEN
            TYPE='RRS'
            IPTR=0
            IREAD=1
            INCLUDE 'scommon/callsrrrs'
            IF (IERR.NE.0) THEN
               WRITE (LP,240) TYPE,STAID
               CALL SUWRNS (LP,2,NUMWRN)
               GO TO 50
               ENDIF
            IF (LDEBUG.GT.0) THEN
               INCLUDE 'scommon/callsprrs'
               ENDIF
            DO 30 I=1,NRRSTP
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,250) STAID,IRTIME(I),
     *               RRSTYP(I),URMISS(I)
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (IRTIME(I).GT.0) THEN
                  RTYPE=RRSTYP(I)
                  IF (URMISS(I).NE.'SAME') RTYPE=URMISS(I)
                  CALL SLCHK (RTYPE,STAID,
     *               LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),LSWORK,SWORK,
     *               IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *               IERR)
                  IF (IERR.NE.0) INDERR=1
                  ENDIF
30             CONTINUE
            ENDIF
         ENDIF
C
C  -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  CHECK IF RUNCHECK OPTION SPEFICIED
      IF (ISRNCK.EQ.1) GO TO 80
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0.OR.INDERR.EQ.1) THEN
         WRITE (LP,230) STAID
         GO TO 50
         ENDIF
C
C  SET INDICATOR FOR DELETE ROUTINES TO NOT READ INPUT FIELDS
      INDFLD=-2
C
      WRITE (LP,*)
      CALL SULINE (LP,1)      
C
C  DELETE PCPN PARAMETERS
      IF (IPCPN.EQ.1) THEN
         TYPE='PCPN'
         CALL SLPPP (INDFLD,STAID,TYPE,NUMERR,NUMWRN,LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) INDERR=1
         ENDIF
C
C  DELETE TEMP PARAMETERS
      IF (ITEMP.EQ.1) THEN
C     READ TEMP PARAMETER TO GET FORECAST MAX/MIN INDICATOR
         TYPE='TEMP'
         IPTR=0
         IREAD=1
         INCLUDE 'scommon/callsrtemp'
         IF (IERR.NE.0) THEN
            WRITE (LP,240) TYPE,STAID
            CALL SUWRNS (LP,2,NUMWRN)
            ELSE
               IF (LDEBUG.GT.0) THEN
                  LEVEL=1
                  INCLUDE 'scommon/callsptemp'
                  ENDIF
               CALL SLPPP (INDFLD,STAID,TYPE,NUMERR,NUMWRN,LARRAY,ARRAY,
     *            IERR)
               IF (IERR.NE.0) INDERR=1
            ENDIF
         ENDIF
C
C  DELETE PE PARAMETERS
      IF (IPE.EQ.1) THEN
         TYPE='PE'
         CALL SLPPP (INDFLD,STAID,TYPE,NUMERR,NUMWRN,LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) INDERR=1
         ENDIF
C
C  DELETE RRS PARAMETERS AND TIME SERIES
      IF (IRRS.EQ.1) THEN
         TYPE='RRS'
         IPTR=0
         IREAD=1
         INCLUDE 'scommon/callsrrrs'
         IF (IERR.NE.0) THEN
            WRITE (LP,240) TYPE,STAID
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 50
            ENDIF
         IF (LDEBUG.GT.0) THEN
            INCLUDE 'scommon/callsprrs'
            ENDIF
C     DELETE RRS TIME SERIES
         DO 40 I=1,NRRSTP
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,250) STAID,IRTIME(I),
     *            RRSTYP(I),URMISS(I)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IRTIME(I).GT.0) THEN
               RTYPE=RRSTYP(I)
               IF (URMISS(I).NE.'SAME') RTYPE=URMISS(I)
               CALL SLPRD (INDFLD,STAID,RTYPE,IERR)
               ENDIF
40          CONTINUE
         CALL SLPPP (INDFLD,STAID,TYPE,NUMERR,NUMWRN,LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) INDERR=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    IF (NUMERR.GT.0.OR.INDERR.GT.0) GO TO 80
C
C  DELETE STATION FROM PREPROCESSOR DATA BASE
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) GO TO 80
      IDTYPE=0
      CALL SLPPD (INDFLD,STAID,IDTYPE,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NSRCCD.EQ.0) GO TO 70
C
C  DELETE STATION FROM GOES AND/OR SASM CONTROL FILE
      SAID=' '
      SMID=' '
      TYPERR='WARNING'
      DO 60 I=1,NSRCCD
         IF (SRCCD(I).EQ.'GHB5'.OR.
     *       SRCCD(I).EQ.'GPLT'.OR.
     *       SRCCD(I).EQ.'CDAS') THEN
            GOESID=' '
            CALL SUBSTR (SRCID(1,I),1,8,GOESID,1)
            CALL SLGOES (INDFLD,STAID,SRCCD(I),GOESID,TYPERR,IERR)
            ENDIF
         IF (SRCCD(I).EQ.'SA') CALL SUBSTR (SRCID(1,I),1,4,SAID,1)
         IF (SRCCD(I).EQ.'SM') CALL SUBSTR (SRCID(1,I),1,8,SMID,1)
60       CONTINUE
      IF (SAID.NE.' '.OR.SMID.NE.' ') THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,260) STAID,SAID,SMID
            CALL SULINE (IOSDBG,1)
            ENDIF
         CALL SLSASM (INDFLD,STAID,TYPERR,IERR)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE STAN PARAMTERS
70    TYPE='GENL'
      CALL SLPPP (INDFLD,STAID,TYPE,NUMERR,NUMWRN,LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 80
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270) STAID
         CALL SULINE (LP,1)
         ENDIF
C
C  SET NTWK INDICATORS
      IPTWGTI=1
      IPSORT=1
      ITSORT=1
      IESORT=1
      SORTBYX=' '
      IAREA=0
      ISORTBY=0
      CALL SNTWKI (IPCPN,ITPPVR,IPTWGTI,IPSORT,
     *   ITEMP,ITTAVR,ITFMM,ITSORT,
     *   IPE,IESORT,
     *   IRRS,
     *   NUGPA,
     *   SORTBYX,
     *   IAREA,
     *   ISORTBY,
     *   INTWKI,IERR)
      IF (IRNTWK.EQ.0) THEN
         IF (LRNTWK.EQ.0) THEN
            WRITE (LP,280) ZRNTWK(1:LENSTR(ZRNTWK)),
     *         XRNTWK(1:LENSTR(XRNTWK))
            CALL SULINE (LP,2)
            LRNTWK=1
            ENDIF
         ENDIF
      IF (IRNTWK.EQ.1) THEN
         IF (INTWKI.EQ.1.AND.INAUTO.EQ.0) THEN
            WRITE (LP,290)
            CALL SULINE (LP,2)
            INAUTO=1
            ENDIF
         ENDIF
      NDELTE=NDELTE+1
      GO TO 10
C
80    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'NUMWRN=',NUMWRN,
     *      'NUMERR=',NUMERR,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPOS=NUMSTA*NPRSTA
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) THEN
         IF (IUIDF1.EQ.0) CALL SUBSTR ('E',1,1,ISWRK2(IPOS),-1)
         ENDIF
C
C  CHECK IF WARNINGS ENCOUNTERED
      IF (NUMWRN.EQ.0) THEN
         IF (IUIDF1.EQ.0) CALL SUBSTR ('W',1,1,ISWRK2(IPOS),-3)
         ENDIF
C
      GO TO 10
C
C  CHECK NUMBER OF STATIONS DELETED
90    IF (NDELTE.EQ.0) THEN
         IF (ISRNCK.EQ.1) THEN
            WRITE (LP,300) ' BECAUSE RUNCHECK(YES) OPTION SPECIFIED.'
            CALL SULINE (LP,2)
            ELSE
               WRITE (LP,300) '.'
               CALL SULINE (LP,2)
            ENDIF
         GO TO 100
         ENDIF
C
      WRITE (LP,310) NDELTE
      CALL SULINE (LP,2)
C
C  PRINT STATION IDENTIFIER AND PAGE ON WHICH DELETE STARTS
      CALL SUIDF2 (NUMSTA,NUMEXC,LSWRK2,ISWRK2,NUMERR,NUMWRN,IERR)
C
C  UPDATE NTWK PARAMETERS
      WDISP='OLD'
      CALL SWNTWK (IVNTWK,UNSD,NNWFLG,INWFLG,INWDTE,
     *   LARRAY,ARRAY,WDISP,IERR)
C
C  PRINT NTWK PARAMETERS
      CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
C
100   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLSTAN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT ('0*** ERROR - IN SLSTAN - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
140   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
150   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
170   FORMAT ('0*** WARNING - STATION ',A,' IS NOT DEFINED.')
180   FORMAT ('0*** NOTE - STATION ',A,' IS INCOMPLETE.')
190   FORMAT ('0*** NOTE - STATION ',A,' HAS NO DATA GROUPS DEFINED.')
200   FORMAT ('0*** NOTE - NO CHECKS WILL BE MADE FOR ',
     *   'REFERENCES TO THIS STATION',9A)
210   FORMAT ('0*** NOTE - NTWK PARAMETERS NOT SUCCESSFULLY ',
     *   'READ. STATIONS CANNOT BE DELETED.')
220   FORMAT (' ')
230   FORMAT ('0*** WARNING - STATION ',A,' CANNOT BE DELETED ',
     *   'BECAUSE IT IS STILL REFERENCED.')
240   FORMAT ('0*** WARNING - ',A,' PARAMETERS FOR STATION ',A,
     *   'NOT SUCCESSFULLY READ.')
250   FORMAT (' STAID=',A,3X,'IRTIME=',I3,3X,'RRSTYP=',A,3X,
     *   'URMISS=',A)
260   FORMAT (' STAID=',A,3X,'SAID=',A,3X,'SMID=',A)
270   FORMAT (' STAN PARAMETERS DELETED FOR STATION ',A)
280   FORMAT ('0*** NOTE - ONE OR MORE NETWORK INDICATORS HAVE BEEN ',
     *   'SET BUT OPTION NOT SET TO RUN NETWORK COMMAND '
     *   'BECAUSE ',A,'(',A,') SPECIFIED.')
290   FORMAT ('0*** NOTE - ONE OR MORE NETWORK INDICATORS HAVE BEEN ',
     *   'SET. OPTION SET TO RUN NETWORK COMMAND.')
300   FORMAT ('0*** NOTE - NO STATIONS SUCCESSFULLY DELETED',A)
310   FORMAT ('0*** NOTE - ',I3,' STATIONS SUCCESSFULLY DELETED.')
C
      END
