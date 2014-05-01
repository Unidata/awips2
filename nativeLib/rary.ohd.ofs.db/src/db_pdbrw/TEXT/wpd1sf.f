C MODULE WPD1SF
C-----------------------------------------------------------------------
C
      SUBROUTINE WPD1SF (ISTAID,ISTATP,NTYPES,IDATYP,IUNITS,IFHOUR,
     *   LHOUR,LDATA,DATA,LENDAT,IDATES,IWRITE,IREV,ISTAT)
C
C  THIS ROUTINE WRITES FORECAST DATA FOR DATA TYPES OTHER THAN RRS, 
C  MDR6 AND PPSR TO THE PPDB FOR ONE STATION.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM    DESCRIPTION
C       ------   ----   ---   ---    -----------
C       ISTAID    A8     I     2     STATION IDENTIFIER
C                 I      I     1     STATION NUMBER
C       ISTATP    I      I     1     IDENTIFIER TYPE:
C                                      0=ISTAID IS CHARACTER
C                                      1=ISTAID IS INTEGER
C       NDTYPE    I      I     1     NUMBER OF DATA TYPES
C       IDATYP    A4     I   NDTYPE  DATA TYPES
C       IUNITS    A4     I   NDTYPE  UNITS FOR EACH TYPE
C       IFHOUR    I      I     1     JULIAN HOUR OF FIRST DATA
C       LHOUR     I      I     1     JULIAN HOUR OF LAST DATA
C       LDATA     I      I     1     LENGTH OF DATA ARRAY
C       DATA      R      I   LDATA   DATA ALL DAYS FOR EACH TYPE
C       IWRITE    I      O   NDTYPE  WRITE STATUS FOR EACH TYPE:
C                                      0=WRITTEN
C                                      1=TYPE NOT FOUND
C                                      2=PERIOD NOT VALID
C                                      3=INVALID UNITS
C                                      4=INVALID REV FLAG
C                                      5=INVALID VALUE
C       IREV      I      I     1     REVISION FLAG:
C                                      0=NEW DATA
C                                      1=REV
C       ISTAT     I      O     1     STATUS:
C                                      0=OKAY
C                                      1=ID NOT FOUND
C                                      2=ONE OR MORE TYPES NOT FOUND
C                                      3=ONE OR MORE PERIODS NOT VALID
C                                      4=INVALID UNITS
C                                      5=2 AND 3 COMBINATION
C                                      6=2 AND 4 COMBINATION
C                                      7=3 AND 4 COMBINATION
C                                      8=2, 3 AND 4 COMBINATION
C                                      9=NOT VALID DATA TYPE
C                                     10=SYSTEM ERROR
C                                     11=NOT ENOUGH DATA IN PERIOD
C                                     20+=INVALID REVISION FLAG
C                                     30+=INVALID VALUE
C
      CHARACTER*4 DTYPE,IUNITS(1),IDATYP(1)
      CHARACTER*4 IUWANT/'DEGF'/
      CHARACTER*8 ISTAID
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIRAY(LSIBUF)
      INTEGER*2 IDTREC(32)
C
      DIMENSION DATA(1)
      DIMENSION IDATES(*),IWRITE(*)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpd1sf.f,v $
     . $',                                                             '
     .$Id: wpd1sf.f,v 1.3 2001/06/13 12:57:00 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER WPD1SF'
C
      ISTAT=0
C
      CALL UMEMST (0,IWRITE,NTYPES)
      LRCPD2=LRCPDD*2
C
C  CHECK IF STATION EXISTS
      IFIND=0
      IF (ISTATP.EQ.0) THEN
         CALL PDFNDR (ISTAID,LSIBUF,IFIND,ISIREC,ISIRAY,IFREE,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         ENDIF
      IF (ISTATP.EQ.1) THEN
         CALL PDFNDI (ISTAID,LSIBUF,IFIND,ISIREC,ISIRAY,IFREE,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         ENDIF
      IF (IFIND.EQ.0) THEN
         ISTAT=1
         GO TO 270
         ENDIF
C
C  CHECK NUMBER OF DATA TYPES
      IF (NTYPES.LE.0) THEN
         ISTAT=2
         GO TO 270
         ENDIF
C
C  FIND OLDEST DATE ON DAILY FILES - USE TM24 AS THE DATA TYPE
      ITX=IPDCKD('TM24')
      CALL UMEMOV (IDDTDR(2,ITX),DTYPE,1)
      CALL UMEMOV (IDDTDR(11,ITX),IOLDTE,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IOLDTE=',IOLDTE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      JDATA=1
C
C  PROCESS EACH DATA TYPE
      DO 230 IT=1,NTYPES
C     CHECK FOR VALID DAILY DATA TYPE
         IF (IDATYP(IT).EQ.'TF24'.OR.
     *       IDATYP(IT).EQ.'TFMN'.OR.
     *       IDATYP(IT).EQ.'TFMX') THEN
            ITX=IPDCKD(IDATYP(IT))
            IF (ITX.EQ.0) THEN
               IWRITE(IT)=1
               GO TO 230
               ENDIF
            ELSE
               IWRITE(IT)=1
               GO TO 230
            ENDIF
C     CHANGE HOURS TO DAYS
         LDAY=((LHOUR-NHOPDB+23)/24)+1
         IFDAY=((IFHOUR-NHOPDB+23)/24)+1
         IF (MOD((LHOUR-NHOPDB),24).NE.0) GO TO 40
            IF (MOD((IFHOUR-NHOPDB),24).EQ.0) GO TO 50
40          IF (IPDDB.GT.0) WRITE (IOGDB,300) LHOUR,IFHOUR,NHOPDB
            IWRITE(IT)=2
            GO TO 230
C     IFIXDT IS AMOUNT TO SKIP FOR EACH DATA WORD IF DOING ONLY MAX OR 
C     MIN
50       IFIXDT=1
         NDTWDS=IDDTDR(6,ITX)
         CALL UMEMOV (IDDTDR(2,ITX),JDDTDR,1)
         IPOINT=IPDFDT(ISIRAY,JDDTDR)
C     CHECK IF STATION HAS DATA TYPE
         IF (IPOINT.NE.0) GO TO 60
            IWRITE(IT)=1
            GO TO 230
C     CHECK IF ONLY A MAX OR ONLY A MIN
60       IIXX=IPDCDW(IDATYP(IT))
         IF (IDDTDR(4,IIXX).GT.0) GO TO 70
C        CORRECT POINTER AND SKIP VALUE
            IPOINT=IPOINT+IDDTDR(5,IIXX)
            NDTWDS=IDDTDR(6,IIXX)
C     FOUND POINTER NOW CHECK DATES
70       IF (IPDDB.GT.0) WRITE (IOGDB,310) (IDDTDR(K,ITX),K=1,24)
C     CHECK IF NO DATA
         IF (IDDTDR(8,ITX).NE.0.OR.IDDTDR(9,ITX).NE.0) GO TO 80
C        SET DATES OF FIRST AND LAST DAY OF DATA
            CALL UMEMOV (IFDAY,IDDTDR(8,ITX),1)
            CALL UMEMOV (LDAY,IDDTDR(11,ITX),1)
            IDDTDR(10,ITX)=IDDTDR(15,ITX)
80       CALL UMEMOV (IDDTDR(8,ITX),IEDATE,1)
         CALL UMEMOV (IDDTDR(11,ITX),LDATE,1)
C     CHECK IF FIRST DAY AFTER OLDEST DATE OF REGULAR DATA PLUS MAXIMUM 
C     DIFFERENCE BETWEEN LAST OBSERVED DATA DAY AND FIRST DAY OF FUTURE 
C     DATA AND AFTER LAST DAY PLUS 1
         IF (IFDAY.GT.IOLDTE+MXDDOD.AND.IFDAY.GT.LDATE+1) GO TO 90
C     CHECK IF FIRST DAY IS GREATER THAN OLDEST REGULAR AND LESS THAN 
C     OLDEST DATE OF REGULAR PLUS MAXIMUM DAYS
         IF (IFDAY.GE.IOLDTE.AND.
     *       IFDAY.LE.IOLDTE+IDDTDR(7,ITX)) GO TO 100
C        FIRST DATE IS TOO EARLY
90          IF (IPDDB.GT.0) WRITE (IOGDB,320) IFDAY,IOLDTE,MXDDOD,
     *         ITX,IDDTDR(7,ITX)
            IWRITE(IT)=2
            GO TO 230
C     SET UP WORDS AND RECORDS FOR WRITE
100      NREC1D=IDDTDR(21,ITX)
         IFILE=IDDTDR(4,ITX)
         NDAYS=LDAY-IFDAY+1
         JEND=JDATA-1+(NDAYS*NDTWDS)
C     MAKE SURE ENOUGH DATA
         IF (JEND.GT.LDATA) THEN
            ISTAT=11
            GO TO 270
            ENDIF
         IF (IPDDB.GT.0) WRITE (IOGDB,340) NREC1D,IFILE,
     *      NDTWDS,IEDATE,LDATE
C     GET UNITS FOR CONVERSION
         CALL UDUCNV (IUNITS(IT),IUWANT,2,1,FACTOR,TFACT,ISTAT)
         IF (ISTAT.EQ.0) GO TO 110
            IWRITE(IT)=3
            JDATA=JEND+1
            GO TO 230
110      IRCOFS=IUNRCD(IPOINT,LRCPD2)
C     READ IN DATE ARRAY
         CALL PDGFUD (IDATES,LENDAT,ITX,IRECD,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         NDTS=IDATES(1)
         IDAY=IFDAY
C     SEE IF ANY OTHER LATER DATES MUST BE RESET TO MISSING
C     IF WRITING A DAY BEFORE LAST AND REVISION IS 0, RESET ANY DAYS
C     AFTER THE CURRENT DAY
         IF (IDAY.LT.LDATE.AND.IREV.EQ.0) THEN
            CALL PDRSTF (IDAY,IDATES,IPOINT,IRCOFS,IFILE,
     *         NDTWDS,LDATE,ISTAT)
            IF (ISTAT.NE.0) GO TO 270
            ENDIF
         IWRDTF=0
C     SEE IF DATE IS ALREADY THERE
120      IF (NDTS.EQ.0) GO TO 150
         CALL PDFNDD (IDAY,IDATES,IDTXX)
         IF (IDTXX.NE.0) GO TO 170
C     DATE NOT FOUND
         IF (NDTS.LT.IDDTDR(7,ITX)) GO TO 150
C     MUST REPLACE OLD DATE - FIND EARLIEST JULIAN DATE
         CALL PDFEFD (IDATES,IDTXX)
C     MAKE SURE NOT REPLACING FIRST FUTURE DAY AFTER OBSERVED DAY
         IF (IDATES(IDTXX).LE.IOLDTE) GO TO 140
            IF (IPDDB.GT.0) WRITE (IOGDB,330) IDTXX,IDATES(IDTXX),
     *         IOLDTE
            IWRITE(IT)=2
            GO TO 230
140      IDATES(IDTXX)=IDAY
         IDREC=IDATES(IDTXX+1)
C     RESET EARLIEST DATE
         CALL PDFEFD (IDATES,IDD)
         IEDATE=IDATES(IDD)
         CALL UMEMOV (IEDATE,IDDTDR(8,ITX),1)
         CALL PDSET0 (DTYPE,IDREC,NREC1D,IFILE,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         GO TO 160
C     DATE NOT THERE AND THERE IS ROOM WITHOUT REPLACING
150      IDREC=NDTS*NREC1D+IDDTDR(15,ITX)
         IDTXX=2*NDTS+2
         NDTS=NDTS+1
         IDATES(IDTXX)=IDAY
         IDATES(IDTXX+1)=IDREC
         IDATES(1)=NDTS
C     SET REWRITE DATES FLAG
160      IWRDTF=1
         GO TO 180
C     DATE IS THERE GET RECORD
170      IDREC=IDATES(IDTXX+1)
180      IF (IPDDB.GT.0) WRITE (IOGDB,310) (IDDTDR(K,ITX),K=1,24)
C     CALCULATE RECORD NUMBER
         IRCRED=IDREC+IRCOFS-1
         IDX=IPOINT-(IRCOFS-1)*LRCPD2
         IF (IPDDB.GT.0) WRITE (IOGDB,350) IDREC,IRCOFS,IPOINT,IRCRED,
     *      IDX
C     READ DATA RECORD
         CALL UREADT (KPDDDF(IFILE),IRCRED,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
C     MOVE IN THE DATA - CONVERT UNITS FIRST IF NECESSARY
         I=1
200      RMAGNI=10.0
         CALL PDCVDD (DATA,IFIXDT,RMAGNI,IUNITS(IT),IUWANT,JDATA,JEND,
     *      IDTREC(IDX),1,IDDTDR(2,ITX),ISTAID,ISTA)
         IF (ISTA.EQ.30) IWRITE(IT)=5
         IF (ISTA.EQ.20) IWRITE(IT)=4
         IF (I.EQ.NDTWDS) GO TO 210
            I=I+1
            IDX=IDX+1
            GO TO 200
C     WRITE RECORD
210      CALL UWRITT (KPDDDF(IFILE),IRCRED,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         IF (IDAY.EQ.LDAY) GO TO 220
            IDAY=IDAY+1
            GO TO 120
220      IF (IWRDTF.EQ.0) GO TO 230
         N=NDTS*2+1
         NREC=IUNRCD(N,LRCPDD)
         CALL WVLRCD (KPDDDF(IFILE),IRECD,NREC,IDATES,LRCPDD,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
C     UPDATE LATEST DATE
         IF (LDAY.GT.LDATE) CALL UMEMOV (LDAY,IDDTDR(11,ITX),1)
230      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK STATUS
      CALL PDWRST (IWRITE,NTYPES,ISTAT)
      GO TO 270
C
C  READ/WRITE ERROR
240   ISTAT=4
      GO TO 270
C
270   IF (IPDTR.GT.0) WRITE (IOGDB,360) (ISIRAY(K),K=2,5),ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
300   FORMAT (' LHOUR=',I6,3X,'IFHOUR=',I6,3X,'NHOPDB=',I2)
310   FORMAT (' IDDTDR=',I3,2A2,21I5)
320   FORMAT (' IFDAY=',I6,3X,'IOLDTE=',I6,3X,'MXDDOD=',I2,3X,
     *   'ITX=',I2,3X,'IDDTDR(7,ITX)=',I3)
330   FORMAT (' IDTXX=',I2,3X,'IDATES(IDTXX)=',I6,3X,'IOLDTE=',I6)
340   FORMAT (' NREC1D=',I2,3X,'IFILE=',I2,3X,'NDTWDS=',I2,3X,
     *   'IEDATE=',I6,3X,'LDATE=',I6)
350   FORMAT (' IDREC=',I4,3X,'IRCOFS=',I2,3X,'IPOINT=',I2,3X,
     *   'IRCRED=',I2,3X,'IDX=',I2)
360   FORMAT (' EXIT WPD1SF - STAID=',4A2,3X,'ISTAT=',I2)
C
      END
