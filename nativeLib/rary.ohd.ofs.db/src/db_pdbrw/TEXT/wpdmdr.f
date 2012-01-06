C MEMBER WPDMDR
C  (from old member PDBRWMDR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBRWMDR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE WPDMDR(JDAY,IPER,NVALS,LDATA,MDATA,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  WPDMDR                                         *
C                                                                      *
C 8-5-83 CHANGE TO CHECK FOR RANGE BY CALLING PDCKVL
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  2-9-83                                         *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE WRITES 6-HOUR SUMS OF MDR DATA FOR THE ENTIRE     *
C    RFC AREA FOR A SPECIFIED 6-HOUR PERIOD.                           *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C        JDAY     I     I     1     JULIAN DAY OF DATA TO BE WRITTEN   *
C        IPER     I     I     1     PERIOD TO BE WRITTEN               *
C        NVALS    I     I     1     NUMBER OF VALUES TO BE WRITTEN     *
C        LDATA    I     I     1     LENGTH OF DATA ARRAY(I*2 WRDS)     *
C        MDATA    I     I     1     ARRAY CONTAINING 6 HR SUMS         *
C        ISTAT    I     O     1     STATUS INDICATOR                   *
C                                      0 = OK,NORMAL RETURN            *
C                                      1 = # OF VALUES GT SPACE ON PPDB*
C                                      2 = OUTSIDE OF PERIOD (NO WRITE)*
C                                      3 = DAIOIT WRITE ERROR          *
C                                      30+ = OUT OF RANGE
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdbdta'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER*2 MDRBUF(32)
      DIMENSION MDR(2)
      INTEGER*2 MDATA(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdmdr.f,v $
     . $',                                                             '
     .$Id: wpdmdr.f,v 1.1 1995/09/17 18:44:58 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA MDR/4hMDR6,4hDATA/
C***********************************************************************
C
C
      LRCP = LRCPDD * 2
C
      ISTAT = 0
      ISAV = 0
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,100)
  100 FORMAT(' SUBROUTINE WPDMDR ENTERED')
C
C  CHECK IF DATA TYPE IS LEGAL
C
      IX = IPDCKD(MDR(1))
      IDF = IDDTDR(4,IX)
C
C  CHECK IF DATE IS WITHIN RETENTION PERIOD
C
      IF(IDDTDR(8,1).NE.0.OR.IDDTDR(9,1).NE.0) GO TO 150
C
C  FILES ARE EMPTY MUST RE-INITIALIZE
C
      CALL PDDAY1(JDAY)
  150 CONTINUE
      CALL UMEMOV(IDDTDR(8,IX),IEDATE,1)
      CALL UMEMOV(IDDTDR(11,IX),LDATE,1)
      IF(JDAY.GE.IEDATE.AND.JDAY.LE.LDATE + 1) GO TO 200
      ISTAT = 2
      GO TO 999
  200 CONTINUE
C
C  CREATE NEW DAYS' DATA
C
      IF(JDAY.GE.IEDATE.AND.JDAY.LE.LDATE) GO TO 300
      CALL PDNEWD(JDAY,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
C
C  RESET EARLIEST DAY WHEN A NEW DAY IS CREATED
C
      CALL UMEMOV(IDDTDR(8,IX),IEDATE,1)
  300 CONTINUE
C
C  FIND WHERE THE RECORD FOR THIS PERIOD BEGINS
C
      IF(NVALS.GT.IDDTDR(16,IX)) GO TO 900
      IF(IDDTDR(16,IX).EQ.0) GO TO 925
C
C  CHECK IF PERIOD IS VALID
C
      IF(IPER.LT.1.OR.IPER.GT.4) GO TO 925
      IF(NVALS.GT.IDDTDR(17,IX)) IDDTDR(17,IX) = NVALS
      IF(IDDTDR(19,IX).EQ.0) IDDTDR(19,IX) = IDDTDR(16,IX) * 4
      IOFF = (IDDTDR(16,IX)) * (IPER - 1) + 1
      NREC = IUNRCD(IOFF,LRCP)
      CALL PDCREC(JDAY,IEDATE,IX,IREC)
      IF(IPDDB.GT.0) WRITE(IOGDB,305) NREC,IREC
  305 FORMAT(' NREC,IREC= ',2I6)
      IREC = IREC + NREC - 1
      IF(IPDDB.EQ.1) WRITE(IOGDB,310) IREC,IOFF
  310 FORMAT(' THIS PERIOD BEGINS IN RECORD ',I4,' IOFF= ',I4)
C
C  READ RECORD INTO BUFFER
C
      CALL UREADT(KPDDDF(IDF),IREC,MDRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
C
C  FIND WHERE THIS PERIOD BEGINS IN THE RECORD
C
      IOFF2 = IOFF - (NREC - 1) * LRCP
      IF(IPDDB.EQ.1) WRITE(IOGDB,325) IOFF2,IREC
  325 FORMAT(' THIS PERIOD BEGINS AT ',I4,' IN RECORD ',I4)
C
C  CALCULATE HOW MANY VALUES WILL BE MOVED INTO RECORD
C
      NUMOV = LRCP - IOFF2 + 1
      IF(NVALS.LT.NUMOV) NUMOV = NVALS
      IF(NVALS.GT.IDDTDR(16,IX)) GO TO 900
  350 CONTINUE
C
C  UPDATE AND WRITE THE RECORD
C
      DO 400 I = 1,NUMOV
       VAL = MDATA(I)
       CALL PDCKVL(MDR,I,IDDTDR(2,IX),VAL,ISTAT)
       IF(ISTAT.EQ.0) GO TO 380
       ISAV = ISTAT
       MDRBUF(IOFF2) = MISSNG
       GO TO 390
  380  MDRBUF(IOFF2) = MDATA(I)
  390  IOFF2 = IOFF2 + 1
  400 CONTINUE
  450 CONTINUE
      CALL UWRITT(KPDDDF(IDF),IREC,MDRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
  475 CONTINUE
C
C  SEE IF ANY MORE VALUES FOR UPDATE
C
      MORE = NVALS - NUMOV
      IF(IPDDB.EQ.1) WRITE(IOGDB,495) MORE
  495 FORMAT(' REMAINING VALUES = ',I4)
      IF(MORE.EQ.0) GO TO 950
      IF(MORE.LT.LRCP) GO TO 600
C
C  MORE THAN ONE RECORD TO PROCESS
C
      MORE = LRCP
      IOFF2 = 1
      IREC = IREC + 1
      DO 500 I = 1,MORE
       VAL = MDATA(NUMOV + I)
       CALL PDCKVL(MDR,I,IDDTDR(2,IX),VAL,ISTAT)
       IF(ISTAT.EQ.0) GO TO 480
       ISAV = ISTAT
       MDRBUF(IOFF2) = MISSNG
       GO TO 490
  480  MDRBUF(IOFF2) = MDATA(NUMOV + I)
  490  IOFF2 = IOFF2 + 1
  500 CONTINUE
      NUMOV = NUMOV + MORE
      CALL UWRITT(KPDDDF(IDF),IREC,MDRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      GO TO 475
  600 CONTINUE
C
C  PARTIAL RECORD REMAINING
C
      IOFF2 = 1
      IREC = IREC + 1
      CALL UREADT(KPDDDF(IDF),IREC,MDRBUF,ISTAT)
C
C  COMPLETE THE UPDATE
C
      DO 650 I = 1,MORE
          VAL=MDATA(NUMOV + I)
          CALL PDCKVL(MDR,I,IDDTDR(2,IX),VAL,ISTAT)
          IF(ISTAT.EQ.0) GO TO 630
          ISAV = ISTAT
          MDRBUF(IOFF2)=MISSNG
          GO TO 640
  630     MDRBUF(IOFF2) = MDATA(NUMOV + I)
  640     IOFF2 = IOFF2 + 1
  650 CONTINUE
      CALL UWRITT(KPDDDF(IDF),IREC,MDRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      GO TO 950
  900 CONTINUE
C
C  NOT ENOUGH SPACE ON PPDB
C
      ISTAT = 1
      GO TO 950
  925 CONTINUE
C
C  OUT OF RANGE
C
      ISTAT = 2
  950 CONTINUE
C
C  DEBUG AND RETURN
C
      ISTAT = ISTAT + ISAV
      IF(IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE(IOGDB,975) ISTAT
  975 FORMAT(' SUBROUTINE WPDMDR EXECUTED. ISTAT = ',I2)
      GO TO 999
  990 CONTINUE
C
C  SYSTEM ERROR
C
      ISTAT = 3
  999 CONTINUE
      RETURN
      END
