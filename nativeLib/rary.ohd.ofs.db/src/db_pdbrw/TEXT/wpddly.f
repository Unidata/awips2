C MEMBER WPDDLY
C  (from old member PDWPDDLY)
C***********************************************************************
C                                                                      *
C         MEMBER PDWPDDLY                                              *
C                                                                      *
C***********************************************************************
C***********************************************************************
       SUBROUTINE WPDDLY(ITYPE,JDAY,LDATA,IDATA,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  WPDDLY                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE: 12-12-86                                        *
C                                                                      *
C              AUTHOR:  SONJA R. SIEGEL                                *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE WRITES ALL DATA OF A GIVEN DAILY DATA TYPE FOR    *
C    ONE DAY. THE DATA ARE IN THE EXACT FORM NEEDED FOR STORAGE ON THE *
C    PPDB. IT IS RESTRICTED TO TYPE APIG AND PG24 GRID POINTS          *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C         ITYPE    A4    I    1      DATA TYPE 'APIG' OR 'PG24'        *
C         JDAY    I*4    I    1      JULIAN DAY OF DATA TO BE WRITTEN  *
C         LDATA  I*4     I     1     NUMBER OF VALUES TO BE WRITTEN    *
C         IDATA  I*2     I  LDATA    ARRAY OF DATA VALUES FOR THE DAY  *
C         ISTAT    I     O     1     STATUS INDICATOR                  *
C                                       0 = OK                         *
C                                       1 = TOO MANY VALUES            *
C                                       2 = INVALID DATE              *
C                                       3 = SYSTEM ERROR ACCESSING FL*
C                                       4 = TYPE NOT DEFINED ON PPDBL*
C                                            OR NOT PG24 OR APIG
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
C
      INTEGER*2 IDATA(1),IWORK(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpddly.f,v $
     . $',                                                             '
     .$Id: wpddly.f,v 1.1 1995/09/17 18:44:56 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
      DATA LPG24/4hPG24/,LAPIG/4hAPIG/
C***********************************************************************
C
C
      ISTAT = 0
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,100)
  100 FORMAT(' SUBROUTINE WPDDLY ENTERED')
C
C  CHECK IF DATA TYPE IS LEGAL
C
      IF(ITYPE.NE.LPG24 .AND. ITYPE.NE.LAPIG) GO TO 190
      IX = IPDCKD(ITYPE)
      IF(IX.NE.0 .AND. IDDTDR(16,IX).GT.0) GO TO 300
  190 CONTINUE
      IF(IPDDB.EQ.1) WRITE(LPE,200) ITYPE
  200 FORMAT(' **ERROR** TYPE: ',A4,' NOT DEFINED ON PPDB')
      ISTAT = 4
      GO TO 999
  300 CONTINUE
C
      LRCP = LRCPDD * 2
C
      IDF = IDDTDR(4,IX)
C
C  CHECK IF DATE IS WITHIN RETENTION PERIOD
C
      DO 310 I = 1,NMDTYP
        IF(IDDTDR(8,I).NE.0.OR.IDDTDR(9,I).NE.0) GO TO 320
  310 CONTINUE
C
C  FILES ARE EMPTY MUST INITIALIZE
C
      CALL PDDAY1(JDAY)
  320 CONTINUE
      CALL UMEMOV(IDDTDR(8,IX),IEDATE,1)
      CALL UMEMOV(IDDTDR(11,IX),LDATE,1)
      IF(JDAY.GE.IEDATE.AND.JDAY.LE.LDATE + 1) GO TO 330
      ISTAT = 2
      GO TO 999
  330 CONTINUE
C
C  CREATE NEW DAYS' DATA
C
      IF(JDAY.GE.IEDATE.AND.JDAY.LE.LDATE) GO TO 340
      CALL PDNEWD(JDAY,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
C
C  RESET EARLIEST DAY WHEN A NEW DAY IS CREATED
C
      CALL UMEMOV(IDDTDR(8,IX),IEDATE,1)
  340 CONTINUE
C
C  FIND WHERE THE RECORD FOR THIS PERIOD BEGINS
C  CHECK NUMBER OF VALUES
C
      IF(LDATA.GT.IDDTDR(16,IX)-1) GO TO 900
      IF(LDATA.GT.IDDTDR(17,IX)) IDDTDR(17,IX) = LDATA
      IF(LDATA.GE.IDDTDR(19,IX)) IDDTDR(19,IX) = LDATA + 1
      CALL PDCREC(JDAY,IEDATE,IX,IREC)
      LSTREC = IDDTDR(21,IX) + IREC - 1
      IF(IPDDB.GT.0) WRITE(IOGDB,350) LSTREC,IREC
  350 FORMAT(' LSTREC,IREC= ',2I6)
C
C  SETUP FOR MOVING DATA INTO WORK BUFFER AND WRITING TO FILE
C DO 32 VALUES AT A TIME EXCEPT FOR FIRST WHEN DO 31 SINCE
C FIRST WORD WILL BE SET TO LDATA
C
      IWORK(1) = LDATA
      IBEG = 1
      NUM = IBEG + 30
      J = 2
360   CONTINUE
      IF(LDATA.LT.NUM) NUM = LDATA
      IF(IPDDB .GT. 0) WRITE(IOGDB,361) IBEG,NUM,J
  361 FORMAT(' IBEG,NUM,J=',3I6)
      DO 370 IX = IBEG,NUM
         IWORK(J) = IDATA(IX)
         J = J + 1
370   CONTINUE
      IF(J .EQ. 33) GO TO 390
C
C  SET REST OF RECORD TO MSNG
C
      DO 380 I = J,32
         IWORK(I) = MISSNG
 380  CONTINUE
C
C WRITE THE RECORD
C
  390 CONTINUE
      CALL UWRITT(KPDDDF(IDF),IREC,IWORK,ISTAT)
      IF(ISTAT .NE. 0) GO TO 990
      IREC = IREC + 1
      IF(NUM .GE. LDATA) GO TO 400
      IBEG = NUM + 1
      NUM = IBEG + 31
      J = 1
      GO TO 360
C
C SEE IF ANY OTHER RECORDS (SHOULD GET SET TO MISSING)
C
  400 CONTINUE
      IF(IREC .GT. LSTREC) GO TO 999
C
C MUST RESET REMAINING RECORDS
C
      DO 410 I = 1,32
         IWORK(I) = MISSNG
 410  CONTINUE
      IF(IPDDB .GT. 0) WRITE(IOGDB,411) IREC,LSTREC
  411 FORMAT(' RESET ANY LEFTOVERS IREC,LSTREC=',2I6)
      DO 420 I = IREC,LSTREC
         CALL UWRITT(KPDDDF(IDF),I,IWORK,ISTAT)
420   CONTINUE
      GO TO 999
C
  900 CONTINUE
C
C  TOO MANY VALUES
C
      ISTAT = 1
      GO TO 999
C
  990 CONTINUE
C
C  SYSTEM ERROR
C
      ISTAT = 3
  999 CONTINUE
C  DEBUG AND RETURN
C
      IF(IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE(IOGDB,975) ISTAT
  975 FORMAT(' SUBROUTINE WPDDLY EXECUTED. ISTAT = ',I2)
      RETURN
      END
