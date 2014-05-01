C MEMBER RPD1SF
C  (from old member PDRPD1SF)
C***********************************************************************
C                                                                      *
C         MEMBER PDRPD1SF                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE RPD1SF(ISTAID,IDTYPE,IDATYP,IFDAY,LDAY,LDATA,DATA,
     1                    LENDAT,IDATES,LDFILL,IDELT,NVPDT,MSNG,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  RPD1SF                                         *
C                                                                      *
C  3/12/86 CALL PDRSDT TO RESET DATES IF BEFORE OR AFTER DATA ON FILE
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  4-19-83                                        *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE READS FUTURE DATA FOR ONE NON-RRS DATA TYPE       *
C    FOR A SINGLE STATION FOR A SPECIFIED PERIOD.  NOT USED            *
C    TO READ RRS DATA TYPES.                                           *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       ISTAID     A8    I     2     STATION IDENTIFIER                *
C          OR     I*4    I     1     STATION NUMBER                    *
C       IDTYPE    I*4    I     1     =0,ISTAID IS IDENTIFIER           *
C                                    =1,ISTAID IS NUMBER               *
C       IDATYP     A4    I     1     DATA TYPE CODE REQUESTED          *
C       IFDAY     I*4   IO     1     JULIAN DAY OF FIRST DAY OF DATA   *
C       LDAY      I*4   IO     1     JULIAN DAY OF LAST DAY OF DATA    *
C       LDATA     I*4    I     1     LENGTH OF DATA(I*2 WORDS)         *
C       LENDAT    I*4    I     1     LENGTH OF IDATES(I*4 WORDS)       *
C       IDATES    I*4    I    LENDAT FUTURE DATES ARRAY                *
C       DATA      I*2    O    LDATA  ARRAY CONTAINING DATA FROM IFDAY  *
C                                    TO LDAY                           *
C       LDFILL    I*4    O     1     # OF I*2 WORDS FILLED IN DATA     *
C       IDELT     I*4    O     1     TIME INTERVAL FOR DATA TYPE       *
C       NVDPT     I*4    O     1     # OF DATA VALUES PER TIME         *
C                                     INTERVAL FOR DATA TYPE           *
C       MSNG      I*2    O     1     VALUE FOR MISSING DATA            *
C       ISTAT     I*4    O     1     STATUS INDICATOR                  *
C                                      0 = NORMAL RETURN               *
C                                      1 = ISTAID NOT FOUND            *
C                                      2 = DATA TYPE NOT FOUND         *
C                                      3 = NO VALUES ON FILE WITHIN    *
C                                          SPECIFIED PERIOD            *
C                                      4 = SOME DATES NOT ON FILE, AT  *
C                                          LEAST 1 DAY MISSING         *
C                                      5 = DATA ARRAY TOO SMALL        *
C                                      6 = STATUS 4 AND 5 COMBINED     *
C                                      7 = SYSTEM ERROR ACCESSING FILE *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION ISTAID(2),IDATES(1)
      INTEGER*2 ISIRAY(128),DATA(32),IWORK(32),MSNG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpd1sf.f,v $
     . $',                                                             '
     .$Id: rpd1sf.f,v 1.1 1995/09/17 18:44:32 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA ITF24/4hTF24/,ITFMX/4hTFMX/,ITFMN/4hTFMN/
C                                                                      *
C***********************************************************************
C
      LRCP = LRCPDD*2
      LDFILL = 0
      ISTAT = 0
      ISTAT1 = 0
      IRDTF = 0
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,100)
  100 FORMAT(' SUBROUTINE RPD1SF ENTERED')
C
C  FIND STATION IN THE DATA BASE
C
      IFIND = 0
      LEN = 128
      IF(IDTYPE.EQ.0) CALL PDFNDR(ISTAID,LEN,IFIND,ISIREC,ISIRAY,
     1                            IFREE,ISTAT)
      IF(IDTYPE.EQ.1) CALL PDFNDI(ISTAID,LEN,IFIND,ISIREC,ISIRAY,
     1                           IFREE,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      IF(IFIND.NE.0) GO TO 200
      ISTAT = 1
      GO TO 999
  200 CONTINUE
C
C  SEE IF THIS IS A VALID FUTURE DATA TYPE
C
      IF(IDATYP.NE.ITF24.AND.IDATYP.NE.ITFMX.AND.IDATYP
     1   .NE.ITFMN) GO TO 210
C
C  OBTAIN SUBSCRIPT OF TYPE FROM DIRECTORY
C
      IX = IPDCKD(IDATYP)
      IF(IX.NE.0) GO TO 220
  210 ISTAT = 2
      GO TO 999
  220 CONTINUE
C
C CHECK DATES, FOR VALID RANGE, AND RESET IF NECESSARY
C
      CALL PDRSDT(IFDAY,LDAY,1,ITF24,0,ISIRAY,1,ITMEP,NT,ISTAT2)
C
C  FIND THE POINTER IN THE STATION INFORMATION RECORD
C
      IFILE = IDDTDR(4,IX)
      NDTWDS = IDDTDR(6,IX)
       CALL UMEMOV(IDDTDR(2,IX),JDDTDR,1)
      IPOINT = IPDFDT(ISIRAY,JDDTDR)
CZ    IPOINT = IPDFDT(ISIRAY,IDDTDR(2,IX))
      IF(IPOINT.EQ.0) GO TO 210
C
C  CHECK IF FUTURE TYPE IS MAX/MIN
C
      IIXX = IPDCDW(IDATYP)
      IF(IDDTDR(4,IIXX).GT.0) GO TO 230
C
C  IT IS A MAX/MIN CORRECT POINTER
C
      IPOINT = IPOINT + IDDTDR(5,IIXX)
      NDTWDS = IDDTDR(6,IIXX)
  230 CONTINUE
C
C  FIND WHERE THIS TYPE STARTS IN THE RECORD
C
      IRCOFS = IUNRCD(IPOINT,LRCP)
C
C  GET THE SUBSCRIPT OF TYPE IN RECORD
C
      IDX = IPOINT-(IRCOFS-1) * LRCP
C
C   READ IN FUTURE DATES ARRAY
C
      CALL PDGFUD(IDATES,LENDAT,IX,IRECD,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      NDTS = IDATES(1)
      JDAY = IFDAY
      IF(IPDDB.EQ.1) WRITE(IOGDB,235) IPOINT,IRCOFS,NDTS
  235 FORMAT(' IPOINT = ',I4,' IRCOFS = ',I4,' # OF DATES = ',I4)
C
C  SEE IF DATE IS THERE
C
      I = 1
      IF(NDTS.EQ.0) GO TO 900
  240 CONTINUE
C
C  SEE IF ARRAY IS FILLED
C
      IF(I + (NDTWDS -1).GT.LDATA) GO TO 950
C
C    LOOK FOR THIS DAY IN DATES ARRAY
C
      CALL PDFNDD(JDAY,IDATES,IDTXX)
      IF(IDTXX.EQ.0) GO TO 260
C
C  DATE IS THERE  NOW GET RECORD
C
      IDREC = IDATES(IDTXX + 1)
C
C  FIND THE RECORD WITH THE DATA VALUES
C
      IREC = IDREC + IRCOFS - 1
C
C  READ IN DATA FROM FILE
C
      CALL UREADT(KPDDDF(IFILE),IREC,IWORK,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
C
C
C  IF FUTURE DATA MOVE TWO VALUES
C
      DATA(I) = IWORK(IDX)
      IF(IPDDB.EQ.1) WRITE(IOGDB,241) IWORK(IDX)
  241 FORMAT(' ONE VALUE READ = ',I4)
      IF(IDATYP.NE.ITF24) GO TO 250
      DATA(I+1) = IWORK(IDX+1)
      IF(IPDDB.EQ.1) WRITE(IOGDB,242) IWORK(IDX+1)
  242 FORMAT(' TWO VALUES READ = ',I4)
  250 CONTINUE
      IRDTF = 1
      GO TO 270
  260 CONTINUE
C
C  DATE NOT THERE SET TO MISSING
C
      DATA(I) = MISSNG
      IF(NDTWDS.EQ.2) DATA(I+1) = MISSNG
C
C  SET MISSING FLAG
C
      ISTAT1 = 4
      IF(IPDDB.EQ.1) WRITE(IOGDB,265) JDAY,I
  265 FORMAT(' FUTURE DAY ',I6,' SET TO MISSING IN DATA ARRAY ',
     1       ' SUBSCRIPT ',I4)
  270 CONTINUE
C
C  INCREMENT DAY
C
      I = I + NDTWDS
      JDAY = JDAY + 1
      IF(JDAY.LE.LDAY) GO TO 240
      LDFILL = I - 1
C
C FINISHED LAST DAY OF INTERVAL
C
      IDELT = 24
      NVPDT = NDTWDS
      MSNG = MISSNG
C
C  CHECK FLAG TO SET ISTAT
C
      IF(IRDTF.NE.1.OR.ISTAT1.EQ.4 .OR. ISTAT2.EQ.6) ISTAT = 4
      GO TO 999
  900 CONTINUE
C
C  NO VALUES ON FILE FOR THESE DAYS
C
      ISTAT = 3
      GO TO 999
  950 CONTINUE
C
C  DATA ARRAY FILLED
C
      ISTAT = 5
C
C  ARRAY FILLED SEE IF SOME DAYS ARE MISSING
C
      LDFILL = I
      IF(ISTAT1.EQ.4 .OR. ISTAT2.EQ.6) ISTAT = 6
      GO TO 999
  990 CONTINUE
C
C  SYSTEM ERROR
C
      ISTAT = 7
  999 CONTINUE
      IF(IPDDB.EQ.1) WRITE(IOGDB,995) LDFILL
  995 FORMAT(' LDFILL = ',I4)
C
C  DEBUG AND RETURN
C
      IF(IPDDB.EQ.1) WRITE(IOGDB,1000) ISTAT
 1000 FORMAT(' SUBROUTINE RPD1SF COMPLETE.  STATUS = ',I4)
      RETURN
      END
