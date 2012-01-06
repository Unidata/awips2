C MEMBER PDCVPP
C  (from old member PP24DATA)
C***********************************************************************
C                                                                      *
C         MEMBER PP24DATA                                              *
C                             LAST UPDATE: 02/06/95.17:23:03 BY $WC21DT
C
C                                                                      *
C***********************************************************************
       SUBROUTINE PDCVPP(DATA,FACTOR,IREV,IHOUR,IDATA,JDATA,
     1ISTAID,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDCVPP                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  7-27-83                                        *
C                                                                      *
C              AUTHOR:  SONJA R SIEGEL                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS ROUTINE WILL ENCODE PP24 DATA FROM DATA, AND IHOUR           *
C    TO IDATA.  IT WILL CONVERT TO ENGLISH IF NEC                      *
C    IT ALSO CHECKS THE REVISION FLAG TO MAKE SURE HOUR IS GT          *
C    CURRENT HOUR OR EQUAL WITH REV FLAG OF 1                          *
C    IF HOUR IS GREATER , IT WRITES ANYWAY                             *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       DATA        R    I     1    THE REAL PRECIP24 VALUE            *
C       FACTOR      R    I     1    CONVERSION FACTOR                  *
C       IREV        I    I     1    REVISION FLAG 0=NEW, 1=REV         *
C       IHOUR       I    I     1    THE HOUR OF THE OBS                *
C       IDATA      I*2   I/O   1    THE ENCODED VALUE (OLD AND NEW)    *
C       JDATA       I    I/O   1    SUBSCRIPT OF DATA IN DATA ARRAY
C       ISTAID      I    I     2    STATION ID
C       ISTAT       I    I     1    STATUS, 0=OK, NOT 0 INVAL
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER*2 IDATA,ITYPE
      DIMENSION ISTAID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcvpp.f,v $
     . $',                                                             '
     .$Id: pdcvpp.f,v 1.1 1995/09/17 18:43:37 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
      DATA ITYPE/2hPP/
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
      RMISS=-999.0
      ISTAT=0
      IHSAVE = IHOUR
C
C IF DATA WAS MISSING, REPLACE IT
C
      IF(IDATA.EQ.-9) GO TO 200
C
C PARSE THE OLD VALUE
C IF OLD VALUE WAS ESTIMATED, ALWAYS REPLACE IT
C IF OLD HOUR GT NEW HOUR OR NEW ESTIMATED AND OLD NOT, DO NOT REPLACE
C IF NEW HOUR = OLD HOUR AND REVISION=0, DO NOT REPLACE
C OTHERWISE, REPLACE
C
      CALL PDGTPP(IDATA,PP24,IHRWAS,IPP)
      IF(IHRWAS.EQ.24) GO TO 200
C
C RESET ENDING HOUR OF 0 TO BE 23 SINCE THIS IS 12Z AND IS LAST HR
C  OF DAY
C
      IF(IHOUR .EQ. 0) IHSAVE = 23
      IF(IHRWAS.EQ. 0) IHRWAS = 23
      IF(IHSAVE.LT.IHRWAS.OR.IHSAVE.EQ.24)  GO TO 900
      IF(IHSAVE.EQ.IHRWAS .AND. IREV.EQ.0) GO TO 900
C
C CHANGE THE DATA
C
200   IF(DATA.LE.RMISS) GO TO 300
      TFACT=0.0
      R=DATA
      R=R*FACTOR+TFACT
C
C CHECK RANGE
C
      CALL PDCKVL(ISTAID,JDATA,ITYPE,R,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      CALL PDPTPP(R,IHOUR,IDATA)
      GO TO 999
300   IDATA=-9
      GO TO 999
900   ISTAT=20
999   JDATA=JDATA+1
      IF(IPDDB.EQ.1) WRITE(IOGDB,2000) DATA,IREV,IHOUR,IDATA,ISTAT
2000  FORMAT(' SUB PDCVPP DATA,IREV,IHOUR,IDATA,ISTAT=',F10.3,4I8)
      RETURN
      END
