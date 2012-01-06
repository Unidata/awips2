C MEMBER PDMVDR
C  (from old member PDBMOVDR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBMOVDR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDMVDR(ITIME,IVAL,IPER,ISKIP,MIN,IFUT,IREV,IPOS,
     1                    IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMVDR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-19-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE WRITES AN OBSERVATION SET TO A RRS RECORD WHEN    *
C    THE VALUE IS WITHIN THE CURRENT RANGE OF THE RECORD. IF THERE     *
C    IS NO SPACE IN THE RECORD, THE EARLIEST DATA IS MOVED TO A FREE   *
C    POOL RECORD TO MAKE SPACE.  THE DATA WITHIN THE RECORD IS THEN    *
C    REORDERED TO MAKE A SLOT FOR THE NEW DATA.                        *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       ITIME      I     I     1    TIME OF DATA (JULIAN HOUR)         *
C       IVAL       I     I     1    DATA VALUE                         *
C       IPER       I     I     1    TIME PERIOD OF DATA                *
C       ISKIP      I     I     1    NUMBER OF WORDS IN OBSERVATION     *
C       MIN        I     I     1    MINUTE OF DATA                     *
C       IFUT       I     I     1    FUTURE WRITE INDICATOR             *
C      IREV       I     I     1    REVISION WRITE INDICATOR            *
C       IPOS       I     I     1    POSITION IN RECORD                 *
C       IRRBUF     I    I/O    ?    BUFFER FOR RRS RECORD              *
C       IFRBUF     I    I/O    ?    BUFFER FOR FREE POOL RECORD        *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                      0 = NORMAL RETURN               *
C                                      OTHER = READ/WRITE ERROR        *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER IRRBUF(1),IFRBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmvdr.f,v $
     . $',                                                             '
     .$Id: pdmvdr.f,v 1.1 1995/09/17 18:44:07 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
C
C  DEBUG
C
      IF(IPDTR.GT.1) WRITE(IOGDB,10)
  10  FORMAT(' *** ENTER PDMVDR ')
C
      IF(IRRBUF(7).NE.IRRBUF(8)) GO TO 50
C
C        MAKE SPACE IN RECORD
C
      CALL PDMKSP(IRRBUF,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        SEE IF POSITION WAS MOVED TO FREE POOL RECORD
C
      IF(IPOS.GE.IRRBUF(9).OR.(IPOS.LE.IRRBUF(11).AND.IRRBUF(11).
     1   LT.IRRBUF(9))) GO TO 50
      CALL PDFVFR(1,ITIME,IVAL,IPER,ISKIP,MIN,IFUT,IREV,IRRBUF,
     1              IFRBUF,ISTAT)
      GO TO 999
 50   CONTINUE
      IF(IRRBUF(9).NE.IRRBUF(11)) GO TO 60
      IMPOS = IRRBUF(11) - ISKIP
      IRRBUF(9) = IMPOS
      IF(IMPOS.GE.IRRBUF(1) - IRRBUF(7) * ISKIP + 1) GO TO 260
      IMPOS = IRRBUF(1) - ISKIP + 1
      IRRBUF(9) = IMPOS
      GO TO 260
 60   CONTINUE
      IF(IPOS.GE.IRRBUF(9).AND.IRRBUF(9).NE.IRRBUF(1) - IRRBUF(7)
     1        * ISKIP + 1) GO TO 200
C
C        MOVE FROM IPOS TO LATEST DATA UP 1 OBSERVATION SET
C
      NUM = IRRBUF(11) - IPOS + ISKIP
      IF(NUM.EQ.0) GO TO 110
      K = IRRBUF(11) + ISKIP * 2 - 1
      J = IRRBUF(11) + ISKIP - 1
      DO 100 I = 1,NUM
         IRRBUF(K) = IRRBUF(J)
         K = K - 1
         J = J - 1
 100  CONTINUE
      IRRBUF(11) = IRRBUF(11) + ISKIP
C
C        MOVE OBSERVATION SET IN
C
 110  CONTINUE
      IRRBUF(IPOS) = ITIME
      IF(ISKIP.EQ.2) IRRBUF(IPOS) = ITIME * 100 + MIN
      IRRBUF(IPOS + 1) = IVAL
      IF(ISKIP.EQ.3) IRRBUF(IPOS + 2) = IPER
      IRRBUF(8) = IRRBUF(8) + 1
      GO TO 999
 200  CONTINUE
C
C        MOVE FROM IPOS TO EARLIEST DATA DOWN 1 OBSERVATION
C
      NUM = IPOS - IRRBUF(9)
      IF(NUM.EQ.0) GO TO 250
      CALL UMEMOV(IRRBUF(IRRBUF(9)),IRRBUF(IRRBUF(9) - ISKIP),NUM)
      IRRBUF(9) = IRRBUF(9) - ISKIP
 250  CONTINUE
C
C        MOVE OBSERVATION IN
C
      IMPOS = IPOS - ISKIP
 260  CONTINUE
      IRRBUF(IMPOS) = ITIME
      IF(ISKIP.EQ.2) IRRBUF(IMPOS) = ITIME * 100 + MIN
      IRRBUF(IMPOS + 1) = IVAL
      IF(ISKIP.EQ.3) IRRBUF(IMPOS + 2) = IPER
      IRRBUF(8) = IRRBUF(8) + 1
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMVDR')
      RETURN
      END
