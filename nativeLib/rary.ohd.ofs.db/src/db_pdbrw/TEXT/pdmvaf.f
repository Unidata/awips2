C MEMBER PDMVAF
C  (from old member PDBMOVAF)
C***********************************************************************
C                                                                      *
C         MEMBER PDBMOVAF                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDMVAF(ITIME,IVAL,IPER,ISKIP,MIN,IFHOUR,IRRBUF,IFRBUF,
     1                   ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMVAF                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-18-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE WRITES AN OBSERVATION SET TO A RRS RECORD WHEN    *
C    THE TIME OF THE VALUE IS AFTER THE LAST TIME CURRENTLY IN THE     *
C    RECORD.  IF THERE IS ALREADY A FREE POOL RECORD, THE OLDEST       *
C    DATA IS NOT WRITTEN OVER, A NEW FREE POOL RECORD IS ADDED.        *
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
C       IFHOUR     I     I     1    HOUR OF FIRST DATA WRITTEN IN CALL *
C       IRRBUF     I    I/O    ?    BUFFER FOR RRS RECORD              *
C       IFRBUF     I    I/O    ?    BUFFER FOR FREE POOL RECORD        *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                     0 = NORMAL RETURN                *
C                                     OTHER = READ/WRITE ERROR         *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmvaf.f,v $
     . $',                                                             '
     .$Id: pdmvaf.f,v 1.1 1995/09/17 18:44:05 dws Exp $
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
  10  FORMAT(' *** ENTER PDMVAF ')
C
      ISTRT = IRRBUF(1) - IRRBUF(7) * ISKIP + 1
      IFLAG = 0
C
C        SEE IF RECORD IS FULL
C
      IF(IRRBUF(8).EQ.0) GO TO 300
      IF(IRRBUF(8).LT.IRRBUF(7)) GO TO 200
      IF(IRRBUF(13).NE.0) GO TO 50
      IF(IFHOUR.NE.0.AND.IFHOUR.LE.JULMIN(ISKIP,IRRBUF(IRRBUF(9))))
     1    GO TO 50
      IF(IRRBUF(16).EQ.0) GO TO 100
      IF(IRRBUF(16).LT.JULMIN(ISKIP,IRRBUF(IRRBUF(11)))) GO TO 20
C
C        ADDING OBSERVED ONTO END OF OBSERVED
C
      IF(ITIME - JULMIN(ISKIP,IRRBUF(IRRBUF(9))).GE.IRRBUF(6) * 24)
     1        GO TO 100
      GO TO 50
 20   CONTINUE
C
C        ADDING FUTURE ONTO END WITH OBSERVED
C
      IF(IRRBUF(16) - JULMIN(ISKIP,IRRBUF(IRRBUF(9))).GE.IRRBUF(6)
     1   * 24) GO TO 100
C
C        GET FREE POOL RECORD AND MAKE SPACE
C
 50   CONTINUE
      CALL PDMKSP(IRRBUF,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      GO TO 200
C
C        MOVE IN OBSERVATION - FULL RECORD
C
 100  CONTINUE
      IPOS = IRRBUF(11) + ISKIP
      IF(IPOS.GT.IRRBUF(1)) GO TO 150
 110  CONTINUE
      IRRBUF(IPOS) = ITIME
      IF(ISKIP.EQ.2) IRRBUF(IPOS) = ITIME * 100 + MIN
      IRRBUF(IPOS + 1) = IVAL
      IF(ISKIP.EQ.3) IRRBUF(IPOS + 2) = IPER
      IRRBUF(11) = IPOS
      IF(IFLAG.EQ.1) GO TO 999
      IRRBUF(9) = IRRBUF(9) + ISKIP
      IF(IRRBUF(9).GT.IRRBUF(1)) IRRBUF(9) = ISTRT
      GO TO 999
 150  CONTINUE
C
C        HAVE TO WRAP AROUND
C
      IPOS = ISTRT
      GO TO 110
 200  CONTINUE
C
C        RECORD NOT FULL
C
      IRRBUF(8) = IRRBUF(8) + 1
      IFLAG = 1
      GO TO 100
C
C        EMPTY RECORD
C
 300  CONTINUE
      IRRBUF(9) = ISTRT
      IRRBUF(8) = 1
      IPOS = ISTRT
      IFLAG = 1
      GO TO 110
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMVAF')
      RETURN
      END
