C MEMBER PDMVBF
C  (from old member PDBMOVBF)
C***********************************************************************
C                                                                      *
C         MEMBER PDBMOVBF                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDMVBF(ITIME,IVAL,IPER,ISKIP,MIN,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMVBF                                         *
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
C    THIS SUBROUTINE WRITES AN OBSERVATION SET TO A RRS RECORD         *
C    WHEN THE TIME OF THE OBSERVATION IS BEFORE THE EARLIEST           *
C    TIME IN THE RECORD.  THE RECORD CANNOT HAVE AN EXISTING           *
C    FREE POOL RECORD.                                                 *
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
C       MIN        I     I     1    MINUTES OF OBSERVATION             *
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
      INCLUDE 'pdbcommon/pdbdta'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmvbf.f,v $
     . $',                                                             '
     .$Id: pdmvbf.f,v 1.1 1995/09/17 18:44:06 dws Exp $
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
C  DEBUG
C
      IF(IPDTR.GT.1) WRITE(IOGDB,10)
  10  FORMAT(' *** ENTER PDMVBF ')
C
C         SEE WHAT TO DO
C
      IF(IRRBUF(7).EQ.IRRBUF(8)) GO TO 200
      ISTRT = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
      IF(ISTRT.EQ.IRRBUF(9)) GO TO 100
C
C        RECORD NOT FULL - PUT OBSERVATION BEFORE EARLIEST
C
      IPOS = IRRBUF(9) - ISKIP
 20   CONTINUE
      IRRBUF(IPOS) = ITIME
      IF(ISKIP.EQ.2) IRRBUF(IPOS) = ITIME * 100 + MIN
      IRRBUF(IPOS + 1) = IVAL
      IF(ISKIP.EQ.3) IRRBUF(IPOS + 2) = IPER
      IRRBUF(8) = IRRBUF(8) + 1
      IRRBUF(9) = IPOS
      GO TO 999
 100  CONTINUE
C
C        RECORD NOT FULL - PUT VALUE AT END OF RECORD
C
      IPOS = IRRBUF(1) - ISKIP + 1
      GO TO 20
 200  CONTINUE
C
C        PUT THE VALUE IN A FREE POOL RECORD
C
      CALL PDGTFR(IFREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IRRBUF(LHDRRS + 12) = IRRBUF(LHDRRS + 12) + 1
      IRRBUF(13) = IFREC
      IRRBUF(15) = ITIME
      IFRBUF(1) = 0
      IFRBUF(2) = 1
      IFRBUF(3) = ITIME
      IF(ISKIP.EQ.2) IFRBUF(3) = ITIME * 100 + MIN
      IFRBUF(4) = IVAL
      IF(ISKIP.EQ.3) IFRBUF(5) = IPER
      CALL PDWRFR(IFREC,IFRBUF,ISTAT)
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMVBF')
      RETURN
      END
