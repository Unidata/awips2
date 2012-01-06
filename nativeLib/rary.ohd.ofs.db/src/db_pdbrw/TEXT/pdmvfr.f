C MEMBER PDMVFR
C  (from old member PDBMOVFR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBMOVFR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDMVFR(ITIME,IVAL,IPER,ISKIP,MIN,ISTRT,IPOS,IFRBUF,
     1                        ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMVFR                                         *
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
C    THIS SUBROUTINE INSERTS A OBSERVATION SET INTO A FREE POOL        *
C    RECORD.  THE VALUE CAN BE BEFORE OR DURING THE RANGE OF THE       *
C    EXISTING DATA IN THE CHAIN OF FREE POOL RECORDS FOR A RRS STATION.*
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       ITIME      I     I     1    TIME OF DATA (JULIAN HOUR)         *
C       IVAL       I     I     1    VALUE                              *
C       IPER       I     I     1    TIME PERIOD OF DATA                *
C       ISKIP      I     I     1    NUMBER OF WORDS IN OBSERVATION     *
C       MIN        I     I     1    MINUTES OF OBSERVATION             *
C       ISTRT      I     I     1    LOGICAL RECORD NUMBER              *
C       IPOS       I     I     1    POSITION IN IFRBUF                 *
C       IFRBUF     I    I/O    ?    BUFFER FOR FREE POOL RECORDS       *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                    0 = NORMAL RETURN                 *
C                                    OTHER = ERROR                     *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'pdbcommon/pdrrsc'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER IFRBUF(1),ISAVE(3),IOBS(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmvfr.f,v $
     . $',                                                             '
     .$Id: pdmvfr.f,v 1.1 1995/09/17 18:44:08 dws Exp $
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
      ISTAT = 0
      IF(IPDTR.GT.1) WRITE(IOGDB,1)
 1    FORMAT(' *** ENTER PDMVFR')
C
C        INITIALIZE
C
      IOBS(1) = ITIME
      IF(ISKIP.EQ.2) IOBS(1) = ITIME * 100 + MIN
      IOBS(2) = IVAL
      IOBS(3) = IPER
      IREC = ISTRT
      NVPR = (IFREEL - 2) / ISKIP
      ILAST = IFRBUF(2) * ISKIP + 2 - ISKIP + 1
      IEND = ILAST + ISKIP - 1
      IMPOS = IPOS
C
C        SEE IF ROOM - MOVE LAST OBSERVATION SET
C
 10   CONTINUE
      IF(IFRBUF(2).NE.NVPR) GO TO 200
      CALL UMEMOV(IFRBUF(ILAST),ISAVE,ISKIP)
C
C        MOVE REST OF RECORD TO END OF RECORD
C
      NUM = ILAST - IMPOS
      IF(NUM.LT.0) GO TO 100
      IF(NUM.EQ.0) GO TO 50
      J = IEND
      K = ILAST - 1
      DO 40 I = 1,NUM
         IFRBUF(J) = IFRBUF(K)
         J = J - 1
         K = K - 1
 40   CONTINUE
C
C        PUT OBSERVATION SET INTO SPACE
C
 50   CONTINUE
      CALL UMEMOV(IOBS,IFRBUF(IMPOS),ISKIP)
      CALL UMEMOV(ISAVE,IOBS,ISKIP)
      IMPOS = 3
C
C        WRITE RECORD AND GET NEXT
C
      IF(IFRBUF(1).EQ.0) GO TO 100
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IREC = IFRBUF(1)
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      GO TO 10
C
C        GET A NEW FREE POOL RECORD AND WRITE THE LAST ONE
C
 100  CONTINUE
      CALL PDGTFR(IFREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IFRBUF(1) = IFREC
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IREC = IFREC
      CALL UMEMST(0,IFRBUF,IFREEL)
      IMPOS = 3
      GO TO 250
C
C        HAVE A RECORD WITH SPACE - MAKE A SLOT FOR OBSERVATION
C
 200  CONTINUE
      J = IFRBUF(2) * ISKIP + 2 + ISKIP
      K = J - ISKIP
      NUM = J - ISKIP + 1 - IMPOS
      IF(NUM.EQ.0) GO TO 250
      DO 230 I = 1,NUM
         IFRBUF(J) = IFRBUF(K)
         J = J - 1
         K = K - 1
 230  CONTINUE
C
C        PUT OBSERVATION IN SLOT
C
 250  CONTINUE
      IFRBUF(2) = IFRBUF(2) + 1
      CALL UMEMOV(IOBS,IFRBUF(IMPOS),ISKIP)
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
 999  CONTINUE
C
C        END
C
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMVFR')
      RETURN
      END
