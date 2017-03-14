C MEMBER PDDFUT
C  (from old member PDBFUTPT)
C
       SUBROUTINE PDDFUT(IFUT,IREV,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDFUT                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  12-8-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE DELETES ALL FUTURE DATA FROM AN RRS RECORD AND    *
C    ANY ASSOCIATED FREE POOL RECORDS.  IT DOES THIS WHEN THE          *
C    RRS WRITE ROUTINE IS CALLED WITH A NON-REVISION FUTURE WRITE.     *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IFUT       I     I     1    FUTURE WRITE FLAG                  *
C                                     0 = NOT FUTURE                   *
C                                     1 = FUTURE                       *
C       IREV       I     I     1    REVISION WRITE FLAG                *
C                                     0 = NOT REVISION                 *
C                                     1 = REVISION                     *
C       IRRBUF     I    I/O    ?    ARRAY FOR RRS RECORD               *
C       IFRBUF     I    I/O    ?    ARRAY FOR FREE POOL RECORD         *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                     0 = NORMAL RETURN                *
C                                     OTHER = READ/WRITE ERROR         *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pddfut.f,v $
     . $',                                                             '
     .$Id: pddfut.f,v 1.1 1995/09/17 18:43:43 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,5)
 5    FORMAT(' *** ENTER PDDFUT')
      IF(IFUT.NE.1.OR.IREV.NE.0) GO TO 999
      IF(IRRBUF(8).EQ.0) GO TO 999
C
C      DELETE ALL DATA IN REGULAR RECORD
C
      IF(IRRBUF(16).NE.0) GO TO 100
      IRRBUF(8) = 0
      IRRBUF(9) = 0
      IRRBUF(11) = 0
      IRRBUF(15) = 0
      IREC = IRRBUF(13)
      IRRBUF(13) = 0
      IF(IREC.EQ.0) GO TO 999
C
C        DELETE ALL FREE POOL DATA
C
 10   CONTINUE
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      NREC = IFRBUF(1)
      IFRBUF(1) = -1
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IREC = NREC
      IF(IREC.EQ.0) GO TO 999
      GO TO 10
C
C        START AT END OF RECORD DELETING FUTURE
C
 100  CONTINUE
      IPER = IRRBUF(14)
      ISTRT = IRRBUF(1) - IRRBUF(7) * IPER + 1
      IE = IRRBUF(9)
      IF(IRRBUF(9).GT.IRRBUF(11)) IE = ISTRT
      IS = IRRBUF(11)
 110  CONTINUE
      NUM = (IS - IE + IPER) / IPER
      DO 150 I = 1,NUM
         IF(IRRBUF(16).EQ.JULMIN(IPER,IRRBUF(IS))) GO TO 999
         IS = IS - IPER
         IRRBUF(11) = IS
         IRRBUF(8) = IRRBUF(8) - 1
 150  CONTINUE
C
C        GO BACK FOR WRAP AROUND
C
      IF(IE.EQ.IRRBUF(9)) GO TO 200
      IS = IRRBUF(1) - IPER + 1
      IE = IRRBUF(9)
      IRRBUF(11) = IS
      GO TO 110
C
C        RESET POINTERS
C
 200  CONTINUE
      IRRBUF(8) = 0
      IRRBUF(9) = 0
      IRRBUF(11) = 0
      IF(IRRBUF(13).EQ.0) GO TO 999
C
C        MOVE FREE POOL TO RECORD AND GO BACK
C
      CALL PDMFTR(IRRBUF,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      GO TO 100
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDDFUT')
      RETURN
      END
