C MEMBER PDFLOR
C  (from old member PDBFDLOB)
C
       SUBROUTINE PDFLOR(IFUT,NHOUR,IPOS,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDFLOR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  12-6-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE FINDS THE LAST OBSERVED DATA VALUE IN AN RRS      *
C    RECORD WHEN THE CURRENT LAST OBSERVED VALUE IS DELETED.           *
C    THIS SUBROUTINE IS CALLED WHEN THE CURRENT LAST OBSERVED VALUE    *
C    IS IN THE RRS RECORD -- NOT A FREE POOL RECORD.                   *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IFUT       I     I     1    FUTURE FLAG                        *
C                                     0 = NON FUTURE                   *
C                                     1 = FUTURE WRITE                 *
C       NHOUR      I     I     1    JULIAN HOUR OF VALUE DELETED       *
C       IPOS       I     I     1    POSITION OF DELETED VALUE IN RECORD*
C       IRRBUF     I    I/O    ?    RRS RECORD                         *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdflor.f,v $
     . $',                                                             '
     .$Id: pdflor.f,v 1.1 1995/09/17 18:43:49 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,5)
 5    FORMAT(' *** ENTER PDFLOR')
      ISTAT = 0
      IPER = IRRBUF(14)
      IF(IFUT.EQ.1) GO TO 999
      IF(NHOUR.NE.IRRBUF(16)) GO TO 999
C
C       IF NOT INSTANTANEOUS  CHECK NEXT OB
C
      IF(IPER.EQ.2) GO TO 100
      I = IPOS + IPER
      IF(IRRBUF(9).GT.IRRBUF(11)) GO TO 50
C
C       CHECK HERE FOR NON WRAP RECORD
C
      IF(I.GT.IRRBUF(11)) GO TO 100
      IF(NHOUR.EQ.IRRBUF(I)) GO TO 999
      GO TO 100
C
C       CHECK HERE FOR WRAP IN RECORD
C
 50   CONTINUE
      IF(I.GT.IRRBUF(1)) I = IRRBUF(9)
      IF(I.GT.IRRBUF(11).AND.I.LE.IRRBUF(9)) GO TO 100
      IF(NHOUR.EQ.IRRBUF(I)) GO TO 999
C
C        GO BACK FOR NEXT VALUE
C
 100  CONTINUE
      I = IPOS - IPER
      IF(IRRBUF(9).GT.IRRBUF(11)) GO TO 150
C
C        NO WRAP IN RECORD
C
      IF(I.LT.IRRBUF(9)) GO TO 200
      IRRBUF(16) = JULMIN(IPER,IRRBUF(I))
      GO TO 999
C
C        WRAP AROUND RECORD
C
 150  CONTINUE
      ISTRT = IRRBUF(1) - IRRBUF(7) * IPER + 1
      IF(I.LT.ISTRT) I = IRRBUF(1) - IPER + 1
      IF(I.LT.IRRBUF(9).AND.I.GE.IRRBUF(11)) GO TO 200
      IRRBUF(16) = JULMIN(IPER,IRRBUF(I))
      GO TO 999
C
C        LOOK IN FREE POOL
C
 200  CONTINUE
      IF(IRRBUF(13).NE.0) GO TO 210
      IRRBUF(16) = 0
      GO TO 999
 210  CONTINUE
C
C        GET LAST FREE POOL RECORD
C
      IREC = IRRBUF(13)
 220  CONTINUE
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IFRBUF(1).EQ.0) GO TO 230
      IREC = IFRBUF(1)
      GO TO 220
 230  CONTINUE
      I = IFRBUF(2) * IPER + 2 - IPER + 1
      IRRBUF(16) = JULMIN(IPER,IFRBUF(I))
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDFLOR')
      RETURN
      END
