C MEMBER PDDLRR
C  (from old member PDBDELRR)
C
       SUBROUTINE PDDLRR(IPOS,IFUT,NHOUR,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDLRR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-21-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE DELETES AN EXISTING VALUE IN A RRS RECORD.        *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IPOS       I     I     1    POSITION OF VALUE IN IRRBUF        *
C       IFUT       I     I     1    FUTURE FLAG 0 = REG  1 = FUTURE    *
C       NHOUR      I     I     1    HOUR OF DATA VALUE TO DELETE       *
C       IRRBUF     I    I/O    ?    RRS RECORD BUFFER                  *
C       IFRBUF     I    I/O    ?    FREE POOL RECORD BUFFER            *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pddlrr.f,v $
     . $',                                                             '
     .$Id: pddlrr.f,v 1.1 1995/09/17 18:43:45 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,10)
 10   FORMAT(' *** EXTER PDDLRR')
      IRRBUF(8) = IRRBUF(8) - 1
      ISTRT = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
C
C        CHECK FOR EMPTY RECORD
C
      IF(IRRBUF(8).GT.0) GO TO 50
      IRRBUF(9) = 0
      IRRBUF(11) = 0
      IF(IRRBUF(13).NE.0) GO TO 40
      IRRBUF(16) = 0
      GO TO 999
C
C        MOVE DATA FROM FREE POOL TO RECORD
C
 40   CONTINUE
      CALL PDMFTR(IRRBUF,IFRBUF,ISTAT)
      GO TO 999
 50   CONTINUE
C
C        CHECK ON LAST OBSERVED VALUE
C
      CALL PDFLOR(IFUT,NHOUR,IPOS,IRRBUF,IFRBUF,ISTAT)
C
C        SEE WHAT TO DO
C
      IF(IPOS.GT.IRRBUF(11).OR.
     1   (IRRBUF(9).LT.IRRBUF(11).AND.ISTRT.NE.IRRBUF(9))) GO TO 100
C
C        MOVE OLDEST DATA TO IPOS DOWN ONE OBSERVATION SET
C
      NUM = IRRBUF(11) - IPOS
      IRRBUF(11) = IRRBUF(11) - IRRBUF(14)
      IF(NUM.EQ.0) GO TO 90
      IMPOS = IPOS + IRRBUF(14)
      CALL UMEMOV(IRRBUF(IMPOS),IRRBUF(IPOS),NUM)
 90   CONTINUE
      IF(IRRBUF(11).GE.ISTRT) GO TO 999
      IRRBUF(11) = IRRBUF(1) - IRRBUF(14) + 1
      GO TO 999
 100  CONTINUE
C
C        MOVE EARLIEST DATA TO IPOS UP ONE OBSERVATION SET
C
      NUM = IPOS - IRRBUF(9)
      IRRBUF(9) = IRRBUF(9) + IRRBUF(14)
      IF(NUM.EQ.0) GO TO 120
      J = IPOS + IRRBUF(14) - 1
      K = IPOS - 1
      DO 110 I = 1,NUM
          IRRBUF(J) = IRRBUF(K)
          J = J - 1
          K = K - 1
 110  CONTINUE
 120  CONTINUE
      IF(IRRBUF(9).LT.IRRBUF(1)) GO TO 999
      IRRBUF(9) = ISTRT
C
C       END
C
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDDLRR')
      RETURN
      END
