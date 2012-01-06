C MEMBER PDDELF
C  (from old member PDBDELRR)
C
       SUBROUTINE PDDELF(IREC,IPOS,IFUT,NHOUR,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDELF                                         *
C                                                                      *
C CORRECT PROBLEM IN DELETING FREEPOOLS IN PDMFTR 7-20-84
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
C    THIS SUBROUTINE DELETES A VALUE IN A FREE POOL RECORD OF AN       *
C    RRS RECORD.                                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IREC       I     I     1    RECORD NUMBER OF FREE POOL RECORD  *
C       IPOS       I     I     1    POSITION IN FREE POOL RECORD       *
C       IFUT       I     I     1    FUTURE FLAG 0 = REG   1 = FUTURE   *
C       NHOUR      I     I     1    HOUR OF DATA BEING DELETEDRD       *
C       IRRBUF     I    I/O    ?    BUFFER FOR RRS RECORD              *
C       IFRBUF     I    I/O    ?    BUFFER FOR FREE POOL RECORD        *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                    0 = NORMAL RETURN                 *
C                                    OTHER = READ/WRITE ERROR          *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pddelf.f,v $
     . $',                                                             '
     .$Id: pddelf.f,v 1.1 1995/09/17 18:43:40 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,5)
  5   FORMAT(' *** ENTER PDDELF ')
C
C        CHECK FOR DELETE OF LAST OBSERVED DATA
C
      CALL PDFLOF(IFUT,NHOUR,IPOS,IRRBUF,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        CHECK FOR EMPTY RECORD
C
      IFRBUF(2) = IFRBUF(2) - 1
      IF(IFRBUF(2).GT.0) GO TO 100
      IF(IFRBUF(1).EQ.0.AND.IRRBUF(13).EQ.IREC) GO TO 30
      IF(IREC.EQ.IRRBUF(13)) GO TO 20
C
C        REARRANGE THE CHAIN OF POINTERS
C
      CALL PDDFPC(IRRBUF(13),IREC,IFRBUF,ISTAT)
      GO TO 999
 20   CONTINUE
C
C        PUT POINTER TO NEXT FREE POOL IN RRS RECORD
C
      IRRBUF(13) = IFRBUF(1)
      IFRBUF(1) = -1
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      CALL PDRDFR(IRRBUF(13),IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IRRBUF(15) = JULMIN(IRRBUF(14),IFRBUF(3))
      GO TO 999
 30   CONTINUE
C
C         DELETE THE ONLY FREE POOL RECORD
C
      IRRBUF(13) = 0
      IRRBUF(15) = 0
      IFRBUF(1) = -1
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      GO TO 999
 100  CONTINUE
C
C        MOVE DATA DOWN IN RECORD
C
      IMPOS = IPOS + IRRBUF(14)
      NUM = (IFRBUF(2) + 1) * IRRBUF(14) + 3 - IMPOS
      IF(NUM.EQ.0) GO TO 110
      CALL UMEMOV(IFRBUF(IMPOS),IFRBUF(IPOS),NUM)
 110  CONTINUE
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        IF ITS FIRST VALUE IN FIRST FREE POOL RECORD CHANGE POINTER
C
      IF(IREC.NE.IRRBUF(13).OR.IPOS.NE.3) GO TO 999
      IRRBUF(15) = JULMIN(IRRBUF(14),IFRBUF(3))
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDDELF')
      RETURN
      END
