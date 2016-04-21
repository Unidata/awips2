C MEMBER PDMFTR
C  (from old member PDBDELRR)
C
       SUBROUTINE PDMFTR(IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMFTR                                         *
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
C    THIS SUBROUTINE MOVES DATA STORED IN FREE POOL RECORDS TO THE     *
C    REGULAR RRS RECORD.  THIS ROUTINE IS CALLED WHEN ALL THE DATA     *
C    IN THE REGULAR RECORD IS DELETED.                                 *
C    DATA LOSS OCCURS IF ALL FREE POOL DATA WONT FIT.                  *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IRRBUF     I    I/O    ?    ARRAY FOR RRS RECORD               *
C                                                                      *
C       IFRBUF     I    I/O    ?    ARRAY FOR FREE POOL RECORD         *
C                                                                      *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmftr.f,v $
     . $',                                                             '
     .$Id: pdmftr.f,v 1.1 1995/09/17 18:44:02 dws Exp $
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
 5    FORMAT(' *** ENTER PDMFTR')
      IPOS = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
      IRRBUF(9) = IPOS
      IREC = IRRBUF(13)
C
C        READ A FREE POOL RECORD AND MOVE DATA TO RECORD
C
 10   CONTINUE
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      NWDS = IFRBUF(2) * IRRBUF(14)
      IF(IPOS + NWDS - 1.GT.IRRBUF(1)) GO TO 500
      IRRBUF(8) = IRRBUF(8) + IFRBUF(2)
      CALL UMEMOV(IFRBUF(3),IRRBUF(IPOS),NWDS)
      IPOS = IPOS + NWDS
C
C        WRITE THE DELETED FREE POOL RECORD
C
      IDREC = IREC
      IREC = IFRBUF(1)
      IFRBUF(1) = -1
      CALL PDWRFR(IDREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IREC.NE.0) GO TO 10
C
C        PUT IN POINTERS
C
      IRRBUF(11) = IPOS - IRRBUF(14)
      IRRBUF(13) = 0
      IRRBUF(15) = 0
      GO TO 999
C
C        LOOSE DATA THAT WONT FIT IN RECORD
C
 500  CONTINUE
      NWDS = IRRBUF(1) - IPOS + 1
      IF(NWDS.EQ.0) GO TO 510
      IRRBUF(8) = IRRBUF(8) + NWDS / IRRBUF(14)
      CALL UMEMOV(IFRBUF(3),IRRBUF(IPOS),NWDS)
C
C        SET POINTERS
C
 510  CONTINUE
      IRRBUF(11) = IRRBUF(1) - IRRBUF(14) + 1
      IRRBUF(13) = 0
      IRRBUF(15) = 0
      IF(IRRBUF(16).EQ.0) GO TO 550
      LSTHR = JULMIN(IRRBUF(14),IRRBUF(IRRBUF(11)))
      IF(LSTHR.GE.IRRBUF(16)) GO TO 550
      IRRBUF(16) = LSTHR
C
C        DELETE REST OF FREE POOL RECORDS
C
 550  CONTINUE
      IDREC = IREC
      IREC = IFRBUF(1)
      IFRBUF(1) = -1
      CALL PDWRFR(IDREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IREC.EQ.0) GO TO 999
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.EQ.0) GO TO 550
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMFTR')
      RETURN
      END
