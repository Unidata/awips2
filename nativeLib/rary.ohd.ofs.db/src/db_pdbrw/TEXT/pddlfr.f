C MEMBER PDDLFR
C  (from old member PDBDELFR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBDELFR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDDLFR(IFHOUR,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDLFR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-20-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE DELETES FREE POOL RECORDS FOR A RRS RECORD.       *
C    IT READS THE FREE POOL RECORDS AND DELETES THEM WHEN THE          *
C    LAST VALUE IN THE RECORD IS MORE THAN THE MINIMUM DAYS OF         *
C    DATA OLDER THAN THE LAST OBSERVED DATA.  IF THERE IS ONLY FUTURE  *
C    DATA, THE MOST RECENT DATA IS CHECKED INSTEAD OF THE LAST         *
C    OBSERVED.  A RECORD CONTAINING THE FIRST HOUR WRITTEN IN THE CALL *
C    WILL NOT BE DELETED.                                              *
C    AN ENTIRE FREE POOL RECORD IS DELETED AT A TIME.                  *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IFHOUR     I     I     1    FIRST HOUR WRITTEN IN CALL         *
C                                                                      *
C       IRRBUF     I    I/O    ?    BUFFER FOR RRS RECORD              *
C                                                                      *
C       IFRBUF     I    I/O    ?    BUFFER FOR FREE POOL RECORD        *
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pddlfr.f,v $
     . $',                                                             '
     .$Id: pddlfr.f,v 1.1 1995/09/17 18:43:44 dws Exp $
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
      ISTAT = 0
      IF(IPDTR.GT.1) WRITE(IOGDB,5)
  5   FORMAT(' *** ENTER PDDLFR ')
      IREC = IRRBUF(13)
      IF(IREC.EQ.0) GO TO 999
      ISKIP = IRRBUF(14)
      MINDAY = IRRBUF(6) * 24
      IHOUR = IRRBUF(16)
      IF(IHOUR.EQ.0) IHOUR = JULMIN(ISKIP,IRRBUF(IRRBUF(11)))
C
C        READ A FREE POOL RECORD
C
 10   CONTINUE
      CALL PDRDFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        CHECK FOR MINDAY OF DATA
C
      IRRBUF(15) = JULMIN(ISKIP,IFRBUF(3))
      IPOS = IFRBUF(2) * ISKIP + 2 - ISKIP + 1
      LHOUR = JULMIN(ISKIP,IFRBUF(IPOS))
      IF(MINDAY.GT.IHOUR - LHOUR) GO TO 999
      IF(IFHOUR.NE.0.AND.IFHOUR.LE.LHOUR) GO TO 999
C
C        DELETE THE FREE POOL RECORD
C
      IFREC = IFRBUF(1)
      IFRBUF(1) = -1
      IRRBUF(13) = IFREC
      IRRBUF(15) = 0
      CALL PDWRFR(IREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        SEE IF MORE FREE POOL RECORDS
C
      IREC = IFREC
      IF(IREC.NE.0) GO TO 10
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDDLFR')
      RETURN
      END
