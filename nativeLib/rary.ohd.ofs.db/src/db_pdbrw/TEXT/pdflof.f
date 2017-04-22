C MEMBER PDFLOF
C  (from old member PDBFDLOB)
C***********************************************************************
C                                                                      *
C         MEMBER PDBFDLOB                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDFLOF(IFUT,NHOUR,IPOS,IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDFLOF                                         *
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
C    IS IN A FREE POOL RECORD -- NOT THE REGULAR RECORD.               *
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
      INCLUDE 'pdbcommon/pdrrsc'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdflof.f,v $
     . $',                                                             '
     .$Id: pdflof.f,v 1.1 1995/09/17 18:43:48 dws Exp $
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
 5    FORMAT(' *** ENTER PDFLOF')
      ISTAT = 0
      IF(IFUT.EQ.1) GO TO 999
      IF(NHOUR.NE.IRRBUF(16)) GO TO 999
      IPER = IRRBUF(14)
C
C        IF PERIOD THERE  CHECK NEXT VALUE
C
      NUM = (IPOS - 2 + IPER - 1) / IPER
      IF(IPER.EQ.2) GO TO 100
      IF(NUM + 1.GT.IFRBUF(2)) GO TO 30
      I = IPOS + IPER
      IF(IFRBUF(I).EQ.NHOUR) GO TO 999
      GO TO 100
C
C       CHECK NEXT FREE POOL
C
 30   CONTINUE
      IF(IFRBUF(1).EQ.0) GO TO 50
      CALL PDRDFR(IFRBUF(1),IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IFRBUF(IFREEL + 3).EQ.NHOUR) GO TO 999
C
C        CHECK FIRST OB IN REGULAR RECORD
C
 50   CONTINUE
      IF(IRRBUF(8).EQ.0) GO TO 100
      IF(IRRBUF(IRRBUF(9)).EQ.NHOUR) GO TO 999
C
C        GO BACK AN OBSERVATION
C
 100  CONTINUE
      NUM = NUM - 1
      IF(NUM.EQ.0) GO TO 150
      I = IPOS - IPER
      IRRBUF(16) = JULMIN(IPER,IFRBUF(I))
      GO TO 999
C
C        HAVE TO GO BACK 1 FREE POOL RECORD
C
 150  CONTINUE
      IREC = IRRBUF(13)
      IRSAV = IFRBUF(1)
      IF(IREC.NE.IRSAV) GO TO 160
      IRRBUF(16) = 0
      GO TO 999
C
C        READ FREE POOL RECORDS
C
 160  CONTINUE
      CALL PDRDFR(IREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IFRBUF(IFREEL + 1).EQ.IRSAV) GO TO 200
      IREC = IFRBUF(IFREEL + 1)
      GO TO 160
C
C        USE THE LAST VALUE IN THIS RECORD
C
 200  CONTINUE
      I = IFRBUF(IFREEL + 2) * IPER + 2 - IPER + 1 + IFREEL
      IRRBUF(16) = JULMIN(IPER,IFRBUF(I))
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDFLOF')
      RETURN
      END
