C MEMBER PDMKSP
C  (from old member PDBMAKSP)
C***********************************************************************
C                                                                      *
C         MEMBER PDBMAKSP                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDMKSP(IRRBUF,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDMKSP                                         *
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
C    THIS SUBROUTINE MAKES SPACE IN A RRS RECORD.  IT DOES IT BY       *
C    MOVING THE OLDEST DATA IN THE RECORD TO A FREE POOL RECORD.       *
C    IF THERE IS ALREADY A FREE POOL RECORD, IT ADDS A NEW ONE ONTO    *
C    THE CHAIN                                                         *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IRRBUF     I    I/O    ?    RRS RECORD BUFFER                  *
C                                                                      *
C       IFRBUF     I    I/O    ?    FREE POOL RECORD BUFFER            *
C                                                                      *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                    0 = NORMAL RETURN                 *
C                                    OTHER = READ WRITE ERROR          *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdrrsc'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmksp.f,v $
     . $',                                                             '
     .$Id: pdmksp.f,v 1.1 1995/09/17 18:44:03 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,1)
 1    FORMAT(' *** ENTER PDMKSP')
C
C        SEE IF THERE IS ALREADY A FREE RECORD
C
      IREC = IRRBUF(13)
      IF(IREC.EQ.0) GO TO 100
C
C        GET LAST FREE POOL RECORD IN CHAIN
C
 10   CONTINUE
      CALL PDRDFR(IREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IFRBUF(IFREEL + 1).EQ.0) GO TO 100
      IREC = IFRBUF(IFREEL + 1)
      GO TO 10
C
C        GET A NEW FREE RECORD
C
 100  CONTINUE
      CALL PDGTFR(IFREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IRRBUF(LHDRRS + 12) = IRRBUF(LHDRRS + 12) + 1
C
C        PUT POINTER IN RECORD - NO PREVIOUS FREE POOL
C
      IF(IRRBUF(13).NE.0) GO TO 110
      IRRBUF(13) = IFREC
      GO TO 120
C
C        PUT POINTER IN LAST FREE POOL RECORD AND WRITE IT
C
 110  CONTINUE
      IFRBUF(IFREEL + 1) = IFREC
      CALL PDWRFR(IREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        MOVE OLDEST DATA IN RECORD TO FREE POOL RECORD
C
 120  CONTINUE
      IFRBUF(1) = 0
      IFRBUF(2) = (IFREEL - 2) / IRRBUF(14)
      IF(IFRBUF(2).GT.IRRBUF(8)) IFRBUF(2) = IRRBUF(8) - 1
      IRRBUF(8) = IRRBUF(8) - IFRBUF(2)
      ISTRT = IRRBUF(1) - IRRBUF(7) * IRRBUF(14) + 1
      NUM = IFRBUF(2) * IRRBUF(14)
      IEND = IRRBUF(9) + NUM - 1
      N = NUM
      IF(IEND.GT.IRRBUF(1)) N = IRRBUF(1) - IRRBUF(9) + 1
      CALL UMEMOV(IRRBUF(IRRBUF(9)),IFRBUF(3),N)
      IF(N.NE.NUM) GO TO 130
C
C         WRITE THE RECORD AND UPDATE POINTERS
C
      CALL PDWRFR(IFREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IRRBUF(9) = IEND + 1
      IF(IRRBUF(9).GT.IRRBUF(1)) IRRBUF(9) = ISTRT
      IF(IFREC.EQ.IRRBUF(13)) IRRBUF(15) = JULMIN(IRRBUF(14),IFRBUF(3))
      GO TO 999
C
C        GET REST OF DATA FROM BEGINNING OF RECORD
C
 130  CONTINUE
      IPOS = N + 3
      N = NUM - N
      CALL UMEMOV(IRRBUF(ISTRT),IFRBUF(IPOS),N)
      CALL PDWRFR(IFREC,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IRRBUF(9) = ISTRT + N
      IF(IFREC.EQ.IRRBUF(13)) IRRBUF(15) = JULMIN(IRRBUF(14),IFRBUF(3))
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDMKSP')
      RETURN
      END
