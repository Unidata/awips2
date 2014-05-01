C MEMBER PDDFPC
C  (from old member PDBDELRR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBDELRR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDDFPC(IFIRST,IDEL,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDFPC                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  2-22-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE DELETES A FREE POOL RECORD FOR AN RRS RECORD.     *
C    IT IS USED WHEN THE FREE POOL RECORD TO DELETE IS IN A CHAIN      *
C    OF FREE POOL RECORDS.  IT REMOVES THE RECORD TO DELETE FROM       *
C    THE CHAIN AND WRITES A DELETED RECORD.                            *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IFIRST     I     I     1    FIRST FREE POOL RECORD NUMBER      *
C                                                                      *
C       IDEL       I     I     1    FREE POOL RECORD NUMBER TO DELETE  *
C                                                                      *
C       IFRBUF     I    I/O    ?    FREE POOL BUFFER                   *
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
      INCLUDE 'pdbcommon/pdrrsc'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER IFRBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pddfpc.f,v $
     . $',                                                             '
     .$Id: pddfpc.f,v 1.1 1995/09/17 18:43:42 dws Exp $
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
 5    FORMAT(' *** ENTER PDDFPC')
      ISTAT = 0
      IREC = IFIRST
C
C        READ A FREE POOL RECORD
C
 10   CONTINUE
      CALL PDRDFR(IREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
C
C        SEE IF NEXT RECORD IS ONE TO DELETE
C
      IF(IFRBUF(IFREEL + 1).EQ.0) GO TO 900
      IF(IFRBUF(IFREEL + 1).EQ.IDEL) GO TO 100
      IREC = IFRBUF(IFREEL + 1)
      GO TO 10
 100  CONTINUE
C
C        MOVE POINTERS AND WRITE THE RECORDS
C
      IFRBUF(IFREEL + 1)= IFRBUF(1)
      CALL PDWRFR(IREC,IFRBUF(IFREEL + 1),ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IFRBUF(1) = -1
      CALL PDWRFR(IDEL,IFRBUF,ISTAT)
      GO TO 999
 900  CONTINUE
C
C        SYSTEM ERROR - NO FIND POINTER TO RECORD
C
      WRITE(LPE,909)
 909  FORMAT(' ** ERROR ** SYSTEM ERROR IN PDDFPC')
      ISTAT = 1
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDDFPC')
      RETURN
      END
