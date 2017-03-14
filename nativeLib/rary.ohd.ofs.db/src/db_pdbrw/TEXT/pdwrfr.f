C MEMBER PDWRFR
C  (from old member PDBRWFR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBRWFPR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDWRFR(IREC,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDWRFR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-17-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE WRITES A RECORD TO THE FREE RECORD POOL.  IT SETS *
C    THE LOGICAL UNIT AND WRITES TO THE RECORD NUMBER PASSED.          *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IREC       I     I     1    LOGICAL RECORD NUMBER              *
C                                                                      *
C       IFRBUF     I     I     ?    BUFFER TO WRITE FROM               *
C                                                                      *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                      0 = NORMAL RETURN               *
C                                      OTHER = WRITE ERROR             *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdwrfr.f,v $
     . $',                                                             '
     .$Id: pdwrfr.f,v 1.1 1995/09/17 18:44:23 dws Exp $
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
      IF(IPDTR.GT.1) WRITE(IOGDB,10)
 10   FORMAT(' *** ENTER PDWRFR')
C
C        SET LOGICAL UNIT AND WRITE A FREE POOL RECORD
C
      IUNIT = KPDDDF(LUFREE)
      CALL UWRITT(IUNIT,IREC,IFRBUF,ISTAT)
      IF(IPDDB.GT.1) WRITE(IOGDB,100) IREC
 100  FORMAT(' PDWRFR WROTE A FREE POOL RECORD TO ',I7)
      IF(IPDTR.GT.1) WRITE(IOGDB,110)
 110  FORMAT(' *** EXIT PDWRFR')
      RETURN
      END
