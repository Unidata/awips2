C MEMBER PDRDFR
C  (from old member PDBRWFR)
C
       SUBROUTINE PDRDFR(IREC,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDRDFR                                         *
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
C    THIS SUBROUTINE READS A FREE POOL RECORD.  IT SETS THE LOGICAL    *
C    UNIT AND READS THE RECORD.                                        *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IREC       I     I     1    LOGICAL RECORD NUMBER              *
C                                                                      *
C       IFRBUF     I     I     ?    BUFFER TO READ TO                  *
C                                                                      *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                     0 = NORMAL RETURN                *
C                                     OTHER = READ ERROR               *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdrdfr.f,v $
     . $',                                                             '
     .$Id: pdrdfr.f,v 1.1 1995/09/17 18:44:13 dws Exp $
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
 10   FORMAT(' *** ENTER PDRDFR')
C
C        SET UNIT AND READ A RECORD
C
      IUNIT = KPDDDF(LUFREE)
      CALL UREADT(IUNIT,IREC,IFRBUF,ISTAT)
      IF(IPDDB.GT.1) WRITE(IOGDB,199) IREC,IFRBUF(1)
 199  FORMAT(' PDRDFR READ FREE POOL RECORD ',I7,I6)
      IF(IPDTR.GT.1) WRITE(IOGDB,200)
 200  FORMAT(' *** EXIT PDRDFR')
      RETURN
      END
