C MEMBER PDGTFR
C  (from old member PDBGTFPR)
C***********************************************************************
C                                                                      *
C         MEMBER PDBGTFPR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDGTFR(IFREC,IFRBUF,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDGTFR                                         *
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
C    THIS SUBROUTINE SEARCHES THE FREE RECORD POOL FOR AN UNUSED       *
C    RECORD (FIRST WORD = -1).  IT STOPS SEARCHING AFTER IT RESETS     *
C    THE POINTER ONCE AND HITS THE LAST RECORD AGAIN.  THE SEARCH      *
C    BEGINS AT THE NEXT FREE RECORD IN THE POOL.                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IFREC      I     O     1    LOGICAL RECORD NUMBER              *
C                                                                      *
C       IFRBUF     I     O     ?    BUFFER TO READ RECORDS INTO        *
C                                                                      *
C       ISTAT      I     O     1    STATUS INDICATOR                   *
C                                    0 = NORMAL RETURN                 *
C                                    1 = NO FREE POOL RECORDS          *
C                                    OTHER = READ ERROR                *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdunts'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdgtfr.f,v $
     . $',                                                             '
     .$Id: pdgtfr.f,v 1.1 1995/09/17 18:43:58 dws Exp $
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
C  DEBUG
C
      IF(IPDTR.GT.1) WRITE(IOGDB,5)
  5   FORMAT(' *** ENTER PDGTFR ')
C
      ISTAT = 0
      IFLAG = 0
C
C        IF AT END OF FREE POOL SET NEXT TO START
C
 10   CONTINUE
      IF(IFREEN.LE.MXFRER) GO TO 50
      IF(IFLAG.EQ.0) GO TO 20
      WRITE(LPE,19)
 19   FORMAT(' **ERROR** NO AVAILABLE FREE POOL RECORDS')
      ISTAT = 1
      GO TO 999
 20   CONTINUE
      IFLAG = 1
      IFREEN = IFREE1
C
C        READ NEXT FREE POOL RECORD - SEE IF BEEING USED
C
 50   CONTINUE
      CALL PDRDFR(IFREEN,IFRBUF,ISTAT)
      IF(ISTAT.NE.0) GO TO 999
      IF(IFRBUF(1).EQ.-1) GO TO 100
      IFREEN = IFREEN + 1
      GO TO 10
 100  CONTINUE
C
C        FOUND AN UNUSED FREE POOL RECORD
C
      IFREC = IFREEN
      IFREEN = IFREEN + 1
      IF(IPDDB.GT.1) WRITE(IOGDB,199) IFREC
 199  FORMAT(' PDGTFR FOUND UNUSED RECORD ',I7)
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,200)
 200  FORMAT(' *** EXIT PDGTFR')
      RETURN
      END
