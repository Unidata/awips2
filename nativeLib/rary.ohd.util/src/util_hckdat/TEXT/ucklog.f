C  MEMBER UCKLOG
C======================================================================
       SUBROUTINE UCKLOG(IS,IE,NUM,IERR)
C======================================================================
C          SUBROUTINE:  UCKLOG
C             VERSION:  1.0.0
C                DATE: 8-17-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C----------------------------------------------------------------------
C  DESCRIPTION:
C    ROUTINE TO CHECK A SPECIFIED FIELD FOR A VALID LOGICAL VALUE
C    AND RETURN THAT VALUE OR A VALUE OF 1 FOR IERR
C      (FROM OLD MODULE HCLCK1)
C       JTOSTROWSKI - 3/93
C----------------------------------------------------------------------
C  ARGUMENT LIST:
C    NAME    TYPE  I/O   DIM   DESCRIPTION
C    ----    ----  ---   ---   -------------------------------
C    IS       I     I     1     STARTING COLUMN
C    IE       I     I     1     ENDING COLUMN
C    NUM      I     O     1     LOGICAL VALUE IF FOUND
C    IERR     I     O     1     ERROR INDICATOR
C                                 0 = LOGICAL FOUND
C                                 1 = LOGICAL NOT FOUND
C
C----------------------------------------------------------------------
C  DECLARATION SECTION
C---------------------
C
      INCLUDE 'ufreei'
      INCLUDE 'udsi'
      INCLUDE 'uio'
      INCLUDE 'udatas'
C
      INTEGER VALUE(2),T1,T(2),F1,F(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_hckdat/RCS/ucklog.f,v $
     . $',                                                             '
     .$Id: ucklog.f,v 1.1 1995/09/17 19:04:23 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA T1/4h.T. /, T(1)/4h.TRU/, T(2)/4hE.  /,
     *     F1/4h.F. /, F(1)/4h.FAL/, F(2)/4hSE. /
C
C----------------------------------------------------------------------
C  EXECUTION SECTION
C---------------------
C
      IERR = 0
C
C  PACK THE FIELD
C
      IF(IS.GT.IE) GO TO 99
      KOUNT = IE - IS + 1
      IF(KOUNT.GT.8) GO TO 99
      CALL UPACK1(IBUF(IS),VALUE,KOUNT)
C
C  CHECK FIELD FOR LOGICAL
C
      IF(KOUNT.GT.4) GO TO 50
C
      VALUE(2) = IBLNK
      IF(VALUE(1).EQ.T1) GO TO 100
      IF(VALUE(1).EQ.F1) GO TO 200
      GO TO 99
C
 50   CONTINUE
      CALL UNAMCP(VALUE,T,ISTAT)
      IF(ISTAT.EQ.0) GO TO 100
      CALL UNAMCP(VALUE,F,ISTAT)
      IF(ISTAT.EQ.0) GO TO 200
C
C  NO LOGICAL IN FIELD
C
 99   CONTINUE
      IERR = 1
      GO TO 999
C
C  LOGICAL FOUND - TRUE
C
 100  CONTINUE
      NUM = 1
      GO TO 999
C
C  LOGICAL FOUND - FALSE
C
 200  CONTINUE
      NUM = 0
C
C  WRITE DEBUG AND RETURN
C
 999  CONTINUE
      IF(NOBUG.EQ.3) WRITE(LPD,1000) IS,IE,VALUE,IERR,NUM
 1000 FORMAT(' SUBROUTINE UCKLOG EXECUTED, START = ',I2,
     1 ' END = ',I2,' VALUE = ',2A4,' IERR = ',I2,' NUM = ',I2)
      RETURN
      END
