C MEMBER FPCL50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      REAL FUNCTION FPCL50( RKPKS, NBASINS, NPR_PRDS, WP_B_PRD,
     1 RKPOLD, WP )
C
C    THIS ROUTINE CALCULTES THE PRECIPITATION PORTION OF THE OBJECTIVE
C    VALUE.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED ARGUMENTS
      DIMENSION RKPKS(*), WP_B_PRD(*), RKPOLD(*)
      INTEGER NBASINS, NPR_PRDS
      REAL WP

C    LOCAL ARGUMENTS
      INTEGER IA, I, J
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/fpcl50.f,v $
     . $',                                                             '
     .$Id: fpcl50.f,v 1.1 1995/09/17 18:55:35 dws Exp $
     . $' /
C    ===================================================================
C

      FPCL50 = 0.0
      FPC = 0.0
      IA = 0

      DO I = 1, NBASINS
           DO J = 1, NPR_PRDS
              IA = IA + 1
              FPC = FPC +
     1 WP_B_PRD(IA) * ABS( RKPKS(IA) - RKPOLD(IA) )
           END DO
      END DO
      FPCL50 = FPC * WP
      RETURN
      END
