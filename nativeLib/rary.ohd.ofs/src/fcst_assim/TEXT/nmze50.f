C MEMBER NMZE50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE NMZE50( NBASINS, W_B )
C
C
C    THIS ROUTINE NORMALIZES AN ARRAY OF WEIGHTS
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C
C

C    PASSED ARGUMENTS
      INTEGER NBASINS
      DIMENSION W_B(*)

C    LOCAL VARIABLES
      INTEGER I
      REAL SUM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/nmze50.f,v $
     . $',                                                             '
     .$Id: nmze50.f,v 1.1 1995/09/17 18:55:41 dws Exp $
     . $' /
C    ===================================================================
C

C    INSURE DIVIDE BY ZERO ERROR DOES NOT OCCUR
      SUM = 0.00001

      DO I = 1, NBASINS
          SUM = SUM + W_B(I)
      END DO

      DO I = 1, NBASINS
          W_B(I) = W_B(I) / SUM
      END DO

      END
