C MEMBER NPRS50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE NPRS50( WP_B, WP_B_PRD, NBASINS, NPR_PRDS)
C
C    ROUTINE THAT LOADS WP_B TO WP_B_PRD IF SENSITIVITY OPTION
C    IS NOT INCLUDED
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C
C    PASSED ARGUMENTS
      INTEGER NBASINS, NPR_PRDS
      DIMENSION WP_B(*), WP_B_PRD(*)

C    LOCAL VARIABLES
      INTEGER I, J
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/nprs50.f,v $
     . $',                                                             '
     .$Id: nprs50.f,v 1.1 1995/09/17 18:55:41 dws Exp $
     . $' /
C    ===================================================================
C

      DO I = 1, NBASINS
          DO J = 1, NPR_PRDS
              WP_B_PRD( (I-1)*NPR_PRDS + J) = WP_B(I)
          END DO
      END DO

      END
