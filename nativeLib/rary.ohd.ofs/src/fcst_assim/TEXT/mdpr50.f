C MEMBER MDPR50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE MDPR50( NBASINS, NPR_PRDS, NDP_PER_PRD,
     1  IP_PR, IPR_IDT, D, RKPKS, INV )
C
C    THIS ROUTINE MODIFIES PRECIPITATION.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED ARGUMENTS
      INTEGER NBASINS, NPR_PRDS, NDP_PER_PRD, INV
      DIMENSION  IP_PR(*), IPR_IDT(*), D(*), RKPKS(*)
      REAL MULT
      INCLUDE 'common/fctime'



C    LOCAL ARGUMENTS
      INTEGER I, J, K, IA, ISTART, IEND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/mdpr50.f,v $
     . $',                                                             '
     .$Id: mdpr50.f,v 1.1 1995/09/17 18:55:38 dws Exp $
     . $' /
C    ===================================================================
C

      DO I = 1, NBASINS
        DO J = 1, NPR_PRDS

          MULT = RKPKS(J + (I-1)*NPR_PRDS)
          IF (INV.EQ.1) MULT = 1/MULT
          CALL SMTS50( D, IP_PR(I), IPR_IDT(I), MULT, J,
     1 NDP_PER_PRD, NPR_PRDS )

        END DO
      END DO
      RETURN
      END
