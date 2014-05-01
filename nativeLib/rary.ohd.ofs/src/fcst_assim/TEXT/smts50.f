C MEMBER SMTS50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCTS50
C @PROCESS LVL(77)
      SUBROUTINE SMTS50( D, I_PR, I_IDT, MULT, PRD_NUM,
     1 NDP_PER_PRD, NPR_PRDS )
C
C     THIS ROUTINE TAKES A GIVEN TIME SERIES WITH
C     A GIVEN TIME STEP AND MULTIPLIES EACH VALUE BY
C     A MULTIPLIER
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

      INCLUDE 'common/fctime'



C    PASSED ARGUMENTS
C    D           - D ARRAY
C    I_PR        - A POINTER TO A SUBBASINS PRECIP T.S. IN THE D ARRAY
C    I__IDT      - A POINTER TO  A SUBBASINS PRECIP IDT IN THE D ARRAY
C    MULT        - MULTIPLIER TO USE ON A PARTICULAR SUBBASIN T.S.
C    PRD_NUM     - THE PERIOD OF WHICH THE MULT IS BEING APPLIED
C    NDP_PER_PRD - THE NUMBER OF DAYS PER PERIOD FOR THE PRECIPITATION

      DIMENSION D(*)
      INTEGER I_PR, I_IDT, PRD_NUM, NDP_PER_PRD, NPR_PRDS
      REAL MULT

C    LOCAL ARGUMENTS
C    ISTART        - STARTING POINT FOR THE MODIFICATION
C    IEND       - ENDING POINT FOR THE MODIFICATION
C    J              - COUNTER
      INTEGER ISTART, IEND, J
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/smts50.f,v $
     . $',                                                             '
     .$Id: smts50.f,v 1.1 1995/09/17 18:55:46 dws Exp $
     . $' /
C    ===================================================================
C


      ISTART = (IDA - IDADAT) * 24/I_IDT + (IHR-1)/I_IDT
     Z + NDP_PER_PRD * (PRD_NUM-1) * 24/I_IDT + PRD_NUM

      IF ( PRD_NUM .LT. NPR_PRDS ) THEN
         IEND =  (IDA - IDADAT) * 24/I_IDT + (IHR-1) /I_IDT
     Z +  NDP_PER_PRD * PRD_NUM *24/I_IDT  + PRD_NUM
      ELSE
         IEND =  (LDA - IDADAT) * 24/I_IDT + LHR/I_IDT
      END IF
      DO J = ISTART, IEND
         D( I_PR - 1 + J ) = D( I_PR -1 + J ) * MULT
      END DO
      RETURN
      END
