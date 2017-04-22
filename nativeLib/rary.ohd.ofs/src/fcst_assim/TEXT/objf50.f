C MEMBER OBJF50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCOBFN50
C @PROCESS LVL(77)
      SUBROUTINE OBJF50( VALUE, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D, RKPKS, IST_OP, ISTNF, C, IP_RRCO, QS, QO,
     2 NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P, MP, MC, T, MT, TS, MTS,
     4 MD, IHZERO, IP_RRPO )
C
C    THIS ROUTINE CALCULTES THE OBJECTIVE VALUE.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1

C    PASSED ARGUMENTS
      INTEGER NBASINS, NPR_PRDS, NDP_PER_PRD, NDAYS, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, MP, MC, MT, MTS, IHZERO, NOSTMD

      REAL VALUE, WQ, QAVE, WP, WS
      DIMENSION IP_PR(*), IPR_IDT(*), D(MD), RKPKS(*), IST_OP(*),
     1 ISTNF(*), C(MC), IP_RRCO(*), QS(*), QO(*), RKPOLD(*),
     2 WP_B_PRD(*), WS_B(*), P(MP), TS(MTS), IP_RRPO(*)
      INTEGER T(MTS)
C    EJM 01/11/95
      INCLUDE 'common/fcassm'

C    LOCAL VARIABLES
C    INV        - 0 MULT VALUES BY KPKS VALUES
C               - 1 MULT VALUES BY INVERSE KPKS VALUES
C    NOSTMD     - 0 MODIFY STATES
C               - 1 DON'T MODIFY STATES

      INTEGER INV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/objf50.f,v $
     . $',                                                             '
     .$Id: objf50.f,v 1.1 1995/09/17 18:55:42 dws Exp $
     . $' /
C    ===================================================================
C
      INV = 0
      IF ((ASSMPAR.EQ.1).OR.(ASSMPAR.EQ.3)) THEN
         CALL MDPR50( NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D, RKPKS, INV )
      END IF
      IF ((ASSMPAR.EQ.2).OR.(ASSMPAR.EQ.3)) THEN
         CALL MDST50( NBASINS, IST_OP, ISTNF, C, IP_RRCO,
     1 NPR_PRDS, RKPKS, IP_RRPO, P, INV)
      END IF

      CALL LDRV50( P,MP,C,MC,T,MT,TS,MTS,D,MD,IHZERO )

      VALUE = FCAL50( QS, QO, NDAYS, WQ, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, QAVE, RKPKS, NBASINS, NPR_PRDS,
     2 RKPOLD, WP_B_PRD, WP, WS_B, WS, IST_OP, FQ, FP, FS )

      INV = 1

      IF ((ASSMPAR.EQ.1).OR.(ASSMPAR.EQ.3)) THEN
         CALL MDPR50( NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D, RKPKS, INV )
      END IF

      IF ((ASSMPAR.EQ.2).OR.(ASSMPAR.EQ.3)) THEN
          CALL MDST50( NBASINS, IST_OP, ISTNF, C, IP_RRCO,
     1 NPR_PRDS, RKPKS, IP_RRPO, P, INV )
      END IF

      END
