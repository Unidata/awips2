C MEMBER FCAL50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCCALC50
C @PROCESS LVL(77)
      REAL FUNCTION FCAL50( QS, QO, NDAYS, WQ, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, QAVE, RKPKS, NBASINS, NPR_PRDS,
     2 RKPOLD, WP_B_PRD, WP, WS_B, WS, IST_OP, FQ, FP, FS )

C    THIS ROUTINE CALCULATES THE ENTIRE OBJECTIVE
C    VALUE TAKING INTO THE ACCOUNT THE VALUES
C    FROM FLOW, PRECIP, AND STATES
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED VARIABLES
      DIMENSION QS(*), QO(*), RKPKS(*), WP_B_PRD(*),
     1 RKPOLD(*), WS_B(*), IST_OP(*)
      INTEGER NDAYS, NDQ_PER_PRD, NQ_PRDS, ISTART, NBASINS,
     1 NPR_PRDS
      REAL WQ, QAVE, WS, WP, FQ, FP, FS

      INCLUDE 'common/fcassm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/fcal50.f,v $
     . $',                                                             '
     .$Id: fcal50.f,v 1.1 1995/09/17 18:55:35 dws Exp $
     . $' /
C    ===================================================================
C

      FQ = 0.0
      FP = 0.0
      FS = 0.0

      FQ = FQCL50( QS, QO, NDAYS, WQ, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, QAVE)

      IF  ((ASSMPAR.EQ.1).OR.(ASSMPAR.EQ.3)) THEN
           FP = FPCL50( RKPKS, NBASINS, NPR_PRDS, WP_B_PRD,
     1 RKPOLD, WP )
      END IF

      IF ((ASSMPAR.EQ.2).OR.(ASSMPAR.EQ.3)) THEN
           FS = FSCL50( NBASINS, NPR_PRDS, RKPKS, WS_B, WS,
     1 IST_OP )
      END IF
      FCAL50 = FQ + FP + FS

      RETURN
      END
