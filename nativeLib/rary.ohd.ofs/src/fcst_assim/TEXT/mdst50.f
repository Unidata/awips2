C MEMBER MDST50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE MDST50( NBASINS, IST_OP, ISTNF, C, IP_RRCO,
     1 NPR_PRDS, RKPKS, IP_RRPO, P, INV )
C
C    THIS ROUTINE MODIFIES THE STATES.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C
C    PASSED ARGUMENTS
      DIMENSION IST_OP(*), ISTNF(*), C(*), IP_RRCO(*), RKPKS(*),
     1 IP_RRPO(*), P(*)
      INTEGER NBASINS, NPR_PRDS, INV

      INCLUDE 'common/fdbug'

C    LOCAL ARGUMENTS
      INTEGER NSTATES, I, K
      REAL UZTWM, UZFWM, LZTWM, LZFSM, LZFPM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/mdst50.f,v $
     . $',                                                             '
     .$Id: mdst50.f,v 1.1 1995/09/17 18:55:39 dws Exp $
     . $' /
C    ===================================================================
C

      DO I = 1, NBASINS
        IF (IST_OP(I).EQ.1) NSTATES = 6   !!!! SAC-SMA OPTION
        DO K = 0, NSTATES-1
           IF (ISTNF(I+K).EQ.1) THEN
              IF (INV.EQ.0) THEN
                 C(IP_RRCO(I)+K) = C(IP_RRCO(I)+K) *
     1 RKPKS(I + NPR_PRDS * NBASINS)
              ELSE
                 C(IP_RRCO(I)+K) = C(IP_RRCO(I)+K) /
     1 RKPKS(I + NPR_PRDS * NBASINS)
              END IF
           END IF
        END DO
      END DO

      END
