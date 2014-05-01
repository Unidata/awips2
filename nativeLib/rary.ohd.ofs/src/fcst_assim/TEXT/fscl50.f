C MEMBER FSCL50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      REAL FUNCTION FSCL50( NBASINS, NPR_PRDS, RKPKS, WS_B,
     1 WS, IST_OP )
C
C    THIS ROUTINE CALCULTES THE STATE PORTION OF THE OBJECTIVE
C    VALUE.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED ARGUMENTS
      DIMENSION RKPKS(*), WS_B(*), IST_OP(*)
      INTEGER NBASINS, NPR_PRDS
      REAL WS

C    LOCAL ARGUMENTS
      INTEGER I
      REAL F_S
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/fscl50.f,v $
     . $',                                                             '
     .$Id: fscl50.f,v 1.1 1995/09/17 18:55:37 dws Exp $
     . $' /
C    ===================================================================
C

      FSCL50 = 0.0

      DO I = 1, NBASINS
         IF (IST_OP(I).EQ.1) THEN
               F_S =  ABS( RKPKS(I + NBASINS*NPR_PRDS) - 1.0 )
               FSCL50 = FSCL50 + F_S * WS_B(I)
          END IF
      END DO

      FSCL50 = FSCL50 * WS
      RETURN
      END
