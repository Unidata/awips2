C MEMBER GCYINT
C  (from old member PPGCYINT)
C
      SUBROUTINE GCYINT(W, WN, WX, A, BI, E1, E2, G1, G2, CP, IRFRO,
     * NUM, Y, C, B)
C
C.....THIS IS THE SUBROUTINE THAT COMPUTES VALUES OF THE INTERIM
C.....RAINFALL-RUNOFF EQUATIONS "Y", "C", AND "B".
C
C.....THIS SUBROUTINE WAS ADAPTED FROM THE WGRFC ONECL PROGRAM.
C
C.....PROGRAM EXTENSIVELY MODIFIED FROM ORIGINAL 1/13/88.
C.....MODIFICATION DONE BY:   JERRY M. NUNN       WGRFC, FT. WORTH, TEX.
C
C.....HERE IS THE ARGUMENT LIST TO GCYINT:
C
C.....W      - WEEK NUMBER.
C.....WN     - WETTEST WEEK. (RAINFALL-RUNOFF CONSTANT).
C.....WX     - DRIEST WEEK. (RAINFALL-RUNOFF CONSTANT).
C.....A      - RAINFALL-RUNOFF CONSTANT.
C.....BI     - RAINFALL-RUNOFF CONSTANT.
C.....E1     - RAINFALL-RUNOFF CONSTANT.
C.....E2     - RAINFALL-RUNOFF CONSTANT.
C.....G1     - DERIVED RAINFALL-RUNOFF CONSTANT (E1-E2).
C.....G2     - DERIVED RAINFALL-RUNOFF CONSTANT (E2-E1).
C.....CP     - RAINFALL-RUNOFF CONSTANT.
C.....IRFRO  - RAINFALL-RUNOFF RELATION NUMBER ARRAY.
C.....NUM    - NUMBER OF Y, C, AND B COMPUTATIONS TO MAKE.
C.....Y      - INTERIM RESULT.
C.....C      - INTERIM RESULT.
C.....B      - INTERIM RESULT.
C
      DIMENSION WN(1), WX(1), A(1), IRFRO(1), Y(1), C(1), B(1)
      DIMENSION BI(1), E1(1), E2(1), G1(1), G2(1), CP(1)
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gcyint.f,v $
     . $',                                                             '
     .$Id: gcyint.f,v 1.1 1995/09/17 19:01:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA PI, PI2 /3.14159265, 1.57079632/
      DATA GRNF /4hGRNF/
C
  902 FORMAT(1H0, '*** ENTER SUBROUTINE GCYINT ***')
  903 FORMAT(1X, '*** EXIT SUBROUTINE GCYINT ***')
C
C.....SET INITIAL VALUES.
C
      IF(IPTRCE .GE. 8) WRITE(IOPDBG,902)
C
C.....INITIALIZE THE VARIABLES.
C
      DO 100 NP = 1, NUM
      B(NP) = 0.0
      Y(NP) = 0.0
      C(NP) = 0.0
  100 CONTINUE
C
C.....NOW COMPUTE B.
C
      DO 200 NP = 1, NUM
      B(NP) = (BI(NP) - A(NP))/2.0
  200 CONTINUE
C
C.....NOW COMPUTE Y AND C. DEPENDING UPON THE RELATION OF W TO WN AND
C.....WX...ONE OF THE SIX EQUATIONS SETS WILL BE USED.
C
      DO 1200 NP = 1, NUM
      IF(W .LE. WN(NP)) GOTO 300
      IF(W .GT. WX(NP)) GOTO 900
      GOTO 600
C
C.....EQUATIONS SETS 1 AND 2. (W .LE. WN(NP)).
C
  300 W1 = W + 52.0 - WX(NP)
      W2 = 52.0 + WN(NP) - WX(NP)
      W3 = WN(NP) - W
      W4 = W2/2.0
      Q  = PI*W1/W2
      IF(Q .GT. PI2) GOTO 400
      GOTO 500
C
C.....SET 1.
C
  400 Y(NP) = 1.0 - COS(PI - Q)**CP(NP)
      C(NP) = E1(NP) + (G1(NP)/2.0)*W3/W4
      GOTO 1200
C
C.....SET 2.
C
  500 Y(NP) = 1.0 + COS(Q)**CP(NP)
      C(NP) = E2(NP) + (G2(NP)/2.0)*W1/W4
      GOTO 1200
C
C.....EQUATION SETS 3 AND 4.  (W .GT. WN(NP) .AND. (W .LE. WX(NP)).
C
  600 W1 = W - WN(NP)
      W2 = WX(NP) - WN(NP)
      W3 = WX(NP) - W
      W4 = W2/2.0
      Q  = PI*W1/W2
      IF(Q .GT. PI2) GOTO 700
      GOTO 800
C
C.....SET 3.
C
  700 Y(NP) = 1.0 + COS(PI - Q)**CP(NP)
      C(NP) = E2(NP) + (G2(NP)/2.0)*W3/W4
      GOTO 1200
C
C.....SET 4.
C
  800 Y(NP) = 1.0 - COS(Q)**CP(NP)
      C(NP) = E1(NP) + (G1(NP)/2.0)*W1/W4
      GOTO 1200
C
C.....EQUATION SETS 5 AND 6. (W .GT. WX(NP)).
C
  900 W1 = W - WX(NP)
      W2 = 52.0 + WN(NP) - WX(NP)
      W3 = WN(NP) + 52.0 - W
      W4 = W2/2.0
      Q  = PI*W1/W2
      IF(Q .GT. PI2) GOTO 1000
      GOTO 1100
C
C.....SET 5.
C
 1000 Y(NP) = 1.0 - COS(PI - Q)**CP(NP)
      C(NP) = E1(NP) + (G1(NP)/2.0)*W3/W4
      GOTO 1200
C
C.... SET 6.
C
 1100 Y(NP) = 1.0 + COS(Q)**CP(NP)
      C(NP) = E2(NP) + (G2(NP)/2.0)*W1/W4
C
C.....END OF LOOP.
C
 1200 CONTINUE
C
      IF(IPTRCE .GE. 8) WRITE(IOPDBG,903)
      RETURN
      END
