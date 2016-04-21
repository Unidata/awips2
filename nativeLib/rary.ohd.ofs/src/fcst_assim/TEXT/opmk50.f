C MEMBER OPMK50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCOPMK50
C
C  ************   ROSENBROCK'S OPTIMIZATION METHOD  ***************
C @PROCESS LVL(77)
      SUBROUTINE OPMK50(NB,X,AB,ZB,M,MOPT,DELTF,VALUEF,IX,XN,XV,
     1 NBASINS, NPR_PRDS, NDP_PER_PRD, IP_PR, IPR_IDT, D_A, IST_OP,
     2 ISTNF, C_A, IP_RRCO, QS, QO, NDAYS, WQ, NDQ_PER_PRD, NQ_PRDS,
     3 ISTART, QAVE, RKPOLD, WP_B_PRD, WP, WS_B, WS, P_A,
     4 MP, MC, T_A, MT, TS,  MTS, MD, IHZERO, IP_RRPO, ITSIZE )

      REAL X(*),AB(ITSIZE,ITSIZE),ZB(3,ITSIZE)
C
      DIMENSION IX(*), XN(*), XV(*)
C
C    PASSED ARGUMENTS
      INTEGER NBASINS, NPR_PRDS, NDP_PER_PRD, NDAYS, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, MP, MC, MT, MTS, IHZERO

      REAL WQ, QAVE, WP, WS

      DIMENSION IP_PR(1), IPR_IDT(1), D_A(MD), IST_OP(1),
     1 ISTNF(1), C_A(MC), IP_RRCO(1), QS(1), QO(1), RKPOLD(1),
     2 WP_B_PRD(1), WS_B(1), P_A(MP), TS(MTS),
     3 IP_RRPO(1)
      INTEGER T_A(MT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/opmk50.f,v $
     . $',                                                             '
     .$Id: opmk50.f,v 1.1 1995/09/17 18:55:43 dws Exp $
     . $' /
C    ===================================================================
C

      A06=0.6
      AP2=2.
      A15=1.5
      A3=3.
      A5=5.
      A225=2.25
      A125=1.25
      AE10=1.E-10
      A10=10.
      AE12=1.E-12
      A1=1.
      A01=0.0001
      ICS=0
      DO 1 I=1,NB
      IF (X(I).EQ.0.) X(I)=A01
      IF (ABS(X(I)).LT.A01) X(I)=SIGN(A01,X(I))
    1 CONTINUE
      DO 3 I=1,NB
      ZB(1,I)=0.04
      ZB(2,I)=1.
      ZB(3,I)=X(I)
      DO 2 J=1,NB
    2 AB(J,I)=0.
    3 AB(I,I)=1.

    4 CALL OBJF50( F, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D_A, X, IST_OP, ISTNF, C_A, IP_RRCO, QS,
     2 QO, NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P_A, MP, MC, T_A, MT, TS,
     4 MTS, MD, IHZERO, IP_RRPO )

      F1=F
      M=1
    5 DO 20 L=1,NB
      AA=A06*ZB(1,L)
      B=0.
      CALL AUG50(AA,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)

      CALL OBJF50( F, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D_A, X, IST_OP, ISTNF, C_A, IP_RRCO, QS,
     2 QO, NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P_A, MP, MC, T_A, MT, TS,
     4 MTS, MD, IHZERO, IP_RRPO )

      M=M+1
      IF (F1.GT.F) GOTO 6
      A2=-2.
      A2=A2*AA
      CALL AUG50(A2,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)
      F2=F

      CALL OBJF50( F, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D_A, X, IST_OP, ISTNF, C_A, IP_RRCO, QS,
     2 QO, NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P_A, MP, MC, T_A, MT, TS,
     4 MTS, MD, IHZERO, IP_RRPO )

      M=M+1
      IF (F1.GT.F) GOTO 7
      CALL AUG50(AA,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)
      TOP=F-F2
      BOT=AP2*(F+F2-AP2*F1)
      GOTO 8
    7 AA=-AA
    6 F2=F1
      F1=F
      B=B+AA
      AA=A15*AA
      CALL AUG50(AA,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)

      CALL OBJF50( F, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D_A, X, IST_OP, ISTNF, C_A, IP_RRCO, QS,
     2 QO, NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P_A, MP, MC, T_A, MT, TS,
     4 MTS, MD, IHZERO, IP_RRPO )

      M=M+1
      IF (F1.GT.F) GOTO 6
      CALL AUG50(-AA,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)
      AA=AA/A15
      ZB(1,L)=AA
      BOT=A3*F2-A5*F1+AP2*F
      TOP=A225*F2-A125*F1-F
    8 IF (BOT.GT.AE10) GOTO 9
      GOTO 10
    9 D=AA*TOP/BOT
      CALL AUG50(D,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)

      CALL OBJF50( F, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D_A, X, IST_OP, ISTNF, C_A, IP_RRCO, QS,
     2 QO, NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P_A, MP, MC, T_A, MT, TS,
     4 MTS, MD, IHZERO, IP_RRPO )

      M=M+1
      IF (F.GT.F1) GOTO 11
      ZB(1,L)=AA+D
      F1=F
      B=B+D
      GOTO 12
   11 CALL AUG50(-D,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)
   10 IF (B.NE.0.) GOTO 12
      B=AA/A10
   12 DO 19 J=1,NB
   19 AB(J,L)=B*AB(J,L)
   20 CONTINUE
      CALL SCRB50(NB,X,F1,ICS,M,MOPT,DELTF,VALUEF,STORE)
      IF (ICS.EQ.2) GOTO 14
      DO 15 J=2,NB
      L=NB-J+1
      DO 15 I=1,NB
      AB(I,L)=AB(I,L)+AB(I,L+1)
   15 CONTINUE
      DO 27 J=1,NB
      IF (J.EQ.1) GOTO 16
      I=J-1
      DO 22 L=1,I
      AA=0.
      DO 17 K=1,NB
   17 AA=AB(K,J)*AB(K,L)+AA
      DO 21 K=1,NB
   21 AB(K,J)=AB(K,J)-AA*AB(K,L)
   22 CONTINUE
   16 AA=0.
      DO 23 K=1,NB
      IF (ABS(AB(K,J)).LT.AE12) GOTO 23
      AA=AB(K,J)*AB(K,J)+AA
   23 CONTINUE
      IF (AA.LT.AE12) AA=AE12
      AA=A1/SQRT(AA)
      DO 29 K=1,NB
   29 AB(K,J)=AA*AB(K,J)
   27 CONTINUE
      GOTO 5
   14 CONTINUE
  106 FORMAT(2X,2HM=,I4)
      RETURN
      END
