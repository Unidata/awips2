C-----------------------------------------------------------------------
C       THIS IS A GATEWAY FOR KALMAN FILTER DEVELOPED BY DR.MING JIN
C       IN 1992 AS NATIONAL RESEARCH COUNCIL ASSOCIATE
C-----------------------------------------------------------------------
C
C       Y1=(YC,QC)=Y(j-1|j-1), Y2=(YU,QU)=Y(j|j-1), are state variables.
C       When exit, Y2 and (YU,QU) are updated, Y2(j|j) .
C       P = covariance of Y1 (when enter) and Y2 (when exit).
C       Q = covariance of noise of St.Venant equations.
C       R = covariance of stage observations.
C       H = observation matrix.
C       C = (dF/dYj)
C       D = (dF/dYj-1)
C       E = a spare matrix for matrix operations.
C       AK = Kalman gain matrix.
C       OBS = observed data for time t(j)
C
C-----------------------------------------------------------------------
      SUBROUTINE MJIN55(PO,JR,KC,NS,NO,YU,QU,YC,QC,K1,K2,K3,K4,K9,
     #  F1,DT,Y1,Y2,P,Q,R,H,OBS,C,D,E,V,AK,STT,LTSTT,NGS,K7,K8,XX,
     #  T,NGAGE,K16)

      DIMENSION PO(*),STT(*),LTSTT(*),YU(K2,K1),QU(K2,K1),YC(K2,K1)
      DIMENSION QC(K2,K1),NGAGE(K1),Y1(2*NS),Y2(2*NS),P(2*NS,2*NS)
      DIMENSION Q(2*NS,2*NS),R(NO,NO),OBS(NO),H(NO,2*NS),V(10,NS)
      DIMENSION AK(2*NS,NO),NGS(K4,K1),XX(K2,K1),C(2*NS,2*NS)
      DIMENSION D(2*NS,2*NS),E(2*NS,2*NS),T(4,1)

      INCLUDE 'common/ofs55'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mjin55.f,v $
     . $',                                                             '
     .$Id: mjin55.f,v 1.1 1999/04/23 18:08:44 dws Exp $
     . $' /
C    ===================================================================
C

      NS2=2*NS
      DO 200 L=1,NS
      Y1(2*L)=YC(L,JR)
      Y2(2*L)=YU(L,JR)
      Y1(2*L-1)=QC(L,JR)
      Y2(2*L-1)=QU(L,JR)
200   CONTINUE
      LIJR=LCAT21(I,JR,NGAGE)
      LST=LTSTT(LIJR)-1
      DO 220 I=1,NO
      OBS(I)=STT(KC+LST)
220   CONTINUE
      CALL FIL055(NS,NO,KC,P,R,Q,H,NGS,JR,K1,K4)
      CALL FTMV55(Z,JR,NS,Y1,V,K1,K2,K7,K8,K9,PO(LONQCM))
      CALL JAGD55(NS,Y1,V,F1,DT,D,XX(1,JR),K1,K16,JR,PO(LONLAD),
     . PO(LOLAD))
      CALL JAGC55(NS,C,T,PO(LOKU+JR-1),PO(LOKD+JR-1),V,Y1)
      CALL VERS55(C,E,NS)
      CALL TIME55(E,D,C,NS2,NS2,NS2)
      CALL FILT55(Y2,C,H,P,Q,R,AK,OBS,NS,NO,D,E)
C        CALL FTBC(JR,KC,H,OBS,NO,NS,YU,K1,K2,K3,PO(LOKU),PO(LOKD),
C     #            ST1,LTST1,STN,LTSTN))
      DO 300 L=1,NS
      YU(L,JR)=Y2(2*L)
      QU(L,JR)=Y2(2*L-1)
300   CONTINUE
      RETURN
      END
