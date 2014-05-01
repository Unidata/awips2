      SUBROUTINE MIXI55(PO,JNK,J,LRT,IRT1,IRTN,YU,QU,HS,AS,IFR,KSP,KS1,
     * KSN,LRMIX,YCR,KRCH,DFR,FRC,K1,K2,K5,K9)
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*)
      DIMENSION YU(K2,K1),QU(K2,K1),HS(K9,K2,K1),AS(K9,K2,K1)
      DIMENSION IFR(K2,K1),KSP(K2,K5,K1),KS1(K2,K5,K1),KSN(K2,K5,K1)
      DIMENSION LRMIX(K5,K1),YCR(K2,K1),KRCH(K2,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mixi55.f,v $
     . $',                                                             '
     .$Id: mixi55.f,v 1.1 1999/04/23 18:08:44 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HMIXI,4H55  /

      CALL FPRBUG(SNAME,1,55,IBUG)

      DFRM=FRC-DFR
      DFRP=FRC+DFR
      L=1
      LN=KSN(1,LRT,J)
      DO 10 I=IRT1,IRTN
      Y=YU(I,J)
      CALL SECT55(PO(LCPR),AS,PO(LOBS),HS,PO(LOASS),PO(LOBSS),J,I,Y,
     . PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      F=ABS(QU(I,J)/(A*SQRT(32.2*A/B)))
      IFR(I,J)=2
      IF(F.LT.DFRM) IFR(I,J)=0
      IF(F.GT.DFRP) IFR(I,J)=1
      IF(AS(1,I,J).GE.0.01) THEN
        YCR(I,J)=HS(1,I,J)
        IFR(I,J)=0
        GO TO 10
      END IF
      YMN=HS(1,I,J) 
      DO 2 LL=1,NCS
      LST=NCS-LL+1
      IF(HS(LST,I,J).GT.0.0001) GO TO 4
    2 CONTINUE
    4 YMX=HS(LST,I,J)
      QIX=QU(I,J)
      YC=YCR(I,J)
      CALL HCRIT55(PO,1,J,I,YC,QIX,YMN,YMX,ITC,K1,K2,K9)
      YCR(I,J)=YC
   10 CONTINUE
      IB=IRT1+1
      IE=IRTN-1
      KS1(1,LRT,J)=1
      IF(JNK.GT.9) WRITE(IPR,4097) (IFR(I,J),I=IRT1,IRTN)
      DO 12 I=IB,IE
      CALL MIX155(J,I,KFRM,KFR,KFRP,IFR,K1,K2)
      IF(KFRM.NE.KFRP) GO TO 12
      IF(KFR.EQ.KFRM) GO TO 12
      IFR(I,J)=2
   12 CONTINUE
      DO 13 I=IB,IE
      CALL MIX155(J,I,KFRM,KFR,KFRP,IFR,K1,K2)
      IF(KFRM.NE.KFRP) GO TO 13
      IF(KFR.EQ.KFRM) GO TO 13
      KRA=IABS(KRCH(I,J))
      IF(KRA.LT.10 .OR. KRA.GT.30) IFR(I,J)=IFR(I-1,J)
   13 CONTINUE
      LR=1
      IF(KRCH(1,J).LT.10.OR.KRCH(1,J).GT.30) IFR(1,J)=IFR(2,J)
      IFR(IRTN,J)=IFR(IE,J)
      KS1(1,LRT,J)=1
      KSP(1,LRT,J)=0
      IF(IFR(1,J).GE.1) KSP(1,LRT,J)=1
      DO 50 I=IB,IE
      CALL MIX155(J,I,KFRM,KFR,KFRP,IFR,K1,K2)
      IF(KFRM.EQ.KFR) GO TO 50
      IF(KFR.EQ.1) GO TO 30
C         SUPER TO SUB
      KSN(LR,LRT,J)=I-1
      LR=LR+1
      KS1(LR,LRT,J)=I
      KSP(LR,LRT,J)=0
      GO TO 50
C         SUB TO SUPER
   30 KSN(LR,LRT,J)=I
      LR=LR+1
      KS1(LR,LRT,J)=I
      KSP(LR,LRT,J)=1
   50 CONTINUE
      LRMIX(LRT,J)=LR
      KSN(LR,LRT,J)=IRTN
      IF(JNK.GT.9) WRITE(IPR,4097) (IFR(I,J),I=IRT1,IRTN)
      DO 60 L=1,LR
      IF(JNK.GE.9)
     * WRITE(IPR,4099) L,KSP(L,LRT,J),KS1(L,LRT,J),KSN(L,LRT,J)
   60 CONTINUE
      LRMIX(LRT,J)=LR
 4097 FORMAT(10X,4HIFR=,50I2)
 4099 FORMAT(10X,2HL=,I5,5X,4HKSP=,I3,5X,4HKS1=,I3,5X,4HKSN=,I3)
      RETURN
      END
