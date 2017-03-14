      SUBROUTINE CONV55(QKC,HKC,BEV,NKC,J,I,Y,QK,DQK,BET,DBE,IH,K1,K2)
C  Q OR H OR CONVEYANCE CURVE CONVERSION
C  IH=1 -- H = F(Q)
C  IH=0 -- Q = F(H); QKC = F(HKC)
      COMMON/FLP55/KFLP

      INCLUDE 'common/fdbug'

      DIMENSION QKC(30,K2,K1),HKC(30,K2,K1),BEV(30,K2,K1),NKC(K2,K1)
      DIMENSION SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/conv55.f,v $
     . $',                                                             '
     .$Id: conv55.f,v 1.1 1999/04/23 18:08:24 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HCONV,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      NC=NKC(I,J)
      IF(IH.EQ.1) GO TO 20
      DO 10 KK=2,NC
      K=KK
      IF(Y.LE.HKC(KK,I,J)) GO TO 15
   10 CONTINUE
   15 L=K-1
      DH=HKC(K,I,J)-HKC(L,I,J)
      IF(ABS(DH).LT.0.01) DH=0.01
      DQK=(QKC(K,I,J)-QKC(L,I,J))/DH
      QK=QKC(L,I,J)+DQK*(Y-HKC(L,I,J))
      DBE=(BEV(K,I,J)-BEV(L,I,J))/DH
      BET=BEV(L,I,J)+DBE*(Y-HKC(L,I,J))
      GO TO 50
   20 DO 25 KK=2,NC
      IF(QK.LE.QKC(KK,I,J)) GO TO 30
   25 CONTINUE
   30 L=K-1
      DQ=QKC(K,I,J)-QKC(L,I,J)
      IF(ABS(DQ).LT.0.01) DQ=0.01
      DY=(HKC(K,I,J)-HKC(L,I,J))/DQ
      Y=HKC(L,I,J)+DY*(QK-QKC(L,I,J))
   50 RETURN
      END
