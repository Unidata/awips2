      SUBROUTINE SECBR55(BRGHS,BRGBS,AREA,TW,DTW,KL,J,YY,TOPN,K1,K16)
C
C  SECBR COMPUTES TOP WIDTH AND FLOW AREA AT WATER DEPTH YY
C  OF BRIDGE OPENING
C
      INCLUDE 'common/fdbug'

      DIMENSION BRGHS(8,K16,K1),BRGBS(8,K16,K1),TOPN(K16,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/secbr55.f,v $
     . $',                                                             '
     .$Id: secbr55.f,v 1.1 1999/04/23 18:08:53 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HSECB,4HR55 /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      YX=YY
      IF(YX.GT.BRGHS(1,KL,J)) GO TO 7
      DTW=0.
      TW=0.01
      A=TW*(BRGHS(1,KL,J)-YX)
      GO TO 30
    7 IF(YX.GE.TOPN(KL,J)) YX=TOPN(KL,J)-0.01
      DO 10 L=2,8
      LU=L
      IF(YX.LE.BRGHS(L,KL,J)) GO TO 20
   10 CONTINUE
   20 LL=LU-1
      DTW=(BRGBS(LU,KL,J)-BRGBS(LL,KL,J))/
     &(BRGHS(LU,KL,J)-BRGHS(LL,KL,J))
      TW=BRGBS(LL,KL,J)+DTW*(YX-BRGHS(LL,KL,J))
      A=0.
      IF(LL.EQ.1) GO TO 28
      DO 27 K=2,LL
   27 A=A+0.5*(BRGBS(K,KL,J)+BRGBS(K-1,KL,J))*
     &(BRGHS(K,KL,J)-BRGHS(K-1,KL,J))
   28 A=A+0.5*(BRGBS(LL,KL,J)+TW)*(YX-BRGHS(LL,KL,J))
      AREA=A
   30 RETURN
      END
