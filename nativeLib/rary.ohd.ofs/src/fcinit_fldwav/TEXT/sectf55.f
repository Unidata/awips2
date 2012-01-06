      SUBROUTINE SECTF55(NCS,J,I,Y,BL,AL,BR,AR,HS,BSL,BSR,ASL,ASR,
     . K1,K2,K9)
C       THIS SUBROUTINE COMPUTES THE FLOOD PLAIN CROSS-SECT. PROPERTIES.
      DIMENSION HS(K9,K2,K1),BSL(K9,K2,K1),BSR(K9,K2,K1)
      DIMENSION ASL(K9,K2,K1),ASR(K9,K2,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/sectf55.f,v $
     . $',                                                             '
     .$Id: sectf55.f,v 1.1 1999/04/23 18:09:45 dws Exp $
     . $' /
C    ===================================================================
C

      DO 20 K=2,NCS
      KT=K
      IF(Y.LE.HS(K,I,J)) GO TO 30
   20 CONTINUE
   30 KL=KT-1
      DH=HS(KT,I,J)-HS(KL,I,J)
      DY=Y-HS(KL,I,J)
      DBL=(BSL(KT,I,J)-BSL(KL,I,J))/DH
      DBR=(BSR(KT,I,J)-BSR(KL,I,J))/DH
      BL=BSL(KL,I,J)+DBL*DY
      BR=BSR(KL,I,J)+DBR*DY
      AL=ASL(KL,I,J)+0.5*(BSL(KL,I,J)+BL)*DY
      AR=ASR(KL,I,J)+0.5*(BSR(KL,I,J)+BR)*DY
      IF(BL.LT.0.01) BL=0.01
      IF(BR.LT.0.01) BR=0.01
      RETURN
      END

