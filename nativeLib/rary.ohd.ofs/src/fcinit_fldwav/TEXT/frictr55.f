      SUBROUTINE FRICTR55(NCML,CMR,YQCM,J,L,YQ,CMM,DCM,K1,K7,K8)
      DIMENSION CMR(K8,K7,K1),YQCM(K8,K7,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/frictr55.f,v $
     . $',                                                             '
     .$Id: frictr55.f,v 1.1 1999/04/23 18:09:27 dws Exp $
     . $' /
C    ===================================================================
C

      IF(YQ.LT.YQCM(1,L,J)) CMM=CMR(1,L,J)
      IF(YQ.LT.YQCM(1,L,J)) GO TO 20
      IF(YQ.GT.YQCM(NCML,L,J)) CMM=CMR(NCML,L,J)
      IF(YQ.GT.YQCM(NCML,L,J)) GO TO 20
      DO 10 KK=2,NCML
      KT=KK
      IF (YQ.LE.YQCM(KT,L,J)) GO TO 12
   10 CONTINUE
   12 KL=KK-1
      DH=YQCM(KT,L,J)-YQCM(KL,L,J)
      IF(ABS(DH).LE.0.01) DH=0.01
      DCM=(CMR(KT,L,J)-CMR(KL,L,J))/DH
      CMM=CMR(KL,L,J)+DCM*(YQ-YQCM(KL,L,J))
   20 IF(CMM.LT.0.001) CMM=0.01
      RETURN
      END
