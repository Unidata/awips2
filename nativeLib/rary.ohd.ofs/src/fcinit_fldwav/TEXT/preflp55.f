      SUBROUTINE PREFLP55(JNK,JN,NCS,NB,SNC,BS,AS,BSL,BSR,
     & ASL,ASR,K2,K9)

C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION NB(1),SNC(K9,K2,1),BS(K9,K2,1),AS(K9,K2,1)
      DIMENSION BSL(K9,K2,1),BSR(K9,K2,1),ASL(K9,K2,1),ASR(K9,K2,1)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/preflp55.f,v $
     . $',                                                             '
     .$Id: preflp55.f,v 1.2 2004/09/24 22:46:29 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/'PREFLP55'/

      CALL FPRBUG(SNAME, 1, 55, IBUG)
      DO 30 J=1,JN
      N=NB(J)
C  WEIGHTED SINUOSITY COEFF.
      IF(JNK.GE.4 .AND.IBUG.EQ.1) WRITE(IODBUG,125)
  125 FORMAT(///)
      DO 10 I=1,N
      IP=I+1
      SMA=0.
      DO 8 K=2,NCS
      KM=K-1
      APC=0.5*(AS(K,I,J)-AS(KM,I,J)+AS(K,IP,J)-AS(KM,IP,J))
      APL=0.5*(ASL(K,I,J)-ASL(KM,I,J)+ASL(K,IP,J)-ASL(KM,IP,J))
      APR=0.5*(ASR(K,I,J)-ASR(KM,I,J)+ASR(K,IP,J)-ASR(KM,IP,J))
      SMA=SMA+SNC(K,I,J)*APC+1.0*APL+1.0*APR
      AT=0.5*(AS(K,I,J)+ASL(K,I,J)+ASR(K,I,J)+
     & AS(K,IP,J)+ASL(K,IP,J)+ASR(K,IP,J))
      DUMY=SMA/AT
      IF(DUMY.LE.1.0) DUMY=1.0
      SNC(K,I,J)=DUMY
    8 CONTINUE
      IF(JNK.LT.4) GO TO 10
      IF(IBUG.EQ.1) WRITE(IODBUG,126) I,J,(SNC(K,I,J),K=1,NCS)
  126 FORMAT(5X,'SNC(K,',I3,1H,,I2,')=',8F10.2)
   10 CONTINUE
C  TOP WIDTH AND AREA OF COMPOSITE CHANNEL
      DO 20 I=1,N
      DO 18 K=1,NCS
      BS(K,I,J)=BS(K,I,J)+BSL(K,I,J)+BSR(K,I,J)
      AS(K,I,J)=AS(K,I,J)+ASL(K,I,J)+ASR(K,I,J)
   18 CONTINUE
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
