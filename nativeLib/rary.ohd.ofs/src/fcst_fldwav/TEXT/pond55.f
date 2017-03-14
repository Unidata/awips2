      SUBROUTINE POND55(SAPOND,HSAP,QPOND,HPOND)
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/LE55V/NLEV,DHLV,NPOND,DTHLV,IDTHLV

      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'

      DIMENSION SAPOND(8,1),HSAP(8,1),QPOND(1),HPOND(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/pond55.f,v $
     . $',                                                             '
     .$Id: pond55.f,v 1.1 1999/04/23 18:08:48 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HPOND,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)
C
      DO 100 L=1,NPOND
      YY=HPOND(L)
      DO 50 KK=2,8
      K=KK
      IF(YY.GE.HSAP(K,L)) GO TO 60
      IF(K.EQ.8) GO TO 60
      IF(ABS(HSAP(K+1,L)-0.001).LT.0.01) GO TO 60
   50 CONTINUE
   60 KL=K-1
      DSA=(SAPOND(K,L)-SAPOND(KL,L))/(HSAP(K,L)-HSAP(KL,L))
      SA=SAPOND(KL,L)+DSA*(YY-HSAP(KL,L))
      IF(SA.LT.0.00001) SA=0.1
      HPOND(L)=YY+DT/43560.*QPOND(L)/SA
  100 CONTINUE
      RETURN
      END
