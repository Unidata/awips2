C   MEMBER MPSTAT
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/26/95.08:57:29 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE MPSTAT (NUMM5,NUMM6,NUM,JIMM1,SUN3,SUN4,
     *              RSTA,K,KK,X25,X26,NN5,X21,X23,SUN,C)
C
C  THIS ROUTINE DEVELOPS STATISTICS TO
C  EVALUATE THE PRECIPITATION ESTIMATING PROCEDURES
C
C     SUN(1,JJ) IS MONTHLY SUMS FOR STATION REPLACED BY DUMMY
C     SUN(2,JJ) IS MONTHLY SUMS FOR DUMMY STATION WHICH REPLACES
C               EXISTING STATION
C
      DIMENSION    SUN(2,M3)
      DIMENSION    C(M1,817)
C
      INTEGER      RSTA
      INTEGER      DEBUG,DEBUGA
C
      INCLUDE 'common/ionum'
      COMMON  /MAP/  DEBUG,DEBUGA
      COMMON  /DIM/  M1,M2,M3,M4,M5,M6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpstat.f,v $
     . $',                                                             '
     .$Id: mpstat.f,v 1.1 1997/01/27 15:33:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (DEBUG.EQ.1.OR.DEBUGA.EQ.1) WRITE (IPR,130)
C
      JIMM1=JIMM1+1
      JJ=JIMM1
C
C
      DO 80 I=1,NUM
      IF (I.EQ.NUMM5) GO TO 10
      IF (I.EQ.NUMM6) GO TO 60
      GO TO 80
C
10    IF (I.LE.RSTA) GO TO 40
20    CC=C(NUMM5,K)
      IF (CC.GT.999.0) GO TO 30
      SUN(1,JJ)=SUN(1,JJ)+CC
30    K=K+24
      IF (K.GT.KK) GO TO 80
      GO TO 20
C
40    DO 50 J=25,768
      IF (C(I,J).GT.999.0) GO TO 50
      SUN(1,JJ)=SUN(1,JJ)+ C(I,J)
50    CONTINUE
      GO TO 80
C
60    DO 70 J=25,KK
      SUN(2,JJ)=SUN(2,JJ)+C(I,J)
70    CONTINUE
C
80    CONTINUE
C
      SUN3=SUN3+SUN(1,JJ)
      SUN4=SUN4 + SUN(2,JJ)
      IF (NUMM5.LE.RSTA)  GO TO 90
      GO TO 120
C
90    DO 110 J=25,768
      IF (C(NUMM5,J).GT.999.0)  GO TO 110
      IF (C(NUMM5,J).GT.0.0)    GO TO 100
      IF (C(NUMM6,J).GT.0.0)    GO TO 100
      GO TO 110
C
C  COMPUTE BIAS AND RMS
100   NN5=NN5+1
C
      X20=C(NUMM6,J)-C(NUMM5,J)
      X21=X21+X20
      X22=X20*X20
      X23=X23+X22
C
110   CONTINUE
C
      X25=X21/NN5*100.0
      X26=SQRT(X23/NN5)
120   CONTINUE
C
      RETURN
C
130   FORMAT (' *** ENTER MPSTAT')
C
      END
