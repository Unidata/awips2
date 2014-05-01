C MODULE MTESTD
C-----------------------------------------------------------------------
C
      SUBROUTINE MTESTD (NSTA,IP,IL,IRGCK,MONTH,IYEAR,LAST,
     *   MXSTA,X,Y,TMM,NDUMST,PXNAME)
C
C  ROUTINE TO ESTIMATE MISSING TEMP DATA FOR NON-MOUNTAINOUS AREAS. 
C  ELEVATION IS NOT CONSIDERED.
C
      INTEGER ISTAQ    
      DIMENSION X(MXSTA),Y(MXSTA),TMM(MXSTA,66),NDUMST(MXSTA),
     *   PXNAME(MXSTA,5)
      DIMENSION ISTAQ(4),DQ(4),RDQ(4)
      DIMENSION TIME(2)
C
      INCLUDE 'uiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/mtestd.f,v $
     . $',                                                             '
     .$Id: mtestd.f,v 1.3 2002/02/11 18:56:25 dws Exp $
     . $' /
C    ===================================================================
C
      DATA TIME/3HMIN,3HMAX/
C
C
      DO 140 IRG=1,NSTA
C
      DO 130 I=IP,IL
C
      IF (TMM(IRG,I).LT.998.0) GO TO 130
C
C  FIND NEAREST VALID STATION IN EACH QUADRANT
      ISTAQ(1)=0
      ISTAQ(2)=0
      ISTAQ(3)=0
      ISTAQ(4)=0
      DQ(1)=99999.
      DQ(2)=99999.
      DQ(3)=99999.
      DQ(4)=99999.
      DO 80 JRG=1,NSTA
      IF ((NDUMST(IRG).EQ.1).AND.(JRG.EQ.IRGCK)) GO TO 80
      IF (TMM(JRG,I).GT.998.0) GO TO 80
      DELX=X(JRG)-X(IRG)
      DELY=Y(JRG)-Y(IRG)
      D2=DELX*DELX+DELY*DELY
      IF (D2.LT.1.0)D2=1.0
      D=SQRT(D2)
      IF (DELX)10,30,20
10    IF (DELY)40,50,50
20    IF (DELY)60,60,70
30    IF (DELY)40,60,70
40    IF (D.GT.DQ(1)) GO TO 80
      DQ(1)=D
      ISTAQ(1)=JRG
      GO TO 80
50    IF (D.GT.DQ(4)) GO TO 80
      DQ(4)=D
      ISTAQ(4)=JRG
      GO TO 80
60    IF (D.GT.DQ(2)) GO TO 80
      DQ(2)=D
      ISTAQ(2)=JRG
      GO TO 80
70    IF (D.GT.DQ(3)) GO TO 80
      DQ(3)=D
      ISTAQ(3)=JRG
80    CONTINUE
      SUMD=0.0
      DO 90 JRG=1,4
         IF (ISTAQ(JRG).EQ.0) GO TO 90
         RDQ(JRG)=1.0/DQ(JRG)
         SUMD=SUMD+RDQ(JRG)
90       CONTINUE
      IF (SUMD.GT.0.0005) GO TO 110
         IF(I.GT.((LAST*2)+2).OR.I.LT.3) GO TO 130
         ID=(I-1)/2
         IMM=I-(ID*2)
         WRITE(LP,100) (PXNAME(IRG,J),J=1,5),TIME(IMM),MONTH,ID,IYEAR
100   FORMAT (' ALL ESTIMATORS MISSING FOR ',5A4,2X,A3,' TEMP',
     1   5X,I2,'/',I2,'/',I4)
         GO TO 130
110   EST=0.0
      DO 120 JRG=1,4
         IF (ISTAQ(JRG).EQ.0) GO TO 120
         RDQ(JRG)=RDQ(JRG)/SUMD
         KRG=ISTAQ(JRG)
         EST=EST+TMM(KRG,I)*RDQ(JRG)
120   CONTINUE
C  ADD 2000.0 TO INSURE ESTIMATED STATION WILL NOT BE USED TO ESTIMATE
C  ANOTHER STATION.
      TMM(IRG,I)=EST+2000.0
C
130   CONTINUE
C
140   CONTINUE
C
      RETURN
C
      END
