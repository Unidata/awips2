C
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE MEGWP (NSTA,X,Y,W,N)
C
C  ROUTINE TO READ AREA GRID MAPS AND COMPUTE POINT WEIGHTS
C
      DIMENSION X(25),Y(25),IGP(80),SW(25),W(10,25)
C
      INCLUDE 'uiox'
      COMMON /MEESTX/ ISTAQ(4),DQ(4),RDQ(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/megwp.f,v $
     . $',                                                             '
     .$Id: megwp.f,v 1.1 1997/03/17 17:41:33 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DO 10 IRG=1,NSTA
         SW(IRG)=0.0
10       CONTINUE
C
C  BEGIN GRID POINT ROW LOOP
      DO 130 I=1,80
         J=81-I
         READ (ICD,160) (IGP(K),K=1,80)
         YY=J
C     BEGIN GRID POINT COLUMN LOOP
         DO 120 K=1,80
C         CHECK IF GRID POINT IS IN THE AREA
            IF (IGP(K).EQ.0) GO TO 120
            ISTAQ(1)=0
            ISTAQ(2)=0
            ISTAQ(3)=0
            ISTAQ(4)=0
            DQ(1)=99999.0
            DQ(2)=99999.0
            DQ(3)=99999.0
            DQ(4)=99999.0
            XX=K
C         BEGIN LOOP TO CALCULATE THE DISTANCE FROM THE GRID
C         POINT TO EACH STATION
            DO 90 IRG=1,NSTA
            DELX=XX-X(IRG)
            DELY=YY-Y(IRG)
            D2=DELX*DELX+DELY*DELY
            IF (D2.LT.1.0) D2=1.0
            D=SQRT(D2)
C         DETERMINE WHICH QUADRANT THE STATION LIES IN AND SELECT
C         THE CLOSEST STATION IN THE QUADRANT TO THE GRID POINT
            IF (DELX) 20,40,30
20          IF (DELY) 50,60,60
30          IF (DELY) 70,70,80
40          IF (DELY) 50,70,80
50          IF (D.GT.DQ(1)) GO TO 90
            DQ(1)=D
            ISTAQ(1)=IRG
            GO TO 90
60          IF (D.GT.DQ(4)) GO TO 90
            DQ(4)=D
            ISTAQ(4)=IRG
            GO TO 90
70          IF (D.GT.DQ(2)) GO TO 90
            DQ(2)=D
            ISTAQ(2)=IRG
            GO TO 90
80          IF (D.GT.DQ(3)) GO TO 90
            DQ(3)=D
            ISTAQ(3)=IRG
90          CONTINUE
C         CALCULATE THE SUM OF (1/DISTANCE) OF THE SELECTED STATONS
            SUMD=0.0
            DO 100 IRG=1,4
               IF (ISTAQ(IRG).EQ.0) GO TO 100
               RDQ(IRG)=1.0/DQ(IRG)
               SUMD=SUMD+RDQ(IRG)
100            CONTINUE
C         CALCULATE THE WEIGHT OF THE SELECTED STATONS FOR THE GRID
C         POINT AS (1/DISTATNCE OVER THE SUM OF 1/DISTANCE OF THE
C         SELECTED STATIONS)
            DO 110 IRG=1,4
               IF (ISTAQ(IRG).EQ.0) GO TO 110
               RDQ(IRG)=RDQ(IRG)/SUMD
               JRG=ISTAQ(IRG)
C           SW(JRG) SUMS THE STATION WEIGHTS OVER ALL GRID POINTS
               SW(JRG)=SW(JRG)+RDQ(IRG)
110            CONTINUE
120         CONTINUE
130      CONTINUE
C
C   CALCULATE THE SUM OF ALL THE STATION WEIGHTS FOR ALL
C   GRID POINTS
      SUM=0.0
      DO 140 IRG=1,NSTA
         SUM=SUM+SW(IRG)
140      CONTINUE
C
C  CALCULATE THE TOTAL WEIGHT OF EACH STATION AS ITS WEIGHT
C  FOR ALL GRID POINTS DIVIDED BY THE SUM OF ALL THE STATION
C  WEIGHTS
      DO 150 IRG=1,NSTA
         W(N,IRG)=SW(IRG)/SUM
150      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT (80I1)
C
      RETURN
C
      END
