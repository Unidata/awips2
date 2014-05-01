      SUBROUTINE DSQUAR(N,NSTA,M,PX,X,Y,NEST,IGN,W,NNY,MO)
C     DETERMINES STATION WEIGHTS USING 1.0 OVER DISTANCE SQUARED.
c
c specify the maximum number of stations allowed
c
      PARAMETER (NSX=500,MaxMonth=1200)
      DIMENSION PX(nsx,MaxMonth),X(nsx),Y(nsx),IGN(5),W(5),ISTAQ(4),
     1          D2Q(4)
      DIMENSION NNY(nsx,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/dsquar.f,v $
     . $',                                                             '
     .$Id: dsquar.f,v 1.3 2002/05/15 18:55:01 dws Exp $
     . $' /
C    ===================================================================
C
      NEST=0
      DO 11 J=1,4
      ISTAQ(J)=0
   11 D2Q(J)=99999.0
      DO 10 I=1,NSTA
      IF(I.EQ.N)GO TO 10
      IF(PX(I,M).LT.0.0)GO TO 10
      IF  (NNY(I,MO).LT.1) GO TO 10
      DELX=X(I)-X(N)
      DELY=Y(I)-Y(N)
      D2=DELX*DELX+DELY*DELY
      IF(D2.LT.0.1)D2=0.1
      IF(DELX)21,27,22
   21 IF(DELY)23,24,24
   22 IF(DELY)25,25,26
   27 IF(DELY)23,25,26
   23 IF(D2.GT.D2Q(1))GO TO 10
      D2Q(1)=D2
      ISTAQ(1)=I
      GO TO 10
   24 IF(D2.GT.D2Q(4))GO TO 10
      D2Q(4)=D2
      ISTAQ(4)=I
      GO TO 10
   25 IF(D2.GT.D2Q(2))GO TO 10
      D2Q(2)=D2
      ISTAQ(2)=I
      GO TO 10
   26 IF(D2.GT.D2Q(3))GO TO 10
      D2Q(3)=D2
      ISTAQ(3)=I
   10 CONTINUE
      DO 15 J=1,4
      IF(ISTAQ(J).EQ.0)GO TO 15
      NEST=NEST+1
      IGN(NEST)=ISTAQ(J)
      W(NEST)=1.0/D2Q(J)
   15 CONTINUE
      RETURN
      END
