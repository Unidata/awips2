C   MEMBER MPDIST
C-----------------------------------------------------------------------
C
      SUBROUTINE  DISREC  (RSTA,KSTART,K1,OPT9,NUMM5,N,OUTMO,DUNITS,
     *                  OPT8,X,Y,C,IND,STAID,NORMAL,hourly_flag)
C
C   CHECK RECORDER STATIONS FOR MISSING OR ACCUMULATED DATA AND
C   ESTIMATE OR DISTRIBUTE ACCORDING TO OTHER RECORDER STATIONS.
C
      COMMON/DIST/IHRT,ILE5(200),IGT5(200),ACCMAX(200)
      COMMON  /DIM/  M1,M2,M3,M4,M5,M6
      INCLUDE 'common/ionum'
C
      DIMENSION    X(M1),Y(M1)
      DIMENSION    C(M1,817),IND(M1),STAID(7,M1),NORMAL(M1,12)
      DIMENSION    STAQ(4),D2Q(4),RD2Q(4),P(4),ACC(4)
C
      integer*2    hourly_flag(200,817)
      INTEGER      RSTA,OPT9,STAQ,OUTMO,OPT8
      INTEGER      DEBUG,DEBUGA
      REAL         NORMAL
C
      COMMON  /MAP/  DEBUG,DEBUGA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/disrec.f,v $
     . $',                                                             '
     .$Id: disrec.f,v 1.4 1999/01/19 15:28:22 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (DEBUG.EQ.1.OR.DEBUGA.EQ.1) WRITE (IPR,500)
C
C
      DO 470 J=1,RSTA
      DO 470 K=KSTART,K1
      IF (OPT9.EQ.1) GO TO 10
      IF (J.EQ.NUMM5) GO TO 470
10    CONTINUE
c
      hourly_flag(j,k)=0
      IF (C(J,K).LT.999.975) GO TO 470
      IF (C(J,K).LT.999.985) then
         hourly_flag(j,k)=0
         GO TO 190
      end if

      hourly_flag(j,k)=1
C
C  ESTIMATE MISSING DATA
20    DO 30 I=1,4
      STAQ(I)=0
30    D2Q(I)=99999.0
      DO 130 JJ=1,RSTA
      IF (OPT9.EQ.1) GO TO 40
      IF (JJ.EQ.NUMM5) GO TO 130
40    CONTINUE
      IF (C(JJ,K).LT.0.0) GO TO 130
      IF (C(JJ,K).GT.999.97) GO TO 130
      IF (K.EQ.1) GO TO 50
      IF (C(JJ,(K-1)).EQ.999.98) GO TO 130
50    DELX=X(JJ)-X(J)
      DELY=Y(JJ)-Y(J)
      D2=DELX*DELX+DELY*DELY
C
C  IF TWO STATIONS HAVE THE SAME COORDINATES, SET D2 TO A NON-ZERO VALUE
      IF (D2.LT.0.1)  D2=0.1
      IF (DELX)60,80,70
60    IF (DELY)90,100,100
70    IF (DELY)110,110,120
80    IF (DELY)90,110,120
90    IF (D2.GT.D2Q(1)) GO TO 130
      D2Q(1)=D2
      STAQ(1)=JJ
      GO TO 130
100   IF (D2.GT.D2Q(4)) GO TO 130
      D2Q(4)=D2
      STAQ(4)=JJ
      GO TO 130
110   IF (D2.GT.D2Q(2)) GO TO 130
      D2Q(2)=D2
      STAQ(2)=JJ
      GO TO 130
120   IF (D2.GT.D2Q(3)) GO TO 130
      D2Q(3)=D2
      STAQ(3)=JJ
130   CONTINUE
      DO 150 L=1,4
      IF (STAQ(L).EQ.0) GO TO 140
      JJJ=STAQ(L)
      P(L)=C(JJJ,K)
      RD2Q(L)=1.0/D2Q(L)
      GO TO 150
140   P(L)=0.0
      RD2Q(L)=0.0
150   CONTINUE
      C(J,K)=0.0
      DO 170 L=1,4
      JJJ=STAQ(L)
      IF (JJJ.EQ.0) GO TO 160
      P(L)=P(L)*(NORMAL(J,OUTMO)/NORMAL(JJJ,OUTMO))*RD2Q(L)
      GO TO 170
160   P(L)=0.0
170   CONTINUE
      SUMD=RD2Q(1)+RD2Q(2)+RD2Q(3)+RD2Q(4)
      IF (SUMD.GT.0.00005) GO TO 180
      IHR=K-24
      C(J,K)=-0.00001
      IF(J.EQ.1) IHRT=IHRT+1
      IF(OPT8.EQ.0)WRITE(IPR,480) (STAID(I,J),I=1,7),IHR
      GO TO 470
180   C(J,K)=-(P(1)+P(2)+P(3)+P(4))/SUMD
      IF (C(J,K).GT.-0.00001) C(J,K)=-0.00001
      GO TO 470
c
C   DISTRIBUTE ACCUMULATED DATA (OF RECORDER STATION) ACCORDING TO
C     OTHER RECORDER STATIONS.
C   MISSING TIME DISTRIBUTION FOLLOWED BY 0.0 DOES NOT END ACCUMULATED
C     PERIOD
c
c   N = number of days in current month
c
  190 continue
      KMAX=(N+3)*24
      ACCUM=0.0
c
c   loop on hours extending to first two days of next month
c
      DO 220 L=K,KMAX
      LX=L
      IF (C(J,L).GT.999.97) GO TO 210
      IF (C(J,L).GT.0.0) GO TO 200
      C(J,L)=999.98
      GO TO 220
c
  200 continue
      ACCUM=C(J,L)
      hourly_flag(j,l)=1
      GO TO 230
c
210   IF (C(J,L).LT.999.985) GO TO 220
      GO TO 250
220   CONTINUE
c
c  value > or = 0.0 found  OR  end of 220 loop on hours without finding
c   value > or = 0.0
c
  230 continue
      IF ((K.EQ.KSTART).AND.(IND(J).EQ.1)) GO TO 240
      IF (ACCUM.GT.0.001) GO TO 270
240   IND(J)=0
      IF (LX.EQ.KMAX) IND(J)=1
c
c   missing data found while looking for accumulated value
c   fill C array with 999.99 up to and including this hour
c
  250 continue
      DO 260 L=K,LX
      C(J,L)=999.99
260   CONTINUE
c
      IHR1=K-24
      IHR2=LX-24
      IF (OPT8.EQ.1)  GO TO 20
      IF(OPT8.EQ.0)WRITE(IPR,490) (STAID(I,J),I=1,7),IHR1,IHR2
      GO TO 20
c
c   accumulated value > 0.0 found
c   attempt to distribute
c
  270 continue
      DO 280 M=1,4
      STAQ(M)=0
280   D2Q(M)=99999.0
      DO 380 JJ=1,RSTA
      IF (OPT9.EQ.1) GO TO 290
      IF (JJ.EQ.NUMM5) GO TO 380
290   CONTINUE
      DO 300 KX=K,LX
      IF (C(JJ,KX).LT.0.0) GO TO 380
      IF (C(JJ,KX).GT.999.97) GO TO 380
300   CONTINUE
      DELX=X(JJ)-X(J)
      DELY=Y(JJ)-Y(J)
      D2=DELX*DELX+DELY*DELY
      IF (D2.LT.0.1)  D2=0.1
      IF (DELX) 310,330,320
310   IF (DELY) 340,350,350
320   IF (DELY) 360,360,370
330   IF (DELY) 340,360,370
340   IF (D2.GT.D2Q(1)) GO TO 380
      D2Q(1)=D2
      STAQ(1)=JJ
      GO TO 380
350   IF (D2.GT.D2Q(4)) GO TO 380
      D2Q(4)=D2
      STAQ(4)=JJ
      GO TO 380
360   IF (D2.GT.D2Q(2)) GO TO 380
      D2Q(2)=D2
      STAQ(2)=JJ
      GO TO 380
370   IF (D2.GT.D2Q(3)) GO TO 380
      D2Q(3)=D2
      STAQ(3)=JJ
380   CONTINUE
      DO 400 L=1,4
      ACC(L)=0.0
      IF (STAQ(L).EQ.0) GO TO 400
      L1=STAQ(L)
      DO 390 KX=K,LX
      ACC(L)=ACC(L)+C(L1,KX)
390   CONTINUE
      IF (ACC(L).GT.0.0) GO TO 400
      STAQ(L)=0
400   CONTINUE
      MISS=1
      DO 410 L=1,4
      IF (STAQ(L).EQ.0) GO TO 410
      MISS=0
410   CONTINUE
      IF (MISS.EQ.1) GO TO 450
      DO 420 KX=K,LX
      C(J,KX)=0.0
      DO 420 L=1,4
      IF (STAQ(L).EQ.0) GO TO 420
      L1=STAQ(L)
      C(J,KX)=C(J,KX)+(C(L1,KX)*ACCUM)/(ACC(L)*D2Q(L))
420   CONTINUE
      XD2Q=0.0
      DO 430 L=1,4
      IF (STAQ(L).EQ.0) GO TO 430
      XD2Q=XD2Q+1.0/D2Q(L)
430   CONTINUE
      IF (XD2Q.LT.0.00005) GO TO 450
      DO 440 KX=K,LX
      C(J,KX)=-C(J,KX)/XD2Q
      IF (C(J,KX).GT.-0.00001)C(J,KX)=-0.00001
440   CONTINUE
      GO TO 470
c
c   case of unable to distribute precip - accum left in last hour
c
  450 continue
      DO 460 KX=K,LX
      C(J,KX)=-0.00001
460   CONTINUE
c
      C(J,LX)=-ACCUM
      IHR1=K-24
      IHR2=LX-24
      IF (OPT8.EQ.1)  GO TO 470
      IF(OPT8.EQ.0)WRITE(IPR,510) (STAID(I,J),I=1,7),IHR1,IHR2,ACCUM,
     *   DUNITS
470   CONTINUE
C
      RETURN
C
480   FORMAT (1H ,30HALL ESTIMATORS ARE MISSING FOR,1X, 6A4,A1,1X,
     *5HHOUR=,I3,5X,24HPRECIP SET EQUAL TO ZERO)
490   FORMAT (1H ,38HMISSING TIME DIST. SET TO MISSING DATA,5X, 6A4,A1,
     *5X,5HHOURS,I4,4H TO ,I4,'***490***')
500   FORMAT ( 20H0SUBROUTINE  DISREC.   )
510   FORMAT (1H ,37HDISTRIBUTION CAN NOT BE ESTIMATED FOR,1X, 6A4,A1,
     *  3X,5HHOURS,I4,4H TO ,I4,2X,25HACCUM. LEFT IN LAST HOUR=,F5.2,
     *  2X,A4,'***510***')
C
      END
