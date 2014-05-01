C-----------------------------------------------------------------------
C
      SUBROUTINE  DISNRC  (RSTA,KSTART,K1,NUM,OPT9,NUMM5,N,OUTMO,DUNITS,
     *                  OPT8,X,Y,OBTMO,OBTMOX,C,IND,STAID,NORMAL,
     *                  hourly_flag)
C
C   DISTRIBUTE daily STATIONS DATA ACCORDING TO TIME OF
C   OBSERVATION AND hourly STATION NETWORK.
C
c   PASS = 1 -- distributes observed daily values into hourly values
c            -- defines hourly_flag array
c
c   PASS = 2 -- estimates missing daily values
c
c   rsta = number of hourly stations
c   jj = counter on daily stations
c   n = number of days in current month
c   obtime = obs hour for station
c
      COMMON/DIST/IHRT,ILE5(200),IGT5(200),ACCMAX(200)
      COMMON  /DIM/  M1,M2,M3,M4,M5,M6
      INCLUDE 'common/ionum'
C
      DIMENSION    X(M1),Y(M1),OBTMO(M1),OBTMOX(M1)
      DIMENSION    C(M1,817),IND(M1),STAID(7,M1),NORMAL(M1,12)
      DIMENSION    STAQ(4),D2Q(4),RD2Q(4),P(4),ACC(4)
      DIMENSION    STAX(4),RD2X(4),D2X(4),NEGV(4),D2RS(4)
C
      integer*2    hourly_flag(200,817)
      INTEGER      RSTA,OPT9,PASS,STAX,STAQ,OPT8
      INTEGER      OBTMOX,OBTMO,OUTMO,OBTIME
      INTEGER      DEBUG,DEBUGA
      REAL         NORMAL,INCHES
C
      COMMON  /MAP/  DEBUG,DEBUGA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/disnrc.f,v $
     . $',                                                             '
     .$Id: disnrc.f,v 1.4 1999/01/19 15:28:06 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA INCHES/4HIN  /
C
      IF (DEBUG.EQ.1.OR.DEBUGA.EQ.1) WRITE (IPR,570)
C
      PASS=1
C
      DO 10 J=1,RSTA
      DO 10 K=KSTART,K1
      IF (ABS(C(J,K)).LT.0.00002) C(J,K)=0.0
10    CONTINUE
C
      IF (NUM.EQ.RSTA) GO TO 540
C
20    JJ=RSTA+1
      DO 530 J=JJ,NUM
      IF (OPT9.EQ.1) GO TO 30
      IF (J.EQ.NUMM5) GO TO 530
30    CONTINUE
      OBTIME=OBTMO(J)
      DO 40 I=1,4
      STAQ(I)=0
      D2RS(I)=99999.0
      D2Q(I)=99999.0
   40 continue
c
c   loop on hourly stations to calculate hourly station
c     closest to daily sta - 1 in each quadrant
c
c   staq(1) = station index of closest sta in quadrant
c   d2 = distance bet an hourly and daily station
c
      DO 170 JJJ=1,RSTA
      IF (OPT9.EQ.1) GO TO 50
      IF (JJJ.EQ.NUMM5) GO TO 170
c
50    CONTINUE
      DELX=X(JJJ)-X(J)
      DELY=Y(JJJ)-Y(J)
      D2=DELX*DELX+DELY*DELY
C
C  IF TWO STATIONS HAVE THE SAME COORDINATES, SET D2 TO A NON-ZERO VALUE
      IF (D2.LT.0.1)  D2=0.1
      IF (DELX)60,80,70
60    IF (DELY)90,110,110
70    IF (DELY)130,130,150
80    IF (DELY)90,110,150
90    IF (D2.GT.D2RS(1))GO TO 170
      IF (D2.GT.D2Q(1))GO TO 100
      D2RS(1)=D2Q(1)
      D2Q(1)=D2
      STAQ(1)=JJJ
      GO TO 170
100   D2RS(1)=D2
      GO TO 170
110   IF (D2.GT.D2RS(4))GO TO 170
      IF (D2.GT.D2Q(4))GO TO 120
      D2RS(4)=D2Q(4)
      D2Q(4)=D2
      STAQ(4)=JJJ
      GO TO 170
120   D2RS(4)=D2
      GO TO 170
130   IF (D2.GT.D2RS(2))GO TO 170
      IF (D2.GT.D2Q(2))GO TO 140
      D2RS(2)=D2Q(2)
      D2Q(2)=D2
      STAQ(2)=JJJ
      GO TO 170
140   D2RS(2)=D2
      GO TO 170
150   IF (D2.GT.D2RS(3))GO TO 170
      IF (D2.GT.D2Q(3))GO TO 160
      D2RS(3)=D2Q(3)
      D2Q(3)=D2
      STAQ(3)=JJJ
      GO TO 170
160   D2RS(3)=D2
170   CONTINUE
c
      K=(KSTART-1)+OBTIME
      KB=(K-1)/24
      KKK=K-23
      KXX=N+2
c
      INDON=0
      IF (IND(J).GT.0)INDON=1
c
c   begin loop on all days in month
c
      DO 520 IDAY=KB,KXX
      IF (PASS.EQ.1) then
         hourly_flag(j,k)=0
         IF (C(J,K).GT.0.009) GO TO 190
         IF (IND(J).EQ.0) GO TO 510
         GO TO 210
      else
         IF (C(J,K).LT.999.985) GO TO 510
         GO TO 240
      end if
c
c   PASS = 1 only
c
  190 continue
      IF (C(J,K).LT.999.975) GO TO 200
      IF (C(J,K).LT.999.985) GO TO 210
      hourly_flag(j,k)=1
      IND(J)=0
      GO TO 220
c
c   normal observed data value > 0.009 (PASS = 1 only)
c
  200 continue
      IND(J)=0
      hourly_flag(j,k)=0
      IF (INDON.GT.0) GO TO 220
      GO TO 240
C
C  TIME DISTRIBUTION (PASS = 1 only)
c  C(J,K) = 999.98  or  C(J,K) <= 0.009
C  NO MESSAGE PRINTED IF T.D. IS FOLLOWED BY M.D.
C  TIME DISTRIBUTION FOLLOWED BY 0.0 DOES NOT END ACCUMULATED period
c
  210 continue
      IND(J)=1
      C(J,K)=0.0
      hourly_flag(j,k)=0
      IF (IDAY.NE.KXX) GO TO 510
      INDON=1
c
c   missing data (PASS = 1 only)
c   fill in with 999.99 for 23 hours back from obs time 
c
  220 continue
      DO 230 I=KKK,K
      C(J,I)=999.99
  230 CONTINUE
c
      IHR1=KKK-24
      IHR2=K-24
C
      IF (OPT8.EQ.1)  GO TO 510
      IF (INDON.GT.0.AND.OPT8.EQ.0) WRITE(IPR,560) (STAID(I,J),I=1,7),  
     *   IHR1,IHR2
      GO TO 510
c
c   begin distribution of daily value calculations (PASS = 1 or 2)
c   for PASS = 1, come here for case of normal observed data value only
c   for PASS = 2, come here for case of C(j,k) = 999.99
c
  240 continue
      KY=K
c
c   look at 4 closest stations
c   jjj = station number of closest station
c
      DO 260 L=1,4
      RD2Q(L)=0.0
      ACC(L)=0.0
      NEGV(L)=0
      JJJ=STAQ(L)
      IF (JJJ.LT.1) GO TO 260
      RD2Q(L)=1.0/D2Q(L)
c
      DO 250 M=KKK,KY
      ACC(L)=ACC(L)+ABS(C(JJJ,M))
      IF (C(JJJ,M).LT.0.0)NEGV(L)=1
250   CONTINUE
260   CONTINUE
c
c   check for small accumulation at hourly station
c
c   srd2q = distance squared related weight
c
      SRD2Q=0.0
      DO 270 L=1,4
      IF (ACC(L).LT.0.009) GO TO 270
      SRD2Q=SRD2Q+RD2Q(L)
270   CONTINUE
c
      ACCUM=C(J,K)
      IF (PASS.EQ.1) GO TO 460
C
C  MISSING DATA ESTIMATION  PASS=2
c
      DO 280 NN=1,4
      STAX(NN)=STAQ(NN)
      D2X(NN)=D2Q(NN)
  280 continue
c
      DO 380 JX=JJ,NUM
      XACCUM=0.0
      IF (OPT9.EQ.1) GO TO 290
      IF (JX.EQ.NUMM5) GO TO 380
c
290   CONTINUE
      DO 300 M=KKK,KY
      XACCUM=XACCUM+C(JX,M)
300   CONTINUE
c
      IF (XACCUM.GT.999.97) GO TO 380
      DELX=X(JX)-X(J)
      DELY=Y(JX)-Y(J)
      D2=DELX*DELX+DELY*DELY
      IF (D2.LT.0.1)  D2=0.1
      IF (DELX)310,330,320
310   IF (DELY)340,350,350
320   IF (DELY)360,360,370
330   IF (DELY)340,350,370
340   IF ((D2.GT.D2X(1)).AND.(NEGV(1).EQ.0))GO TO 380
      IF (D2.GT.D2RS(1))GO TO 380
      D2X(1)=D2
      STAX(1)=JX
      NEGV(1)=0
      GO TO 380
c
  350 continue
      IF ((D2.GT.D2X(4)).AND.(NEGV(4).EQ.0))GO TO 380
      IF (D2.GT.D2RS(4))GO TO 380
      D2X(4)=D2
      STAX(4)=JX
      NEGV(4)=0
      GO TO 380
360   IF ((D2.GT.D2X(2)).AND.(NEGV(2).EQ.0))GO TO 380
      IF (D2.GT.D2RS(2))GO TO 380
      D2X(2)=D2
      STAX(2)=JX
      NEGV(2)=0
      GO TO 380
370   IF ((D2.GT.D2X(3)).AND.(NEGV(3).EQ.0))GO TO 380
      IF (D2.GT.D2RS(3))GO TO 380
      D2X(3)=D2
      NEGV(3)=0
      STAX(3)=JX
380   CONTINUE
c
      DO 420 KX=1,4
      JX=STAX(KX)
      IF (JX.EQ.0) GO TO 400
      IF (JX.LE.RSTA) GO TO 410
      P(KX)=0.0
c
      DO 390 M=KKK,KY
      P(KX)=P(KX)+C(JX,M)
390   CONTINUE
c
      RD2X(KX)=1.0/D2X(KX)
      GO TO 420
400   P(KX)=0.0
      RD2X(KX)=0.0
      GO TO 420
410   P(KX)=ACC(KX)
      RD2X(KX)=RD2Q(KX)
420   CONTINUE
c
      ACCUM=0.0
c
      DO 430 KX=KKK,KY
      C(J,KX)=0.0
430   CONTINUE
c
      DO 450 KX=1,4
      JX=STAX(KX)
      IF (JX.EQ.0) GO TO 440
      P(KX)=P(KX)*(NORMAL(J,OUTMO)/NORMAL(JX,OUTMO))*RD2X(KX)
      GO TO 450
440   P(KX)=0.0
450   CONTINUE
c
c   SUMD = sum of weights
c
      SUMD=RD2X(1)+RD2X(2)+RD2X(3)+RD2X(4)
      IF (SUMD.LT.0.00005) GO TO 460
c
c   successfully estimated a missing daily value
c
      ACCUM=(P(1)+P(2)+P(3)+P(4))/SUMD
      C(J,K)=ACCUM
c
c  PASS = 1 continuation
c
  460 continue
      IF (SRD2Q.GT.0.00001) GO TO 480
      IHR1=KKK-24
      IHR2=KY-24
c
      IF (PASS.EQ.2) then
         IF (ACCUM.LT.0.005)  GO TO 510
         IF(OPT8.EQ.0)WRITE(IPR,590) (STAID(I,J),I=1,7),IHR1,IHR2,
     *      ACCUM,DUNITS
         GO TO 510
      end if
c
c   PASS = 1 only
c
      IF (ACCUM.GE.0.005) THEN
         IF(ACCUM.GT.ACCMAX(J)) ACCMAX(J)=ACCUM
         HINCH=0.5
         IF(DUNITS.NE.INCHES) HINCH=2.54*HINCH
         IF(ACCUM.LE.HINCH) ILE5(J)=ILE5(J)+1
         IF(ACCUM.GT.HINCH) IGT5(J)=IGT5(J)+1
      END IF
      IF(OPT8.EQ.0)WRITE(IPR,580) (STAID(I,J),I=1,7),IHR1,IHR2,ACCUM,
     *   DUNITS
      GO TO 510
c
c   PASS = 1 or 2
c
c   distribute daily amount into C array for 23 hours previous to obs time
c     of daily station
c
  480 continue
      DO 500 M=KKK,KY
      C(J,M)=0.0
      if(pass.eq.1) hourly_flag(j,m)=0
      IF (ACCUM.LT.0.009) GO TO 500
c
      DO 490 L=1,4
      JJJ=STAQ(L)
      IF (JJJ.LT.1) GO TO 490
      IF (ACC(L).LT.0.009) GO TO 490
      C(J,M)=C(J,M)+ACCUM*RD2Q(L)*ABS(C(JJJ,M))/ACC(L)
c
490   CONTINUE
      C(J,M)=C(J,M)/SRD2Q
500   CONTINUE
c
c   PASS = 1 or 2
c
510   CONTINUE
      IF (IND(J).EQ.0) INDON=0
      KF=K
      K=K+24
      IF ((IDAY+1).GT.N) K=(IDAY+1)*24+OBTMOX(J)
      IF ((IND(J).GT.0).AND.(PASS.EQ.1)) GO TO 520
      KKK=KF+1
520   CONTINUE
530   CONTINUE
c
c   switch to PASS = 2 
c
      IF (PASS.EQ.1) then
         PASS=2
         GO TO 20
      end if
C
c   multiply hourly stations values < 0.0  by -1 for current month
c   hourly values < 0.0 are from cases where hourly data was unable
c    to be distributed 
c
  540 continue
      DO 550 J=1,RSTA
      DO 550 K=KSTART,K1
      IF (C(J,K).LT.0.0) C(J,K)=-C(J,K)
550   CONTINUE
      RETURN
C
560   FORMAT (1H ,38HMISSING TIME DIST. SET TO MISSING DATA,5X, 6A4,A1,
     *  5X,5HHOURS,I4,4H TO ,I4,'***560***')
570   FORMAT ( 20H0SUBROUTINE  DISNRC.   )
580   FORMAT (1H ,37HDISTRIBUTION CAN NOT BE ESTIMATED FOR,1X, 6A4,A1,
     *  3X,5HHOURS,I4,4H TO ,I4,2X,25HACCUM. LEFT IN LAST HOUR=,F5.2,
     *  2X,A4,'***580***')
590   FORMAT (1H ,37HDISTRIBUTION CAN NOT BE ESTIMATED FOR,1X, 6A4,A1,
     *  3X,5HHOURS,I4,4H TO ,I4,2X,25HEST. ACCUM LEFT, LAST HR=,F5.2,2X,
     *  A4,'***590***')
      END
