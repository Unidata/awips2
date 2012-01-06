C MODULE SFQUAD
C-----------------------------------------------------------------------
C
      SUBROUTINE SFQUAD (IRTYPE,LX,LY,STAID,NSTQ,IPARM,ITYPE,IMNT,FE,
     *   IELEV,IPT,ISTAT)
C
C  ROUTINE TO COMPUTE THE WEIGHTS FOR THE NSTQ CLOSEST STATIONS PER
C  QUADRANT.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
      DIMENSION STAID(2),IPT(4,1)
      DIMENSION DIST(4,5),WT(4,5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfquad.f,v $
     . $',                                                             '
     .$Id: sfquad.f,v 1.3 2000/03/13 16:08:08 page Exp $
     . $' /
C    ===================================================================
C
      DATA DEGRAD/.01745329/,STLAT/60./
C
C
      ISTAT=0
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFQUAD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('WGHT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'LX=',LX,' LY=',LY,' NSTQ=',NSTQ,' IPARM=',ITYPE
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,'(1X,A,2A4)')
     *      'STAID=',STAID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  INITIALIZE VARIABLES
      DO 20 I=1,4
         DO 10 J=1,NSTQ
            DIST(I,J)=1.E10
            IPT(I,J)=0
10          CONTINUE
20       CONTINUE
C
C  COMPUTATIONAL LOOP
      NFOUND=0
      DO 200 I=1,INWFIL
         GO TO (30,40,50,50,60,70),IPARM
            GO TO 200
30       IF (PPVRNW(I).EQ.0) GO TO 200
            IF (IRTYPE.EQ.2.AND.SFLGNW(I)/10.NE.3) GO TO 200
            GO TO 80
40       IF (PP24NW(I).EQ.0) GO TO 200
            IF (IRTYPE.EQ.1.AND.PP24NW(I).LT.0) GO TO 200
            IF (IRTYPE.EQ.2.AND.SFLGNW(I)/10.LT.2) GO TO 200
            IF (IRTYPE.EQ.2.AND.PP24NW(I).GT.99999) GO TO 200
            GO TO 80
50       IF (TA24NW(I).EQ.0) GO TO 200
            IF (IRTYPE.EQ.1.AND.TA24NW(I).LT.0) GO TO 200
            IF (IRTYPE.EQ.2.AND.SFLGNW(I)-(SFLGNW(I)/10*10).NE.2)
     *         GO TO 200
         GO TO 80
60       IF (TAINNW(I).EQ.0) GO TO 200
            GO TO 80
70       IF (TF24NW(I).EQ.0) GO TO 200
C     COMPUTE STATION LOCATION DIFFERENCES
80       KX=CORDNW(1,I)
         KY=CORDNW(2,I)
         IF (KX.NE.LX.OR.KY.NE.LY) GO TO 90
            IF (STIDNW(1,I).NE.STAID(1).OR.STIDNW(2,I).NE.STAID(2))
     *         GO TO 90
            GO TO 200
90       DELX=KX-LX
         DELY=KY-LY
         D=SQRT(DELX*DELX+DELY*DELY)
C     COMPUTE QUADRANT
         IF (DELX) 100,110,120
100      IF (DELY) 150,150,140
110      IF (DELY) 160,130,140
120      IF (DELY) 160,130,130
130      N=1
         GO TO 170
140      N=2
         GO TO 170
150      N=3
         GO TO 170
160      N=4
C     SORT STATIONS INTO ASCENDING ORDER ACCORDING TO DISTANCE
170      IF (D.GE.DIST(N,NSTQ)) GO TO 190
            DIST(N,NSTQ)=D
            IPT(N,NSTQ)=I
            IF (NSTQ.EQ.1) GO TO 190
               NNSTQ=NSTQ-1
               DO 180 J=1,NNSTQ
                  K=NSTQ-J
                  L=NSTQ+1-J
                  IF (D.GE.DIST(N,K)) GO TO 190
                     TEMP=DIST(N,K)
                     DIST(N,K)=DIST(N,L)
                     DIST(N,L)=TEMP
                     ITEMP=IPT(N,K)
                     IPT(N,K)=IPT(N,L)
                     IPT(N,L)=ITEMP
180                  CONTINUE
190         NFOUND=NFOUND+1
200      CONTINUE
C
C  CHECK NUMBER OF STATIONS FOUND
      IF (NFOUND.EQ.0) THEN
         WRITE (LP,480) IPARM,ITYPE
         CALL SUWRNS (LP,2,-1)
         ISTAT=1
         GO TO 460
         ENDIF
C
C  COMPUTE WEIGHTS FOR CLOSEST NSTQ STATIONS PER QUADRANT
      IF (IPARM.NE.1.AND.IPARM.NE.2) GO TO 250
         DO 240 I=1,4
            DO 230 J=1,NSTQ
               IF (IPT(I,J).EQ.0) GO TO 220
                  IF (DIST(I,J).GT.0.01) GO TO 210
                     WT(I,J)=10000.
                     GO TO 230
210               WT(I,J)=1./DIST(I,J)**2
                  GO TO 230
220            WT(I,J)=0.0
230            CONTINUE
240         CONTINUE
      GO TO 310
C
250   IF (IPARM.NE.3) GO TO 370
260   DO 300 I=1,4
         DO 290 J=1,NSTQ
            IF (IPT(I,J).EQ.0) GO TO 280
               IF (DIST(I,J).GT.0.01) GO TO 270
                  WT(I,J)=100.
                  GO TO 290
270            WT(I,J)=1./DIST(I,J)
               GO TO 290
280         WT(I,J)=0.0
290         CONTINUE
300      CONTINUE
C
C  NORMALIZE WEIGHTS FOR EACH GRID POINT
310   IF (ITYPE.NE.1) GO TO 340
      SUM=0.0
      J=1
      DO 320 I=1,4
         SUM=SUM+WT(I,J)
320      CONTINUE
      DO 330 I=1,4
         WT(I,J)=WT(I,J)/SUM
330      CONTINUE
C
340   DO 360 I=1,4
         DO 350 J=1,NSTQ
            LPT=IPT(I,J)
            WORKNW(LPT)=WORKNW(LPT)+WT(I,J)
350         CONTINUE
360      CONTINUE
      GO TO 410
C
370   IF (IPARM.NE.4.AND.IPARM.NE.5.AND.IPARM.NE.6) GO TO 410
C
C  CHECK TO SEE IF STATION IS MOUNTAINOUS
      IF (IMNT.EQ.0) GO TO 260
C
C  PERFORM MOUNTAINOUS COMPUTATIONS
C
C  COMPUTE GRID LENGTH-KM CONVERSION FACTOR
      S=SIN(STLAT*DEGRAD)
      FLX=FLATMX*DEGRAD
      S1=(1+S)/(1+SIN(FLX))
      X1=4.7625/S1
      FLN=FLATMN*DEGRAD
      S2=(1+S)/(1+SIN(FLN))
      X2=4.7625/S2
      CONV=(X1+X2)/2
C
C  COMPUTE TEMP STATION WEIGHTS
      DO 400 I=1,4
         DO 390 J=1,NSTQ
            LPT=IPT(I,J)
            IF (LPT.EQ.0) GO TO 390
               DELEV=IABS(IELEV-ELEVNW(LPT))/1000.
               D10=DIST(I,J)/10
               DWT=(D10*CONV)+(FE*DELEV)
               IF (DWT.GT.0.05) GO TO 380
                  WORKNW(LPT)=WORKNW(LPT)+20.
                  GO TO 390
380            WORKNW(LPT)=WORKNW(LPT)+1./DWT
390         CONTINUE
400      CONTINUE
C
410   DO 430 I=1,4
         DO 430 J=1,NSTQ
            IF (IPT(I,J).GT.99999) THEN
               IPT(I,J)=IPT(I,J)-99999
               GO TO 430
               ENDIF
            IPT(I,J)=IABS(IPT(I,J))
430         CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         DO 440 J=1,NSTQ
            WRITE (IOSDBG,*) 'J=',J,' IPT(1...4,J)=',(IPT(I,J),I=1,4)
            CALL SULINE (IOSDBG,1)
440         CONTINUE
         NPER=10
         NTIME=(INWFIL/NPER)+1
         DO 450 J=1,NTIME
            K=((J-1)*NPER)+1
            L=J*NPER
            IF (L.GT.INWFIL) L=INWFIL
            WRITE (IOSDBG,'(1X,A,I3,A,I3,A,10(E10.5,1X))')
     *         'WORKNW(',K,'...',L,')=',(WORKNW(I),I=K,L)
            CALL SULINE (IOSDBG,1)
450         CONTINUE
         ENDIF
C
460   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFQUAD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
480   FORMAT ('0*** WARNING - IN SFQUAD - NO STATIONS FOUND IN ',
     *   'NETWORK COMMON BLOCK FOR SPECIFIED DATA TYPE ',
     *   '(IPARM=',I1,',ITYPE=',I1,').')
C
      END
