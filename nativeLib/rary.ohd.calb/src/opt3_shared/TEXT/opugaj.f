C     MEMBER OPUGAJ
C
      SUBROUTINE OPUGAJ(UGH,UGV,QI,QO,QW,QW2,NORDS,VADJ,ISTAT)
C
C.......................................
C     THIS SUBROUTINE ADJUSTS THE UNIT HYDROGRAPH ACCORDING
C     TO HORIZONTAL(UGH) AND VERTICAL(UGV) ADJUSTMENT FACTORS.
C.......................................
C     SUBROUTINE WRITTEN BY
C            LARRY BRAZIL - HRL   DEC 1984   VERSION 1
C.......................................
C
C
C     QI=INPUT UNIT HYDROGRAPH
C     QO=OUTPUT UNIT HYDROGRAPH
C     QW=WORK SPACE (GE.NORDS)
C     QW2=WORK SPACE (GE.NORDS)
C     NORDS=NO. OF UNIT HYDROGRAPH ORDINATES
C     VADJ=VOLUME ADJUSTMENT MADE TO NEW UNIT HYDROGRAPH (%)
C     ISTAT=STATUS FLAG : 0 - NO ERRORS
C                         1 - SPECIFIED UGH VALUE TOO LARGE
C                             (UGH SET TO ALLOWABLE VALUE)
C                         2 - SPECIFIED UGV VALUE TOO LARGE
C                             (UGV SET TO ALLOWABLE VALUE)
C                         3 - BOTH UGH & UGV TOO LARGE
C                             (UGH & UGV RESET TO NEW VALUES)
C
      DIMENSION QI(1),QO(1),QW(1),QW2(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/opugaj.f,v $
     . $',                                                             '
     .$Id: opugaj.f,v 1.1 1995/09/17 19:52:58 dws Exp $
     . $' /
C    ===================================================================
C
C
      QMAX=0.0
      QVOL=0.0
      ISTAT=0
C
C  FIND PEAK Q AND TIME OF PEAK
      DO 10 I=1,NORDS
      Q=QI(I)
      QO(I)=Q
      QW(I)=Q
      QVOL=QVOL+Q
      IF(QMAX.GE.Q) GO TO 10
      QMAX=Q
      ITP=I
   10 CONTINUE
C
C
C  CHECK VALUES OF UGH & UGV
      TP=ITP
      RTP=(NORDS/TP)*.95
      IF(UGH.LE.RTP) GO TO 14
      UGH=RTP
      ISTAT=1
C
   14 QV95=(QVOL/QMAX)*.95
      IF(UGV.LE.QV95) GO TO 16
      UGV=QV95
      IF(ISTAT.EQ.0) ISTAT=2
      IF(ISTAT.EQ.1) ISTAT=3
C
C  VERTICAL ADJUSTMENT
C
   16 IF(UGV.GT.0.9999.AND.UGV.LT.1.0001) GO TO 50
      QAVG=QVOL/NORDS
C
C  SET INITIAL BREAK POINT EQUAL TO AVERAGE Q
      QBRK=QAVG
      VDIFF=UGV-1.0
      DELTA=.1*QBRK
      IPLUS=1
C
   20 QVOLV=0.0
      DO 30 I=1,NORDS
      Q=QI(I)
      QMDIFF=QMAX-QBRK
      IF(Q.LT.QBRK) QMDIFF=QBRK
      QBDIFF=Q-QBRK
      QRATIO=QBDIFF/QMDIFF
      VRATIO=QRATIO*VDIFF
      QW(I)=Q+Q*VRATIO
      IF(QW(I).LT.0.0) QW(I)=0.0
      QVOLV=QVOLV+QW(I)
   30 CONTINUE
C
      QVOLDF=QVOLV-QVOL
      RATIO=QVOLV/QVOL
      RATIOD=RATIO-1.0
C
C
C  CHECK TO SEE IF STOPPING CRITERIA ARE MET
      IF(ABS(RATIOD).LT..02) GO TO 50
      QD=(QBRK-QMAX)/QMAX
      IF(ABS(QD).LT..02) GO TO 50
      QB=QBRK/QMAX
      IF(QB.LT..02) GO TO 50
C
C  COMPUTE NEW QBRK
      IF(RATIO.GT.1.0) GO TO 40
      IF(IPLUS.EQ.1) GO TO 36
      QBRK=QBRK-DELTA
      GO TO 20
C
   36 IPLUS=0
      DELTA=DELTA*.5
      QBRK=QBRK-DELTA
      GO TO 20
C
   40 IF(IPLUS.EQ.0) GO TO 46
      QBRK=QBRK+DELTA
      GO TO 20
C
   46 IPLUS=1
      DELTA=DELTA*.5
      QBRK=QBRK+DELTA
      GO TO 20
C
C  HORIZONTAL ADJUSTMENT
C
   50 DO 52 I=1,NORDS
   52 QO(I)=QW(I)
      IF(UGH.GT.0.9999.AND.UGH.LT.1.0001) GO TO 82
C
      HDIFF=UGH-1.0
C
      HDTP=HDIFF*ITP
      TP=ITP
      DO 60 I=1,ITP
   60 QW2(I)=I+(I/TP)*HDTP
C
      TPNO=NORDS-ITP
      ITP1=ITP+1
      DO 62 I=ITP1,NORDS
   62 QW2(I)=I+((NORDS-I)/TPNO)*HDTP
C
      NORDP1=NORDS+1
      IORD=1
      DO 80 I=1,NORDP1
      Y1=0.0
      X1=0.0
      Y2=0.0
      X2=NORDP1+0.000001
      IF(I.EQ.1) GO TO 64
      Y1=QW(I-1)
      X1=QW2(I-1)
      IF(I.EQ.NORDP1) GO TO 66
   64 Y2=QW(I)
      X2=QW2(I)
   66 X21=X2-X1
      IF(X21.GT.0.000001.OR.X21.LT.-0.000001) GO TO 68
      SLOPE=1.E10
      GO TO 69
   68 SLOPE=(Y2-Y1)/X21
   69 B=Y2-SLOPE*X2
C
      DO 70 J=1,NORDS
      IF(IORD.GT.X2) GO TO 80
      IF(IORD.GT.NORDS) GO TO 82
      QO(IORD)=(SLOPE*IORD)+B
      IORD=IORD+1
   70 CONTINUE
C
   80 CONTINUE
C
C  CHECK VOLUME AND ADJUST Q'S
C
C
   82 VOLNEW=0.0
      DO 90 I=1,NORDS
   90 VOLNEW=VOLNEW+QO(I)
C
C  CHECK VOLUME RATIO
      GRATIO=QVOL/VOLNEW
      VADJ=ABS(GRATIO-1.)*100.
C
      DO 100 I=1,NORDS
  100 QO(I)=QO(I)*GRATIO
C
      RETURN
      END
