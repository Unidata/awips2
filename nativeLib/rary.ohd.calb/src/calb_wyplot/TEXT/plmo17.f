C MODULE PLMO17
C
      SUBROUTINE PLMO17(NPLOT,IDA,LDA,CONV,IS,CYCLE,BASE,PMAX,LIMIT,LMD,
     1   ORD,ORDI,LSYM,PSYM,LPLOTQ,LPX,PX,LRO,ROC,LSM,SM,D,MD)
C.......................................
C     THIS SUBROUTINE PLOTS DAILY DISCHARGE VALUES AND TABULATES
C        RAIN+MELT, RUNOFF COMPONENTS, AND SOIL-MOISTURE STORAGES
C        IN A SACRAMENTO TYPE PLOTTING FORMAT.  THE SUBROUTINE
C        IS EXECUTED FOR ALL OR PART OF A MONTH DURING ONE CALL.
C        THE SUBROUTINE IS USED BY THE 'WY-PLOT ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   DEC. 1979
C.......................................
C
      CHARACTER*4 CHAR
      LOGICAL DOT
C
      DIMENSION D(MD)
      DIMENSION PX(*),ROC(*),SM(*)
      DIMENSION ORD(101),ORDI(1),LSYM(*),PSYM(*),LPLOTQ(*)
      DIMENSION DRO(6),IRO(6),DSM(5),QD(3)
      CHARACTER*4  FVAL(4)
      CHARACTER*32 FMT1
      CHARACTER*44 FMT2
      CHARACTER*40 FMT3
      CHARACTER*52 FMT4
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/plmo17.f,v $
     . $',                                                             '
     .$Id: plmo17.f,v 1.3 2000/12/19 15:06:53 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA QD/1.0,10.0,100.0/
      DATA FMT1/'(1H ,I2,2X,A4,2F    2X,101A1)   '/
      DATA FMT2/'(1H ,I2,2X,A4,2F    2X, 81A1,2X,A4,1X,6I3)  '/
      DATA FMT3/'(1H ,I2,2X,A4,2F    2X, 76A1,5(1X,A4))  '/
      DATA FMT4/'(1H ,I2,2X,A4,2F    2X, 56A1,5(1X,A4),2X,A4,1X,6I3) '/
      DATA FVAL/'7.3,','7.2,','7.1,','7.0,'/
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1)WRITE(IODBUG,10)
10    FORMAT(1H0,17H** PLMO17 ENTERED)
C.......................................
C     INITIAL VALUES.
      DPX=0.0
      Q2=-0.0001
      L=4
      LDPX=0
      IF (METRIC.EQ.0) LDPX=1
      LDRO=1
      LDSM=0
      IF (METRIC.EQ.0) LDSM=1
      KD=IDA
C.......................................
C     BEGIN MAIN DAILY LOOP.
      DO 380 KDA=IDA,LDA
      KOFF=KDA-1
C
C     OBTAIN  DAILY RAIN+MELT
      IF (LPX.EQ.0) GO TO 20
      DPX=PX(KDA)
      IF (DPX.LT.0.0) DPX=0.0
      IF (METRIC.EQ.0) DPX=DPX*0.0394
C     GET CHARACTER REPRESENTATION OF DPX.
20    CONTINUE
      NDEC=1
      IF (DPX.GT.-1.AND.DPX.LT.0.) THEN
         NDEC=2
         GOTO 30
         ENDIF
      IF (DPX.GE.0.AND.DPX.LT.0.9995) THEN
         NDEC=3
         GOTO 30
         ENDIF
      IF (DPX.GE.0.9995.AND.DPX.LT.1.) THEN
         NDEC=2
         GOTO 30
         ENDIF
      IF (DPX.GE.1..AND.DPX.LT.9.995) THEN
         NDEC=2
         GOTO 30
         ENDIF
      IF (DPX.GE.1000.) THEN
         NDEC=0
         GOTO 30
         ENDIF
      IF (DPX.GE.9.995.AND.DPX.LT.10.) GOTO 30
      IF (DPX.GT.-10..AND.DPX.LE.-1.) GOTO 30
      IF (DPX.GE.10..AND.DPX.LT.100.) GOTO 30
      IWHOLE=ABS(DPX)
      WHOLE=IWHOLE
      REMAIN=MOD(ABS(DPX),WHOLE)
      NDEC=0
      IF (REMAIN.GE.0.5) THEN
         CALL UROUND('REAL4',DPX,1.0,'HIGH',DPX1,IERR)
         GOTO 30
         ENDIF
      CALL UROUND('REAL4',DPX,1.0,'LOW',DPX1,IERR)
      DPX=DPX1
30    CALL UFF2A(DPX,CPX,1,L,NDEC,0,6,ISTAT)
      CALL UMEMOV(CPX,CHAR,1)
      DOT=.FALSE.
      DO 40 II=1,L
         IF (CHAR(II:II).EQ.'.') DOT=.TRUE.
40    CONTINUE
      IF (.NOT.DOT.AND.DPX.LT.1000.) THEN
         IF (CHAR(1:1).EQ.' ') THEN
            DO 50 JJ=2,4
               CHAR(JJ-1:JJ-1)=CHAR(JJ:JJ)
50          CONTINUE
            CHAR(4:4)='.'
            CALL UMEMOV(CHAR,CPX,1)
            GOTO 60
            ENDIF
         CALL UMEMOV('****',CPX,1)
         ENDIF
60    CONTINUE
C
C     FIND PLOT LOCATIONS FOR EACH TIME SERIES.
      MAXL=0
      DO 140 N=1,NPLOT
      SYM=PSYM(N)
      LOC=LPLOTQ(N)
      Q=D(LOC+KOFF)
      IF (Q.GE.0.0) GO TO 70
C
C     FLOW VALUE IS MISSING
      Q=-0.0001
      LV=0
      GO TO 130
C
C     FLOW VALUE EXISTS.
70    Q=Q*CONV
      PQ=Q
      IF (IS.GT.1) GO TO 90
C     SEMI-LOG SCALE
      IF (PQ.GT.0.0) GO TO 80
      LV=1
      GO TO 120
80    PQ=ALOG(Q)
      GO TO 110
90    IF (IS.LT.3) GO TO 110
C     MODIFIED SCALE.
      IF (PQ.GT.CYCLE) GO TO 100
      LV=((PQ-BASE)/(CYCLE-BASE))*50.0+1.5
      GO TO 120
100   LV=((PQ-CYCLE)/(PMAX-CYCLE))*50.0+51.5
      GO TO 120
110   LV=((PQ-BASE)/(PMAX-BASE))*100.0+1.5
120   IF (LV.GT.LIMIT) LV=LIMIT
      IF (LV.LT.1) LV=1
      ORDI(N)=ORD(LV)
      ORD(LV)=SYM
130   LSYM(N)=LV
      IF (LV.GT.MAXL) MAXL=LV
      IF (N.EQ.1) Q1=Q
      IF (N.EQ.2) Q2=Q
140   CONTINUE
C
C     GET RUNOFF COMPONENTS FOR THE DAY.
      IF (LRO.EQ.0) GO TO 220
      J=KOFF*7
      F=ROC(J+1)
      IF (METRIC.EQ.0) F=F*0.0394
      NDEC=1
      IF (F.GT.-1.AND.F.LT.0.) THEN
         NDEC=2
         GOTO 150
         ENDIF
      IF (F.GE.0.AND.F.LT.0.9995) THEN
         NDEC=3
         GOTO 150
         ENDIF
      IF (F.GE.0.9995.AND.F.LT.1.) THEN
         NDEC=2
         GOTO 150
         ENDIF
      IF (F.GE.1..AND.F.LT.9.995) THEN
         NDEC=2
         GOTO 150
         ENDIF
      IF (F.GE.1000.) THEN
         NDEC=0
         GOTO 150
         ENDIF
      IF (F.GE.9.995.AND.F.LT.10.) GOTO 150
      IF (F.GT.-10..AND.F.LE.-1.) GOTO 150
      IF (F.GE.10..AND.F.LT.100.) GOTO 150
      IWHOLE=ABS(F)
      WHOLE=IWHOLE
      REMAIN=MOD(ABS(F),WHOLE)
      NDEC=0
      IF (REMAIN.GE.0.5) THEN
         CALL UROUND('REAL4',F,1.0,'HIGH',F1,IERR)
         GOTO 150
         ENDIF
      CALL UROUND('REAL4',F,1.0,'LOW',F1,IERR)
      F=F1
150   CALL UFF2A(F,R,1,L,NDEC,0,6,ISTAT)
      CALL UMEMOV(R,CHAR,1)
      DOT=.FALSE.
      DO 160 II=1,L
         IF (CHAR(II:II).EQ.'.') DOT=.TRUE.
160   CONTINUE
      IF (.NOT.DOT.AND.F.LT.1000.) THEN
         IF (CHAR(1:1).EQ.' ') THEN
            DO 170 JJ=2,4
               CHAR(JJ-1:JJ-1)=CHAR(JJ:JJ)
170         CONTINUE
            CHAR(4:4)='.'
            CALL UMEMOV(CHAR,R,1)
            GOTO 180
            ENDIF
         CALL UMEMOV('****',R,1)
         ENDIF
180   CONTINUE
      T=0.0
      DO 190 I=1,6
      DRO(I)=ROC(J+I+1)
      T=T+DRO(I)
190   CONTINUE
      DO 210 I=1,6
      J=I+2
      IF (J.EQ.7)J=2
      IF (J.EQ.8)J=1
      IF (T.GT.0.0) GO TO 200
      IRO(J)=0
      GO TO 210
200   IRO(J)=(DRO(I)/T)*100.0+0.5
210   CONTINUE
C
C     GET SOIL-MOISTURE STORAGES FOR THE DAY.
220   IF (LSM.EQ.0) GO TO 280
      J=KOFF*5
      DO 270 I=1,5
      F=SM(J+I)
      IF (METRIC.EQ.0) F=F*0.0394
      NDEC=1
      IF (F.GT.-1.AND.F.LT.0.) THEN
         NDEC=2
         GOTO 230
         ENDIF
      IF (F.GE.0.AND.F.LT.0.9995) THEN
         NDEC=3
         GOTO 230
         ENDIF
      IF (F.GE.0.9995.AND.F.LT.1.) THEN
         NDEC=2
         GOTO 230
         ENDIF
      IF (F.GE.1..AND.F.LT.9.995) THEN
         NDEC=2
         GOTO 230
         ENDIF
      IF (F.GE.1000.) THEN
         NDEC=0
         GOTO 230
         ENDIF
      IF (F.GE.9.995.AND.F.LT.10.) GOTO 230
      IF (F.GT.-10..AND.F.LE.-1.) GOTO 230
      IF (F.GE.10..AND.F.LT.100.) GOTO 230
      IWHOLE=ABS(F)
      WHOLE=IWHOLE
      REMAIN=MOD(ABS(F),WHOLE)
      NDEC=0
      IF (REMAIN.GE.0.5) THEN
         CALL UROUND('REAL4',F,1.0,'HIGH',F1,IERR)
         GOTO 230
         ENDIF
      CALL UROUND('REAL4',F,1.0,'LOW',F1,IERR)
      F=F1
230   CALL UFF2A(F,DSM(I),1,L,NDEC,0,6,ISTAT)
      CALL UMEMOV(DSM(I),CHAR,1)
      DOT=.FALSE.
      DO 240 II=1,L
         IF (CHAR(II:II).EQ.'.') DOT=.TRUE.
240   CONTINUE
      IF (.NOT.DOT.AND.F.LT.1000.) THEN
         IF (CHAR(1:1).EQ.' ') THEN
            DO 250 JJ=2,4
               CHAR(JJ-1:JJ-1)=CHAR(JJ:JJ)
250         CONTINUE
            CHAR(4:4)='.'
            CALL UMEMOV(CHAR,DSM(I),1)
            GOTO 260
            ENDIF
         CALL UMEMOV('****',DSM(I),1)
         ENDIF
260   CONTINUE
270   CONTINUE
C
C     PRINT A LINE OF THE PLOT.
280   LORD=LIMIT
      IF (LSM.EQ.0) GO TO 290
      IF (MAXL.LE.LMD) LORD=LMD
290   T=Q1
      IF ((Q1.LE.0.0).AND.(Q2.GT.0.0)) T=Q2
      DO 300 I=1,3
      IF (T.GE.QD(I)) GO TO 300
      J=I
      GO TO 310
300   CONTINUE
      J=4
310   IF (LORD.EQ.56) GO TO 350
      IF (LORD.EQ.76) GO TO 340
      IF (LORD.EQ.81) GO TO 330
      IF (LORD.EQ.101) GO TO 320
      GO TO 360
320   FMT1(17:20)=FVAL(J)
      WRITE (IPR,FMT1) KD,CPX,Q1,Q2,ORD
      GO TO 360
330   FMT2(17:20)=FVAL(J)
      WRITE (IPR,FMT2) KD,CPX,Q1,Q2,(ORD(I),I=1,81),R,IRO
      GO TO 360
340   FMT3(17:20)=FVAL(J)
      WRITE (IPR,FMT3) KD,CPX,Q1,Q2,(ORD(I),I=1,76),DSM
      GO TO 360
350   FMT4(17:20)=FVAL(J)
      WRITE (IPR,FMT4) KD,CPX,Q1,Q2,(ORD(I),I=1,56),DSM,R,IRO
C
C     SET VALUES IN ORD() BACK TO INITIAL SETTING
360   DO 370 N=1,NPLOT
      J=NPLOT-N+1
      LV=LSYM(J)
      IF (LV.EQ.0) GO TO 370
      ORD(LV)=ORDI(J)
370   CONTINUE
C     INCREMENT KD.
      KD=KD+1
380   CONTINUE
C     END OF MAIN DAILY LOOP.
C.......................................
      RETURN
      END
