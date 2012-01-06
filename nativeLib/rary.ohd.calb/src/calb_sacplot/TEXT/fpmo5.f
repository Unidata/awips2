C MODULE FPMO5
C
      SUBROUTINE  FPMO5(NPLOT,KD,IDT,CONV,IS,CYCLE,BASE,PMAX,LIMIT,LMD,
     1   ORD,ORDI,LSYM,PSYM,LPLOTQ,LPX,PX,LRO,ROC,LSM,SM,D,MD)
C.......................................
C     THIS SUBROUTINE PLOTS DAILY DISCHARGE VALUES AND TABULATES
C        RAIN+MELT, RUNOFF COMPONENTS, AND SOIL-MOISTURE STORAGES
C        IN A SACRAMENTO TYPE PLOTTING FORMAT.  THE SUBROUTINE
C        IS USUALLY EXECUTED FOR ALL OR PART OF A MONTH DURING
C        ONE CALL.  THE SUBROUTINE IS USED BY THE 'SAC-PLOT'
C        OPERATION.
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
      DIMENSION ORD(101),ORDI(*),LSYM(*),PSYM(*),LPLOTQ(*)
      DIMENSION DRO(6),IRO(6),DSM(5),QD(3)
      CHARACTER*4  FVAL(4)
      CHARACTER*32 FMT1
      CHARACTER*44 FMT2
      CHARACTER*56 FMT3
      CHARACTER*68 FMT4
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_sacplot/RCS/fpmo5.f,v $
     . $',                                                             '
     .$Id: fpmo5.f,v 1.3 2000/12/19 15:07:26 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA QD/1.0,10.0,100.0/
      DATA FMT1/'(1H ,I2,2X,A4,2F    2X,101A1)   '/
      DATA FMT2/'(1H ,I2,2X,A4,2F    2X, 81A1,2X,A4,1X,6I3)  '/
      DATA FMT3/
     $ '(1H ,I2,2X,A4,2F    2X, 76A1,F5.0,F5.1,F5.0,F5.1,F5.0)  '/
      DATA FMT4/
     $ '(1H ,I2,2X,A4,2F    2X, 56A1,F5.0,F5.1,F5.0,F5.1,F5.0,2X,A4,1X,6
     $I3) '/
      DATA FVAL/'7.3,','7.2,','7.1,','7.0,'/
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1)WRITE(IODBUG,10)
10    FORMAT('1H0** FPMO5 ENTERED')
C.......................................
cav          WRITE(LP,'(''I AM HERE IN fpmo5'')')
C     INITIAL VALUES.
      DPX=0.0
      Q2=-0.0001
      L=4
      LDPX=0
      IF (METRIC.EQ.0) LDPX=1
      LDRO=1
C.......................................
C     BEGIN MAIN DAILY LOOP.
      DO 370 KDA=IDA,LDA
      KOFF=KDA-IDADAT
C
C     COMPUTE DAILY RAIN+MELT
      IF (LPX.EQ.0) GO TO 50
      IF (IDT.LT.24) GO TO 20
C     TIME INTERVAL OF RAIN+MELT=24
      I=KOFF+1
      DPX=PX(I)
      GO TO 40
C     TIME INTERVAL OF RAIN+MELT IS LESS THAN 24.
20    DPX=0.0
      I=IDT
      IF (KDA.EQ.IDA)I=IHR
      I1=KOFF*(24/IDT)+I/IDT
      I=24
      IF (KDA.EQ.LDA)I=LHR
      I2=KOFF*(24/IDT)+I/IDT
      DO 30 I=I1,I2
30    DPX=DPX+PX(I)
40    IF (DPX.LT.0.0) DPX=0.0
      IF (METRIC.EQ.0) DPX=DPX*0.0394
C
C     GET CHARACTER REPRESENTATION OF DPX.
C
50    CONTINUE
      NDEC=1
      IF (DPX.GT.-1.AND.DPX.LT.0.) THEN
         NDEC=2
         GOTO 60
         ENDIF
      IF (DPX.GE.0.AND.DPX.LT.0.9995) THEN
         NDEC=3
         GOTO 60
         ENDIF
      IF (DPX.GE.0.9995.AND.DPX.LT.1.) THEN
         NDEC=2
         GOTO 60
         ENDIF
      IF (DPX.GE.1..AND.DPX.LT.9.995) THEN
         NDEC=2
         GOTO 60
         ENDIF
      IF (DPX.GE.1000.) THEN
         NDEC=0
         GOTO 60
         ENDIF
      IF (DPX.GE.9.995.AND.DPX.LT.10.) GOTO 60
      IF (DPX.GT.-10..AND.DPX.LE.-1.) GOTO 60
      IF (DPX.GE.10..AND.DPX.LT.100.) GOTO 60
      IWHOLE=ABS(DPX)
      WHOLE=IWHOLE
      REMAIN=MOD(ABS(DPX),WHOLE)
      NDEC=0
      IF (REMAIN.GE.0.5) THEN
         CALL UROUND('REAL4',DPX,1.0,'HIGH',DPX1,IERR)
         GOTO 60
         ENDIF
      CALL UROUND('REAL4',DPX,1.0,'LOW',DPX1,IERR)
      DPX=DPX1
60    CALL UFF2A(DPX,CPX,1,L,NDEC,0,6,ISTAT)
      CALL UMEMOV(CPX,CHAR,1)
      DOT=.FALSE.
      DO 70 IX=1,L
         IF (CHAR(IX:IX).EQ.'.') DOT=.TRUE.
70    CONTINUE
      IF (.NOT.DOT.AND.DPX.LT.1000.) THEN
         IF (CHAR(1:1).EQ.' ') THEN
            DO 80 JX=2,4
               CHAR(JX-1:JX-1)=CHAR(JX:JX)
80          CONTINUE
            CHAR(4:4)='.'
            CALL UMEMOV(CHAR,CPX,1)
            GOTO 90
            ENDIF
         CALL UMEMOV('****',CPX,1)
         ENDIF
90    CONTINUE
C
C     FIND PLOT LOCATIONS FOR EACH TIME SERIES.
      MAXL=0
      DO 170 N=1,NPLOT
      SYM=PSYM(N)
      LOC=LPLOTQ(N)
      Q=D(LOC+KOFF)
      IF (Q.GE.0.0) GO TO 100
C
C     FLOW VALUE IS MISSING
      Q=-0.0001
      LV=0
      GO TO 160
C
C     FLOW VALUE EXISTS.
100   Q=Q*CONV
      PQ=Q
      IF (IS.GT.1) GO TO 120
C     SEMI-LOG SCALE
      IF (PQ.GT.0.0) GO TO 110
      LV=1
      GO TO 150
110   PQ=ALOG(Q)
      GO TO 140
120   IF (IS.LT.3) GO TO 140
C     MODIFIED SCALE.
      IF (PQ.GT.CYCLE) GO TO 130
      LV=((PQ-BASE)/(CYCLE-BASE))*50.0+1.5
      GO TO 150
130   LV=((PQ-CYCLE)/(PMAX-CYCLE))*50.0+51.5
      GO TO 150
140   LV=((PQ-BASE)/(PMAX-BASE))*100.0+1.5
150   IF (LV.GT.LIMIT) LV=LIMIT
      IF (LV.LT.1) LV=1
      ORDI(N)=ORD(LV)
      ORD(LV)=SYM
160   LSYM(N)=LV
      IF (LV.GT.MAXL) MAXL=LV
      IF (N.EQ.1) Q1=Q
      IF (N.EQ.2) Q2=Q
170   CONTINUE
C
C     GET RUNOFF COMPONENTS FOR THE DAY.
      IF (LRO.EQ.0) GO TO 250
      J=KOFF*7
      F=ROC(J+1)
      IF (METRIC.EQ.0) F=F*0.0394
C
      NDEC=1
      IF (F.GT.-1.AND.F.LT.0.) THEN
         NDEC=2
         GOTO 180
         ENDIF
      IF (F.GE.0.AND.F.LT.0.9995) THEN
         NDEC=3
         GOTO 180
         ENDIF
      IF (F.GE.0.9995.AND.F.LT.1.) THEN
         NDEC=2
         GOTO 180
         ENDIF
      IF (F.GE.1..AND.F.LT.9.995) THEN
         NDEC=2
         GOTO 180
         ENDIF
      IF (F.GE.1000.) THEN
         NDEC=0
         GOTO 180
         ENDIF
      IF (F.GE.9.995.AND.F.LT.10.) GOTO 180
      IF (F.GT.-10..AND.F.LE.-1.) GOTO 180
      IF (F.GE.10..AND.F.LT.100.) GOTO 180
      IWHOLE=ABS(F)
      WHOLE=IWHOLE
      REMAIN=MOD(ABS(F),WHOLE)
      NDEC=0
      IF (REMAIN.GE.0.5) THEN
         CALL UROUND('REAL4',F,1.0,'HIGH',F1,IERR)
         GOTO 180
         ENDIF
      CALL UROUND('REAL4',F,1.0,'LOW',F1,IERR)
      F=F1
180   CALL UFF2A(F,R,1,L,NDEC,0,6,ISTAT)
      CALL UMEMOV(R,CHAR,1)
      DOT=.FALSE.
      DO 190 IZ=1,L
         IF (CHAR(IZ:IZ).EQ.'.') DOT=.TRUE.
190   CONTINUE
      IF (.NOT.DOT.AND.F.LT.1000.) THEN
         IF (CHAR(1:1).EQ.' ') THEN
            DO 200 JZ=2,4
               CHAR(JZ-1:JZ-1)=CHAR(JZ:JZ)
200         CONTINUE
            CHAR(4:4)='.'
            CALL UMEMOV(CHAR,R,1)
            GOTO 210
            ENDIF
         CALL UMEMOV('****',R,1)
         ENDIF
210   CONTINUE
C
      T=0.0
      DO 220 I=1,6
      DRO(I)=ROC(J+I+1)
      T=T+DRO(I)
220   CONTINUE
      DO 240 I=1,6
      J=I+2
      IF (J.EQ.7)J=2
      IF (J.EQ.8)J=1
      IF (T.GT.0.0) GO TO 230
      IRO(J)=0
      GO TO 240
230   IRO(J)=(DRO(I)/T)*100.0+0.5
240   CONTINUE
C
C     GET SOIL-MOISTURE STORAGES FOR THE DAY.
250   IF (LSM.EQ.0) GO TO 270
      J=KOFF*5
      DO 260 I=1,5
260   DSM(I)=SM(J+I)
C
C     PRINT A LINE OF THE PLOT.
270   LORD=LIMIT
      IF (LSM.EQ.0) GO TO 280
      IF (MAXL.LE.LMD) LORD=LMD
280   T=Q1
      IF ((Q1.LE.0.0).AND.(Q2.GT.0.0)) T=Q2
      DO 290 I=1,3
      IF (T.GE.QD(I)) GO TO 290
      J=I
      GO TO 300
290   CONTINUE
      J=4
300   IF (LORD.EQ.56) GO TO 340
      IF (LORD.EQ.76) GO TO 330
      IF (LORD.EQ.81) GO TO 320
      IF (LORD.EQ.101) GO TO 310
      GO TO 350
310   FMT1(17:20)=FVAL(J)
      WRITE (IPR,FMT1) KD,CPX,Q1,Q2,ORD
      GO TO 350
320   FMT2(17:20)=FVAL(J)
      WRITE (IPR,FMT2) KD,CPX,Q1,Q2,(ORD(I),I=1,81),R,IRO
      GO TO 350
330   FMT3(17:20)=FVAL(J)
      WRITE (IPR,FMT3) KD,CPX,Q1,Q2,(ORD(I),I=1,76),DSM
      GO TO 350
340   FMT4(17:20)=FVAL(J)
      WRITE (IPR,FMT4) KD,CPX,Q1,Q2,(ORD(I),I=1,56),DSM,R,IRO
C
C     SET VALUES IN ORD() BACK TO INITIAL SETTING
350   DO 360 N=1,NPLOT
      J=NPLOT-N+1
      LV=LSYM(J)
      IF (LV.EQ.0) GO TO 360
      ORD(LV)=ORDI(J)
360   CONTINUE
C     INCREMENT KD.
      KD=KD+1
370   CONTINUE
C     END OF MAIN DAILY LOOP.
C.......................................
      RETURN
      END
