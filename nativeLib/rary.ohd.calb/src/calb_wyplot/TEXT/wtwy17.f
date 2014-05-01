C MEMBER WTWY17
C  (from old member MCEX17)
C
C
C @PROCESS LVL(77) OPT(2)
C
      SUBROUTINE  WTWY17(MOWY,NREC,NPLOT,CO,LPLOTQ,D,MD,LPX,IDT,PX,
     1DPX,LRO,ROC,LSM,SM)
C.......................................
C     THIS SUBROUTINE WRITES DATA TO THE SCRATCH FILE AND COMPUTES
C        MONTHLY DISCHARGE TOTALS FOR THE 'WY-PLOT ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C     ERIC ANDERSON - HRL     APRIL 1980
C.......................................
C
C     VARIABLES IN THE ARGUMENT LIST - WTWY17
C
C        MOWY    -  CURRENT MONTH WITHIN THE WATER YEAR (INPUT)
C        NREC    -  NUMBER OF RECORD TO WRITE TO ON THE SCRATCH FILE
C                     (BOTH)
C        NPLOT   -  NUMBER OF DISCHARGE TIME SERIES TO BE PLOTTED
C                     (INPUT)
C        CO      -  CARRYOVER ARRAY (BOTH)
C        LPLOTQ  -  LOCATIONS OF DISCHARGE TIME SERIES IN THE D ARRAY
C                     (INPUT)
C        D       -  THE ENTIRE D ARRAY (INPUT)
C        MD      -  DIMENSION OF THE D ARRAY (INPUT)
C        LPX     -  LOCATION OF RAIN+MELT DATA IN THE D ARRAY (INPUT)
C        IDT     -  TIME INTERVAL OF RAIN+MELT DATA (INPUT)
C        PX      -  RAIN+MELT DATA (INPUT)
C        DPX     -  DAILY RAIN+MELT VALUES (OUTPUT)
C        LRO     -  LOCATION OF RUNOFF COMPONENTS IN THE D ARRAY (INPUT)
C        ROC     -  RUNOFF COMPONENTS DATA (INPUT)
C        LSM     -  LOCATION OF SOIL-MOISTURE STORAGES IN D ARRAY
C                     (INPUT)
C        SM      -  SOIL-MOISTURE STORAGE DATA (INPUT)
C
C.......................................
      DIMENSION D(MD),PX(*),ROC(*),SM(*),DPX(*),CO(*),LPLOTQ(*)
C
C     COMMON BLOCKS.
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fwydat'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/wtwy17.f,v $
     . $',                                                             '
     .$Id: wtwy17.f,v 1.2 1996/07/11 19:50:38 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,10)
10    FORMAT(1H0,17H** WTWY17 ENTERED)
C.......................................
C     INITIAL VALUE
      L=1
C.......................................
C     FLOW DATA - COMPUTE TOTAL AS DATA PUT INTO WY().
      DO 40 N=1,NPLOT
      SUM=0.0
      LOC=LPLOTQ(N)
      LCO=(N-1)*12
      DO 30 KDA=IDA,LDA
      KOFF=KDA-IDADAT
      Q=D(LOC+KOFF)
      WY(L+KOFF)=Q
      IF(SUM.LT.0.0) GO TO 30
      IF(Q.LT.-990.0) GO TO 20
      SUM=SUM+Q
      GO TO 30
20    SUM=-0.001
30    CONTINUE
      CO(LCO+MOWY)=SUM
      L=L+31
      IF(L.LT.372) GO TO 40
      WRITE (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=1
40    CONTINUE
C.......................................
C     RAIN+MELT DATA - COMPUTE DAY TOTALS AS PUT INTO WY().
      IF(LPX.EQ.0) GO TO 90
      DO 80 KDA=IDA,LDA
      KOFF=KDA-IDADAT
      I=KOFF+1
      IF(IDT.LT.24) GO TO 50
C
C     TIME INTERVAL OF RAIN+MELT DATA=24
      DPX(I)=PX(I)
      GO TO 70
C
C     TIME INTERVAL OF RAIN+MELT IS LESS THAN 24.
50    DPX(I)=0.0
      J=IDT
      IF(KDA.EQ.IDA) J=IHR
      J1=KOFF*(24/IDT)+J/IDT
      J=24
      IF(KDA.EQ.LDA)J=LHR
      J2=KOFF*(24/IDT)+J/IDT
      DO 60 J=J1,J2
60    DPX(I)=DPX(I)+PX(J)
C
C     STORE DAILY RAIN+MELT IN WY().
70    WY(L+KOFF)=DPX(I)
80    CONTINUE
      L=L+31
      IDT=24
C
C     CHECK IF RECORD SHOULD BE WRITTEN TO THE SCRATCH FILE.
      IF(L.LT.372) GO TO 90
      WRITE (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=1
90    CONTINUE
C.......................................
C     RUNOFF COMPONENTS AND SOIL-MOISTURE STORAGES.
      IF(LRO.EQ.0) GO TO 130
      NV=7
100   DO 120 M=1,NV
      J=(M-1)*31
      DO 110 I=1,31
      K=I-1
      IF(NV.EQ.7) WY(L+K)=ROC(J+I)
      IF(NV.EQ.5) WY(L+K)=SM(J+I)
110   CONTINUE
      L=L+31
      IF(L.LT.372) GO TO 120
      WRITE (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=1
120   CONTINUE
      IF (NV.EQ.5) GO TO 140
130   IF(LSM.EQ.0) GO TO 140
      NV=5
      GO TO 100
C.......................................
C     WRITE DATA IN WY() TO SCRATCH FILE IF ANY IS THERE.
140   IF(L.EQ.1) RETURN
      WRITE (IRWY,REC=NREC) WY
C.......................................
      RETURN
      END
