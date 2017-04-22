C MEMBER RDWY17
C  (from old member MCEX17)
C
C @PROCESS LVL(77) OPT(2)
C
      SUBROUTINE RDWY17(NREC,NPLOT,LPLOTQ,D,MD,LPX,DPX,LRO,ROC,LSM,SM,
     1   IDAY,LDAY)
C.......................................
C     THIS SUBROUTINE READS ONE MONTH OF DATA FROM THE SCRATCH FILE
C        FOR USE BY THE 'WY-PLOT' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL     APRIL 1980
C.......................................
C
C     VARIABLES IN THE ARGUMENT LIST - RDWY17
C
C        NREC    -  RECORD NUMBER TO READ FROM ON SCRATCH FILE (INPUT)
C        NPLOT   -  NUMBER OF DISCHARGE TIME SERIES TO BE PLOTTED
C                     (INPUT)
C        LPLOTQ  -  LOCATIONS OF DISCHARGE TIME SERIES IN THE D ARRAY
C                     (INPUT)
C        D       -  ENTIRE D ARRAY (INPUT)
C        MD      -  DIMENSION OF THE D ARRAY (INPUT)
C        LPX     -  LOCATION TO STORE DAILY RAIN+MELT DATA IN D ARRAY
C                     (INPUT)
C        DPX     -  DAILY RAIN+MELT DATA (OUTPUT)
C        LRO     -  LOCATION TO STORE RUNOFF COMPONENTS IN D ARRAY
C                     (INPUT)
C        ROC     -  RUNOFF COMPONENTS DATA (OUTPUT)
C        LSM     -  LOCATION TO STORE SOIL-MOISTURE STORAGES IN D ARRAY
C                     (INPUT)
C        SM      -  SOIL-MOISTURE STORAGE DATA (OUTPUT)
C        IDAY    -  INITIAL DAY FOR THIS MONTH (INPUT)
C        LDAY    -  LAST DAY FOR THIS MONTH (INPUT)
C
C.......................................
      DIMENSION D(MD),DPX(*),ROC(*),SM(*),LPLOTQ(*)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fwydat'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/rdwy17.f,v $
     . $',                                                             '
     .$Id: rdwy17.f,v 1.2 1996/07/11 19:42:35 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,10)
10    FORMAT(1H0,17H** RDWY17 ENTERED)
C.......................................
C     READ FIRST RECORD FOR THE MONTH.
      READ (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=0
C.......................................
C     RETRIEVE DISCHARGE DATA
      DO 40 N=1,NPLOT
      IF(L.LT.372) GO TO 20
      READ (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=0
20    LOC=LPLOTQ(N)-1
      DO 30 KD=IDAY,LDAY
      D(LOC+KD)=WY(L+KD)
30    CONTINUE
      L=L+31
40    CONTINUE
C.......................................
C     RETRIEVE RAIN+MELT IF AVAILABLE
      IF(LPX.EQ.0) GO TO 70
      IF(L.LT.372) GO TO 50
      READ (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=0
50    DO 60 KD=IDAY,LDAY
      DPX(KD)=WY(L+KD)
60    CONTINUE
      L=L+31
C.......................................
C     RETRIEVE RUNOFF COMPONENTS AND SOIL-MOISTURE STORAGES IF AVAILABLE
70    IF(LRO.EQ.0) GO TO 120
      NV=7
80    DO 110 M=1,NV
      J=(M-1)*31
      IF(L.LT.372) GO TO 90
      READ (IRWY,REC=NREC) WY
      NREC=NREC+1
      L=0
90    DO 100 I=1,31
      IF(NV.EQ.7) ROC(J+I)=WY(L+I)
      IF(NV.EQ.5) SM(J+I)=WY(L+I)
100   CONTINUE
      L=L+31
110   CONTINUE
      IF (NV.EQ.5) GO TO 130
120   IF(LSM.EQ.0) GO TO 130
      NV=5
      GO TO 80
C.......................................
130   CONTINUE
      RETURN
      END
