C MEMBER RO36
C  (from old member FCEX36)
C
      SUBROUTINE RO36(PXV,EM,E,R,RS,RI,RG,CIN,W,WU,WL,WD,S,QI,QG,FR)
C......................................
C     THIS SUBROUTINE PERFORMS THE XINANJIANG RAINFALL CHANNEL INFLOW
C     OPERATION FOR ONE TIME INTERVAL.               IT COMPUTES :
C     RUNOFF, SOIL MOISTURE ,CHANNEL INFLOW COMPONENTS AND ACTUAL EVAPO-
C     TRANSPIRATION.
C     INITIAL WRITTEN BY
C                 QINGPING ZHU   YRCC CHINA   JULY  1988
C.......................................
C      VARIABLES FOR INPUT:
C      PXV              .... PRECIPITATION .
C      EM               .... EVAPOTRANSPIRATION DEMAND .
C      VARIABLES FOR BOTH INPUT AND OUTPUT (STATE VARIABLES):
C      W                .... TENSION WATER CONTENT.
C      WU               .... UPPER ZONE TENSION WATER CONTENT.
C      WL               .... LOWER ZONE TENSION WATER CONTENT.
C      WD               .... DEEP  ZONE TENSION WATER CONTENT.
C      S                .... FREE WATER CONTENT.
C      FR               .... PORTION OF AREA  RUNOFF OCCURED.
C      QI               .... INTERFLOW INFLOW.
C      QG               .... GROUNDWATER INFLOW.
C      VARIABLES FOR OUTPUT
C      R                .... TOTAL RUNOFF .
C      RS               .... SURFACE RUNOFF .
C      RI               .... INTERFLOW RUNOFF .
C      RG               .... GROUND RUNOFF .
C      CIN              .... TOTAL CHANNEL INFLOW.
C     NOTE : UNITS OF ALL VARIABLES EXCEPT FR ARE IN MM.
C.......................................
      INTEGER G
      REAL K,IMP,KSS,KG,KSSD,KGD,ID
C     COMMON BLOCKS
      COMMON/FPM36/K,IMP,WM,WUM,WLM,WDM,WMM,SM,SMM,B,EX,C,KSS,KG,
     1KSSD,KGD,CI,CG,CID,CGD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_xinan/RCS/ro36.f,v $
     . $',                                                             '
     .$Id: ro36.f,v 1.1 1995/09/17 18:58:39 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
      PE=PXV-EM
C     PE IS EFFECTIVE PRECIP.
      IF (PE.LE.0) GO TO 135
C     COMPUTE THE RUNOFF FROM IMPEVIOUS PART OF AREA.
      RB=IMP*PE
C.......................................
C     CALCULATE TOTAL RUNOFF (INCLUDING RB )
      A=WMM
      IF(W.LT.WM) A=WMM*(1.-(1.-W/WM)**(1./(1.+B)))
      R=PE+W-WM
      IF(A+PE.LT.WMM) R=PE-WM+W+WM*(1.-(A+PE)/WMM)**(1.+B)
C.......................................
C     DIVIDE TOTAL RUNOFF INTO SURFACE RS,INTERFLOW RI, GROUND RG .
      OLDFR=FR
C     GET FR FOR THIS INTERVAL.
      FR=(R-RB)/PE
      IF (FR.LT.0.0001) FR=0.0001
C     TRANSFER FREE WATER VOLUME FROM OLD FR TO NEW FR.
      S=S*OLDFR/FR
      OLDS=S
C.......................................
C     DETERMINE COMPUTATIONAL TIME INCREMENTS FOR THE BASIC TIME INTERVA
      G=INT(PE/5)+1
      Q=PE/G
C     G = NUMBER OF TIME INCREMENTS FOR THAT THE TIME INTERVAL
C     IS DIVIDED INTO FOR FURTHER SOIL MOISTURE ACCOUNTING .
C     Q = ONE INCREMENT OF EFFECTIVE PERCIPITATION (PE) .
C     NO ONE INCREMENT WILL EXCEED 5.0 MILLIMETERS  .
      ID=(1.-(1.-(KSSD+KGD))**(1./G))/(1.+KGD/KSSD)
      GD=ID*KG/KSS
      RS=0.
      RI=0.
      RG=0.
C.......................................
C     BEGINNING OF INCREMENTS LOOP.
      DO 130 J=1,G
      AU=SMM
      IF(S.LT.SM) AU=SMM*(1.-(1.-S/SM)**(1./(1.+EX)))
      RR=(Q+S-SM)*FR
      IF(AU+Q.LT.SMM) RR=(Q+S-SM+SM*(1.-(AU+Q)/SMM)**(1.+EX))*FR
      RS=RR+RS
      S=Q-RR/FR+S
      RG=S*GD*FR+RG
      RI=S*ID*FR+RI
      S=FLOAT(J)*Q+OLDS-(RS+RI+RG)/FR
  130 CONTINUE
C.......................................
C     RETAIN RB AS A PART OF SURFACEFLOW RS .
      RS=RS+RB
      GO TO 140
C.......................................
C     NO EFFECETIVE PRECIPITATION OCCURED .
  135 R=0.
      RS=0.
      RI=S*KSSD*FR
      RG=RI*KG/KSS
      S=S-(RG+RI)/FR
C.......................................
C     COMPUTE SOIL MOISTURES AND ACTUAL EVAPOTRANSPIRATION.
  140 E=EM
      D=WU
      WU=WU+PE-R
      IF (WU.GT.0) GO TO 150
      E=(E-D-PXV)*WL/WLM
      WL=WL-E
      E=E+D+PXV
      WU=0.
      IF (C*(EM-PXV-D).GT.E) GO TO 160
      GO TO 170
  150 IF (WU.LT.WUM) GO TO 170
      WL=WL+WU-WUM
      WU=WUM
      IF (WL.LT.WLM) GO TO 170
  520 WD=WD+WL-WLM
      WL=WLM
      GO TO 170
  160 WL=WL+E
      E=C*EM-PXV
      WL=WL-E
      IF (WL.GT.0.) GO TO 170
      WD=WD+WL
      WL=0.
  170 WVAL=WU+WL+WD
      IF(WVAL.GT.0.0001) GO TO 180
C     ELIMINATE IMPOSSIBLE SOIL MOISTURE VALUES
      E=W+PXV
      WD=0.0
  180 W=WU+WL+WD
C.......................................
C     COMPUTES CHANNEL INFLOW
      QI=QI*CID+RI*(1-CID)
      QG=QG*CGD+RG*(1-CGD)
      CIN=RS+QI+QG
C.......................................
      RETURN
      END
