C MEMBER NILE54
C
C  ****  SIMULATION OF THE WATER BALANCE COMPONENTS   ***********
C            INPUT INFORMATION: PAR, DT, P, EP
C            OUTPUT INFORMATION: Q, QS, QG, ES, D, DRT
C     NOTICE:  FIRST TIME IT IS NEED TO INPUT  D  AND  DRT
C
C
      SUBROUTINE NILE54 (DT,P,EP,T,
     &                    Q,QS,QG,ES,ERT,E2,FCR,DRT,D,
     &                    DMAX,KG,ALPSM,ALPRT,KDT,
     &                    KIMP,DSOIL,POROS,WWP,CVICE,
     &                    DZU,DZB,THAWU,THAWB,
     &                    WICEU,WICEB,SDP,SDN,IFRZE,
     &                    IDAY,IHOUR,IOUT,IBUG)
C
C  ****  THIS SUBROUTINE WAS WRITTEN BY VICTOR KOREN
C        MODIFIED BY QINGYUN DUAN   ****
C
C  ****  SIMULATION OF THE WATER BALANCE COMPONENTS   ***********
C        WITH TAKING INTO ACCOUNT SOIL FREEZING/THAWING
C            INPUT INFORMATION: PAR, DT, P, EP, SDP, T
C            OUTPUT INFORMATION: Q, QS, QG, ES, D, DRT, WICEU, WICEB
C     NOTICE:  VARIABLES D, DRT, WICEU AND WICEB SHOULD BE INITIALIZED
C              FOR THE START DATE
C  **  PAR IS THE ARRAY OF THE MODEL PARAMETERS:
C
C  PARAMETERS FOR WATER BALANCE MODEL:
C
C  **  DMAX  IS THE MAX SOIL MOISTURE DEFICIT OF BOTTOM LAYER, IN MM
C  **  KG    IS THE POTENTIAL SUBSURFACE FLOW, IN MM/DAY
C  **  ALPSM IS THE BOTTM LAYER PART WHICH PRODUCE SUBSURFACE FLOW
C  **  ALPRT IS THE UPPER LAYER DEFICIT PROPORTION
C  **  KDT   IS THE TIME SCALE FACTOR
C
C  PARAMETERS FOR FREEZING COMPONENT:
C
C  **  KIMP  IS PARAMETER OF IMPERMEABLE FROZEN SOIL
C  **  DSOIL IS SOIL DENSITY, IN G/CM**3
C  **  POROS IS SOIL POROSITY
C  **  WWP   IS WILTING POINT VOLUMETRIC MOISTURE
C  **  CVICE IS THE CV OF ICE CONTENT DISTRIBUTION
C
C  **  DT IS THE TIME STEP, IN DAYS
C  **  P IS PRECIPITATION, IN MM/DAY
C  **  EP IS POTENTIAL EVAPORATION, IN MM/DAY
C  **  T IS SURFACE (SOIL/SNOW) TEMPERATURE, C
C      Q,QS,QG,ES,D,DRT ARE SIMULATED WATER BALANCE COMPONENTS
C      Q IS TOTAL RUNOFF, IN MM/(DT)
C      QS IS SURFACE RUNOFF, IN MM/(DT)
C      QG IS SUBSURFACE RUNOFF, IN MM/(DT)
C      ES IS EVAPOTRANSPIRATION, MM/(DT)
C      ERT IS EVAPOTRANSPIRATION IN UPPER ZONE, MM/(DT)
C      E2 IS EVAPOTRANSPIRATION IN LOWER ZONE, MM/(DT)
C      D IS SOIL MOISTURE DEFICIT OF THE BOTTOM LAYER, IN MM
C      DRT IS SOIL MOISTURE DEFICIT OF THE UPPER LAYER, IN MM
C      DZU IS FROZEN DEPTH IN THE UPPER ZONE, IN MM
C      DZB IS FROZEN DEPTH IN THE LOWER ZONE, IN MM
C      THAWU IS THAWING DEPTH IN THE UPPER ZONE, IN MM
C      THAWB IS THAWING DEPTH IN THE LOWER ZONE, IN MM
C  **  SDP IS SNOW DEPTH, IN CM
C  **  SDN IS SNOW DENSITY IN G/CM**3
C      WICEU IS AN ICE CONTENT OF THE UPPER LAYER, MM
C      WICEB IS A ICE CONTENT OF THE BOTTOM LAYER, MM
C
      REAL KG,KDT,KIMP
      COMMON/SUMS54/SP,SR,SET,SRS,SRG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/nile54.f,v $
     . $',                                                             '
     .$Id: nile54.f,v 1.1 1997/09/22 15:33:55 page Exp $
     . $' /
C    ===================================================================
C
C
      DMRT=ALPRT*DMAX
      SMAX=ALPSM*DMAX
      A=DMRT-DRT-WICEU
C    Calculate soil depth
      SODPU=DMRT/(POROS-WWP)
      SODPB=DMAX/(POROS-WWP)
C    Calculate average soil moisture
      SMAVG = ((DMRT-DRT) + (DMAX-D))/(DMRT+DMAX)
C    New treatment
      URATIO=(DRT+WICEU)/DMRT
      ERT=EP*DT*(1-URATIO)
      IF(ERT.GT.A) ERT=A
C
C New version 6/2/94
      EE=EP*DT
      BRATIO=(D+WICEB)/DMAX
      E2=EP*DT*URATIO*(1-BRATIO)
      ES=ERT+E2
      IF(DT.GE.15.) DRT=DRT+ERT
cc++  modification on 4/20/95, change px computation
cc++       PX=P*DT-DRT
      PP=P*DT
      PFRC=1.-((1.-EXP(-DRT/DMRT))/(1.-EXP(-1.)))**2
      PX1=PP*PFRC
      PX2=PP*(1.-PFRC)-DRT
      IF(PX2.GE.0.) GOTO 1
      PX2=0.
      DRT=DRT-PP*(1.-PFRC)
      GOTO 2
    1 DRT=0.
    2 PX=PX1+PX2
      IF(DT.LT.15.) DRT=DRT+ERT
cc## 2nd modification on 4/19/95
cc## bfrc - Fraction of available free water for baseflow
      BFRC = 0.1
      WA = DMAX - D
      DB1 = 1.0 - (DMAX-WA*BFRC+WICEB)/SMAX
      IF (DB1 .GT. 0.0) THEN
        QG1 = KG * DB1 * DT
      ELSE
        QG1 = 0.0
      END IF
      DB2 = 1.0 - (DMAX-WA*BFRC)/DMAX
      QG2 = KG * DB2 * DT
      QG = QG1 + QG2
cc## end of change on 4/19/95
      IF(QG.LT.0.) QG=0.
      IF(DT.LT.15.) GOTO 3
      D=D+E2+QG
      IF(D.GT.DMAX) D=DMAX
    3 VAL=(1.-EXP(-KDT*SQRT(DT)))
      DDT=D*VAL
      VALM=0.01*DMAX
      IF(D.LT.VALM) DDT=VALM*VAL
      FCR=1.
      IF(IFRZE.EQ.0.OR.(WICEU+WICEB.EQ.0..AND.T.GE.0.)) GOTO 777
C
C  *******  SOIL FREEZING/THAWING COMPONENT
C
      IF((WICEU.LT.(DMRT-DRT).AND.T.LT.0.)
     +    .OR.(WICEU.GT.0..AND.T.GE.0.)) THEN
        IF (T .LT. 0.0) THAWU=0.0
        THAWU0=THAWU
        CALL FREEZ54(DT,T,SDP,SDN,DRT,DMRT,DSOIL,POROS,WWP,
     *             DZU,0.,THAWU,FREZ)
C    Compute frozen depth
        IF (T .GT. 0.0) THEN
          DZB = DZB - (THAWU-THAWU0)
        ELSE
          DZU = DZU + FREZ
        END IF
      ELSE
        IF (T .LT. 0.0) THAWB=0.0
        THAWB0 = THAWB
        CALL FREEZ54(DT,T,SDP,SDN,D,DMAX,DSOIL,POROS,WWP,
     *             DZB,DZU,THAWB,FREZ)
        IF (T .GT. 0.0) THEN
          DZB = DZB - (THAWB - THAWB0)
        ELSE
          DZB = DZB + FREZ
        END IF
      ENDIF
C    Check if the frozen depth is < max depth and > 0.0
      IF (DZU .GT. SODPU) THEN
        DZB = DZB + (DZU - SODPU)
        DZU = SODPU
      END IF
      IF (DZB .GT. SODPB*(1.-BFRC)) DZB=SODPB*(1.-BFRC)
      IF (DZB .LT. 0.) THEN
        DZU = DZU + DZB
        DZB = 0.
        THAWB = 0.
      END IF
      IF (DZU .LT. 0.) THEN
        DZU = 0.
        THAWU = 0.
        THAWB = 0.
      END IF
      DZ = DZU + DZB
C    Compute ice content
      WICEU = (DMRT-DRT)*DZU/SODPU
      WICEB = (DMAX-D)*DZB/SODPB
C
      IF(WICEU+WICEB.GT.1.E-2) THEN
        IALP=(1./CVICE**2) + 0.5
        IF (IALP .LT. 1) IALP = 1
        AIALP = FLOAT(IALP)
          ACRT=IALP*KIMP/(WICEB+WICEU)
          SUM=1.
          DO J=1,IALP-1
            SUM=SUM+ACRT**(IALP-J)/GF54(AIALP-J+1)
          END DO
          FCR=1.-EXP(-ACRT)*SUM
      END IF
C
C  *******    END OF FREEZING/THAWING COMPONENT
C
  777 QS=PX*(1.-FCR*(DDT/(PX+DDT)))
      D=D+QS-PX
C
      IF(DT.LT.15.) D=D+E2+QG
      IF(D.LT.0.) THEN
        QS = QS - D
        D=0.
      END IF
      IF(D.GT.DMAX) D=DMAX
      Q=QS+QG
C
      SP=SP+PP
      SET=SET+ES
      SR=SR+Q
      SRS=SRS+QS
      SRG=SRG+QG
C
      IF (IBUG.EQ.1) THEN
        WRITE (IOUT,900) IDAY,IHOUR,PP,EE,ES,ERT,E2,Q,QS,
     +                   QG,QG1,QG2,BFRC,FCR,DRT,D
  900   FORMAT(1H ,2I3,16F7.3)
        IF (IFRZE.EQ.1) WRITE (905) WICEU,WICEB,DZU,DZB,THAWU,THAWB,
     +                   T,SDP,SDN
  905   FORMAT(1H ,6X,16F7.3)
      END IF
      RETURN
      END
