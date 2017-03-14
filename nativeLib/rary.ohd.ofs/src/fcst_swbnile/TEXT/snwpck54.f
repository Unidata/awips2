C MEMBER SNWPCK54
C
      SUBROUTINE SNWPCK54(P,T,PP,WE,SNO,DP,DN,DZ,SMAVG,DSOIL,DTH,LSN)
C *******************************************************************
C **        SUBROUTINE TO CALCULATE SNOW COMPACTION               ***
C **  EQUATION OF INCREASING OF SNOW DENSITY WAS OBTAINED         ***
C     AS AN APPROXIMATE SOLUTION OF E. ANDERSON DIFFERENTIAL      ***
C     EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19,              ***
C                 by   VICTOR KOREN   03/25/95                    ***
C *******************************************************************
C  P      =    RAIN+SNOWMELT, IN MM                               ***
C  T      =    AIR TEMPERATURE, IN C                              ***
C  PP     =    RAIN+SNOWFALL, IN MM                               ***
C  WE     =    WATER EQUIVALENT OF SNOW, IN MM                    ***
C  SNO    =    OBSERVED SNOWFALL, IN MM                           ***
C  SNW    =    ESTIMATED SNOWFALL, IN MM                          ***
C  DP     =    SNOW DEPTH, IN MM                                  ***
C  DN     =    SNOW DENSITY, IN G/CM3                             ***
C  DZ     =    FROZEN(THAWED) DEPTH, IN CM                        ***
C  SMAVG  =    VOLUMETRIC MOISTURE CONTENT OF FROZEN LAYER        ***
C  DSOIL  =    SOIL DENSITY, IN G/CM3                             ***
C  DTH    =    TIME STEP, IN HR                                   ***
C  PLW    =    PERCENT OF LIQUID WATER IN SNOW, IN FRACTION       ***
C                                                                 ***
C      SUBROUTINE WILL RETURN NEW VALUES OF DP AND DN             ***
C *******************************************************************
C
      SAVE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/snwpck54.f,v $
     . $',                                                             '
     .$Id: snwpck54.f,v 1.1 1997/09/22 15:34:00 page Exp $
     . $' /
C    ===================================================================
C
      DATA PLW0/0./
C
C ** DEFINE CONSTANTS **
C    SCONST=CONSTANT TO CONVERT LIQUID PORTION OF SNOWCOVER
C    INTO LIQUID WATER AMOUNT IN MM. SCONST IS CALCULATED AS:
C
C       SCONST=2*ADSN*TAVG/DP=G*L*PLW/DTH,
C
C    WHERE ADSN IS TEMPERATURE CODUCTIVITY FOR SNOW
C          L IS THE SPECIFIC HEAT CONSTANT FOR CONVERTING
C               LIQUID WATER TO ICE (=80 CAL/G)
C          G IS DENSITY OF WATER (=1 G/CM3)
C
      SCONST=2.5
C ** PLWM=MAXIMUM POSSIBLE LIQUID WATER IN SNOW
      PLWM=WE*0.13
C
C ** COMPUTE SNOWFALL **
      SNW=0.0
      IF (T.LT.0.0) SNW=PP
C ** IF LSN IS NOT ZERO, USE OBSERVED SNOWFALL
      IF (LSN.NE.0) SNW=SNO
C
C ** CONVERSION OF SNOW DEPTH FROM MM TO CM *************************
   10 DP=DP/10.
C
C ** CALCULATING OF NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE **
      IF(T.LT.-15.) THEN
        DN0=0.05
      ELSE
        DN0=0.05+0.0017*(T+15.)**1.5
      END IF
C
C ** ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL
      DPNEW=0.1*SNW/DN0
      IF (DP+DPNEW.GT.0.0) THEN
        DN=(DP*DN+DPNEW*DN0)/(DP+DPNEW)
        DP=DP+DPNEW
      ELSE
        DN=0.0
        DP=0.0
      END IF
C
C ** CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK               ***
C ** AS TAVG=0.5*(T+TSOIL) AND TSOIL,TEMPERATURE ON SOIL SURFACE,  ***
C ** CALCULATED BY EQUATION                                        ***
C **    TSOIL=T*(1.+ADSL*DP/(ADSN*DZ))                             ***
C ** ADSN IS A TEMPERATURE CONDUCTIVITY OF SNOW, in Cal/(CM*HR*C)  ***
C ** ADSL IS A TEMPERATURE CONDUCTIVITY OF SOIL, in Cal/(CM*HR*C)  ***
C ** DZ IS A SOIL FREEZING/THAWING DEPTH, in CM                    ***
C
      IF((DP.GT.0.).AND.(DZ.GT.0.)) THEN
        ADSN=CSNOW54(DN)
        ADSL=CSOIL54(DSOIL,SMAVG,T)
        TSOIL=T/(1.+ADSL*DP/(ADSN*DZ))
        TAVG=0.5*(T+TSOIL)
      ELSE
        TAVG=T
      ENDIF
C
C ** CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
C              DN=DN0*(EXP(B*W)-1.)/(B*W)
C              B=DTH*c1*EXP(0.08*TAVG-c2*DN0)
C ** c1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR))
C ** c2 IS A CONSTANT (CM3/G) Kojima estimated as 21 cms/g
C
      C1=0.01
      C2=21.0
      IF(WE.GT.0. .AND. DN.GT.0.) THEN
        B=DTH*C1*EXP(0.08*TAVG-C2*DN)
        BWX=B*0.1*WE
        IF (BWX.LT.1.E-5) THEN
          DN=1.0
        ELSE
          DN=DN*((EXP(B*0.1*WE)-1.)/(B*0.1*WE))
        END IF
        DP=WE/(10.*DN)
      ELSE
        DP=0.
        DN=0.
        GOTO 7
      ENDIF
C
C ** CORRECTION OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID
C ** WATER CONTENT IN SNOW
C
      IF (T.GT.0.0) THEN
        PLWT=PLW0*WE+P
        IF (PLWT.GT.PLWM) PLWT=PLWM
      ELSE
        PLWT=PLW0*WE+SCONST*(TAVG*DTH*ADSN/DP)
        IF (PLWT.LT.0.0) PLWT=0.0
      END IF
      PLW=PLWT/WE
C
      IF (PLW-PLW0 .GT. 0.0) THEN
        DP=(WE/(10.*DN))*(1.-(PLW-PLW0)*(1.-DN))
        DS=WE/(10.*DP)
      END IF
C
C ** REPLACEMENT OF PREVIOUS LIQUID WATER BY NEW ONE
    7 PLW0=PLW
      DP=DP*10.
C      WRITE(1,*) TAVG,TSOIL,ADSN,ADSL
      RETURN
      END
