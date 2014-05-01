C MEMBER APIC24
C  (from old member FCAPIC24)
C
      SUBROUTINE APIC24(P,STORM,PFRS,AI,GWRO,R,PE,AESC,TA,TAVG,WE,AIADJ,
C                             LAST UPDATE: 02/22/94.14:10:21 BY $WC30EA
C
     -KHR,IMO,IDA,IHR,IVOPT,IFRZE,LWE,LSC,IPRINT,IOUT,ITP,IBUG)
C
C*************************************************************
C   THIS SUBROUTINE EXECUTES THE CONTINUOUS,
C       INCREMENTAL API MODEL FOR ONE TIME STEP
C*************************************************************
C
C************************************************************
C    INITIALLY WRITTEN BY :  ERIC ANDERSON, HRL, MARCH 1990 *
C************************************************************
C
      DIMENSION JDM(12)
C
      REAL  DP1,DP2
      PARAMETER ( DP1=0.9, DP2=1.0 )
C
C*******************************
C    COMMON BLOCKS
C*******************************
C
      INCLUDE 'common/fengmt'
      COMMON/RSPM24/APIK,AIXW,AIXD,CW,CD,SMIX,CS,FRSX,APIX,PEX,PEN,
     -EFC,PIMPV,RIVA,RVAI,WKW,WKD,AEIK,AEIX,AEIN,ATIR,ATIX,ATIN,APIKS
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
      COMMON/FIPM24/CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA,ITTA
      COMMON/RGCO24/BFSC,BFI
      COMMON/SUMS24/SP,SR,SIMP,SRS,SRG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/apic24.f,v $
     . $',                                                             '
     .$Id: apic24.f,v 1.1 1995/09/17 18:56:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C********************************
C    DATA STATEMENTS
C********************************
C
      DATA JDM/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA PI/3.1416/
C
C*****************************************************
C    CHECK AI ADJUSTMENT--CANNOT CAUSE AI TO BECOME NEGATIVE
C*****************************************************
C
      IF ((AIXW+AIADJ).GT.0.0) GO TO 50
      AIADJ=-AIXW+0.01
C
C***********************************************
C    COMPUTE JULIAN DAY -- IGNORE LEAP YEAR
C***********************************************
C
   50 JDA=JDM(IMO)+IDA
      FJDA=JDA
C
C*********************************************************
C    IF FIRST PERIOD OF THE DAY,COMPUTE INDICATORS THAT
C    CHANGE ON A DAILY BASIS
C*********************************************************
C
      IF (KHR.NE.ITP) GO TO 100
C     COMPUTE DAILY PE OR SEASONAL PE ADJUSTMENT
      FMJD=FJDA-105.0
      PED=(PEX+PEN)*0.5+0.5*(PEX-PEN)*SIN(2.0*PI*FMJD/365.0)
      IF (IVOPT.NE.0) GO TO 60
C
C**********************************************************
C   COMPUTE WEEK NUMBER AND POSITION BETWEEN WET AND DRY
C**********************************************************
C
      WK=FJDA/7.0
      CALL WEEK24(WK,WKW,WKD,F,IRISE)
      Y=(1.0+COS(PI*(1.0-F)))*0.5
      IF (IRISE.EQ.0) Y=Y**CS
      GO TO 100
C
   60 IF (IVOPT.NE.1) GO TO 70
C****************************************
C    COMPUTE AEI AND POSITION
C****************************************
C
      AEI=AEIK*AEI+PE
      IF (AEI.GT.AEIX)  AEI=AEIX
      IF (AEI.LT.AEIN)  AEI=AEIN
      Y=(AEI-AEIN)/(AEIX-AEIN)
      GO TO 100
C
C*************************************************
C   COMPUTE ATI AND POSITION
C*************************************************
C
   70 ATI=ATI+ATIR*(TAVG-ATI)
      IF (ATI.GT.ATIX) ATI=ATIX
      IF (ATI.LT.ATIN) ATI=ATIN
      Y=(ATI-ATIN)/(ATIX-ATIN)
C
C*********************************************
C     MAIN COMPUTATIONAL LOOP
C*********************************************
C
  100 NINC=1
      IF (P.LE.0.2) GO TO 101
      NINC=(P/0.2)+1.0
  101 FINC=NINC
      PINC=P/FINC
      FDT=ITP
      DINC=(FDT/FINC)/24.0
      RS=0.0
      RG=0.0
      ETD=0.0
      ESM=0.0
      GWIN=0.0
      DO 200 I=1,NINC
C
C*********************************************************
C   COMPUTE AI - ANTECEDENT INDEX
C*********************************************************
C
      AIXWT=AIXW+AIADJ
      AIXDT=AIXD+AIADJ
      AIW=AIXWT*(CW**API)
      AID=AIXDT*(CD**API)
      IF (AID.GT.AIW) GO TO 201
      AI=AIW
      GO TO 205
C
  201 AI=AIW+Y*(AID-AIW)
C
  205 AIX=AIXWT+Y*(AIXDT-AIXWT)
C
C****************************************************
C   MODIFY AI FOR SURFACE MOISTURE CONDITIONS
C****************************************************
C        NOTE,  DP1 = 0.9,  DP2 = 1.0
C
      SMR=SMI/SMIX
      IF (SMR.LT.0.01) SMR=0.01
      AIF=AI*((ALOG(SMR)/ALOG(DP1))+DP2)
      IF (AIF.GT.50.0) AIF=50.0
C
C***************************************************************
C   COMPUTE FRACTIONAL RUNOFF FROM FINAL AI
C****************************************************************
C
      FRS=FRSX*(0.7**AIF)
C
C************************************
C   FROZEN GROUND MODIFICATIONS
C************************************
C
      IF (IFRZE.EQ.0) GO TO 210
      FRS=FRS+(1.0-FRS)*(FEI**2)*EFA
C
C**************************************
C     COMPUTE SURFACE RUNOFF
C**************************************
C
  210 RSINC=FRS*PINC
      RS=RS+RSINC
C
C***************************************
C     COMPUTE GROUNDWATER RUNOFF
C***************************************
C
      CALL BASF24(PINC,SMR,AIF,RSINC,RGINC,GWINC,DINC)
      RG=RG+RGINC
      GWIN=GWIN+GWINC
C
C***************************************
C     UPDATE API AND SMI
C***************************************
C
      AIR=AI/AIX
      CALL INDX24(PINC,LSC,AESC,LWE,WE,IFRZE,FEI,FI,AIR,DINC,PED,PE,ED,
     -   ES,API,SMI,IVOPT)
      ETD=ETD+ED
      ESM=ESM+ES
C
  200 CONTINUE
C
C**********************************************
C   UPDATE FROST INDEX IF USED
C**********************************************
C
      IF (IFRZE.EQ.0) GO TO 130
      IF (((KHR/ITTA)*ITTA).NE.KHR) GO TO 130
      DT=ITTA/24.0
      AIR=AI/AIX
      CALL FRZE24(TA,LWE,WE,LSC,AESC,FI,FEI,AIR,DT,IBUG,IOUT)
C
C******************************************************
C     ACCOUNT FOR IMPERVIOUS AREA, COMPUTE TOTAL RUNOFF AND
C       INCREMENT SUMS
C******************************************************
C
  130 RS=RS*(1.0-PIMPV)
      RG=RG*(1.0-PIMPV)
      R=RS+RG+PIMPV*P
      GWIN=GWIN*(1.0-PIMPV)
      SP=SP+P
      SR=SR+R
      SIMP=SIMP+PIMPV*P
      SRS=SRS+RS
      SRG=SRG+RG
      PFRS=0.0
      IF (P.GT.0.0) PFRS=(RS/(1.0-PIMPV))/P
      STORM=RS+PIMPV*P
C
C***************************************
C   ACCOUNT FOR RIPARIAN EVAPORATION LOSS
C***************************************
C
      RLOSS=0.0
      IF (RIVA.EQ.0.0) GO TO 140
      IF (AI.LE.RVAI) GO TO 140
      RES=ETD-ESM
      IF (RES.LE.0.0) GO TO 140
      RLOSS=RES*RIVA*((AI-RVAI)/(AIX-RVAI))
      IF (RLOSS.GT.RG) RLOSS=RG
      R=R-RLOSS
      IF (R.LT.0.0) R=0.0
  140 GWRO=RG-RLOSS
      IF (GWRO.LT.0.0) GWRO=0.0
C
C***************************************
C   PRINT VALUES IF REQUESTED
C***************************************
C
      IF (IBUG.EQ.1) GO TO 150
      IF (IPRINT.EQ.1) GO TO 150
      GO TO 180
C
  150 CONV=1.0
      IF (METRIC.EQ.1) CONV=25.4
      PAPI=API*CONV
      PAI=AI*CONV
      PIDX=WK
      IF (IVOPT.EQ.1) PIDX=AEI*CONV
      IF (IVOPT.EQ.2) PIDX=(ATI-32.0)/1.8
      PSMI=SMI*CONV
      PAIF=AIF*CONV
      PP=P*CONV
      PRS=RS*CONV
      IF (P.LT.(0.001*FINC)) GO TO 151
      IF (P*(1.0-PIMPV).LE.RS) GO TO 151
      GF=GWIN/((P*(1.0-PIMPV))-RS)
      GO TO 153
  151 GF=0.0
  153 PGWIN=GWIN*CONV
      PBFSC=BFSC*CONV
      PBFI=BFI*CONV
      PRG=RG*CONV
      PR=R*CONV
      PFI=-99.9
      PFEI=-99.99
      IF (IFRZE.EQ.0) GO TO 155
      PFI=FI
      IF (METRIC.EQ.1) PFI=(FI-32.0)/1.8
      PFEI=FEI
C
  155 WRITE(IOUT,900) IDA,IHR,PP,PRS,PAPI,PIDX,Y,PAI,PSMI,PAIF,FRS,
     1  GF,PGWIN,PBFSC,PBFI,PRG,PR,PFI,PFEI
  900 FORMAT(1H ,2I3,3F7.2,F7.1,4F7.2,F7.2,4F7.2,F7.3,F7.2,F7.1,F7.2)
C
  180 CONTINUE
      RETURN
      END
