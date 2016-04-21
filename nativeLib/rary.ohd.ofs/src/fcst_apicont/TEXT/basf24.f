C MEMBER BASF24
C  (from old member FCBASF24)
C
      SUBROUTINE BASF24(P,SMR,AI,RS,RG,GWIN,DT)
C
C***************************************************************
C    THIS SUBROUTINE EXECUTES THE GROUNDWATER RUNOFF PORTION
C    OF THE CONTINOUS INCREMENTAL API MODEL.
C***************************************************************
C
C  WRITTEN BY -- ERIC ANDERSON, HRL , MARCH 1990
C
C******************************
C   COMMON BLOCKS
C******************************
C
      COMMON/RGPM24/BFPK,BFIK,BFIM,AICR,CG
      COMMON/RGCO24/BFSC,BFI
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/basf24.f,v $
     . $',                                                             '
     .$Id: basf24.f,v 1.1 1995/09/17 18:56:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C*********************************************************
C  COMPUTE THE TOTAL INFLOW TO GROUNDWATER STORAGES
C*********************************************************
C
      IF (SMR.LT.1.0) GO TO 100
      IF (P.LT.0.001) GO TO 100
      IF (AI.LE.AICR) GO TO 110
      GF=CG**(AI-AICR)
      GO TO 120
  100 GF=0.0
      GO TO 120
  110 GF=1.0
  120 GWIN=GF*(P-RS)
C
C**************************************************
C  INCREMENT BASEFLOW INFLOW INDEX AND STORAGE
C**************************************************
C
      BFI=BFI*(BFIK**DT)+GWIN
      IF (BFI.LT.0.001) BFI=0.0
      BFSC=BFSC+GWIN
C
C*******************************************
C   COMPUTE BASEFLOW RUNOFF
C*******************************************
C
      PWR=1.0-(BFPK**DT)
      RG=PWR*(1.0+BFIM*BFI)*BFSC
      IF (RG.LE.BFSC) GO TO 130
      RG=BFSC
  130 BFSC=BFSC-RG
      IF (BFSC.LT.0.001) BFSC=0.0
C
      RETURN
      END
