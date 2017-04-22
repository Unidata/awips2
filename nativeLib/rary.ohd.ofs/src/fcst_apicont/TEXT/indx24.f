C MEMBER INDX24
C  (from old member FCAPIC24)
C
      SUBROUTINE INDX24(P,LSC,AESC,LWE,WE,IFRZE,FEI,FI,AIR,DT,PED,PE,ED,
     -  ES,API,SMI,IVOPT)
C
C*******************************************************************
C     THIS SUBROUTINE UPDATES THE API, SMI AND FEI INDICIES BASED ON
C       DATA FOR THE CURRENT PERIOD.
C*******************************************************************
C     WRITTEN BY ERIC ANDERSON -- HRL,  JUNE 1990
C*********************************************
C    COMMON BLOCKS
C*********************************************
C
      COMMON/RSPM24/APIK,AIXW,AIXD,CW,CD,SMIX,CS,FRSX,APIX,PEX,PEN,
     -EFC,PIMPV,RIVA,RVAI,WKW,WKD,AEIK,AEIX,AEIN,ATIR,ATIX,ATIN,APIKS
      COMMON/FIPM24/CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA,ITTA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/indx24.f,v $
     . $',                                                             '
     .$Id: indx24.f,v 1.1 1995/09/17 18:58:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C********************************
C    DATA STATEMENT
C********************************
C
      DATA PI/3.1416/
C
C*****************************************************
C    UPDATE FROST EFFICIENCY INDEX FOR RAIN+MELT CHANGES
C*****************************************************
C
      IF (IFRZE.EQ.0) GO TO 130
      IF (FI.GE.FICR) GO TO 130
      IF (P.EQ.0.0) GO TO 130
      FID=FICR-FI
      IF (FID.GT.70.0) FID=70.0
      R=0.5+0.5*(COS(PI*(1.0-(FID/70.0))))
      W=1.0-AIR
      FEI=FEI+(1.0/CP)*R*W*P
      IF (FEI.GT.1.0) FEI=1.0
C
C*********************************************
C    UPDATE  API
C*********************************************
C
  130 PEFF=P
      IF (IFRZE.EQ.0) GO TO 135
      PEFF=(1.0-FEI*EFA)*P
C
  135 PK=APIK
      IZ=0
      IF (IFRZE.EQ.0) GO TO 136
      IF (FI.GE.FICR) GO TO 136
      IZ=1
      IF (EFA.LT.1.0) GO TO 136
      PK=1.0
      GO TO 139
  136 IF (LSC.GT.0) GO TO 137
      IF (LWE.EQ.0) GO TO 138
      AESC=0.0
      IF (WE.GT.0.1) AESC=1.0
  137 PK=PK+(APIKS-PK)*AESC
  138 IF (IZ.EQ.1) PK=1.0*EFA+(1.0-EFA)*PK
C
  139 PK=PK**DT
      API=PK*API+PEFF
      IF (API.GT.APIX) API=APIX
      IF (API.GE.0.005) GO TO 140
      API=0.0
C
C********************************
C     UPDATE SMI
C********************************
C
  140 ED=PED*DT
      IF (IVOPT.EQ.1) ED=PE*PED*DT
      IF (IZ.EQ.0) GO TO 145
      IF (EFA.LT.1.0) GO TO 145
      ED=0.0
      GO TO 148
  145 IF (LSC.GT.0) GO TO 146
      IF (LWE.EQ.0) GO TO 147
      AESC=0.0
      IF (WE.GT.10.0) AESC=1.0
  146 ED=EFC*ED+(1.0-EFC)*(1.0-AESC)*ED
  147 IF (IZ.EQ.1) ED=(1.0-EFA)*ED
C
  148 ES=ED*(SMI/SMIX)
      IF (ES.GT.SMI) ES=SMI
      SMI=SMI-ES+P
      IF (SMI.LT.0.01) SMI=0.0
      IF (SMI.GT.SMIX) SMI=SMIX
C
      RETURN
      END
