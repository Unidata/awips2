C MEMBER EX24
C  (from old member FCEX24)
C
      SUBROUTINE EX24(PO,CO,PTS,RTS,PETS,SCTS,WETS,TATS,RSTS,
C                             LAST UPDATE: 02/23/94.15:02:38 BY $WC30EA
C
     -RGTS,AITS,APITS,FITS,APICTS,AEITS,ATITS,FEITS,FRSTS)
C
C****************************************************************
C     THIS IS THE EXECUTION CONTROL SUBROUTINE FOR THE 'API-CONT'
C     OPERATION
C****************************************************************
C     INITIALLY WRITTEN BY -- ERIC ANDERSON,HRL,MARCH 1990
C****************************************************************
C
      DIMENSION PO(1),CO(1),PTS(1),RTS(1),PETS(1),SCTS(1),WETS(1)
      DIMENSION TATS(1),RSTS(1),RGTS(1),AITS(1),APITS(1),FITS(1)
      DIMENSION APICTS(1),AEITS(1),ATITS(1),FEITS(1),FRSTS(1)
      DIMENSION SNAME(2),NDAYS(12),UNIT(2),VAR1(3),CTEMP(7)
      DIMENSION HLR(24),PLR(24),TVLR(24)
C
C***********************
C     COMMON BLOCKS   **
C***********************
C
      COMMON/RSPM24/APIK,AIXW,AIXD,CW,CD,SMIX,CS,FRSX,APIX,PEX,PEN,
     -EFC,PIMPV,RIVA,RVAI,WKW,WKD,AEIK,AEIX,AEIN,ATIR,ATIX,ATIN,APIKS
      COMMON/RGPM24/BFPK,BFIK,BFIM,AICR,CG
      COMMON/FIPM24/CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA,ITTA
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
      COMMON/RGCO24/BFSC,BFI
      COMMON/SUMS24/SP,SR,SIMP,SRS,SRG
      COMMON/MOD135/NDT35,IDT35(5),VAL35(5)
      COMMON/FCOAPI/NACV,JHAPI(10),APICO(4,10),BFX(10),FGCO(2,10)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/fsacpr'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ffgctl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/ex24.f,v $
     . $',                                                             '
     .$Id: ex24.f,v 1.1 1995/09/17 18:56:47 dws Exp $
     . $' /
C    ===================================================================
C
C
C***************************
C     DATA STATEMENTS     **
C***************************
C
      DATA SNAME/4HEX24,4H    /
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA UNIT/4HIN. ,4HMM. /
      DATA VAR1/4H  WK,4H AEI,4H ATI/
      DATA BLANK/4H    /
      DATA HLR/.33,.27,.2,.13,.07,0.,.11,.22,.33,.44,.56,.67,.78,.89,
     1 1.0,.93,.87,.8,.73,.67,.6,.53,.47,.4/
C
C***************************************************************
C     CHECK DEBUG AND TRACE CONTROL --  TRACE LEVEL = 1
C***************************************************************
C
      CALL FPRBUG(SNAME,1,24,IBUG)
C
C************************************************
C     GET VALUES OF CONTROL VARIABLES
C************************************************
C
      IVER=PO(1)
      ITP=PO(7)
      IVOPT=PO(14)
      IR=1
      IF (PO(13).EQ.BLANK) IR=0
      LPE=0
      IF (IVOPT.EQ.1) LPE=PO(15)
      IF (LPE.GT.0) PEADJ=PO(LPE+3)
      LSC=PO(16)
      IF (LSC.GT.0) ITSC=PO(LSC+3)
      LWE=PO(17)
      IF (LWE.GT.0) ITWE=PO(LWE+3)
      LFRZE=PO(24)
      LTA=PO(18)
      IF (LTA.GT.0) ITTA=PO(LTA+3)
      IRS=PO(19)
      LRG=PO(20)
      IF (LRG.GT.0) AREA=PO(LRG+3)
      LAI=PO(21)
      IF (LAI.GT.0) ITAI=PO(LAI+3)
      LAPI=PO(22)
      IF (LAPI.GT.0) ITAPI=PO(LAPI+3)
      IPRINT=PO(23)
      IF (IPRSAC.EQ.-1) IPRINT=0
      IF (IPRSAC.EQ.1) IPRINT=1
      IF (NOPROT.EQ.1) IPRINT=0
      LRSPM=PO(26)
      LRGPM=PO(27)
      LPO=PO(28)
      LSUMS=PO(32)
      NRSPM=17
      IF (IVER.EQ.1) NRSPM=16
      LCO=7
      IF (LFRZE.GT.0) ITFI=PO(LFRZE+3)
      IF (LFRZE.GT.0) ITFEI=PO(LFRZE+7)
      IFRZE=0
      IF (LFRZE.GT.0) IFRZE=1
      IF (NOFRZE.EQ.1) IFRZE=0
      LAPIC=PO(29)
      IF (LAPIC.GT.0) ITAPIC=PO(LAPIC+3)
      LAETI=PO(30)
      IF (LAETI.GT.0) ITAETI=PO(LAETI+3)
      LFRS=PO(31)
      AIADJ=PO(25)
      CONV=25.4
      FNPD=24/ITP
      CONVD=(26.89/35.3147)*FNPD
C
C****************************************************
C     DEBUG OUTPUT - PRINT THE PO AND CO ARRAYS
C****************************************************
C
      IF (IBUG.EQ.0) GO TO 100
C
      WRITE(IODBUG,900) LPO,LCO
  900 FORMAT(1H0,'CONTENTS OF THE PO AND C ARRAYS FOR API-CONT',
     -5X,'NUMBER OF VALUES--PO=',I3,2X,'CO=',I3)
C
      WRITE(IODBUG,901) (PO(I),I=1,LPO)
  901 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,901) (CO(I),I=1,7)
      WRITE(IODBUG,920)
  920 FORMAT(1H0,'CONTROL VARIABLES, POINTERS, AND TIME INTERVALS')
      WRITE(IODBUG,921) ITP,IVOPT,IPRINT,LRSPM,LRGPM,IFRZE,NOFRZE,
     -  IR,IRS,LRG,LSUMS,IPRSAC
  921 FORMAT(1H0,12I10)
      WRITE(IODBUG,921) LPE,LSC,ITSC,LWE,ITWE,LTA,ITTA,
     -  LAI,ITAI,LAPI,ITAPI
      WRITE(IODBUG,921) LFRZE,ITFI,LAPIC,ITAPIC,LAETI,ITAETI,
     -  LFRS,ITFEI
      WRITE(IODBUG,921) IDA,IHR,LDA,LHR,LDACPD,LHRCPD,IDADAT,
     -  MAINUM,IFFG,IFILLC,NCSTOR
      IF ((IVOPT.EQ.1).AND.(NDT35.GT.0)) WRITE(IODBUG,923)
     -  NDT35,(IDT35(I),VAL35(I),I=1,NDT35)
  923 FORMAT(1H0,'MOD135 DEBUG',I5,5(I7,F7.2))
      IF (NACV.EQ.0) GO TO 100
      WRITE(IODBUG,924) NACV
  924 FORMAT(1H0,'FCOAPI COMMON BLOCK CONTENTS -- NACV=',I3)
      DO 90 N=1,NACV
      WRITE(IODBUG,925) JHAPI(N),(APICO(I,N),I=1,4),BFX(N),
     -  (FGCO(I,N),I=1,2)
  925 FORMAT(1H ,5X,I10,7F10.2)
   90 CONTINUE
C
C***************************************
C     GET THE CURRENT EXTERNAL TIME
C***************************************
C
  100 CALL MDYH1(IDA,IHR,MONTH,IDAY,IYEAR,IHOUR,NOUTZ,NOUTDS,
     -TZCODE)
      LAST=NDAYS(MONTH)
      IF((MONTH.EQ.2).AND.(((IYEAR/4)*4).EQ.IYEAR)) LAST=LAST+1
C
C*************************************************************
C     PRINT HEADING FOR THE DETAILED OUTPUT -- IF REQUESTED
C*************************************************************
C
      IF (IBUG.EQ.1) IPRINT=1
      IF (IPRINT.EQ.0) GO TO 105
      IF (IPRINT.EQ.1) GO TO 103
C
C*****************************************************************
C     CHECK IF DETAILED OUTPUT NEEDED FOR THIS MONTH - CALIBRATION
C*****************************************************************
C
      ICKPD=PO(IPRINT)
      JMO=IYEAR*12+MONTH
      L=(ICKPD-1)*2+1
      MO1=PO(IPRINT+L)
      MO2=PO(IPRINT+L+1)
      IF ((JMO.GE.MO1).AND.(JMO.LE.MO2)) GO TO 101
      IPRINT=0
      GO TO 105
C
  101 IF (JMO.NE.MO2) GO TO 102
      IF (ICKPD.EQ.3) GO TO 102
      ICKPD=ICKPD+1
      PO(IPRINT)=ICKPD+0.01
  102 IPRINT=1
C
  103 IOUT=IPR
      IF (IBUG.EQ.1) IOUT=IODBUG
      IF((IBUG.EQ.0).AND.(MAINUM.GT.1)) WRITE (IOUT,902)
C
  902 FORMAT(1H1)
C
      WRITE(IOUT,903) (PO(I),I=2,6),MONTH,IYEAR,TZCODE,UNIT(METRIC+1)
  903 FORMAT(1H0,'DETAILED API-CONT OUTPUT FOR',1X,5A4,5X,I2,'/',
     -I4,3X,'TIME ZONE=',A4,10X,'UNITS ARE',1X,A4)
C
      WRITE(IOUT,904) VAR1(IVOPT+1)
  904 FORMAT(1H0,'DAY-HR',1X,'PRECIP',1X,'SUR-RO',4X,'API',3X,A4,
     -6X,'Y',5X,'AI',4X,'SMI',4X,'AIF',5X,'FS',5X,'FG',5X,'GI',
     -5X,'GS',4X,'BFI',2X,'GW-RO',1X,'TOT-RO',5X,'FI',4X,'FEI')
C
C*****************************************************
C   GET TEMPERATURE INFORMATION AND LAPSE RATES IF NEDED
C********************************************************
C
  105 IF ((IFRZE.EQ.0).AND.(IVOPT.NE.2)) GO TO 110
      EDIFF=PO(LTA+4)
      EDIFF=EDIFF*0.001
      TALX=PO(LTA+5)
      TALN=PO(LTA+6)
      DIFF=TALX-TALN
      NP=24/ITP
      DO 106 I=1,NP
      PLR(I)=0.0
      L1=(I-1)*ITP+1+LOCAL
      L2=I*ITP+LOCAL
      DO 107 II=L1,L2
      LH=II
      IF (II.GT.24) LH=II-24
  107 PLR(I)=PLR(I)+(TALN+HLR(LH)*DIFF)
      PLR(I)=PLR(I)/ITP
  106 CONTINUE
      IF (IVOPT.NE.2) GO TO 110
      NP=24/ITTA
      DO 108 I=1,NP
      TVLR(I)=0.0
      L1=(I-1)*ITTA+1+LOCAL
      L2=I*ITTA+LOCAL
      DO 109 II=L1,L2
      LH=II
      IF (II.GT.24) LH=II-24
  109 TVLR(I)=TVLR(I)+(TALN+HLR(LH)*DIFF)
      TVLR(I)=TVLR(I)/ITTA
  108 CONTINUE
C
C********************************
C     GET PARAMETER VALUES
C     SURFACE RUNOFF
C********************************
C
  110 PXADJ=PO(LRSPM)
      APIK=PO(LRSPM+1)
      AIXW=PO(LRSPM+2)
      AIXD=PO(LRSPM+3)
      CW=PO(LRSPM+4)
      CD=PO(LRSPM+5)
      SMIX=PO(LRSPM+6)
      CS=PO(LRSPM+7)
      FRSX=PO(LRSPM+8)
      APIX=PO(LRSPM+9)
      PEX=PO(LRSPM+10)
      PEN=PO(LRSPM+11)
      EFC=PO(LRSPM+12)
      PIMPV=PO(LRSPM+13)
      RIVA=PO(LRSPM+14)
      RVAI=PO(LRSPM+15)
      IF (IVER.EQ.1) GO TO 114
      APIKS=PO(LRSPM+16)
      GO TO 113
  114 APIKS=0.9
      IF ((LSC.GT.0).OR.(LWE.GT.0)) APIKS=APIK+(1.0-APIK)*(1.0-EFC)
  113 IF (IVOPT.NE.0) GO TO 111
      WKW=PO(LRSPM+NRSPM)
      WKD=PO(LRSPM+NRSPM+1)
      GO TO 115
C
  111 IF (IVOPT.NE.1) GO TO 112
      AEIK=PO(LRSPM+NRSPM)
      AEIX=PO(LRSPM+NRSPM+1)
      AEIN=PO(LRSPM+NRSPM+2)
      GO TO 115
C
  112 ATIR=PO(LRSPM+NRSPM)
      ATIX=PO(LRSPM+NRSPM+1)
      ATIN=PO(LRSPM+NRSPM+2)
C
C*********************************************
C      GROUND WATER RUNOFF PARAMETERS
C*********************************************
C
  115 BFPK=PO(LRGPM)
      BFIK=PO(LRGPM+1)
      BFIM=PO(LRGPM+2)
      AICR=PO(LRGPM+3)
      CG=PO(LRGPM+4)
C
C*********************************************
C     FROZEN GROUND PARAMETERS IF NEEDED
C*********************************************
C
      IF (IFRZE.EQ.0) GO TO 120
      FICR=PO(LFRZE+8)
      CF=PO(LFRZE+9)
      CP=PO(LFRZE+10)
      CSOIL=PO(LFRZE+11)
      CSNOW=PO(LFRZE+12)
      GHC=PO(LFRZE+13)
      CT=PO(LFRZE+14)
      EFA=PO(LFRZE+15)
C
C*******************************
C    GET CARRYOVER VALUES
C*******************************
C
  120 API=CO(1)
      SMI=CO(2)
      BFSC=CO(3)
      BFI=CO(4)
      IF (IVOPT.EQ.1) AEI=CO(5)
      IF (IVOPT.EQ.2) ATI=CO(5)
      IF (LFRZE.EQ.0) GO TO 130
      FI=CO(6)
      FEI=CO(7)
      IF (IFRZE.EQ.0) GO TO 130
      GO TO 135
  130 FI=32.0
      FEI=0.0
C
C     CHECK FOR CHANGES TO THE INITIAL CARRYOVER
  135 NM=0
      KPHR=IHR-ITP
      IF (MAINUM.NE.1) GO TO 140
      IF (NACV.EQ.0) GO TO 140
      JH=(IDA-1)*24+KPHR
      DO 136 I=1,NACV
      IF (JH.NE.JHAPI(I)) GO TO 136
      NM=1
      IF (BFX(I).GE.0.0) BFX(I)=-999.
      CALL CHCO24(IDA,KPHR,NOUTZ,NOUTDS,I,IFRZE,IPRINT,IOUT,APIX,SMIX,
     -  IBUG)
  136 CONTINUE
C
C***************************************
C    INITIALIZE WATER BALANCE SUMS
C***************************************
C
  140 SP=0.0
      SR=0.0
      SIMP=0.0
      SRS=0.0
      SRG=0.0
C
C*****************************************************
C    INITIAL TIMING VALUE AND CARRYOVER COUNTER
C*****************************************************
C
      KDA=IDA
      KHR=IHR
      KOFF=KDA-IDADAT
      IF (MAINUM.NE.1) GO TO 150
      IF (IFFG.EQ.0) GO TO 150
C     CALCULATE THE COMPUTATIONAL PERIOD AT OR JUST BEFORE LSTCMPDY
      KDAFFG=LDACPD
      KHRFFG=(LHRCPD/ITP)*ITP
      IF (KHRFFG.GT.0) GO TO 150
      KDAFFG=KDAFFG-1
      KHRFFG=24
  150 IC=1
      ICAEI=1
      IF (IBUG.EQ.0) GO TO 200
      WRITE(IODBUG,922)
  922 FORMAT(1H0,'INITIAL TIMING VARIABLES')
      WRITE(IODBUG,921) IDA,IHR,IDADAT,KDA,KHR,KOFF,
     -  MONTH,IDAY,IYEAR,IHOUR,LAST,ITP
C
C***************************************************
C     BEGIN DAY AND HOUR LOOP
C     GET DATA VALUES FOR THE TIME INTERVAL
C     PRECIPITATION
C***************************************************
C
  200 L=KOFF*(24/ITP)+KHR/ITP
      P=PTS(L)/CONV
      P=PXADJ*P
C
C*******************************
C     POTENTIAL EVAPORATION
C*******************************
C
      IF (LPE.EQ.0) GO TO 205
      IF (KHR.EQ.ITP) GO TO 201
      GO TO 205
C
201   L=KOFF+1
      PE=PETS(L)/CONV
      PE=PEADJ*PE
C     CHECK IF AEI IS TO BE CHANGED -- OBSERVED DATA PERIOD ONLY
      IF (ICAEI.GT.NDT35) GO TO 205
      IF ((IDT35(ICAEI)*24).GT.(LDACPD*24+LHRCPD)) GO TO 205
      IF (IDT35(ICAEI).EQ.(KDA+1)) GO TO 202
      GO TO 205
  202 AEI=VAL35(ICAEI)
      ICAEI=ICAEI+1
      IF (AEI.GT.AEIX) AEI=AEIX
      IF (AEI.LT.AEIN) AEI=AEIN
C     MAKE CHANGES SO INPUT AEI USED FOR THE DAY INSPITE OF APIC24 EQ.
      PE=0.0
      AEI=AEI/AEIK
C
C****************************************
C    AREAL EXTENT OF SNOW COVER
C****************************************
C
  205 IF (LSC.EQ.0) GO TO 210
      IF (ITSC.EQ.ITP) GO TO 206
      IF ((KHR-((KHR/ITSC)*ITSC)).EQ.ITP) GO TO 206
      GO TO 210
C
  206 L=KOFF*(24/ITSC)+(KHR-1)/ITSC+1
      AESC=SCTS(L)
C
C*********************************
C    WATER-EQUIVALENT
C*********************************
C
  210 IF (LWE.EQ.0) GO TO 215
      IF (ITWE.EQ.ITP) GO TO 211
      IF ((KHR-((KHR/ITWE)*ITWE)).EQ.ITP) GO TO 211
      GO TO 215
C
  211 L=KOFF*(24/ITWE)+(KHR-1)/ITWE+1
      WE=WETS(L)/CONV
C
C****************************
C    AIR TEMPERATURE
C****************************
C
  215 IF ((IFRZE.EQ.0).AND.(IVOPT.NE.2)) GO TO 220
      IF (ITTA.EQ.ITP) GO TO 216
      IF ((KHR-((KHR/ITTA)*ITTA)).EQ.ITP) GO TO 216
      GO TO 220
C
  216 L=KOFF*(24/ITTA)+(KHR-1)/ITTA+1
      TA=TATS(L)*1.8+32.0-PLR(KHR/ITP)*EDIFF
      IF (IVOPT.NE.2) GO TO 220
      IF (KHR.GT.ITP) GO TO 220
      N=24/ITTA
      TAVG=0.0
      DO 217 I=1,N
      L=KOFF*(24/ITTA)+I
  217 TAVG=TAVG+(TATS(L)*1.8+32.0-TVLR(I)*EDIFF)
      TAVG=TAVG/N
C
C*********************************************
C    PERFORM RUNOFF COMPUTATIONS
C*********************************************
C
  220 CALL APIC24(P,RS,FRS,AI,RG,R,PE,AESC,TA,TAVG,WE,AIADJ,KHR,MONTH,
     -IDAY,IHOUR,IVOPT,IFRZE,LWE,LSC,IPRINT,IOUT,ITP,IBUG)
C
C**************************************************
C     CHECK FOR CHANGES TO CARRYOVER
C**************************************************
C
      IF (MAINUM.NE.1) GO TO 223
      IF (NM.EQ.NACV) GO TO 223
      JH=JH+ITP
      DO 221 I=1,NACV
      IF (JH.NE.JHAPI(I)) GO TO 221
      NM=NM+1
      CALL CHCO24(KDA,KHR,NOUTZ,NOUTDS,I,IFRZE,IPRINT,IOUT,APIX,SMIX,
     -  IBUG)
  221 CONTINUE
C
C*******************************
C    SAVE OUTPUT VARIABLES
C    STORM RUNOFF
C*******************************
C
  223 IF (IRS.EQ.0) GO TO 224
      L=KOFF*(24/ITP)+KHR/ITP
      RSTS(L)=RS*CONV
C
C***************************************
C   FRACTION SURFACE RUNOFF
C***************************************
C
  224 IF (LFRS.EQ.0) GO TO 225
      L=KOFF*(24/ITP)+KHR/ITP
      FRSTS(L)=FRS
C
C************************
C   ANTECEDENT INDEX
C************************
C
  225 IF (LAI.EQ.0) GO TO 230
      IF ((KHR/ITAI)*ITAI.NE.KHR) GO TO 230
      L=KOFF*(24/ITAI)+KHR/ITAI
      AITS(L)=AI*CONV
C
C*****************
C    API
C*****************
C
  230 IF (LAPI.EQ.0) GO TO 231
      IF ((KHR/ITAPI)*ITAPI.NE.KHR) GO TO 231
      L=KOFF*(24/ITAPI)+KHR/ITAPI
      APITS(L)=API*CONV
C
C*****************
C    AEI
C*****************
C
  231 IF (LAETI.EQ.0) GO TO 232
      IF (IVOPT.EQ.2) GO TO 232
      IF ((KHR/ITAETI)*ITAETI.NE.KHR) GO TO 232
      L=KOFF*(24/ITAETI)+KHR/ITAETI
      AEITS(L)=AEI*CONV
C
C*****************
C    ATI
C*****************
C
  232 IF (LAETI.EQ.0) GO TO 235
      IF (IVOPT.EQ.1) GO TO 235
      IF ((KHR/ITAETI)*ITAETI.NE.KHR) GO TO 235
      L=KOFF*(24/ITAETI)+KHR/ITAETI
      ATITS(L)=(ATI-32.0)/1.8
C
C***************************************
C    FROST INDEX AND FROST EFFICIENCY INDEX
C***************************************
C
  235 IF (LFRZE.EQ.0) GO TO 240
      IF (ITFI.EQ.0) GO TO 236
      IF ((KHR/ITFI)*ITFI.NE.KHR) GO TO 236
      L=KOFF*(24/ITFI)+KHR/ITFI
      FITS(L)=(FI-32.0)/1.8
  236 IF (ITFEI.EQ.0) GO TO 240
      IF ((KHR/ITFEI)*ITFEI.NE.KHR) GO TO 240
      L=KOFF*(24/ITFEI)+KHR/ITFEI
      FEITS(L)=FEI
C
C************************************
C      GROUNDWATER RUNOFF OR DISCHARGE
C************************************
C
  240 IF (LRG.EQ.0) GO TO 244
      L=KOFF*(24/ITP)+KHR/ITP
      IF (AREA.GT.0.0) GO TO 241
      RGTS(L)=RG*CONV
      GO TO 244
  241 RGTS(L)=RG*CONVD*AREA
C
C***************************
C     API CONTENTS
C***************************
C
  244 IF (LAPIC.EQ.0) GO TO 245
      IF ((KHR/ITAPIC)*ITAPIC.NE.KHR) GO TO 245
      L=KOFF*(24/ITAPIC)*5+((KHR-1)/ITAPIC)*5
      APICTS(L+1)=API*CONV
      APICTS(L+2)=AI*CONV
      APICTS(L+3)=SMI*CONV
      APICTS(L+4)=BFSC*CONV
      APICTS(L+5)=BFI*CONV
C
C************************
C    TOTAL RUNOFF
C************************
C
  245 IF (IR.EQ.0) GO TO 246
      L=KOFF*(24/ITP)+KHR/ITP
      RTS(L)=R*CONV
C
C****************************************
C   STORE FFG CARRYOVER IF REQUESTED
C****************************************
C
  246 IF (IFILLC.EQ.0) GO TO 248
      IF (MAINUM.NE.1) GO TO 248
      IF (IFFG.EQ.0) GO TO 248
      IF ((KDA.EQ.KDAFFG).AND.(KHR.EQ.KHRFFG)) GO TO 247
      GO TO 248
  247 CO(1)=API
      CO(2)=SMI
      CO(3)=BFSC
      CO(4)=BFI
      CO(5)=0.0
      IF (IVOPT.EQ.1) CO(5)=AEI
      IF (IVOPT.EQ.2) CO(5)=ATI
      CO(6)=FI
      CO(7)=FEI
C
C*******************************************
C  CHECK IF CARRYOVER SHOULD BE SAVED
C*******************************************
C
  248 IF (IFILLC.EQ.0) GO TO 250
      IF (NCSTOR.EQ.0) GO TO 250
      IF (IC.GT.NCSTOR) GO TO 250
      IF ((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GO TO 249
      GO TO 250
C
C***************************
C    CARRYOVER
C***************************
C
  249 CTEMP(1)=API
      CTEMP(2)=SMI
      CTEMP(3)=BFSC
      CTEMP(4)=BFI
      CTEMP(5)=0.0
      IF (IVOPT.EQ.1) CTEMP(5)=AEI
      IF (IVOPT.EQ.2) CTEMP(5)=ATI
      CTEMP(6)=FI
      CTEMP(7)=FEI
      CALL FCWTCO(KDA,KHR,CTEMP,LCO)
      IC=IC+1
C
C**********************************************
C   CHECK FOR END OF EXECUTION PERIOD
C**********************************************
C
  250 IF ((KDA.EQ.LDA).AND.(KHR.EQ.LHR)) GO TO 300
C
C   INCREMENT TIME AND GO TO THE NEXT PERIOD
C
      IHOUR=IHOUR+ITP
      IF (IHOUR.LE.24) GO TO 260
      IHOUR=IHOUR-24
      IDAY=IDAY+1
      IF (IDAY.LE.LAST) GO TO 260
      IDAY=1
      MONTH=MONTH+1
      IF (MONTH.LE.12) GO TO 255
      MONTH=1
      IYEAR=IYEAR+1
C
  255 LAST=NDAYS(MONTH)
      IF ((MONTH.EQ.2).AND.(((IYEAR/4)*4).EQ.IYEAR)) LAST=LAST+1
C
  260 KHR=KHR+ITP
      IF (KHR.LE.24) GO TO 200
      KHR=ITP
      KDA=KDA+1
      KOFF=KOFF+1
      GO TO 200
C
C********************************************************
C    END OF DAY AND HOUR LOOP
C    STORE CARRYOVER FOR END OF PERIOD IF REQUESTED
C********************************************************
C
  300 IF (IFILLC.EQ.0) GO TO 350
      IF (MAINUM.NE.1) GO TO 310
      IF (IFFG.NE.0) GO TO 350
  310 CO(1)=API
      CO(2)=SMI
      CO(3)=BFSC
      CO(4)=BFI
      CO(5)=0.0
      IF (IVOPT.EQ.1) CO(5)=AEI
      IF (IVOPT.EQ.2) CO(5)=ATI
      CO(6)=FI
      CO(7)=FEI
C
C***************************************
C    STORE SUMS IF REQUESTED
C***************************************
C
  350 IF (LSUMS.EQ.0) GO TO 390
      PO(LSUMS)=SP
      PO(LSUMS+1)=SR
      PO(LSUMS+2)=SIMP
      PO(LSUMS+3)=SRS
      PO(LSUMS+4)=SRG
C
C**********************
C** EXIT SUBROUTINE  **
C**********************
C
  390 IF (ITRACE.LT.1) GO TO 399
C
      WRITE (IODBUG,905) SNAME
  905 FORMAT(1H0,'**EXIT',1X,2A4)
C
  399 RETURN
      END
