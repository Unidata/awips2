C MEMBER EX54
C
      SUBROUTINE EX54(PO,CO,PTS,RTS,PETS,RSTS,RGTS,SMTS,TATS,PPTS,WETS,
     +                FETS,SNTS)
C
C     THIS IS THE EXECUTION SUBROUTINE FOR THE SIMPLE WATER BALANCE
C        MODEL (SWB-NILE) OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        QINGYUN DUAN - GCIP CLIMATE PROJECT - SEPTEMBER 1995  VERSION 1
C
      DIMENSION PO(1),CO(1),PTS(1),RTS(1),PETS(1),RSTS(1),RGTS(1)
      DIMENSION SMTS(1),PPTS(1),WETS(1),TATS(1),FETS(1),SNTS(1)
      DIMENSION SNAME(2),NDAYS(12),UNIT(2),EPDIST(24),DIST(24)
      REAL DMAX,KG,ALPSM,ALPRT,KDT,SU,SB
      REAL KIMP,DSOIL,POROS,WWP,CVICE
C
C***********************
C     COMMON BLOCKS   **
C***********************
C
      COMMON/PM54/DMAX,KG,ALPSM,ALPRT,KDT
      COMMON/FZPM54/KIMP,DSOIL,POROS,WWP,CVICE
      COMMON/CO54/SU,SB,FDP(2),TDP(2),SDP,SDN,WICE(2)
      COMMON/SUMS54/SP,SR,SET,SRS,SRG
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C                                           *--> FROM COMMON.IONUM
      COMMON/IONUM/IN,IPR,IPU
C
C                                           *--> FROM COMMON.FCTIME
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     +              LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C
C                                           *--> FROM COMMON.FNOPR
      COMMON /FNOPR/ NOPROT
C
C                                           *--> FROM COMMON.FENGMT
      COMMON /FENGMT/ METRIC
C
C                                           *--> FROM COMMON.FSACPR
      COMMON /FSACPR/IPRSAC,NOFRZE
C
C                                           *--> FROM COMMON.FCARY
      COMMON /FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
C
C                                           *--> FROM COMMON.FPROG
      COMMON /FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/ex54.f,v $
     . $',                                                             '
     .$Id: ex54.f,v 1.2 1998/04/07 12:13:29 page Exp $
     . $' /
C    ===================================================================
C
C
C
C***************************
C     DATA STATEMENTS     **
C***************************
C
C     DATA STATEMENTS.
      DATA DIST/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.02,0.05,0.10,0.16,
     1          0.20,0.18,0.14,0.09,0.05,0.01,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA SNAME/4HEX24,4H    /
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA UNIT/4HIN. ,4HMM. /
      DATA BLANK/4H    /
C
C***************************************************************
C     CHECK DEBUG AND TRACE CONTROL --  TRACE LEVEL = 1
C***************************************************************
C
      CALL FPRBUG(SNAME,1,54,IBUG)
C
C************************************************
C     GET VALUES OF CONTROL VARIABLES
C************************************************
C
      ITP=PO(7)
      NV=24/ITP
      LPM=PO(14)
      NPM=PO(15)
      LET=PO(16)
      ITET=24
      IET=1
      IF(PO(LET+2).EQ.BLANK) IET=0
      LRS=PO(19)
      LRG=PO(20)
      LSM=PO(21)
      IF (LSM.GT.0) ITSM=PO(LSM+3)
      IFRZE=0
      LFRZE=PO(22)
      IF (LFRZE.GT.0) IFRZE=1
      IF (NOFRZE.EQ.1) IFRZE=0
      LTA=PO(23)
      IF (LTA.GT.0) ITTA=PO(LTA+3)
      LPP=PO(24)
      IF (LPP.GT.0) ITPP=PO(LPP+3)
      LWE=PO(25)
      IF (LWE.GT.0) ITWE=PO(LWE+3)
      LFE=PO(26)
      IF (LFE.GT.0) ITFE=PO(LFE+3)
      LSN=PO(27)
      IF (LSN.GT.0) ITSN=PO(LSN+3)
      IPRINT=PO(28)
      IF (NOPROT.EQ.1) IPRINT=0
      LPO=PO(29)
      LCO=PO(30)
      LSUMS=PO(31)
C
C****************************************************
C     DEBUG OUTPUT - PRINT THE PO AND CO ARRAYS
C****************************************************
C
      IF (IBUG.EQ.0) GO TO 100
C
      WRITE(IODBUG,900) LPO,LCO
  900 FORMAT(1H0,'CONTENTS OF THE PO AND C ARRAYS FOR SWB-NILE',
     -       5X,'NUMBER OF VALUES--PO=',I3,2X,'CO=',I3)
      WRITE(IODBUG,902) (PO(I),I=1,LPO)
  902 FORMAT(1H0,16F8.3)
      WRITE(IODBUG,902) (CO(I),I=1,LCO)
      WRITE(IODBUG,904)
  904 FORMAT(1H0,'CONTROL VARIABLES, POINTERS, AND TIME INTERVALS')
      WRITE(IODBUG,906) ITP,IPRINT,LPM,IFRZE
  906 FORMAT(1H0,16I5)
      WRITE(IODBUG,906) LET,ITET,LRS,LRG,LSM,ITSM,LTA,ITTA,LPP,ITPP,LWE,
     -                  ITWE,LFE,ITFE,LSN,ITSN
C
C************************************
C     GET THE CURRENT EXTERNAL TIME
C************************************
C
  100 CALL MDYH1(IDA,IHR,MONTH,IDAY,IYEAR,IHOUR,NOUTZ,NOUTDS,TZCODE)
      LAST=NDAYS(MONTH)
      IF((MONTH.EQ.2).AND.(((IYEAR/4)*4).EQ.IYEAR)) LAST=LAST+1
C
C*************************************************************
C     PRINT HEADING FOR THE DETAILED OUTPUT -- IF REQUESTED
C*************************************************************
C
      IF (IBUG.EQ.1) IPRINT=1
      IF (IPRINT.EQ.0) GO TO 108
      IF (IPRINT.EQ.1) GO TO 106
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
      IF ((JMO.GE.MO1).AND.(JMO.LE.MO2)) GO TO 102
      IPRINT=0
      GO TO 108
C
  102 IF (JMO.NE.MO2) GO TO 104
      IF (ICKPD.EQ.3) GO TO 104
      ICKPD=ICKPD+1
      PO(IPRINT)=ICKPD+0.01
  104 IPRINT=1
C
  106 IOUT=IPR
      IF (IBUG.EQ.1) IOUT=IODBUG
      IF((IBUG.EQ.0).AND.(MAINUM.GT.1)) WRITE (IOUT,908)
  908 FORMAT(1H1)
C
      WRITE(IOUT,910) (PO(I),I=2,6),MONTH,IYEAR,TZCODE,UNIT(METRIC+1)
  910 FORMAT(1H0,'DETAILED SWB-NILE OUTPUT FOR',1X,5A4,5X,I2,'/',
     -       I4,3X,'TIME ZONE=',A4,10X,'UNITS ARE',1X,A4)
C
      IF (IFRZE.EQ.0) WRITE(IOUT,911)
  911 FORMAT(1H0,'DAY-HR',2X,'PRECIP',2X,'POT-ET',2X,'EST-ET',3X,
     +       'ET-UP',3X,'ET-BT',2X,'TOT-RO',2X,'SUR-RO',2X,'GRD-RO',
     +       6X,'SU',6X,'SB')
      IF (IFRZE.EQ.1) WRITE(IOUT,912)
  912 FORMAT(1H0,'DAY-HR',2X,'PRECIP',2X,'POT-ET',2X,'EST-ET',3X,
     +       'ET-UP',3X,'ET-BT',2X,'TOT-RO',2X,'SUR-RO',2X,'GRD-RO',
     +       6X,'SU',6X,'SB',2X,'FDP-UP',2X,'FDP-BT',2X,'TDP-UP',
     +       2X,'FDP-BT',2X,'WICE-U',2X,'WICE-B')
C
C********************************
C     GET PARAMETER VALUES
C********************************
C
  108 PXADJ=PO(LPM)
      PEADJ=PO(LPM+1)
      DMAX=PO(LPM+2)
      KG=PO(LPM+3)
      ALPSM=PO(LPM+4)
      ALPRT=PO(LPM+5)
      KDT=PO(LPM+6)
      IOPTET=PO(LPM+7)
C
C*********************************************
C     FROZEN GROUND PARAMETERS IF NEEDED
C*********************************************
C
      IF (IFRZE.EQ.0) GO TO 110
      KIMP=PO(LFRZE)
      DSOIL=PO(LFRZE+1)
      POROS=PO(LFRZE+2)
      WWP=PO(LFRZE+3)
      CVICE=PO(LFRZE+4)
C
C**************************
C    GET CARRYOVER VALUES
C**************************
C
  110 SU=CO(1)
      SB=CO(2)
C
C**********************************
C   CONVERT LENGTH UNITS CM TO MM
C**********************************
C
      IF (IFRZE.EQ.0) GO TO 112
      FDP(1)=CO(3)*10.0
      FDP(2)=CO(4)*10.0
      TDP(1)=CO(5)*10.0
      TDP(2)=CO(6)*10.0
      SDP=CO(7)*10.0
      SDN=CO(8)
      WICE(1)=CO(9)
      WICE(2)=CO(10)
C
C***************************************
C    INITIALIZE WATER BALANCE SUMS
C***************************************
C
  112 SP=0.0
      SET=0.0
      SR=0.0
      SRS=0.0
      SRG=0.0
C
C******************************************************
C     DEFINE DAILY ET-DISTRIBUTION FOR INTERNAL TIME
C******************************************************
C
      IF (IOPTET.GT.0) GO TO 116
C
C*************************************
C     UNIFORM DAILY ET DISTRIBUTION
C*************************************
C
      V=1.0/NV
      DO 114 I=1,NV
  114 EPDIST(I)=V
      GO TO 120
C
C*************************************
C     DIURNAL DAILY ET DISTRIBUTION
C*************************************
C
  116 DO 118 I=1,NV
      I2=I*ITP
      I1=(I-1)*ITP+1
      EPDIST(I)=0.0
      DO 118 J=I1,I2
      L=J+LOCAL
      IF (L.GT.24) L=L-24
      EPDIST(I)=EPDIST(I)+DIST(L)
  118 CONTINUE
C
C*****************************************
C     INITIAL ET-DEMAND OR PE-ADJUSTMENT
C*****************************************
C
  120 CALL MDYH1(IDA,IHR,MOET,IDET,IYET,I,100,0,V)
C     DETERMINE NUMBER OF DAYS IN MONTH FOR INCREMENTING.
      ND=NDAYS(MOET)
      IF ((MOET.EQ.2).AND.(((IYET/4)*4).EQ.IYET)) ND=ND+1
      IET1=LET+3
      IF (IDET.LT.16) GO TO 122
      EI=PO(IET1+11+MOET)
      V=IDET-16
      ETFAC=PO(IET1+MOET-1)+V*EI
      GO TO 124
  122 J=MOET-1
      IF (J.EQ.0) J=12
      EI=PO(IET1+11+J)
      V=16-IDET
      ETFAC=PO(IET1+MOET-1)-V*EI
C
C*****************************************************
C    INITIAL TIMING VALUE AND CARRYOVER COUNTER
C*****************************************************
C
  124 KDA=IDA
      KHR=IHR
      KOFF=KDA-IDADAT
C
      IF (IBUG.EQ.0) GO TO 126
      WRITE(IODBUG,914)
  914 FORMAT(1H0,'INITIAL TIMING VARIABLES')
      WRITE(IODBUG,906) IDA,IHR,IDADAT,KDA,KHR,KOFF,MONTH,
     +                  IDAY,IYEAR,IHOUR,LAST,ITP
C
  126 IC=1
      GO TO 130
C
C*******************************
C     BEGIN DAY AND HOUR LOOP
C     GET DATA VALUES FOR THE TIME INTERVAL
C     POTENTIAL EVAPORATION
C*******************************
C
  128 IF (KHR.NE.ITP) GO TO 136
      ETFAC=ETFAC+EI
      IF (IDAY.EQ.16) EI=PO(IET1+11+MONTH)
  130 IF (IET.GT.0) GO TO 132
C     NO PE INPUT - GET ET DEMAND FROM SEASONAL CURVE.
      EPUADJ=ETFAC
      GO TO 134
C     DAILY PE TIME SERIES AVAIABLE
  132 EPUADJ=PETS(KOFF+1)
      IF (EPUADJ.LT.0.00001) EPUADJ=0.0
      EPUADJ=EPUADJ*ETFAC
  134 EP=EPUADJ*PEADJ
      ETD=EP
C     OBTAIN ET DEMAND FOR CURRENT TIME INTERVAL.
  136 J=KHR/ITP
      ETDM=ETD*EPDIST(J)
      KINT=KHR/ITP
      J=KOFF*(24/ITP)+KINT
C
C***************************************************
C     PRECIPITATION (RAIN+SNOWMELT)
C***************************************************
C
      L=KOFF*(24/ITP)+KHR/ITP
      RM=PTS(L)
      RM=PXADJ*RM
C
C****************************************
C    PRECIPITATION (RAIN+SNOWFALL)
C****************************************
C
      IF (LPP.EQ.0) GO TO 145
      IF (ITPP.EQ.ITP) GO TO 140
      IF ((KHR-((KHR/ITPP)*ITPP)).EQ.ITP) GO TO 140
      GO TO 145
C
  140 L=KOFF*(24/ITPP)+(KHR-1)/ITPP+1
      PP=PXADJ*PPTS(L)
C
C*********************************
C    WATER-EQUIVALENT
C*********************************
C
  145 IF (LWE.EQ.0) GO TO 155
      IF (ITWE.EQ.ITP) GO TO 150
      IF ((KHR-((KHR/ITWE)*ITWE)).EQ.ITP) GO TO 150
      GO TO 155
C
  150 L=KOFF*(24/ITWE)+(KHR-1)/ITWE+1
      TWE=WETS(L)
C
C****************************
C    AIR TEMPERATURE
C****************************
C
  155 IF (LTA.EQ.0) GO TO 165
      IF (ITTA.EQ.ITP) GO TO 160
      IF ((KHR-((KHR/ITTA)*ITTA)).EQ.ITP) GO TO 160
      GO TO 165
C
  160 L=KOFF*(24/ITTA)+(KHR-1)/ITTA+1
      TA=TATS(L)*1.8+32.0
C
C****************************
C    SNOWFALL
C****************************
C
  165 IF (LSN.EQ.0) GO TO 180
      IF (ITSN.EQ.ITP) GO TO 170
      IF ((KHR-((KHR/ITSN)*ITSN)).EQ.ITP) GO TO 170
      GO TO 180
C
  170 L=KOFF*(24/ITSN)+(KHR-1)/ITSN+1
      SNO=SNTS(L)*1.8+32.0
C
C*********************************************
C    PERFORM RUNOFF COMPUTATIONS
C*********************************************
C
  180 CALL SWB54(RM,ETDM,ET,RS,RG,R,TA,PP,TWE,FEI,SNO,IFRZE,LSN,
     +           ITP,IDAY,IHOUR,IPRINT,IOUT,IBUG)
C
C*******************************
C    SAVE OUTPUT VARIABLES
C    SURFACE RUNOFF
C*******************************
C
      IF (LRS.EQ.0) GO TO 225
      L=KOFF*(24/ITP)+KHR/ITP
      RSTS(L)=RS
C
C************************************
C      GROUNDWATER RUNOFF
C************************************
C
  225 IF (LRG.EQ.0) GO TO 230
      L=KOFF*(24/ITP)+KHR/ITP
      RGTS(L)=RG
C
C************************
C    TOTAL RUNOFF
C************************
C
  230 L=KOFF*(24/ITP)+KHR/ITP
      RTS(L)=R

C
C*********************************************
C    FROST INDEX AND FROST EFFICIENCY INDEX
C*********************************************
C
      IF (IFRZE.EQ.0) GO TO 240
      IF (ITFE.EQ.0) GO TO 240
      IF ((KHR/ITFE)*ITFE.NE.KHR) GO TO 240
      L=KOFF*(24/ITFE)+KHR/ITFE
      FETS(L)=FEI
C
C****************************
C    SOIL MOISTURE CONTENTS
C****************************
C
  240 IF (LSM.EQ.0) GO TO 250
      IF (ITSM.EQ.0) GO TO 250
      IF ((KHR/ITSM)*ITSM.NE.KHR) GO TO 250
      L=KOFF*(24/ITSM)+KHR/ITSM
      SMTS(L+1)=DMAX*ALPRT-SU
      SMTS(L+2)=DMAX-SB
      SMTS(L+3)=0.0
      SMTS(L+4)=0.0
      SMTS(L+5)=0.0
      IF (IFRZE.EQ.0) GO TO 250
      SMTS(L+3)=(FDP(1)+FDP(2))/10.0
      SMTS(L+4)=WICE(1)+WICE(2)
      SMTS(L+5)=SDP/10.
C
C*******************************************
C  CHECK IF CARRYOVER SHOULD BE SAVED
C*******************************************
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
      IF (KHR.LE.24) GO TO 128
      KHR=ITP
      KDA=KDA+1
      KOFF=KOFF+1
      IDET=IDET+1
      IF (IDET.LE.ND) GO TO 128
      IDET=1
      MOET=MOET+1
      IF (MOET.LE.12) GO TO 265
      MOET=1
      IYET=IYET+1
  265 ND=NDAYS(MOET)
      IF ((MOET.EQ.2).AND.(((IYET/4)*4).EQ.IYET)) ND=ND+1
      GO TO 128
C
C********************************************************
C    END OF DAY AND HOUR LOOP
C    STORE CARRYOVER FOR END OF PERIOD IF REQUESTED
C********************************************************
C
  300 IF (IFILLC.EQ.0) GO TO 350
      CO(1)=SU
      CO(2)=SB
C
C********************************************************
C    STORE ADDITIONAL CARRYOVER FOR END OF PERIOD
C    (CONVERT LENGTH UNITS FROM MM TO CM)
C********************************************************
C
      IF (IFRZE.EQ.0) GO TO 350
      CO(3)=FDP(1)/10.0
      CO(4)=FDP(2)/10.0
      CO(5)=TDP(1)/10.0
      CO(6)=TDP(2)/10.0
      CO(7)=SDP/10.0
      CO(8)=SDN
      CO(9)=WICE(1)
      CO(10)=WICE(2)
C
C***************************************
C    STORE SUMS IF REQUESTED
C***************************************
C
  350 IF (LSUMS.EQ.0) GO TO 390
      PO(LSUMS)=SP
      PO(LSUMS+1)=SET
      PO(LSUMS+2)=SR
      PO(LSUMS+3)=SRS
      PO(LSUMS+4)=SRG
C
C**********************
C** EXIT SUBROUTINE  **
C**********************
C
  390 IF (ITRACE.LT.1) GO TO 399
C
      WRITE (IODBUG,916) SNAME
  916 FORMAT(1H0,'**EXIT',1X,2A4)
C
  399 RETURN
      END
