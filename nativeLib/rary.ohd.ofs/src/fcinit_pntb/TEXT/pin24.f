C MODULE PIN24
C-----------------------------------------------------------------------
C
      SUBROUTINE PIN24(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
C
C***********************************************************
C  THIS IS THE PARAMETER AND INITIAL CARRYOVER INPUT
C  SUBROUTINE FOR THE API-CONT OPERATION
C***********************************************************
C
C***********************************************************
C  INITIALLY WRITTEN BY -- ERIC ANDERSON,  HRL,  MARCH 1990
C***********************************************************
C
      CHARACTER*80 CARD
C
      DIMENSION PO(*),CO(*)
      DIMENSION SNAME(2),DESCRP(5),PID(2),RID(2),TSID1(2),TSID2(2)
      DIMENSION TSID3(2),TSID4(2),TSID5(2),IM(3),IY(3),LM(3),LY(3)
C
C*********************************************************
C   COMMON BLOCKS
C*********************************************************
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin24.f,v $
     . $',                                                             '
     .$Id: pin24.f,v 1.2 2002/02/11 19:01:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C*********************************************************
C     DATA STATEMENTS
C*********************************************************
C
      DATA SNAME/4HPIN2,4H4   /
      DATA DL,DLES,DTEMP/4HL   ,4HDLES,4HTEMP/
      DATA CMM,DL3T/4HMM  ,4HL3/T/
      DATA CAEI,CATI,CFRZE,CPROT,BLANK/3HAEI,3HATI,4HFRZE,4HPROT,4H    /
      DATA CAPIC,WKN/4HAPIC,3HWKN/
C
C*********************************************************
C     CHECK IF DEBUG OR TRACE ON -- TRACE LEVEL 1
C*********************************************************
C
      CALL FPRBUG(SNAME,1,24,IBUG)
C
C*********************************************************
C     INITIALIZE VARIABLES
C**********************************************************
C
      IVER=2
      LPO=35
      LRSPM=LPO+1
      NRSPM=17
      NRGPM=5
      IUSEP=0
      IUSEC=0
      LCO=7
      IR=1
      IFRZE=0
      LPROT=0
      LSUMS=0
      NOFILL=0
      LPE=0
      LTA=0
      LFRZE=0
      LSC=0
      LWE=0
      LRS=0
      LRG=0
      LAI=0
      LAPI=0
      LFI=0
      LAPIC=0
      LAETI=0
      LFRS=0
      LFEI=0
      IERRPM=0
      IERRCO=0
C****************************************************************
C      CARD NUMBER 1
C*********************************************************
C
      READ(IN,901) DESCRP,ITP,PID,PTYPE,RID,RTYPE,AREA
  901 FORMAT(5A4,3X,I2,7X,2A4,1X,A4,7X,2A4,1X,A4,F7.0)
C
C**********************************************************
C    CHECK THAT TIME SERIES EXISTS
C***********************************************************
C
      CALL CHEKTS(PID,PTYPE,ITP,1,DL,0,1,IERR)
      IF ((RID(1).EQ.BLANK).OR.(RTYPE.EQ.BLANK)) GO TO 100
      CALL CHEKTS(RID,RTYPE,ITP,1,DL,0,1,IERR)
      GO TO 101
  100 IR=0
C
C************************************************
C     CARD NUMBER 2
C************************************************
C
  101 READ(IN,902) Q1,FRZE,AESC,WE,RS,RG,AI,PI,ZI,APIC,AET,FRS,ZEI,
     - PROT,ISUMS
  902 FORMAT(2X,A3,13(1X,A4),1X,I1)
C
      IVOPT=-1
      IF (Q1.EQ.WKN) IVOPT=0
      IF (Q1.EQ.CAEI) IVOPT=1
      IF (Q1.EQ.CATI) IVOPT=2
      IF (IVOPT.GE.0) GO TO 105
      WRITE(IPR,920) Q1
  920 FORMAT(1H0,10X,'**ERROR**  THE FIRST QUADRANT OPTION=',A3,
     -  ', IT SHOULD BE WKN, AEI, OR ATI.')
      CALL ERROR
      GO TO 990
  105 LRGPM=LRSPM+NRSPM+2
      IF (IVOPT.GT.0) LRGPM=LRGPM+1
      LPO=LRGPM+NRGPM-1
      IF (IVOPT.NE.1) GO TO 110
      LPE=LPO+1
      LPO=LPO+4
      GO TO 115
C
  110 IF (IVOPT.NE.2) GO TO 115
      LTA=LPO+1
      LPO=LPO+7
C
  115 IF (FRZE.EQ.CFRZE) IFRZE=1
      IF (IFRZE.NE.1) GO TO 120
      LFRZE=LPO+1
      LPO=LPO+16
      IF (LTA.GT.0) GO TO 120
      LTA=LPO+1
      LPO=LPO+7
C
  120 IF (AESC.EQ.BLANK) GO TO 125
      LSC=LPO+1
      LPO=LPO+4
C
  125 IF (WE.EQ.BLANK) GO TO 130
      LWE=LPO+1
      LPO=LPO+4
C
  130 IF (RS.EQ.BLANK) GO TO 135
      LRS=LPO+1
      LPO=LPO+3
C
  135 IF (RG.EQ.BLANK) GO TO 140
      LRG=LPO+1
      LPO=LPO+4
C
  140 IF (AI.EQ.BLANK) GO TO 145
      LAI=LPO+1
      LPO=LPO+4
C
  145 IF (PI.EQ.BLANK) GO TO 150
      LAPI=LPO+1
      LPO=LPO+4
C
  150 IF (ZI.EQ.BLANK) GO TO 151
      IF (IFRZE.EQ.0)  GO TO 151
      LFI=LFRZE
C
  151 IF (APIC.EQ.BLANK) GO TO 152
      IF (APIC.NE.CAPIC) GO TO 152
      LAPIC=LPO+1
      LPO=LPO+4
C
  152 IF (AET.EQ.BLANK) GO TO 153
      IF (IVOPT.EQ.0) GO TO 153
      LAETI=LPO+1
      LPO=LPO+4
C
  153 IF (FRS.EQ.BLANK) GO TO 154
      LFRS=LPO+1
      LPO=LPO+3
C
  154 IF (ZEI.EQ.BLANK) GO TO 155
      IF (IFRZE.EQ.0) GO TO 155
      LFEI=LFRZE+4
C
  155 IF (PROT.NE.CPROT) GO TO 157
      IF (MAINUM.GT.2) GO TO 156
      LPROT=1
      GO TO 157
  156 LPROT=LPO+1
      LPO=LPO+7
C
  157 IF (ISUMS.NE.1) GO TO 160
      LSUMS=LPO+1
      LPO=LPO+5
C
C*********************************************************
C  SIZE OF THE PO ARRAY KNOWN -- CHECK IF SPACE AVAILABLE
C*********************************************************
C
  160 CALL CHECKP(LPO,LEFTP,IERR)
      IF (IERR.EQ.1) NOFILL=1
C
C*********************************************************
C  CHECK SPACE FOR CO ARRAY
C*********************************************************
C
      CALL CHECKC(LCO,LEFTC,IERR)
      IF (IERR.EQ.1) NOFILL=1
      IF (NOFILL.EQ.1) GO TO 990
C
C*****************************************************
C LOAD INITIAL VALUES INTO THE PO ARRAY
C******************************************************
C
      PO(1)=IVER+0.01
      DO 165 I=1,5
  165 PO(I+1)=DESCRP(I)
      PO(7)=ITP+0.01
      PO(8)=PID(1)
      PO(9)=PID(2)
      PO(10)=PTYPE
      IF (IR.EQ.0) GO TO 166
      PO(11)=RID(1)
      PO(12)=RID(2)
      PO(13)=RTYPE
      GO TO 167
  166 PO(11)=BLANK
      PO(12)=BLANK
      PO(13)=BLANK
  167 PO(14)=IVOPT+0.01
      PO(15)=LPE+0.01
      PO(16)=LSC+0.01
      PO(17)=LWE+0.01
      PO(18)=LTA+0.01
      PO(19)=LRS+0.01
      PO(20)=LRG+0.01
      PO(21)=LAI+0.01
      PO(22)=LAPI+0.01
      PO(23)=LPROT+0.01
      PO(24)=LFRZE+0.01
      PO(25)=0.0
      PO(26)=LRSPM+0.01
      PO(27)=LRGPM+0.01
      PO(28)=LPO+0.01
      PO(29)=LAPIC+0.01
      PO(30)=LAETI+0.01
      PO(31)=LFRS+0.01
      PO(32)=LSUMS+0.01
      PO(33)=0.01
      PO(34)=0.01
      PO(35)=0.01
C
C*********************************************************
C   CARD NUMBER 3
C*********************************************************
C
      IF ((LPE.EQ.0).AND.(LTA.EQ.0)) GO TO 210
      READ(IN,903) TSID1,PETYPE,PEADJ,TSID2,TATYPE,ITTA,DELEV,TALX,TALN
  903 FORMAT(2X,2A4,1X,A4,F5.2,2X,2A4,1X,A4,3X,I2,5X,F5.0,2F5.1)
C
      IF (LPE.EQ.0) GO TO 205
      PO(LPE)=TSID1(1)
      PO(LPE+1)=TSID1(2)
      PO(LPE+2)=PETYPE
      PO(LPE+3)=PEADJ
      CALL CHEKTS(TSID1,PETYPE,24,1,DL,0,1,IERR)
      IF (PEADJ.LE.0.0) IERRPM=1
C
  205 IF (LTA.EQ.0) GO TO 210
      PO(LTA)=TSID2(1)
      PO(LTA+1)=TSID2(2)
      PO(LTA+2)=TATYPE
      PO(LTA+3)=ITTA+0.01
      PO(LTA+4)=DELEV
      PO(LTA+5)=TALX
      PO(LTA+6)=TALN
      CALL CHEKTS(TSID2,TATYPE,ITTA,1,DTEMP,0,1,IERR)
      IF (DELEV.EQ.0.0) GO TO 206
      IF ((TALX.GE.0.0).AND.(TALN.GE.0.0)) GO TO 206
      WRITE(IPR,921)
  921 FORMAT(1H0,10X,'**WARNING** AT LEAST ONE OF THE LAPSE RATES ARE NO
     1T POSITIVE.  CHECK THAT VALUES ARE CORRECT.')
      CALL WARN
  206 IF ((ITTA/ITP)*ITP.EQ.ITTA) GO TO 210
      WRITE(IPR,900) ITTA,TATYPE,ITP,PTYPE
  900 FORMAT(1H0,10X,'**ERROR**  THE TIME INTERVAL(',I2,1X,
     -'HOURS) FOR TYPE',1X,A4,1X,'IS NOT A MULTIPLE',/16X,
     -'OF THE TIME INTERVAL(',I2,1X,'HOURS) FOR TYPE',1X,A4)
      CALL ERROR
C
C*********************************************
C   CARD NUMBER 4
C**********************************************
C
  210 IF ((LSC.EQ.0).AND.(LWE.EQ.0)) GO TO 220
      READ(IN,904) TSID1,IT1,TSID2,IT2
  904 FORMAT(2(2X,2A4,3X,I2))
C
      IF (LSC.EQ.0) GO TO 215
      PO(LSC)=TSID1(1)
      PO(LSC+1)=TSID1(2)
      PO(LSC+2)=AESC
      PO(LSC+3)=IT1
      CALL CHEKTS(TSID1,AESC,IT1,1,DLES,0,1,IERR)
      IF ((IT1/ITP)*ITP.EQ.IT1) GO TO 215
      WRITE(IPR,900) IT1,AESC,ITP,PTYPE
      CALL ERROR
C
  215 IF (LWE.EQ.0) GO TO 220
      PO(LWE)=TSID2(1)
      PO(LWE+1)=TSID2(2)
      PO(LWE+2)=WE
      PO(LWE+3)=IT2
      CALL CHEKTS(TSID2,WE,IT2,1,DL,0,1,IERR)
      IF ((IT2/ITP)*ITP.EQ.IT2) GO TO 220
      WRITE(IPR,900) IT2,WE,ITP,PTYPE
      CALL ERROR
C
  220 IF (LFRZE.EQ.0) GO TO 225
      PO(LFRZE)=BLANK
      PO(LFRZE+1)=BLANK
      PO(LFRZE+2)=BLANK
      PO(LFRZE+3)=0.01
      PO(LFRZE+4)=BLANK
      PO(LFRZE+5)=BLANK
      PO(LFRZE+6)=BLANK
      PO(LFRZE+7)=0.01
C
C*****************************************
C   CARD NUMBER 5
C*****************************************
C
  225 IF ((LRS.EQ.0).AND.(LRG.EQ.0).AND.(LAI.EQ.0).AND.(LAPI.EQ.0)
     -.AND.(LFI.EQ.0)) GO TO 2451
      READ(IN,905) TSID1,TSID2,TSID3,IT3,TSID4,IT4,TSID5,IT5
  905 FORMAT(2(2X,2A4),3(2X,2A4,3X,I2))
C
      IF (LRS.EQ.0) GO TO 230
      PO(LRS)=TSID1(1)
      PO(LRS+1)=TSID1(2)
      PO(LRS+2)=RS
      CALL CHEKTS(TSID1,RS,ITP,1,DL,0,1,IERR)
C
  230 IF (LRG.EQ.0) GO TO 235
      PO(LRG)=TSID2(1)
      PO(LRG+1)=TSID2(2)
      PO(LRG+2)=RG
      PO(LRG+3)=0.0
      CALL FDCODE(RG,UNITS,DIM,MSG,NV,TIME,NX,IERR)
      IF (UNITS.EQ.CMM) GO TO 234
      PO(LRG+3)=AREA
      CALL CHEKTS(TSID2,RG,ITP,1,DL3T,0,1,IERR)
      GO TO 235
  234 CALL CHEKTS(TSID2,RG,ITP,1,DL,0,1,IERR)
C
  235 IF (LAI.EQ.0) GO TO 240
      PO(LAI)=TSID3(1)
      PO(LAI+1)=TSID3(2)
      PO(LAI+2)=AI
      PO(LAI+3)=IT3
      CALL CHEKTS(TSID3,AI,IT3,1,DL,0,1,IERR)
      IF ((IT3/ITP)*ITP.EQ.IT3) GO TO 240
      WRITE(IPR,900) IT3,AI,ITP,PTYPE
      CALL ERROR
C
  240 IF (LAPI.EQ.0) GO TO 245
      PO(LAPI)=TSID4(1)
      PO(LAPI+1)=TSID4(2)
      PO(LAPI+2)=PI
      PO(LAPI+3)=IT4
      CALL CHEKTS(TSID4,PI,IT4,1,DL,0,1,IERR)
      IF ((IT4/ITP)*ITP.EQ.IT4) GO TO 245
      WRITE(IPR,900) IT4,PI,ITP,PTYPE
      CALL ERROR
C
  245 IF (LFI.EQ.0) GO TO 2451
      PO(LFI)=TSID5(1)
      PO(LFI+1)=TSID5(2)
      PO(LFI+2)=ZI
      PO(LFI+3)=IT5
      CALL CHEKTS(TSID5,ZI,IT5,1,DTEMP,0,1,IERR)
      IF ((IT5/ITP)*ITP.EQ.IT5) GO TO 2451
      WRITE(IPR,900) IT5,ZI,ITP,PTYPE
      CALL ERROR
C
C*********************************************************
C     CARD NUMBER 5A
C*********************************************************
C
 2451 IF ((LAPIC.EQ.0).AND.(LAETI.EQ.0).AND.(LFRS.EQ.0).AND.(LFEI.EQ.0))
     -  GO TO 250
      READ(IN,9051) TSID1,IT1,TSID2,IT2,TSID3,TSID4,IT4
 9051 FORMAT(2(2X,2A4,3X,I2),2X,2A4,7X,2A4,3X,I2)
C
      IF (LAPIC.EQ.0) GO TO 2452
      PO(LAPIC)=TSID1(1)
      PO(LAPIC+1)=TSID1(2)
      PO(LAPIC+2)=APIC
      PO(LAPIC+3)=IT1
      CALL CHEKTS(TSID1,APIC,IT1,1,DL,0,5,IERR)
      IF ((IT1/ITP)*ITP.EQ.IT1) GO TO 2452
      WRITE(IPR,900) IT1,APIC,ITP,PTYPE
      CALL ERROR
C
 2452 IF (LAETI.EQ.0) GO TO 2453
      PO(LAETI)=TSID2(1)
      PO(LAETI+1)=TSID2(2)
      PO(LAETI+2)=AET
      PO(LAETI+3)=IT2
      IF (IVOPT.EQ.1) CALL CHEKTS(TSID2,AET,IT2,1,DL,0,1,IERR)
      IF (IVOPT.EQ.2) CALL CHEKTS(TSID2,AET,IT2,1,DTEMP,0,1,IERR)
      IF ((IT2/ITP)*ITP.EQ.IT2) GO TO 2453
      WRITE(IPR,900) IT2,AET,ITP,PTYPE
      CALL ERROR
C
 2453 IF (LFRS.EQ.0) GO TO 2454
      PO(LFRS)=TSID3(1)
      PO(LFRS+1)=TSID3(2)
      PO(LFRS+2)=FRS
      CALL CHEKTS(TSID3,FRS,ITP,1,DLES,0,1,IERR)
C
 2454 IF (LFEI.EQ.0) GO TO 250
      PO(LFEI)=TSID4(1)
      PO(LFEI+1)=TSID4(2)
      PO(LFEI+2)=ZEI
      PO(LFEI+3)=IT4
      CALL CHEKTS(TSID4,ZEI,IT4,1,DLES,0,1,IERR)
      IF ((IT4/ITP)*ITP.EQ.IT4) GO TO 250
      WRITE(IPR,900) IT4,ZEI,ITP,PTYPE
      CALL ERROR
C
C*******************************************
C    CARD NUMBER 6 AND 6A
C*******************************************
  250 READ(IN,906) PXADJ,AIXW,AIXD,CW,CD,SMIX,CS,FRSX
  906 FORMAT(9F5.2)
      READ(IN,906) PIMPV,APIK,PEX,PEN,EFC,RIVA,RVAI,APIX,APIKS
      IF (APIX.LE.0.0) APIX=10.0
      IF (IVOPT.NE.0) CS=0.0
      PO(LRSPM)=PXADJ
      PO(LRSPM+1)=APIK
      PO(LRSPM+2)=AIXW
      PO(LRSPM+3)=AIXD
      PO(LRSPM+4)=CW
      PO(LRSPM+5)=CD
      PO(LRSPM+6)=SMIX
      PO(LRSPM+7)=CS
      PO(LRSPM+8)=FRSX
      PO(LRSPM+9)=APIX
      PO(LRSPM+10)=PEX
      PO(LRSPM+11)=PEN
      PO(LRSPM+12)=EFC
      PO(LRSPM+13)=PIMPV
      PO(LRSPM+14)=RIVA
      PO(LRSPM+15)=RVAI
      PO(LRSPM+16)=APIKS
      IF (PXADJ.LE.0.0) IERRPM=1
      IF (AIXW.LE.0.0) IERRPM=1
      IF (AIXD.LT.AIXW) IERRPM=1
      IF ((CW.LE.0.0).OR.(CW.GE.1.0)) IERRPM=1
      IF ((CD.LE.0.0).OR.(CD.GE.1.0)) IERRPM=1
      IF (SMIX.LT.0.0) IERRPM=1
      IF ((IVOPT.EQ.0).AND.(CS.LE.0.0)) IERRPM=1
      IF ((FRSX.LE.0.0).OR.(FRSX.GT.1.0)) IERRPM=1
      IF ((PIMPV.LT.0.0).OR.(PIMPV.GE.1.0)) IERRPM=1
      IF ((APIK.LE.0.0).OR.(APIK.GE.1.0)) IERRPM=1
      IF (IVOPT.EQ.1) GO TO 251
      IF ((PEX.LT.0.0).OR.(PEN.LT.0.0)) IERRPM=1
      GO TO 252
  251 IF ((PEX.LE.0.0).OR.(PEN.LE.0.0)) IERRPM=1
  252 IF ((EFC.LT.0.0).OR.(EFC.GT.1.0)) IERRPM=1
      IF ((RIVA.LT.0.0).OR.(RIVA.GT.1.0)) IERRPM=1
      IF (RVAI.LT.0.0) IERRPM=1
      IF (APIX.LE.0.0) IERRPM=1
      IF ((LSC.EQ.0).AND.(LWE.EQ.0)) GO TO 253
      IF ((APIKS.LT.APIK).OR.(APIKS.GT.1.0)) IERRPM=1
C
C******************************************************
C   CARD NUMBER 7
C******************************************************
C
  253 IF (IVOPT.NE.0) GO TO 260
      READ(IN,907) WKW,WKD
  907 FORMAT (2F5.0)
      PO(LRSPM+NRSPM)=WKW
      PO(LRSPM+NRSPM+1)=WKD
      IF ((WKW.LE.0.0).OR.(WKW.GT.52.143)) IERRPM=1
      IF ((WKD.LE.0.0).OR.(WKD.GT.52.143)) IERRPM=1
      IF (WKD.EQ.WKW) IERRPM=1
      GO TO 270
C
  260 IF (IVOPT.NE.1) GO TO 265
      READ(IN,9071) AEIX,AEIN,AEIK
 9071 FORMAT(3F5.2)
      PO(LRSPM+NRSPM)=AEIK
      PO(LRSPM+NRSPM+1)=AEIX
      PO(LRSPM+NRSPM+2)=AEIN
      IF (AEIN.LT.0.0) IERRPM=1
      IF (AEIX.LT.AEIN) IERRPM=1
      IF ((AEIK.LE.0.0).OR.(AEIK.GE.1.0)) IERRPM=1
      GO TO 270
C
  265 READ(IN,9072) ATIX,ATIN,ATIR
 9072 FORMAT(2F5.0,F5.2)
      PO(LRSPM+NRSPM)=ATIR
      PO(LRSPM+NRSPM+1)=ATIX
      PO(LRSPM+NRSPM+2)=ATIN
      IF (ATIX.LT.ATIN) IERRPM=1
      IF ((ATIR.LE.0.0).OR.(ATIR.GE.1.0)) IERRPM=1
C
C********************************************
C   CARD NUMBER 8
C*********************************************
C
  270 READ(IN,908) BFPK,BFIK,BFIM,AICR,CG
  908 FORMAT(F5.3,4F5.2)
      PO(LRGPM)=BFPK
      PO(LRGPM+1)=BFIK
      PO(LRGPM+2)=BFIM
      PO(LRGPM+3)=AICR
      PO(LRGPM+4)=CG
      IF ((BFPK.LE.0.0).OR.(BFPK.GE.1.0)) IERRPM=1
      IF ((BFIK.LE.0.0).OR.(BFIK.GE.1.0)) IERRPM=1
      IF (BFIM.LT.0.0) IERRPM=1
      IF (AICR.LT.0.0) IERRPM=1
      IF ((CG.LE.0.0).OR.(CG.GE.1.0)) IERRPM=1
C
C*************************************************
C    CARD NUMBER 9
C*************************************************
C
      IF (IFRZE.EQ.0) GO TO 280
      READ(IN,909) CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA
  909 FORMAT(3F5.2,F5.1,F5.3,F5.2,F5.4,F5.2)
      PO(LFRZE+8)=FICR
      PO(LFRZE+9)=CF
      PO(LFRZE+10)=CP
      PO(LFRZE+11)=CSOIL
      PO(LFRZE+12)=CSNOW
      PO(LFRZE+13)=GHC
      PO(LFRZE+14)=CT
      PO(LFRZE+15)=EFA
      IF (CSOIL.LT.0.0) IERRPM=1
      IF ((CSNOW.LE.0.0).OR.(CSNOW.GE.1.0)) IERRPM=1
      IF (GHC.LT.0.0) IERRPM=1
      IF (FICR.GT.32.0) IERRPM=1
      IF (CF.LT.0.0) IERRPM=1
      IF (CP.LT.0.0) IERRPM=1
      IF (CT.LT.0.0) IERRPM=1
      IF ((EFA.LT.0.0).OR.(EFA.GT.1.0)) IERRPM=1
C
C****************************************************
C    CARD NUMBER 10 CARRYOVER
C******************************************************
C
  280 READ(IN,'(A)') CARD
      READ(CARD,910,ERR=286) API,SMI,BFSC,BFI,AETI,FI,FEI
  910 FORMAT(4F5.2,2F5.1,F5.2)
      GO TO 287
286   CALL FRDERR (IPR,' ',CARD)
287   CO(1)=API
      CO(2)=SMI
      CO(3)=BFSC
      CO(4)=BFI
      CO(5)=0.0
      IF (IVOPT.GT.0) CO(5)=AETI
      CO(6)=32.0
      CO(7)=0.0
      IF (IFRZE.EQ.0) GO TO 281
      CO(6)=FI
      CO(7)=FEI
  281 IF ((API.LT.0.0).OR.(API.GT.APIX)) IERRCO=1
      IF ((SMI.LT.0.0).OR.(SMI.GT.SMIX)) IERRCO=1
      IF (BFSC.LT.0.0) IERRCO=1
      IF (BFI.LT.0.0) IERRCO=1
      IF (IVOPT.NE.1) GO TO 282
      IF ((AETI.LT.AEIN).OR.(AETI.GT.AEIX)) IERRCO=1
  282 IF (IVOPT.NE.2) GO TO 283
      IF ((AETI.LT.ATIN).OR.(AETI.GT.ATIX)) IERRCO=1
  283 IF (IFRZE.EQ.0) GO TO 285
      IF (FI.GT.32.0) IERRCO=1
      IF ((FEI.LT.0.0).OR.(FEI.GT.1.0)) IERRCO=1
C
C************************************
C   CARD NUMBER 11
C***************************************
C
  285 IF (LPROT.LE.1) GO TO 295
      READ(IN,911) (IM(I),IY(I),LM(I),LY(I),I=1,3)
  911 FORMAT(6(3X,I2,1X,I4))
      PO(LPROT)=1.01
      DO 290 I=1,3
      L=LPROT+(I-1)*2+1
      TM=IY(I)*12+IM(I)
      PO(L)=TM+0.01
      TM=LY(I)*12+LM(I)
  290 PO(L+1)=TM+0.01
C
C***************************************
C    CLEAR SUMS AREA
C***************************************
C
  295 IF (LSUMS.EQ.0) GO TO 300
      L=LSUMS+4
      DO 296 I=LSUMS,L
      PO(I)=0.0
  296 CONTINUE
C
C******************************************************
C    PO AND CO ARRAYS NOW FILLED, SET IUSEP AND IUSEC.
C******************************************************
C
  300 IUSEP=LPO
      IUSEC=LCO
C
C*********************************************************
C    CHECK IF ANY PARAMETER OR CARRYOVER VALUES ARE IN ERROR.
C************************************************************
C
      IF (IERRPM.EQ.0) GO TO 310
      WRITE(IPR,922)
  922 FORMAT(1H0,10X,'**ERROR** ONE OR MORE OF THE PARAMETER VALUES FOR
     1THE API-CONT OPERATION EXCEED THEIR ALLOWABLE LIMITS.')
      CALL ERROR
  310 IF (IERRCO.EQ.0) GO TO 980
      WRITE(IPR,923)
  923 FORMAT(1H0,10X,'**ERROR** ONE OR MORE OF THE CARRYOVER VALUES FOR
     1THE API-CONT OPERATION EXCEED THEIR ALLOWABLE LIMITS.')
      CALL ERROR
C
C**************************
C   CHECK FOR DEBUG OUTPUT
C***************************
C
  980 IF (IBUG.EQ.0) GO TO 990
      WRITE(IODBUG,950) LPO,LCO
  950 FORMAT(1H0,'CONTENTS OF THE PO AND CO ARRAYS FOR API-CONT',
     -5X,'NUMBER OF VALUES--PO=',I3,2X,'CO=',I3)
      WRITE(IODBUG,951) (PO(I),I=1,LPO)
  951 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,952) (PO(I),I=1,LPO)
  952 FORMAT(1H0,15(4X,A4))
      WRITE(IODBUG,951) (CO(I),I=1,LCO)
      IF (LPROT.LE.1) GO TO 990
      L=LPROT
      WRITE(IODBUG,954) (PO(L+I),I=1,6)
  954 FORMAT(1H0,'DETAILED OUTPUT PERIODS--',6F8.0)
C
  990 IF (ITRACE.LT.1) GO TO 999
      WRITE(IODBUG,953) SNAME
  953 FORMAT(1H0,'**EXIT',1X,2A4)
C
  999 RETURN
      END
