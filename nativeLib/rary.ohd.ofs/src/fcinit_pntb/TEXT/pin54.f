C MEMBER PIN54
C
      SUBROUTINE PIN54(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
C.......................................
C     THIS IS THE INPUT SUBROUTINE FOR THE SIMPLE WATER
C       BALANCE (SWB) MODEL OPERATION.  THE MODEL IS DEVELOPED
C       BY JOHN SCHAAKE AND VICTOR KOREN ET AL. AND IS DOCUMENTED
C       IN A JGR PAPER BY SCHAAKE ET AL (1995,JGR).
C     THIS SUBROUTINE INPUTS ALL CARDS FOR THE OPERATION AND
C       FILLS THE PO ARRAY AND PLACES INITIAL VALUES OF THE
C       STATE VARIABLES INTO THE CO ARRAY.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C       QINGYUN DUAN - OH CLIMATE PROJECT  SEPTEMBER 1995  VERSION 1
C.......................................
C
      DIMENSION PO(1),CO(1)
      DIMENSION SNAME(2),DESCRP(5),PID(2),RID(2),PEID(2),RSID(2),
     +          RGID(2),SMID(2),TAID(2),PPID(2),WEID(2),FEID(2),SNID(2)
      DIMENSION IM(3),IY(3),LM(3),LY(3),NDAYS(12)
      REAL PXADJ,PEADJ,DMAX,KG,ALPSM,ALPRT,KDT
      REAL KIMP,DSOIL,POROS,WWP,CVICE
      REAL SU,SB,FDP(2),TDP(2),SDP,SDN,WICE(2)
C
C*********************************************************
C   COMMON BLOCKS
C*********************************************************
C
C                                           *--> FROM COMMON.IONUM
      COMMON/IONUM/IN,IPR,IPU
C
C                                           *--> FROM COMMON.FPROG
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin54.f,v $
     . $',                                                             '
     .$Id: pin54.f,v 1.2 1998/04/07 11:41:21 page Exp $
     . $' /
C    ===================================================================
C
C
C*********************************************************
C     DATA STATEMENTS
C*********************************************************
C
      DATA SNAME/4HPIN5,4H4   /
      DATA DL,DLES,DTEMP/4HL   ,4HDLES,4HTEMP/
      DATA CFRZE,CPROT,BLANK/4HFRZE,4HPROT,4H    /
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
C*********************************************************
C     CHECK IF DEBUG OR TRACE ON -- TRACE LEVEL 1
C*********************************************************
C
      CALL FPRBUG(SNAME,1,54,IBUG)
C
C*********************************************************
C     INITIALIZE VARIABLES
C**********************************************************
C
      IVER=1
      IUSEP=0
      IUSEC=0
      NOFILL=0
      LPM=32
      NPM=8
      LET=LPM+NPM
      LPO=LET+27
      NCO=2
      LCO=2
      NXCO=0
      LRS=0
      LRG=0
      LSM=0
      LFRZE=0
      LTA=0
      LPP=0
      LWE=0
      LFE=0
      LSN=0
      LPROT=0
      LSUMS=0
C****************************************************************
C      CARD NUMBER 1
C*********************************************************
C
      READ(IN,900) DESCRP,ITP,PID,PTYPE,RID,RTYPE
  900 FORMAT(5A4,3X,I2,7X,2A4,1X,A4,7X,2A4,1X,A4)
C
C**********************************************************
C    CHECK THAT TIME SERIES EXISTS
C***********************************************************
C
      CALL CHEKTS(PID,PTYPE,ITP,1,DL,0,1,IERR)
      CALL CHEKTS(RID,RTYPE,ITP,1,DL,0,1,IERR)
C
C************************************************
C     CARD NUMBER 2
C************************************************
C
      READ(IN,905) RS,RG,SM,FRZE,TA,PP,WE,FE,SN,PROT,ISUMS
  905 FORMAT(10(1X,A4),4X,I1)
C
      IF (RS.EQ.BLANK) GO TO 120
      LRS=LPO+1
      LPO=LPO+3
C
  120 IF (RG.EQ.BLANK) GO TO 125
      LRG=LPO+1
      LPO=LPO+3
C
  125 IF (SM.EQ.BLANK) GO TO 130
      LSM=LPO+1
      LPO=LPO+4
C
C**********************************************
C     ALLOCATE SPACE FOR TIME SERIES HEADERS
C     FOR FROZEN GROUND MODEL
C**********************************************
C
  130 IF (FRZE.NE.CFRZE) GO TO 155
      LFRZE=LPO+1
      LPO=LPO+5
      NXCO=NXCO+8
      LCO=LCO+NXCO
C
      IF (TA.EQ.BLANK) GO TO 135
      LTA=LPO+1
      LPO=LPO+4
C
  135 IF (PP.EQ.BLANK) GO TO 140
      LPP=LPO+1
      LPO=LPO+4
C
  140 IF (WE.EQ.BLANK) GO TO 145
      LWE=LPO+1
      LPO=LPO+4
C
  145 IF (FE.EQ.BLANK) GO TO 150
      LFE=LPO+1
      LPO=LPO+4
C
  150 IF (SN.EQ.BLANK) GO TO 155
      LSN=LPO+1
      LPO=LPO+4
C
  155 IF (PROT.EQ.CPROT) GO TO 160
      LPROT=1
      GO TO 165
  160 LPROT=LPO+1
      LPO=LPO+7
C
  165 IF (ISUMS.NE.1) GO TO 170
      LSUMS=LPO+1
      LPO=LPO+7
C
C*********************************************************
C  SIZE OF THE PO ARRAY KNOWN -- CHECK IF SPACE AVAILABLE
C*********************************************************
C
  170 CALL CHECKP(LPO,LEFTP,IERR)
      IF (IERR.EQ.1) NOFILL=1
C
C*****************************
C  CHECK SPACE FOR CO ARRAY
C*****************************
C
      CALL CHECKC(LCO,LEFTC,IERR)
      IF (IERR.EQ.1) NOFILL=1
      IF (NOFILL.EQ.1) GO TO 1000
C
C*********************************************************
C     CHECK IF ALL ESSENTIAL TIME SERIES USED IN FROZEN
C     GROUND MODEL ARE DEFINED
C*********************************************************
C
      IF (LFRZE.EQ.0) GO TO 171
      IF ((LTA.EQ.0).OR.(LPP.EQ.0).OR.(LWE.EQ.0)) WRITE (IPR,906)
  906 FORMAT(1H0,10X,'**ERROR**  ONE OR MORE TIME TIME SERIES USED IN ',
     +       'FROZEN GROUND ARE NOT DEFINED !')
      CALL ERROR
C
C****************************************
C LOAD INITIAL VALUES INTO THE PO ARRAY
C****************************************
C
  171 PO(1)=IVER+0.01
      DO 175 I=1,5
  175 PO(I+1)=DESCRP(I)
      PO(7)=ITP+0.01
      PO(8)=PID(1)
      PO(9)=PID(2)
      PO(10)=PTYPE
      PO(11)=RID(1)
      PO(12)=RID(2)
      PO(13)=RTYPE
      PO(14)=LPM+0.01
      PO(15)=NPM+0.01
      PO(16)=LET+0.01
      PO(17)=NCO+0.01
      PO(18)=NXCO+0.01
      PO(19)=LRS+0.01
      PO(20)=LRG+0.01
      PO(21)=LSM+0.01
      PO(22)=LFRZE+0.01
      PO(23)=LTA+0.01
      PO(24)=LPP+0.01
      PO(25)=LWE+0.01
      PO(26)=LFE+0.01
      PO(27)=LSN+0.01
      PO(28)=LPROT+0.01
      PO(29)=LPO+0.01
      PO(30)=LCO+0.01
      PO(31)=LSUMS+0.01
C
C*********************************************************
C   CARD NUMBER 3
C*********************************************************
C
      IPE1=LET+3
      IPE2=IPE1+11
      READ(IN,910) PEID,PETYPE,(PO(I),I=IPE1,IPE2)
  910 FORMAT(2X,2A4,1X,A4,5X,12F5.2)
      PO(LET)=PEID(1)
      PO(LET+1)=PEID(2)
      PO(LET+2)=PETYPE
      IF ((PEID(1).EQ.BLANK).AND.(PEID(2).EQ.BLANK)) GO TO 176
      CALL CHEKTS(PEID,PETYPE,24,1,DL,0,1,IERR)
C
C     COMPUTE DAILY INCREMENT IN DEMAND CURVE OR
C        PE-ADJUSTMENT CURVE.
C
  176 DO 180 I=2,13
      ND=NDAYS(I-1)
      M=IPE1+I-1
      K=M-1
      IF (I.EQ.13) M=IPE1
      L=K+12
      PO(L)=(PO(M)-PO(K))/ND
  180 CONTINUE
C
C*******************************************
C    CARD NUMBER 4
C*******************************************
C
      READ (IN,915) PXADJ,PEADJ,DMAX,KG,ALPSM,ALPRT,KDT,IOPTET
  915 FORMAT (2F5.2,F5.0,F5.2,2F5.3,F5.2,4X,I1)
      IF (IOPTET.GT.0) IOPTET=1
C
C     PARAMETER CHECKS -- TO ELIMINATE IMPOSSIBLE VALUES.
C
      ICHGE=0
      IF (DMAX.GE.1.0E-4) GO TO 181
      DMAX=1.0
      ICHGE=ICHGE+1
  181 IF (KG.GE.1.0E-4) GO TO 182
      KG = 1.0E-4
      ICHGE=ICHGE+1
  182 IF (ALPSM.GE.0.0) GO TO 183
      ALPSM=1.0E-4
      ICHGE=ICHGE+1
  183 IF (ALPRT.GE.1.0E-4) GO TO 184
      ALPRT=1.0E-4
      ICHGE=ICHGE+1
  184 IF (KDT.GE.0.0) GO TO 185
      KDT=0.0
      ICHGE=ICHGE+1
      IF (ICHGE.GE.1) WRITE (IPR,920)
  920 FORMAT (1H0,10X,11H**WARNING**)
      WRITE (IPR,925) ICHGE
  925 FORMAT (1H ,20X,I1,' UNREASONABLE PARAMETER VALUES DETECTED AND ',
     +        'THEIR VALUES HAVE BEEN CHANGED')
C
  185 PO(LPM)=PXADJ
      PO(LPM+1)=PEADJ
      PO(LPM+2)=DMAX
      PO(LPM+3)=KG
      PO(LPM+4)=ALPSM
      PO(LPM+5)=ALPRT
      PO(LPM+6)=KDT
      PO(LPM+7)=IOPTET+0.01
      DMRT=DMAX*ALPRT
C
C*****************************************
C    CARD NUMBER 5 - OPTIONAL
C*****************************************
C
      IF ((LRS.EQ.0).AND.(LRG.EQ.0).AND.(LSM.EQ.0)) GO TO 210
      READ(IN,930) RSID,RGID,SMID,ITSM
  930 FORMAT(3(2X,2A4),3X,I2)
C
      IF (LRS.EQ.0) GO TO 200
      PO(LRS)=RSID(1)
      PO(LRS+1)=RSID(2)
      PO(LRS+2)=RS
      CALL CHEKTS(RSID,RS,ITP,1,DL,0,1,IERR)
C
  200 IF (LRG.EQ.0) GO TO 205
      PO(LRG)=RGID(1)
      PO(LRG+1)=RGID(2)
      PO(LRG+2)=RG
      CALL CHEKTS(RGID,RG,ITP,1,DL,0,1,IERR)
C
  205 IF (LSM.EQ.0) GO TO 210
      PO(LSM)=SMID(1)
      PO(LSM+1)=SMID(2)
      PO(LSM+2)=SM
      PO(LSM+3)=ITSM+0.01
      CALL CHEKTS(SMID,SM,ITSM,1,DL,0,5,IERR)
      IF ((ITSM/ITP)*ITP.EQ.ITSM) GO TO 210
      WRITE(IPR,935) ITSM,SM,ITP,PTYPE
  935 FORMAT(1H0,10X,'**ERROR**  THE TIME INTERVAL(',I2,1X,
     -      'HOURS) FOR TYPE',1X,A4,1X,'IS NOT A MULTIPLE',/16X,
     -      'OF THE TIME INTERVAL(',I2,1X,'HOURS) FOR TYPE',1X,A4)
      CALL ERROR
C
C*********************************************************
C   CARD NUMBER 6 - OPTIONAL
C*********************************************************
C
  210 IF (LFRZE.EQ.0) GO TO 250
      IF ((LTA.EQ.0).AND.(LPP.EQ.0).AND.(LWE.EQ.0).AND.(LFE.EQ.0).AND.
     +    (LSN.EQ.0)) GO TO 240
      READ(IN,940) TAID,ITTA,PPID,ITPP,WEID,ITWE,FEID,ITFE,SNID,ITSN
  940 FORMAT(5(2X,2A4,3X,I2))
C
      IF (LTA.EQ.0) GO TO 215
      PO(LTA)=TAID(1)
      PO(LTA+1)=TAID(2)
      PO(LTA+2)=TA
      PO(LTA+3)=ITTA+0.01
      CALL CHEKTS(TAID,TA,ITTA,1,DTEMP,0,1,IERR)
      IF ((ITTA/ITP)*ITP.EQ.ITTA) GO TO 215
      WRITE(IPR,935) ITTA,TA,ITP,PTYPE
      CALL ERROR
C
  215 IF (LPP.EQ.0) GO TO 220
      PO(LPP)=PPID(1)
      PO(LPP+1)=PPID(2)
      PO(LPP+2)=PP
      PO(LPP+3)=ITPP
      CALL CHEKTS(PPID,PP,ITPP,1,DLES,0,1,IERR)
      IF ((ITPP/ITP)*ITP.EQ.ITPP) GO TO 220
      WRITE(IPR,935) ITPP,PP,ITP,PTYPE
      CALL ERROR
C
  220 IF (LWE.EQ.0) GO TO 225
      PO(LWE)=WEID(1)
      PO(LWE+1)=WEID(2)
      PO(LWE+2)=WE
      PO(LWE+3)=ITWE
      CALL CHEKTS(WEID,WE,ITWE,1,DL,0,1,IERR)
      IF ((ITWE/ITP)*ITP.EQ.ITWE) GO TO 225
      WRITE(IPR,935) ITWE,WE,ITP,PTYPE
      CALL ERROR
C
  225 IF (LFE.EQ.0) GO TO 230
      PO(LFE)=FEID(1)
      PO(LFE+1)=FEID(2)
      PO(LFE+2)=FE
      PO(LFE+3)=ITFE
      CALL CHEKTS(FEID,FE,ITFE,1,DLES,0,1,IERR)
      IF ((ITFE/ITP)*ITP.EQ.ITFE) GO TO 230
      WRITE(IPR,935) ITFE,FE,ITP,PTYPE
      CALL ERROR
C
  230 IF (LSN.EQ.0) GO TO 240
      PO(LSN)=SNID(1)
      PO(LSN+1)=SNID(2)
      PO(LSN+2)=SN
      PO(LSN+3)=ITSN
      CALL CHEKTS(SNID,SN,ITSN,1,DL,0,1,IERR)
      IF ((ITSN/ITP)*ITP.EQ.ITSN) GO TO 240
      WRITE(IPR,935) ITSN,SN,ITP,PTYPE
      CALL ERROR
C
C*************************************************
C    CARD NUMBER 7 - OPTIONAL
C*************************************************
C
  240 READ(IN,955) KIMP,DSOIL,POROS,WWP,CVICE
  955 FORMAT(F5.1,F5.2,2F5.3,F5.1)
      PO(LFRZE)=KIMP
      PO(LFRZE+1)=DSOIL
      PO(LFRZE+2)=POROS
      PO(LFRZE+3)=WWP
      PO(LFRZE+4)=CVICE
C
C****************************************************
C    CARD NUMBER 8 CARRYOVER
C******************************************************
C
  250 READ (IN,960) SU,SB
  960 FORMAT (2F5.1)
C
C     CHECK STATE VARIABLES FOR ILLEGITIMATE VALUES.
C
      CO(1)=SU
      CO(2)=SB
      L=0
      IF (SU.LE.DMRT) GO TO 255
      SU=DMRT
      L=1
  255 IF (SB.LE.DMAX) GO TO 260
      SB=DMAX
      L=1
  260 IF (SU.GE.0.1) GO TO 265
      SU=0.1
      L=1
  265 IF (SB.GE.0.1) GO TO 270
      SB=0.1
      L=1
  270 IF (L.EQ.0) GO TO 275
      WRITE (IPR,920)
      WRITE (IPR,965)
  965 FORMAT (1H0,10X,'INITIAL STATE VARIABLES CONTAIN IMPOSSIBLE ',
     +        'VALUES.',6X,'SU',3X,'SB')
      WRITE (IPR,970) (CO(J),J=1,NCO)
  970 FORMAT (1H ,41X,'INITIAL VALUES WERE',3X,2F7.0)
      WRITE (IPR,975) SU,SB
  975 FORMAT (1H ,50X,'CHANGED TO',3X,2F7.0)
      CALL WARN
C
C****************************************************
C    CARD NUMBER 9 ADDITIONAL CARRYOVER - OPTIONAL
C******************************************************
C
  275 IF (LFRZE.EQ.0) GO TO 300
      READ (IN,976) FDP,TDP,SDP,SDN
  976 FORMAT (5F5.1,F5.2)
C
      WICE(1)=FDP(1)*10.0/DMRT*(POROS-WWP)*SU
      WICE(2)=FDP(2)*10.0/DMAX*(POROS-WWP)*SB
C
C     CHECK STATE VARIABLES FOR ILLEGITIMATE VALUES.
C
      CO(NCO+1)=FDP(1)
      CO(NCO+2)=FDP(2)
      CO(NCO+3)=TDP(1)
      CO(NCO+4)=TDP(2)
      CO(NCO+5)=SDP
      CO(NCO+6)=SDN
      CO(NCO+7)=WICE(1)
      CO(NCO+8)=WICE(2)
      L=0
      IF (FDP(1).GE.0.0) GO TO 278
      CO(1)=0.0
      L=1
  278 IF (FDP(1).LE.DMRT/(POROS-WWP)) GO TO 280
      CO(1)=DMRT/(POROS-WWP)
      L=1
  280 IF (FDP(2).GE.0.0) GO TO 282
      CO(2)=0.0
      L=1
  282 IF (FDP(2).LE.DMAX/(POROS-WWP)) GO TO 285
      CO(2)=DMAX/(POROS-WWP)
      L=1
  285 IF (TDP(1).GE.0.0) GO TO 290
      CO(3)=0.0
      L=1
  290 IF (TDP(2).GE.0.0) GO TO 295
      CO(4)=0.0
      L=1
  295 IF (SDP.GE.0.0) GO TO 300
      CO(5)=0.0
      L=1
  300 IF (SDN.GE.0.0) GO TO 305
      CO(6)=0.0
      L=1
  305 IF (L.EQ.0) GO TO 310
      WRITE (IPR,920)
      WRITE (IPR,980)
  980 FORMAT (1H0,10X,'INITIAL FROZEN GROUND STATE VARIABLES CONTAIN ',
     +        'IMPOSSIBLE VALUES.',/,7X,'FDP(1)',4X,'FDP(2)',4X,
     +        'TDP(1)',4X,'TDP(2)',7X,'SDP',7X,'SDN',3X,'WICE(1)',
     +        3X,'WICE(2)')
      WRITE (IPR,985) FDP,TDP,SDP,SDN,WICE
  985 FORMAT (8(3X,F7.0))
      WRITE (IPR,990) (CO(NCO+J),J=1,NXCO)
  990 FORMAT (1H ,50X,'WERE CHANGED TO',/,8(3X,F7.0))
      CALL WARN
C
C************************************
C   CARD NUMBER 10 - OPTIONAL
C************************************
C
  310 IF (LPROT.LE.1 .OR. MAINUM.LT.3) GO TO 320
      READ(IN,995) (IM(I),IY(I),LM(I),LY(I),I=1,3)
  995 FORMAT(6(3X,I2,1X,I4))
      PO(LPROT)=1.01
      DO 315 I=1,3
      L=LPROT+(I-1)*2+1
      TM=IY(I)*12+IM(I)
      PO(L)=TM+0.01
      TM=LY(I)*12+LM(I)
  315 PO(L+1)=TM+0.01
C
C***************************************
C    CLEAR SUMS AREA
C***************************************
C
  320 IF (LSUMS.EQ.0) GO TO 330
      L=LSUMS+6
      DO 325 I=LSUMS,L
      PO(I)=0.0
  325 CONTINUE
C
C******************************************************
C    PO AND CO ARRAYS NOW FILLED, SET IUSEP AND IUSEC.
C******************************************************
C
  330 IUSEP=LPO
      IUSEC=LCO
C
C***************************
C   CHECK FOR DEBUG OUTPUT
C***************************
C
      IF (IBUG.EQ.0) GO TO 1000
      WRITE(IODBUG,996) LPO,LCO
  996 FORMAT(1H0,'CONTENTS OF THE PO AND CO ARRAYS FOR SWB-NILE',
     -5X,'NUMBER OF VALUES--PO=',I3,2X,'CO=',I3)
      WRITE(IODBUG,997) (PO(I),I=1,LPO)
  997 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,998) (PO(I),I=1,LPO)
  998 FORMAT(1H0,15(4X,A4))
      WRITE(IODBUG,997) (CO(I),I=1,LCO)
      IF (LPROT.LE.1) GO TO 1000
      L=LPROT
      WRITE(IODBUG,999) (PO(L+I),I=1,6)
  999 FORMAT(1H0,'DETAILED OUTPUT PERIODS--',6F8.0)
C
 1000 IF (ITRACE.LT.1) GO TO 9999
      WRITE(IODBUG,1050) SNAME
 1050 FORMAT(1H0,'**EXIT',1X,2A4)
C
 9999 RETURN
      END
