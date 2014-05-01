C MEMBER RSNO32
C  (from old member FCEX32)
C VERSION 1.10
C***********************************************************************
C @PROCESS LVL(77)
CEA      SUBROUTINE RSNO32(PS,CS,PGM,IDT,TWE,AESC)
      SUBROUTINE RSNO32(PS,CS,PGM,IDT,TWE,AESC,IVS17)

C  THIS ROUTINE RETRIEVES PARAMETERS AND CARRYOVER FOR SNOW-17 MODEL
C......................................................................
CEA CHANGES MADE TO INCORPORATE ADDITIONAL SNOW-17 CARRYOVER AND TO
CEA  REMOVE SOME UNNEEDED VARIABLES.  Eric Anderson - Sept. 2006
C......................................................................
      REAL MFMAX,MFMIN,NMF,LIQW,NEGHS,MBASE,MFC
C......................................................................
      DIMENSION PS(*),CS(*),SNAME(2)
C......................................................................
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fsnw'
      COMMON/SNPM19/ALAT,SCF,MFMAX,MFMIN,NMF,UADJ,SI,MBASE,PXTEMP,
     1  PLWHC,TIPM,PA,ADC(11),LMFV,SMFV(12),LAEC,NPTAE,AE(2,14)
CEA EXPAND SNOW-17 COMMON BLOCKS FOR ADDITIONAL CARRYOVER AND VARIABLES
CEA      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
CEA     1  STORGE,AEADJ,NEXLAG,EXLAG(7)
CEA      COMMON/SUMS19/SPX,SSFALL,SRM,SMELT,SMELTR,SROBG,DSFALL,DRAIN,DQNET
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
     1  STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
      COMMON/SUMS19/SPX,SSFALL,SRM,SMELT,SMELTR,SROBG,DSFALL,DRAIN,DQNET
     1 ,DRSL,NDRSP
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
CEA      COMMON /SNOIN/  WE1,NEGHS1,LIQW1,TINDX1,ACCMX1,SB1,SBAES1,SBWS1,
CEA     1                STORG1,AEADJ1,NEXLA1,EXLAG1(7)
      COMMON /SNOIN/  WE1,NEGHS1,LIQW1,TINDX1,ACCMX1,SB1,SBAES1,SBWS1,
     1        STORG1,AEADJ1,NEXLA1,EXLAG1(7),SNDPT1,SNTMP1,TAPRV1
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rsno32.f,v $
     . $',                                                             '
     .$Id: rsno32.f,v 1.4 2006/10/03 19:38:54 hsu Exp $
     . $' /
C    ===================================================================
C
C......................................................................
      DATA SNAME/4HRSNO,4H32  /
C......................................................................
C     TRACE LEVEL=1,DEBUG FLAG=IBUG.
      CALL FPRBUG(SNAME,1,32,IBUG)
C......................................................................
C     CONSTANT
C     IF SNOWFALL IS LESS THAN HSNOF/HR -- DO NOT
C        LEAVE CURRENT DEPLETION CURVE.
      HSNOF=0.2
C......................................................................
C     VALUES OF CONTROL VARIABLES.
C        THOSE NEEDED IN ALL CASES FIRST.
CEA ADD SNOW-17 VERSION NUMBER
      IVS17=PS(1)
      FIDT=IDT
      SNOF=HSNOF*IDT
      IDTPS=PS(10)
      NEXLAG=5/IDTPS+2
CEA LRM, LSWE, AND LCOVER NOT NEEDED FOR FFG - NO TIME SERIES STORED.
CEA      LRM=PS(17)
CEA      LSWE=PS(20)
CEA      IF (LSWE.EQ.0) GO TO 103
CEA      ITSWE=PS(LSWE+3)
CEA      NSWE=24/ITSWE
CEA  103 LCOVER=PS(22)
CEA      IF (LCOVER.EQ.0) GO TO 104
CEA      ITSSC=PS(LCOVER+3)
CEA      NSSC=24/ITSSC
CEA  104 LSUMS=PS(23)
      LSUMS=PS(23)
      LPM=PS(25)
CEA IPRINT NOT USED BY PACK19
CEA      IPRINT=PS(24)
CEA      IF(IBUG.EQ.1) IPRINT=1
      NPS=PS(16)
C
C     THOSE ONLY NEEDED IF NOSNOW=0
      LADC=PS(26)
      LUPPM=PS(28)
      LMFV=PS(29)
      LAEC=PS(30)
C.......................................................................
C     DEBUG OUTPUT - PRINT PS() AND CS()
      IF(IBUG.EQ.0) GO TO 100
      WRITE(IODBUG,900) NOSNOW
  900 FORMAT(1H0,36H DEBUG--CONTENTS OF PS AND CS ARRAYS,5X,7HNOSNOW=,
     1 I1)
      WRITE(IODBUG,901) (PS(I),I=1,NPS)
  901 FORMAT(1H0,15F8.3)
CC      WRITE(IODBUG,902) (PS(I),I=1,NPS)
CC  902 FORMAT(1H0,15(4X,A4))
CEA LENGTH OF CARRYOVER NOW BASED ON SNOW-17 VERSION NUMBER
CEA      NCO=10+NEXLAG
      IF (IVS17.EQ.1) NCO=10+NEXLAG
      IF ((IVS17.GT.1).AND.(IVS17.LT.4)) NCO=12+NEXLAG
      IF (IVS17.GT.3) NCO=13+NEXLAG
      WRITE(IODBUG,901) (CS(I),I=1,NCO)
C.......................................................................
C     RAIN-SNOW ELEVATION PARAMETERS
  100 IF (LAEC.EQ.0) GO TO 110
      NPTAE=PS(LAEC)
      DO 105 I=1,NPTAE
      J=LAEC+5+(I-1)*2
      AE(1,I)=PS(J)
      AE(2,I)=PS(J+1)
  105 CONTINUE
C.......................................................................
C     PARAMETER VALUES
  110 ELEV=PS(LPM+1)*0.01
      PA=29.9-0.335*ELEV+0.00022*(ELEV**2.4)
      PA=33.86*PA
      SCF=PS(LPM+2)
      MFMAX=PS(LPM+3)
      MFMAX=(MFMAX*FIDT)/6.0
      MFMIN=PS(LPM+4)
      MFMIN=(MFMIN*FIDT)/6.0
      NMF=PS(LPM+7)
      NMF=(NMF*FIDT)/6.0
      UADJ=PS(LPM+5)
      UADJ=(UADJ*IDT)/6.0
      SI=PS(LPM+6)
      GM=PS(LPM+12)
      PGM=(GM*FIDT)/24.0
      MBASE=PS(LPM+9)
      PXTEMP=PS(LPM+10)
      PLWHC=PS(LPM+11)
      TIPM=PS(LPM+8)
      TIPM=1.0-((1.0-TIPM)**(FIDT/6.0))
      ALAT=PS(LPM+13)
C
C     AREAL DEPLETION CURVE
      ADC(1)=0.05
      DO 111 I=2,10
      J=LADC+I-2
  111 ADC(I)=PS(J)
      ADC(11)=1.0
      IF(LMFV.EQ.0) GOTO 30
      DO 31 I=1,12
       J=LMFV+I-1
   31  SMFV(I)=PS(J)
C.......................................................................
C     UPDATING PARAMETERS
C
C     OPERATIONAL PROGRAMS - ALL PARAMETERS DEFINED.
   30 WETOL=PS(LUPPM)
      SCTOL=PS(LUPPM+1)
      MFC=PS(LUPPM+4)
      SFALLX=PS(LUPPM+5)
      WINDC=PS(LUPPM+6)
CEA UADJC IN SNUP19 COMMON PREVIOUSLY UNDEFINED
      UADJC=1.0
C.......................................................................
      DO 40 I=1,7
      EXLAG1(I)=0.
   40 CONTINUE
C.......................................................................
C     CARRYOVER VALUES.
  115 WE=CS(1)
      NEGHS=CS(2)
      LIQW=CS(3)
      TINDEX=CS(4)
      ACCMAX=CS(5)
      SB=CS(6)
      SBAESC=CS(7)
      SBWS=CS(8)
      STORGE=CS(9)
      AEADJ=CS(10)
      TEX=0.0
      DO 116 I=1,NEXLAG
      EXLAG1(I)=CS(10+I)
  116 TEX=TEX+EXLAG1(I)
CEA DETERMINE DEPTH AND SNOWPACK TEMPERATURE CARRYOVER BASED ON VERSION
      IF (IVS17.EQ.1) THEN
        SNDPT=-999.0
        SNTMP=-999.0
      ELSE
        SNDPT=CS(11+NEXLAG)
        SNTMP=CS(12+NEXLAG)
      ENDIF
      TAPREV=-99.
      IF (IVS17.GT.3) TAPREV=CS(13+NEXLAG)
C.......................................................................
C     INITIAL VALUES
      TWE=WE+LIQW+TEX+STORGE
CEA TWE1 NOT NEEDED FOR FFG
CEA      TWE1=TWE
      CHGWE=0.0
      DSFALL=0.0
      DRAIN=0.0
      DQNET=0.0
CEA DEFINE REMAINING VARIABLES IN SUMS19 COMMON BLOCK
      DRSL=0.0
      NDRSP=0
CEA PTWE NOT NEEDED FOR FFG
CEA      PTWE=TWE
      AESC=0.
CEA PCOVER NOT NEEDED FOR FFG
CEA      IF(TWE.EQ.0.0) GO TO 117
      IF(TWE.EQ.0.0) GO TO 118
      CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
CEA  117 PCOVER=AESC

  118 SPX=0.0
      SSFALL=0.0
      SRM=0.0
      SMELT=0.0
      SMELTR=0.0
      SROBG=0.0
CEA ITITLE, IC, AND IRS NOT NEEDED FOR FFG
CEA      ITITLE=0
CEA      IC=1
CEA      IRS=1
C.......................................................................
CEA NOTE: THE FOLLOWING CODE IS NEEDED BECAUSE THE COMPUTATIONAL TIME
CEA  INTERVAL USED BY SNOW-17 MAY NOT BE THE SAME AS THE DURATION
CEA  FOR WHICH FFG IS CURRENTLY BEING COMPUTED - THUS VALUES IN THE
CEA  EXLAG ARRAY NEED TO BE ADJUSTED.
C     LAGGED EXCESS WATER.
C     CONTROL VARIABLES
      ITPXO=IDTPS
      ITPXN=FIDT
      NLOLD=5/ITPXO+2
      NLNEW=5/ITPXN+2
      NEXLAG=NLNEW
      IF(ITPXN.NE.ITPXO) GO TO 170
      DO 160 N=1,NLNEW
  160 EXLAG(N)=EXLAG1(N)
      GO TO 200
  170 NHR=NLOLD*ITPXO
      FN=ITPXO
      FHR=1.0/FN
      DO 180 N=1,NLNEW
  180 EXLAG(N)=0.0
      DO 190 N=1,NHR
      NO=(N-1)/ITPXO+1
      NN=(N-1)/ITPXN+1
      IF(NN.GT.NLNEW) GO TO 200
      EXLAG(NN)=EXLAG(NN)+FHR*EXLAG1(NO)
  190 CONTINUE
C.......................................................................
C  INITIAL CONDITIONS FOR SUCCESSIVE ITERATIONS
  200 WE1    = WE
      NEGHS1 = NEGHS
      LIQW1  = LIQW
      TINDX1 = TINDEX
      ACCMX1 = ACCMAX
      SB1    = SB
      SBAES1 = SBAESC
      SBWS1  = SBWS
      STORG1 = STORGE
      AEADJ1 = AEADJ
      NEXLG1 = NEXLAG
      DO 210 I=1,7
  210 EXLAG1(I) = EXLAG(I)
CEA RETAIN INITIAL VALUES FOR DEPTH AND SNOWPACK TEMPERATURE VARIABLES
      SNDPT1=SNDPT
      SNTMP1=SNTMP
      TAPRV1=TAPREV
      RETURN
      END