C MEMBER SNO32
C  (from old member FCEX32)
C VERSION 1.11
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C.......................................................................
C @PROCESS LVL(77)
CEA ADD NEW VARIABLES TO ARGUMENT LIST
CEA      SUBROUTINE SNO32(IDT,PX,TA,PRSL,TWE,COVER,IFUT,NDT,PGM,IMN,IDN,RM,
CEA     . KDA,KHR)
      SUBROUTINE SNO32(IDT,PX,TA,PRSL,TWE,COVER,IFUT,NDT,PGM,IMN,IDN,
     . IYR,RM,KDA,KHR,IVS17)
C.......................................................................
C  This routine computes snowmelt using the SNOW-17 model
c
c.......................................................................
c  Added arguments (IYR,IOUTYP,OPNAME) to PACK19 call and added
c  common blocks OUTCTL and WHERE for ICP.
C        Tim Sweeney, HRL                              Sept 1995
CEA
CEA MODIFIED TO INCLUDE SNOW DEPTH AND SNOWPACK TEMPERATURE CARRYOVER
CEA  AND UPDATE ARGUMENT LIST FOR CALLING SUBROUTINE PACK19.
CEA      Eric Anderson     Sept 2006
c.......................................................................
c
      REAL MFMAX,MFMIN,NMF,LIQW,NEGHS,MBASE,MFC
C
      DIMENSION SNAME(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      COMMON /FSNWUP/ IUPWE,IUPSC
      COMMON/SNPM19/ALAT,SCF,MFMAX,MFMIN,NMF,UADJ,SI,MBASE,PXTEMP,
     1        PLWHC,TIPM,PA,ADC(11),LMFV,SMFV(12),LAEC,NPTAE,AE(2,14)
CEA EXPAND COMMON BLOCKS TO ADD NEW VARIABLES
CEA      COMMON /SNCO19/ WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
CEA     1                STORGE,AEADJ,NEXLAG,EXLAG(7)
CEA      COMMON/SUMS19/SPX,SSFALL,SRM,SMELT,SMELTR,SROBG,DSFALL,DRAIN,DQNET
      COMMON /SNCO19/ WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
     1                STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
      COMMON /SUMS19/ SPX,SSFALL,SRM,SMELT,SMELTR,SROBG,DSFALL,DRAIN,
     1                DQNET,DRSL,NDRSP
      COMMON /SNUP19/ MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC  
CEA      COMMON /SNOIN/  WE1,NEGHS1,LIQW1,TINDX1,ACCMX1,SB1,SBAES1,SBWS1,
CEA     1                STORG1,AEADJ1,NEXLA1,EXLAG1(7)
      COMMON /SNOIN/  WE1,NEGHS1,LIQW1,TINDX1,ACCMX1,SB1,SBAES1,SBWS1,
     1         STORG1,AEADJ1,NEXLA1,EXLAG1(7),SNDPT1,SNTMP1,TAPRV1
      INCLUDE 'common/where'
      COMMON /OUTCTL/ IOUTYP
C
      DIMENSION PX(*),TA(*),RM(*),PCTS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/sno32.f,v $
     . $',                                                             '
     .$Id: sno32.f,v 1.4 2006/10/03 19:33:14 hsu Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/4HSNO3,4H2   /
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C  TRACE LEVEL=1, DEBUG FLAG=IBUG
C
      VERS=1.11
C
      CALL FPRBUG (SNAME,1,32,IBUG)
      IF(IBUG.GT.0) WRITE(IODBUG,7001) SNAME,VERS
 7001 FORMAT(/5X,2A4,' DEBUG OUTPUT.',5X,'VERSION:',F4.2)
C.......................................................................

C  SET INITIAL VALUES
CEA NOTE: BY SETTING PCTS(1) TO 0.0 THIS RESULTS IN ALL THE PRECIPITATION
CEA  INPUT TO PACK19 TO BE IN THE FORM OF RAIN.  I BELIEVE THIS IS CORRECT
CEA  FOR FFG COMPUTATIONS.  RAIN-SNOW ELEVATION VALUE COMPUTED IN EX32
CEA  WILL NEVER BE USED AS IF PERCENT SNOWFALL (PCTS) IS DEFINED, IT
CEA  OVERRIDES OTHER LOGIC FOR DETERMINING THE FORM OF PRECIPITATION.
      PCTS(1) = 0.0
      OWE = -999.0
      OSC = -999.0
CEA SET OBSERVED SNOW DEPTH TO MISSING
      ODPT = -999.0
CC      PRSL = -999.0
C
CEA ADD OBSERVED DEPTH AND VERSION NUMBER TO PACK19 CALL
CEA      CALL PACK19(KDA,KHR,NDT,TA,PX,PCTS,PRSL,OWE,OSC,PGM,RM,TWE,
CEA     1            COVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN,IYR,
CEA     2            IOUTYP,OPNAME)
      CALL PACK19(KDA,KHR,NDT,TA,PX,PCTS,PRSL,OWE,OSC,ODPT,PGM,RM,TWE,
     1            COVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN,IYR,
     2            IOUTYP,OPNAME,IVS17)
C
C  RESET SNOW17 FOR NEXT ITERATION
      WE     = WE1
      NEGHS  = NEGHS1
      LIQW   = LIQW1
      TINDEX = TINDX1
      ACCMAX = ACCMX1
      SB     = SB1
      SBAESC = SBAES1
      STORGE = STORG1
      AEADJ  = AEADJ1
CC      TEX    = TEX1
      DO 1060 I=1,7
 1060 EXLAG(I) = EXLAG1(I)
CEA RESET DEPTH AND SNOWPACK TEMPERATURE CARRYOVER
      SNDPT=SNDPT1
      SNTMP=SNTMP1
      TAPREV=TAPRV1
C
      SPX    = 0.0
      SSFALL = 0.0
      SRM    = 0.0
      SMELT  = 0.0
      SMELTR = 0.0
      SROBG  = 0.0
      DSFALL = 0.0
      DRAIN  = 0.0
CEA CORRECT VARIABLE NAME AND ADD ADDITIONAL VARIABLES
CEA      DQMET  = 0.0
      DQNET  = 0.0
      DRSL   = 0.0
      NDRSP  = 0
C
      RETURN
      END
