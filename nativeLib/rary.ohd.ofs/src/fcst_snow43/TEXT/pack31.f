C     MODULE PACK31
C
      SUBROUTINE PACK31(KDA,KHR,NDT,TA,PX,PCTS,RSL,OWE,owev,gain,OSC,  
     1PGM,RM,rlapse,twe,twev,COVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE EXECUTES THE 'SNOW-43 ' OPERATION FOR ONE
C     COMPUTATIONAL PERIOD.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL MAY 1980 FOR SNOW-17
C     Modified by...
C        Mark Woodbury - RTi, July 1995 for the SNOW-43 operation
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      KDA           I          Current Julian day-internal clock
C      KHR           I          Current hour-internal clock
C      NDT           I          Number of precip, % snowfall
C                               & rain + melt values /
C                               computation time interval
C      TA            I          Air temperature
C      PX            I          Precipitation 
C      PCTS          I          % snowfall
C      RSL           I          Rain Snow Line (elevation)
C      OWE           I          Observed Water Equivalent
C      owev          I          Observed Water Equiv. variance
C      gain          I          Gain for the Observation
C      OSC           I          Observed Areal Extent of Snow Cover
C      PGM           I          Ground Melt
C      RM            O          Rain + Melt
C      rlapse        I          Lapse Rate for rnsnwk factor computation.
C      twe           O          Total Water Equivalent.
C      twev          O          Simulated Water-Equiv. variance
C      COVER         O          Simulated Areal Extent of Snow Cover   
C      CWE           O          Computed Water Equivalent before any
C                               updating
C      CAESC         O          Computed Areal Extent of Snow Cover 
C                               before any updating
C      IFUT          I          Flag (0/1) (observed/forecast)
C      IDT           I          Length of computational time
C                               interval-hours
C      IBUG          I          Flag (0/1) (no debug/debug)
C      IDN           I          Current day number since March 21
C      IMN           I          Current month number
C.......................................
C
C     implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      integer kda, khr, ndt
      real    ta, px, pcts, rsl, owe, 
     1        owev, gain, osc, pfm, prm, rlapse,
     2        twev, cover, cwe, caesc
      integer ifut, idt, ibug, idn, imn
C     --- L O C A L ---
      integer iwemsng, iscmsng
      integer i, j, mc, nr, n, ihour, iyear, iday,
     1        imonth, itpx
      real    tpx, pct, fracs, fracr, robg,
     1        sbci, sfall, rain, cnhspx, freew,
     2        rm, tex, mf, ratio, const, cnhs,                  
     3        qnet, pxi, tz, snew, twe, 
     4        pgm, sbc, rmin, melt, fndt, 
     5        fitpx, rfmin, sfnew, gm, excess,
     6        heat, water, packro, gmro,
     7        rainm
      real    cv, pvar
      real    sname, jbug
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
      integer iupwe, iupsc
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PX(1),PCTS(1),RM(1)
      DIMENSION SNAME(2)
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA SNAME/4HPACK,4H31  /
      DATA JBUG/4hCO31/
C
C----- C O M M O N  B L O C K S -------------------------
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FSNWUP/IUPWE,IUPSC
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/sums31'
      include 'snow43/cntl31'
      include 'snow43/cupdt31'
      include 'snow43/snup31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/pack31.f,v $
     . $',                                                             '
     .$Id: pack31.f,v 1.1 1996/05/07 11:04:46 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
C     CONSTANTS
C     IF SNOWFALL EXCEEDS SNEW/HR--TINDEX=TPX
      SNEW=1.5
C     IF RAIN EXCEEDS RMIN/HR--USE RAIN-ON-SNOW MELT EQUATION
      RMIN=0.25
C     SBC=STEFAN/BOLTZMAN CONSTANT--MM/(((DEGK/100)**4)*HR)
      SBC=.0612
C.......................................
C     CHECK IF DEBUG OUTPUT REQUESTED.
      CALL FPRBUG(SNAME,1,31,IBUG)
      IF(ITRACE.GE.1) WRITE(IODBUG,601)
  601 FORMAT(1H0,17H** PACK31 ENTERED)
C
      IF(IBUG.EQ.0) GO TO 110
      CALL MDYH1(KDA,KHR,imonth,iday,iyear,ihour,100,0,TZ)
      WRITE(IODBUG,610) imonth, iday, iyear, ihour,NDT,IDT,IFUT,MFC
  610 FORMAT(1H0,I2,1H/,I2,1H/,I4,1X,4H HR=,I2,2X,
     14HNDT=,I2,2X,4HIDT=,I2,2X,5HIFUT=,I1,2X,4HMFC=,F6.3)
      WRITE(IODBUG,611)TA,RSL,OWE,OSC,PGM,TWE
  611 FORMAT(1H ,5X,15HINPUT DATA--TA=,F6.2,2X,4HRSL=,F6.0,
     1  2X,4HOWE=,F6.1,2X,4HOSC=,F7.2,2X,4HPGM=,F5.3,2X,5H TWE=,F6.1)
      J=NDT
      IF(J.GT.6) J=6
      WRITE(IODBUG,612) (PX(I),PCTS(I),I=1,J)
  612 FORMAT(1H ,5X,12HPX AND PCTS=,6(F6.2,F8.2))
      IF(J.EQ.NDT) GO TO 100
      WRITE(IODBUG,613) (PX(I),PCTS(I),I=7,NDT)
  613 FORMAT(1H ,17X,6(F5.1,F8.2))
  100 continue
C.......................................
C     INITIAL VALUES
  110 ITPX=IDT/NDT
      FITPX=ITPX
      FNDT=NDT
      GM=PGM/FNDT
      SFNEW=SNEW*FITPX
      RFMIN=RMIN*FITPX
      SBCI=SBC*FITPX
      MC=0
C.......................................
C.......................................
C     CYCLE THROUGH THE COMPUTATIONAL PERIOD FOR EACH PRECIPITATION
C        TIME INTERVAL
      DO 250 I=1,NDT
      MELT=-99.
      WATER=-99.
      HEAT=-99.
      EXCESS=-99.
      GMRO=-99.
      PXI=PX(I)
      IF((PXI.EQ.0.0).AND.(WE.EQ.0.0)) GO TO 230
  120 SFALL=0.0
      CNHSPX=0.0
      RAIN=0.0
      RAINM=0.0
      IF(PXI.EQ.0.0) GO TO 201
C.......................................
C     DETERMINE FORM OF PRECIP. AND ACCUMULATE SNOW COVER IF SNOW.
      PCT=PCTS(I)
      IF(PCT.GT.1.0) PCT=1.0
      TPX=TA
      IF(PCT.LT.0.0) GO TO 130
C
C     FORM OF PRECIP. INPUT.
      FRACS=PCT
      FRACR=1.0-PCT
      GO TO 190
  130 IF (LAEC.EQ.0) GO TO 170
      DRSL=DRSL+RSL
      NDRSP=NDRSP+1
C
C     FORM OF PRECIP. BASED ON RAIN-SNOW ELEVATION
      IF (RSL.GT.AE(1,1)) GO TO 140
      FRACR=0.0
      rnsnwk=0.0
      GO TO 160
  140 DO 150 J=2,NPTAE
      IF (RSL.GT.AE(1,J)) GO TO 150
          FRACR=AE(2,J-1)+(AE(2,J)-AE(2,J-1))*
     1        ((RSL-AE(1,J-1))/(AE(1,J)-AE(1,J-1)))
          rnsnwk = ((ae(2,j)-ae(2,j-1))/
     1        (ae(1,j)-ae(1,j-1))) * (1.0/rlapse)
          GO TO 160
  150 CONTINUE
      FRACR=1.0
      rnsnwk=0.0
  160 FRACS=1.0-FRACR
      GO TO 190
C
C     FORM OF PRECIP. BASED ON TEMPERATURE.
  170 IF(TPX.GT.PXTEMP) GO TO 180
C
C     SNOW
      FRACS=1.0
      FRACR=0.0
      GO TO 190
C
C     RAIN
  180 FRACS=0.0
      FRACR=1.0
  190 IF(FRACS.EQ.0.0) GO TO 200
C
C     ACCUMULATE SNOWFALL
      SFALL=PXI*FRACS*SCF
      IF(IFUT.EQ.0) SFALL=SFALL*SFALLX
      SPX=SPX+SFALL
      SSFALL=SSFALL+SFALL
      DSFALL=DSFALL+SFALL
C
C     RAINFALL AND RAIN MELT.
  200 RAIN=PXI*FRACR
      SPX=SPX+RAIN
      DRAIN=DRAIN+RAIN
C.......................................
C     MELT AT GROUND-SNOW INTERFACE
  201 IF(WE+SFALL.GT.GM*AESC) GO TO 205
      GMRO=WE+LIQW+SFALL
      MELT=0.0
      ROBG=RAIN
      RAIN=0.0
      SROBG=SROBG+ROBG
      EXCESS=0.
      GO TO 210
  205 NR=1
      IF(RAIN.GT.RFMIN) NR=0
C
C.......................................
C     COMPUTE RAIN FALLING ON BARE GROUND
      ROBG=(1.0-AESC)*RAIN
      RAIN=RAIN-ROBG
C
C.......................................
C     COMPUTE SUM 
      SROBG=SROBG+ROBG
C
      RATIO=0.0
      IF(WE.GT.0.0) RATIO=LIQW/WE
      GMRO=GM*(1.+RATIO)*AESC
C
C     Compute the forecasted states using the Non-linear state
C     space equations.
C
      CALL STSP31(PXI,GM,GMRO,RFMIN,SFNEW,SFALL,FRACS,IDN,imn,ndt,TA,
     1 SBCI,MELT,WATER,HEAT,EXCESS,mf,const)
C
      if( iprop .eq. 1 .and. we .gt. 0.1) then
C
C----------------------------------------------------
C         PROPAGATE STATE ERROR COVARIANCE MATRIX 
C         USING A LINEARIZATION OF THE STATE SPACE 
C         EQUATIONS.
C----------------------------------------------------
C
C         SET COVARIANCE ELEMENT OF PREC. BEFORE PROPAGATING.
C         NOTE: VALUE STORED IN CVU MATRIX FOR PREC. IS ACTUALLY
C         THE COEFFICIENT OF VARIATION.
C
          CV=CVU(1,1)
          PVAR=(CV*PXI)**2
          CVU(1,1)=PVAR
C
          call prop31(PXI, GM, FRACS, TA, SBCI, MF, NR, CONST, P)
C
C         RESET CVU ELEMENT TO COEF. OF VAR.
C
          CVU(1,1)=CV
      endif
C
C
      IF(NR.EQ.1) SMELT=SMELT+MELT
      IF(NR.EQ.0) SMELTR=SMELTR+MELT
C
      QNET=MELT-HEAT
      DQNET=DQNET+QNET
C
      IF( WE .LE. 0.1 ) GO TO 210
C.......................................
C     ROUTE EXCESS WATER THROUGH THE SNOW COVER.
      CALL ROUT31(ITPX,EXCESS,WE,AESC,STORGE,NEXLAG,EXLAG,PACKRO)
      IF(ifbug(jbug) .EQ. 1) then
          CALL PRCO31(PXI,TA,MELT,WATER,HEAT,EXCESS,
     1                GMRO,PACKRO,RM,iodbug)
      endif
C
C.......................................
C     ADD GROUNDMELT RUNOFF TO SNOW COVER OUTFLOW.
      PACKRO=PACKRO+GMRO
      IF(IBUG.EQ.1) WRITE(IODBUG,614) SFALL,RAIN,MELT,NR,HEAT,WATER,
     1AESC,QNET,EXCESS,GMRO,PACKRO,ROBG
      GO TO 240
C.......................................
C     SNOW GONE - SET ALL CARRYOVER TO NO SNOW CONDITIONS.
  210 TEX=0.0
      DO 220 N=1,NEXLAG
  220 TEX=TEX+EXLAG(N)
      PACKRO=EXCESS+GMRO+TEX+STORGE
      CALL ZERO31
      AESC=0.0
      go to 240
C.......................................
C     NO SNOW COVER - NO NEW SNOWFALL.
  230 ROBG=PXI
      PACKRO=0.0
      AESC=0.0
      SROBG=SROBG+ROBG
C.......................................
C     COMPUTE RAIN+MELT
  240 RM(I)=PACKRO+ROBG
      SRM=SRM+RM(I)
  250 CONTINUE
C
C.......................................
C     END OF COMPUTATIONAL PERIOD
C.......................................
C     SET SIMULATED AESC AND TOTAL WATER-EQUIVALENT.
      TEX=0.0
      DO 255 N=1,NEXLAG
  255 TEX=TEX+EXLAG(N)
      freew = tex + storge
      TWE = WE + LIQW + freew
C
      COVER=AESC
C.......................................
C
C     STORE VALUES SO COMPUTED VALUES WILL BE AVAILABLE FOR PRINTOUT
C        EVEN IF UPDATING OCCURS.
      CWE=TWE
      CAESC=COVER
C.......................................
C     UPDATING SECTION
C----------------------------------------------------
C     Set missing values flag for water equivalent.
C     iwemsng = 0 when owe and variance exists
C     iwemsng = 1 when owe and gain are provided.
C     iwemsng = 2 when nothing is provided or any time
C                 that iupwe = 0
C----------------------------------------------------
      iwemsng=2
      if ((owe .ge. 0.0) .and. (owev .ge. 0.0)) then
         iwemsng = 0
      else if ((owe .ge. 0.0) .and. (gain .ge. 0.0)) then
         iwemsng = 1
      end if
      if(iupwe .eq. 0) iwemsng = 2
C
C----------------------------------------------------
C     Set missing values flag for snow cover.
C     iscmsng = 1 when osc is provided.
C     iscmsng = 2 when nothing is provided or any time
C                 that iupsc = 0
C----------------------------------------------------
      iscmsng = 2
      if ( osc .ge. 0.  ) iscmsng = 1
      if ( iupsc .eq. 0 ) iscmsng = 2
C
C----------------------------------------------------
C     Call UPDT31 if the filter needs to be run or
C     if there's some updating to be done.
C----------------------------------------------------
      if((iwemsng .lt. 2) .or.
     |   (iscmsng .lt. 2)) then
         CALL UPDT31(iwemsng, iscmsng, OWE, owev, OSC,
     1        TWE, COVER, gain, freew)
      endif
c............................................
c compute the total water equivalent variance
c............................................
      if (iprop .eq. 1) then
          twev = p(1,1) + p(3,3) + 2 * p(1,3)
      endif
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) go to 270
      WRITE(IODBUG,615) TWE,COVER,CWE,CAESC
      WRITE(IODBUG,616) (RM(I),I=1,NDT)
      IF (((TWE.NE.CWE).OR.(COVER.NE.CAESC)) .and. 
     &     ifbug(jbug) .eq. 1) then
          call PRCO31(PXI, TA, MELT, WATER, HEAT, EXCESS,
     1                GMRO, PACKRO, RM, iodbug)
      endif
C
C.......................................
  270 IF(ITRACE.GE.1) WRITE(IODBUG,699)
  614 FORMAT(1H0,8X,5HSFALL,4X,4HRAIN,4X,4HMELT,2X,2HNR,4X,4HHEAT,2X,6HW     
     1ATER ,4X,4HAESC,4X,4HQNET,2X,6HEXCESS,4X,4HGMRO,2X,6HPACKRO,4X,4HR
     2OBG,/6X,3F8.2,I4,8F8.2)
  615 FORMAT(1H0,5X,18HOUTPUT DATA-- TWE=,F6.1,2X,6HCOVER=,F4.2,2X,
     1 4HCWE=,F6.1,2X,6HCAESC=,F4.2)
  616 FORMAT(1H ,5X,3HRM=,24F5.1)
  699 FORMAT(1H0,14H** EXIT PACK31)
      RETURN
      END
