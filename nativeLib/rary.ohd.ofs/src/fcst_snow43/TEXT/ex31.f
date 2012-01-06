C     MODULE EX31
C
      SUBROUTINE EX31(PS,CS,PX,TA,RM,PCTS,RSTS,OWE,owev,SWE,swev,OSC,
     1   COVER,PPX,PPCTS,PRM,TALR)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE EXECUTION ROUTINE FOR THE SNOW-43 OPERATION
C     SNOW-43 WAS DEVELOPED FROM THE SNOW-17 OPERATION, WHICH IS
C     BASED ON NOAA TECH. MEMO. NWS HYDRO-17.  SNOW-43 IMPLEMENTS
C     A STATE-SPACE VERSION OF THE SNOW MODEL COMPUTATIONS AND
C     ADDS A KALMAN FILTERING UPDATING PROCEDURE TO THE EXISTING
C     UPDATING CAPABILITIES.  THE PROCEDURES USED ARE BASED ON
C     NOAA TECHNICAL REPORT, NWS 43.
C
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR SNOW-17
C     Modified by...
C        Mark S. Woodbury, RTi, June 1995 for the SNOW-43 operation.
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PS           I/O         The P - Parameter array
C      CS           I/O         The C - Carryover array
C      PX            I          Precipitation data
C      TA            I          Temperature data
C      RM            O          Rain + Melt values
C      PCTS          I          Percent snowfall data
C      RSTS          I          Rain Snow elevation data
C      OWE           I          Observed Water equivalent data
C      owev          I          Observed Water equiv. variance
C      SWE           O          Simulated Water equiv.
C      swev          O          Simulated Water equiv. variance
C      OSC           I          Observed Areal extent of Snow
C                               Cover data
C      COVER         O          Simulated areal extent of Snow
C                               Cover values
C      PPX           -          Working space
C      PPCTS         -          Working space
C      PRM           -          Working Space
C      TALR          -          Working Space
C.......................................
C
      real    rlapse
      real    jbug
      integer idt31
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PS(*),CS(*),PX(*),TA(*),RM(*),PCTS(*),OWE(*),SWE(*)
      DIMENSION OSC(*),COVER(*),TALR(*),PPX(*),PPCTS(*),PRM(*),RSTS(*)
      DIMENSION LASTDA(12),IDANG(12),CT(43)
      CHARACTER*8  SNAME
      dimension rmonth(12), owev(*), swev(*)
C
C----- C O M M O N  B L O C K S -------------------------
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/where'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ffgctl'
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/sums31'
      include 'snow43/snup31'
      include 'snow43/cntl31'
      include 'snow43/cupdt31'
      COMMON/FSNWUP/IUPWE,IUPSC
      COMMON/FSDATA/NSDV,JHSNW(10),WESNW(10),AESNW(10),WEADD(10)
      COMMON/FPXTYP/NRSV,IJHRS(20),LJHRS(20),PCTSV(20)
      COMMON/FMFC19/NMFC,IJHMF(5),LJHMF(5),VMFC(5)
      include 'snow43/fskdata'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/ex31.f,v $
     . $',                                                             '
     .$Id: ex31.f,v 1.4 2006/03/23 13:40:36 xfan Exp $
     . $' /
C    ===================================================================
C
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA  SNAME / 8hEX31     /
      DATA  JBUG  / 4hSM31 /
CFAN
CFAN  HSD bug r26-20
CFAN
CFAN      DATA LASTDA/31,28,31,30,31,31,31,31,30,31,30,31/
      DATA LASTDA/31,28,31,30,31,30,31,31,30,31,30,31/
CFAN
      DATA IDANG/285,316,345,10,40,71,101,132,163,193,224,254/
C
C.......................................
C     TRACE LEVEL=1,DEBUG FLAG=IBUG.
      CALL FPRBUG(SNAME,1,31,IBUG)
      IF(ITRACE.GE.1) WRITE(IODBUG,601)
  601 FORMAT(1H0,15H** EX31 ENTERED)
C.......................................
C     CONSTANT
C     IF SNOWFALL IS LESS THAN HSNOF/HR -- DO NOT
C        LEAVE CURRENT DEPLETION CURVE.
   90 HSNOF=0.4
C.......................................
C     VALUES OF CONTROL VARIABLES.
C        THOSE NEEDED IN ALL CASES FIRST.
      IDT=PS(14)
      FIDT=IDT
      ITPX=PS(10)
      FITPX=ITPX
      SNOF=HSNOF*ITPX
      NDT=IDT/ITPX
      NEXLAG=5/ITPX+2
      LRM=PS(17)
      LSWE=PS(20)
      IF (LSWE.EQ.0) GO TO 103
      ITSWE=PS(LSWE+3)
      NSWE=24/ITSWE
      lswev = ps(32)
      if (lswev .ne. 0) then
         itswev = itswe
         nswev = nswe
      end if
  103 LCOVER=PS(22)
      IF (LCOVER.EQ.0) GO TO 104
      ITSSC=PS(LCOVER+3)
      NSSC=24/ITSSC
  104 LSUMS=PS(23)
      LPM=PS(25)
      IPRINT=PS(24)
C
C     Find IDPRINT and IPPRINT
C          IDPRINT contains last digit 0, 1 or 2
C          IPPRINT contains the tens 10, 20, 30 or 40
C
      IPPRINT = (IPRINT/10) * 10
      IDPRINT = IPRINT - IPPRINT
C
      IF (MAINUM.NE.1) GO TO 102
      IF (IPRSNW.EQ.0) GO TO 102
      IF (IPRSNW.EQ.1) IDPRINT=1
      IF (IPRSNW.EQ.-1) IDPRINT=0
  102 IF(NOPROT.EQ.1) IDPRINT=0
      IF(NOSNOW.EQ.1) IDPRINT=0
      NPS=PS(16)
      IF(NOSNOW.EQ.1) GO TO 101
C
C     THOSE ONLY NEEDED IF NOSNOW=0
      LPCTS=PS(18)
      LOWE=PS(19)
      IF (LOWE.EQ.0) GO TO 108
      ITOWE=PS(LOWE+3)
      NOWE=24/ITOWE
      lowev = ps(31)
      if (lowev .ne. 0) then
         iowev = itowe
         nowev = nowe
      end if
  108 LOSC=PS(21)
      IF (LOSC.EQ.0) GO TO 109
      ITOSC=PS(LOSC+3)
      NOSC=24/ITOSC
  109 LADC=PS(26)
      LTAPM=PS(27)
      LUPPM=PS(28)
      LMFV=PS(29)
      LAEC=PS(30)
      lkfpm = ps(33)
      iprop=0
      if (lkfpm .ne. 0) iprop = 1
      NCO=10+NEXLAG+1+25*iprop
C.......................................
C     DEBUG OUTPUT - PRINT PS() AND CS()
  101 IF(IBUG.EQ.0) GO TO 100
      WRITE(IODBUG,610) NOSNOW
  610 FORMAT(1H0,43HSNOW-43 DEBUG--CONTENTS OF PS AND CS ARRAYS,5X,7HNOS
     1NOW=,I1)
      WRITE(IODBUG,611)
  611 FORMAT(1H0,'PS Array Real')
      WRITE(IODBUG,612) (PS(I),I=1,NPS)
  612 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,613)
  613 FORMAT(1H0,'PS Array Words')
      WRITE(IODBUG,614) (PS(I),I=1,NPS)
  614 FORMAT(1H0,15(4X,A4))
      WRITE(IODBUG,615)
  615 FORMAT(1H0,'CS Array Real')
      WRITE(IODBUG,612) (CS(I),I=1,NCO)
C.......................................
C     GET TEMPERATURE INFORMATION AND LAPSE RATES FOR EACH IDT PERIOD
C        IF NEEDED.
  100 IF(NOSNOW.EQ.1) GO TO 110
      IF(LTAPM.EQ.0) GO TO 200
      TAELEV=PS(LTAPM)
      ELEV=PS(LPM+1)
      EDIFF=(TAELEV-ELEV)*0.01
      J=24/IDT
      TALMAX=PS(LTAPM+1)
      TALMIN=PS(LTAPM+2)
      DIFF=TALMAX-TALMIN
      FLOCAL=LOCAL
      DO 105 I=1,J
      FI=I
      TI=(FI-1.0)*FIDT+0.5*FIDT
      TL=TI+FLOCAL
      IF(TL.GT.24.0) TL=TL-24.0
      IF(TL.LT.15.0) GO TO 106
      TALR(I)=TALMAX-((TL-15.0)/15.0)*DIFF
      GO TO 105
  106 IF(TL.GT.6.0) GO TO 107
      TALR(I)=TALMIN+((6.0-TL)/15.0)*DIFF
      GO TO 105
  107 TALR(I)=TALMIN+((TL-6.0)/9.0)*DIFF
  105 CONTINUE
C.......................................
C     CHECK FOR DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 200
      WRITE(IODBUG,616)
  616 FORMAT(1H0,27HAIR TEMPERATURE LAPSE RATES)
      WRITE(IODBUG,612) (TALR(I),I=1,J)
C.......................................
C     RAIN-SNOW ELEVATION PARAMETERS
  200 IF (LAEC.EQ.0) GO TO 110
      NPTAE=PS(LAEC)
      DO 205 I=1,NPTAE
      J=LAEC+5+(I-1)*2
      AE(1,I)=PS(J)
      AE(2,I)=PS(J+1)
  205 CONTINUE
C.......................................
C     PARAMETER VALUES
  110 PXADJ=PS(LPM)
      IF(NOSNOW.EQ.1) GO TO 115
      ELEV=PS(LPM+1)*0.01
      PA=29.9-0.335*ELEV+0.00022*(ELEV**2.4)
      PA=33.86*PA
      SCF=PS(LPM+2)
C
C     MFMFAX, MFMIN, NMF, and TIPM are adjusted using FITPX
C     instead of FIDT.  This allows the precipitation time
C     interval to be used for computations in the state-space
C     model and allows the temperature time interval
C     to be greater than the precipitation time interval.
C
      MFMAX=PS(LPM+3)
      MFMAX=(MFMAX*FITPX)/6.0
      MFMIN=PS(LPM+4)
      MFMIN=(MFMIN*FITPX)/6.0
      NMF=PS(LPM+7)
      NMF=(NMF*FITPX)/6.0
      UADJ=PS(LPM+5)
      UADJ=(UADJ*ITPX)/6.0
      SI=PS(LPM+6)
      GM=PS(LPM+12)
      PGM=(GM*FIDT)/24.0
      MBASE=PS(LPM+9)
      PXTEMP=PS(LPM+10)
      PLWHC=PS(LPM+11)
      TIPM=PS(LPM+8)
      TIPM=1.0-((1.0-TIPM)**(FITPX/6.0))
      ALAT=PS(LPM+13)
      rnsnwk = 0.
      rlapse = 0.
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
C.......................................
C     UPDATING PARAMETERS
   30 IF(MAINUM.le.2) then
C
C     OPERATIONAL PROGRAMS - ALL PARAMETERS DEFINED.
         WETOL=PS(LUPPM)
         SCTOL=PS(LUPPM+1)
         MFC=1.0
         SFALLX=PS(LUPPM+5)
         WINDC=PS(LUPPM+6)
C
      else
C        CALIBRATION PROGRAMS
         IUPWE=0
         IUPSC=0
         IF(LUPPM.EQ.0) GO TO 114
         IF(LOWE.GT.0) IUPWE=1
         IF(LOSC.GT.0) IUPSC=1
         WETOL=PS(LUPPM)
         SCTOL=PS(LUPPM+1)
  114    MFC=1.0
         SFALLX=1.0
         WINDC=1.0
      end if
C.......................................
c     Kalman Filtering Updating Parameters
      if (iprop .eq. 1) then
C
C     Initialize h matrix for measurement equation:  (z = we + liqw)
C
         do 397 i = 1, nnfltr
            do 398 j = 1, mmfltr
               h(j, i) = 0.
 398        continue
 397     continue
         h(1,1) = 1.
         h(1,3) = 1.
C
C    Initialize CVU matrix - Note: precipitation value
C    is actually a coefficient of variation and not a
C    variance. This value must be converted to variance
C    for each precipitation value.
C
         loc = lkfpm
         do 401 i = 1, llfltr
            do 400 j = 1, llfltr
               cvu(j,i) = ps (loc)
               loc=loc+1
 400        continue
 401     continue
C
C    Initialize Q matrix for system equation.
C
         do 403 i=1, nnfltr
            do 402 j=1, nnfltr
               q(j,i) = ps(loc)
               loc=loc+1
 402        continue
 403     continue
C
C    Initialize the default monthly R values
C    for the measurement equation.
C
         do 404 i=1,12
            rmonth(i) = ps(loc)
            loc=loc+1
 404     continue
C
         rlapse = ps(lkfpm+43)
C
C     skip over 4 words of rain snow elevation information
         loc = loc + 4
         iscfltr = ps(loc)
      end if
C.......................................
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
      EXLAG(I)=CS(10+I)
  116 TEX=TEX+EXLAG(I)
      aesc = cs(11 + nexlag)
C
C    Retrieve the state error covariance matrix
C    (P) from the CS array.
C
      if(iprop .eq. 1) then
         loc = 11 + nexlag + 1
         do 411 i = 1, nnfltr
            do 410 j = 1, nnfltr
               p(j,i) = cs(loc)
               loc = loc + 1
 410        continue
 411     continue
      end if
C
C     IF NOSNOW=1 SET SNOW CARRYOVER TO ZERO.
      IF(NOSNOW.EQ.0) GO TO 117
      CALL ZERO31
      TWE1=0.0
      CHGWE=0.0
      GO TO 118
C.......................................
C     INITIAL VALUES
  117 TWE=WE+LIQW+TEX+STORGE
      TWE1=TWE
      CHGWE=0.0
C
  119 DSFALL=0.0
      DRAIN=0.0
      DQNET=0.0
      DRSL=0.0
      NDRSP=0
      PTWE=TWE
  118 SPX=0.0
      SSFALL=0.0
      SRM=0.0
      SMELT=0.0
      SMELTR=0.0
      SROBG=0.0
      ITITLE=0
      IC=1
      IRS=1
      IMF=1
C.......................................
C     INITIAL TIMING VALUES
      KDA=IDA
      KHR=IHR
      LAST=0
      IF (MAINUM.NE.1) GO TO 310
      jh=(ida-1)*24+ihr-idt
      IF (IFFG.EQ.0) GO TO 310
C     CALCULATE THE COMPUTATIONAL PERIOD AT OR JUST BEFORE LSTCMPDY
      KDAFFG=LDACPD
      KHRFFG=(LHRCPD/IDT)*IDT
      IF (KHRFFG.GT.0) GO TO 310
      KDAFFG=KDAFFG-1
      KHRFFG=24
  310 IF (NOSNOW.EQ.1) GO TO 120
      CALL MDYH1(KDA,KHR,I,J,L,N,100,0,TZ)
      IDN=IDANG(I)+J
      IMN=I
c
c  changes needed for pack31
c
      kmo31 = i
      kda31 = j
      kyr31 = l-1900
      khr31 = n
      idt31 = idt
c
C.......................................
C     GET INPUT DATA NEEDED FOR COMPUTATIONAL PERIOD.
  120 KOFF=KDA-IDADAT
C
C     PRECIPITATION DATA - ALSO APPLY ADJUSTMENT
      LPX=KOFF*(24/ITPX)+KHR/ITPX
      DO 121 I=1,NDT
      J=LPX-(NDT-I)
  121 PPX(I)=PX(J)*PXADJ
      IF(NOSNOW.EQ.1) GO TO 150
C
C     PERCENT SNOWFALL DATA
      DO 122 I=1,NDT
      IF(LPCTS.EQ.0) GO TO 123
      J=LPX-(NDT-I)
      PPCTS(I)=PCTS(J)
      GO TO 122
  123 PPCTS(I)=-999.0
  122 CONTINUE
C
C     RAINSNOW AND MFC MODS, OPERATIONAL PROGRAM ONLY
      IF (MAINUM.NE.1) GO TO 124
C     OVERRIDE FORM OF PRECIPITATION, IF MOD INPUT.
      IF(NRSV.EQ.0) GO TO 225
 128  IF(IRS.GT.NRSV) GO TO 225
      JH2=JH+IDT
      IF(JH2.LT.IJHRS(IRS)) GO TO 225
      JH1=JH2-IDT+ITPX
      IF(JH1.LT.IJHRS(IRS)) JH1=IJHRS(IRS)
      IF(JH2.GT.LJHRS(IRS)) JH2=LJHRS(IRS)
      J1=(JH1-JH)/ITPX
      J2=(JH2-JH)/ITPX
      DO 129 I=J1,J2
 129  PPCTS(I)=PCTSV(IRS)
      IF(JH2.LT.LJHRS(IRS)) GO TO 225
      IRS=IRS+1
      GO TO 128
C     DETERMINE MFC BASED ON MOD INPUT
  225 IF (NMFC.EQ.0) GO TO 124
      IF (IMF.GT.NMFC) GO TO 229
      JH2=JH+IDT
      IF (JH2.LT.IJHMF(IMF)) GO TO 229
      MFC=VMFC(IMF)
      IF (JH2.LT.LJHMF(IMF)) GO TO 124
      IMF=IMF+1
      GO TO 124
  229 MFC=1.0
C
C     AIR TEMPERATURE DATA - CORRECT FOR LAPSE RATE.
  124 L=KOFF*(24/IDT)+KHR/IDT
      PTA=TA(L)
      IF(LTAPM.EQ.0) GO TO 210
      I=KHR/IDT
      PTALR=TALR(I)
      PTA=PTA+EDIFF*PTALR
C
C     RAIN-SNOW ELEVATION DATA
  210 IF (LAEC.GT.0) GO TO 215
      PRSL=-999.0
      GO TO 125
  215 PRSL=RSTS(L)
C
C     OBSERVED WATER-EQUIVALENT AND SNOW COVER DATA.
C     CHECK FOR RUN-TIME DATA FIRST - OPERATIONAL ONLY.
C     Order of Precedence for updating TWE: 1. WEUPDATE mod,
C        2.  WECHNG mod, 3. Time series values.
C
  125 POWE=-99.0
      POSC=-99.0
      powev = -999.0
      pgain = -999.0
      IF(MAINUM.NE.1) GO TO 130
C
C     nkdv  = Number of snow data values for WEUPDATE mod (Kalman filter)
C     jhsnk = Julian hour of snow data values (WEUPDATE)
C     nsdv  = Number of snow data values for WECHNG and AESCCHNG mods
C     jhsnw = Julian hour of snow data values (WECHNG, AESCCHNG)
C
      JH=JH+IDT
C
C    Process the WEUPDATE mod
C
      if (nkdv .eq. 0) go to 415
      do 414 i = 1, nkdv
        if (jh .eq. jhsnk(i)) then
C
C    Get the values from fskdat common block
C
           powe = wesnk(i)
           powev = vwesnk(i)
           pgain = wegain(i)
           if ( pgain .gt. 1.) pgain = 1.
           if( powe .ge. 0. ) then
               if ( iprop .eq. 1 ) then
C
C              verify that we have a gain or a variance.
C              Use default monthly variance if no gain or
C              variance is provided.
C
                  if (powev .lt. 0.) then
                     if ( pgain .lt. 0. ) powev = rmonth(imn)
                     if ( powev .lt. 0. .and. pgain .lt. 0.) then
                         write (ipr, 617)
                         call warn
                     endif
                  endif
               else
C
C              Not propagating, reset powev to -99.0
C              and verify that we have a gain.
C
                  powev = -99.0
                  if( pgain .lt. 0. ) then
                      write (ipr, 620)
                      call warn
                  endif
               endif
           endif
           go to 415
        endif
 414  continue
C
C    Process AESCCHNG, WECHNG, and WEADD mods
C
 415  IF(NSDV.EQ.0) GO TO 130
      DO 126 I=1,NSDV
      N=I
      IF (JH.EQ.JHSNW(I)) GO TO 127
  126 CONTINUE
      GO TO 130
  127 POSC=AESNW(N)
C
C    Check if POWE has already been assigned a value from WEUPDATE
C
      if ( powe .ne. -99.0 ) go to 137
      POWE=WESNW(N)
      if (powe .ge. 0.0) pgain = 1.0
      IF (WEADD(N).LT.0.0) GO TO 130
      IF (WESNW(N).LT.0.0) GO TO 131
      POWE=POWE+WEADD(N)
      GO TO 130
  131 POWE=TWE+WEADD(N)
      if (powe .ge. 0.0) pgain = 1.0
C
C     WATER-EQUIVALENT FROM TS.
C
 130  IF(POWE .NE. -99.0) GO TO 137
      IF(LOWE.EQ.0) GO TO 133
      IF((KHR/ITOWE)*ITOWE.EQ.KHR) GO TO 134
 133  POWE=-999.0
      GO TO 137
 134  L=KOFF*NOWE+KHR/ITOWE
      POWE=OWE(L)
      if ( powe .ge. 0. ) then
         if ( iprop .eq. 1) then
            if (lowev .ge. 0.) powev = owev(l)
            if (powev .lt. 0.) powev = rmonth(imn)
            if ( powev .lt. 0.) then
                write (ipr, 618)
                call warn
            endif
         else
            pgain = 1.0
         end if
      end if
C
C     AREAL EXTENT OF COVER FROM TS
C
 137  IF(POSC.NE.-99.0) GO TO 140
      IF(LOSC.EQ.0) GO TO 138
      IF((KHR/ITOSC)*ITOSC.EQ.KHR) GO TO 139
 138  POSC=-999.0
      GO TO 140
  139 L=KOFF*NOSC+KHR/ITOSC
      POSC=OSC(L)
C.......................................
C     DETERMINE IF COMPUTATIONAL PERIOD IS IN THE FUTURE.
  140 IFUT=0
      IF(KDA.LT.LDACPD) GO TO 145
      IF((KDA.EQ.LDACPD).AND.(KHR.LE.LHRCPD)) GO TO 145
      IFUT=1
C.......................................
C     PERFORM SNOW MODEL COMPUTATIONS FOR THE COMPUTATIONAL PERIOD.
C
  145 CALL PACK31(KDA,KHR,NDT,PTA,PPX,PPCTS,PRSL,POWE,powev,pgain,POSC,
     1PGM,PRM,rlapse,twe,twev,PCOVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN)
c
      if(ifbug(jbug) .eq. 1) call prsm31
      khr31 = khr31+idt31
      if(khr31 .le. 24) go to 420
      khr31 = khr31-24
      kda31 = kda31+1
 420  continue
c
      IF (TWE.NE.CWE) CHGWE=CHGWE+(TWE-CWE)
C
C.......................................
C     STORE RESULTS - RAIN+MELT FIRST.
      IF (LRM.EQ.0) GO TO 155
      DO 151 I=1,NDT
      J=LPX-(NDT-I)
  151 RM(J)=PRM(I)
      GO TO 155
C
C     NOSNOW=1 RESULTS.
  150 DO 152 I=1,NDT
      SPX=SPX+PPX(I)
      SRM=SRM+PPX(I)
      SROBG=SROBG+PPX(I)
      TWE=0.0
      twev = 0.0
      PCOVER=0.0
      IF (LRM.EQ.0) GO TO 152
      J=LPX-(NDT-I)
      RM(J)=PPX(I)
  152 CONTINUE
C
C     SIMULATED WATER-EQUIVALENT
  155 IF(LSWE.EQ.0) GO TO 160
      IF((KHR/ITSWE)*ITSWE.NE.KHR) GO TO 160
      L=KOFF*NSWE+KHR/ITSWE
      SWE(L)=TWE
C
C     simulated water equivalent variance
      if( lswev .ne. 0 ) then
          swev(l) = twev
          if( iprop .eq. 0 ) swev(l) = -999.0
      endif
C
C     SIMULATED AREAL SNOW COVER
  160 IF(LCOVER.EQ.0) GO TO 170
      IF((KHR/ITSSC)*ITSSC.NE.KHR) GO TO 170
      L=KOFF*NSSC+KHR/ITSSC
      COVER(L)=PCOVER
C.......................................
C     PRINT RESULTS IF REQUESTED.
  170 IF(IDPRINT.EQ.0) GO TO 175
      IPRINT = IPPRINT + IDPRINT
      CALL PRSN31(KDA,KHR,IPRINT,ITITLE,PS(2),TWE,twev,powev,
     1            CWE,PTWE,POWE,PCOVER,CAESC,POSC,LAEC,MOPR,
     2            IDPR,IYPR,IHPR)
C.......................................
C     STORE FFG CARRYOVER IF REQUESTED.
  175 IF (IFILLC.EQ.0) GO TO 180
      IF (MAINUM.NE.1) GO TO 180
      IF (IFFG.EQ.0) GO TO 180
      IF ((KDA.EQ.KDAFFG).AND.(KHR.EQ.KHRFFG)) GO TO 320
      GO TO 180
  320 CALL CSAV31(CS, iprop)
C.......................................
C     STORE CARRYOVER IF REQUESTED.
  180 IF(IFILLC.EQ.0) GO TO 190
      IF(NCSTOR.EQ.0) GO TO 190
      IF(IC.GT.NCSTOR) GO TO 190
      IF((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GO TO 185
      GO TO 190
C
C     SAVE CARRYOVER
  185 CALL CSAV31(CT,iprop)
      CALL FCWTCO(KDA,KHR,CT,NCO)
      IC=IC+1
C.......................................
C     CHECK FOR END OF EXECUTION PERIOD
  190 IF((KDA.EQ.LDA).AND.(KHR.EQ.LHR)) GO TO 195
C
C     INCREMENT TIMES FOR NEXT COMPUTATIONAL PERIOD.
      KHR=KHR+IDT
      IF(KHR.LE.24) GO TO 191
      KHR=IDT
      KDA=KDA+1
      IF (NOSNOW.EQ.0) IDN=IDN+1
      DSFALL=0.0
      DRAIN=0.0
      DQNET=0.0
      DRSL=0.0
      NDRSP=0
  191 IF(ITITLE.EQ.0) GO TO 120
      IF(LAST.GT.0) GO TO 192
      LAST=LASTDA(MOPR)
      IF((MOPR.EQ.2).AND.((IYPR/4)*4.EQ.IYPR)) LAST=LAST+1
  192 IHPR=IHPR+IDT
      IF(IHPR.LE.24) GO TO 120
      IHPR=IHPR-24
      IDPR=IDPR+1
      IF(IDPR.LE.LAST) GO TO 120
      IDPR=1
      MOPR=MOPR+1
      IF(MOPR.LE.12) GO TO 193
      MOPR=1
      IYPR=IYPR+1
  193 LAST=LASTDA(MOPR)
      IF((MOPR.EQ.2).AND.((IYPR/4)*4.EQ.IYPR)) LAST=LAST+1
      GO TO 120
C.......................................
C     END OF EXECUTION PERIOD.
C     COMPUTE WATER BALANCE FOR THE EXECUTION PERIOD.
  195 SBAL=SPX-SRM-(TWE-TWE1)+CHGWE
      IF(ABS(SBAL).LE.1.0) GO TO 196
      WRITE(IPR,619) SBAL
  619 FORMAT(1H0,47H**WARNING** SNOW BALANCE RESIDUAL EXCEEDS 1 MM., 3X,
     19HRESIDUAL=,F7.2)
      CALL WARN
C
C     STORE SUMS IF REQUESTED.
  196 IF(LSUMS.EQ.0) GO TO 197
      PS(LSUMS)=SPX
      PS(LSUMS+1)=SSFALL
      PS(LSUMS+2)=SRM
      PS(LSUMS+3)=SMELT
      PS(LSUMS+4)=SMELTR
      PS(LSUMS+5)=SROBG
      PS(LSUMS+6)=SBAL
C
C     STORE CARRYOVER IF REQUESTED.
  197 IF(IFILLC.EQ.0) GO TO 198
      IF (MAINUM.NE.1) GO TO 330
      IF (IFFG.NE.0) GO TO 198
  330 CALL CSAV31(CS, iprop)
C.......................................
C     PRINT DEBUG OUTPUT IF REQUESTED.
  198 IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,610) NOSNOW
      WRITE(IODBUG,611)
      WRITE(IODBUG,612) (PS(I),I=1,NPS)
      WRITE(IODBUG,615)
      WRITE(IODBUG,612) (CS(I),I=1,NCO)
C.......................................
  199 IF(ITRACE.GE.1) WRITE(IODBUG,699)
  699 FORMAT(1H0,'** EXIT EX31')
  617 format(1h0,10x,'**WARNING** VARIANCE OR GAIN IS NOT PROVIDED. UPD
     1ATING WILL NOT TAKE PLACE.')
  618 format(1h0,10x,'**WARNING** VARIANCE IS NOT PROVIDED. UPDATING WI
     1LL NOT TAKE PLACE.')
  620 format(1h0,10x,'**WARNING** A WEUPDATE MOD WAS SPECIFIED WITHOUT
     1PROVIDING A GAIN. A GAIN IS REQUIRED TO UPDATE USING THIS MOD WHEN
     1 NOT PROPAGATING.')
c
  999 continue
      RETURN
      END
