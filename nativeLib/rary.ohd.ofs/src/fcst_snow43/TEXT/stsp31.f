C     MODULE STSP31
C
      SUBROUTINE STSP31(PXI,GM,GMRO,RFMIN,SFNEW,SFALL,FRACS,IDN,imn,ndt,
     1 TA,SBCI,MELT,WATER,HEAT,EXCESS,mf,const)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This subroutine performs the state space computations
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        Jay Day - RTi, July 1988 for use in the SNOW-43 operation
C     Modified by...
C        Mark Woodbury, Nalneesh Gaur - RTi. July 1995 
C        for the SNOW-43 operation
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Current Percipitation Excess
C      GM            I          Ground melt
C      GMRO          I          Melt at Ground Snow Interface 
C      RFMIN         I          Minimum Rainfall to use 
C                               RAIN-ON-SNOW Melt equation
C      SFNEW         I          Snow fall threshold above which
C                               TINDEX=TPX
C      SFALL         I          Snow fall
C      FRACS         I          Fraction of Precip that is Snow
C      IDN           I          Number of days since March 21
C      imn           i          Current month
C      ndt           i          Number of precip, %snowfall &
C                               rain melt values/ computational
C                               time interval.
C      TA            I          Air Temperature
C      SBCI          I          Stefan Boltzman's constant
C      MELT          O          Amount of melt
C      WATER         O          Water due to melt and precip
C      HEAT          O          Heat gained by the pack
C      EXCESS        O          Excess water in the pack
C      MF            O          Melt factor computed by MELT()
C      CONST         O          A constant computed by F031 needed
C                               for updating later on
C.......................................
C
C
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      integer idn, imn, ndt
      real    pxi, gm, gmro, rfmin, sfnew,
     1        fracs, ta, sbci, melt, water, 
     2        heat, excess, mf, const
C     --- L O C A L ---
      integer ibug, i
      integer nr
      real    f0, x1, x, sname
      real    caesc
      real    arealp, tindx1, neghs1, liqw1, we1,
     1        liqwsp, ratio, cnhs, liqwmx, fracr, sfall
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/czero31'
      include 'snow43/snup31'
      include 'snow43/sums31'
      include 'snow43/cntl31'
      include 'snow43/cupdt31'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C----- D I M E N S I O N --------------------------------
      dimension sname(2)
      DIMENSION X(5), X1(5), F0(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/stsp31.f,v $
     . $',                                                             '
     .$Id: stsp31.f,v 1.1 1996/05/07 11:09:50 page Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      data sname/4hstsp,4h31  /
C
C....................................
C
      call fprbug(sname,1,31,ibug)
      IF(ITRACE.GE.1) then
           WRITE(IODBUG,601)
  601      FORMAT(1H0,17H** STSP31 ENTERED)
      endif
C
C
C     Move states in to the X array
C
      CALL SWST31(X,1)
C
C
      IF(SFALL.LE.0.0) GO TO 130
      IF(WE+LIQW+SFALL.LT.3.*SB) GO TO 100
C
C   SET NEW ACCUMULATION PERIOD
C
      ACCMAX=WE+LIQW
      AEADJ=0.0
C
  100 IF(SBWS.GT.SB) GO TO 110
C
C   ON ADC CURVE
C
      IF(SFALL.LT.SNOF) GO TO 130
      SBWS=SBWS+0.75*SFALL
      GO TO 130
C
C   ON NEW SNOW LINE
C
  110 IF((WE+LIQW).GE.SBWS) GO TO 120
      IF(SFALL.LT.SNOF) GO TO 130
      SBWS=WE+LIQW+0.75*SFALL
      GO TO 130
  120 SBWS=SBWS+0.75*SFALL
C
C
C...................................
C     Set ZERO-ONE variable switches
C...................................
  130 HZERO1=0
      IF(SFALL.GT.SFNEW) HZERO1=1
C
      KZERO1=0
      IF(TA.LT.0) KZERO1=1
C
      CALL MELT31(PXI,FRACS,RFMIN,IDN,imn,ndt,TA,AESC,SBCI,MF,MELT,NR)
C
      LZERO1=0
      IF(MELT.GE.WE+SFALL-GM*AESC) LZERO1=1
      MELT=(1-LZERO1)*MELT+LZERO1*(WE+SFALL-GM*AESC)
C
      AREALP=PXI*(1.-FRACS)*AESC
      WATER=MELT+AREALP
C
      NZERO1=0
      CNHS=MF/MFMAX*NMF*(TINDEX-HZERO1*(TINDEX-TA*KZERO1)-TA*KZERO1)*
     1 AESC
      IF(-CNHS.GT.NEGHS) NZERO1=1
      HEAT=(1-NZERO1)*CNHS-NZERO1*NEGHS-SFALL*TA*KZERO1/160.
C
      JZERO1=0
      IF(NEGHS+HEAT.LT.WATER) JZERO1=1
C
      RATIO=0.0
      IF(WE.GT.0.0) RATIO=LIQW/WE
C
      IZERO1=0
      WE1=WE+PXI*FRACS*SCF-GM*AESC-MELT+WATER+(NEGHS+HEAT-WATER)*
     1 JZERO1
      LIQWMX=PLWHC*WE1
      LIQWSP=LIQWMX+RATIO*GM*AESC-LIQW
      IF(JZERO1*(WATER-NEGHS-HEAT).LE.LIQWSP) IZERO1=1
C
      FZERO1=0
      NEGHS1=NEGHS+HEAT-WATER-JZERO1*(NEGHS+HEAT-WATER)
      IF(NEGHS1.GT.0.33*WE1) FZERO1=1
C
      GZERO1=0
      TINDX1=TINDEX-HZERO1*(TINDEX-TA)*KZERO1+TIPM*(TINDEX-TA)*
     1 (HZERO1-1)
      IF(TINDX1.GT.0.0.OR.NEGHS1.LE.0.0.OR.WE1.LE.0.0) GZERO1=1
C
      LIQW1=LIQW-IZERO1*RATIO*GM*AESC+JZERO1*IZERO1*(WATER-NEGHS-HEAT)+
     1 (1-IZERO1)*(PLWHC*WE1-LIQW)
      weliqw = WE1+LIQW1
C
C     Determine mode for modeling areal extent of snow cover
C     cases described on page 39, Report 43
C
      CALL CASE31
C
C     Compute change in states
C
      CALL F031(PXI,GM,FRACS,TA,MELT,WATER,HEAT,F0,CONST)
C
C   COMPUTE FORECAST
C
      DO 140 I=1,nnfltr
      X1(I)=X(I)+F0(I)
  140 CONTINUE
C
C     Check if forecast states are valid
C
      CALL CHKX31(X1)
C
C     Set AESC Endogenous states that are
C     not included in state space equations.
C
      CALL ENDO31(X1)
C
C   CALCULATE EXCESS 
C
      EXCESS=X(1)+X(3)-X1(1)-X1(3)+AREALP+SFALL-GMRO
      IF(EXCESS.LT.0.0) EXCESS=0.0
C
  300 CALL SWST31(X1,0)
C
C
      IF(ITRACE.GE.1) WRITE(IODBUG,699)
  699 FORMAT(1H0,14H** EXIT STSP31)
      RETURN
      END
