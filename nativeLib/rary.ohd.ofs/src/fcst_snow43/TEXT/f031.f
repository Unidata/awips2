C     Module F031
C
      SUBROUTINE F031(PXI,GM,FRACS,TA,MELT,WATER,HEAT,F0,CONST)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE COMPUTES THE CHANGE IN MODEL STATES FOR A 
C     TIME PERIOD IN THE 'SNOW-43 ' OPERATION. (SEE NWS-43, PP 38, 39)
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        JAY DAY - RTi
C        Nalneesh Gaur - RTi, July 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Precipitation.
C      GM            I          Ground melt.
C      FRACS         I          Fraction of precip that is snow.
C      TA            I          Temperature.
C      MELT          I          Melt from snow pack in the time step.
C      WATER         I          Water due to melt and precip
C      HEAT          I          Heat gained by the pack
C      F0            O          Array containing the change in model 
C                               states
C      CONST         O          (change in AESC/change in WE) this time step
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real     pxi, gm, fracs, ta, melt,
     1         water, heat, f0, const
C     --- L O C A L ---
C
      real     welqold, fn, r, dwe, ratio, aesc1
      integer  nochng, n
C
C----- D I M E N S I O N --------------------------------
      dimension f0(5)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/czero31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/f031.f,v $
     . $',                                                             '
     .$Id: f031.f,v 1.1 1996/05/07 11:01:17 page Exp $
     . $' /
C    ===================================================================
C
C......................................
C
C
C
      F0(1)=PXI*FRACS*SCF-GM*AESC-MELT+WATER+(NEGHS+HEAT-WATER)*JZERO1
C
      F0(2)=(HEAT-WATER-JZERO1*(NEGHS+HEAT-WATER))*(1-FZERO1)+
     1 FZERO1*(0.33*(WE+F0(1))-NEGHS)
C
      RATIO=0.0
      IF(WE.GT.0.0) RATIO=LIQW/WE
      F0(3)=-IZERO1*RATIO*GM*AESC+JZERO1*IZERO1*(WATER-NEGHS-HEAT)+
     1 (1-IZERO1)*(PLWHC*(WE+F0(1))-LIQW)
C
      F0(4)=(-HZERO1*(TINDEX-TA*KZERO1)+TIPM*(TINDEX-TA)*(HZERO1-1))*
     1 (1-GZERO1)-GZERO1*TINDEX
C
      welqold=WE+LIQW
      DWE=WELIQW-welqold
      CONST=0.0
      NOCHNG=0
      IF(DWE.LT.0.00001.AND.DWE.GT.-0.00001) NOCHNG=1
C
      GO TO (100,200,300,400),ICASE
C
  100 F0(5)=1.0-AESC
      GO TO 999
C
  200 IF(NOCHNG.EQ.1) GO TO 210
      R=(WELIQW/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      AESC1=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(AESC1.GT.1.0) AESC1=1.0
      CONST=(AESC1-AESC)/DWE
  210 F0(5)=CONST*DWE
      GO TO 999
C
  300 IF(NOCHNG.EQ.1) GO TO 310
      CONST=(1.0-AESC)/DWE
  310 F0(5)=CONST*DWE
      GO TO 999
C
  400 IF(NOCHNG.EQ.1) GO TO 410
      AESC1=SBAESC+(WELIQW-SB)/(SBWS-SB)*(1.-SBAESC)
      CONST=(AESC1-AESC)/DWE
  410 F0(5)=CONST*DWE
C
  999 RETURN
      END
