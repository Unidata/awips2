C     Module MELT31
C
      SUBROUTINE MELT31(PXI,FRACS,RFMIN,IDN,imn,ndt,TA,AESC,SBCI,
     1                  MF,MELT,NR)
C
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This routine: 
C       1. Computes surface melt for RAIN + NON-RAIN conditions
C       2. Computes Melt factor.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR THE SNOW-17 OPERATION
C     Modified by...
C        Jay Day - RTi, July 1988 for use in the SNOW-43 operation
C        Nalneesh Gaur - RTi, Aug 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      PXI           I          Precipitation data
C      FRACS         I          fraction of Precipitation that is snow
C      RFMIN         I          Minimum Rainfall to use
C                               RAIN-ON-SNOW Melt equation
C      IDN           I          Number of days since March 21
C      imn           I          current month
C      ndt           i          Number of precip, % snowfall & rain+ melt values.
C                               computation time interval
C      TA            I          Air Temperature
C      AESC          I          Areal Extent of Snow Cover
C      SBCI          I          Stefan Boltzman's constant x precip interval.
C      MF            O          Melt Factor
C      MELT          O          Computed melt from snow pack
C      NR            O          Flag (1=NON-RAIN PERIOD/0=RAIN PERIOD)
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S --
      real    pxi,fracs,rfmin,ta,aesc,sbci
      real    melt,mf
      integer nr, idn, imn, ndt
C     --- L O C A L --
      real    diff, dayn, tmx 
      real    fmd, fnd, adjmf, fndt
      real    x, xx
      integer ma, mb, md, nd, mmd
C----- D I M E N S I O N --------------------------------
      dimension mmd(12)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/czero31'
      include 'snow43/snpm31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/melt31.f,v $
     . $',                                                             '
     .$Id: melt31.f,v 1.1 1996/05/07 11:04:30 page Exp $
     . $' /
C    ===================================================================
C
C----- D A T A  S T A T M E N T S ----------------------
      data mmd/301,332,361,26,56,87,117,148,179,209,240,270/
C.......................................
C
      MELT=0.
      DIFF=MFMAX-MFMIN
      DAYN=IDN
      fndt = ndt
      IF(LMFV.EQ.0) GOTO 100
C.......................................
C     USER SPECIFIED MELT FACTOR VARIATION
      MB=IMN
      IF (IDN.LT.MMD(IMN)) MB=MB-1
      IF (MB.EQ.0) MB=12
      MA=MB+1
      IF (MA.EQ.13) MA=1
      MD=MMD(MA)-MMD(MB)
      IF (MD.LT.0) MD=MD+366
      ND=IDN-MMD(MB)
      IF (ND.LT.0) ND=ND+366
      FMD=MD
      FND=ND
      ADJMF= SMFV(MB)+(FND/FMD)*(SMFV(MA)-SMFV(MB))
      MF= MFMIN+ADJMF*DIFF
      GOTO 125
  100 IF(ALAT.LT.54.0) GO TO 120
C.......................................
C     MELT FACTOR VARIATION FOR ALASKA.
      IF(IDN.GE.275) GO TO 102
      IF(IDN.GE.92) GO TO 101
      X=(91.0+DAYN)/183.0
      GO TO 105
  101 X=(275.0-DAYN)/(275.0-92.0)
      GO TO 105
  102 X=(DAYN-275.0)/(458.0-275.0)
  105 XX=(SIN(DAYN*2.0*3.1416/366.0)*0.5)+0.5
      IF(X.LE.0.48) GO TO 111
      IF(X.GE.0.70) GO TO 112
      ADJMF=(X-0.48)/(0.70-0.48)
      GO TO 110
  111 ADJMF=0.0
      GO TO 110
  112 ADJMF=1.0
  110 MF=(XX*ADJMF)*DIFF+MFMIN
      GO TO 125
C.......................................
C     MELT FACTOR VARIATION FOR THE LOWER 48.
  120 MF=(SIN(DAYN*2.0*3.1416/366.0)*DIFF*0.5)+(MFMAX+MFMIN)*0.5
  125 continue
C
      IF(PXI*(1-FRACS).GT.RFMIN) then
C
C     RAIN PERIOD
C
          NR=0
          MELT=SBCI*(((TA+273.)*.01)**4-55.55) +
     +    (2.1029085E9*EXP(-4278.63/(TA+242.792))-51.935)*UADJ +
     +    .004845*PA*UADJ*TA
          IF(MELT.LT.0.) MELT=0.0
      else
C
C     NON-RAIN PERIOD
C
          NR=1
          TMX=TA-MBASE
          MZERO1=0
          IF(TMX.GT.0.0) MZERO1=1
          MELT=(MF*TMX*MZERO1)
      endif
C
      MELT=MELT+.0125*PXI*(1.-FRACS)*TA*(1-KZERO1)
      MELT=MELT*AESC
C
      RETURN
      END
