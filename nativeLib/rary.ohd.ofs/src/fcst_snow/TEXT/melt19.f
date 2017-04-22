C MEMBER MELT19
C  (from old member FCPACK19)
C
      SUBROUTINE MELT19(IDN,IMN,ALAT,TA,MELT,MFMAX,MFMIN,MBASE,TINDEX,
     1   TIPM,CNHS,NMF,LMFV,SMFV)
C.......................................
C     SUBROUTINES COMPUTES SURFACE MELT BASED ON 100 PERCENT
C        SNOW COVER AND NON-RAIN CONDITIONS.
C.......................................
C     INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C.......................................
      REAL MELT,MFMAX,MFMIN,MBASE,NMF,MF,NMRATE
      DIMENSION SMFV(12),MMD(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/melt19.f,v $
     . $',                                                             '
     .$Id: melt19.f,v 1.1 1995/09/17 19:19:57 dws Exp $
     . $' /
C    ===================================================================
C
      DATA MMD/301,332,361,26,56,87,117,148,179,209,240,270/
C.......................................
C     INITIAL VALUES
      CNHS=0.0
      MELT=0.0
      DIFF=MFMAX-MFMIN
      DAYN=IDN
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
  125 RATIO=MF/MFMAX
C.......................................
C     COMPUTE MELT AND NEGATIVE HEAT EXCHANGE INDEX TEMPERATURES.
      TMX=TA-MBASE
      IF(TMX.LT.0.0) TMX=0.0
      TSUR=TA
      IF (TSUR.GT.0.0) TSUR=0.0
      TNMX=TINDEX-TSUR
C.......................................
C     NEGATIVE HEAT EXCHANGE
      NMRATE=RATIO*NMF
      CNHS=NMRATE*TNMX
C
C     UPDATE TINDEX
      TINDEX=TINDEX+TIPM*(TA-TINDEX)
      IF(TINDEX.GT.0.0) TINDEX=0.0
      IF(TMX.LE.0.0) RETURN
C.......................................
C     SURFACE MELT.
      MELT=MF*TMX
C.......................................
      RETURN
      END
