C MEMBER FRZE24
C  (from old member FCAPIC24)
C
      SUBROUTINE FRZE24(TA,LWE,WE,LSC,AESC,FINDX,FEI,AIR,DT,IBUG,IOUT)
C.......................................
C     THIS SUBROUTINE COMPUTES THE CHANGE IN THE FROZEN GROUND
C        INDEX AND THE FROST EFFICIENCY INDEX.
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL   JUNE 1980
C       MODIFIED FOR THE API-CONT OPERATION -- APRIL 1990
C.......................................
C
C     COMMON BLOCKS
      COMMON/FIPM24/CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA,ITTA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/frze24.f,v $
     . $',                                                             '
     .$Id: frze24.f,v 1.1 1995/09/17 18:58:11 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     INITIAL VALUES
      FINDX1=FINDX
      CSOILT=4.0*DT*CSOIL
      CTT=4.0*DT*CT
      GHCT=GHC*DT
C.......................................
C     COMPUTE CHANGE IN THE FROST INDEX
  120 IF ((FINDX.GE.32.0).AND.(TA.GE.32.0)) GO TO 140
C
C     COMPUTE TRANSFER COEFFIENT.
      COVER=0.0
      IF (LWE.EQ.0) GO TO 124
      IF (LSC.GT.0) GO TO 121
      IF (WE.LE.0.1) GO TO 124
      COVER=1.0
      GO TO 122
  121 COVER=AESC
      IF (COVER.EQ.0.0) GO TO 124
  122 TWE=WE/COVER
      C=CSOILT*(1.0-COVER)+CSOILT*((1.0-CSNOW)**TWE)*COVER
      GO TO 125
  124 C=CSOILT
C
C     COMPUTE CHANGE IN FROST INDEX.
  125 IF (TA.GE.32.0) GO TO 126
      SQUARE=(TA-32.0)*(TA-32.0)+(FINDX-32.0)*(FINDX-32.0)
      CFI=-C*SQRT(SQUARE)-C*(FINDX-32.0)+GHCT
      FINDX=FINDX+CFI
      GO TO 140
  126 FINDX=FINDX+C*(TA-32.0)+GHCT
C.......................................
C     CHECK FROST INDEX
  140 IF (FINDX.GT.32.0) FINDX=32.0
      IF (FINDX.LT.-450.0) FINDX=-450.0
C.....................................
C     COMPUTE FROST EFFICIENCY INDEX CHANGES DUE TO TEMPERATURE
      IF (FINDX.GE.FICR) GO TO 170
      IF (FINDX.GT.FINDX1) GO TO 160
C
C     FREEZING
      CFT=CF*((1.0-AIR)**2.0)
      FEI=FEI+(1.0-FEI)*(CFT*(FINDX1-FINDX))
      GO TO 180
C
C     THAWING
  160 IF (TA.LE.32.0) GO TO 180
      IF (COVER.EQ.0.0) GO TO 161
      CTHAW=CTT*(1.0-COVER)+CTT*COVER*((1.0-CSNOW)**TWE)
      GO TO 165
  161 CTHAW=CTT
  165 FEI=FEI-CTHAW*(TA-32.0)
      GO TO 180
C
C     FROST INDEX ABOVE FICR
  170 FEI=0.0
      GO TO 190
C
C     CHECK FEI
  180 IF (FEI.GT.1.0) FEI=1.0
      IF (FEI.LT.0.0) FEI=0.0
C.....................................
C     CHECK FOR DEBUG OUTPUT
  190 IF (FINDX.LT.32.0) GO TO 195
      GO TO 199
C.......................................
C     DEBUG OUTPUT
  195 IF (IBUG.EQ.0) GO TO 199
      WRITE (IOUT,900) FINDX1,FINDX,C,COVER,TA
  900 FORMAT(1H ,5X,21HFROST INDEX--INITIAL=,F6.1,3X,6HFINAL=,F6.1,5X,
     15HCOEF=,F6.3,3X,6HCOVER=,F6.3,3X,3HTA=,F6.1)
      WRITE (IOUT,901) FEI,TWE,AIR,CTHAW
  901 FORMAT(1H ,22X,'FEI=',F6.4,6X,'TWE=',F6.2,6X,'AIR=',F6.3,
     1 6X,'CTHAW=',F6.4)
C.......................................
  199 RETURN
      END
