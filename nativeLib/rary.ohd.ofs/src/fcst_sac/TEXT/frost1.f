C MEMBER FROST1
C  (from old member FCFLAND1)
C
      SUBROUTINE FROST1(PX,SUR,DIR,TA,LWE,WE,ISC,AESC,DT,IBUG)
C.......................................
C     THIS SUBROUTINE COMPUTES THE CHANGE IN THE FROZEN GROUND
C        INDEX AND MOISTURE MOVEMENT DUE TO TEMPERATURE GRADIENTS.
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL   JUNE 1980
C.......................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC
C
C     COMMON BLOCKS
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
      COMMON/FSMPM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,SAVED,PAREA
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sac/RCS/frost1.f,v $
     . $',                                                             '
     .$Id: frost1.f,v 1.1 1995/09/17 18:58:07 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     INITIAL VALUES
      FINDX=FGCO(1)
      FINDX1=FINDX
      CSOIL=4.0*DT*FGPM(1)
      CSNOW=FGPM(2)
      GHC=FGPM(3)*DT
      RTHAW=FGPM(4)
C.......................................
C     COMPUTE MOISTURE MOVEMENT
C        EQUATIONS NOT READY YET.
C.......................................
C     COMPUTE CHANGE IN FROZEN GROUND INDEX.
C     CHANGE DUE TO WATER FREZING IN THE SOIL.
      IF (FINDX.GE.0.0) GO TO 120
      WATER=PX-SUR-DIR
      IF (WATER.LE.0.0) GO TO 120
      FINDX=FINDX+RTHAW*WATER
      IF (FINDX.GT.0.0) FINDX=0.0
C.......................................
C     CHANGE DUE TO TEMPERATURE.
  120 IF ((FINDX.GE.0.0).AND.(TA.GE.0.0)) GO TO 190
C
C     COMPUTE TRANSFER COEFFIENT.
      IF (LWE.EQ.0) GO TO 124
      IF (WE.EQ.0.0) GO TO 124
      IF (ISC.GT.0) GO TO 121
      COVER=1.0
      GO TO 122
  121 COVER=AESC
      IF (COVER.EQ.0.0) GO TO 124
  122 TWE=WE/COVER
      C=CSOIL*(1.0-COVER)+CSOIL*((1.0-CSNOW)**TWE)*COVER
      GO TO 125
  124 C=CSOIL
C
C     COMPUTE CHANGE IN FROST INDEX.
  125 IF (TA.GE.0.0) GO TO 126
      CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
      FINDX=FINDX+CFI
      GO TO 190
  126 FINDX=FINDX+C*TA+GHC
C.......................................
C     CHECK FROST INDEX
  190 IF (FINDX.LT.0.0) GO TO 195
      FINDX=0.0
      GO TO 199
C.......................................
C     DEBUG OUTPUT
  195 IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,900) FINDX1,FINDX,C,WATER,TA
  900 FORMAT(1H ,5X,21HFROST INDEX--INITIAL=,F6.1,3X,6HFINAL=,F6.1,5X,
     15HCOEF=,F6.3,3X,6HWATER=,F6.2,3X,3HTA=,F6.1)
C.......................................
C     SAVE NEW FROST INDEX
  199 FGCO(1)=FINDX
C.......................................
      RETURN
      END
