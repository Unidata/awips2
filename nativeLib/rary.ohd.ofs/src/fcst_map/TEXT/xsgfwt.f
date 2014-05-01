C MEMBER XSGFWT
C  (from old member PPXSGFWT)
C
      SUBROUTINE XSGFWT(PP24,PT24,CHAR,NWT,PTEST,WTEST,LCHAR,VAL)
C.......................................
C     THIS SOUBROUTINE ESTIMATES A MISSING 24 HOUR VALUE USING
C      SIGNIFICANCE WEIGHTS.
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C.......................................
      INTEGER*2 PP24(1),PT24(1),CHAR(1),VAL
      INTEGER*2 MSNG24,MSNG6,MSGMDR,MSNGSR
      DIMENSION PTEST(10),WTEST(10)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1 MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xsgfwt.f,v $
     . $',                                                             '
     .$Id: xsgfwt.f,v 1.1 1995/09/17 19:00:11 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XSGFWT ENTERED)
C.......................................
C     INITIAL VALUES
      WSUM=0.0
      TOP=0.0
      CEST=1.0
      IF(LCHAR.GT.0) CEST=CHAR(LCHAR)*0.01
C.......................................
C     LOOP THROUGH ALL ESTIMATORS
      DO 100 N=1,NWT
      LPT=PTEST(N)
      LPX=(LPT-1)/5+1
      IF((PP24(LPX).EQ.MSNG24).OR.(PP24(LPX).LT.0)) GO TO 100
      PX=PP24(LPX)*0.01
      WT=WTEST(N)
      IF(LCHAR.EQ.0) GO TO 101
      LCH=PT24(LPT+1)
      IF (LCH.GE.10000) LCH=LCH-10000
      IF(LCH.GT.0) GO TO 102
 101  CH=CEST
      GO TO 105
 102  CH=CHAR(LCH)*0.01
 105  WSUM=WSUM+WT
      TOP=TOP+PX*WT*CEST/CH
 100  CONTINUE
C.......................................
C     COMPUTE PRECIP. ESTIMATE
      IF(WSUM.GE.5E-8) GO TO 110
      VAL=0
      GO TO 99
 110  PX24=TOP/WSUM
      VAL=PX24*100.0+0.5
C     ESTIMATE DETERMINED.
C.......................................
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1HO,14H** EXIT XSGFWT)
      RETURN
      END
