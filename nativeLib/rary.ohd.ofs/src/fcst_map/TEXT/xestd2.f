C MEMBER XESTD2
C  (from old member PPXESTD2)
C
      SUBROUTINE XESTD2(PP24,PT24,CHAR,PTEST,WTEST,PPSR,NSSR,
     1  PX,PY,LCHAR,VAL)
C.......................................
C     THIS SUBROUTINE ESTIMATES A MISSING 24 HOUR VALUE USING
C      ONE OVER DISTANCE SQUARE WEIGHTS.
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C.......................................
      INTEGER*2 PP24(1),PT24(1),CHAR(1),PPSR(1),VAL
      INTEGER*2 MSNG24,MSNG6,MSGMDR,MSNGSR
      DIMENSION PP(4),W(4),C(4),PTEST(5,4),WTEST(5,4)
C
C     COMMON BLOCK
C
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XOPT/IXTYPE,XNAME(2),IESTPX,ICONVC,CNVDIS,
     1 CVDISW,NOEST6,IUSESR,PPUSER(2),IMOSUM,IMOWTR,IWTEST
C
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1    MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xestd2.f,v $
     . $',                                                             '
     .$Id: xestd2.f,v 1.1 1995/09/17 18:59:36 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XESTD2 ENTERED)
C     FIND ESTIMATORS IN EACH QUADRANT
C.......................................
C       SET INITIAL VALUES
      CEST=1.0
      IF(LCHAR.GT.0) CEST=CHAR(LCHAR)*0.01
      DO 100 N=1,4
      PP(N)=0.0
      W(N)=0.0
 100  C(N)=1.0
C.......................................
C     CHECK EACH QUADRANT FOR ESTIMATOR
      DO 110 N=1,4
      J=0
      DO 115 I=1,5
      LPT=PTEST(I,N)
      IF(LPT.EQ.0) GO TO 114
      IF(ICONVC.EQ.0) GO TO 116
      IF (WTEST(I,N).GE.CVDISW) GO TO 116
      IF (IESTPX.EQ.1) W(N)=CVDISW
      GO TO 110
  116 LPX=(LPT-1)/5+1
      J=I
      IF((PP24(LPX).EQ.MSNG24).OR.(PP24(LPX).LT.0)) GO TO 115
      W(N)=WTEST(I,N)
      PP(N)=PP24(LPX)*0.01
      IF(LCHAR.EQ.0) GO TO 110
      LCH=PT24(LPT+1)
      IF (LCH.GE.10000) LCH=LCH-10000
      IF(LCH.GT.0) GO TO 117
      C(N)=CEST
      GO TO 110
 117  C(N)=CHAR(LCH)*0.01
      GO TO 110
 115  CONTINUE
C
C     NO ESTIMTOR IN THE QUADRANT
 114  IF(IESTPX.EQ.0) GO TO 110
C
C     TULSA PROCEDURE
      IF(J.EQ.0) GO TO 113
      IF(J.GT.2) J=2
      W(N)=WTEST(J,N)
      GO TO 110
  113 IF (ICONVC.EQ.1) W(N)=CVDISW
 110  CONTINUE
C.......................................
C     STATION WITH CHARACTERISTICS CANNOT USE STRANGER REPORTS.
       IF(LCHAR.GT.0) GO TO 130
C
C     CHECK FOR STRANGER STATION ESTIMATORS
      IF((IUSESR.EQ.0).OR.(NSSR.EQ.0)) GO TO 130
      DO 120 J=1,NSSR
      I=(J-1)*3+1
      IF(PPSR(I+2).EQ.MSNGSR) GO TO 120
      EX=PPSR(I)
      EY=PPSR(I+1)
      DELX=EX-PX
      DELY=EY-PY
      D=SQRT(DELX*DELX+DELY*DELY)
      IF(D.LT.0.01) D=0.01
      WT=1.0/(D*D)
      IF(DELX) 50,51,52
  50  IF(DELY) 30,30,20
  51  IF(DELY) 40,10,20
  52  IF(DELY) 40,10,10
  10  N=1
      GO TO 121
  20  N=2
      GO TO 121
  30  N=3
      GO TO 121
  40  N=4
 121  IF(WT.LE.W(N)) GO TO 120
      IF(ICONVC.EQ.0) GO TO 122
      IF(WT.LT.CVDISW) GO TO 120
 122  W(N)=WT
      PP(N)=PPSR(I+2)*0.01
      C(N)=CEST
 120  CONTINUE
C.......................................
C     DETERMINE THE SUM OF WEIGHTS
 130  WSUM=0.0
      DO 131 N=1,4
 131  WSUM=WSUM+W(N)
C.......................................
C     COMPUTE PRECIP. ESTIMATE
      IF(WSUM.GE.5E-8) GO TO 135
      VAL=0
      GO TO 99
 135  TOP=0.0
      DO 136 N=1,4
 136  TOP=TOP+PP(N)*W(N)*CEST/C(N)
      PX24=TOP/WSUM
      VAL=PX24*100.0+0.5
C     ESTIMATED DETERMINED.
C.......................................
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XESTD2)
      RETURN
      END
