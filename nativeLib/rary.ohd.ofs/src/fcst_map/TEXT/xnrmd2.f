C MEMBER XNRMD2
C  (from old member PPXNRMD2)
C
      SUBROUTINE XNRMD2(PPVR,PTVR,MSNG6,PTEST,WTEST,N6,NORM,INM)
C.......................................
C     THIS SUBROUTINE ESTIMATES NORMALIZED PRECIPITATION VALUES
C         BASED ON SURROUNDING STATIONS.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- FEBRUARY 1983
C.......................................
      INTEGER*2 PPVR(1),PTVR(1),MSNG6,NORM(4)
      DIMENSION PTEST(3,4),WTEST(3,4),PN(4,4),W(4)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xnrmd2.f,v $
     . $',                                                             '
     .$Id: xnrmd2.f,v 1.1 1995/09/17 18:59:59 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** XNRMD2 ENTERED)
C.......................................
C     SET INITIAL VALUES
      DO 100 N=1,4
      DO 101 K=1,N6
  101 PN(K,N)=0.0
  100 W(N)=0.0
      WSUM=0.0
C.......................................
C     CHECK EACH QUADRANT FOR VALID ESTIMATOR
      DO 110 N=1,4
      DO 115 I=1,3
      LPT=PTEST(I,N)
      IF(LPT.EQ.0) GO TO 110
      LPX=PTVR(LPT+3)
      IF((PTVR(LPT).LT.0).OR.(PPVR(LPX).LT.0)) GO TO 115
      W(N)=WTEST(I,N)
      WSUM=WSUM+W(N)
      DO 116 K=1,N6
  116 PN(K,N)=PPVR(LPX-1+K)*0.01
      GO TO 110
  115 CONTINUE
  110 CONTINUE
C
C     CHECK IF ESTIMATOR WERE FOUND
      IF(WSUM.GE.5E-8) GO TO 125
C.......................................
C     NO ESTIMATORS -- SET TO UNIFORM DISTRIBUTION
      NDEFLT=100/N6
      DO 120 K=1,N6
  120 NORM(K)=NDEFLT
      INM=3
      IF(N6.NE.3) GO TO 99
      NORM(N6)=34
      GO TO 99
C.......................................
C     COMPUTE ESTIMATE
C
  125 NORMT=0
      DO 130 K=1,N6
      TOP=0.0
      DO 135 N=1,4
  135 TOP=TOP+PN(K,N)*W(N)
      NORM(K)=(TOP/WSUM)*100.0+0.5
      NORMT=NORMT+NORM(K)
  130 CONTINUE
C
C     MAKE SURE NORMALIZED VALUES SUM TO 100
      IRES=NORMT-100
      IF (IRES.EQ.0) GO TO 145
      DO 140 K=1,N6
      IF (NORM(K).LE.IABS(IRES)) GO TO 140
      NORM(K)=NORM(K)-IRES
      GO TO 145
  140 CONTINUE
  145 INM=1
C     ESTIMATION COMPLETE
C.......................................
   99 IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
  901 FORMAT(1H0,14H** EXIT XNRMD2)
      RETURN
      END
