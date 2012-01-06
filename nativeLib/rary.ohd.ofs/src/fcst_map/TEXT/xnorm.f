C MEMBER XNORM
C  (from old member PPXNORM)
C
      SUBROUTINE XNORM(PP6,PSMALL,N6,NORM,ISTAT)
C.......................................
C     SUBROUTINE COMPUTES NORMALIZED VALUES FROM 6 HOUR
C      VALUES IF POSSIBLE.
C      STATUS=0, NORMALIZED VALUES COMPUTED
C            =1, NOT ENOUGH PRECIP. TO COMPUTE NORMALIZED
C                      VALUES
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C.......................................
      INTEGER*2 PP6(4),NORM(4)
      DIMENSION PP(4)
C
C     COMMON BLOCK
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xnorm.f,v $
     . $',                                                             '
     .$Id: xnorm.f,v 1.1 1995/09/17 18:59:58 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,16H** XNORM ENTERED)
C.......................................
C     COMPUTE TOTAL AND PUT 6 HOUR VALUES IN REAL ARRAY.
      PPTOT=0.0
      DO 100 K=1,N6
      PP(K)=PP6(K)*0.01
 100  PPTOT=PPTOT+PP(K)
C.......................................
C     CHECK IF PRECIP. IS TO SMALL TO NORMALIZE.
      IF(PPTOT.GE.PSMALL) GO TO 110
      ISTAT=1
      GO TO 99
C.......................................
C     COMPUTE NORMALIZED VALUE
  110 NORMT=0
      DO 115 K=1,N6
      NORM(K)=(PP(K)/PPTOT)*100.0+0.5
  115 NORMT=NORMT+NORM(K)
      IRES=NORMT-100
      IF (IRES.EQ.0) GO TO 125
      DO 120 K=1,N6
      IF (NORM(K).LE.IABS(IRES)) GO TO 120
      NORM(K)=NORM(K)-IRES
      GO TO 125
 120  CONTINUE
  125 ISTAT=0
C.......................................
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1H0,13H** EXIT XNORM)
      RETURN
      END
