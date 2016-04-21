C MEMBER XCMBNM
C  (from old member PPXCMBNM)
C
      SUBROUTINE XCMBNM(NORM6,IS6,NORM,N6,MSNG6)
C.......................................
C     SUBROUTINE COMBINES NORMALIZED VALUES BASED ON PARTIAL
C     STATION DATA WITH ESTIMATED NORMALIZED VALUES
C     FROM MDR OR SURROUNDING STATIONS.
C.......................................
C     WRITTEN BY --ERIC ANDERSON, HRL--APRIL 1984
C.......................................
      INTEGER*2 NORM6(4),NORM(4),MSNG6
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xcmbnm.f,v $
     . $',                                                             '
     .$Id: xcmbnm.f,v 1.1 1995/09/17 18:59:27 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1HO,17H** XCMBNM ENTERED)
C.......................................
C     COMPUTE TOTAL OF ESTIMATED NORMALIZED IN MISSING PERIODS.
      IRES=100-IS6
      MSGTOT=0
      NUMMSG=0
      DO 100 K=1,N6
      IF (NORM6(K).NE.MSNG6) GO TO 100
      NUMMSG=NUMMSG+1
      MSGTOT=MSGTOT+NORM(K)
 100  CONTINUE
      IF (MSGTOT.GT.0) GO TO 120
C.......................................
C     EVENLY DISTRIBUTE PARTIAL RESIDUAL OVER MISSING PERIODS.
      INC=IRES/NUMMSG
      DO 110 K=1,N6
      IF (NORM6(K).NE.MSNG6) GO TO 110
      NORM6(K)=INC
      IS6=IS6+INC
 110  CONTINUE
      GO TO 140
C.......................................
C     OBTAIN MISSING PERIODS FROM ESTIMATED NORMALIZED VALUES.
  120 RES=IRES
      TOT=MSGTOT
      DO 130 K=1,N6
      IF(NORM6(K).NE.MSNG6) GO TO 130
      PART=NORM(K)
      NORM6(K)=((PART/TOT)*RES)+0.5
      IS6=IS6+NORM6(K)
 130  CONTINUE
C.......................................
C     MAKE SURE NORMALIZED SUMS TO 100
 140  IRES=IS6-100
      IF (IRES.EQ.0) GO TO 150
      DO 145 K=1,N6
      IF (NORM6(K).LE.IABS(IRES)) GO TO 145
      NORM6(K)=NORM6(K)-IRES
      GO TO 150
 145  CONTINUE
C.......................................
C     TRANSFER NORM6 TO NORM.
 150  DO 160 K=1,N6
 160  NORM(K)=NORM6(K)
C.......................................
      IF (IPTRCE.GE.3) WRITE (IOPDBG,901)
 901  FORMAT(1HO,14H** EXIT XCMBNM)
      RETURN
      END
