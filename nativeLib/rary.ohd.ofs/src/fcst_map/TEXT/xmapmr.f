C MEMBER XMAPMR
C  (from old member PPXMAPMR)
C
      SUBROUTINE XMAPMR(NP,IAMT,DATA,NSKIP,NMR,TMAP,TSMAP)
C.......................................
C     THIS SUBROUTINE COMPUTES MAP BASED ON MDR DERIVED
C      PRECIPITATION AMOUNTS.
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--MARCH 1983
C.......................................
      INTEGER*2 DATA(1)
      DIMENSION TSMAP(1),PP6(4)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xmapmr.f,v $
     . $',                                                             '
     .$Id: xmapmr.f,v 1.1 1995/09/17 18:59:44 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XMAPMR ENTERED)
C.......................................
C     COMPUTE 6 HR MAP FROM MDR VALUES AND 24 HOUR TOTAL
      PP24=0.0
      DO 100 I=1,NP
      PP6(I)=0.0
      DO 110 N=1,NMR
      J=(N-1)*NP+I
      PP=DATA(NSKIP+J)*0.01
 110  PP6(I)=PP6(I)+PP
      PP6(I)=PP6(I)/NMR
 100  PP24=PP24+PP6(I)
      IF(IAMT.EQ.0) GO TO 120
C.......................................
C     MAP BASED SOLEY ON MDR DATA
      TMAP=PP24
      DO 115 I=1,NP
 115  TSMAP(I)=PP6(I)
      GO TO 99
C.......................................
C     USE MDR TO NORMALIZE GIVEN 24 HOUR MAP
  120 IF (PP24.GT.0.0) GO TO 130
C
C     MDR ESTIMATE EQUALS ZERO, USE UNIFORM DISTRIBUTION
      F=1.0/NP
      DO 125 I=1,NP
 125  TSMAP(I)=TMAP*F
      GO TO 99
C
C     SOME MDR PRECIPITATION
 130  DO 135 I=1,NP
      F=PP6(I)/PP24
 135  TSMAP(I)=TMAP*F
C.......................................
C     CHECK TRACE LEVEL
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XMAPMR)
      RETURN
      END
