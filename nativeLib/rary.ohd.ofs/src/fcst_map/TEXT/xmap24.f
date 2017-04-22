C MEMBER XMAP24
C  (from old member PPXMAP24)
C
      SUBROUTINE XMAP24(NSTA,DATA,WTS,TMAP)
C.......................................
C     THIS SUBROUTINE COMPUTES A 24 HOUR MAP VALUE FROM STATION
C       24 HOUR DATA AND PRECOMPUTED WEIGHTS
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--MARCH 1983
C.......................................
      INTEGER*2 DATA(1)
      DIMENSION WTS(1)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xmap24.f,v $
     . $',                                                             '
     .$Id: xmap24.f,v 1.1 1995/09/17 18:59:42 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XMAP24 ENTERED)
C.......................................
C     COMPUTE 24 HOUR MAP
      TMAP=0.0
      DO 100 N=1,NSTA
      PP=DATA(N)*0.01
 100  TMAP=TMAP+PP*WTS(N)
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XMAP24)
      RETURN
      END
