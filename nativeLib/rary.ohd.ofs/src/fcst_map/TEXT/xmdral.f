C MEMBER XMDRAL
C  (from old member PPXMDRAL)
C
      SUBROUTINE XMDRAL(MDRBOX,LOC,ISTAT)
C.......................................
C     SUBROUTINE COMPUTES THE LOCATION IN THE MDR SUBSET ARRAY
C        OF A GIVEN NATIONAL GRID BOX NUMBER.
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C.......................................
C     COMMON BLOCKS
      COMMON/XMDR/MDRSTA,ICTMDR,MDRST6,ICMDR,NCMDR,IRMDR,NRMDR,
     1 NMDR,MDR,MDRNUV
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xmdral.f,v $
     . $',                                                             '
     .$Id: xmdral.f,v 1.1 1995/09/17 18:59:45 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.5) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XMDRAL ENTERED)
C.......................................
C     COMPUTE ARRAY LOCATION
      ISTAT=0
      NR=(MDRBOX-1)/113+1
      NC=MDRBOX-(NR-1)*113
      IF((NR.LT.IRMDR).OR.(NR.GE.IRMDR+NRMDR)) GO TO 90
      IF((NC.LT.ICMDR).OR.(NC.GE.ICMDR+NCMDR)) GO TO 90
      LOC=(NR-IRMDR)*NCMDR+NC-ICMDR+1
      GO TO 99
  90  ISTAT=1
C.......................................
  99  IF(IPTRCE.GE.5) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XMDRAL)
      RETURN
      END
