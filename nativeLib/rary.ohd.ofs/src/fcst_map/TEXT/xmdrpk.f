C MEMBER XMDRPK
C  (from old member PPXMDRPK)
C
      SUBROUTINE XMDRPK(MDR,LDFILL,NMDR)
C.......................................
C     SUBROUTINE PACKS THE MDR DATA ARRAY TO REMOVE UNUSED SPACE.
C.......................................
C     WRITTEN BY--ERIC ANDERSON, HRL--FEBRUARY 1983
C.......................................
      INTEGER*2 MDR(1)
C
C     COMMON BLOCK
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xmdrpk.f,v $
     . $',                                                             '
     .$Id: xmdrpk.f,v 1.1 1995/09/17 18:59:49 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.1) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XMDRPK ENTERED)
C.......................................
C     PACK MDR ARRAY
      NPDT=LDFILL/4
      K=NMDR+1
      DO 100 I=2,4
      L=(I-1)*NPDT
      DO 105 J=1,NMDR
      MDR(K)=MDR(L+J)
      K=K+1
 105  CONTINUE
 100  CONTINUE
C.......................................
C     EXIT
      IF(IPTRCE.GE.1) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XMDRPK)
      RETURN
      END
