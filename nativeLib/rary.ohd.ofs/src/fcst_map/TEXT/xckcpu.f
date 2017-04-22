C MEMBER XCKCPU
C  (from old member PPXCKCPU)
C
      SUBROUTINE XCKCPU(CHAR1,CHAR2)
C.......................................
C     THIS IS A CPU CHECK SUBROUTINE FOR THE MAP FUNCTION.
C.......................................
C     INITIALLY WRITTEN BY -- ERIC ANDERSON,HRL   NOVEMBER 1983
C.......................................
      DIMENSION CHAR1(2),CHAR2(2)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XCPU/ICKCPU,ICPU,KCPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xckcpu.f,v $
     . $',                                                             '
     .$Id: xckcpu.f,v 1.1 1995/09/17 18:59:26 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
      CALL URTIMR(LAPSE,KCPU)
      ELAPSE=LAPSE/100.
      TCPU=KCPU/100.
      WRITE(IOPDBG,900) CHAR1,CHAR2,ELAPSE,TCPU
  900 FORMAT(1H0,20X,9HCPU CHECK,1X,2A4,1X,2A4,1X,13HELAPSED TIME=,F6.2,
     1  1X,4HSEC.,3X,11HTOTAL TIME=,F7.2,1X,4HSEC.)
C.......................................
      RETURN
      END
