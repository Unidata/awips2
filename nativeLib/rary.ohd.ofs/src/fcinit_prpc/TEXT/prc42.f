C MEMBER PRC42
C  (from old member FCPRC42)
C
      SUBROUTINE PRC42(CO)
C.......................................
C    CARRYOVER PRINT SUBROUTINE FOR THE 'RSNWELEV' OPERATION
C.......................................
C    INITAILLY WRITTEN BY ERIC ANDERSON - HRL   DEC 1991
C.......................................
      DIMENSION CO(1)
C    COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc42.f,v $
     . $',                                                             '
     .$Id: prc42.f,v 1.1 1995/09/17 18:49:47 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C    TRACE LEVEL=1, NO DEBUG
      IF(ITRACE.GE.1.) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PRC ENTERED')
C.......................................
C    PRINT CARRYOVER
      WRITE(IPR,901) CO(1)
  901 FORMAT(1H0,10X,'PREVIOUS FREEZING LEVEL=',F6.0,1X,'M')
C.......................................
      IF (ITRACE.GE.1) WRITE(IODBUG,902)
  902 FORMAT(1H0,'** EXIT PRC42')
      RETURN
      END
