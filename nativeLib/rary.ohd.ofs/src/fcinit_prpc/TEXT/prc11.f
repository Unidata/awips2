C MEMBER PRC11
C  (from old member FCPRC11)
C
      SUBROUTINE PRC11(P,C)
C
C     THIS SUBROUTINE PRINTS THE CARRYOVER VALUES
C     FOR THE LAYERED COEFFICIENT ROUTING
C
C     THIS SUBROUTINE INITIALLY WRITTEN BY
C           DAVID REED--HRL     OCT 1979
C
C
      DIMENSION P(1),C(1)
C
C     COMMON BLOCKS
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/IVALUE
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc11.f,v $
     . $',                                                             '
     .$Id: prc11.f,v 1.1 1995/09/17 18:49:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C     CHECK TRACE LEVEL-- FOR THIS SUBROUTINE=1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,15H**PRC11 ENTERED)
C
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C
      NC=P(16)
      ICVAL=P(15)
C
C     PRINT CARRYOVER
C
      IF((ICVAL.EQ.0).AND.(IVALUE.EQ.1)) GO TO 101
      WRITE(IPR,901)(P(I),I=2,6)
  901 FORMAT(1H0,10X,31HLAYERED COEFFICIENT ROUTING FOR,5A4,//,
     111X,5HLAYER,10X,13HRESIDUAL FLOW)
      DO 100 I=1,NC
  100 WRITE(IPR,902)I,C(I)
  902 FORMAT(1H0,13X,I2,10X,F10.3)
      GO TO 102
  101 WRITE(IPR,903)
  903 FORMAT(1H0,10X,36HINITIAL CARRYOVER VALUES SET TO ZERO,11H BY DEFA
     1ULT)
  102 RETURN
      END
