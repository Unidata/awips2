C MEMBER PUC11
C  (from old member FCPUC11)
C
      SUBROUTINE PUC11(P,C)
C
C     THIS IS THE PUNCH CARD SUBROUTINE FOR THE
C      LAYERED COEFFICIENT ROUTING OPERATION
C
C
C     THIS SUBROUTINE INITIALLY WRITTEN BY
C           DAVID REED -HRL         OCT 1979
C
C
      DIMENSION P(1),C(1)
C
C     COMMOM BLOCKS
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc11.f,v $
     . $',                                                             '
     .$Id: puc11.f,v 1.1 1995/09/17 18:50:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C     CHECK TRACE LEVEL-FOR THIS SUBROUTINE=1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PUC11 ENTERED)
C
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C
      ICVAL=0
      IDTQI=P(10)
      IDTQO=P(14)
      NC=P(16)
      NM1=NC-1
      NM2=NCM2
      NQST=17+NC
      IF(IPDFLT.EQ.0)ICVAL=1
      IF(IDTQO.GT.0)GO TO 100
C
C     PUNCH CARD #1 IF ROUTING AT A POINT
C
      WRITE(IPU,901)(P(I),I=2,9),IDTQI,NC,ICVAL
  901 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,22X,I3,4X,I1)
      GO TO 101
  100 WRITE(IPU,902)(P(I),I=2,9),IDTQI,(P(J),J=11,13),IDTQO,NC,ICVAL
  902 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,2X,I3,4X,I1)
  101 WRITE(IPU,903)(P(16+I),I=1,NC)
  903 FORMAT(7F10.3)
      IF(NC.EQ.1)GO TO 102
      WRITE(IPU,903)(P(NQST+I-1),I=1,NM1)
  102 IF(ICVAL.EQ.0)GO TO 103
      WRITE(IPU,903)(C(I),I=1,NC)
  103 RETURN
      END
