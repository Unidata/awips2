C MEMBER PUC13
C  (from old member FCPUC13)
C
      SUBROUTINE PUC13(P,C)
C
C     THIS IS THE PUNCH CARD SUBROUTINE FOR THE
C     TATUM COEFFICIENT ROUTING OPERATION
C
C     THIS SUBROUTINE INITIALLY WRITTEN BY
C          DAVID B. REED--HRL  DEC 1979
C
C
      DIMENSION P(1),C(1)
C
C     COMMON BLOCKS
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc13.f,v $
     . $',                                                             '
     .$Id: puc13.f,v 1.1 1995/09/17 18:50:36 dws Exp $
     . $' /
C    ===================================================================
C
C
C     CHECK TRACE LEVEL-FOR THIS SUBROUTINE =1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PUC13 ENTERED)
C
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C
      ICVAL=0
      IDTQI=P(10)
      IDTQO=P(14)
      NL=P(16)
      NLM1=NL-1
      NQST=17+NL
      NCST=NQST+NLM1
      IF(IPDFLT.EQ.0)ICVAL=1
      IF(IDTQO.GT.0)GO TO 100
      WRITE(IPU,901)(P(I),I=2,9),IDTQI,NL,ICVAL
  901 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,22X,I3,4X,I1)
      GO TO 101
  100 WRITE(IPU,902)(P(I),I=2,9),IDTQI,(P(J),J=11,13),IDTQO,NL,ICVAL
  902 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,2X,I3,4X,I1)
  101 WRITE(IPU,903)(P(16+I),I=1,NL)
  903 FORMAT(7F10.3)
      IF(NL.EQ.1)GO TO 102
      WRITE(IPU,903)(P(NQST+I-1),I=1,NLM1)
  102 IPTR=NCST
      DO 103 I=1,NL
      NCL=P(16+I)
      WRITE(IPU,903)(P(IPTR+J-1),J=1,NCL)
  103 IPTR=IPTR+NCL
      IPTR=1
      IF(ICVAL.EQ.0)GO TO 104
      DO 105 I=1,NL
      NCL=P(16+I)
      NCLM=NCL-1
      IF(NCL.EQ.1)GO TO 105
      WRITE(IPU,903)(C(IPTR+J-1),J=1,NCLM)
  105 IPTR=IPTR+NCLM
  104 RETURN
      END
