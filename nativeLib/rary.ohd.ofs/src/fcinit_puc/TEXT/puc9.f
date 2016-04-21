C MEMBER PUC9
C  (from old member FCPUC9)
C
      SUBROUTINE PUC9(PO,CO)
C
C     THIS IS THE PUNCH SUBROUTINE FOR
C     MUSKINGUM ROUTING OPERATION
C
C     THIS SUBROUTINE INITIALLY WRITTEN BY
C          DAVID REED-HRL    OCT 1979
C
C
      DIMENSION PO(1),CO(1)
C
C     COMMON BLOCKS
C
      COMMON /PUDFLT/IPDFLT
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc9.f,v $
     . $',                                                             '
     .$Id: puc9.f,v 1.1 1995/09/17 18:51:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C     CHECK TRACE LEVEL-THIS SUBROUTINE =1
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** PUC9 ENTERED)
C
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C
C
C
      ICVAL=0
      IDT=PO(10)
      IDTQO=PO(14)
      IF(IPDFLT.EQ.0)ICVAL=1
      IF(IDTQO.GT.0) GO TO 101
C
C     PUNCH CARD #1 IF ROUTING AT APOINT
C
      WRITE(IPU,902) (PO(I),I=2,9),IDT,PO(15),PO(16),ICVAL
  902 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,20X,F5.1,F5.3,1X,I1)
      GO TO 102
C
C     PUNCH CARD #1 IF ROUTING THROUGH A REACH
C
  101 WRITE(IPU,903)(PO(I),I=2,9),IDT,(PO(J),J=11,13),IDTQO,PO(15),PO(16
     1),ICVAL
  903 FORMAT(5A4,2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,F5.1,F5.4,1X,I1)
  102 IF(ICVAL.EQ.0) GO TO 103
C
C     PUNCH CARRYOVER
C
      WRITE(IPU,904) CO(1),CO(2)
  904 FORMAT(2F10.4)
  103 RETURN
      END
