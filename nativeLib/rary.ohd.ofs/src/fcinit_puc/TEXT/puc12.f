C MEMBER PUC12
C  (from old member FCPUC12)
C
      SUBROUTINE PUC12(P)
C
C     THIS IS THE PUNCH SUBROUTINE FOR THE INSTANTANEOUS DISCHARGE
C     PLOT OPERATION
C
C     THIS SUBROUTINE INITIALLY WRITTEN BY
C          DAVID REED --HRL    DEC 1979
C
C
      DIMENSION P(1),RCID(2)
C
C     COMMON BLOCKS
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc12.f,v $
     . $',                                                             '
     .$Id: puc12.f,v 1.1 1995/09/17 18:50:35 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK/4H    /
C
C     CHECK TRACE LEVEL- FOR THIS SUBROUTINE =1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PUC12 ENTERED)
C
C
      IOPTAB=P(8)
      IOPLT=P(9)
      NPLOTS=P(7)
      IRC=P(11)
      IPTR=11
C
      RCID(1)=BLANK
      RCID(2)=BLANK
      IF(IRC.EQ.0)GO TO 10
      RCID(1)=P(IRC)
      RCID(2)=P(IRC+1)
C
C     PUNCH CARD #1-GENERAL TITLE,NPLOTS,IOPLT,IOPTAB,RCID
C
   10 WRITE(IPU,901)(P(I),I=2,6),NPLOTS,IOPTAB,IOPLT,RCID
  901 FORMAT(5A4,3X,I2,4X,I1,3X,I2,2X,2A4)
      IF(IOPTAB.EQ.0)GO TO 100
      K=1
      IF(IOPTAB.EQ.3)K=2
      DO 102 L=1,K
      IPTR3=IPTR+3
      IPTR1=IPTR+1
      IPTR5=IPTR+5
      IPTR7=IPTR+7
      IDT=P(IPTR+4)
      WRITE(IPU,902)(P(I),I=IPTR1,IPTR3),IDT,(P(J),J=IPTR5,IPTR7)
  902 FORMAT(2X,2A4,1X,A4,3X,I2,8X,3A4)
      IPTR=IPTR+7
  102 CONTINUE
  100 DO 105 K=1,NPLOTS
      IDT=P(IPTR+4)
      IPTR1=IPTR+1
      IPTR3=IPTR+3
      IPTR5=IPTR+5
      IPTR8=IPTR+8
      WRITE(IPU,903)(P(I),I=IPTR1,IPTR3),IDT,(P(J),J=IPTR5,IPTR8)
      IPTR=IPTR+8
  105 CONTINUE
  903 FORMAT(2X,2A4,1X,A4,3X,I2,8X,3A4,4X,A1)
      IF(IRC.GT.0)WRITE(IPU,904)(P(IRC+1+I),I=1,3)
  904 FORMAT(3(4X,A1))
      RETURN
      END
