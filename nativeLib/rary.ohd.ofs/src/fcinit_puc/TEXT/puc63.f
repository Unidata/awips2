C MEMBER PUC63
C
      SUBROUTINE PUC63(TSC)
C.......................................
C THIS SUBROUTINE PUNCHES THE INPUT CARD NEEDED
C    BY THE SET-TS OPERATION.
C.......................................
C SUBROUTINE INITIALLY WRITTEN BY. . .
C        JASON SPERFSLAGE - HRC   FEBRUARY 2001
C.......................................
      DIMENSION TSC(1)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc63.f,v $
     . $',                                                             '
     .$Id: puc63.f,v 1.1 2002/05/15 13:41:27 hank Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,16H** PUC63 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     PUNCH INPUT CARD.
      IDT=TSC(5)
      VAL=TSC(6)
      WRITE(IPU,901) (TSC(I),I=2,4),IDT,VAL
  901 FORMAT (2X,2A4,1X,A4,3X,I2,3X,E10.3)
C.......................................
      RETURN
      END
