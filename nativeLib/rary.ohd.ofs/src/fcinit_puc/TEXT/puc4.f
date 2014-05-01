C MEMBER PUC4
C  (from old member FCPUC4)
C
      SUBROUTINE PUC4(TSC)
C.......................................
C     THIS SUBROUTINE PUNCHES THE INPUT CARD NEEDED
C        BY THE CLEAR OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     AUGUST 1979
C.......................................
      DIMENSION TSC(1)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc4.f,v $
     . $',                                                             '
     .$Id: puc4.f,v 1.1 1995/09/17 18:50:59 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,15H** PUC4 CNTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     PUNCH INPUT CARD.
      IDT=TSC(6)
      WRITE(IPU,901) (TSC(I),I=3,5),IDT
  901 FORMAT (2X,2A4,1X,A4,3X,I2)
C.......................................
      RETURN
      END
