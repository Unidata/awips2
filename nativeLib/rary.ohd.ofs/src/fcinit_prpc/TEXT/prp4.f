C MEMBER PRP4
C  (from old member FCPRP4)
C
      SUBROUTINE PRP4(TSC)
C.......................................
C     THIS SUBROUTINE PRINTS THE INFORMATION ASSOCIATED
C        WITH THE CLEAR OPERATION.
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp4.f,v $
     . $',                                                             '
     .$Id: prp4.f,v 1.1 1995/09/17 18:50:18 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,15H** PRP4 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     PRINT INFORMATION ON TIME SERIES THAT IS TO
C        BE CLEARED.
      IDT=TSC(6)
      WRITE (IPR,901) (TSC(I),I=3,5),IDT
  901 FORMAT (1H0,10X,52HTHE FOLLOWING TIME SERIES IS SET TO ALL ZERO VA
     1LUES.,//16X,5HI.D.=,2A4,3X,5HTYPE=,A4,3X,14HTIME INTERVAL=,I2,1X,
     25HHOURS)
C.......................................
      RETURN
      END
