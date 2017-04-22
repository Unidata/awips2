C MEMBER PRP6
C  (from old member FCPRP6)
C
      SUBROUTINE PRP6(PO)
C.......................................................................
C     THIS SUBROUTINE PRINTS PARAMETER VALUES FOR
C        THE MEAN DISCHARGE OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   SEPTEMBER 1979  VERSION 1
C.......................................................................
      DIMENSION PO(1)
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp6.f,v $
     . $',                                                             '
     .$Id: prp6.f,v 1.1 1995/09/17 18:50:24 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** PRP6 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C.......................................................................
C     PRINT TITLE
      WRITE(IPR,903)
  903 FORMAT(1H0,10X,24HMEAN DISCHARGE OPERATION)
      IDTQ=PO(5)
      WRITE(IPR,904) IDTQ
  904 FORMAT(1H0,15X,30HCOMPUTATIONAL TIME INTERVAL IS,I3,1X,6HHOURS.)
C.......................................................................
C     PRINT TIME SERIES INFORMATION.
      WRITE(IPR,905)
  905 FORMAT(1H0,20X,35HTIME SERIES USED BY THIS OPERATION.)
      WRITE(IPR,906)
  906 FORMAT(1H0,15X,8HCONTENTS,14X,4HI.D.,7X,4HTYPE,5X,13HTIME INTERVAL
     1)
      WRITE(IPR,908) PO(2),PO(3),PO(4),IDTQ
  908 FORMAT(1H0,10X,23HINSTANTANEOUS DISCHARGE,2X,2A4,5X,A4,7X,I2,1X,
     15HHOURS)
      IDTQB=PO(9)
      WRITE(IPR,910) PO(6),PO(7),PO(8),IDTQB
  910 FORMAT(1H ,10X,14HMEAN DISCHARGE,11X,2A4,5X,A4,7X,I2,1X,5HHOURS)
C.......................................................................
C     PRINT TIME PERIOD
      WRITE(IPR,912) IDTQB
  912 FORMAT(1H0,10X,32HMEAN DISCHARGES ARE COMPUTED FOR,I3,19H HOUR TIM
     1E PERIODS.)
C.......................................................................
      RETURN
      END
