C MEMBER PRP17
C  (from old member MCPRP17)
C
      SUBROUTINE PRP17(PO)
C.......................................
C     THIS IS THE PRINT PARAMETER SUBROUTINE FOR THE 'WY-PLOT '
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     APRIL 1980
C.......................................
      DIMENSION PO(1)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fwyds'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/prp17.f,v $
     . $',                                                             '
     .$Id: prp17.f,v 1.2 1996/07/11 19:36:27 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** PRP17 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     PRINT NAME OF FLOW-POINT.
      WRITE (IPR,901) (PO(I),I=1,5)
  901 FORMAT (1H0,10X,25HPLOT MEAN DAILY FLOWS FOR, 1X,5A4)
C
C     PRINT AREA AND MAX. PLOT ORDINATE.
      WRITE (IPR,902) PO(8)
  902 FORMAT (1H0,15X,22HAREA ABOVE FLOW-POINT=,F9.1,1X,3HKM2)
      IPLOT=PO(7)
      IF (IPLOT.GT.1) GO TO 101
      WRITE (IPR,903)
  903 FORMAT (1H0,15X,22HPLOT SCALE IS SEMI-LOG)
      GO TO 100
  101 IF (IPLOT.GT.2) GO TO 102
      WRITE (IPR,904) PO(9)
  904 FORMAT (1H0,15X,24HPLOT SCALE IS ARITHMETIC, 5X,14HMAX. ORDINATE=,
     1F6.0,1X,4HCMSD)
      GO TO 100
  102 WRITE (IPR,905) PO(9)
  905 FORMAT (1H0,15X,33HPLOT SCALE IS MODIFIED ARITHMETIC,5X,
     114HMAX. ORDINATE=,F4.0,1X,14H(CMSD/KM2)*100)
C.......................................
C     PRINT TIME SERIES PLOTTED.
  100 NPLOT=PO(6)
      WRITE (IPR,906) NPLOT
  906 FORMAT (1H0,10X,I2,1X,34HDAILY FLOW TIME SERIES ARE PLOTTED,
     1//16X,9HT.S. I.D.,3X,9HDATE TYPE,7X,4HNAME,5X,11HPLOT SYMBOL)
      DO 105 N=1,NPLOT
      I1=15+(N-1)*7
      I2=I1+6
      WRITE (IPR,907) (PO(I),I=I1,I2)
  907 FORMAT (1H ,15X,2A4,6X,A4,6X,3A4,6X,A1)
  105 CONTINUE
C.......................................
C     PRINT RAIN+MELT, RUNOFF, AND SOIL MOISTURE TIME SERIES TABULATED.
      LPX=PO(10)
      LRO=PO(11)
      LSM=PO(12)
      I=LPX+LRO+LSM
      IF (I.EQ.0) GO TO 120
      WRITE (IPR,908)
  908 FORMAT (1H0,10X,68HDAILY TOTALS OF THE FOLLOWING TIME SERIES ARE T
     1ABULATED ON THE PLOT.,//16X,9HT.S. I.D.,5X,9HDATA TYPE,5X,13HTIME 
     1INTERVAL)
      IF (LPX.EQ.0) GO TO 110
      IDT=PO(LPX+3)
      WRITE (IPR,909) PO(LPX),PO(LPX+1),PO(LPX+2),IDT
  909 FORMAT (1H ,15X,2A4,8X,A4,10X,I2,1X,5HHOURS)
  110 IF (LRO.EQ.0) GO TO 111
      IDT=24
      WRITE (IPR,909) PO(LRO),PO(LRO+1),PO(LRO+2),IDT
  111 IF (LSM.EQ.0) GO TO 120
      IDT=24
      WRITE (IPR,909) PO(LSM),PO(LSM+1),PO(LSM+2),IDT
C.......................................
C     PRINT INFORMATION ABOUT SCRATCH FILE.
  120 LWY=PO(13)
      NPMO=PO(14)
      LRWY=LWY+(NPMO*12)-1
      WRITE(IPR,910) LWY,LRWY,IRWY
  910 FORMAT(1H0,10X,22HOPERATION USES RECORDS,I5,1X,4HTHRU,I5,1X,
     115HON SCRATCH FILE,I3,1H.)
C.......................................
      RETURN
      END
