C MEMBER PRP16
C  (from old member MCPRP16)
C
      SUBROUTINE PRP16(PO)
C.......................................................................
C     THIS SUBROUTINE PRINTS PARAMETER VALUES FOR
C     THE QME STATISTICS OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
      DIMENSION PO(1)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/prp16.f,v $
     . $',                                                             '
     .$Id: prp16.f,v 1.2 1996/07/11 19:36:10 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PRP16 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C.......................................................................
C     PRINT TITLE
      WRITE(IPR,902) (PO(I),I=2,6)
  902 FORMAT(1H0,10X,25HSTATISTICS OPERATION FOR ,5A4)
C     PRINT TIME SERIES INFO.
      WRITE(IPR,904)
  904 FORMAT(1H0,20X,35HTIME SERIES USED BY THIS OPERATION.)
      WRITE(IPR,906)
  906 FORMAT(1H0,15X,8HCONTENTS,14X,4HI.D.,7X,4HTYPE,5X,13HTIME INTERVAL
     1)
      IDTQS=PO(11)
      IDTQO=PO(15)
      WRITE(IPR,908) PO(8),PO(9),PO(10),IDTQS
  908 FORMAT(1H0,10X,19HSIMULATED DISCHARGE,6X,2A4,5X,A4,7X,I2,1X,5HHOUR
     1S)
      WRITE(IPR,910) PO(12),PO(13),PO(14),IDTQO
  910 FORMAT(1H ,10X,18HOBSERVED DISCHARGE,7X,2A4,5X,A4,7X,I2,1X,5HHOURS
     1)
      IYR=PO(16)
      IQR=PO(17)
      IFR=PO(18)
      IF(IYR.LT.1) GO TO 110
C     PRINT STATISTICS OPTIONS
      WRITE(IPR,912)
  912 FORMAT(1H0,10X,49HMULTIYEAR AND YEARLY STATISTICS WILL BE COMPUTED
     1.)
      GO TO 112
  110 WRITE(IPR,914)
  914 FORMAT(1H0,10X,38HMULTIYEAR STATISTICS WILL BE COMPUTED.)
  112 IF(IQR.EQ.0.AND.IFR.EQ.0.AND.PO(19).LT.0.1) GO TO 124
      WRITE(IPR,916)
  916 FORMAT(1H0,10X,37HOPTIONAL STATISTICAL OUTPUT SELECTED:)
      INUM=1
      IF(IQR.EQ.0) GO TO 120
      WRITE(IPR,918)INUM
  918 FORMAT(1H0,13X,I1,36H.  QUARTERLY FLOW ACCUMULATION TABLE)
      INUM=INUM+1
  120 IF(IFR.EQ.0) GO TO 122
      WRITE(IPR,920) INUM
  920 FORMAT(1H0,13X,I1,30H.  CUMMULATIVE FREQUENCY TABLE)
      INUM=INUM+1
  122 IF(PO(19).LT.0.1) GO TO 124
      WRITE(IPR,922) INUM,PO(19)
  922 FORMAT(1H0,13X,I1,55H.  DISCHARGE EXCEEDENCE PLOT, WITH MAXIMUM DI
     1SCHARGE OF,F10.0,4H CMS)
C.......................................................................
  124 RETURN
      END
