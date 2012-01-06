C MEMBER PRP14
C  (from old member FCPRP14)
C.......................................................................
C
      SUBROUTINE PRP14(PADJ)
C
C     SUBROUTINE PRINTS THE INFORMATION STORED IN THE P (PADJ) ARRAY
C     FOR THE ADJUST OPERATION.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE    FEBRUARY 1980
C.......................................................................
      DIMENSION PADJ(1),PR14(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp14.f,v $
     . $',                                                             '
     .$Id: prp14.f,v 1.1 1995/09/17 18:50:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA PR14/4HPRP1,4H4   /
C.......................................................................
C     CHECK TRACE LEVEL
      CALL FPRBUG(PR14,1,14,IBUG)
C.......................................................................
C     PRINT HEADING
      WRITE(IPR,900) (PADJ(I),I=2,6)
 900  FORMAT(1H0,10X,21HADJUST OPERATION FOR ,5A4,35H. SIMULATED INSTANT
     1ANEOUS DISCHARGE/,11X,49HIS ADJUSTED BY AVAILABLE OBSERVED DISCHAR
     2GE DATA.)
C.......................................................................
C     PRINT TIME SERIES INFORMATION
C
      IOI=PADJ(7)
      IOM=PADJ(8)
      WRITE(IPR,910)
 910  FORMAT(1H0,10X,71HTIME SERIES USED:         ID                 TYP
     1E              TIME(HR))
      ITS=PADJ(12)
      WRITE(IPR,915) PADJ(9),PADJ(10),PADJ(11),ITS
 915  FORMAT(1H ,36X,2A4,11X,A4,14X,I2)
      WRITE(IPR,915) PADJ(13),PADJ(14),PADJ(15),ITS
      IF(IOM.EQ.0) GO TO 10
      IT=24
      WRITE(IPR,915) PADJ(26),PADJ(27),PADJ(28),IT
      IF(IOI.EQ.0) GO TO 20
      ITQ=PADJ(32)
      IOPT=PADJ(33)
      WRITE(IPR,915) PADJ(29),PADJ(30),PADJ(31),ITQ
      GO TO 20
 10   IF(IOI.EQ.0) GO TO 20
      ITQ=PADJ(28)
      IOPT=PADJ(29)
      WRITE(IPR,915) PADJ(25),PADJ(26),PADJ(27),ITQ
 20   CONTINUE
C.......................................................................
C     PRINT PARAMETERS
      IF((IOM.EQ.1).AND.(IOI.EQ.1)) WRITE(IPR,940)
      IF((IOM.EQ.0).AND.(IOI.EQ.1)) WRITE(IPR,945)
      IF((IOM.EQ.1).AND.(IOI.EQ.0)) WRITE(IPR,950)
 940  FORMAT(1H0,10X,72HBOTH OBSERVED MEAN DAILY AND INSTANTANEOUS DISCH
     1ARGE DATA ARE AVAILABLE.)
 945  FORMAT(1H0,10X,57HONLY OBSERVED INSTANTANEOUS DISCHARGE DATA ARE A
     1VAILABLE.)
 950  FORMAT(1H0,10X,54HONLY OBSERVED MEAN DAILY DISCHARGE DATA ARE AVAI
     1LABLE.)
C
      WRITE(IPR,920) ITS
 920  FORMAT(1H0,10X,47HCOMPUTATIONAL TIME INTERVAL FOR THE OPERATION =,
     1I3,7H HOURS.)
      NSTEPS=PADJ(17)
      WRITE(IPR,925) NSTEPS
 925  FORMAT(1H0,10X,33HNUMBER OF PERIODS USED TO BLEND =,I5,1H.)
      IF(IOM.EQ.0)GO TO 30
      PTOL=PADJ(25)*100.
      WRITE(IPR,930) PTOL
 930  FORMAT(1H0,10X,77HTOLERANCE FOR COMPARING SIMULATED AND OBSERVED M
     1EAN DAILY DISCHARGE VOLUMES =,F5.1,9H PERCENT.)
  30  CONTINUE
      IF(IOI.EQ.0)GO TO 50
      IF(IOPT.EQ.0)WRITE(IPR,936)
      IF(IOPT.EQ.1)WRITE(IPR,935)
 935  FORMAT(1H0,10X,95HADJUSTMENTS USING INSTANTANEOUS OBS. DATA BASED
     1ON DIFFERENCES BETWEEN OBS. AND SIM. DISCHARGE.)
 936  FORMAT(1H0,10X,84HADJUSTMENTS USING INSTANTANEOUS OBS. DATA BASED
     1ON RATIOS OF OBS. TO SIM. DISCHARGE.)
  50  CONTINUE
C.......................................................................
      RETURN
      END
