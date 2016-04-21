C MEMBER VSUM17
C  (from old member MCEX17)
C
C @PROCESS LVL(77) OPT(2)
C
      SUBROUTINE VSUM17(NPLOT,AREA,CO,CMO)
C.......................................
C     THIS SUBROUTINE PRINTS THE STREAMFLOW VOLUME SUMMARY FOR THE
C        'WY-PLOT ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL     APRIL 1980
C.......................................
C
C     VARIABLES IN THE ARGUMENT LIST - VSUM17
C
C        NPLOT   -  NUMBER OF DISCHARGE TIME SERIES TO BE PLOTTED
C                     (INPUT)
C        AREA    -  DRAINAGE AREA - SQ. KM (INPUT)
C        CO      -  CARRYOVER ARRAY - (BOTH)
C        CMO     -  CHARACTER REPRESENTATION FOR EACH MONTH (INPUT)
C
C.......................................
      DIMENSION CO(*),CMO(12)
      DIMENSION UNITS(2,2),QM(12),UNIT(2)
C
C     COMMON BLOCK
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/vsum17.f,v $
     . $',                                                             '
     .$Id: vsum17.f,v 1.2 1996/07/11 19:49:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA UNITS/4HCFSD,4H  IN,4HCMSD,4H  MM/
C.......................................
C     TRACE LEVEL=1, NO DEBUG OUTPUT.
      IF(ITRACE.GE.1) WRITE(IODBUG,10)
10    FORMAT(1H0,17H** VSUM17 ENTERED)
C.......................................
C     PRINT HEADINGS
      WRITE(IPR,20)
20    FORMAT(1H0,25HSTREAMFLOW VOLUME SUMMARY)
      WRITE(IPR,30) (CMO(I),I=10,12),(CMO(I),I=1,9)
30    FORMAT (1H0,12X,12(5X,A3),9X,6HANNUAL)
C.......................................
C     CONVERSION FACTORS
      IF(METRIC.EQ.1) GO TO 40
      C1=35.3147
      C2=.03937
      GO TO 50
40    C1=1.0
      C2=1.0
50    CONV=1.0/(0.011574*AREA)
      UNIT(1)=UNITS(1,METRIC+1)
      UNIT(2)=UNITS(2,METRIC+1)
C.......................................
C     PRINT VOLUMES FOR EACH DISCHARGE TIME SERIES.
C        PRINTED IN VOLUME UNITS, RUNOFF UNITS, AND DEPARTURE
C        FROM THE FIRST TIME SERIES
      DO 200 N=1,NPLOT
      J=(N-1)*12
C
C     PRINT VOLUME UNITS
      SUM=0.0
      DO 70 M=1,12
      Q=CO(J+M)
      IF(Q.LT.0.0) GO TO 60
      QM(M)=Q*C1
      SUM=SUM+QM(M)
      CO(J+M)=Q*CONV
      GO TO 70
60    QM(M)=-0.001
70    CONTINUE
      IF (N.GT.9) GO TO 90
      WRITE(IPR,80)N,QM,SUM,UNIT(1)
80    FORMAT(1H0,2HQ(,I1,1H),8X,12F8.0,F12.0,1X,A4)
      GO TO 110
90    WRITE(IPR,100)N,QM,SUM,UNIT(1)
100   FORMAT(1H0,2HQ(,I2,1H),7X,12F8.0,F12.0,1X,A4)
C
C     PRINT RUNOFF UNITS
110   SUM=0.0
      DO 130 M=1,12
      Q=CO(J+M)
      IF(Q.LT.0.0) GO TO 120
      QM(M)=Q*C2
      SUM=SUM+QM(M)
      GO TO 130
120   QM(M)=-0.001
130   CONTINUE
      WRITE(IPR,140) QM,SUM,UNIT(2)
140   FORMAT(1H ,12X,12F8.1,F12.1,1X,A4)
      IF(N.EQ.1) GO TO 200
C
C     PRINT DEPARTURE FROM THE FIRST TIME SERIES.
      SUM=0.0
      DO 160 M=1,12
      Q=CO(J+M)
      Q1=CO(M)
      IF((Q.LT.0.0).OR.(Q1.LT.0.0)) GO TO 150
      DQ=(Q-Q1)*C2
      QM(M)=DQ
      SUM=SUM+DQ
      GO TO 160
150   QM(M)=-0.001
160   CONTINUE
      IF (N.GT.9) GO TO 180
      WRITE(IPR,170) N,QM,SUM,UNIT(2)
170   FORMAT(1H ,2HQ(,I1,6H)-Q(1),3X,12F8.1,F12.1,1X,A4)
      GO TO 200
180   WRITE(IPR,190) N,QM,SUM,UNIT(2)
190   FORMAT(1H ,2HQ(,I2,6H)-Q(1),2X,12F8.1,F12.1,1X,A4)
200   CONTINUE
C.......................................
      RETURN
      END
