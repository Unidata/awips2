C MEMBER BQIL51
C DESC COMPUTE PERIOD START AND END VALUES FOR INFLOW AND LOCAL FLOW
C----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE BQIL51(CO,LCO,D,WK,LWQO1X,LWQO2X,LAOBS,CQSTX)
C-------------------------------------------------------------------
C
C----------------------------------------------------------------------
C  WRITTEN BY - KUANG HSU - HRL - OCTOBER 1996
C----------------------------------------------------------------------
C
      INCLUDE 'common/sarr51'
      INCLUDE 'common/lts51'
C
      DIMENSION CO(*),D(*),WK(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/bqil51.f,v $
     . $',                                                             '
     .$Id: bqil51.f,v 1.1 1996/12/09 21:42:41 dws Exp $
     . $' /
C    ===================================================================
C
      S1 = CO(LCO+2)
C
      DO 1000 I=1,LAOBS
      QO1N = WK(LWQO1+I-1)
      QO2 = WK(LWQO2+I-1)
      QOM = WK(LWQM+I-1)
      S2 = WK(LWST+I-1)
      QIM = (S2-S1)/CQSTX+QOM
      DQ = QOM-QIM
C
      IF(IRES.EQ.2) GO TO 800
      BQI1 = QO1N-DQ
      BQI2 = QO2-DQ
      GO TO 850
 800  CONTINUE
C
C  INFLOW = OUTFLOW FROM UPSTREAM RESERVOIR
      QI1 = WK(LWQO1X+I-1)
      QI2 = WK(LWQO2X+I-1)
      BQI1 = QO1N-QI1-DQ
      BQI2 = QO2-QI2-DQ
C
 850  IF(BQI1.LE.0.) BQI1=0.0
      IF(BQI2.LE.0.) BQI2=0.0
      BQIM = 0.5*(BQI1+BQI2)
      WK(LWBQI1+I-1) = BQI1
      WK(LWBQI2+I-1) = BQI2
      WK(LWBQIM+I-1) = BQIM
      S1 = S2
 1000 CONTINUE
      RETURN
      END


