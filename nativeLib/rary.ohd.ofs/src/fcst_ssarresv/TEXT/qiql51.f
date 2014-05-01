C MEMBER QIQL51
C DESC COMPUTE PERIOD START AND END VALUES FOR INFLOW AND LOCAL FLOW
C----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE QIQL51(CO,D,I,QI1,QI2,LDQL1X,QL1,LDQL2X,QL2,
     & WK,LWQO1X,LWQO2X)
C-------------------------------------------------------------------
C
C----------------------------------------------------------------------
C  WRITTEN BY - KUANG HSU - HRL - OCTOBER 1994
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/qiql51.f,v $
     . $',                                                             '
     .$Id: qiql51.f,v 1.1 1996/03/21 13:42:44 page Exp $
     . $' /
C    ===================================================================
C
C
      QI1=0.0
      QL1=0.0
      QL2=0.0
      IF(IRES.EQ.2) GO TO 800
C
C  INFLOW
      QI2=D(LDQI2+I-1)
      IF(LDQI1.GT.0) GO TO 510
C
C  SET INITIAL VALUES FROM CARRYOVER
C
      LDQI1X=LDQI2-1
      IF(I.EQ.1) THEN
        QI1  = CO(1)
        GO TO 520
      END IF
 510  QI1 = D(LDQI1X+I-1)
 520  CONTINUE
      GO TO 1000
 800  CONTINUE
C
C  INFLOW
      QI2=WK(LWQO2X+I-1)
      IF(LWQO1X.GT.0) GO TO 795
C
C  SET INITIAL VALUES FROM CARRYOVER
C
      LWQO1X=LWQO2X-1
      IF(I.EQ.1) THEN
        QI1  = CO(2)
        GO TO 796
      END IF
 795  QI1 = WK(LWQO1X+I-1)
 796  CONTINUE
C
C  LOCAL FLOW
      IF(LDQL2X.LE.0) GO TO 1000
      QL2=D(LDQL2X+I-1)
      IF(LDQL1X.GT.0) GO TO 895
C
C  SET INITIAL VALUES FROM CARRYOVER
C
      LDQL1X=LDQL2X-1
      IF(I.EQ.1) THEN
        QL1  = CO(5)
        GO TO 1000
      END IF
 895  QL1 = D(LDQL1X+I-1)
 1000 CONTINUE

      RETURN
      END
