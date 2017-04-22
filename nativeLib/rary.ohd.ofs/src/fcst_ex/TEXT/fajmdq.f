C MEMBER FAJMDQ
C  (from old member FCFAJMDQ)
C.......................................................................
      SUBROUTINE FAJMDQ(QS,MISS,SQME,QME,IBQM,IEQM,IBSQ,IESQ,TOL,
     1CADJ,LASTOB,ITSQ,IHR,LHR)
C.......................................................................
C     SUBROUTINE FAJMDQ ADJUSTS SIMULATED INSTANTANEOUS DISCHARGE
C     SO THAT THE RESULTING MEAN DAILY VOLUMES ARE WITHIN THE SPECIFIED
C     TOLERANCE LIMITS OF THE OBSERVED MEAN DAILY VOLUMES.
C.......................................................................
C     PROGRAMMED BY KAY KROUSE    MARCH 1980
C.......................................................................
C
      DIMENSION QS(1),MISS(1),SQME(1),QME(1),CADJ(1)
      INCLUDE 'common/ionum'
C  THE FOLLOWING LINE ADDED ON 9/18/89 -- MAIN. #C0038
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/fajmdq.f,v $
     . $',                                                             '
     .$Id: fajmdq.f,v 1.1 1995/09/17 18:57:28 dws Exp $
     . $' /
C    ===================================================================
C
C  END OF CHANGE OF 9/18/89
C
C     COMPUTE MDQ VALUES FROM THE SIMULATED DISCHARGES
      ITER=0
      NP=24/ITSQ
      NC=IHR/ITSQ
 70   NEC=0
      NB=1
      NE=NB+NP-1
      LASTOB=0
      K=0
      DO 100 I=IBQM,IEQM
      SQME(I)=0.
      IF(MISS(I).GT.0) GO TO 95
      IF((I.EQ.IEQM).AND.(LHR.LT.24))GO TO 100
      SUM=0.0
      DO 80 J=NB,NE
      IF(J.LT.NC) GO TO 72
      IF(J.EQ.NC) GO TO 74
      K=IBSQ+J-NC-1
      X=QS(K)
      Y=QS(K+1)
      GO TO 75
 72   X=CADJ(J)
      Y=CADJ(J+1)
      GO TO 75
 74   L=J
      X=CADJ(L)
      Y=QS(IBSQ)
C
C MODIFY 7/9/86 EJV
C NEED TO CHECK IF PREVIOUS INST VALUE (X) IS MISSING
C IF SO SET IT = TO CURRENT INST (Y)
75    CONTINUE
      IF (IFMSNG(X).EQ.1) X=Y
C END OF EJV MODIFY
      SUM=SUM+(X+Y)*0.5
 80   CONTINUE
      LASTOB=K+1
      SQME(I)=SUM/NP
      IF(SQME(I).LT..0001)GO TO 92
      IF(QME(I).LT..0001)GO TO 92
C     IS DIFFERENCE WITHIN TOLERANCE?
      ER=ABS(SQME(I)-QME(I))/QME(I)
      RTOL=TOL
      IF(I.EQ.IBQM)RTOL=TOL+((NC-1.)/(NP+1.))*TOL
      IF(ER.GT.RTOL) NEC=1
      GO TO 95
 92   IF(ITER.EQ.0)NEC=1
 95   NB=NE+1
      NE=NB+NP-1
 100  CONTINUE
C.......................................................................
C
C     IF NEC=1, BEGIN ADJUSTMENT PROCESS--SIMULATED VALUES ARE
C     ADJUSTED BY RATIO OF QME TO SQME.
C
      IF(NEC.EQ.0) GO TO 170
      NB=1
      NE=NB+NP
      QMID=0.
      DO 150 I=IBQM,IEQM
      MOB=0
      ISIM=0
      IF(MISS(I).GT.0) GO TO 140
      IF((I.EQ.IEQM).AND.(LHR.LT.24))GO TO 150
      IF(QME(I).LT..0001)MOB=1
      IF(SQME(I).GT..0001)GO TO 200
      ISIM=1
      GO TO 210
 200  RATIO=QME(I)/SQME(I)
 210  CONTINUE
      DO 120 J=NB,NE
      IF(J.LE.NC) GO TO 120
      K=IBSQ+J-NC-1
C EJV MODIFY 7/9/86
C NEED TO CHECK IF THE INST IS MISSING
      IF (IFMSNG(QS(K)).EQ.1) GO TO 120
C END MODIFY
      IF(J.EQ.NB) GO TO 105
      IF(J.EQ.NE) GO TO 110
      IF(ISIM.EQ.1)GO TO 220
      QS(K)=QS(K)*RATIO
      GO TO 120
 220  QS(K)=QME(I)
      GO TO 120
 105  IF(ISIM.EQ.1)GO TO 230
      QS(K)=((QS(K)*RATIO)+QMID)/2.
      IF(MOB.EQ.1)QS(K)=0.
      GO TO 120
 230  QS(K)=(QME(I)+QMID)/2.
      GO TO 120
 110  IF(ISIM.EQ.1)GO TO 240
      QMID=QS(K)*RATIO
      GO TO 120
 240  QMID=QME(I)
 120  CONTINUE
      GO TO 145
 140  IF(I.NE.IBQM)QS(K)=(QMID+QS(K))/2.
      K=IBSQ+NE-NC-1
      QMID=QS(K)
 145  NB=NE
      NE=NB+NP
 150  CONTINUE
      QS(K)=(QMID+QS(K))/2.
      IF(MOB.EQ.1)QS(K)=0.
C     RE-COMPUTE SQME VALUES
      IF(ITER.GE.15) GO TO 160
      ITER=ITER+1
      GO TO 70
C  THE FOLLOWING CHANGES MADE ON 9/18/89
C 160  WRITE(IPR,905) (QME(I),I=IBQM,IEQM)
 160  IF(MAINUM.EQ.4) GO TO 170
      WRITE(IPR,905) (QME(I),I=IBQM,IEQM)
C  END OF CHANGE OF 9/18/89
 905  FORMAT(1H0,10X,103H**WARNING** ADJUSTMENTS WERE STOPPED AFTER 15
     1ITERATIONS-ERROR WAS NOT REDUCED TO THE TOLERANCE LEVEL.
     2 / 10X,'MEAN DAILY VALUES ARE' / 12(1X,F9.1))
      WRITE(IPR,906) (QS(I),I=IBSQ,IESQ)
906   FORMAT(10X,'INSTANTANEOUS VALUES ARE' / 12(1X,F9.1))
      CALL WARN
C
 170  CONTINUE
      RETURN
      END
