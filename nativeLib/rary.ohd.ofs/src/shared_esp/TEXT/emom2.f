C MEMBER EMOM2
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EMOM2(X,N,S,YRWT)
C
C       THIS SUBROUTINE COMPUTES THE FIRST TWO MOMENTS
C       OF A TIME SERIES X WITH N OBSERVATIONS.
C
C
      DIMENSION YRWT(1),X(1),S(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/emom2.f,v $
     . $',                                                             '
     .$Id: emom2.f,v 1.1 1995/09/17 19:18:53 dws Exp $
     . $' /
C    ===================================================================
C
      S(1)=X(1)
      S(2)=0.0
      IF (N.LT.2) RETURN
 30   CONTINUE
      SUM=X(1)*YRWT(1)
      SSQ=(X(1)**2)*YRWT(1)
      WT=YRWT(1)**2
      DO 20 I=2,N
      SUM=SUM+X(I)*YRWT(I)
      SSQ=SSQ+(X(I)**2)*YRWT(I)
      WT=WT+YRWT(I)**2
 20   CONTINUE
      SSQ=SSQ-SUM**2
      WT=1./(1.-WT)
      IF (ABS(SSQ).LT.1.E-5) SSQ=0.0
      SS1=.01
      IF (SSQ.LT.SS1) GO TO 60
      SSQ=SSQ*WT
      STD=SQRT(SSQ)
      GO TO 70
 60   STD=0.0
 70   S(1)=SUM
      S(2)=STD
      RETURN
      END
