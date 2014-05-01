C MEMBER EMOM3
C  (from old member EEDEX01)
C
       SUBROUTINE EMOM3(X,XM,XSD,YRWT,NUM,N,IMS)
C
C
C
C           THIS SUBROUTINE COMPUTES THE MEAN AND STANDARD
C           DEVIATION FOR EACH OF NUM TIME
C           SERIES, EACH OF WHICH CONTAINS N OBSERVATIONS.
C
C
      INCLUDE 'common/ionum'
      DIMENSION YRWT(1),IMS(12),X(12,50),XM(1),XSD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/emom3.f,v $
     . $',                                                             '
     .$Id: emom3.f,v 1.1 1995/09/17 19:18:54 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
      IF (N.LT.2) GO TO 800
      FN=N
C
C
      DO 30 I=1,NUM
      IF (IMS(I).NE.1) GO TO 10
      XM(I)=-999.
      XSD(I)=-999.
      GO TO 30
 10   W=X(I,1)
      SUM=W*YRWT(1)
      SSQ=W**2*YRWT(1)
      WT=YRWT(1)**2
C
C
      DO 20 J=2,N
      W=X(I,J)
      SUM=SUM+W*YRWT(J)
      SSQ=SSQ+W**2*YRWT(J)
      WT=WT+YRWT(J)**2
 20   CONTINUE
      SSQ=SSQ-SUM**2
      IF (ABS(SSQ).LT.1.E-5) SSQ=0.0
      SS1=.01
      IF (SSQ.LT.SS1) GO TO 22
      WT=1./(1.-WT)
      SSQ=SSQ*WT
      STD=SQRT(SSQ)
      GO TO 28
 22   STD=0.
   28 XM(I)=SUM
      XSD(I)=STD
 30   CONTINUE
      GO TO 810
 800  WRITE(IPR,820)
 820  FORMAT(10X,33H**ERROR** NUMBER OF YEARS MUST BE,
     X  17HGREATER THAN ONE.)
      CALL ERROR
 810  RETURN
      END
