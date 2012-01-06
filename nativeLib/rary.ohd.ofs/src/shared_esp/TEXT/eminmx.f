C MEMBER EMINMX
C  (from old member EEDEX01)
C
      SUBROUTINE EMINMX(X,NUM,N,XMIN,XMAX,IMS)
C
C
C
C         THIS SUBROUTINE COMPUTES THE MINIMUM AND MAXIMUM
C         VALUES FOR EACH OF NUM TIME SERIES, EACH OF WHICH
C         CONTAINS N OBSERVATIONS.
C
C
      INCLUDE 'common/ionum'
      REAL X(12,50),XMIN(NUM),XMAX(NUM)
      DIMENSION IMS(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eminmx.f,v $
     . $',                                                             '
     .$Id: eminmx.f,v 1.1 1995/09/17 19:18:52 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DO 10 I=1,NUM
      XMIN(I)=X(I,1)
      XMAX(I)=X(I,1)
 10   CONTINUE
      IF (N.LT.2) GO TO 800
      DO 30 I=1,NUM
      IF (IMS(I).NE.1) GO TO 15
      XMIN(I)=-999.
      XMAX(I)=-999.
      GO TO 30
 15   DO 20 J=2,N
      IF (X(I,J).GT.XMIN(I)) GO TO 20
      XMIN(I)=X(I,J)
 20   CONTINUE
      DO 25 J=2,N
      IF (X(I,J).LT.XMAX(I)) GO TO 25
      XMAX(I)=X(I,J)
 25   CONTINUE
 30   CONTINUE
      GO TO 810
 800  WRITE(IPR,805)
 805  FORMAT(10X,33H**ERROR** NUMBER OF YEARS MUST BE,
     X  17HGREATER THAN ONE.)
      CALL ERROR
 810  CONTINUE
      RETURN
      END
