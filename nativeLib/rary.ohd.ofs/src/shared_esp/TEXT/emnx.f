C MEMBER EMNX
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EMNX(X,N,IT)
C
C
C
      REAL X(1)
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      COMMON/EPARM/AVG(6),STD(6),YMIN(6),YMAX(6),LBUG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/emnx.f,v $
     . $',                                                             '
     .$Id: emnx.f,v 1.1 1995/09/17 19:18:52 dws Exp $
     . $' /
C    ===================================================================
C
      XMN=X(1)
      XMX=X(1)
      IF (N.LE.1) GO TO 800
      DO 10 I=2,N
      IF (X(I).GT.XMN) GO TO 10
      XMN=X(I)
 10   CONTINUE
      DO 20 J=2,N
      IF (X(J).LT.XMX) GO TO 20
      XMX=X(J)
 20   CONTINUE
      YMIN(IT)=XMN
      YMAX(IT)=XMX
      RETURN
 800  WRITE(IPR,805)
 805  FORMAT(10X,33H**ERROR** NUMBER OF YEARS MUST BE,
     X  17HGREATER THAN ONE.)
      RETURN
      END
