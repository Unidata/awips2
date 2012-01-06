C MEMBER ESETFT
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE ESETFT(IP,ILOC,IDIST,NUMT,XLL,XSS,DY)
C
C
CC
      DIMENSION ILOC(6),IP(6,51)
      COMMON/EPARM/AVG(6),STD(6),YMIN(6),YMAX(6),LBUG
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/esetft.f,v $
     . $',                                                             '
     .$Id: esetft.f,v 1.1 1995/09/17 19:19:02 dws Exp $
     . $' /
C    ===================================================================
C
C
C
CC
      NL=51
      DO 20 I=1,NUMT
      DO 20 J=1,51
 20   IP(I,J)=0.
C
C
      DO 200 I=1,NUMT
      II=ILOC(I)
      YMX=YMAX(II)
      YMN=YMIN(II)
      AV=AVG(II)
      ST=STD(II)
      YMX=ETRAN(YMX,IDIST)
      YMN=ETRAN(YMN,IDIST)
      X=XLL
C
C
C
      DO 41 J=1,51
      IF (YMX.LT.X) GO TO 40
      Z=0
      IF (ST.NE.0) Z=(X-AV)/ST
      CALL EIPOS(Z,K)
      IP(I,J)=K
 40   X=X-DY
      IF (X.LT.YMN) GO TO 200
 41   CONTINUE
 200  CONTINUE
      RETURN
      END
