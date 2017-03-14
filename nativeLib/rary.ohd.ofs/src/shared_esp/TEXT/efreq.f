C MEMBER EFREQ
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EFREQ(N,X,P)
      DIMENSION X(1),P(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efreq.f,v $
     . $',                                                             '
     .$Id: efreq.f,v 1.1 1995/09/17 19:18:41 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
 100  IF (N) 200,200,201
 201  CALL ESINCR(N,X)
      FN=N
      DO 110 I=1,N,5
      J=N-I+1
      IF (J-5) 105,105,106
 106  J=5
 105  DO 115 K=1,J
      L=I+K-1
 115  P(L)=L/(FN+1.)
 110  CONTINUE
 200  RETURN
      END
