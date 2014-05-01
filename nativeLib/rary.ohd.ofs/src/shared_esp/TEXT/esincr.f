C MEMBER ESINCR
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE ESINCR(N,X)
      DIMENSION X(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/esincr.f,v $
     . $',                                                             '
     .$Id: esincr.f,v 1.1 1995/09/17 19:19:05 dws Exp $
     . $' /
C    ===================================================================
C
      DO 10 I=1,N
      T=X(I)
      DO 10 J=1,N
      IF (X(J)-T) 10,10,15
 15   X(I)=X(J)
      X(J)=T
      T=X(I)
 10   CONTINUE
      RETURN
      END
