C MEMBER ELTFIT
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE ELTFIT(Y,N,IT,YRWT)
C
C       PARAMETER ESTIMATION FOR LOG TRANFORM,
C       X=LOG(Y).
C
      INCLUDE 'common/ionum'
      COMMON/EPARM/AVG(6),STD(6),YMIN(6),YMAX(6),LBUG
      DIMENSION YRWT(1),Y(N),STAT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eltfit.f,v $
     . $',                                                             '
     .$Id: eltfit.f,v 1.1 1995/09/17 19:18:51 dws Exp $
     . $' /
C    ===================================================================
C
      CALL EMOM2(Y,N,STAT,YRWT)
      S=(STAT(1)**2+STAT(2)**2)/STAT(1)**2
      S=ALOG(S)
      STD(IT)=SQRT(S)
      AVG(IT)=ALOG(STAT(1))-.5*S
      RETURN
      END
