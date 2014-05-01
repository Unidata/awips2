C MEMBER EMPFIT
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EMPFIT(Y,N,PP,NUMFR,PX,NVAR,PNE)
      DIMENSION PP(1),Y(1),PX(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/empfit.f,v $
     . $',                                                             '
     .$Id: empfit.f,v 1.1 1995/09/17 19:18:55 dws Exp $
     . $' /
C    ===================================================================
C
      CALL ESINCR(N,Y)
      FN=N
      XMIN=1./(FN+1.)
      XMAX=FN/(FN+1.)
      IF (NVAR.GE.7) GO TO 400
      DO 200 J=1,NUMFR
      IF (PX(J).LT.XMIN) GO TO 180
      DO 100 K=2,N
      XMNK=K*XMIN
      IF (PX(J).GT.XMNK) GO TO 100
      PP(J)=Y(K-1)+(Y(K)-Y(K-1))*(PX(J)-XMNK+XMIN)*(FN+1.)
      GO TO 200
 100  CONTINUE
 180  PP(J)=-999.
 200  CONTINUE
      RETURN
C
 400  CONTINUE
      DO 500 J=1,NUMFR
      IF (PX(J).LT.XMIN) GO TO 480
      IF (PX(J).GE.XMAX) GO TO 480
      DO 600 K=2,N
      IF (Y(K).GE.9998.5) GO TO 460
      XMNK=K*XMIN
      IF (PX(J).GE.XMNK) GO TO 600
      PP(J)=Y(K-1)+(Y(K)-Y(K-1))*(PX(J)-XMNK+XMIN)*(FN+1.)
      GO TO 500
 600  CONTINUE
 480  PP(J)=-999.
      GO TO 500
 460  PP(J)=9999.
 500  CONTINUE
      ICNT=0
      DO 666 K=1,N
      IF (Y(K).LT.9998.5) GO TO 666
      ICNT=ICNT+1
 666  CONTINUE
      FCNT=INCT
      PNE=FNCT/FN
      RETURN
      END
