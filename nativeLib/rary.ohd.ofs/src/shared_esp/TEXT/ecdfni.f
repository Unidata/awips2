C MEMBER ECDFNI
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      FUNCTION ECDFNI(P)
C
      REAL  DP1
      PARAMETER ( DP1=1.0 )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecdfni.f,v $
     . $',                                                             '
     .$Id: ecdfni.f,v 1.1 1995/09/17 19:18:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C       INVERSE FUNCTION FOR STANDARD NORMAL DISTRIBUTION.
C
C
      IF (P-0.5) 10,20,30
 10   IF (P.LE.0.0) GO TO 300
      T2=ALOG(DP1/P**2)
      T=SQRT(T2)
      X=T-(2.515517+0.802853*T+0.010328*T2)/(1.0+1.432788*T
     Z  +0.189269*T2+0.001308*T*T2)
      ECDFNI=-X
      RETURN
 20   ECDFNI=0.0
      RETURN
 30   IF (P.GE.1.0) GO TO 300
      T2=ALOG(DP1/(DP1-P)**2)
      T=SQRT(T2)
      X=T-(2.515517+0.802853*T+0.010328*T2)/(1.0+1.432788*T
     Z  +0.189269*T2+0.001308*T*T2)
      ECDFNI=X
      GO TO 400
 300  WRITE(IPR,310)
 310  FORMAT(10X,40H**ERROR** IN A CALL TO FUNCTION ECDFNI A,
     Z 45HPROBABILITY VALUE IS OUT OF THE RANGE 0.0-1.0)
      CALL ERROR
  400 CONTINUE
      RETURN
      END
