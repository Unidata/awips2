C MEMBER CSOIL54
C===========================================================
C
       FUNCTION CSOIL54(DSOIL,W,T)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/csoil54.f,v $
     . $',                                                             '
     .$Id: csoil54.f,v 1.1 1997/09/22 15:29:41 page Exp $
     . $' /
C    ===================================================================
C
C   ****  SIMULATION OF THERMAL SOIL CONDUCTIVITY
C   ****     CSOIL54 IN CAL/(CM*HR* C)
       CSOIL54=((5.42*DSOIL-3.34)*(LOG(W/DSOIL)+4.6)-
     +       (1.6*DSOIL-0.5))
       IF(T.LT.0.) CSOIL54=CSOIL54+(0.8+26.7*(W/DSOIL-0.1))
       RETURN
       END
