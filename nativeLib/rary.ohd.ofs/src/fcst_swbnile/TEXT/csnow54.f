C MEMBER CSNOW54
C===========================================================
C
       FUNCTION CSNOW54(DSNOW)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/csnow54.f,v $
     . $',                                                             '
     .$Id: csnow54.f,v 1.1 1997/09/22 15:29:27 page Exp $
     . $' /
C    ===================================================================
C
C   ****   SIMULATION OF THERMAL SNOW CONDUCTIVITY
C   ****  CSNOW54 IN CAL/(CM*HR* C)
       CSNOW54=0.328*10**(2.25*DSNOW)
       RETURN
       END
