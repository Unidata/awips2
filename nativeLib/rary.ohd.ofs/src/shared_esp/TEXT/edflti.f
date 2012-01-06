C MEMBER EDFLTI
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
       FUNCTION EDFLTI(P,IT)
C
C        FIND X FOR WHICH F(X)=P, FOR LOG TRANFORM
C        Y=LOG(X).
C
      COMMON/EPARM/AVG(6),STD(6),YMIN(6),YMAX(6),LBUG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/edflti.f,v $
     . $',                                                             '
     .$Id: edflti.f,v 1.1 1995/09/17 19:18:32 dws Exp $
     . $' /
C    ===================================================================
C
      Z=ECDFNI(P)
      Y=AVG(IT)+STD(IT)*Z
      X=EXP(Y)
      EDFLTI=X
      RETURN
      END
