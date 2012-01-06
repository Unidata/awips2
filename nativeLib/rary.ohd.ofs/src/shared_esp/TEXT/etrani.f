C MEMBER ETRANI
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      FUNCTION ETRANI(X,I)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/etrani.f,v $
     . $',                                                             '
     .$Id: etrani.f,v 1.1 1995/09/17 19:19:13 dws Exp $
     . $' /
C    ===================================================================
C
      GO TO (10,20,10),I
 10   ETRANI=X
      RETURN
 20   ETRANI=EXP(X)
      RETURN
      END
