C MEMBER EFNORE
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      FUNCTION EFNORE(X)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efnore.f,v $
     . $',                                                             '
     .$Id: efnore.f,v 1.1 1995/09/17 19:18:40 dws Exp $
     . $' /
C    ===================================================================
C
      EFNORE=0
      IF (X.GT.9998.5.AND.X.LT.9999.5) EFNORE=1
      RETURN
      END
