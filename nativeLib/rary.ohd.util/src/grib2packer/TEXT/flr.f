      REAL FUNCTION FLR(VALUE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/flr.f,v $
     . $',                                                             '
     .$Id: flr.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      IF(VALUE.GE.0)THEN
         FLR=AINT(VALUE)
      ELSE
C
         IF(AMOD(VALUE,1.0).EQ.0)THEN
            FLR=VALUE
         ELSE
            FLR=AINT(VALUE)-1 
         ENDIF
C
      ENDIF
C
      RETURN
      END
