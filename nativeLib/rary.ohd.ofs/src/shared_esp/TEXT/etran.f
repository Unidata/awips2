C MEMBER ETRAN
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      FUNCTION ETRAN(X,IDIST)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/etran.f,v $
     . $',                                                             '
     .$Id: etran.f,v 1.1 1995/09/17 19:19:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C          IDIST =     1-SAMPLE,NO TRANFORM
C                      2-LOG TRANFORM
C                      3-NORMAL,NO TRANFORM
C
      IF (IDIST.NE.2) GO TO 40
      ETRAN=ALOG(X)
      RETURN
 40   ETRAN=X
      RETURN
      END
