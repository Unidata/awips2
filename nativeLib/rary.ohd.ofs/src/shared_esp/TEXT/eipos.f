C MEMBER EIPOS
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EIPOS(Z,K)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eipos.f,v $
     . $',                                                             '
     .$Id: eipos.f,v 1.1 1995/09/17 19:18:50 dws Exp $
     . $' /
C    ===================================================================
C
      K=(Z+3)*20+1+.49
      IF (K-1) 10,20,20
 20   IF (K-121) 30,30,40
 10   K=1
      GO TO 30
 40   K=121
 30   RETURN
      END
