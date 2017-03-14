C MEMBER EPPG
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EPPG(NUMT,G4,IPP,IDTS,J)
      DIMENSION G4(1),IPP(6,51,3),IDTS(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eppg.f,v $
     . $',                                                             '
     .$Id: eppg.f,v 1.1 1995/09/17 19:18:58 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DO 20 I=1,NUMT
      DO 10 K=1,3
      IF (IPP(I,J,K)) 10,10,11
 11   L=IPP(I,J,K)
      G4(L)=ESYMB(IDTS(I),2)
 10   CONTINUE
 20   CONTINUE
      RETURN
      END
