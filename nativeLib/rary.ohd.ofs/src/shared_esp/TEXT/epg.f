C MEMBER EPG
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EPG(NUMT,G4,IP,IDTS,J)
      DIMENSION G4(1),IP(6,51),IDTS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/epg.f,v $
     . $',                                                             '
     .$Id: epg.f,v 1.1 1995/09/17 19:18:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
      DO 10 I=1,NUMT
      L=IP(I,J)
      IF (L.EQ.0) GO TO 10
      G4(L)=ESYMB(IDTS(I),1)
 10   CONTINUE
      RETURN
      END
