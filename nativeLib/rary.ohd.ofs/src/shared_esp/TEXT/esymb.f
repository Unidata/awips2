C MEMBER ESYMB
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      FUNCTION ESYMB(I,J)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/esymb.f,v $
     . $',                                                             '
     .$Id: esymb.f,v 1.1 1995/09/17 19:19:09 dws Exp $
     . $' /
C    ===================================================================
C
      DATA CON1/1HC/,CON2/1H*/,OBS1/1HO/,OBS2/1H+/
      DATA HIS1/1HH/,HIS2/1HX/,ADJ1/1HA/,ADJ2/1H&/
      DATA BAS1/1HB/,BAS2/1H%/
C
C
      GO TO (10,100),J
 10   GO TO (20,30,40,50,60),I
 20   ESYMB=HIS1
      RETURN
 30   ESYMB=ADJ1
      RETURN
 40   ESYMB=CON1
      RETURN
 50   ESYMB=OBS1
      RETURN
 60   ESYMB=BAS1
      RETURN
C
C
 100  GO TO (120,130,140,150,160),I
 120  ESYMB=HIS2
      RETURN
 130  ESYMB=ADJ2
      RETURN
 140  ESYMB=CON2
      RETURN
 150  ESYMB=OBS2
      RETURN
 160  ESYMB=BAS2
      RETURN
      END
