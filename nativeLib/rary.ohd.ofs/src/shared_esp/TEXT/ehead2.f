C MEMBER EHEAD2
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EHEAD2(NVAR,NUM,AHD1,AHD2,KODE,UO)
C
C
      DIMENSION AHD1(1),AHD2(1)
      INCLUDE 'common/evar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ehead2.f,v $
     . $',                                                             '
     .$Id: ehead2.f,v 1.1 1995/09/17 19:18:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA DAYS/4HDAYS/,TO/4H TO /,VALU/4HVALU/,E/4HE   /,
     X  BIN/4H IN /,BLNK/4H    /,VAL/4H VAL/,UE/4HUE  /,
     X  DA/4H  DA/,YS/4HYS  /,CON/4H CON/,SEC/4HSEC /,
     X  AB/4H  AB/,BOVE/4HOVE /,BE/4H  BE/,LOW/4HLOW /
C
      GO TO (10,10,10,10,10,10,400,500),NVAR
 10   IF (NVPV(NVAR).EQ.2) GO TO 200
      DO 100 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=BLNK
      AHD1(II+1)=VAL
      AHD1(II+2)=UE
      AHD1(II+3)=BLNK
      AHD2(II)=BLNK
      AHD2(II+1)=BIN
      AHD2(II+2)=UO
      AHD2(II+3)=BLNK
 100  CONTINUE
      RETURN
 200  DO 300 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=VALU
      AHD1(II+1)=E
      AHD1(II+2)=DAYS
      AHD1(II+3)=TO
      AHD2(II)=BIN
      AHD2(II+1)=UO
      AHD2(II+2)=VAL
      AHD2(II+3)=UE
 300  CONTINUE
      RETURN
 400  CONTINUE
      IF (NVPV(7).EQ.2) GO TO 450
      DO 410 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=BLNK
      AHD1(II+1)=DAYS
      AHD1(II+2)=TO
      AHD1(II+3)=BLNK
      AHD2(II)=BLNK
      AHD2(II+1)=VAL
      AHD2(II+2)=UE
      AHD2(II+3)=BLNK
 410  CONTINUE
      RETURN
 450  CONTINUE
      DO 490 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=DAYS
      AHD1(II+1)=TO
      AHD1(II+2)=DA
      AHD1(II+3)=YS
      AHD2(II)=VAL
      AHD2(II+1)=UE
      AHD2(II+2)=BLNK
      AHD2(II+3)=BLNK
 490  CONTINUE
      RETURN
 500  CONTINUE
      IF (NVPV(8).EQ.2) GO TO 550
      DO 510 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=BLNK
      AHD1(II+1)=DA
      AHD1(II+2)=YS
      AHD1(II+3)=BLNK
      IF (KODE.EQ.2) GO TO 505
      AHD2(II)=BLNK
      AHD2(II+1)=AB
      AHD2(II+2)=BOVE
      AHD2(II+3)=BLNK
      GO TO 510
 505  AHD2(II)=BLNK
      AHD2(II+1)=BE
      AHD2(II+2)=LOW
      AHD2(II+3)=BLNK
 510  CONTINUE
      RETURN
 550  CONTINUE
      DO 590 I=1,NUM
      II=4*(I-1)+1
      AHD1(II)=DA
      AHD1(II+1)=YS
      AHD1(II+2)=CON
      AHD1(II+3)=SEC
      IF (KODE.EQ.2) GO TO 570
      AHD2(II)=AB
      AHD2(II+1)=BOVE
      AHD2(II+2)=DA
      AHD2(II+3)=YS
      GO TO 590
 570  AHD2(II)=BE
      AHD2(II+1)=LOW
      AHD2(II+2)=DA
      AHD2(II+3)=YS
 590  CONTINUE
      RETURN
      END
