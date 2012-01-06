C MEMBER ETITLE
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
C          THIS SUBROUTINE WRITES THE TITLE BLOCK FOR
C          THE PLOT DISPLAY.
C
      SUBROUTINE ETITLE(ISAMP,NUMT,IDTS)
      INCLUDE 'common/ionum'
      DIMENSION IDTS(1),IND(5),X(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/etitle.f,v $
     . $',                                                             '
     .$Id: etitle.f,v 1.1 1995/09/17 19:19:10 dws Exp $
     . $' /
C    ===================================================================
C
      DATA CON1/1HC/,CON2/1H*/,OBS1/1HO/,OBS2/1H+/
      DATA HIS1/1HH/,HIS2/1HX/,ADJ1/1HA/,ADJ2/1H&/
      DATA BAS1/1HB/,BAS2/1H%/
      IF (ISAMP.EQ.1) GO TO 40
      WRITE(IPR,900)
      WRITE(IPR,910)
      GO TO 60
 40   WRITE(IPR,920)
      WRITE(IPR,930)
 60   DO 100 I=1,5
      IND(I)=0
 100  CONTINUE
      DO 150 I=1,NUMT
      J=IDTS(I)
      IND(J)=1
 150  CONTINUE
      X(2)=BLNK
      IF (IND(1).EQ.0) GO TO 200
      X(1)=HIS1
      IF (ISAMP.EQ.1) X(2)=HIS2
      WRITE(IPR,940) (X(I),I=1,2)
 200  X(2)=BLNK
      IF (IND(2).EQ.0) GO TO 300
      X(1)=ADJ1
      IF (ISAMP.EQ.1) X(2)=ADJ2
      WRITE(IPR,950) (X(I),I=1,2)
 300  X(2)=BLNK
      IF (IND(3).EQ.0) GO TO 400
      X(1)=CON1
      IF (ISAMP.EQ.1) X(2)=CON2
      WRITE(IPR,960) (X(I),I=1,2)
 400  X(2)=BLNK
      IF (IND(4).EQ.0) GO TO 500
      X(1)=OBS1
      IF (ISAMP.EQ.1) X(2)=OBS2
      WRITE(IPR,970) (X(I),I=1,2)
 500  X(2)=BLNK
      IF (IND(5).EQ.0) GO TO 600
      X(1)=BAS1
      IF (ISAMP.EQ.1) X(2)=BAS2
      WRITE(IPR,980) (X(I),I=1,2)
 600  CONTINUE
 900  FORMAT(12X,6HLEGEND,24X,6HFITTED)
 910  FORMAT(42X,12HDISTRIBUTION)
 920  FORMAT(12X,6HLEGEND,24X,6HFITTED,11X,9HEMPIRICAL)
 930  FORMAT(42X,12HDISTRIBUTION,7X,12HDISTRIBUTION)
 940  FORMAT(10X,22HHISTORICAL SIMULATION ,15X,A1,22X,A1)
 950  FORMAT(10X,19HADJUSTED SIMULATION,18X,A1,22X,A1)
  960  FORMAT(10X,22HCONDITIONAL SIMULATION,15X,A1,22X,A1)
 970  FORMAT(10X,8HOBSERVED,29X,A1,22X,A1)
 980  FORMAT(10X,11HBASE PERIOD,26X,A1,22X,A1)
      RETURN
      END
