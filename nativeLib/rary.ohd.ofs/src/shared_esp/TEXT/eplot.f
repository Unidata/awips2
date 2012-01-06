C MEMBER EPLOT
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EPLOT(IDIST,A,NYRS,NBYRS,DSP,ITTS,IPLTTS,NVAR,LOC,NUM,
     X   IP,IPP,HEAD,VARNAM,IWIND,TDSP,KODE,VALUE,UNITS)
C
C        EPLOT IS THE PLOT SUBROUTINE FOR THE
C        EFREQUENCY DISPLAY
C
      LOGICAL LBUG
C
      DIMENSION VARNAM(1),TDSP(1),HEAD(1),ITTS(1),IDTS(5),A(1),
     X   DSP(1),IPLTTS(6)
       DIMENSION PLOC(6),ILOC(6),LOC(6),IP(6,51),IPP(6,51,3)
      COMMON/EPARM/AVG(6),STD(6),YMIN(6),YMAX(6),LBUG
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eplot.f,v $
     . $',                                                             '
     .$Id: eplot.f,v 1.1 1995/09/17 19:18:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
      IDIST=DSP(3)
      IF (NVAR.GE.7) IDIST=1
      IF (IDIST.GE.3) IDIST=3
      NUMFR=DSP(4)
      II=NUMFR+6
      ISAMP=DSP(II)
      NUMT=0
      DO 10 J=1,NUM
      K=II+ITTS(J)
      INDC=DSP(K)
      IF (INDC.EQ.0) GO TO 10
      IF(IPLTTS(J).EQ.0) GO TO 10
      NUMT=NUMT+1
      ILOC(NUMT)=J
      IDTS(NUMT)=ITTS(J)
      PLOC(NUMT)=LOC(J)
 10   CONTINUE
C...DEBUG TIME.......................
      IF(LBUG) WRITE(IODBUG,700) INUM,IDIST,NUMT,ISAMP,IDTS(1),
     X  IDTS(2),IDTS(3)
 700  FORMAT(5X,7I5)
C..............................
C
C
      I=ILOC(1)
      XLL=YMAX(I)
      XSS=YMIN(I)
      IF (LBUG) WRITE(IODBUG,717) XSS
      IF (NUMT.EQ.1) GO TO 30
C
C
      DO 20 J=2,NUMT
      I=ILOC(J)
      IF (YMIN(I).GE.XSS) GO TO 15
      XSS=YMIN(I)
      IF (LBUG) WRITE(IODBUG,717) XSS
 15   IF (YMAX(I).LE.XLL) GO TO 20
      XLL=YMAX(I)
 20   CONTINUE
 30   CONTINUE
C
C
CC
      XLL=ETRAN(XLL,IDIST)
      XSS=ETRAN(XSS,IDIST)
      IF (LBUG) WRITE(IODBUG,717) XSS
 717  FORMAT(5X,F10.2)
      NL=51
      DY=(XLL-XSS)/(NL-1)
      XLL=XLL+DY
      IF (XSS-DY.GE.0.) XSS=XSS-DY
      DY=(XLL-XSS)/(NL-1)
C
C
      IF (IDIST.EQ.1) GO TO 100
      CALL ESETFT(IP,ILOC,IDIST,NUMT,XLL,XSS,DY)
      IF (ISAMP.EQ.0) GO TO 200
 100  CALL ESETSM(IPP,IDIST,XSS,DY,A,ILOC,PLOC,NYRS,NBYRS,NVAR,
     X  NUMT,NUM,IDTS)
 200  CONTINUE
C
C
C
      CALL EGOPLT(ISAMP,NUMT,IP,IPP,XSS,XLL,IDIST,IDTS,
     X   NVAR,HEAD,VARNAM,IWIND,TDSP,KODE,VALUE,UNITS)
      RETURN
      END
