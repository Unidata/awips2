C MEMBER SCAL12
C  (from old member FCEX12)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/27/95.08:22:15 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SCAL12(NPLOTS,IDTQ,LPLOTQ,D,ORD,DIV,SCALE)
C
C   ROUTINE TO DETERMINE SCALE AND MAX AND MIN FLOWS FOR PLOT OPTION
C   TO PLOT ALL DAYS REGARDLESS OF MISSING VALUES
C
C
C   INITIALLY WRITTEN BY
C             DAVID REED HRL      JUNE 1980
C
C
C
      DIMENSION IDTQ(1),LPLOTQ(1),D(1),ORD(1),SCALE(1)
      INCLUDE 'common/fctime'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_insqplot/RCS/scal12.f,v $
     . $',                                                             '
     .$Id: scal12.f,v 1.1 1995/09/17 18:58:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DATA DOT/1H./,BLANK/1H /,EXCLAM/1H!/
C
      IF(MAINUM.EQ.1)DOT=EXCLAM
C
C     DETERMINE MAXIMUM FLOW TO BE PLOTTED
C
      QMAX=0.
      QMIN=999999999.
      DO 150 I=1,NPLOTS
      I1=(IDA-IDADAT)*24/IDTQ(I)+IHR/IDTQ(I)
      IF(IHR/IDTQ(I).EQ.0)I1=I1+1
      I2=(LDA-IDADAT)*24/IDTQ(I)+LHR/IDTQ(I)
      I1=I1+LPLOTQ(I)-1
      I2=I2+LPLOTQ(I)-1
      DO 150 J=I1,I2
      IF(D(J).GT.1.0E-7)GO TO 149
      GO TO 150
  149 IF(D(J).LT.QMIN)QMIN=D(J)
      IF(D(J).GT.QMAX)QMAX=D(J)
  150 CONTINUE
C
C     ESTABLISH SCALE FOR PLOT
C
      IF(METRIC.EQ.1)GO TO 151
      QMIN=QMIN*35.3147
      QMAX=QMAX*35.3147
  151 RATIO=QMAX/QMIN
      IF(RATIO.LE.5.)QMAX=QMAX*5.
      IF(QMAX.GT.10.)GO TO 152
      N=QMAX+1.
      PMAX=FLOAT(N)
      IF(PMAX.GT.10.)PMAX=10.
      GO TO 22
  152 DO 20 J=1,75
      X=10.**J
      IF((QMAX/X).LE.10.) GO TO 165
      GO TO 20
  165 N=(QMAX/X)+1.00001
      IF(N.GT.10)N=10
      PMAX=N*X
      GO TO 22
   20 CONTINUE
C
C     SET SCALES
   22 CONTINUE
      DO 24 I=1,10
      DEC=I
   24 SCALE(I)=DEC*0.1*PMAX
      DIV=PMAX/100.
C
C     SET UP ORD ARRAY
C
      DO 160 I=1,101,10
      ORD(I)=DOT
      IF(I.EQ.101) GO TO 160
      DO 170 J=1,9
  170 ORD(I+J)=BLANK
  160 CONTINUE
C
      RETURN
C
      END
