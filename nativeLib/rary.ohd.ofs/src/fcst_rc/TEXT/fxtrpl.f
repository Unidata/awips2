C MEMBER FXTRPL
C  (from old member FCFXTRPL)
C
C @PROCESS LVL(77)
C   DESC -- LOG/LINEAR EXTRAPOLATION GIVEN XX TO FIND YY
C
C                             LAST UPDATE: 02/09/94.07:49:23 BY $WC30KH
C
C ......................................................................
C
      SUBROUTINE FXTRPL(XXO,XVECT,N1,YVECT,YY,IBUG)
C
C .....................................................................
C
C THIS SUBROUTINE EXTRAPOLATES LOGARITHMICALLY THE GIVEN
C XVECT TO THE POINT 'XX' AND THEN FINDS THE CORRESPONDING
C VALUE 'YY' FROM THE EXTRAPOLATED YVECT
C
C    ARGUMENT LIST:
C      XXO     - GIVEN VALUE TO BE EXTRAPOLATED FROM X-VECTOR
C      XVECT  - VECTOR OF FIRST ELEMENT OF ORDERED PAIRS
C      N1     - INDEX OF LAST POINT OF R.C. AT EXTREMITY WHERE
C                                 EXTRAPOLATION IS TO BE DONE
C      YVECT  - VECTOR OF SECOND ELEMENT OF ORDERED PAIRS
C      YY      - OUTPUT VALUE ASSOC. WITH GIVEN XX
C
C.......................................................................
C
C   SUBROUTINE ORIGINALLY WRITTEN BY --
C       JONATHAN WETMORE - HRL - 801031
C
C.......................................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fratng'
C
C
      DIMENSION SUBNAM(2),OLDSUB(2)
       DIMENSION XVECT(*),YVECT(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fxtrpl.f,v $
     . $',                                                             '
     .$Id: fxtrpl.f,v 1.1 1995/09/17 19:05:22 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HFXTR,4HPL  /
C
C  SET /WHERE/ INFO
C
      XX=XXO
      DO 500 I=1,2
      OLDSUB(I) = OPNAME(I)
  500 OPNAME(I) = SUBNAM(I)
      IOLDOP = IOPNUM
      IOPNUM = 0
C
      IF (ITRACE.GE.2) WRITE(IODBUG,601)
  601 FORMAT(1H ,21H *** ENTER FXTRPL ***)
C
C  SET INDECES
C
       IF(N1.EQ.1) N2=2
       IF(N1.GT.1) N2=N1-1
C
      X1=XVECT(N1)
      X2=XVECT(N2)
      Y1=YVECT(N1)
      Y2=YVECT(N2)
      IF(IBUG.GE.1) WRITE(IODBUG,603) XX,X1,X2,Y1,Y2
  603 FORMAT(1H0,10X,'GIVEN XX VALUE FOR EXTRAPOLATION =',G15.7,
     1/10X,'EXTREME X-VECTOR VALUES =',2G15.7,
     1/10X,'EXTREME Y-VECTOR VALUES =',2G15.7)
C
      IF (EMPTY(4).GT.1.0) GO TO 120
      X1=ALOG(X1)
      X2=ALOG(X2)
      Y1=ALOG(Y1)
      Y2=ALOG(Y2)
      XX=ALOG(XX)
  120 DYDX=(Y1-Y2)/(X1-X2)
      YY=(XX-X1)*DYDX+Y1
C
      IF(IBUG.GE.1) WRITE(IODBUG,607)X1,X2,Y1,Y2,XX,DYDX,
     1YY
 607  FORMAT(1H0,10X,3HX1=,G15.7,2X,3HX2=,G15.7,2X,3HY1=,G15.7,
     12X,3HY2=,G15.7,
     1/10X,3HXX=,G15.7,2X,5HDYDX=,G15.7,2X,3HYY=,G15.7)
C
      IF (EMPTY(4) .GT. 1.0) GO TO 130
      IF (YY .LT. -100.0) YY = -100.0
      YY=EXP(YY)
  130 CONTINUE
C
      IF(IBUG.GE.1) WRITE(IODBUG,604) YY
 604  FORMAT(1H0,10X,24H EXTRAPOLTED YY VALUE = ,G15.7)
C
C
      IF(ITRACE.GE.2) WRITE(IODBUG,602)
  602 FORMAT(1H0,20H *** EXIT FXTRPL ***)
      OPNAME(1) = OLDSUB(1)
      OPNAME(2) = OLDSUB(2)
      IOPNUM = IOLDOP
C
      RETURN
      END
