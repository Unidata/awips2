C MEMBER FTERPL
C  (from old member FCFTERPL)
C
C @PROCESS LVL(77)
C  DESC -- LOG-LOG/LINEAR INTERPOLATION GIVEN XX TO FIND YY
C
C                             LAST UPDATE: 10/27/94.11:16:55 BY $WC30KH
C
C***********************************************************************
C
      SUBROUTINE FTERPL(XXO,XVECT,N,YVECT,YY,IBUG)
C
C***********************************************************************
C
C THIS SUBROUTINE INTERPOLATES THE INPUT VALUE XX INTO XVECT AND
C      FINDS AND OUTPUTS THE CORRESPONDING VALUE YY FROM YVECT
C
C    ARGUMENT LIST:
C      XXO     - GIVEN VALUE TO BE INTERPOLATED INTO X-VECTOR
C      XVECT  - VECTOR OF FIRST ELEMENT OF ORDERED PAIRS
C      N      - NUMBER OF ORDERED PAIRS IN XVECT/YVECT
C      YVECT  - VECTOR OF SECOND ELEMENT OF ORDERED PAIRS
C      YY      - OUTPUT VALUE ASSOC. WITH GIVEN XX
C
C.......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JONATHAN WETMORE - HRL - 801031
C
C.......................................................................
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fratng'
C
      DIMENSION XVECT(*),YVECT(*)
      DIMENSION SUBNAM(2),OLDSUB(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fterpl.f,v $
     . $',                                                             '
     .$Id: fterpl.f,v 1.3 2004/06/17 12:50:00 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HFTER,4HPL  /
C
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
  601 FORMAT(1H ,21H *** ENTER FTERPL ***)
C
      IF(IBUG.GE.1) WRITE(IODBUG,603) XX,XVECT(1),XVECT(N),YVECT(1),
     1YVECT(N)
  603 FORMAT(1H0,10X,'GIVEN XX VALUE FOR INTERPOLATION =',F10.3,
     1/10X,'EXTREME X-VECTOR VALUES =',2F10.3,
     1/10X,'EXTREME Y-VECTOR VALUES=',2F10.3)
C
      IF(XX.GT.XVECT(1))GO TO 100
      YY=YVECT(1)
      GO TO 600
C
  100 DO 10 I=2,N
C  THE FOLLOWING CHANGE MADE ON 02/05/92 -- MAIN. #0673
C  N2 IS NOT DEFINED IF XX > XVECT(N)
      N2=I
      IF(XX-XVECT(I)) 20,20,10
C  END OF CHANGE OF 02/05/92
 10   CONTINUE
 20   N1=N2-1
      X1=XVECT(N1)
      X2=XVECT(N2)
      Y1=YVECT(N1)
      Y2=YVECT(N2)
C jgg replaced for bug r24-37 5/04
C  I'm not sure what case this statement was trying to solve but it 
C  effectively eliminated calculations for negative stages - I think 
C  this may have been what was intended in the fix below.     
C jgg      IF(X1.LT.0.0001) X1=0.0001
      IF(X1.LT.0.0001.AND.X1.GT.0) X1=0.0001
      IF(X1.GT.-0.0001.AND.X1.LT.0) X1=-0.0001
C jgg end

cfan  HSD Bugr22-26
cfan  both Y1 and Y2 can be less than 0 (negative stage is reasonable)
cfan  Y1 should less than Y2 during the interpolation.
cfan  So there is problem when Y1 and y2 are less than 0.
cfan  IF(Y1.LT.0.0001) Y1=0.0001
      IF(XX.LT.X1) XX=X1

C  THE FOLLOWING CHANGE MADE ON 02/05/92 -- MAIN. #0673
      DYDX=(Y2-Y1)/(X2-X1)
      YLIN=(XX-X1)*DYDX+Y1
      IF(IBUG.GE.1) WRITE(IODBUG,607)X1,X2,Y1,Y2,XX,DYDX,
     1YLIN
C      IF(IBUG.GE.1) WRITE(IODBUG,605) X1,X2,Y1,Y2
C 605  FORMAT(1H0,2X,27HBRACKETING X-VECT VALUES = ,2F10.3,/2X,27HBRACKE
C     1ING Y-VECT VALUES = ,2F10.3,/)
C
      IF (EMPTY(4) .GT. 1.0) GO TO 830
C  END OF CHANGE OF 02/05/92
      X1=ALOG(X1)
      X2=ALOG(X2)
      Y1=ALOG(Y1)
      Y2=ALOG(Y2)
      XX=ALOG(XX)
C  THE FOLLOWING CHANGE MADE ON 02/05/92 -- MAIN. #0673
      DYDX=(Y2-Y1)/(X2-X1)
      YLOG=(XX-X1)*DYDX+Y1
  830 IF(IBUG.GE.1) WRITE(IODBUG,607)X1,X2,Y1,Y2,XX,DYDX,
     1YLOG
C  END OF CHANGE OF 02/05/92
 607  FORMAT(1H0,10X,3HX1=,G15.7,2X,3HX2=,G15.7,2X,3HY1=,G15.7,
     12X,3HY2=,G15.7,
     1/10X,3HXX=,G15.7,2X,5HDYDX=,G15.7,3HYY=,G15.7)
C
C  THE FOLLOWING CHANGE MADE ON 02/05/92 -- MAIN. #0673
C      IF (EMPTY(4) .LT. 1.0) YY=EXP(YY)
      YY=YLIN
      IF (EMPTY(4) .LT. 1. .AND. YLOG.LE.174.0) YY=EXP(YLOG)
C  END OF CHANGE OF 2/5/92
C
C
  600 IF(IBUG.GE.1) WRITE(IODBUG,6040) YY
 6040 FORMAT(1H0,10X,24H INTERPOLTED YY VALUE = ,G15.7)
C
      IF(ITRACE.GE.2) WRITE(IODBUG,604)
 604  FORMAT(1H0,20H *** EXIT FTERPL ***)
      OPNAME(1) = OLDSUB(1)
      OPNAME(2) = OLDSUB(2)
      IOPNUM = IOLDOP
C
      RETURN
      END
