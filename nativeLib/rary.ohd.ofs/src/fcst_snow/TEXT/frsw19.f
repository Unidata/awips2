C MEMBER FRSW19
C  (from old member FCFSNWTR)
C
      SUBROUTINE FRSW19(A,IJ,LJ,V,IHZ,IJH,LJH,
     1 IT,LW,IBUG,IODBUG)
C.......................................
C     THIS SUBROUTINE STORES RAINSNOW MOD INPUT INTO A WORK ARRAY.
C.......................................
C     WRITTEN BY ERIC ANDERSON--HRL, JAN. 1985.
C.......................................
      DIMENSION A(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/frsw19.f,v $
     . $',                                                             '
     .$Id: frsw19.f,v 1.2 1995/11/14 20:09:18 erb Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK IF WITHIN RUN PERIOD.
      IF((IJ.GT.LJH).OR.(LJ.LT.IJH)) GO TO 190
C
C     TRUNCATE PORTION OUTSIDE RUN PERIOD
      IF(IJ.LT.IJH) IJ=IJH
      IF(LJ.GT.LJH) LJ=LJH
C
C     ADJUST TO CLOSEST TIME INTERVAL
      IJ=((IJ+IT/2)/IT)*IT
      LJ=((LJ+IT/2)/IT)*IT
C
C     COMPUTE WORK ARRAY POSITIONS.
      I1=(IJ-IHZ)/IT
      I2=(LJ-IHZ)/IT
C
C     STORE IN WORK ARRAY.
      DO 100 I=I1,I2
      IF(A(I).GE.0.0) GO TO 100
      A(I)=V
 100  CONTINUE
C.......................................
C     CHECK FOR DEBUG OUTPUT.
 190  IF(IBUG.EQ.0)RETURN
      WRITE(IODBUG,900)
 900  FORMAT (1H0,17HFSNWTR WORK ARRAY)
      WRITE(IODBUG,901)(A(I),I=1,LW)
 901  FORMAT(1H ,12F10.3)
C.......................................
      RETURN
      END
