C MEMBER LPLT12
C  (from old member FCEX12)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/27/95.08:22:15 BY $WC20SV
C
C @PROCESS LVL(77)
C
      INTEGER FUNCTION LPLT12(X,Y)
C
C     THIS FUNCION COMPUTES THE PLOTTING POSITION
C     FOR VALUE X GIVEN ARRAY Y
C
      DIMENSION Y(11)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_insqplot/RCS/lplt12.f,v $
     . $',                                                             '
     .$Id: lplt12.f,v 1.1 1995/09/17 18:58:26 dws Exp $
     . $' /
C    ===================================================================
C
      LPLT12=0
      IF(X.LT.Y(1).OR.X.GT.Y(11))GO TO 20
C
      YM1=Y(1)
      DO 10 I=1,11
      IF(X.GE.Y(I).OR.X.LT.YM1)GO TO 9
      PORTON=(Y(I)-X) / (Y(I)-YM1)
      LPLT12 = (I-1-PORTON)*10 + 1.5
      GO TO 20
    9 YM1=Y(I)
   10 CONTINUE
C
      LPLT12=101
C
   20 RETURN
C
      END
