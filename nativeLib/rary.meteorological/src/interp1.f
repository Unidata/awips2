C---- Given Y1 AT X1, Y3 AT X3, and X2, calculate Y2 (INTERP) AT X2.

      REAL FUNCTION INTERP1(Y1,Y3,X1,X2,X3)
      IMPLICIT NONE
      REAL Y1,Y3,X1,X2,X3,D
      D=X3-X1
      IF (D.EQ.0) D=0.01
      INTERP1=Y1+((Y3-Y1)*((X2-X1)/(D)))
      RETURN
      END
