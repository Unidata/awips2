      SUBROUTINE UVCOMP(DIR,SPD,U,V,NLVLS)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This subroutine computes rectangular wind components given wind direction
C and speed.
C
C History.
C --------                    
C Don Baker      01 Jul 84    Original version.
C Don Baker      15 May 85    Modified for CWP.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C DIR         Real Array    Wind directions (deg clockwise from north).
C SPD         Real Array    Wind speeds (m/s).
C NLVLS       Integer       Number of winds passed.
C
C On output:
C ----------               
C U           Real Array    U (east-west) wind component (m/s).
C V           Real Array    V (north-south) wind component (m/s).
C
C User notes:
C -----------
C 1) This routine can be used to return components for just one wind
C    report if NLVLS is passed as 1.
C
C
C Input arguments.
C
      REAL DIR(1),SPD(1)
      INTEGER NLVLS
C
C Output arguments.
C
      REAL U(1),V(1)
C
C Internal variables.
C
      REAL ANGLE
      INTEGER I
C
C Subroutine constants.
C
      REAL FLAG,RPD
      PARAMETER (FLAG=99999.,RPD=0.0174533)
C
C Calculate components.  Assign flag if direction or speed is bad.
C
      DO 100 I=1,NLVLS
         IF (DIR(I).LT.0. .OR. DIR(I).GT.360. .OR.
     +       SPD(I).LT.0. .OR. SPD(I).GT.250.) THEN
            U(I)=FLAG
            V(I)=FLAG
         ELSE
            ANGLE=RPD*DIR(I)
            U(I)=(-SPD(I))*SIN(ANGLE)
            V(I)=(-SPD(I))*COS(ANGLE)
         ENDIF
 100  CONTINUE
C
C EXIT.
C
      RETURN
      END
