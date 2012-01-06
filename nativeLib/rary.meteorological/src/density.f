      SUBROUTINE DENSITY(P,TVIR,NLVLS,RHO)
      IMPLICIT NONE

C Statement of purpose.
C ---------------------
C Compute the air density at each level of a sounding.
C
C History.
C --------                    
C D. Baker       01 Jul 84    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C P           Real Array    Sounding pressures (mb).
C TVIR        Real Array    Sounding virtual temperatures (C).
C NLVLS       Integer       Number of sounding levels passed.
C
C On output:
C ----------               
C RHO         Real Array    Air density at each level (kg/cu m).


C---- Variable/array declarations.

      INTEGER I,NLVLS
      REAL P(NLVLS),TVIR(NLVLS),RHO(NLVLS)

C---- Calculate density at each level.

      DO I=1,NLVLS
       RHO(I)=100.*P(I)/(287.04*TVIR(I))
      END DO

C---- Exit.

      RETURN
      END
