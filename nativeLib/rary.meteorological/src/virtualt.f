      SUBROUTINE VIRTUALT(T,TD,P,NLVLS,TVIR)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes virtual temperature at all sounding levels.
C
C History.
C --------                    
C Don Baker      01 Jul 84    Original version.
C Don Baker      02 Jul 85    Adapted for CWP.
C Dale Perry     18 Sep 96    Adapted for WFO.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C T           Real Array    Sounding temperatures (K).
C TD          Real Array    Sounding dew points (K).
C P           Real Array    Sounding pressures (mb).
C NLVLS       Integer       Number of sounding levels passed.
C
C On output:
C ----------               
C TVIR        Real Array    Sounding virtual temperatures (K).
C
C
C Input arguments.
C
      INTEGER NLVLS
      REAL T(NLVLS),TD(NLVLS),P(NLVLS)
C
C Output arguments.
C
      REAL TVIR(NLVLS)
C
C Internal variables.
C
      INTEGER I
C
C External functions.
C
      REAL VIRTTEMP
C
C Compute virtual temperature at each level.  Note:  routine VIRTTEMP
C can take a temperature input of C or K in it's computations.  Here
C we assume the sounding temps are in Kelvin.
C
      DO 1 I=1,NLVLS
         TVIR(I)=VIRTTEMP(T(I),TD(I),P(I))    
 1    CONTINUE
C
C Exit.
C
      RETURN
      END
