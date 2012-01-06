      SUBROUTINE WBZERO(ELEV,P,HT,T,TD,NLVLS,PWBZ,HWBZ,TWBZ)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine determines the pressure, temperature, and height of the
C wet-bulb zero.
C
C History.
C --------                    
C Don Baker      01 Jul 84    Original version.
C Don Baker      12 May 85    Updated for CWP Spec Appendix V method.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C ELEV        Real          Station elevation (m asl).
C P           Real Array    Sounding pressures (mb).
C HT          Real Array    Sounding heights (m asl).
C T           Real Array    Sounding temperatures (C).
C TD          Real Array    Sounding dew points (C).
C NLVLS       Integer       Number of sounding levels passed.
C
C On output:
C ----------               
C PWBZ        Real          Pressure level of the wet-bulb zero (mb).
C HWBZ        Real          Height of the wet-bulb zero (m agl).
C TWBZ        Real          Temperature at the wet-bulb zero.
C
C 
C Input arguments.
C
      INTEGER NLVLS
      REAL HT(NLVLS),P(NLVLS),T(NLVLS),TD(NLVLS),ELEV
C
C Output arguments.
C
      REAL PWBZ,HWBZ,TWBZ
C
C Internal variables.
C
      REAL TWET1,TWET2,THETAE
      INTEGER I
C
C External functions.
C
      REAL INTERP1,TSA,EPT
C
C Subroutine constants.
C
      REAL ZERO
      PARAMETER (ZERO=273.15)
C
C Initilize the wet-bulb zero assuming wet-bulb zero below surface.
C
C      HWBZ=0.
      HWBZ=ELEV
      PWBZ=P(1)
      TWBZ=T(1)
C
C Test for surface wet-bulb temperature less than zero.  If so, exit
C with surface values assigned to wet-bulb zero level.
C
C      THETAE=MYTW(T(1),TD(1),P(1))
C      TWET1=TEMP_OF_TE(THETAE,P(1))
      THETAE=EPT(T(1),TD(1),P(1))
      TWET1=TSA(THETAE,P(1))
      IF (TWET1.LE.ZERO) GO TO 999
C
C Iterate upward through sounding until wet-bulb less than zero.  When
C this occurs, interpolate to get desired parameters at level where
C the wet-bulb temperature is zero.
C 
      DO 100 I=2,NLVLS
         THETAE=EPT(T(I),TD(I),P(I))
         TWET2=TSA(THETAE,P(I))
C         THETAE=MYTW(T(I),TD(I),P(I))
C         TWET2=TEMP_OF_TE(THETAE,P(I))
         IF (TWET2.LE.ZERO) THEN
            HWBZ=INTERP1(HT(I),HT(I-1),TWET1,ZERO,TWET2)
            PWBZ=INTERP1(P(I),P(I-1),HT(I),HWBZ,HT(I-1))
            TWBZ=INTERP1(T(I),T(I-1),HT(I),HWBZ,HT(I-1))
C            HWBZ=HWBZ-ELEV            
            GO TO 999
         ENDIF
         TWET1=TWET2
 100  CONTINUE
C
C Exit.
C
 999  CONTINUE
      RETURN
      END
