      SUBROUTINE HAILSIZ(VVMAX,HSIZE)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This subroutine computes the largest hailstone that can be supported
C by the undiluted parcel updraft.
C
C History.
C --------                    
C Don Baker      28 Jun 85    Original version based on the equation given
C                             in the article in the BAMS, Vol. 62, No. 11,
C                             November 1981 concerning the Ft. Collins, CO
C                             hailstorm of 30 Jul 79.
C
C Dale Perry        Sep 96    Adapted code to work on WFO
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C VVMAX       Real          Maximum undiluted parcel vertical velocity.
C
C On output:
C ----------               
C HSIZE       Real          The diameter of the maximum size hailstone that
C                           could be supported by the parcel updraft (cm).
C User notes:
C -----------
C 1) Hail is assumed to be a spherical object of density 90% that of water.
C 2) The hail size is the size of an object of above density that has a
C    terminal velocity equal to the maximum vertical velocity.  This is
C    the size of hail aloft and is usually an exaggeration of the actual
C    hail size expected at the ground.
C 3) The air density below was computed for a temperature of 22 deg celsius
C    at a pressure of 850 mb (parameter RHOAIR).
C
C
C Input arguments.
C
      REAL VVMAX
C
C Output arguments.
C
      REAL HSIZE
C
C Subroutine constants.
C
      REAL DRAG,RHOICE,RHOAIR,GE
      PARAMETER (DRAG=0.55,RHOICE=900.,RHOAIR=1.0033,GE=9.8)
C
C Compute hail size by solving the terminal velocity equation (diameter).
C
      HSIZE=2*((3*DRAG*RHOAIR*(VVMAX*VVMAX))/(8*GE*RHOICE))*100
C
C Exit.
C
      RETURN
      END
