      SUBROUTINE VVEL(pcb,PEQLEV,P,HT,TP,TVE,TVP,WLCL,NPAR,VV,VVMAX)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine uses a 1-dimensional cloud model to compute the vertical
C velocity profile of the lifted parcel.
C
C History.
C --------
C John Weaver    ???? 1979    Original version at NSSL.
C Tom Schlatter  ???? 1981    Adapted code to the PROFS VAX system.
C Don Baker      01 Jul 84    Converted code to a singular module.
C Don Baker      01 Jul 85    Adapted code for CWP.
C Dale Perry        Sep 96    Adapted code for WFO.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C PEQLEV      Real          Equilibrium level pressure (mb).
C P           Real Array    Lifted parcel pressure levels (mb).
C HT          Real Array    Lifted parcel heights (m asl).
C TP          Real Array    Lifted parcel temperatures (K).
C TVE         Real Array    Sounding virtual temperatures at parcel pressure
C                           levels (K).
C TVP         Real Array    Parcel virtual temperatures at parcel pressure
C                           levels (K).
C WLCL        Real Array    Mixing ratio at the LCL (g/kg).
C NPAR        Integer       Number of lifted parcel levels passed.
C
C On output:
C ----------
C VV          Real Array    Parcel positive vertical velocity at each
C                           parcel level (m/s).
C VVMAX       Real          Maximum parcel vertical velocity (m/s)
C                           up to the equilibrium level.
C
C User notes:
C -----------
C 1) Precipitation drag and entrainment have been crudely accounted for
C    by empirically derived coefficients which workers at NSSL developed
C    to get better agreement between computed and observed cloud tops.
C
C
C Input arguments.
C
      INTEGER NPAR
      REAL HT(NPAR),TP(NPAR),TVE(NPAR),TVP(NPAR),P(NPAR)
      REAL WLCL,PEQLEV,pcb
C
C Output arguments.
C
      REAL VV(1),VVMAX
C
C Internal variables.
C
      REAL WATER(NPAR)
      REAL E,DH,DTMP,TEM,TPM,WM,VV2
      INTEGER I
C
C External functions.
C
      REAL ESAT
C
C Subroutine constants.
C
      REAL DRAG,ENTRN,GE
      PARAMETER (DRAG=0.33,ENTRN=0.67,GE=9.8)
C
C Compute theoretical condensed liquid water of the parcel at each level
C of the lifted parcel.
C
      DO 100 I=1,NPAR
         E=ESAT(TP(I))
         WATER(I)=WLCL-((0.622*E)/(P(I)-E))
 100  CONTINUE
C
C Compute the vertical velocity at each level.  If no buoyancy exists,
C then the vertical velocity is set to zero.  A derivation of this formula
C can be found in math notes by Tom Schlatter.
C
      VV(1)=0.0
      VVMAX=VV(1)
      DO 150 I=2,NPAR
         VV(I)=0.0
         If (P(i).gt.pcb) Go To 150
         DH=HT(I)-HT(I-1)
         WM=0.0005*(WATER(I)+WATER(I-1))
         TEM=0.5*(TVE(I)+TVE(I-1))
         TPM=0.5*(TVP(I)+TVP(I-1))
         DTMP=TPM-TEM
         VV2=VV(I-1)*VV(I-1)+2*GE*DH*(ENTRN*DTMP/TEM-DRAG*WM)
         IF (VV2.GT.0.) VV(I)=SQRT(VV2)
         IF (P(I).GE.PEQLEV) THEN
            IF (VVMAX.LT.VV(I)) VVMAX=VV(I)
         ENDIF
 150  CONTINUE
C
C Exit.
C
      RETURN
      END
