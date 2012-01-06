        SUBROUTINE MXTP (  ANSOL,    !Energy units derived from total heat (*)
     .			   DELTAP,   !Pressure increment
     .			   SFCP,     !Surface pressure (mb)
     .			   P2,       !Second pressure in array (mb)
     .	                   TL,       !Interpolated temperature array (C)
     .			   DELTAZ,   !Thickness array
     .			   LVL,      !Dimension of interpolated arrays
     .			   CTMAX)    !Forecast maximum temperature (C)

C       (*) Note: refer to comments in main program concerning these energy 
C                 units

C============================================================================
C   This routine forecasts the maximum temperature based on the total amount
C   of energy available for heating and the sounding temperature profile.
C============================================================================
      implicit none
c declare formal arguments
      real     ANSOL, DELTAP, SFCP, P2, TL(*), DELTAZ(*)
      integer  LVL
      real     CTMAX

C==========================
C   Variable declarations:
C==========================
c      REAL     TL(1), DELTAZ(1)
C	LOGICAL  BUGS
c ------------------------
      real       adtemp, delz, deldt, dsum, dpress, dt, ftmax,
     +           press, r, rbot, SFCT, tdiff 
      integer*2  mlyr
c ------------------------
      real*4 dlaps
      DATA   DLAPS /0.009767/   !Dry adiabatic lapse rate (deg C/m)

C===========================
C   Set surface temperature
C===========================

	SFCT= TL(1)

C===========================================================================
C   As a first guess, 70% of the available heat is added in the lowest
C   10 mb. This usually leads to an overestimate of the maximum temperature
C   on the first iteration.
C===========================================================================

	DT= 0.70*ANSOL

 20	CONTINUE
	PRESS= SFCP
	DPRESS= P2
	RBOT= (SFCP-P2)/DELTAP

C=========================================================================
C   DSUM contains the sum of the differences between the air temperature
C   at successive pressure levels and ADTEMP (temperature along a dry
C   adiabat). CTMAX is initialized to the first guess temperature along
C   the dry adiabat (i.e., 70% of ANSOL added to surface temperature).
C=========================================================================

	DSUM= 0.
	ADTEMP= SFCT+DT
	CTMAX= ADTEMP

C===========================================================================
C   This loop tests and sums the temperature differences between ADTEMP and
C   the environment (in DSUM)
C===========================================================================

	DO 100 MLYR= 1,LVL
	   R= 1.		         !R ranges from 0 to 1 in the first
	   IF (MLYR.EQ.1) R= RBOT	 !layer since it takes less than 1 unit
	   TDIFF= ADTEMP-TL(MLYR)	 !of ASOL to heat a layer <10 mb thick.
	   IF (TDIFF.LE.0.) GO TO 105
	   DSUM= DSUM+(TDIFF*R)
	   DELZ= DELTAZ(MLYR)
	   ADTEMP= ADTEMP-(DLAPS*DELZ)
	   PRESS= DPRESS
	   DPRESS= PRESS-DELTAP
	   IF (DPRESS.LT.400.) GO TO 105
 100	CONTINUE

C==========================================================================
C   When 400 mb is reached in the iteration, or, ADTEMP is less than the 
C   environmental temperature, control is transferred to this section
C   where it is determined whether a new value of ADTEMP is needed. ADTEMP
C   is recalculated if the difference between DSUM and ANSOL is larger than
C   2 units.
C==========================================================================

 105	CONTINUE
	DELDT= DSUM-ANSOL
	IF (ABS(DELDT).LE.2.) THEN
	   FTMAX= ctmax
	ELSE IF (DELDT.GT.2. .AND. DSUM.GT.0) THEN
	   DT= DT-0.5*DELDT/DSUM
	   IF (DT.GT.0.) GO TO 20
	ELSE
	   CTMAX= SFCT
	   FTMAX= ctmax
	ENDIF

C=================
C   Normal return
C=================

	RETURN
	END
