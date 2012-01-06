	SUBROUTINE SOLAX (  JULDAY,       !Julian day (ddd)
     .			    MONTH,        !Month number of year
     .			    SLAT,	  !Station latitude (deg)
     .			    TYMINC,	  !Summation time increment (min)
     .			    TSTART,	  !Local time to start summation (hr)
     .			    TSTOP,	  !Local time (24 hr clock) to stop
     .			    TSRAD)	  !Output: solar rad. @ top of atm (ly)

C==========================================================================
C   This routine computes the total solar radiation incident at the top of
C   the atmosphere (undepleted).
C==========================================================================
      Implicit None
c declare formal arguments
      integer*2 julday, month
      real*4    slat
      integer*2 tyminc, tstart, tstop
      real*4    tsrad
c
	REAL      OPTDPH(12),
     .		  SDF(12)
c ----------------
      real*4 anginc, cn, cosx, cosz, cos89, 
     +       darc, declnatn, deg2rad, delta, dt, ecc, 
     +       hlfangl, hrang, kstatus,
     +       phi, PI, radx, ratio, s, sdecmx, sdiff, 
     +       sdrct, secz, sinx, strtang, tau, time, twopi, 
     +       veday, yrlen
c ----------------
	DATA      PI /3.1415927/,     !Value of PI (rad)
     .		  YRLEN /365.2563/,   !Average length of one year (days)
     .		  ECC /0.0167330/,    !Eccentricity of earth's orbit
     .		  COS89 /0.01745241/, !Cosine of 89 degrees
     .		  S /1.94/,           !Solar constant (ly/min)
     .		  SDECMX /0.40927/,   !Angle between the earth's equatorial
     .				      !plane and orbital plane (rad)
     .		  VEDAY /79.25/,      !Mean julian day of vernal equinox
     .		  CN /1.0/,           !Clearness number
     .
     .            OPTDPH /.142,.144,.156,.180,
     .			  .196,.205,.207,.201,
     .			  .177,.160,.149,.142/,

     .		  SDF /.058,.060,.071,.097,
     .		       .121,.134,.136,.122,
     .		       .092,.073,.063,.057/


C---------------------------------------------------------------------------

C===========================================================================
C   Initialize value of twice PI and the conversion from degrees to radians
C===========================================================================

	TWOPI= 2.*PI
	DEG2RAD= PI/180.

C==============================================================================
C   TAU approximates the eccentric anomaly (radians) of the earth in its orbit
C   about the sun. The approximation works because the earth's orbit is
C   very nearly circular and because perihelion occurs during the first few
C   days of the year.
C==============================================================================

	TAU= (TWOPI*JULDAY)/YRLEN

C===========================================================================
C   Compute the ratio of the mean to instantaneous sun-earth distance. The
C   mean distance from the earth to the sun is defined as one astronomical
C   unit (AU). It is the mean value of the greatest and least distances (at
C   aphelion and perihelion). The sum of these two distances is 2a, where
C   a is the semi-major axis of the earth's orbit. Thus, 1 AU is equivalent
C   to a. The equation for the ratio of the distances comes from:
C   Cogley, J.G., 1979: "The Albedo of Water as a Function of Latitude",
C   Monthly Weather Review, Vol. 107, No. 6 (June), p.775. The denominator
C   comes from an equation for the instantaneous sun-earth distance in:
C   Smith, E.A., 1980: "Orbital Mechanics and Analytic Modeling of Meteor-
C   ological Satellite Orbits", Paper No. 321, Dept. of Atmospheric Science,
C   Colorado State University, Fort Collins, Colorado 80523, Eq. 5.75, p.65.
C============================================================================

	RATIO= 1./(1.-ECC*COS(TAU))

C=========================================================================
C   Compute the solar declination (formula also from Cogley, cited above)
C=========================================================================

	DECLNATN= SDECMX*SIN(TWOPI*(JULDAY-VEDAY)/YRLEN)

C===============================
C   Convert latitude to radians
C===============================

	PHI= SLAT*DEG2RAD

C==========================================================================
C   Compute the angle increment (ANGINC) in degrees. This comes from DARC,
C   the number of degrees through which the earth turns per unit time.
C==========================================================================

	DARC= 0.25*TYMINC    !0.25 because earth rotates 360 deg per 1440 mins
	HLFANGL= 0.5*DARC
	ANGINC= DARC

C========================================================================
C   Compute starting angle (STRTANG) and the hour angle (HRANG) which is 
C   zero at solar noon.
C========================================================================

	STRTANG= 180.+TSTART*15.
	HRANG= STRTANG+HLFANGL

C============================================================================
C   Convert time increment to fraction of an hour. Initialize time and solar
C   radiation (TSRAD).
C============================================================================

	DT= TYMINC/60.
	TIME= TSTART
	TSRAD= 0.

C===========================================================================
C   Loop 140 computes the summed incident solar radiation at the top of the
C   atmosphere.
C===========================================================================

 140	CONTINUE

C=======================================================
C   Compute the cosine of the solar zenith angle (COSZ)
C=======================================================

	DELTA= DECLNATN
	SINX= SIN(PHI)*SIN(DELTA)
	COSX= COS(PHI)*COS(DELTA)
	COSZ= SINX+(COSX*(COS(HRANG*DEG2RAD)))
	IF (ABS(COSZ).LT.COS89) THEN
	   IF (COSZ.LT.0.) COSZ= -COS89
	   IF (COSZ.GT.0.) COSZ= COS89
	ENDIF
	SECZ= 1./COSZ

C===========================================================
C   Compute the shortwave flux at the top of the atmosphere.
C===========================================================

	SDRCT= CN*S*(RATIO**2)*COSZ*EXP((-OPTDPH(MONTH))*SECZ)
	SDIFF= SDF(MONTH)*(SDRCT/(CN*CN))
	RADX= (SDRCT+SDIFF)*TYMINC

C==================
C   Sum the fluxes
C==================

	IF (RADX.GT.0.) TSRAD= TSRAD+RADX

C===========================================================================
C   Increment hour angle and time step and continue iteration (if necessary)
C===========================================================================

	HRANG= HRANG+ANGINC
	TIME= TIME+DT
	IF (TIME.LE.TSTOP) GO TO 140

C=================
C   Normal return
C=================

	KSTATUS= 1
	RETURN
	END
