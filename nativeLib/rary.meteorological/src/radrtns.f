	SUBROUTINE RADRTNS
C***Some solar radiation routines from SERI

C	G. Williams	   Oct 81	From SERI
C	J. Wakefield	   Jan 82	Removed entry points RADVEC (replaced
C					by RADNORM) and SOLDEC (replaced by
C					TIMEQ and SOLDEC in SUNFUNCS.FOR).
C					Also changed argument lists in ETRxxx.
C			   Feb 82	Made SUNSET a function, and in radians
C			19 Feb 82	Added ERTAVG.
      Implicit None

	REAL*4 LAT,DEC,SSHA,DO,DETRHZ
	REAL*4 PI,RPH,RPD
	REAL*4 MIDHR,HETRHZ,HETRDN
c ----------------
      real     hpr
c ----------------

C***Constants and conversions
	parameter (PI = 3.1415927)
	parameter (RPD = PI/180.)
	parameter (RPH = PI/12.)
	parameter (HPR = 12./PI)

C------------------------------------------------------------------------------
	ENTRY ETRAVG(DO,LAT,DEC,HOUR,DELTIM,HETRHZ,HETRDN)
C***Compute extraterrestrial radiation (ETR) averaged over given time period
C***for a horizontal sfc and direct normal

C***Arguments
C	DO	Input	Solar flux at top of atm. (W/m**2)
C	LAT	Input	Latitude of location (radians)
C	DEC	Input	Solar declination (radians)
C	HOUR	Input	Time (decimal hours)
C	DELTIM	Input	Averaging time (decimal hours)
C	HETRHZ	Output	Average ETR on horiz sfc (J/m**2)
C	HETRDN	Output	Average direct normal ETR (J/m**2)


c ----------------
      real     delta, deltim, hour, t, timss, tmabove, xlha
      external sunset
      real     sunset 
c ----------------
C***Constants and conversions
	DELTA = DELTIM*RPH
	XLHA =(HOUR - 12.)*RPH

C***Check for sunrise/sunset during interval
	SSHA=SUNSET(LAT,DEC)
	TIMSS = SSHA - ABS(XLHA)

	IF(TIMSS.LT.-DELTA)THEN			!Dark
	 HETRDN = 0
	 HETRHZ = 0

	ELSE IF(ABS(TIMSS).LT.DELTA)THEN	!Partial sun
	 TMABOVE = TIMSS + DELTA
	 HETRDN = DO*TMABOVE*HPR
	 T = COS(LAT)*COS(DEC) * (SIN(SSHA)-SIN(SSHA-TMABOVE))
	 HETRHZ = HETRDN*(SIN(LAT)*SIN(DEC) + T/TMABOVE)

	ELSE					!Light
	 HETRDN = DO
	 T = SIN(LAT)*SIN(DEC) +
     +	    (SIN(DELTA)/DELTA)*COS(LAT)*COS(DEC)*COS(XLHA)
	 HETRHZ = HETRDN*T 
	ENDIF

	RETURN
C------------------------------------------------------------------------------
	ENTRY ETRHRLY(DO,LAT,DEC,MIDHR,HETRHZ,HETRDN)
C***Compute hourly extraterrestrial radiation (ETR)
C***for a horizontal sfc and direct normal

C***Arguments
C	DO	Input	SOLAR FLUX AT top of atm. (W/m**2)
C	LAT	Input	LATITUDE OF LOCATION (Radians)
C	DEC	Input	SOLAR DECLINATION (Radians)
C	MIDHR	Input	MID-POINT IN HOUR (DECIMAL HOURS)
C	HETRHZ	Output	HOURLY ETR ON HORIZ (J/m**2)
C	HETRDN	Output	HOURLY DIRECT normal ETR (J/m**2)

C***Constants and conversions
	DELTA = .5*RPH
	XLHA =(MIDHR - 12.)*RPH

C***Check for sunrise/sunset during interval
	SSHA=SUNSET(LAT,DEC)
	TIMSS = SSHA - ABS(XLHA)

	IF(TIMSS.LT.-DELTA)THEN			!Dark
	 HETRDN = 0
	 HETRHZ = 0

	ELSE IF(ABS(TIMSS).LT.DELTA)THEN	!Partial sun
	 TMABOVE = TIMSS + DELTA
	 HETRDN = DO*TMABOVE*HPR
	 T = COS(LAT)*COS(DEC) * (SIN(SSHA)-SIN(SSHA-TMABOVE))
	 HETRHZ = HETRDN*(SIN(LAT)*SIN(DEC) + T/TMABOVE)

	ELSE					!Light
	 HETRDN = DO
	 T = SIN(LAT)*SIN(DEC) +
     +	    (SIN(DELTA)/DELTA)*COS(LAT)*COS(DEC)*COS(XLHA)
	 HETRHZ = HETRDN*T 
	ENDIF

	RETURN
C------------------------------------------------------------------------------
	ENTRY ETRDAY(LAT,DEC,DO,DETRHZ)
C***Compute the daily total ETR on a horizontal sfc.

C***Argument definition
	! ARGUMENT	I/O	DESCRIPTION		UNITS
	!
	! LAT		I	LATITUDE OF LOCATION	Radians
	! DEC		I	SOLAR DECLINATION	Radians
	! DO 		I	SOLAR FLUX AT		ENERGY/AREA/HR
	!			  TOP OF ATM.
	! DETRHZ	O	DAILY TOTAL ETR		ENERGY/AREA
	!			  ON HORIZONTAL 
C***Constants and conversions
c	PI = 3.1415927
	SSHA=SUNSET(LAT,DEC)

C***Compute daily total ETR on a horizontal sfc.
	DETRHZ = COS(LAT)*COS(DEC)*SIN(SSHA) + SSHA*SIN(LAT)*SIN(DEC)
	DETRHZ = (24/PI)*DO*DETRHZ

	RETURN
	END
C------------------------------------------------------------------------------
	real FUNCTION SUNSET(LAT,DEC)
C***Compute sunset hour angle in hours.
C***Input LAT and DEC are latitude and solar declination in radians.
      Implicit None
c declare formal arguments
      REAL*4 LAT
      real   dec
c
	REAL*4 drad, PI, sqr, temp, tmpsq
	parameter (PI = 3.1415927)
c
	TEMP = (-TAN(LAT))*TAN(DEC)
	TMPSQ=TEMP*TEMP

	IF(TMPSQ.LT.1)THEN
	 SQR = SQRT(1.-TMPSQ)
	 DRAD = PI*.5 - ATAN(TEMP/SQR)
	ELSE
	 DRAD = 0
	 IF(TEMP.LT.0)DRAD = PI
	ENDIF

	SUNSET = DRAD

	RETURN
	END
