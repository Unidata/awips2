	SUBROUTINE RADIATION(LAT,LNG,LSM,JD,HR,BEXT,OD,SOLRAD)
C***Calculate mean solar radiation at ground

C	G. Williams	   Oct 81	From code at SERI
C	J. Wakefield	   Jan 82	Put everything in radians
C			29 Jan 82	Only calculate if sun above horizon
C			12 Feb 82	Made HR decimal hours (was integer)
C			25 Mar 82	Corrected sign error in TIMCORR
C	G. Williams	24 May 82	Changed extinction coefficient 
C					and optical depth from fixed
C					values to subroutine I/O
C	K. Brundage	18 Oct 84	Changed bug in optical depth

C***Arguments
C	LAT	Input	R*4 Latitude (degrees)
C	LNG	Input	R*4 Longitude (degrees)
C	LSM	Input	R*4 Longitude of standard meridian (degrees)
C	JD	Input	I*4 Julian day
C	HR	Input	R*4 Local standard time (decimal hours)
C	BEXT   Input 	R*4 Rayleigh extinction coefficient
C	OD	Input	R*4 Optical depth 
C	SOLRAD	Output	R*4 Mean solar radiation at ground (W/m**2)
c
      Implicit None
c declare formal arguments
      real*4    LAT, LNG, LSM
      integer*4 JD, HR
      real*4    BEXT, OD, SOLRAD
c
	REAL*4 AVGTIM, HOUR
c ----------------
      real*4    coszen, do, dec, eqt, erv, hetrdn, hetrhz, hrangle, 
     +          PI, pl, rpd, rph, rlat, sc, timcorr
      external  radnorm, soldec, timeq
      real      radnorm, soldec, timeq
c ----------------
	parameter(PI=3.14159265)

	RPD=PI/180.
	RPH=PI/12.
	RLAT=LAT*RPD
	SC = 1353.			!Solar constant in W/m**2

	AVGTIM=5./60.			!Average for 5 min period

	DEC=SOLDEC(JD)			!Solar declination
	HRANGLE = (HR-12.)*RPH
	COSZEN=SIN(RLAT)*SIN(DEC)+COS(RLAT)*COS(DEC)*COS(HRANGLE)

	IF(COSZEN.LE..0)THEN
	 SOLRAD=.0			!Sun at or below horizon
	ELSE
	 ERV=RADNORM(JD)		!Distance from sun factor
	 DO = SC*ERV			!Horiz rad at top of atmosphere

	 EQT=TIMEQ(JD)			!Equation of time
	 TIMCORR=(LNG-LSM)/15.+EQT/RPH	!In hours

C***Compute ETR for the hour
	 HOUR = HR-TIMCORR
	 CALL ETRAVG(DO,RLAT,DEC,HOUR,AVGTIM,HETRHZ,HETRDN)
	 PL = OD/COSZEN			!Path length
	 SOLRAD = HETRHZ*EXP((-BEXT)*PL)
	ENDIF

	RETURN
	END
