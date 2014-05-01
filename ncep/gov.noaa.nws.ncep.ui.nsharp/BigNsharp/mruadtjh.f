	SUBROUTINE MR_UADTJH  ( datman, nman, datsgt, nsgt, datsgw, nsgw,
     +			      datamn, namn, datast, nast, datasw, nasw,
     +			      datmnw, nmnw, datamw, namw, dattrp, ntrp,
     +			      datmxw, nmxw, datatr, natr, datamx, namx,
     +			      selv, iztype, stndat, nlev, idtype, iret )
C************************************************************************
C* MR_UADT                                                              *
C*									*
C* This subroutine merges mandatory and significant level data.		*
C*									*
C* MR_UADT  ( DATMAN, NMAN, DATSGT, NSGT, DATSGW, NSGW, DATAMN, NAMN,	*
C*            DATAST, NAST, DATASW, NASW, DATMNW, NMNW, DATAMW, NAMW,   *
C*	      DATTRP, NTRP, DATMXW, NMXW, DATATR, NATR, DATAMX, NAMX,   *
C*	      SELV, IZTYPE, STNDAT, NLEV, IDTYPE, IRET )		*
C*									*
C* Input parameters:							*
C*	DATMAN (6,NMAN)	REAL		Mandatory data below 100 mb	*
C*	NMAN		INTEGER		Number of levels		*
C*	DATSGT (3,NSGT)	REAL		Sig temp data below 100 mb	*
C*	NSGT		INTEGER		Number of levels		*
C*	DATSGW (3,NSGW)	REAL		Sig wind data below 100 mb	*
C*	NSGW		INTEGER		Number of levels		*
C*	DATAMN (6,NAMN)	REAL		Mandatory data above 100 mb	*
C*	NAMN		INTEGER		Number of levels		*
C*	DATAST (3,NAST)	REAL		Sig temp data above 100 mb	*
C*	NAST		INTEGER		Number of levels		*
C*	DATASW (3,NASW)	REAL		Sig wind data above 100 mb	*
C*	NASW		INTEGER		Number of levels		*
C*	DATMNW (3,NMNW)	REAL		Mandatory wind data below 100 mb*
C*	NMNW		INTEGER		Number of levels		*
C*	DATAMW (3,NAMW)	REAL		Mandatory wind data above 100 mb*
C*	NAMW		INTEGER		Number of levels		*
C*	DATTRP (5,NTRP)	REAL		Tropopause data below 100 mb	*
C*	NTRP		INTEGER		Number of levels		*
C*	DATMXW (3,NMXW)	REAL		Max wind data below 100 mb	*
C*	NMXW		INTEGER		Number of levels		*
C*	DATATR (5,NATR)	REAL		Tropopause data above 100 mb	*
C*	NATR		INTEGER		Number of levels		*
C*	DATAMX (3,NAMX)	REAL		Max wind data above 100 mb	*
C*	NAMX		INTEGER		Number of levels		*
C*	SELV		REAL		Surface elevation		*
C*	IZTYPE		INTEGER		Type of height interpolation	*
C*					  1 = int wrt log p		*
C*					  2 = moist hydrostatic comp	*
C*					  3 = scaled moist hydro comp	*
C*									*
C* Output parameters:							*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*	NLEV		INTEGER		Number of levels		*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/86						*
C* M. desJardins/GSFC	 9/87	Added sig winds on pressure surfaces	*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* M. desJardins/GSFC	 4/89	Changes for missing data		*
C* K. Brill/NMC		01/92	Pass IZTYPE=1 to MR_INTP; CALL MR_COND  *
C*			  	to add below ground mand lvls		*
C* D. Kidwell/NCEP	 2/01	Added mand wind args and call to MR_MANW*
C* D. Kidwell/NCEP	 3/01	Added tropopause and max wind data      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datman (6,*), datsgt (3,*), datsgw (3,*),
     +			datamn (6,*), datast (3,*), datasw (3,*),
     +			datmnw (3,*), datamw (3,*), dattrp (5,*),
     +			datmxw (3,*), datatr (5,*), datamx (3,*),
     +			stndat (6,*)
	INTEGER		idtype (*)
C
	INTEGER		ipt ( LLMXLV )
	REAL		sclhgt ( LLMXLV )
	LOGICAL		zbwind, zawind
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the scale height array.
C
	DO  i = 1, LLMXLV
	    sclhgt (i) = RMISSD
	END DO
C
C*	Check significant wind data to see if the data is reported on
C*	pressure or height surfaces.
C
	CALL MR_CHKW ( datsgw, nsgw, datasw, nasw, zbwind, zawind, ier )
C
C*	Find the surface data.
C
	CALL MR_SRFC  ( datman, nman, datsgt, nsgt, datsgw, nsgw, 
     +			zbwind, selv, nlev, ipt, stndat, idtype, ier )
C
C*	Merge the mandatory below 100 mb data and the mandatory above
C*	data.
C
	IF ( (nman .ge. 2) .or. (namn .ge. 1) ) 
     +	    CALL MR_MAND ( datman, nman, datamn, namn, nlev, ipt,
     +		           stndat, idtype, ier )
C
C*	Merge the mandatory below 100 mb wind data and the mandatory
C*	above wind data.
C
	IF ( ( (nmnw .ge. 1) .or. (namw .ge. 1) ) .and. (nlev .ge. 2) )
     +	    CALL MR_MANW ( datmnw, nmnw, datamw, namw, nlev, stndat,
     +		           ier )
C
C*	Merge the tropopause data.
C
	IF ( ( ntrp .ge. 1 ) .or. ( natr .ge. 1 ) )  THEN
	    CALL MR_TROP ( dattrp, ntrp, datatr, natr, nlev, ipt, 
     +			   stndat, idtype, ier )
	END IF
C
C*	Merge the significant temperature data.
C
	IF ( ( nsgt .gt. 1 ) .or. ( nast .gt. 1 ) )  THEN
	    CALL MR_SIGT ( datsgt, nsgt, datast, nast, nlev, ipt, 
     +			   stndat, idtype, ier )
	END IF
C
C*	Interpolate the height field.
C
	IF  ( iztype .eq. 2 ) THEN
	    CALL MR_MHGT ( nlev, ipt, stndat, sclhgt, ier )
	  ELSE IF  ( iztype .eq. 3 ) THEN
	    CALL MR_SCMZ ( nlev, ipt, stndat, sclhgt, ier )
	  ELSE
	    CALL MR_INTZ ( nlev, ipt, stndat, ier )
	END IF
C
C*	Merge the significant wind data on pressure surfaces.
C
	IF  ( ( .not. zbwind ) .or. ( .not. zawind ) )  THEN
	    msgw = 0
	    masw = 0
	    IF  ( .not. zbwind ) msgw = nsgw
	    IF  ( .not. zawind ) masw = nasw
	    CALL MR_PWND  ( datsgw, msgw, datasw, masw, nlev,
     +			    ipt, stndat, idtype, ier )
	END IF
C
C*	Merge the maximum wind data on pressure surfaces.
C
	IF  ( ( nmxw .ge. 1 ) .or. ( namx .ge. 1 ) )  THEN
	    CALL MR_PWND  ( datmxw, nmxw, datamx, namx, nlev,
     +			    ipt, stndat, idtype, ier )
	END IF
C
C*	Reinterpolate height for wind surfaces.
C
	CALL MR_INTZ  ( nlev, ipt, stndat, ier )
C
C*	Process the significant wind on height surfaces.
C
	IF  ( zbwind .or. zawind )  THEN
	    msgw = 0
	    masw = 0
	    IF  ( zbwind ) msgw = nsgw
	    IF  ( zawind ) masw = nasw
C
C*	    Merge the data.
C
	    CALL MR_SIGW  ( datsgw, msgw, datasw, masw, nlev, 
     +			    ipt, stndat, idtype, sclhgt, ier )
C
C*	    Interpolate missing pressures using heights.
C
	    CALL MR_INTP  ( sclhgt, 1, nlev, ipt, stndat, ier )
	END IF
C
C*	Order data in output array.
C
	CALL MR_ORDR  ( nlev, ipt, stndat, idtype, ier )
C
C*	Interpolate missing data.
C
C	CALL MR_MISS ( nlev, stndat, ier )
C
C*	Add in the underground mandatory levels.
C
	CALL MR_COND ( datman, nman, nlev, stndat, idtype, ipt, ier )
C*
	RETURN
	END
