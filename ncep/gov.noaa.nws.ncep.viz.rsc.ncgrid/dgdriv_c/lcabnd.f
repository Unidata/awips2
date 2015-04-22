	SUBROUTINE LC_ABND  ( area, iartyp, rlatll, rlonll, rlatur,
     +			      rlonur, stn, nstn, stcn, cdproj, cenlat,
     +			      cenlon, iret )
C************************************************************************
C* LC_ABND								*
C*									*
C* This subroutine translates a GEMPAK subarea.  For area types 1-3,	*
C* the latitude/longitude bounds are returned.  For area type 1, CDPROJ	*
C* contains the default projection string, and CENLAT & CENLON contain	*
C* the centroid location.  For area type 2, STN	contains the center	*
C* station.  For area type 6, STN contains the list of stations.  For	*
C* area types 5 and 7, the state or country is returned in STCN.  Area	*
C* types 2 and 3 may be followed by a number of * or - to contract or	*
C* expand the region.							*
C*									*
C* LC_ABND  ( AREA, IARTYP, RLATLL, RLONLL, RLATUR, RLONUR, STN, NSTN,	*
C*            STCN, CDPROJ, CENLAT, CENLON, IRET )			*
C*									*
C* Input parameters:							*
C*	AREA		CHAR*		Area name			*
C*									*
C* Output parameters:							*
C*	IARTYP		INTEGER		Area type 			*
C*					  -1 = none			*
C*					   1 = area name 		*
C*					   2 = center on station 	*
C*					   3 = lat/lon bounds 		*
C*					   4 = DSET 	 		*
C*					   5 = @ST	 		*
C*					   6 = @STN1;...;STNN		*
C*					   7 = @CN:C			*
C*	RLATLL		REAL		Lower left latitude		*
C*	RLONLL		REAL		Lower left longitude		*
C*	RLATUR		REAL		Upper right latitude		*
C*	RLONUR		REAL		Upper right longitude		*
C*	STN  (NSTN)	CHAR*		Stations			*
C*	NSTN		INTEGER 	Number of stations 		*
C*	STCN		CHAR*		State/country			*
C*	CDPROJ		CHAR*		Default projection string	*
C*	CENLAT		REAL		Centroid latitude		*
C*	CENLON		REAL		Centroid longitude		*
C*	IRET		INTEGER		Return code			*
C*					   0 = area found		*
C*					  -1 = invalid area name	*
C*					  -3 = station file open error	*
C**									*
C* Log:									*
C* I. Graffman/CSC	 3/82	Original 				*
C* M. Koslowski/CSC	 4/83	Modified to accept stations, * and - 	*
C* M. Goodman/RDS       12/84   Fixed @xx processing			*
C* M. desJardins/GSFC	 4/86	Removed sort of latitudes,longitudes	*
C* I. Graffman/RDS	11/87	Added :C for country			*
C* I. Graffman/RDS	 5/88	Remove STNLST				*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 8/88	Added #clat;clon;dlat;dlon		*
C* L. Williams/EAI	 5/94	Added "+" symbol to zoom factor		*
C* S. Jacobs/NMC	 7/94	Added SFSTBL to call to LC_FSTN		*
C* D. Keiser/GSC	12/95	Changed SFSTBL to 'sfstns.tbl'		*
C* G. Krueger/EAI	 6/96	Add corner points, default projection	*
C* K. Tyle/GSC		 7/96	Added case for AREA = GAREA		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	area, stn(*), stcn, cdproj
C*
	REAL		rltln (4)
	CHARACTER	loc*132, c*1
C------------------------------------------------------------------------
C*	Initialize.
C
	iartyp   = -1
	rlatll   = 0.0
	rlonll   = 0.0
	rlatur   = 0.0
	rlonur   = 0.0
	iret     = -1
	stn(1)   = ' '
	stcn     = ' '
	nstn     = 0
	cdproj   = ' '
	cenlat   = 0.0
	cenlon   = 0.0
C
C*	Translate area name to upper case and find length.
C
	CALL ST_LCUC  ( area, loc, ier )
	CALL ST_LSTR  ( loc, lens, ier )
C
C*	No name was entered if length of string was 0.
C
	IF  ( lens .eq. 0 )  RETURN
C
C*	Check to see if DS or DSET was entered.
C
	IF  ( ( loc .eq. 'DS' ) .or. ( loc .eq. 'DSET' ) )  THEN
	    rlatll = -90.
	    rlonll = -180.
	    rlatur = 90.
	    rlonur = 180.
	    iartyp = 4
	    iret   = 0
	    RETURN
	ELSE IF ( loc .eq. 'GAREA' ) THEN
	    CALL GQBND ( 'M', rlatll, rlonll, rlatur, rlonur, iret )
	    iartyp = 1
	    RETURN
	ENDIF
C
C*	Check if latitudes and longitudes were explicitly entered.
C*	If the first character is #, then clat;clon;dlat;dlon were
C*	entered.
C*	Code to sort latitudes and longitudes removed to allow drawing
C*	world maps.
C
	IF  ( loc (1:1) .eq. '#' )  THEN
	    is = 2
	  ELSE
	    is = 1
	END IF
ccc	print*, ' ENTER LC_ABND  1 ---> ', loc(is:),'XXXXXXend'
	CALL ST_RLST  ( loc (is:), ';', RMISSD, 4, rltln, num, ier )
ccc	print*, ' ENTER LC_ABND  2 --->  ', ier, num
	IF  ( ( ier .eq. 0 ) .and. ( num .eq. 4 ) )  THEN
	    IF  ( is .eq. 1 )  THEN
		rlatll = rltln (1)
		rlonll = rltln (2)
		rlatur = rltln (3)
		rlonur = rltln (4)	
		cenlat = (rlatll + rlatur) / 2.
		cenlon = (rlonll + rlonur) / 2.
	      ELSE
		rlatll = rltln (1) - rltln (3)
		rlatur = rltln (1) + rltln (3)
		rlonll = rltln (2) - rltln (4)
		rlonur = rltln (2) + rltln (4)
		cenlat = rltln (1)
		cenlon = rltln (2)
	    END IF
	    IF ( cenlat .gt. 25. ) THEN
		cdproj = 'NPS'
	    ELSE IF ( cenlat .lt. -25. ) THEN
		cdproj = 'SPS'
	    ELSE
		cdproj = 'MER'
	    END IF
	    iartyp = 3
	    iret   = 0
	    RETURN
	ENDIF
C
C*	Check for @ followed by state or station list.
C
	IF  ( loc (1:1) .eq. '@' )  THEN
C
C*	    Check if the 2:3 characters are numbers
C
	    CALL ST_CRNM  ( loc (2:3), stnum, ierr )
C
C*	    If 3 characters total were entered, this is @ST.
C
	    IF  ( ( lens .eq. 3 ) .and. ( ierr .ne. 0 ) )  THEN
	        iartyp = 5
	        iret   = 0
	        stcn   = loc (2:3)
	        RETURN
C
C*		Check for @CN:C.
C
	      ELSE IF  ( ( lens .eq. 5 ) .and. ( ierr .ne. 0 ) .and.
     +			 ( loc (4:5) .eq. ':C' ) )  THEN
		iartyp = 7
		iret   = 0
		stcn   = loc (2:3)
		RETURN
C
C*	  	Otherwise, this is a station list.
C
	      ELSE
C
C*		Note that a maximum of LLMXST stations will be read.
C
		CALL ST_CLST  ( loc (2:), ';', ' ', LLMXST, stn, nstn,
     +				ier )
	        iartyp = 6
	        iret   = 0
	        RETURN
	    END IF
	END IF
C
C*	Have eliminated special cases.  Now find area using area name.
C
C*	Check for zoom factors.
C
	izoom = 0
	i     = lens
	c     = loc (i:i)
C
	DO WHILE  ( ( c .eq. '*' ) .or. ( c .eq. '-' ) .or.
     +		    ( c .eq. '+' ) ) 
	    IF  ( ( c .eq. '*' ) .or. ( c .eq. '+' ) )
     +		izoom = izoom + 1
	    IF  ( c .eq. '-' )  izoom = izoom - 1
	    loc  (i:i) = ' '
	    i = i - 1
	    IF  ( i .gt. 0 )  THEN
		c = loc (i:i)
	      ELSE
		c = ' '
	    ENDIF
	ENDDO
C
C*	Check for area in the geographic file list.
C
	CALL TB_FGEO  ( loc, tlatll, tlonll, tlatur, tlonur, cdproj,
     +			cenlat, cenlon, ier )
	IF  ( ier .eq. 0 )  THEN
	    iartyp = 1
	    iret   = 0
	ELSE
C
C*	    Check for area in the station file.
C
	    CALL LC_FSTN  ( 'sfstns.tbl', loc, cenlat, cenlon, ier )
	    IF  ( ier .eq. 0 )  THEN
		diflat = 4.0
		diflon = 7.0
		iartyp = 2
		stn(1) = loc
		IF ( cenlat .gt. 25. ) THEN
		    cdproj = 'NPS'
		ELSE IF ( cenlat .lt. -25. ) THEN
		    cdproj = 'SPS'
		ELSE
		    cdproj = 'MER'
		END IF
		iret   = 0
	    ELSE IF  ( ier .eq. -3 )  THEN
		iret   = -3
	    ENDIF
	ENDIF
C
C*	If area has been found--compute bounds with zoom factors.
C
	IF  ( iartyp .gt. 0 )  THEN
	    IF  ( izoom .le. 0 )  THEN
		zoomul = 2 ** (-izoom)
	    ELSE
		zoomul = 1. / (2 ** izoom)
	    ENDIF
	    IF ( iartyp .eq. 1 ) THEN
		zmofst = (1. - zoomul) / 2.
		rlatll = tlatll + (tlatur - tlatll) * zmofst
		rlatur = tlatur - (tlatur - tlatll) * zmofst
		rlonll = tlonll + (tlonur - tlonll) * zmofst
		rlonur = tlonur - (tlonur - tlonll) * zmofst
	    ELSE
		rlatll = cenlat - zoomul * diflat
		rlatur = cenlat + zoomul * diflat
		rlonll = cenlon - zoomul * diflon
		rlonur = cenlon + zoomul * diflon
	    ENDIF	
	ENDIF
C*
	RETURN
	END
