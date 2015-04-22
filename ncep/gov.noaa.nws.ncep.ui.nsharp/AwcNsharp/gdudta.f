	SUBROUTINE GDUDTA  ( iflno, gdatim, gvcord, gpoint, 
     +			     time, ivcord, data, nlev,
     +			     rgx, rgy, rlat, rlon, y, havsfc, iret )
C************************************************************************
C* GDUDTA								*
C*									*
C* This subroutine gets the data to plot for a profile.			*
C*									*
C* GDUDTA  ( IFLNO, GDATIM, GVCORD, GFUNC, GPOINT, TIME, IVCORD,	*
C*           RGX, RGY, RLAT, RLON, NPTS, X, Y, PARM, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Grid file number		*
C*	GDATIM		CHAR*		User input date/time		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GFUNC		CHAR*		User input function		*
C*	GPOINT		CHAR*		User input point to plot	*
C*	TIME  (2)	CHAR*		Time to search for levels	*
C*	IVCORD		INTEGER		Vertical coordinate for search	*
C*      YSTRT	 	REAL            Starting vert coord value	*
C*      YSTOP           REAL            Stopping vert coord value       *
C*									*
C* Output parameters:							*
C*	RGX		REAL		X grid coordinate		*
C*	RGY		REAL		Y grid coordinate		*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	NPTS		INTEGER		Number of points		*
C*	X    (NPTS)	REAL		X coordinates			*
C*	Y    (NPTS)	REAL		Y coordinates			*
C*	PARM		CHAR*		Parameter name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points		*
C*					-10 = no levels at this time	*
C**									*
C* Log:									*
C* J. Whistler/SSAI	11/92						*
C* L. Hinson/AWC     7/06 Changed to call new DG subroutines
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, gpoint, time (2)
c	REAL		data (6,*), y (*), tdata(6,LLMXLV)
	REAL		data (7,*), y (*), tdata(7,LLMXLV)
	LOGICAL		havsfc, havgfs
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80
	CHARACTER	gvecx*72, parmu*72, parmv*72,
     +			gfunc*72, parms(7)*4, parm*12
c     +			gfunc*72, parms(6)*4, parm*12
	INTEGER		lev (2)
	REAL		grid ( LLMXGD ), rlvl ( LLMXLV )
	REAL		gridv ( LLMXGD ), gridu ( LLMXGD )
C*
	INTEGER		level ( 2, LLMXLV ), iloc (7)
c	INTEGER		level ( 2, LLMXLV ), iloc (6)
C*
	DATA		parms / 'PRES', 'TMPC', 'DWPC', 
     +			        'UWND', 'VWND', 'HGHT', 'OMEG'/
	DATA		iloc / 1, 2, 3, 6, 7, 4, 5 /
c	DATA		iloc / 1, 2, 3, 6, 4, 5 /
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	nlev = 0
C
C*	Find plotting location.
C
C       LJH Modify to accept Lat/Lon Coord passed in rlat rlon when
C         gpoint is set to ZZZ
        IF ( gpoint .eq. 'ZZZ' ) THEN
            CALL GTRANS('M','G', 1, rlat,rlon, rgx, rgy, ier)
        ELSE
	    CALL GR_PLOC  ( gpoint, rgx, rgy, rlat, rlon, iret )
	ENDIF
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GR', iret, gpoint, ier )
	    RETURN
	END IF
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
C
	dattim ( 1 ) = time ( 1 )
	dattim ( 2 ) = ' '
	CALL DG_GLEV  ( iflno, dattim, ivcord,
     +			LLMXLV, level, nlev, ier )
	IF  ( nlev .eq. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'GDSNDG', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Float the levels for sorting and look for surface.
C
        CALL GDPGTS ( iflno, dattim, ivcord, rgx, rgy,
     +                ysfc, havsfc, parm, havgfs, ier  )
C
C*      Float the levels for sorting.
C
	j = 0
	DO i = 1, nlev
	  IF ( level (2,i) .eq. -1 ) THEN
	      j = j + 1
              rlvl ( j ) = FLOAT ( level ( 1, i ) )
	  END IF
	END DO
	nlev = j
C*
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
C
C*      Assign all levels, temporarily.
C
122	FORMAT ( 1x, f10.2 )
	DO i = 1, nlev
	  y ( i ) = rlvl ( i )
	END DO
C
C*	Set surface value.
C
C	IF ( havsfc .and. .not. ERMISS ( ysfc ) )
C     +          y ( 1 ) = ysfc
	IF ( havsfc .and. ERMISS ( ysfc ) ) THEN	
	  ii = 0
	  DO i = 2, nlev
	    ii = ii + 1
            y ( ii ) = rlvl ( i )
	    rlvl ( ii ) = y ( ii )
       	  END DO
	  nlev = nlev - 1
	  havsfc = .false.
	END IF
C
C*	Do subset in the vertical--eliminate unneeded levels.
C
	IF ( havsfc ) THEN
	    ys1 = ysfc
	ELSE
	    ys1 = y (1)
	END IF
	i = 1
	istrt = 0
	istop = 0
	DO WHILE ( ( i .lt. nlev ) .and. ( istrt .eq. 0 ) )
	  i = i + 1
	  IF ( (ys1 .ge. y ( i-1 ) .and. ys1 .lt. y ( i ) )
     +                               .or.
     +         (ys1 .le. y ( i-1 ) .and. ys1 .gt. y ( i ) ) ) THEN
	      IF ( ys1 .eq. y ( i-1 ) ) THEN
                  istrt = i - 1
	      ELSE 
                  istrt = i
	      END IF
	  END IF
	END DO
	IF ( istrt .eq. 0 ) istrt = 1
	IF ( istop .eq. 0 ) istop = nlev
C
C*	Loop through single levels finding data.
C
	npts = 0
c	DO  kk = 1, 6 - 2
	DO  kk = 1, 7 - 2
	    gfunc = parms (iloc(kk))
	    DO  i = istrt, istop
		npts = npts + 1
	        intlvl = int ( rlvl ( i ) )
	        CALL ST_INCH ( intlvl, glevel, ier )
C*
	        CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			        pfunc,  grid,   kx, ky, dattim, lev, 
     +			        jvcord, parm, ierdg )
C*
	        IF  ( ierdg .eq. 0 )  THEN
C
C*	        Check that grid includes point to be found.
C
	            rkx = FLOAT ( kx )
	            rky = FLOAT ( ky )
	            IF  ( ( rgx .gt. rkx ) .or. ( rgy .gt. rky ) )  THEN
		        iret = -4
		        RETURN
	            END IF
C
C*	            Interpolate to correct point.
C
	            CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid,
     +				    tdata (iloc(kk),i-istrt+1 ), ier )
	            y (npts) = rlvl ( i )
		    IF ( ( ERMISS ( tdata (iloc(kk),i-istrt+1) ) ) .and. 
     +			 ( i .eq. 1 ) )
     +				havgfs = .false.
		ELSE 
		    y (npts) = rlvl (i)
		    tdata (iloc(kk), i-istrt+1 ) = RMISSD
	  	END IF
	    END DO
	END DO
C
C*
C
	gvecx = 'WIND'
	npts = 0
	DO  i = istrt, istop
	    intlvl = int ( rlvl ( i ) )
	    npts = npts + 1
	    CALL ST_INCH ( intlvl, glevel, ier )
C*
            CALL DG_VECT ( gdatim, glevel, gvcord, gvecx,
     +                     pfunc,  gridu, gridv, kx, ky, dattim,
     +                     lev, jvcord, parmu, parmv, ierdg )
C*
	    IF  ( ierdg .eq. 0 )  THEN
C
C*	    Check that grid includes point to be found.
C
	        rkx = FLOAT ( kx )
	        rky = FLOAT ( ky )
	        IF  ( ( rgx .gt. rkx ) .or. ( rgy .gt. rky ) )  THEN
		    iret = -4
		    RETURN
	        END IF
C
C*	        Interpolate to correct point.
C
	        CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, gridu,
     +				tdata (4,i-istrt+1), ier )
	        CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, gridv,
     +				tdata (5,i-istrt+1), ier )
	        y (npts) = rlvl ( i )
		IF ( ERMISS ( tdata (5,i-istrt+1) ) .and. i .eq. 1 ) 
     +			      havgfs = .false.
	    ELSE
		y (npts) = rlvl (i)
		tdata (4,i-istrt+1) = RMISSD
		tdata (5,i-istrt+1) = RMISSD
	    END IF
	END DO
C
	nlev = npts
C
C*	Check that there are some points.
C
	IF  ( nlev .le. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', ierdg , pfunc, ier )
	    iret = -9
	    CALL ER_WMSG  ( 'GDSNDG', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Set vertical coordinate for GFUNC value at surface.
C
	IF ( havgfs ) y ( 1 ) = ysfc 
C
C*	Filter out level with missing data of PRES, TMPC, or HGHT.
C
	knt = 0
	DO i = 1, nlev
	  IF ( ( .not. ERMISS ( tdata(1,i) ) ) .and.
     +         ( .not. ERMISS ( tdata(2,i) ) ) .and.
     +         ( .not. ERMISS ( tdata(6,i) ) ) ) THEN
	      knt = knt + 1
c	      DO j = 1, 6
	      DO j = 1, 7
		  data(j, knt) = tdata(j,i)
	      END DO
	      y(knt) = rlvl (i+istrt-1)
	  END IF	
	END DO
	nlev = knt
C*
	RETURN
	END
