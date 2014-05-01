	SUBROUTINE GET_MDL_SND(gdfile, parmstr, gdatim, gpoint,
     +			   rdata, numlev, sfcpres, sfct, sfctd, 
     +			   sfch, sdir, sspd, ier)

	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, parmstr, gdatim, gpoint
	CHARACTER       dummy1*40, dummy2*40
     	REAL		rdata(*), sfcpres, sfct, sfctd
        REAL		sdir, sspd
	CHARACTER	gdoutf*256
	INTEGER         numlev, ier
	LOGICAL		gottm

C* Local vars

	CHARACTER       myparms(40)*12
	REAL		y(LLMXLV), data(LLMXDT), maxgrd
	CHARACTER	time(2)*20, lastim*20
	INTEGER         igdfln, npts
	INCLUDE         'ERMISS.FNC'
C*
C-----------------------------------------------------------------------

	npts   = 0
	numlev = 0
C
C*	Open up the grid file.
C
CJL        print *, 'GET_MDL_SND gdfile = ', gdfile
CJL        print *, 'GET_MDL_SND parmstr = ', parmstr
CJL        print *, 'GET_MDL_SND gdatim = ', gdatim
CJL        print *, 'GET_MDL_SND gpoint = ', gpoint
CJL        print *, 'GET_MDL_SND numlev = ', numlev
CJL        print *, 'GET_MDL_SND sfcpres = ', sfcpres
CJL        print *, 'GET_MDL_SND sfct = ', sfct
CJL        print *, 'GET_MDL_SND sfctd = ', sfctd
CJL        print *, 'GET_MDL_SND sfch = ', sfch
CJL        print *, 'GET_MDL_SND sdir = ', sdir
CJL        print *, 'GET_MDL_SND sspd = ', sspd
	CALL DG_NFIL( gdfile, ' ', ier )
CJL        print *, 'GET_MDL_SND DG_NFIL returns ier = ', ier
	IF ( ier .ne. 0 ) RETURN
	CALL DG_NDTM( gdatim, ier )
CJL        print *, 'GET_MDL_SND DG_NDTM ier = ', ier
CJL        print *, 'GET_MDL_SND calling DG_NTIM'
	CALL DG_NTIM(.false., .false., time, gottm, ier)
CJL        print *, 'GET_MDL_SND DG_NTIM returns ier = ', ier
C
C*      Parse the input string
C
        call ST_CLST( parmstr, ';', 'UNKNOWN', 40, myparms, num,
     +                ier )
CJL        print *, 'GET_MDL_SND ST_CLST returns ier = ', ier
        IF (ier .lt. 0 .or. ier .gt. 1) THEN
  	  CALL DG_NEND ( iret )
          return
        ENDIF
C
C*      Set the subset region to speed calculations.
C
        CALL DG_SUBG ( 'N', imin, imax, jmin, jmax, iret )
CJL        print *, 'GET_MDL_SND DG_SUBG returns ier = ', ier
        IF ( iret .ne. 0 ) THEN
           CALL ER_WMSG  ( 'DG', iret, ' ', ier )
           RETURN
        END IF
C
C*	Get data.
C
	time(1) = gdatim
	time(2) = ' '
	CALL GDUDTA( igdfln, myparms, num, time, 'PRES',
     +		     gpoint, 1, data, npts,
     +               rgx, rgy, rlat,  rlon, y,
     +               sfcpres, sfct, sfctd, sfch,
     +               sdir, sspd, ier )
        print*,'ier from GDUDTA was ', ier
	CALL DG_NEND ( iret )
        print*,'ier from DG_NEND was ', ier

	IF ( ier .ne. 0 ) THEN
	  RETURN
	ENDIF

C
C*	Reformat the data.
C

c Make sure this is not too large that we are trashing somebodys memory
c should clean this up. maybe pass in nlev and use that as a maxlev type
c of thing
        DO i = 1, 8192
	  rdata(i) = RMISSD
	ENDDO

	knt = 0

C Make life easy. increment the DO loop by the # of parms

        DO i = 1, npts, num

C 	  Make sure we're at a pressure level betw. sfc and 100mb

          IF ( data(i) .ge. 100. ) THEN
            numlev = numlev + 1

            DO j = 0, num-1
	        knt = knt + 1
	        itmp = i + j
                IF (.not. ERMISS(data(itmp))) THEN
                    rdata(knt) = data(itmp)
                ELSE
                    rdata(knt) = RMISSD
                END IF
            END DO

	  ELSE
C        Mark all data as missing above 100mb
            DO j = 0, num-1
	        knt = knt + 1
                rdata(knt) = RMISSD
            END DO
          END IF
        END DO

C*
	END


	SUBROUTINE GDUDTA( iflno, myparms, nparms, gdatim, gvcord, 
     +			   gpoint, ivcord, data, nlev, rgx,
     +			   rgy, rlat, rlon, y, sfcpres, sfct, sfctd, 
     +	                   sfch, sdir, sspd, iret )

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
C*	GDATIM(2)	CHAR*		User input date/time		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GPOINT		CHAR*		User input point to plot	*
C*	IVCORD		INTEGER		Vertical coordinate for search	*
C*									*
C* Output parameters:							*
C*	RGX		REAL		X grid coordinate		*
C*	RGY		REAL		Y grid coordinate		*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points		*
C*					-10 = no levels at this time	*
C**									*
C* Log:									*
C* J. Whistler/SSAI	11/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C	INCLUDE         'XYDEF.CMN'
C*
	CHARACTER*(*)	gdatim, gvcord, gpoint, myparms(*)
	INTEGER         iflno, nparms, iret
	REAL		data(nparms,*), tdata(nparms,LLMXLV), y(*)
	LOGICAL		havsfc, havgfs
C*
c dattim is used for output in this routine. not input
	CHARACTER	dattim(2)*20, glevel*20, pfunc*80
	CHARACTER	gfunc*72, parm*12
	INTEGER		lev(2)
	REAL		grid(LLMXGD), rlvl(LLMXLV)
	REAL		sfcpres, sfct, sfctd, sfch, sdir, sspd
C*
	INTEGER		level(2, LLMXLV), kk
C*
C------------------------------------------------------------------------
	iret = 0
	nlev = 0
C	print *, "Beginning GDUDTA 1"
C
C*	Find plotting location.
C
C	print *, "gpoint = ", gpoint
C	print *, "prjnam = ", prjnam
	CALL GR_PLOC( gpoint, rgx, rgy, rlat, rlon, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG( 'GR', iret, gpoint, ier )
	    RETURN
	END IF
C	print *, "Selected point:  ", rgx, rgy, rlat, rlon
C
C*	Set the subset region to speed calculations.
C
	imin = INT(rgx) - 2
	imax = INT(rgx) + 2
	jmin = INT(rgy) - 2
	jmax = INT(rgy) + 2
	CALL DG_SUBG( 'N', imin, imax, jmin, jmax, iret )
	IF ( iret .ne. 0 ) THEN
	   CALL ER_WMSG( 'DG', iret, ' ', ier )
	   RETURN
	END IF

C
C*	Get levels which might have data.
C
	CALL DG_GLEV( iflno, gdatim, ivcord,
     +		      LLMXLV, level, nlev, ier )
	IF  ( nlev .eq. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG( 'GDSNDG', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Look for surface pressure.
C
C	print *, "Beginning GDUDTA 1", iflno
	dattim(1) = gdatim
	dattim(2) = ' '
        CALL GDPGTS( 1, dattim, ivcord, rgx, rgy,
     +               ysfc, sfct, sfctd, sfch, sdir, sspd,
     +               havsfc, parm, havgfs, ier )
C	print *, "Beginning GDUDTA 2"
        IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG( 'GDPGTS', ier, ' ', iret )
        END IF
	sfcpres = ysfc

C	print*,'GDUDTA: havsfc: ',havsfc,' havsfcdata: ', havgfs
C	print*,'sfc pressure: ', sfcpres
C	print*,'x=',rgx,'  y=',rgy,'  parm=',parm
C
C*      Float the levels for sorting.
C
	j = 0
	DO i = 1, nlev
C
C*         I guess do this for parms that are a single level and not a layer
C*         (level(2,i) is -1 for levels)
C
	  IF ( level (2,i) .eq. -1 ) THEN
	      j = j + 1
              rlvl(j) = FLOAT(level(1, i))
	  END IF
	END DO

	nlev = j
	CALL LV_SORT( ivcord, nlev, rlvl, iret )
C
C*      Assign all levels, temporarily.
C
	DO i = 1, nlev
	  y(i) = rlvl(i)
	END DO
C
C*	Set surface value.
C
	IF ( havsfc .and. ERMISS ( ysfc ) ) THEN	
	  ii = 1
	  DO i = 1, nlev
	    ii = ii + 1
            y(i+1)    = rlvl(i)
	    rlvl(i+1) = y(i+1)
       	  END DO
	  y(1) = ysfc				
	END IF
C
C*	Do subset in the vertical--eliminate unneeded levels.
C
	IF (havsfc) THEN
	    ys1 = ysfc
	ELSE
	    ys1 = y(2)
	END IF

	i     = 1
	istrt = 0

	DO WHILE ((i .lt. nlev) .and. (istrt .eq. 0))
	  i = i + 1
	  IF ((ys1 .ge. y(i-1) .and. ys1 .lt. y(i)) .or.
     +        (ys1 .le. y(i-1) .and. ys1 .gt. y(i))) THEN
	      IF (ys1 .eq. y(i)) THEN
                  istrt = i
	      ELSE 
                  istrt = i-1 
	      END IF
	  END IF
	END DO

	IF (istrt .eq. 0) istrt = 1

C
C*	Loop through single levels finding data.
C

	npts = 0
	DO  kk = 1, nparms
	    gfunc = myparms(kk)(1:)

	    DO  i = istrt, nlev
		npts = npts + 1
	        intlvl = INT(rlvl(i))
	        CALL ST_INCH( intlvl, glevel, ier )

C*
	        CALL DG_GRID( gdatim, glevel, gvcord, gfunc, 
     +			      pfunc,  grid, kx, ky, dattim, lev, 
     +			      jvcord, parm, ierdg )
C*
	        y(npts) = rlvl(i)

	        ind = i - istrt + 1  ! mkay

	        IF ( ierdg .eq. 0 ) THEN
C
C*	        Check that grid includes point to be found.
C
	            rkx = FLOAT(kx)
	            rky = FLOAT(ky)
	            IF ( ( rgx .gt. rkx ) .or. ( rgy .gt. rky ) ) THEN
		        iret = -4
		        RETURN
	            END IF
C
C*	            Interpolate to correct point.
C

	            CALL GR_INTP( 1, rgx, rgy, 1, kx, ky, grid,
     +				  tdata(kk, ind), ier )
		    IF ( ( ERMISS(tdata(kk, ind))) .and. 
     +			 ( i .eq. 1 ))
     +				havgfs = .false.
		ELSE 
		    tdata(kk, ind) = RMISSD
	            ier = ierdg
	  	END IF

	    END DO
	END DO

	nlev = npts
C
C*	Check that there are some points.
C
	IF ( nlev .le. 0 ) THEN
	    CALL ER_WMSG( 'DG', ierdg , pfunc, ier )
	    iret = -9
	    CALL ER_WMSG( 'GDSNDG', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Set vertical coordinate for GFUNC value at surface.
C
	IF (havgfs) y(1) = ysfc 
C
C*	Filter out levels with missing data of PRES, TMPC, or HGHT.
C
	knt = 0
	DO i = 1, nlev
	  IF ((.not. ERMISS(tdata(1,i))) .and.
     +        (.not. ERMISS(tdata(2,i))) .and.
     +        (.not. ERMISS(tdata(3,i)))) THEN
	      knt = knt + 1
	      DO j = 1, nparms
		  data(j, knt) = tdata(j, i)
	      END DO
	      y(knt) = rlvl(i+istrt-1)
	  END IF	
	END DO
	nlev = knt
C*
	RETURN
	END



	SUBROUTINE GDPGTS( iflno, time, ivcord, rgx, rgy,  
     +			   vclsfc, stmpc, stdc, sfch, sdir, sspd,
     +                     havsfc, parm, havgfs, iret)

C************************************************************************
C* GDPGTS								*
C*									*
C* This subroutine gets the surface data for a cross section.		*
C*									*
C* GDPGTS  ( IFLNO, TIME, IVCORD, RGX, RGY,  VCLSFC, HAVSFC, 		*
C*	     PARM, HAVGFS, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		  INTEGER	Grid file number		*
C*	TIME  (2)	  CHAR*		Time to search for levels	*
C*	IVCORD		  INTEGER	Vertical coordinate for search	*
C*	RGX  		  REAL		X grid coordinate		*
C*	RGY  		  REAL		Y grid coordinate		*
C*									*
C* Output parameters:							*
C*      VCLSFC            REAL 		Vert coord location of sfc	*
C*      HAVSFC            LOGICAL       Flag for existence of sfc	*
C*	PARM		  CHAR*		Parameter name			*
C*      HAVGFS            LOGICAL       Flag for existence of sfc data	*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -6 = GVCORD is invalid		*
C*					 +2 = no sfc value found	*
C**									*
C* Log:									*
C* D. McCann/NSSFC	12/94		Created from gdxgts.f		*
C************************************************************************
	INCLUDE		'DGCMN.CMN'
C*
	CHARACTER*80	timex(2), time1(2), pfunc
	CHARACTER*(*)	time(2), parm
        CHARACTER*80   jhlevel, jhcord, jhparm, errmsg, parm2
        REAL		rgx , rgy , vclsfc, stmpc, stdc, sfch
        REAL		sdir, sspd
	LOGICAL		havsfc, havgfs
	INTEGER		igx, igy, iflno, ivcord, ier
C*
	REAL		grid( LLMXGD ), grid2( LLMXGD )
	INTEGER		level( 2 ), igrhdr( LLGDHD )
C------------------------------------------------------------------------
	iret   = 0
	havsfc = .false.
	havgfs = .false.
        vclsfc = RMISSD

	level(1) =  0
	level(2) = -1
	CALL LV_CCRD( ivcord, parm, ier )

        time1(1) = time(1)
        time1(2) = time(2)
C	print *, "GDPGTS 1"
C
C*      #########################################################################
C*      # Read the surface pressure 
C*      #########################################################################
C*	Try to read parm at level 0 (surface) with vertical coord ivcord
C
C	print *, "GDPGTS 2 "
C	print *, "iflno = ", iflno
C	print *, "time(1) = ", time(1)
C	print *, "time(2) = ", time(2)
C	print *, "level(1) = ", level(1)
C	print *, "level(2) = ", level(2)
C	print *, "ivcord = ", ivcord
C	print *, "parm = ", parm
	CALL DG_NRDT  ( iflno, time, level, ivcord, parm,
     +                  grid, igx, igy, igrhdr, ier )
C	print *, "GDPGTS 3", ier
	IF ( ier .eq. 0 ) THEN
	    havsfc = .true.        ! We have a surface value
	    havgfs = .true.        ! We actually know the value
	ELSE
C
C*	    Try to read the surface data on JVCORD = 0.
C*          jvcord of 0 means NONE (no vertical coord)
C
	    jvcord = 0
	    CALL DG_NRDT  ( iflno, time, level, jvcord, parm,
     +                      grid, igx, igy, igrhdr, ier )
C	    print *, "GDPGTS 4", ier
	    IF ( ier .eq. 0 ) THEN
		havsfc = .true.
	        havgfs = .true.
	    END IF
	END IF

	IF (havsfc) THEN
C
C*          Interpolate grid value to the right location (rgx, rgy)
C
	    CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid, vclsfc, iret )
	    IF ( iret .ne. 0 ) THEN
		vclsfc = RMISSD
		havsfc = .false.
	        havgfs = .false.
	    END IF
	ELSE
	  iret = 2
	  RETURN
	END IF

C*      #########################################################################
C*      # If SfcPres was found, Determine 2m Temperature
C*      #########################################################################
C*
      jhlevel = "2"
      jhcord  = "HGHT"
      jhparm  = "TMPC"
      errmsg  = ""
      CALL DG_GRID(time1, jhlevel, jhcord, jhparm, errmsg, grid2,
     +                    igx, igy, timex, level, jvcord, parm2,
     +                    iret)
	CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid2, stmpc, iret )
C        print *, "Surface Temperature =", stmpc
C        print *, "Error Msg= ", errmsg

C*      #########################################################################
C*      # If SfcPres was found, Determine 2m Dewpoint
C*      #########################################################################
C*
      jhlevel = "2"
      jhcord  = "HGHT"
      jhparm  = "DWPC"
      errmsg  = ""
      CALL DG_GRID(time1, jhlevel, jhcord, jhparm, errmsg, grid2,
     +                    igx, igy, timex, level, jvcord, parm2,
     +                    iret)
	CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid2, stdc, iret )
C        print *, "Surface Dewpoint =", stdc

C*      #########################################################################
C*      # If SfcPres was found, Determine Sfc Height
C*      #########################################################################
C*
      jhlevel = "0"
      jhcord  = "NONE"
      jhparm  = "HGHT"
      errmsg  = ""
      CALL DG_GRID(time1, jhlevel, jhcord, jhparm, errmsg, grid2,
     +                    igx, igy, timex, level, jvcord, parm2,
     +                    iret)
	CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid2, sfch, iret )
C        print *, "Surface Height =", sfch

C*      #########################################################################
C*      # If SfcPres was found, Determine Sfc Wind Dir
C*      #########################################################################
C*
      jhlevel = "10"
      jhcord  = "HGHT"
      jhparm  = "DRCT"
      errmsg  = ""
      CALL DG_GRID(time1, jhlevel, jhcord, jhparm, errmsg, grid2,
     +                    igx, igy, timex, level, jvcord, parm2,
     +                    iret)
	CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid2, sdir, iret )
C        print *, "Surface Wind Dir =", sdir

C*      #########################################################################
C*      # If SfcPres was found, Determine Sfc Wind Speed
C*      #########################################################################
C*
      jhlevel = "10"
      jhcord  = "HGHT"
      jhparm  = "SPED"
      errmsg  = ""
      CALL DG_GRID(time1, jhlevel, jhcord, jhparm, errmsg, grid2,
     +                    igx, igy, timex, level, jvcord, parm2,
     +                    iret)
	CALL GR_INTP( 1, rgx, rgy, 1, igx, igy, grid2, sspd, iret )
C        print *, "Surface Wind Speed =", sspd

	END
