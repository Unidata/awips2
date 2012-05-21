	SUBROUTINE CCBOXT (cval, clabel, ilabel, start, ill, jll, 
     +			   idirc, scflag, iret)
C************************************************************************
C* CCBOXT								*
C*									*
C* This subroutine draws a contour line through a box defined by four	*
C* grid points.								*
C*									*
C* CCBOXT  ( CVAL, CLABEL, ILABEL, START, ILL, JLL, IDIRC, SCFLAG, 	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour level			*
C*	CLABEL		CHAR*		Contour label			*
C*	ILABEL		INTEGER		Label type			*
C*	START		LOGICAL		Start flag			*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*									*
C* Input and output parameters:						*
C*	ILL		INTEGER		Lower left x value for box	*
C*	JLL		INTEGER		Lower left y value for box	*
C*	IDIRC		INTEGER		Direction entering box		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = edge reached		*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* M. desJardins/GSFC	 6/89	Rewritten for GEMPAK 5			*
C* J. Whistler/SSAI	 6/91	Set internal grids to size LLMXDG	*
C* M. desJardins/NMC	12/91	Renamed: CBOXIT-->CCBOXT		*
C* K. Brill/NMC		02/92	Shorten some too-long lines of code	*
C* G. Krueger/EAI	12/94	Conditional CCOEFF; CCGTBX call->CCCRSS	*
C* S. Jacobs/NCEP	 5/99	Modified check for multiple directions	*
C* C. Bailey/HPC	 6/06	Added contour label to calling sequence	*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	CHARACTER*24	clabel(LLCLEV)
	LOGICAL		start, scflag
C*
	REAL		box (4), boxmod (4)
	LOGICAL		tstr, tsts, tstl, edge
C*
	INTEGER		ippt1 (4), ippt2 (4), ippt3 (4), ippt4 (4)
	DATA		ippt1 / 4, 1, 2, 3 /, ippt2 / 3, 4, 1, 2 /,
     +			ippt3 / 2, 3, 4, 1 /, ippt4 / 1, 2, 3, 4 /
C
C*	This statement function gets the fractional distance along a
C*	side to the value 0.
C
	FIND0 ( x, y ) = x / ( x - y )
C------------------------------------------------------------------------
	iret = 0
	xll  = FLOAT ( ill )
	yll  = FLOAT ( jll )
C
C*	Set flag indicating we've been here.
C
	IF  ( idirc .eq. 1 )  THEN
	    ihline ( (jll-1)*isize+ill ) = ihline ( (jll-1)*isize+ill )
     +					   + 1
	  ELSE IF  ( idirc .eq. 2 )  THEN
	    ivline ( (jll-1)*isize+ill ) = ivline ( (jll-1)*isize+ill )
     +					   + 1
	  ELSE IF  ( idirc .eq. 3 )  THEN
	    ihline ( (jll)*isize+ill ) = 
     +				   ihline ( (jll)*isize+ill ) + 1
	  ELSE IF  ( idirc .eq. 4 )  THEN
	    ivline ( (jll-1)*isize+ill+1 ) = ivline ( (jll-1)*isize+ill
     +							+1 ) + 1
	END IF
C
C*	First check for missing data.
C
	CALL CCCRSS ( 0, 0, ill, jll, cval, iret )
	IF  ( iret .eq. -2 .or. iret .eq. -1 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Compute the coefficients to evaluate points inside box, if we
C*	are subboxing.
C
	IF ( numsub .gt. 2 ) CALL CCOEFF ( ill, jll, ier )
C
C*	For a new contour line, find the point to start.
C
	IF  ( start )  THEN
	    CALL CCSTRT  ( cval, ill, jll, idirc, 1, illbox, jllbox, 
     +			   iret )
	    IF  ( iret .ne. 0 )  RETURN
C
C*	    Otherwise, check if point left by last box needs to be
C*	    modified for new coefficients.
C
	  ELSE
	    CALL CCCHCK  ( cval, ill, jll, idirc, illbox, jllbox, iret )
	    IF  ( iret .ne. 0 )  RETURN
	END IF
C
C*	Loop through all the grid boxes until an edge is reached.
C
	knt  =  1
	maxk =  4 * numsub
	edge = .false.
	DO WHILE  ( ( iret .eq. 0 ) .and. ( .not. edge ) )
C
C*	    Get corners of this subbox.
C
	    CALL CCGTSB  ( illbox, jllbox, ill, jll, cval, box, boxmod,
     +			   iret )
C
C*	    Get points to look to right, straight and left.
C
	    ip1 = ippt1 ( idirc )
	    ip2 = ippt2 ( idirc )
	    ip3 = ippt3 ( idirc )
	    ip4 = ippt4 ( idirc )
C
C*	    Check for crossings to right, straight and left.
C
	    tstr = ( boxmod ( ip1 ) * boxmod ( ip2 ) .lt. 0. )
	    tsts = ( boxmod ( ip2 ) * boxmod ( ip3 ) .lt. 0. )
	    tstl = ( boxmod ( ip3 ) * boxmod ( ip4 ) .lt. 0. )
C
C*	    If there is more than one possible direction, the top
C*	    and bottom sides turn left, and the left and right
C*	    sides turn right.
C
	    IF  ( tstr .and. tstl )  THEN
		IF  ( ( idirc .eq. 1 ) .or. ( idirc .eq. 3 ) )  THEN
		    tstr = .false.
		  ELSE
		    tstl = .false.
		END IF
		tsts = .false.
	    END IF
C
C*	    Get new direction and points to interpolate.
C
	    IF  ( tstr )  THEN
		idirn = idirc + 1
		IF  ( idirn .eq. 5 )  idirn = 1
		dist = FIND0 ( box (ip1), box (ip2) )
		IF  ( ( idirc .eq. 3 ) .or. ( idirc .eq. 4 ) )
     +					dist = 1. - dist
	      ELSE IF  ( tsts )  THEN
		idirn = idirc
		dist  = FIND0 ( box (ip2), box (ip3) )
		IF  ( ( idirc .eq. 1 ) .or. ( idirc .eq. 4 ) )
     +					dist = 1. - dist
	      ELSE IF  ( tstl )  THEN
		idirn = idirc - 1
		IF  ( idirn .eq. 0 )  idirn = 4
		dist = FIND0 ( box (ip3), box (ip4) )
		IF  ( ( idirc .eq. 1 ) .or. ( idirc .eq. 2 ) )
     +					dist = 1. - dist
	      ELSE
		iret = -3
		RETURN
	    END IF
C
C*	    Compute value based on new direction.
C
	    IF  ( idirn .eq. 1 )  THEN
		x1 = xll + ( illbox - 1. + dist ) * fincxy
		y1 = yll + ( jllbox ) * fincxy
		jllbox = jllbox + 1
		IF  ( jllbox .eq. numsub )  THEN
		    edge = .true.
		    jll  = jll + 1
		    IF  ( jll .ge. jsize )  THEN
			iret = -3
			ihline ( (jll-1)*isize+ill ) = 
     +					ihline ( (jll-1)*isize+ill ) + 1
		      ELSE IF  ( ihline ( (jll-1)*isize+ill ) .gt. 1 )
     +								    THEN
			iret = -5
		    END IF
		END IF
	      ELSE IF  ( idirn .eq. 2 )  THEN
		x1 = xll + ( illbox ) * fincxy
		y1 = yll + ( jllbox - 1. + dist ) * fincxy
		illbox = illbox + 1
		IF  ( illbox .eq. numsub )  THEN
		    edge = .true.
		    ill  = ill + 1
		    IF  ( ill .ge. isize )  THEN
			iret = -3
			ivline ( (jll-1)*isize+ill ) = 
     +				      ivline ( (jll-1)*isize+ill ) + 1
		      ELSE IF  ( ivline ( (jll-1)*isize+ill ) .gt. 1 )
     +								    THEN
			iret = -5
		    END IF
		END IF
	      ELSE IF  ( idirn .eq. 3 )  THEN
		x1 = xll + ( illbox - 1. + dist ) * fincxy
		y1 = yll + ( jllbox - 1. ) * fincxy
		jllbox = jllbox - 1
		IF  ( jllbox .lt. 1 )  THEN
		    edge = .true.
		    jll  = jll - 1
		    IF  ( jll .lt. 1 )  THEN
			iret = -3
			ihline ( ill ) = 
     +					  ihline ( ill ) + 1
		      ELSE IF  ( ihline ( (jll)*isize+ill ) .gt.
     +							      1 )  THEN
			iret = -5
		    END IF
		END IF
	      ELSE IF  ( idirn .eq. 4 )  THEN
		x1 = xll + ( illbox - 1. ) * fincxy
		y1 = yll + ( jllbox - 1. + dist ) * fincxy
		illbox = illbox - 1
		IF  ( illbox .lt. 1 )  THEN
		    edge = .true.
		    ill  = ill - 1
		    IF  ( ill .lt. 1 )  THEN
			iret = -3
			ivline ( (jll-1)*isize+1 ) =
     +					ivline ( (jll-1)*isize+1 )
     +						     + 1
		      ELSE IF  ( ivline ( (jll-1)*isize+ill+1 ) .gt. 1 )
     +								    THEN
			iret = -5
		    END IF
		END IF
	    END IF
C
C*	    Add point to buffer.
C
	    number = number + 1
	    IF  ( number .eq. 1024 )  THEN
		number = 1023
		xval1  = xval (1023)
		yval1  = yval (1023)
		CALL CCPLOT  ( cval, clabel, ilabel, scflag, iret )
		xval (1) = xval1
		yval (1) = yval1
		number   = 2
	    END IF
	    xval (number) = x1
	    yval (number) = y1
C
C*	    Set new direction.
C
	    idirc = idirn
C
C*	    Check for looping within box.
C
	    knt = knt + 1
	    IF  ( knt .gt. maxk )  THEN
		iret = -5
		edge = .true.
	    END IF
	END DO
C*
	RETURN
	END
