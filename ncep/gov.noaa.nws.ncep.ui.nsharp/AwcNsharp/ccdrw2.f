	SUBROUTINE CCDRW2  ( cval, clabel, ilabel, intror, ii, jj, 
     +			     idire, scflag, iret )
C************************************************************************
C* CCDRW2								*
C*									*
C* This subroutine draws contours.					*
C*									*
C* CCDRW2  ( CVAL, CLABEL, ILABEL, INTROR, II, JJ, IDIRE, SCFLAG, 	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour level			*
C*	CLABEL		CHAR*		Contour label			*
C*	ILABEL		INTEGER		Label type			*
C*	INTROR		LOGICAL		Interior start flag		*
C*	II		INTEGER		Lower left column of box	*
C*	JJ		INTEGER		Lower left row of box		*
C*	IDIRE		INTEGER		Direction entering box		*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C* 					 -5 = TROUBLE			*
C**									*
C* Log:									*
C* M. Li/GSC		 1/00	Copied from CCDRAW			*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* C. Bailey/HPC	 6/06	Added contour label string		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	CHARACTER*24	clabel(LLCLEV)
        LOGICAL		intror, scflag
C*
        LOGICAL		start, edge, closed, revers, cont,
     +			tstr, tsts, tstl
C*
        REAL		box (4), bval (4)
C*
        INTEGER		ippt1 (4), ippt2 (4), ippt3 (4), ippt4 (4),
     +			ip1arr (4), ip2arr (4)
	INCLUDE         'ERMISS.FNC'
	DATA		ippt1 / 4, 1, 2, 3 /,
     +			ippt2 / 3, 4, 1, 2 /,
     +			ippt3 / 2, 3, 4, 1 /,
     +			ippt4 / 1, 2, 3, 4 /,
     +			ip1arr / 1, 1, 2, 4 /,
     +			ip2arr / 4, 2, 3, 3 /
C*
C
C*	This statement function gets the fractional distance along a
C*	side to the value 0.
C
	FIND0 ( x, y ) = x / ( x - y )
C
C*	Statement function to get grid value.
C
	FVAL ( i, j ) = z ( (j-1) * isize + i )
C*
C------------------------------------------------------------------------
	iret   = 0
	jlabel = ilabel
C
C*	Initialize variables.
C
	number = 0
	ill    = ii
	jll    = jj
	idirc  = idire
	revers = .false.
	edge   = .false.
	closed = .false.
	start  = .true.
	cont   = .true.
C
C*	Save initial points.
C
	ills  = ill
	jlls  = jll
	idirs = idirc
C
C*	Draw grid till the edge is reached.
C
	knt = 0
	DO WHILE  ( ( .not. edge ) .and. ( .not. closed ) )
	    knt = knt + 1
	    IF ( knt .gt. 2048 ) THEN
	      iret = -5
              RETURN
            END IF
C*
	    xll  = FLOAT ( ill )
	    yll  = FLOAT ( jll )
C
C*	    Set flag indicating we've been here.
C
	    IF  ( idirc .eq. 1 )  THEN
		ihline ( (jll-1)*isize+ill ) =
     +			ihline ( (jll-1)*isize+ill ) + 1
	      ELSE IF  ( idirc .eq. 2 )  THEN
		ivline ( (jll-1)*isize+ill ) =
     +			ivline ( (jll-1)*isize+ill ) + 1
	      ELSE IF  ( idirc .eq. 3 )  THEN
		ihline ( (jll)*isize+ill ) = 
     +			ihline ( (jll)*isize+ill ) + 1
	      ELSE IF  ( idirc .eq. 4 )  THEN
		ivline ( (jll-1)*isize+ill+1 ) =
     +			ivline ( (jll-1)*isize+ill+1 ) + 1
	    END IF
C
C*	    First check for missing data.
C
	    bval (1) = FVAL ( ill,   jll   )
	    bval (2) = FVAL ( ill,   jll+1 )
	    bval (3) = FVAL ( ill+1, jll+1 )
	    bval (4) = FVAL ( ill+1, jll   )
	    IF  ( ERMISS ( bval(1) ) .or. ERMISS ( bval(2) ) .or. 
     +	          ERMISS ( bval(3) ) .or. ERMISS ( bval(4) ) )  THEN
		cont = .false.
		edge = .true.
	    END IF
C
C*	    If the points are ok, continue.
C
	    IF  ( cont )  THEN
C
C*	    	Get corners of this box.
C
		DO  k = 1, 4
		    box (k) = bval (k) - cval
		END DO
C
C*	    	Start the contour.
C
		IF  ( start )  THEN
		    ip1 = ip1arr ( idirc )
		    ip2 = ip2arr ( idirc )
		    IF  ( box ( ip1 ) * box ( ip2 ) .lt. 0. )  THEN
			dist = FIND0 ( box ( ip1 ), box ( ip2 ) )
			IF  ( idirc .eq. 1 )  THEN
			    x1 = xll + dist
			    y1 = yll
			  ELSE IF  ( idirc .eq. 2 )  THEN
			    x1 = xll
			    y1 = yll + dist
			  ELSE IF  ( idirc .eq. 3 )  THEN
			    x1 = xll + dist
			    y1 = yll + 1
			  ELSE IF  ( idirc .eq. 4 )  THEN
			    x1 = xll + 1
			    y1 = yll + dist
			END IF
			number = number + 1
			xval ( number ) = x1
			yval ( number ) = y1
		    END IF
		END IF
C
C*	    	Get points to look to right, straight and left.
C
		ip1 = ippt1 ( idirc )
		ip2 = ippt2 ( idirc )
		ip3 = ippt3 ( idirc )
		ip4 = ippt4 ( idirc )
C
C*	    	Check for crossings to right, straight and left.
C
		tstr = ( box ( ip1 ) * box ( ip2 ) .lt. 0. )
		tsts = ( box ( ip2 ) * box ( ip3 ) .lt. 0. )
		tstl = ( box ( ip3 ) * box ( ip4 ) .lt. 0. )
C
C*	    	If there is more than one possible direction, the top
C*		and bottom sides turn left, and the left and right
C*		sides turn right.
C
		IF  ( tstr .and. tstl )  THEN
		    IF  ( ( idirc .eq. 1 ) .or.
     +			  ( idirc .eq. 3 ) )  THEN
			tstr = .false.
		      ELSE
			tstl = .false.
		    END IF
		    tsts = .false.
		END IF
C
C*	    	Get new direction and points to interpolate.
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
		    cont = .false.
		END IF
	    END IF
C
C*	    If there are no errors, continue.
C
	    IF  ( cont )  THEN
C
C*	    	Compute value based on new direction.
C
		IF  ( idirn .eq. 1 )  THEN
		    x1  = xll + dist
		    y1  = yll + 1
		    jll = jll + 1
		    IF  ( jll .ge. jsize )  THEN
			edge = .true.
			ihline ( (jll-1)*isize+ill ) = 
     +				ihline ( (jll-1)*isize+ill ) + 1
		      ELSE IF  (ihline((jll-1)*isize+ill) .gt. 1) THEN
			edge = .true.
		    END IF
		  ELSE IF  ( idirn .eq. 2 )  THEN
		    x1  = xll + 1
		    y1  = yll + dist
		    ill = ill + 1
		    IF  ( ill .ge. isize )  THEN
			edge = .true.
			ivline ( (jll-1)*isize+ill ) = 
     +				ivline ( (jll-1)*isize+ill ) + 1
		      ELSE IF  (ivline((jll-1)*isize+ill) .gt. 1) THEN
			edge = .true.
		    END IF
		  ELSE IF  ( idirn .eq. 3 )  THEN
		    x1  = xll + dist
		    y1  = yll
		    jll = jll - 1
		    IF  ( jll .lt. 1 )  THEN
			edge = .true.
			ihline ( ill ) = ihline ( ill ) + 1
		      ELSE IF  (ihline((jll)*isize+ill) .gt. 1) THEN
			edge = .true.
		    END IF
		  ELSE IF  ( idirn .eq. 4 )  THEN
		    x1  = xll
		    y1  = yll + dist
		    ill = ill - 1
		    IF  ( ill .lt. 1 )  THEN
			edge = .true.
			ivline ( (jll-1)*isize+1 ) =
     +				ivline ( (jll-1)*isize+1 ) + 1
		      ELSE IF (ivline((jll-1)*isize+ill+1) .gt. 1) THEN
			edge = .true.
		    END IF
		END IF
C
C*	    	Add point to buffer.
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
C*	    	Set new direction.
C
		idirc = idirn
	    END IF
C*
	    IF  ( cont )  THEN
		IF  ( start )  THEN
		    x1save = xval (1)
		    y1save = yval (1)
		    start  = .false.
		END IF
C
C*	    	Check for closed curve.  In this case, set the last point
C*	    	to be the same as the first point to ensure closure.
C
		closed = ( idirc .eq. idirs ) .and.
     +			 ( ills .eq. ill ) .and.
     +			 ( jlls .eq. jll )
		IF  ( closed )  THEN
		    number = number + 1
		    xval ( number ) = x1save
		    yval ( number ) = y1save
		END IF
	    END IF
C
C*	    If an edge has been reached and the curve is not closed,
C*	    reverse directions and start again.
C
	    IF  ( edge .and. ( .not. revers ) .and. intror )  THEN
		revers = .true.
		idirc  = 4
		ill    = ills - 1
		jll    = jlls
		edge   = .false.
		CALL CCPLOT  ( cval, clabel, jlabel, scflag, ier )
		jlabel = 0
		number = 1
		xval (1) = x1save
		yval (1) = y1save
		cont = .true.
	    END IF
	END DO
C
C*	Contour has reached an edge so we are done.
C
	CALL CCPLOT  ( cval, clabel, ilabel, scflag, ier )
C*
	RETURN
	END
