	SUBROUTINE CCLIP ( npin, xin, yin, ipoint, npout, xout, yout,
     +			   more, iret )
C************************************************************************
C* CCLIP								*
C*									*
C* This routine will clip points, from a line, that are not visible in	*
C* the bounded area.							*
C*									*
C* CCLIP ( NPIN, XIN, YIN, IPOINT, NPOUT, XOUT, YOUT, MORE, IRET )	*
C*									*
C* Input parameters:							*
C*	NPIN		INTEGER		Number of input points		*
C*									*
C* Input and output parameters:						*
C*	XIN (NPIN)	REAL		Input X grid points		*
C*	YIN (NPIN)	REAL		Input Y grid points		*
C*	IPOINT		INTEGER		Start point from the input pts	*
C*									*
C* Output parameters:							*
C*	NPOUT		INTEGER		Number of output points		*
C*	XOUT (NPOUT)	REAL		Output X grid points		*
C*	YOUT (NPOUT)	REAL		Output Y grid points		*
C*	MORE		LOGICAL		Flag for more points to process	*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	10/00	Created					*
C* S. Jacobs/NCEP	 5/01	Reorder points if the line is closed	*
C************************************************************************
	REAL		xin (*), yin (*), xout (*), yout (*)
	LOGICAL		more
C*
	LOGICAL		vis (1024), viso (1024)
C------------------------------------------------------------------------
	iret = 0
C
C*	Find the visible points for this line.
C
	CALL GPTVIS ( 'G', npin, xin, yin, vis, ier )
C
C*	If the line is closed work backward until the first
C*	non visible point. Then reorder the points so that the
C*	first visible point becomes point number 1.
C
	IF  ( ( ipoint .eq. 1 ) .and. vis (1) )  THEN
C
	    IF  ( ( xin(1) .eq. xin(npin) ) .and.
     +		  ( yin(1) .eq. yin(npin) ) )  THEN
C
		nn = npin - 1
C
		DO WHILE ( ( nn .ge. 1 ) .and. vis ( nn ) )
		    nn = nn - 1
		END DO
C
		IF  ( nn .gt. 0 )  THEN
		    j = nn + 1
		    DO  i = 1, npin - 1 
		    	IF  ( j .gt. npin - 1 )  j = 1
			xout ( i ) = xin ( j )
			yout ( i ) = yin ( j )
			viso ( i ) = vis ( j )
			j = j + 1
		    END DO
		    xout ( npin ) = xout ( 1 )
		    yout ( npin ) = yout ( 1 )
		    viso ( npin ) = viso ( 1 )
C
		    DO  i = 1, npin
		    	xin ( i ) = xout ( i )
		    	yin ( i ) = yout ( i )
		    	vis ( i ) = viso ( i )
		    END DO
		END IF
C
	    END IF
C
	END IF
C
C*	Loop through all of the input points, stop at the first point
C*	that is not visible.
C
	npout = 0
	DO WHILE ( ( ipoint .le. npin ) .and. vis ( ipoint ) )
	    npout = npout + 1
	    xout (npout) = xin (ipoint)
	    yout (npout) = yin (ipoint)
	    ipoint = ipoint + 1
	END DO
C
C*	Find the next visible point.
C
	DO WHILE ( ( ipoint .le. npin ) .and. .not. vis ( ipoint ) )
	    ipoint = ipoint + 1
	END DO
C
C*	Check for more points to process.
C
	more = ipoint .le. npin
C*
	RETURN
	END
