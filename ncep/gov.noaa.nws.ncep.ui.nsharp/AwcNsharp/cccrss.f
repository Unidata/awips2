	SUBROUTINE CCCRSS  ( ifrst, iscnd, i, j, cval, iret )
C************************************************************************
C* CCCRSS								*
C*									*
C* This subroutine checks the four corners of the grid box for missing	*
C* data. If ifrst and iscnd are non-zero, the routine determines	*
C* whether the current contour level crosses between the specified	*
C* corners.
C*									*
C* CCCRSS  ( IFRST, ISCND, I, J, CVAL, IRET )				*
C*									*
C* Input parameters:							*
C*	IFRST		INTEGER		One corner to get		*
C*	ISCND		INTEGER		Other corner to get		*
C*	I		INTEGER		Lower left x coordinate		*
C*	J		INTEGER		Lower left y coordinate		*
C*	CVAL		REAL		Contour level			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = corner invalid		*
C*					 -2 = missing data		*
C*					 -6 = contour crossing		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89	GEMPAK 5				*
C* J. Whistler/SSAI	 6/91	Set internal grids to size LLMXDG	*
C* M. desJardins/NMC	12/91	Renamed: CGTBOX-->CCGTBX		*
C* J. Whistler/NSSFC	12/94	Changed to compute only two corners and *
C*				Added a call to check for missing data	*
C* G. Krueger/EAI	12/94	CCGTBX->CCCRSS				*
C* S. Jacobs/NMC	 1/95	Moved check for out of bounds		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
 	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4)
	INCLUDE		'ERMISS.FNC'
C
C*	Statement function to get value.
C
	FVAL ( i, j ) = z ( (j-1)*isize+i )
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for endpoints out of bounds.
C
	IF  ( ( i .lt. 1 ) .or. ( j .lt. 1 ) .or.
     +	      ( i .gt. isize - 1 ) .or. ( j .gt. jsize - 1 ) )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Get values at corners.
C
	box ( 1 ) = FVAL ( i  , j   )
	box ( 2 ) = FVAL ( i  , j+1 )
	box ( 3 ) = FVAL ( i+1, j+1 )
	box ( 4 ) = FVAL ( i+1, j   )
C
C*	Set return code.
C
	IF  ( ERMISS ( box (1) ) .or. ERMISS ( box (2) ) .or.
     +	      ERMISS ( box (3) ) .or. ERMISS ( box (4) ) ) THEN
	    iret = -2
	    RETURN
	ELSE IF ( ifrst .gt. 0 .and. iscnd .gt. 0 .and.
     +		  ( ( box (ifrst) .ge. cval .and.
     +		      box (iscnd) .lt. cval ) .or.
     +		    ( box (ifrst) .lt. cval .and.
     +		      box (iscnd) .ge. cval ) ) ) THEN
	    iret = -6
	    RETURN
	END IF
C*
	RETURN
	END
