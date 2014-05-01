	SUBROUTINE CCGTSB  ( i, j, ill, jll, cval, box, boxmod, iret )
C************************************************************************
C* CCGTSB								*
C*									*
C* This subroutine gets the grid values minus the contour level at	*
C* the four corners of the box whose lower left corner is ( i, j ).	*
C* Any value of 0. is set to a positive number in boxmod.  If any	*
C* corner lies on a grid point, the grid value is returned.		*
C*									*
C* CCGTSB  ( I, J, ILL, JLL, CVAL, BOX, BOXMOD, IRET )			*
C*									*
C* Input parameters:							*
C*	I		INTEGER		Lower left x coordinate		*
C*	J		INTEGER		Lower left y coordinate		*
C*	ILL		INTEGER		??				*
C*	JLL		INTEGER		??				*
C*	CVAL		REAL		Contour level			*
C*									*
C* Output parameters:							*
C*	BOX    (4)	REAL		LL, UL, UR, LR values		*
C*	BOXMOD (4)	REAL		Non-zero values for BOX		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = corner invalid		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89	GEMPAK 5				*
C* J. Whistler/SSAI	 6/91	Set internal grids to size LLMXDG	*
C* M. desJardins/NMC	12/91	Renamed: CGTSUB-->CCGTSB		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4), boxmod (4)
C
C*	Statement function to evaluate subgrid points.
C
C
	ZVAL (X,Y,CVAL) = (((((C33*Y+C32)*Y+C31)*Y+C30)*X+((C23*Y+C22)*
     +				Y+C21)*Y+C20)*X+
     +			     ((C13*Y+C12)*Y+C11)*Y+C10)*X+((C03*Y+C02)*
     +				Y+C01)*Y+C00 - cval 
C------------------------------------------------------------------------
C*	Set return code.
C
	IF  ( ( i .lt. 1 ) .or. ( j .lt. 1 ) .or. ( i .ge. numsub )
     +			   .or. ( j .ge. numsub ) )  THEN
	    iret = -1
	    RETURN
	  ELSE
	    iret = 0
	END IF
C
C*	Evaluate curve at all four points.
C
	x1 = FLOAT ( i - 1 ) * fincxy
	y1 = FLOAT ( j - 1 ) * fincxy
C
C*	Get all four corners.
C
	box (1) = ZVAL ( x1, y1, cval )
	box (2) = ZVAL ( x1, y1+fincxy, cval )
	box (3) = ZVAL ( x1+fincxy, y1+fincxy, cval )
	box (4) = ZVAL ( x1+fincxy, y1, cval )
C
C*	Check whether actual grid points should be used.
C
	IF  ( ( i .eq. 1 ) .and. ( j .eq. 1 ) )  THEN
	    box (1) = z ( (jll-1)*isize+ill ) - cval
	END IF
	IF  ( ( i .eq. 1 ) .and. ( j .eq. numsub - 1 ) )  THEN
	    box (2) = z ( (jll)*isize+ill ) - cval
	END IF
	IF  ( ( i .eq. numsub - 1 ) .and.
     +		     ( j .eq. numsub - 1 ) )  THEN
	    box (3) = z ( (jll)*isize+ill+1 ) - cval
	END IF
	IF  ( ( i .eq. numsub - 1 ) .and. ( j .eq. 1 ) )  THEN
	    box (4) = z ( (jll-1)*isize+ill+1 ) - cval
	END IF
C
C*	If the value if zero, set it to a positive number.
C
	DO  k = 1, 4
	    IF  ( box (k) .ne. 0. )  THEN
		boxmod (k) = box (k)
	      ELSE
		boxmod (k) = 1.
	    END IF
	END DO
C*
	RETURN
	END
