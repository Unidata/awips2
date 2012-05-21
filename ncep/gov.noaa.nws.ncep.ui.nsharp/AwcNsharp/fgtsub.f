	SUBROUTINE FGTSUB  ( ill, jll, isub, jsub, box, xx1, yy1, xx2,
     +			     yy2, iret )
C************************************************************************
C* FGTSUB								*
C*									*
C* This subroutine gets the grid values at the four corners of the 	*
C* subbox whose lower left corner is ( ill, jll ).			*
C*									*
C* FGTSUB  ( ILL, JLL, ISUB, JSUB, BOX, XX1, YY1, XX2, YY2, IRET )	*
C*									*
C* Input parameters:							*
C*	ILL		INTEGER		Grid box x coordinate		*
C*	JLL		INTEGER		Grid box y coordinate		*
C*	ISUB		INTEGER		Subgrid x coordinate		*
C*	JSUB		INTEGER		Subgrid y coordinate		*
C*									*
C* Output parameters:							*
C*	BOX    (4)	REAL		LL, UL, UR, LR values		*
C*	XX1		REAL		Lower left x grid coordinate	*
C*	YY1		REAL		Lower left y grid coordinate	*
C*	XX2		REAL		Upper right x grid coordinate	*
C*	YY2		REAL		Upper right y grid coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = corner invalid		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91	From CCGTSB				*
C* T. Lee/GSC		 9/97	Fixed actual box indices and values	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4)
C
C*	Statement function to evaluate subgrid points.
C
C
	ZVAL (X,Y) = (((((C33*Y+C32)*Y+C31)*Y+C30)*X+((C23*Y+C22)*
     +			      Y+C21)*Y+C20)*X+
     +			((C13*Y+C12)*Y+C11)*Y+C10)*X+((C03*Y+C02)*
     +			      Y+C01)*Y+C00
C------------------------------------------------------------------------
	iret = 0
C
C*	Evaluate curve at all four points.
C
	x1 = FLOAT ( isub - 1 ) * fincxy
	y1 = FLOAT ( jsub - 1 ) * fincxy
	IF  ( isub .lt. numsub - 1 )  THEN
	    x2 = FLOAT ( isub ) * fincxy
	  ELSE
	    x2 = 1.
	END IF
	IF  ( jsub .lt. numsub - 1 )  THEN
	    y2 = FLOAT ( jsub ) * fincxy
	  ELSE
	    y2 = 1.
	END IF
C
C*	Get all four corners.
C
	box (1) = ZVAL ( x1, y1 )
	box (2) = ZVAL ( x2, y1 )
	box (3) = ZVAL ( x2, y2 )
	box (4) = ZVAL ( x1, y2 )
C
C*	Check whether actual grid points should be used.
C
	IF  ( ( isub .eq. 1 ) .and. ( jsub .eq. 1 ) )  THEN
	    box (1) = z ( (jll-1)*isize+ill )
	END IF
	IF  ( ( isub .eq. numsub - 1 ) .and. ( jsub .eq. 1 ) )  THEN
	    box (2) = z ( (jll-1)*isize+ill+1 )
	END IF
	IF  ( ( isub .eq. numsub - 1 ) .and.
     +		     ( jsub .eq. numsub - 1 ) )  THEN
	    box (3) = z ( (jll)*isize+ill+1 )
	END IF
	IF  ( ( isub .eq. 1 ) .and. ( jsub .eq. numsub - 1 ) )  THEN
	    box (4) = z ( (jll)*isize+ill )
	END IF
C
C*	Get corner coordinates of box.
C
	xx1 = x1 + ill
	yy1 = y1 + jll
	IF  ( isub .eq. numsub )  THEN
	    xx2 = ill + 1
	  ELSE
	    xx2 = x2 + ill
	END IF
	IF  ( jsub .eq. numsub )  THEN
	    yy2 = jll + 1
	  ELSE
	    yy2 = y2 + jll
	END IF
C*
	RETURN
	END
