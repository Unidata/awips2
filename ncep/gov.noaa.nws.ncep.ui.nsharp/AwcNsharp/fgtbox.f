	SUBROUTINE FGTBOX  ( ill, jll, box, xx1, yy1, xx2, yy2, iret )
C************************************************************************
C* FGTBOX								*
C*									*
C* This subroutine gets the corners of the current grid box.  The grid	*
C* itself is stored in common.						*
C*									*
C* FGTBOX  ( ILL, JLL, BOX, XX1, YY1, XX2, YY2, IRET )			*
C*									*
C* Input parameters:							*
C*	ILL		INTEGER		X coordinate			*
C*	JLL		INTEGER		Y coordinate			*
C*									*
C* Output parameters:							*
C*	BOX (4)		REAL		Corners of box			*
C*	XX1		REAL		Lower left x coordinate		*
C*	YY1		REAL		Lower left y coordinate		*
C*	XX2		REAL		Upper right x coordinate	*
C*	YY2		REAL		Upper right y coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91				 		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4)
C*
	INCLUDE		'ERMISS.FNC'
C----------------------------------------------------------------------
	iret = 0
C
C*	Get the corners of the box.
C
	box (1) = z ( (jll-1) * isize + ill )
	box (2) = z ( (jll-1) * isize + ill + 1 )
	box (3) = z ( ( jll ) * isize + ill + 1 )
	box (4) = z ( ( jll ) * isize + ill )
C
C*	Check for missing data.
C
	IF  ( ( ERMISS ( box (1) ) ) .or.
     +	      ( ERMISS ( box (2) ) ) .or.
     +	      ( ERMISS ( box (3) ) ) .or.
     +	      ( ERMISS ( box (4) ) ) )  THEN
	    iret = -1
	  ELSE
	    xx1 = ill
	    yy1 = jll
	    xx2 = xx1 + 1.0
	    yy2 = yy1 + 1.0
	END IF
C*
	RETURN
	END
