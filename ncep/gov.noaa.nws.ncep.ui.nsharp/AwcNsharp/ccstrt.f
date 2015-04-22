	SUBROUTINE CCSTRT  ( cval, ill, jll, idirc, isubb, illbox, 
     +			     jllbox, iret )
C************************************************************************
C* CCSTRT								*
C*									*
C* This subroutine finds the start point of a contour line as it	*
C* enters the box whose lower left corner is ILL, JLL.			*
C*									*
C* CCSTRT  ( CVAL, ILL, JLL, IDIRC, ISUBB, ILLBOX, JLLBOX, IRET )	*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour level			*
C*	ILL		INTEGER		Lower left corner of box	*
C*	JLL		INTEGER		Lower left corner of box	*
C*	IDIRC		INTEGER		Direction entering box		*
C*	ISUBB		INTEGER		First subbox to check		*
C*									*
C* Output parameters:							*
C*	ILLBOX		INTEGER		Lower left corner of subbox	*
C*	JLLBOX		INTEGER		Lower left corner of subbox	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = no crossing found		*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* M. desJardins/GSFC	 6/89	Rewritten for GEMPAK 5			*
C* M. desJardins/NMC	12/91	Renamed: CNSTRT-->CCSTRT		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4), boxmod (4)
	LOGICAL		found
C*
	INTEGER		ip1arr (4), ip2arr (4)
	DATA		ip1arr / 1, 1, 2, 4 /, 
     +			ip2arr / 4, 2, 3, 3 /
C
C*	This statement function gets the fractional distance along a
C*	side to the value 0.
C
	FIND0 ( x, y ) = x / ( x - y )
C------------------------------------------------------------------------
	iret   = 0
	xll    = FLOAT ( ill )
	yll    = FLOAT ( jll )
C
C*	Check the edge which is entered.
C
	IF  ( idirc .eq. 1 )  THEN
	    illbox = isubb
	    jllbox = 1
	  ELSE IF  ( idirc .eq. 2 )  THEN
	    illbox = 1
	    jllbox = isubb
	  ELSE IF  ( idirc .eq. 3 )  THEN
	    illbox = isubb
	    jllbox = numsub - 1
	  ELSE IF  ( idirc .eq. 4 )  THEN
	    illbox = numsub - 1
	    jllbox = isubb
	END IF
C
C*	Loop through all subgrid boxes until crossing point is found.
C
	found = .false.
	DO WHILE  ( ( .not. found ) .and. ( illbox .lt. numsub )
     +				    .and. ( jllbox .lt. numsub ) )
C
C*	    Get corners of the subbox.
C
	    CALL CCGTSB  ( illbox, jllbox, ill, jll, cval, box, boxmod, 
     +			   iret )
C
C*	    Get points on corners along side entering box.
C
	    ip1 = ip1arr ( idirc )
	    ip2 = ip2arr ( idirc ) 
	    z1  = boxmod ( ip1 )
	    z2  = boxmod ( ip2 )
C
C*	    Check for contour crossing.
C
	    IF  ( z1 * z2 .lt. 0. )  THEN
		z1 = box ( ip1 )
		z2 = box ( ip2 )
		dist  = FIND0 ( z1, z2 )
		IF  ( dist .eq. 0. )  dist = fincxy / 4.
		IF  ( dist .eq. 1. )  dist = 1. - fincxy / 4.
		found = .true.
	      ELSE
		IF  ( ( idirc .eq. 2 ) .or. ( idirc .eq. 4 ) )  THEN
		    jllbox = jllbox + 1
		  ELSE
		    illbox = illbox + 1
		END IF
	    END IF
	END DO
C
C*	Exit if crossing was not found.
C
	IF  ( .not. found )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Get x and y value of point and save in common.
C
	IF  ( idirc .eq. 1 )  THEN
	    x1 = xll + ( illbox - 1. + dist ) * fincxy
	    y1 = yll
	  ELSE IF  ( idirc .eq. 2 )  THEN
	    x1 = xll
	    y1 = yll + ( jllbox - 1. + dist ) * fincxy
	  ELSE IF  ( idirc .eq. 3 )  THEN
	    x1 = xll + ( illbox - 1. + dist ) * fincxy
	    y1 = yll + 1.
	  ELSE IF  ( idirc .eq. 4 )  THEN
	    x1 = xll + 1.
	    y1 = yll + ( jllbox - 1. + dist ) * fincxy
	END IF
C
C*	Add this point to the common area.
C
	number = number + 1
	xval ( number ) = x1
	yval ( number ) = y1
C*
	RETURN
	END
