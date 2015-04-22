	SUBROUTINE CCCHCK  ( cval, ill, jll, idirc, illbox, jllbox,
     +			     iret )
C************************************************************************
C* CCCHCK								*
C*									*
C* This subroutine checks that the contour has entered the correct	*
C* subbox.								*
C*									*
C* CCCHCK  ( CVAL, ILL, JLL, IDIRC, ILLBOX, JLLBOX, IRET )		*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour value			*
C*	ILL		INTEGER		Lower left x value of box	*
C*	JLL		INTEGER		Lower left y value of box	*
C*	IDIRC		INTEGER		Current direction		*
C*									*
C* Output parameters:							*
C*	ILLBOX		INTEGER		Lower left x value of subbox	*
C*	JLLBOX		INTEGER		Lower left y value of subbox	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = edge reached		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89	GEMPAK 5				*
C* M. desJardins/NMC	12/91	Renamed: CSCHCK-->CCCHCK		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		box (4), boxmod (4)
C*
	INTEGER		ip1arr (4), ip2arr (4)
	DATA		ip1arr / 1, 1, 2, 4 /, 
     +			ip2arr / 4, 2, 3, 3 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Get last point from contour buffer.
C
	x1 = xval (number)
	y1 = yval (number)
C
C*	Compute fraction along side of box.
C
	IF  ( ( idirc .eq. 1 ) .or. ( idirc .eq. 3 ) )  THEN
	    ix   = x1
	    frac = x1 - FLOAT ( ix )
	  ELSE
	    iy   = y1
	    frac = y1 - FLOAT ( iy )
	END IF
C
C*	Get subbox crossing.
C
	isubb = frac / fincxy
	isubb = isubb + 1
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
C*	Get corners of the subbox.
C
	CALL CCGTSB  ( illbox, jllbox, ill, jll, cval, box, boxmod, 
     +		       iret )
C
C*	Get points on corners along side entering box.
C
	ip1 = ip1arr ( idirc )
	ip2 = ip2arr ( idirc ) 
	z1  = boxmod ( ip1 )
	z2  = boxmod ( ip2 )
C
C*	Check for contour crossing.  If found return. Everything is ok.
C
	IF  ( z1 * z2 .lt. 0. )  RETURN
C
C*	Otherwise, find first point.
C
	isub1  = isubb - 3
	IF  ( isub1 .lt. 1 )  isub1 = 1
	number = number - 1
	CALL CCSTRT  ( cval, ill, jll, idirc, isub1, illbox, jllbox,
     +		       iret )
C
C*	Check for error in box.
C
	IF  ( iret .ne. 0 )  THEN
	    number = number + 1
	    RETURN
	END IF
C
C*	Check that box is within 3 boxes of original subbox.
C
	x1 = xval (number)
	y1 = yval (number)
C
C*	Compute fraction along side of box.
C
	IF  ( ( idirc .eq. 1 ) .or. ( idirc .eq. 3 ) )  THEN
	    inewb = illbox
	  ELSE
	    inewb = jllbox
	END IF
C
C*	Get subbox crossing.
C
	idiff = ABS ( inewb - isubb )
	IF  ( idiff .gt. 3 )  THEN
	    iret = -3
	END IF
C*
	RETURN
	END
