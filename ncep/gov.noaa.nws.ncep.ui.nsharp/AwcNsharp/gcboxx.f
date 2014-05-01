	SUBROUTINE GCBOXX  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
     +                       clvl, icolr, lintyp, linwid, linlbl, iret )
C************************************************************************
C* GCBOXX								*
C* 									*
C* This subroutine draws contours through a grid of data.  The 		*
C* contour fill algorithm is used and the subgrid boxes are outlined	*
C* with the specified lines instead of filled polygons.			*
C* 									*
C* GCBOXX  ( KX, KY, GRID, IOFFX, IOFFY, ISKIP, NLVL, CLVL, ICOLR,	*
C*           LINTYP, LINWID, LINLBL, IRET )				*
C*									*
C* Input parameters:							*
C*									*
C*	KX		INTEGER		Number of x grid points 	*
C*	KY		INTEGER		Number of y grid points		*
C*	GRID (KX,KY)	REAL		Grid data array			*
C*	IOFFX		INTEGER		X offset to first point		*
C*	IOFFY		INTEGER		Y offset to first point		*
C*	ISKIP		INTEGER		Skip factor in original grid	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour level values		*
C*	ICOLR  (NLVL+1)	INTEGER		Contour color numbers		*
C*	LINTYP (NLVL+1)	INTEGER		Contour line types		*
C*	LINWID (NLVL+1)	INTEGER		Contour line widths		*
C*	LINLBL (NLVL+1)	INTEGER		Contour label types		*
C*									*
C* Output parameters:							*
C*									*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From GCONTR				*
C* S. Jacobs/NCEP	 1/96	Added CADJST to adjust grid values	*
C* T. Lee/GSC		 9/97	Fixed typo, NOMOMO -> NOMONO		*
C* S. Jacobs/NCEP	 2/00	Eliminate subboxing when CONTUR=1	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		grid (*), clvl (*)
	INTEGER		icolr (*), linlbl (*), lintyp (*), linwid (*)
C*
	LOGICAL		linflg, bad
	REAL		clev (LLCLEV)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Move the contour levels into a local array in order to add a
C*	maximum level.
C
	IF  ( nlvl .lt. LLCLEV )  THEN
	    nlev = nlvl
	  ELSE
	    nlev = LLCLEV - 1
	END IF
	bad = .false.
	DO  i = 1, nlev
	    clev (i) = clvl (i)
	    IF  ( i .gt. 1 )  THEN
		IF  ( clev (i) .le. clev (i-1) )  bad = .true.
	    END IF
	END DO
C
C*	If levels are not in the correct order, set error and return.
C
	IF  ( bad )  THEN
	    iret = NOMONO
	    RETURN
	END IF
C
C*	Build dummy maximum level.
C
	CALL GR_STAT  ( grid, kx, ky, 1, 1, kx, ky, gmin, gmax, gavg,
     +			gdev, ier )
	high = 2 * ABS ( gmax ) + ABS ( gmin )
	IF  ( high .le. clev (nlev) ) high = clev (nlev) + 1
	nlev = nlev + 1
	clev (nlev) = high
C
C*	Read in the box subset factor.  Check that it is valid and set
C*	it in common for the rest of the original contouring code.
C
	IF  ( jbxsub .lt. 2 )  THEN
	    numsub = 2
	  ELSE IF  ( jbxsub .gt. 48 )  THEN
	    numsub = 50
	  ELSE
	    numsub = jbxsub + 2
	END IF
C
C*	Move offsets and skip factor into common.
C
	offx = FLOAT ( ioffx )
	offy = FLOAT ( ioffy )
	skip = FLOAT ( iskip )
C
C*	This program expects the data in the Z array in common.
C
	DO  i = 1, kx * ky
	    z (i) = grid (i)
	END DO
C
C*	Adjust the grid values.
C
	CALL CADJST ( kx, ky, nlev, clev, z, ier )
C
C*	Save grid size as ISIZE, JSIZE.
C
	isize = kx
	jsize = ky
C
C*	Compute distance along each subbox.
C
	fincxy = 1. / FLOAT ( numsub - 1 )
C
C*	Call the driver for the fill contouring.
C
	linflg = .true.
	CALL FFDRIV  ( nlev, clev, icolr, lintyp, linwid, linlbl, 
     +		       linflg, ier )
C*
	RETURN
	END
