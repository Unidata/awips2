	SUBROUTINE GCNTLN  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
     +			     clvl, clbl, icolr, lintyp, linwid, linlbl, 
     +			     scflag, iret )
C************************************************************************
C* GCNTLN								*
C* 									*
C* This subroutine draws contours through a grid of data.  The 		*
C* algorithm used is based on a two-dimensional Lagrangian fitting of	*
C* the grid points.  It is the original GEMPAK contouring program.	*
C* The original subroutine, GCONTR, had a different calling sequence.	*
C* 									*
C* GCNTLN  ( KX, KY, GRID, IOFFX, IOFFY, ISKIP, NLVL, CLVL, CLBL, 	*
C*	     ICOLR, LINTYP, LINWID, LINLBL, SCFLAG, IRET )		*
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
C*	CLBL   (NLVL)	CHAR*		Contour labels			*
C*	ICOLR  (NLVL)	INTEGER		Contour color numbers		*
C*	LINTYP (NLVL)	INTEGER		Contour line types		*
C*	LINWID (NLVL)	INTEGER 	Contour line widths		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*									*
C* Output parameters:							*
C*									*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Li/GSC		 1/00	Copied from GCLGRN			*
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		grid (*), clvl (*)
	INTEGER		icolr (*), lintyp (*), linwid (*), linlbl (*)
	CHARACTER*(*)	clbl (*)
	LOGICAL		scflag
C------------------------------------------------------------------------
	iret = NORMAL
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
	CALL CADJST ( kx, ky, nlvl, clvl, z, ier )
C
C*	Save grid size as ISIZE, JSIZE.
C
	isize = kx
	jsize = ky
C
C*	Call the driver for the Lagrangian contouring.
C
	CALL CLDRV2  ( nlvl, clvl, clbl, icolr, lintyp, linwid, 
     +		       linlbl, scflag, ier )
C*
	RETURN
	END
