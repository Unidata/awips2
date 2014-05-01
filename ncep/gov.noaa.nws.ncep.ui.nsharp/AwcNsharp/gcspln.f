	SUBROUTINE GCSPLN  ( kx, ky, ggrid, ioffx, ioffy, iskip, nlvl, 
     +			     clvl, icolr, lintyp, linwid, linlbl, iret )
C************************************************************************
C* GCSPLN								*
C* 									*
C* This subroutine draws contours through a grid of data.  The 		*
C* algorithm used is based on a two-dimensional Lagrangian fitting of	*
C* the grid points.  It is the original GEMPAK contouring program.	*
C* The original subroutine, GCONTR, had a different calling sequence.	*
C* 									*
C* GCSPLN  ( KX, KY, GGRID, IOFFX, IOFFY, ISKIP, NLVL, CLVL, ICOLR,	*
C*           LINTYP, LINWID, LINLBL, IRET )				*
C*									*
C* Input parameters:							*
C*									*
C*	KX		INTEGER		Number of x grid points 	*
C*	KY		INTEGER		Number of y grid points		*
C*	GGRID (KX,KY)	REAL		Grid data array			*
C*	IOFFX		INTEGER		X offset to first point		*
C*	IOFFY		INTEGER		Y offset to first point		*
C*	ISKIP		INTEGER		Skip factor in original grid	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour level values		*
C*	ICOLR  (NLVL)	INTEGER		Contour color numbers		*
C*	LINTYP (NLVL)	INTEGER		Contour line types		*
C*	LINWID (NLVL)	INTEGER 	Contour line widths		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*									*
C* Output parameters:							*
C*									*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From GCONTR				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C*
	REAL		ggrid (*), clvl (*)
	INTEGER		icolr (*), lintyp (*), linwid (*), linlbl (*)
C------------------------------------------------------------------------
	iret = NORMAL
	write (6,*) 'GPLT -- GCSPLN'
	RETURN
	END
