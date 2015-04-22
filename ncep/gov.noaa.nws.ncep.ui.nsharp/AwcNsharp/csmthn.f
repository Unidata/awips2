	SUBROUTINE CSMTHN  ( nsmoth, npts, xpt, ypt, iret )
C************************************************************************
C* CSMTHN								*
C*									*
C* This subroutine applies a 3-point smoothing to the buffered contour	*
C* line.  The smoothing is performed NSMOTH times.			*
C*									*
C* CSMTHN  ( NSMOTH, NPTS, XPT, YPT, IRET )				*
C*									*
C* Input parameters:							*
C*	NSMOTH		INTEGER		Number of smoothing passes	*
C*	NPTS		INTEGER		Number of points		*
C*									*
C* Input and output parameters:						*
C*	XPT (NPTS)	REAL		X grid points			*
C*	YPT (NPTS)	REAL		Y grid points			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From old CSMOTH				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		xpt (*), ypt (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that there are at least two points.
C
	IF  ( npts .lt. 2 )  RETURN
C
C*	Loop through the points NSMOTH times.
C
	DO  ii = 1, nsmoth
	  xim1 = xpt (1)
	  yim1 = ypt (1)
	  DO  i = 2, npts - 1 
	    xi = xpt (i)
	    yi = ypt (i)
	    xpt (i) = ( xim1 + xpt (i+1) + xi + xi ) / 4.
	    ypt (i) = ( yim1 + ypt (i+1) + yi + yi ) / 4.
	    xim1 = xi
	    yim1 = yi
	  END DO
	END DO
C*
	RETURN
	END
