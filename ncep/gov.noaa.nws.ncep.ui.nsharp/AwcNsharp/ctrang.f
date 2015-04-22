	SUBROUTINE CTRANG  ( npts, xpt, ypt, xout, yout, iret )
C************************************************************************
C* CTRANG								*
C*									*
C* This subroutine transforms the subset grid point values to the	*
C* actual grid points.							*
C*									*
C* CTRANG  ( NPTS, XPT, YPT, XOUT, YOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	NPTS		INTEGER		Number of points		*
C*									*
C* Input and output parameters:						*
C*	XPT (NPTS)	REAL		X grid points			*
C*	YPT (NPTS)	REAL		Y grid points			*
C*									*
C* Output parameters:							*
C*	XOUT (NPTS)	REAL		Scaled x grid points		*
C*	YOUT (NPTS)	REAL		Scaled y grid points		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From old CSMOTH				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		xpt (*), ypt (*), xout (*), yout (*)
C------------------------------------------------------------------------
	iret = 0
	skip1 = skip + 1.
C
C*	Loop through the points.
C
	DO  i = 1, npts
	   xout (i) = ( xpt (i) - 1. ) * skip1 + 1. + offx
	   yout (i) = ( ypt (i) - 1. ) * skip1 + 1. + offy
	END DO
C*
	RETURN
	END
