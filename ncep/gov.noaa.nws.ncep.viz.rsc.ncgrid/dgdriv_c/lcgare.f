	SUBROUTINE LC_GARE  ( garea, grltln, cdproj, centrd, iret )
C************************************************************************
C* LC_GARE								*
C*									*
C* This subroutine processes the input variable GAREA.  Information	*
C* about the type of area input is returned.  Only those area types	*
C* which specify a latitude/longitude range are valid.  If CDPROJ is	*
C* not blank, it contains the default projection string.		*
C*									*
C* LC_GARE  ( GAREA, GRLTLN, CDPROJ, CENTRD, IRET )			*
C*									*
C* Input parameters:							*
C*	GAREA		CHAR*		Graphics area name		*
C*									*
C* Output parameters:                                                   *
C*	GRLTLN (4)	REAL		Latitude/longitude bounds	*
C*	CDPROJ		CHAR*		Default projection string	*
C*	CENTRD (2)	REAL		Centroid latitude/longitude	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -5 = invalid garea name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84	IPAREA					*
C* I. Graffman/RDS	12/84	modified IPAREA for IPGAREA		*
C* I. Graffman/RDS	 5/86	Converted to IN_GAREA			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence		*
C* G. Krueger/EAI	 6/96	Add default projection			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grltln (*), centrd (*)
	CHARACTER*(*)	garea, cdproj
C*
	CHARACTER	state*8, stn (LLMXST)*8
C-----------------------------------------------------------------------
	iret = 0
C
	centrd (1) = 0.0
	centrd (2) = 0.0
C
C*	Get the area type from LC_ABND. 
C
	CALL LC_ABND  ( garea, igarty, grltln (1), grltln (2), 
     +			grltln(3), grltln (4), stn, nstn, state,
     +			cdproj, cenlat, cenlon, ier )
cc	print*, ' In LC_GARE ----> ', ier, igarty
	IF ( (ier .lt. 0) .or. (igarty .gt. 3) )  THEN
	    iret = -5
	ELSE
	    centrd (1) = cenlat
	    centrd (2) = cenlon
	END IF
C*
	RETURN
	END
