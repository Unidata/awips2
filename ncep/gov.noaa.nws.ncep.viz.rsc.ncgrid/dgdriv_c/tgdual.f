	SUBROUTINE TG_DUAL  ( time, clntm, iret )
C************************************************************************
C* TG_DUAL								*
C*									*
C* This subroutine converts a time(2) array into a colon separated dual	*
C* grid time stamp.							*
C*									*
C* TG_DUAL  ( TIME, CLNTM, IRET )					*
C*									*
C* Input parameters:							*
C*	TIME (2)	CHAR*		Grid time array			*
C*									*
C* Output parameters:							*
C*	CLNTM		CHAR*36		Colon separated dual grid time	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/HPC		 2/04						*
C************************************************************************
	CHARACTER*(*)	time (*), clntm
C*
C------------------------------------------------------------------------
	iret  = 0
C*
	CALL ST_RMBL ( time (1), time(1), len1, ier )
	CALL ST_RMBL ( time (2), time(2), len2, ier )
	CALL ST_LSTR ( time (1), lt1, ier )
	CALL ST_LSTR ( time (2), lt2, ier )
	IF ( time (2) .ne. ' ' ) THEN
	    clntm = time (1)(1:lt1) // ':' // time (2)
	ELSE
	    clntm = time (1)(1:lt1)
	END IF
C*
	RETURN
	END
