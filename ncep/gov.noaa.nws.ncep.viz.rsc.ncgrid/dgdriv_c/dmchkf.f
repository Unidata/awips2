	SUBROUTINE DM_CHKF ( iflno, iret )
C************************************************************************
C* DM_CHKF								*
C*									*
C* This subroutine checks that the input file number is in the		*
C* proper range and that a file has been opened using that number.	*
C*									*
C* DM_CHKF  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number to check		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Check that file number is in proper range and then see that a 
C*	file has been opened using that file number, i.e. the logical 
C*	unit number for the file is non-negative.
C
	iret = 0
	IF  ( ( iflno .lt. 1) .or. ( iflno .gt. MMFILE ) ) THEN
	    iret = -4
	  ELSE
	    IF  ( lundm (iflno) .lt. 0 ) iret = -4
	END IF
C*
	RETURN
	END
