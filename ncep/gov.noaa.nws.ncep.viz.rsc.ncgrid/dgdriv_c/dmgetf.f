	SUBROUTINE  DM_GETF ( iflno, iret )
C************************************************************************
C* DM_GETF								*
C*									*
C* This subroutine gets the next file number to use.			*
C*									*
C* DM_GETF  ( IFLNO, IRET )						*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = too many files open	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/86						*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
C*	Initialize and find first available file number.
C
	iflno = 0
	iret  = 0
	DO  i = MMFILE, 1, -1
	    IF  ( lundm (i) .lt. 0 ) iflno = i
	END DO
	IF  ( iflno .eq. 0 ) iret = -3
C*
	RETURN
	END
