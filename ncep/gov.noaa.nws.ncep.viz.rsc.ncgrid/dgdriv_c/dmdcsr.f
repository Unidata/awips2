	SUBROUTINE DM_DCSR  ( iflno, iret )
C************************************************************************
C* DM_DCSR								*
C*									*
C* This subroutine deletes the conditional searches for a DM file.	*
C*									*
C* DM_DCSR  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* K. Brill/NMC		12/91	Set SRCFLG to false. Zero KSROW, KSCOL  *
C* K. Brill/NMC		01/92	Should NOT set SRCFLG to false		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN

C
C*      Initial for A2DB
C       
        IF ( iflno .ge. 90 ) THEN
            stnindx = 0
            ntotstn = 0
            firstdb = .true.
            RETURN
        END IF
C
C*	Set the number of searches to 0.
C
	nsrch  ( iflno ) = 0
	DO  i = 1, MMSRCH
	    ksnrow ( i, iflno ) = 0
	    ksncol ( i, iflno ) = 0
	END DO
C
C*	Zero KSROW, KSCOL.
C
	ksrow  ( iflno ) = 0
	kscol  ( iflno ) = 0
C*
	RETURN
	END
