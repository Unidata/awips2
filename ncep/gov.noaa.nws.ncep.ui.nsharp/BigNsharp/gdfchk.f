	SUBROUTINE GD_FCHK ( iacss, igdfln, iret )
C************************************************************************
C* GD_FCHK								*
C*									*
C* This subroutine checks that the input grid file access # is valid.	*
C*									*
C* GD_FCHK  ( IACSS, IGDFLN, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS		INTEGER		File access number to check	*
C*									*
C* Output parameters:							*
C*	IGDFLN		INTEGER		Grid file number for DM		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open 		*
C*					-16 = invalid/stale access #	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* K. Brill/HPC		12/03  Check nucode flg; check access number;	*
C*			       return the DM file number		*
C* R. Tian/SAIC		 3/04  Changed file access number translation	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'grdcmn.cmn'
C------------------------------------------------------------------------
	iret = 0
C
C*	Translate file access number to DM file number
C

	IF ( iacss .le. 0 ) THEN
	    iret = -4
	    RETURN
	  ELSE IF ( iacss .gt. 0 .and. iacss .le. MMFILE ) THEN
	    igdfln = iacss
	  ELSE
	    igdfln = 0
	    DO i = 1, MMFILE
		IF ( iacss .eq. iflacc (i) ) THEN
		    igdfln = i
		END IF
	    END DO
	    IF ( igdfln .eq. 0 ) THEN
		iret = -16
		CALL ER_WMSG ( 'GD', iret, ' ', ier )
		RETURN
	    END IF
	END IF
C
C*	Check that file number is in proper range and then see that
C*	a file has been opened using that file number, i.e. the file
C*	number for the file is non-negative.
C
	IF  ( ( igdfln .lt. 1) .or. ( igdfln .gt. MMFILE ) ) THEN
	    iret = -4
	ELSE
	    IF  ( igrdfn (igdfln) .lt. 0 ) THEN
	        iret = -4
	        print *, 'gdfchk.f: igrdfn (',igdfln,') = ',igrdfn(igdfln)
	    END IF
	END IF
C*
	RETURN
	END
