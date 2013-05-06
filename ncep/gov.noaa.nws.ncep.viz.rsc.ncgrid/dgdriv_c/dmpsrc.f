	SUBROUTINE DM_PSRC (iflno, nkeys, keynam, iloval, ihival, iret)
C************************************************************************
C* DM_PSRC								*
C*									*
C* This subroutine defines criteria for the primary search.  If		*
C* the result of this primary search is false for any location, no	*
C* conditional search will be made.					*
C*									*
C* DM_PSRC  ( IFLNO, NKEYS, KEYNAM, ILOVAL, IHIVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NKEYS		INTEGER		Number of keys used in search	*
C*	KEYNAM (NKEYS)	CHAR*4		Key names			*
C*	ILOVAL (NKEYS)	INTEGER		Low values			*
C*	IHIVAL (NKEYS)	INTEGER		High values			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-14 = invalid key name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	keynam (*)
	INTEGER		iloval (*), ihival (*)
	CHARACTER	type*4
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
        IF  ( dbread ) THEN
            iret = 0
            RETURN
        END IF
C
C*	Check that at least one key is to be used in search.
C
	IF  ( ( nkeys .lt. 1 ) .or. ( nkeys .gt. MMKEY ) ) THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Reset search pointers.
C
	srcflg (iflno) = .false.
	ksrow  (iflno) = 0
	kscol  (iflno) = 0
C
C*	Set number of row, column searches.
C
	ksnrow ( 0, iflno ) = 0
	ksncol ( 0, iflno ) = 0
C
C*	For each key name, determine type and save in common.
C
	DO  i = 1, nkeys
	    CALL DM_FKEY ( iflno, keynam (i), type, loc, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = ier
	      ELSE IF ( type .eq. 'ROW' ) THEN
		ksnrow ( 0, iflno ) = ksnrow ( 0, iflno ) + 1
		kslrow ( ksnrow (0,iflno), 0, iflno ) = loc
		ksrlov ( ksnrow (0,iflno), 0, iflno ) = iloval (i)
		ksrhiv ( ksnrow (0,iflno), 0, iflno ) = ihival (i)
	      ELSE IF ( type .eq. 'COL' ) THEN
		ksncol ( 0, iflno ) = ksncol ( 0, iflno ) + 1
		kslcol ( ksncol (0,iflno), 0, iflno ) = loc
		ksclov ( ksncol (0,iflno), 0, iflno ) = iloval (i)
		kschiv ( ksncol (0,iflno), 0, iflno ) = ihival (i)
	    END IF
	END DO
C
C*	If all keys were found, set search flag.
C
	IF  ( iret .eq. 0 ) THEN
	    srcflg (iflno) = .true.
	  ELSE
	    ksnrow ( 0, iflno ) = 0
	    ksncol ( 0, iflno ) = 0
	END IF
C*
	RETURN
	END
