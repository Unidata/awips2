	SUBROUTINE GET_GEM_TIMES( snfile, time_list, ntimf, iret )
C
C* 10/99 cleaned up and simplified -mkay
C

	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile
	CHARACTER	time_list(500)*16
	CHARACTER	pmdset (MMPARM)*4
	LOGICAL		mrgflg

	ntimf  = 0

C
C*	Open the input file.
C
        CALL SN_OPNF( snfile, .false., isnfln, isrc, npmdst, pmdset,
     +                ivert, mrgflg, ier )

	IF ( ier .eq. 0 ) THEN
C
C*	   Get input times and pointers.
C
	   CALL SN_GTIM( isnfln, LLMXTM, ntimf, time_list, ier )
C
	   CALL SN_CLOS( isnfln, iret )
	END IF
C*
	END
