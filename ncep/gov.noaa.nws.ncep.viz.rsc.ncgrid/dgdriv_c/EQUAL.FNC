C************************************************************************
C									*
C* Function statement for equality test.  When using function, it is	*
C* important to remember to include this file AFTER all declarations	*
C* parameter statements, and SAVE statements, but BEFORE any DATA 	*
C* statements (for IRIX compatibility). 				*
C* 									*
C**									*
C* Log:									*
C* T. Lee/SAIC		04/07						*
C* T. Lee/SAIC		02/08	Added polygon warnings			*
C************************************************************************
C
	CHARACTER*2	CX, CY, CZ
	LOGICAL		C1, C2, C3, C4, C5, C6, C7, CEQ, CNE
	LOGICAL		EQ, NE, GE, LT, LE, IRMISS
	LOGICAL		CFLAG, GFLAG, SFLAG, HFLAG
	C1 ( KA, KB, KC ) =  ( KA .eq. KB .and. KB .eq. KC )
	C2 ( KA, KB, KC ) =  ( KA .ne. KB .and. KB .eq. KC )
	C3 ( KA, KB, KC ) =  ( KA .eq. KB .and. KB .ne. KC )
	C4 ( KA, KB, KC ) =  ( ( KA .lt. KB .and. KB .lt. KC ) .or.
     +			     ( KA .gt. KB .and. KB .gt. KC ) )
	C5 ( KA, KB, KC ) =  ( KA .lt. KB .and. KB .gt. KC )
	C6 ( KA, KB, KC ) =  ( ( KA .gt. KB .and. KB .lt. KC ) .and.
     +			     ( KA .ge. KC ) )
	C7 ( KA, KB, KC ) =  ( ( KA .gt. KB .and. KB .lt. KC ) .and.
     +			     ( KA .lt. KC ) )
	EQ ( KA, KB ) = ( KA .eq. KB )	
	NE ( KA, KB ) = ( KA .ne. KB )	
	GE ( KA, KB ) = ( KA .ge. KB )	
	LE ( KA, KB ) = ( KA .le. KB )	
	LT ( KA, KB ) = ( KA .lt. KB )	
	CNE ( CX, CY ) = ( CX .ne. CY )	
	CEQ ( CX, CY, CZ ) = ( CX .eq. CY .and. CY .eq. CZ )
	CFLAG ( XX ) = ( XX .ge. WSCA  )
	GFLAG ( XX ) = ( XX .ge. WGALE )
	SFLAG ( XX ) = ( XX .ge. WSTRM )
	HFLAG ( XX ) = ( XX .ge. WHURR ) 
	IRMISS ( kkkk ) = ( ABS ( kkkk - IMISSD ) .le. RDIFFD ) 
