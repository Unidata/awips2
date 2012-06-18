	SUBROUTINE DM_RFHR  ( iflno, fhdnam, mxword, rheadr, nword, 
     +			      iret )
C************************************************************************
C* DM_RFHR								*
C*									*
C* This subroutine reads a real file header from a DM file.  The 	*
C* length of the file header must be less than MXWORD.			*
C*									*
C* DM_RFHR  ( IFLNO, FHDNAM, MXWORD, RHEADR, NWORD, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		File header name		*
C*	MXWORD		INTEGER		Maximum words to return		*
C*									*
C* Output parameters:							*
C*	RHEADR (NWORD)	REAL		File header 			*
C*	NWORD		INTEGER		Header length			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					 -8 = file header undefined	*
C*					-18 = file header too long	*
C*					-21 = incorrect data type	*
C*					-29 = invalid file hdr name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	fhdnam
	REAL		rheadr (*)
C------------------------------------------------------------------------
	nword = 0

        IF ( dbread ) THEN
           IF ( INDEX(dbdatasrc,'grid') .gt. 0 ) THEN
              CALL DM_GETGNAV ( iflno, fhdnam, mxword, rheadr, nword, 
     +                          iret )
              RETURN
	   END IF
	END IF
C
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that this is a valid file header name.
C
	knt = 0
	DO  i = 1, kfhdrs ( iflno )
	    IF  ( kfhnam ( i, iflno ) .eq. fhdnam )  knt = i
	END DO
C
C*	Check for invalid name.
C
	IF  ( knt .eq. 0 )  THEN
	    iret = -29
C
C*	    Check for valid data type.
C
	  ELSE IF  ( kfhtyp ( knt, iflno ) .ne. MDREAL )  THEN
	    iret = -21
	END IF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Compute header location.
C
	iread = kpfile ( iflno ) + 3 * kfhdrs ( iflno )
	DO  i = 1, knt - 1
	    iread = iread + kfhlen ( i, iflno ) + 1
	END DO
C
C*	Read actual length and return error if too long.
C
	CALL DM_RINT  ( iflno, iread, 1, nword, iret )
	IF  ( iret .ne. 0 )  RETURN
	IF  ( nword .gt. mxword )  THEN
	    iret  = -18
	    nword = 0
	  ELSE IF  ( nword .le. 0 )  THEN
	    iret = -8
	  ELSE
C
C*	    Read in header.  If this is the grid navigation block,
C*	    do not convert projection name which is in word 2.
C
	    IF  ( ( fhdnam .eq. 'NAVB' ) .and.
     +		  ( kmachn ( iflno ) .ne. MTMACH ) )  THEN
		iread = iread + 1
		CALL DM_RFLT  ( iflno, iread, 1, rheadr, iret )
		IF  ( iret .ne. 0 )  RETURN
		machin = kmachn ( iflno )
		kmachn ( iflno ) = MTMACH
		iread = iread + 1
		CALL DM_RFLT  ( iflno, iread, 1, rheadr (2), iret )
		IF  ( iret .ne. 0 )  RETURN
		kmachn ( iflno ) = machin
		iread = iread + 1
		nword = nword - 2
		CALL DM_RFLT  ( iflno, iread, nword, rheadr (3), iret )
	      ELSE
		iread = iread + 1
		CALL DM_RFLT  ( iflno, iread, nword, rheadr, iret )
	    END IF
	END IF
C*
	RETURN
	END
