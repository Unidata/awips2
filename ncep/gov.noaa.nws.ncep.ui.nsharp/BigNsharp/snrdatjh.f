	SUBROUTINE SN_RDATJH  ( isnfln, nlev, data, ihhmm, iret )
C************************************************************************
C* SN_RDAT								*
C*									*
C* This subroutine reads data from a sounding data file.  The time	*
C* and station must be set before calling this subroutine. 		*
C*									*
C* SN_RDAT  ( ISNFLN, NLEV, DATA, IHHMM, IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	DATA (*)	REAL		Station data			*
C*	IHHMM		INTEGER		Station hour and minute		*
C*	IRET		INTEGER	 	Return code			*
C*				    	   1 = no data at station	*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -8 = location not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 1/89	Added level types			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	INTEGER		idthdr (LLSTHL)
	REAL		data (*)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check that station is set.
C
	isnlev ( isnfln ) = 0
	IF  ( ( krow ( isnfln ) .le. 0 ) .or.
     +        ( kcol ( isnfln ) .le. 0 ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Set observation time to be returned to missing.
C
	idthdr ( 1 ) = IMISSD
C
C*	Check whether data is already merged.
C
	IF  ( mrgtyp ( isnfln ) )  THEN
C
C*	    Read merged data directly from file.
C
	    CALL DM_RDTR  ( isnfln, krow ( isnfln ), kcol ( isnfln ), 
     +			    'SNDT', idthdr, data, nw, ier )
C
C*	    Check return code.  Change error to +1 if there was no data.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = +1
		nlev =  0
	      ELSE
C
C*		Compute number of levels.
C
		nlev = nw / kparm ( isnfln )
		DO  i = 1, nlev
		    mdtype ( i, isnfln ) = 1
		END DO
	    END IF
C*
	  ELSE
C
C*	    Merge data if it is not already merged.
C
	    CALL SN_MERGJH  ( isnfln, nlev, data, idthdr, 
     +			    mdtype (1, isnfln), iret )
	END IF
C
C*	Compute station time.
C
	IF  ( iret .eq. 0 )  THEN
	    ihhmm = idthdr (1)
	END IF
C
C*	Save number of levels.
C
	isnlev ( isnfln ) = nlev
C*
	RETURN
	END
