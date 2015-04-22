	SUBROUTINE SN_MERGJH  ( isnfln, nlev, stndat, idthdr, idtype, 
     +			      iret )
C************************************************************************
C* SN_MERG								*
C*									*
C* This subroutine merges upper air data when the parts are stored	*
C* separately.								*
C*									*
C* SN_MERG  ( ISNFLN, NLEV, STNDAT, IDTHDR, IDTYPE, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of merged levels		*
C*	STNDAT (6,NLEV)	REAL		Merged data			*
C*	IDTHDR (*)	INTEGER		Data header			*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-16 = too many levels		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* M. desJardins/GSFC	 8/90	Change array sizes			*
C* S. Jacobs/NMC	 3/95	Removed mflag from check for TTCC part	*
C* S. Jacobs/NMC	 4/95	Added check for manflg before MR_UADT	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* D. Kidwell/NCEP	 2/01	Added parts PPAA & PPCC, trop and mx wnd*
C* D. Kidwell/NCEP	 3/01	Changed MR_UADT call sequence           *
C* T. Piper/SAIC	 1/02	Initialized rsgtdt & rtrpdt, 		*
C*							not always set	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	PARAMETER	( MXLEVL = 3 * LLMXLV )
C*	
	REAL		stndat (*)
	INTEGER		idthdr (*), idtype (*)
C*
	REAL		rmandt ( 6, 30 ),     ramndt ( 6, 30 ), 
     +			rsgtdt ( 3, LLMXLV ), rsgwdt ( 3, LLMXLV ),
     +			rastdt ( 3, LLMXLV ), raswdt ( 3, LLMXLV ),
     +			rmnwdt ( 3, 30 ),     ramwdt ( 3, 30 ),
     +			rtrpdt ( 5, 30 ),     ratrdt ( 5, 30 ),
     +			rmxwdt ( 3, 30 ),     ramxdt ( 3, 30 )
	LOGICAL		ta, tb, pb, tc, td, pd, pa, pc, tflag, mflag
C*
	DATA		rsgtdt, rtrpdt/MXLEVL*RMISSD, 150*RMISSD/
C-----------------------------------------------------------------------
	iret  = 0
	nlev  = 0
	tflag = .false.
C
C*	Set flags indicating which data to read.
C
	mflag = .not. manflg ( isnfln )
	ta    = taflg  ( isnfln )
	tb    = tbflg  ( isnfln ) .and. mflag
	pb    = pbflg  ( isnfln ) .and. mflag
	tc    = tcflg  ( isnfln )
	td    = tdflg  ( isnfln ) .and. mflag
	pd    = pdflg  ( isnfln ) .and. mflag
	pa    = paflg  ( isnfln )
	pc    = pcflg  ( isnfln )
C
C*	Initialize number of levels.
C
	nman = 0
	nsgt = 0
	nsgw = 0
	namn = 0
	nast = 0
	nasw = 0
	nmnw = 0
	namw = 0
	ntrp = 0
	natr = 0
	nmxw = 0
	namx = 0
C
C*	Get row and column.
C
	irow = krow ( isnfln )
	icol = kcol ( isnfln )
C
C*	Read in mandatory data.
C
	IF  ( ta )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'TTAA', idthdr, 
     +			    rmandt, nw,   ier )
	    IF  ( ier .eq. 0 )  THEN
		itime = idthdr (1)
		tflag = .true.
		nman  = nw / 6
	        CALL DM_RDTR  ( isnfln, irow, icol, 'TRPA', idthdr, 
     +			        rtrpdt, nw,   ier )
		IF ( ( ier .eq. 0 ) .and. ( itime .eq. idthdr (1) ) )
     +		       ntrp = nw / 5
	        CALL DM_RDTR  ( isnfln, irow, icol, 'MXWA', idthdr, 
     +			        rmxwdt, nw,   ier )
		IF ( ( ier .eq. 0 ) .and. ( itime .eq. idthdr (1) ) )
     +		       nmxw = nw / 3
	    END IF
	END IF
C
C*	Read in significant temperature data.
C
	IF  ( tb )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'TTBB', idthdr, 
     +			    rsgtdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		nsgt  = nw / 3
		IF  ( nsgt .gt. LLMXLV )  THEN
		    iret = -16
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Read in significant wind data.
C
	IF  ( pb )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'PPBB', idthdr, 
     +			    rsgwdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		nsgw  = nw / 3
		IF  ( nsgw .gt. LLMXLV )  THEN
		    iret = -16
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Read in above mandatory data.
C
	IF  ( tc )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'TTCC', idthdr, 
     +			    ramndt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		itime = idthdr (1)
		tflag = .true.
		namn  = nw / 6
	        CALL DM_RDTR  ( isnfln, irow, icol, 'TRPC', idthdr, 
     +			        ratrdt, nw,   ier )
		IF ( ( ier .eq. 0 ) .and. ( itime .eq. idthdr (1) ) )
     +		       natr = nw / 5
	        CALL DM_RDTR  ( isnfln, irow, icol, 'MXWC', idthdr, 
     +			        ramxdt, nw,   ier )
		IF ( ( ier .eq. 0 ) .and. ( itime .eq. idthdr (1) ) )
     +		       namx = nw / 3
	    END IF
	END IF
C
C*	Read in above significant temperature data.
C
	IF  ( td )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'TTDD', idthdr, 
     +			    rastdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		nast  = nw / 3
		IF  ( nast .gt. LLMXLV )  THEN
		    iret = -16
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Read in above significant wind data.
C
	IF  ( pd )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'PPDD', idthdr, 
     +			    raswdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		nasw  = nw / 3
		IF  ( nasw .gt. LLMXLV )  THEN
		    iret = -16
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Read in mandatory wind data.
C
	IF  ( pa )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'PPAA', idthdr, 
     +			    rmnwdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		itime = idthdr (1)
		tflag = .true.
		nmnw  = nw / 3
	    END IF
	END IF
C
C*	Read in above mandatory wind data.
C
	IF  ( pc )  THEN
	    CALL DM_RDTR  ( isnfln, irow, icol, 'PPCC', idthdr, 
     +			    ramwdt, nw,   ier )
	    IF  ( ( ier .eq. 0 )  .and.  ( ( .not. tflag ) .or.
     +				( itime .eq. idthdr (1) ) ) )  THEN
		namw  = nw / 3
	    END IF
	END IF
C
C*	Merge the data that is left.
C
	iztype = imrtyp ( isnfln )
	selv   = shght  ( isnfln )
	IF ( tflag ) idthdr ( 1 ) = itime
C
C*	Return if there is no data.
C
	IF  ( ( nman .le. 0 ) .and. ( nsgt .le. 0 ) .and. 
     +	      ( nsgw .le. 0 ) .and. ( namn .le. 0 ) .and.
     +	      ( nast .le. 0 ) .and. ( nasw .le. 0 ) .and.
     +	      ( nmnw .le. 0 ) .and. ( namw .le. 0 ) .and.
     +	      ( ntrp .le. 0 ) .and. ( nmxw .le. 0 ) .and.
     +        ( natr .le. 0 ) .and. ( namx .le. 0 ) )  THEN
	    iret = + 1
	    RETURN
	END IF
C 
	IF  ( manflg ( isnfln ) )  THEN
C
C*	    Set the data to be only the mandatory level data as
C*	    read from the data file.
C
	    CALL SN_MMAN ( .false., 1, rmnwdt, nmnw, rmandt, nman,
     +		           stndat, nlev, iret )
	    jlev = nlev + 1
	    CALL SN_MMAN ( .true., jlev, ramwdt, namw, ramndt, namn,
     +		           stndat, nlev, iret )
	  ELSE
C
C*	    Merge all the available data.
C
	    CALL MR_UADTJH  ( rmandt, nman, rsgtdt, nsgt, rsgwdt, nsgw,
     +			    ramndt, namn, rastdt, nast, raswdt, nasw,
     +			    rmnwdt, nmnw, ramwdt, namw, rtrpdt, ntrp,
     +			    rmxwdt, nmxw, ratrdt, natr, ramxdt, namx,
     +			    selv, iztype, stndat, nlev, idtype, iret )
	END IF
C
C*	Check that there is data.
C
	IF  ( nlev .eq. 0 )  THEN
	    iret = +1
	END IF
C*
	RETURN
	END
