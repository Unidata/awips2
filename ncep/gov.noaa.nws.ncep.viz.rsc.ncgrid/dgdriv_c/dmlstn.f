	SUBROUTINE DM_LSTN  ( iflno, sttype, ilstid, ilstnm, ilslat,
     +			      ilslon, ilselv, ilstat, ilcoun,
     +			      ilstd2, iret )
C************************************************************************
C* DM_LSTN								*
C*									*
C* This subroutine finds the location of SN station keywords.  Both	*
C* SLAT and SLON must be row or column keys.  The locations of the	*
C* keywords STID, STNM, SELV, STAT, COUN, and STD2 are also checked.	*
C* If present, they must be the same type as the SLAT and SLON keys.  	*
C* If a key is not found, the location is set to 0.			*
C*									*
C* DM_LSTN  ( IFLNO,  STTYPE, ILSTID, ILSTNM, ILSLAT, ILSLON, 		*
C*            ILSELV, ILSTAT, ILCOUN, ILSTD2, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	STTYPE		CHAR*		Location type:  ROW or COL	*
C*	ILSTID		INTEGER		Location of STID		*
C*	ILSTNM		INTEGER		Location of STNM		*
C*	ILSLAT		INTEGER		Location of SLAT		*
C*	ILSLON		INTEGER		Location of SLON		*
C*	ILSELV		INTEGER		Location of SELV		*
C*	ILSTAT		INTEGER		Location of STAT		*
C*	ILCOUN		INTEGER		Location of COUN		*
C*	ILSTD2		INTEGER		Location of extended stn ID	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-25 = invalid station keywords	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	sttype
	CHARACTER*4	tstid, tstnm, tslat, tslon, tselv, tstat, tcoun
	CHARACTER*4	tstd2
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize variables.
C
	iret   = 0
	sttype = ' '
	ilstid = 0
	ilstnm = 0
	ilslat = 0
	ilslon = 0
	ilselv = 0
	ilstat = 0
	ilcoun = 0
	ilstd2 = 0
        IF ( dbread ) THEN
           iret = 0
           sttype = 'COL'
           ilstid = 1
           ilstnm = 2
           ilslat = 3
           ilslon = 4
           ilselv = 5
           ilstat = 6
           ilcoun = 7
           ilstd2 = 8
           RETURN
        END IF

C
C*	Check for keywords.
C
	CALL DM_FKEY  ( iflno, 'STID', tstid, lstid, irstid )
	CALL DM_FKEY  ( iflno, 'STNM', tstnm, lstnm, irstnm )
	CALL DM_FKEY  ( iflno, 'SLAT', tslat, lslat, irslat )
	CALL DM_FKEY  ( iflno, 'SLON', tslon, lslon, irslon )
	CALL DM_FKEY  ( iflno, 'SELV', tselv, lselv, irselv )
	CALL DM_FKEY  ( iflno, 'STAT', tstat, lstat, irstat )
	CALL DM_FKEY  ( iflno, 'COUN', tcoun, lcoun, ircoun )
	CALL DM_FKEY  ( iflno, 'STD2', tstd2, lstd2, irstd2 )
C
C*	Return error message if either latitude or longitude is missing.
C
	IF  ( ( irslat .ne. 0 )  .or.  ( irslon .ne. 0 )  .or.
     +	      ( tslat .ne. tslon ) )  THEN
	    iret = -25
	  ELSE IF  ( ( irstid .eq. 0 ) .and. ( tstid .ne. tslat ) ) THEN
	    iret = -25
	  ELSE IF  ( ( irstnm .eq. 0 ) .and. ( tstnm .ne. tslat ) ) THEN
	    iret = -25
	  ELSE IF  ( ( irselv .eq. 0 ) .and. ( tselv .ne. tslat ) ) THEN
	    iret = -25
	  ELSE IF  ( ( irstat .eq. 0 ) .and. ( tstat .ne. tslat ) ) THEN
	    iret = -25
	  ELSE IF  ( ( ircoun .eq. 0 ) .and. ( tcoun .ne. tslat ) ) THEN
	    iret = -25
	  ELSE IF  ( ( irstd2 .eq. 0 ) .and. ( tstd2 .ne. tslat ) ) THEN
	    iret = -25
	  ELSE
	    ilstid = lstid
	    ilstnm = lstnm
	    ilslat = lslat
	    ilslon = lslon
	    ilselv = lselv
	    ilstat = lstat
	    ilcoun = lcoun
	    ilstd2 = lstd2
	    sttype = tslat
	END IF
C*
	RETURN
	END
