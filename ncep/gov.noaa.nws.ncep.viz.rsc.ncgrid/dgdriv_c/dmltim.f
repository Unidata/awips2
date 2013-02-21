	SUBROUTINE DM_LTIM  ( iflno, dttype, ildate, iltime, iret )
C************************************************************************
C* DM_LTIM								*
C*									*
C* This subroutine finds the location of the DATE and TIME keywords	*
C* in a DM file.  Both keys must be row keys or column keys.		*
C*									*
C* DM_LTIM  ( IFLNO, DTTYPE, ILDATE, ILTIME, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	DTTYPE		CHAR*		Location type:  ROW or COL	*
C*	ILDATE		INTEGER		Location of DATE		*
C*	ILTIME		INTEGER		Location of TIME		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-27 = invalid time keywords	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	dttype
	CHARACTER	tdate*4, ttime*4
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
        IF ( dbread ) THEN  
            dttype = 'ROW'
            ildate = 1
            iltime = 2
            iret   = 0
            RETURN
        END IF
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize variables.
C
	iret   = 0
	dttype = ' '
	ildate = 0
	iltime = 0
C
C*	Check for keywords.
C
	CALL DM_FKEY  ( iflno, 'DATE', tdate, locd, iretd )
	CALL DM_FKEY  ( iflno, 'TIME', ttime, loct, irett )
C
C*	Return error message if either date or time is missing.
C
	IF  ( ( iretd .ne. 0 )  .or.  ( irett .ne. 0 )  .or.
     +	      ( tdate .ne. ttime ) )  THEN
	    iret = -27
	  ELSE
	    ildate = locd
	    iltime = loct
	    dttype = tdate
	END IF
C*
	RETURN
	END
