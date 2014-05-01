	SUBROUTINE ER_LMSG  ( leverr, errgrp, numerr, errstr, iret )
C************************************************************************
C* ER_LMSG								*
C*									*
C* This subroutine writes error messages whose error level is less	*
C* than or equal to the requested error level.				*
C*									*
C* The output message contains the error group and error number 	*
C* in brackets followed by the message.  If the error file or error	*
C* number cannot be found, only the error group and number are		*
C* written.								*
C*									*
C* The string, ERRSTR, will replace an !AS found in the message.	*
C*									*
C* The messages are stored in error files.  The message is read		*
C* from the file GEMERR:'ERRGRP'.ERR.					*
C*									*
C* ER_LMSG  ( LEVERR, ERRGRP, NUMERR, ERRSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	LEVERR		INTEGER		Error level			*
C*					  0 = always log		*
C*					  2 = Detailed messages		*
C*					  4 = Debug messages		*
C*	ERRGRP		CHAR*		Error group			*
C*	NUMERR		INTEGER		Error number			*
C*	ERRSTR		CHAR*		String to be embedded		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Tyle/GSC		12/96	Based on ER_WMSG			*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C* T. Piper/SAIC	02/04	Calling sequence change for er_smsg	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ercmn.cmn'
C*
	CHARACTER*(*)  	errgrp, errstr 
	CHARACTER*128	outmsg
C------------------------------------------------------------------------
C*      Return if error number is 0 or level of error exceeds IELEVL.
C
        iret = 0
        IF ( numerr .eq. 0 )  RETURN
	IF ( leverr .gt. ielevl ) RETURN
	IF ( iebuff .eq. -1 ) RETURN
C
C*	Create the error message.
C
	CALL ER_MMSG ( errgrp, numerr, errstr, etmflg, outmsg, iret )
	IF ( iebuff .eq. 1 ) THEN
cc	print*, ' In er_lmsg ---> ', outmsg
C
C*	    Write the error message to the buffer.
C
	    CALL ST_NULL ( outmsg, outmsg, lens, ier )
	    CALL ER_SMSG ( outmsg, ier )
	  ELSE
C
C*	    Write the error message to the terminal.
C
	    CALL ST_LSTR  ( outmsg, lens, ier )
	    WRITE  ( 6, 1001 )  outmsg ( : lens )
1001        FORMAT ( 1X, A )
	END IF
C*
	RETURN
	END
