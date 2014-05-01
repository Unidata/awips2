	SUBROUTINE ER_WMSG  ( errgrp, numerr, errstr, iret )
C************************************************************************
C* ER_WMSG								*
C*									*
C* This subroutine writes an error message to the user's terminal.	*
C* The output message contains the error group and error number 	*
C* in brackets followed by the message.  If the error file or error	*
C* number cannot be found, only the error group and number will be	*
C* written.								*
C*									*
C* The string, ERRSTR, replaces an !AS found in the message.		*
C*									*
C* The messages are stored in error files.  The message is read		*
C* from the file GEMERR:'ERRGRP'.ERR.					*
C*									*
C* ER_WMSG  ( ERRGRP, NUMERR, ERRSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	ERRGRP		CHAR*		Error group			*
C*	NUMERR		INTEGER		Error number			*
C*	ERRSTR		CHAR*		String to be embedded		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = error number not found 	*
C*					  2 = error file not found	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 8/84	Cleaned up			 	*
C* I. Graffman/RDS	 3/88	Corrected length in building file name	*
C* M. desJardins/GSFC	 3/88	Documentation				*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* S. Schotz/GSC	 7/90	Use global GEMERR for error dir		*
C* A. Chang/EAI	 	 1/94	Clean up and call ER_MMSG instead	*
C* M. desJardins/NMC	 8/94	Add check for iestat			*
C* K. Tyle/GSC		12/96	Call ER_LMSG				*
C* K. Maxwell/GSC	 6/97	Documentation changes			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)  	errgrp, errstr 
C------------------------------------------------------------------------
C*      Call ER_LMSG with error reporting level = 0.
C
C	print*, " in er_wmsg ----> ", errgrp, numerr
	CALL ER_LMSG ( 0, errgrp, numerr, errstr, iret )
C*
	RETURN
	END
