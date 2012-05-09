	SUBROUTINE ER_MMSG  ( errgrp, numerr, errstr, timflg, outmsg,
     +			      iret )
C************************************************************************
C* ER_MMSG								*
C*									*
C* This subroutine creates an error message.  The output message 	*
C* contains the error group and error number in brackets followed by	*
C* the message.  If the error file or error number cannot be found,	*
C* only the error group and number will be written to the output	*
C* string.								*
C*									*
C* The string, ERRSTR, replaces an !AS found in the message.		*
C*									*
C* If TIMFLG is true, the local system time is prepended to the		*
C* error message.							*
C*									*
C* The messages are stored in error files.  The message is read		*
C* from the file GEMERR:'ERRGRP'.ERR.					*
C*									*
C* ER_MMSG  ( ERRGRP, NUMERR, ERRSTR, TIMFLG, OUTMSG, IRET )		*
C*									*
C* Input parameters:							*
C*	ERRGRP		CHAR*		Error group			*
C*	NUMERR		INTEGER		Error number			*
C*	ERRSTR		CHAR*		String to be embedded		*
C*	TIMFLG		LOGICAL		Flag to prepend system time	*
C*									*
C* Output parameters:							*
C*	OUTMSG		CHAR*		Output message			*
C*	IRET		INTEGER		Return code			*
C*					  3 = error number not found 	*
C*					  2 = error file not found	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 8/84	Use standard FORTRAN			*
C* I. Graffman/RDS	 3/88	Corrected length in building file name	*
C* M. desJardins/GSFC	 3/88	Documentation				*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* S. Schotz/GSC	 7/90	Use global GEMERR for error dir		*
C* A. Chang/EAI	 	 1/94	Remove WRITE and rename to ER_MMSG	*
C* M. desJardins/NMC	 8/94	Add file close				*
C* S. Jacobs/NMC	 8/95	Allow use of local error files		*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* K. Tyle/GSC		12/96	Added TIMFLG and call to SS_LTIM	*
C* S. Jacobs/NCEP	 2/97	Replaced ST_C2I with ST_ILST		*
C* S. Maxwell/GSC	 6/97	Documentation changes        		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* J. Wu/GSC             7/00   Changed ERRSTR insertion logic          *
C* B. Yin/SAIC		 3/04	Changed SS_LTIM to CSS_GTIM		*
C* B. Yin/SAIC		 4/04	Changed string dattim to dattim(1:11)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	errgrp, errstr, outmsg
C*
	CHARACTER*128 	text, errfil, ooo, tmpstr
	CHARACTER	errtyp*4, dattim*12
	LOGICAL		logfil, done, timflg
	INTEGER		itype
C*
	CHARACTER	CRLF*2
	PARAMETER	( CRLF = CHCR // CHLF )
C*
	DATA		errtyp /'.ERR'/
C------------------------------------------------------------------------
C*	First add the error group and error number in square brackets.
C
	outmsg = '[' // errgrp
	CALL ST_LSTR  ( outmsg, lent , ier )
	CALL ST_LCUC  ( outmsg, outmsg, ier )
	IF  ( lent .eq. 1 )  THEN
	    logfil = .false.
	    outmsg = '[GEMPAK'
	    lent = 7
	  ELSE
	    errfil = '$GEMERR/' // errgrp ( : lent-1 ) // errtyp
	    CALL FL_TBOP ( errfil, ' ', lun, ier )
	    logfil =  ier .eq. 0
	END IF
C
C*	Now encode the error number in the output string.
C
	outmsg ( lent+1 : lent+1 ) = ' '
	CALL ST_INCH  ( numerr, outmsg ( lent+2 : ), ier )
	CALL ST_LSTR  ( outmsg, lent, ier )
	outmsg ( lent+1 : lent+2 ) = '] '
	lent = lent + 2
C
C*	Open the error file and search for the correct error number.
C
	IF  ( .not. logfil )  THEN
	    iret = 2
	  ELSE
	    done = .false.
C
C*	    Read through the file searching for the error message.
C
	    DO WHILE  ( .not. done )
		READ   ( lun, 1000, iostat=ier ) text
1000		FORMAT ( A )
		IF  ( ier .ne. 0 )  THEN
		    iret = 3
		    done = .true.
		  ELSE
		    CALL ST_ILST  ( text, '!', 0, 1, num, nnum, ierr )
		    IF (( nnum .eq. 1 ) .and. ( num .eq. numerr ))  THEN
			iret = 0
			ipos = INDEX ( text, '!' )
			IF  ( ipos .gt. 0 )  THEN
			    text = text ( ipos+1: )
			  ELSE
			    text = ' '
			ENDIF
			done = .true.
		    END IF
		END IF
	    END DO
	    CALL FL_CLOS  ( lun, ier )
	END IF
C
C*	Add the text to the OUTPUT.
C
	IF ( iret .ne. 0 ) text = ' '
	outmsg ( lent+1: ) = text
C
C*	Insert ERRSTR into the OUTPUT string if requested.
C
	IF  ( iret .eq. 0 )  THEN
	    ipos = INDEX  ( outmsg, '!AS' )
	    IF  ( ipos .ne. 0 )  THEN
		CALL ST_LSTR  ( errstr, lens, ier )
		ooo    = outmsg
		IF ( lens .gt. 0 ) THEN
 		    outmsg = ooo ( : ipos-1 ) // errstr ( : lens ) //
     +		             ooo ( ipos+3 : )
                  ELSE
 		    outmsg = ooo ( : ipos-1 ) // '...' //
     +		             ooo ( ipos+3 : )		    
     		ENDIF
	    END IF
	    ipos = 1
	    DO WHILE  ( ipos .ne. 0 )
		ipos = INDEX ( outmsg, '!/' )
		IF  ( ipos .ne. 0 )  THEN
		    outmsg ( ipos:ipos+1 ) = CRLF
		END IF
	    END DO
	  ELSE
	    outmsg ( lent+1: ) = errstr
	END IF
C
C*	Check if local system time is to be prepended.
C
	itype = 0
	IF ( timflg ) THEN
	    CALL CSS_GTIM ( itype, dattim, ier )
	    tmpstr = dattim(1:11) // outmsg
	  ELSE
	    tmpstr = outmsg
	END IF
	outmsg = tmpstr
C*
	RETURN
	END
