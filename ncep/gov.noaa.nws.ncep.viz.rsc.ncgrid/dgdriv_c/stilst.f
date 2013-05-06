	SUBROUTINE ST_ILST ( string, sep, idef, nexp, iarr, num, iret )
C************************************************************************
C* ST_ILST								*
C*									*
C* This subroutine breaks a string containing a list of integers into	*
C* an array of integers.  The separator for the integers is input as 	*
C* SEP.  If the separator is a blank, multiple blanks will be changed	*
C* to single blanks before the string is processed.  If null strings	*
C* are encountered or fewer than NEXP strings are found in the		*
C* string, the appropriate IARR locations are set to IDEF.		*
C*									*
C* ST_ILST  ( STRING, SEP, IDEF, NEXP, IARR, NUM, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*	SEP		CHAR*1		Separator			*
C*	IDEF		INTEGER		Default value			*
C*	NEXP		INTEGER		Number of expected values	*
C*									*
C* Output parameters:							*
C*	IARR  (NUM)	INTEGER		Array of integer values		*
C*	NUM		INTEGER		Number of values returned	*
C*	IRET		INTEGER		Return code			*
C*				   	  1 = more than NEXP values 	*
C*				   	  0 = normal return		*
C*				  	 -3 = invalid substring		*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84	Original source for STLIST		*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 2/85	Modified for ST_ILST			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf                *
C************************************************************************
	CHARACTER*(*)	string, sep
	INTEGER		iarr (*)
C*
	CHARACTER	strbuf*160, cchar*1
C------------------------------------------------------------------------
	iret = 0
	num  = 0
cccc	print*, ' String -----> ', string
C
C*	Remove blanks from the input string if the separator is not
C*	a blank.
C
	IF  ( sep .ne. ' ' )  THEN
	    CALL ST_RMBL  ( string, strbuf, isize, iret )
	  ELSE
	    CALL ST_RXBL  ( string, strbuf, isize, iret )
	END IF
C
C*	Initialize output array.
C
	DO  i = 1, nexp
	    iarr (i) = idef
	END DO
C
C*	Check for zero length input string.
C
ccc	print*, '  isize = ', isize

	IF  ( isize .eq. 0 )  THEN
	    num = 0
C
C*	    Check for separator and find list elements.
C
	  ELSE
	    cchar  = sep
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX ( strbuf (ibegin:), cchar )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize + 1
	          ELSE
		    iend = ibegin + loc - 1
	        END IF
C
C*	        Move list element into output list.  Check that num <= nexp.
C
	        IF  ( num .ge. nexp )  THEN
		    iret = 1
		  ELSE
		    num = num + 1
	    	    IF  ( ibegin .ne. iend )  THEN
			CALL ST_NUMB  ( strbuf (ibegin : iend-1), ival, 
     +					ier )
			IF  ( ier .eq. 0 )  THEN
			    iarr (num) = ival
			  ELSE
			    iret = -3
			END IF
	    	    END IF
		END IF
C*
	    ibegin = iend + 1
	    END DO
ccc	print*, ' IARR =======> ', String, num, iarr(1), iarr(2)
	END IF
cccc	print*, ' *********************************************************** '
C*
	RETURN
	END
