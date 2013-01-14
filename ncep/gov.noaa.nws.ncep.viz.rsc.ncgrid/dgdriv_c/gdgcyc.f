	SUBROUTINE GD_GCYC  ( gdfile, sep, ncyc, gdclst, iret )
C************************************************************************
C* GD_GCYC								*
C*									*
C* This subroutine returns all the cycles available for a particular	*
C* model (template) as a string of times separated by 'sep'.		*
C*									*
C* GD_GCYC  ( GDFILE, SEP, NCYC, GDCLST, IRET )				*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	SEP		CHAR*1		Output separator		*
C*									*
C* Output parameters:							*
C*	NCYC		INTEGER		Number of cycles		*
C*	GDCLST  	CHAR*		List of GEMPAK times		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 2/98						*
C* D.W.Plummer/NCEP	 8/98	Rewrote to improve algorithm		*
C* D.W.Plummer/NCEP	10/98	Bug fix -> LLMXGT in CALL GD_FLTM	*
C* T. Lee/GSC		 7/99	Added cycle to call seq of FL_MFLS	*
C* S. Jacobs/NCEP	 9/99	Changed call to FL_MDAT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdfile, gdclst, sep
C
	CHARACTER*20 	gdtlst (LLMXGT), tcycle
	LOGICAL		found
	CHARACTER       filnms(LLMXGT)*256, tmplt*64, gdtime*20
C
C-----------------------------------------------------------------------
	iret = 0
	ncyc = 0
	gdclst = ' '
C
C*      Get all filenames.
C
        tcycle = '*'
        CALL FL_MFLS ( gdfile, ' ', tcycle, LLMXGT, filnms, nfiles,
     +                 tmplt, iret )
        IF ( iret .ne. 0 .or. nfiles .eq. 0 )  RETURN
C
C*      Check template (tmplt).  If it does not exist or if it
C*      does not contain the string 'YYMMDDHH', open all the files to
C*      get all the times.  Otherwise, build all the GEMPAK time
C*	strings from the filenames directly.
C
        IF ( tmplt .ne. ' ' .and.
     +          INDEX ( tmplt, 'YYMMDDHH' ) .ne. 0 )  THEN
C
C*          Filenames are assumed to contain enough GEMPAK date
C*          information to build a list of cycles.
C
            ngdftm = 0
            DO  nf = 1, nfiles
C
                CALL FL_MDAT ( filnms(nf), tmplt, '000101/0000',
     +				gdtime, iret )
C
                ngdftm = ngdftm + 1
                gdtlst ( ngdftm ) = gdtime(:11)
C
            END DO
C
        ELSE
C
            CALL GD_FLTM ( filnms, nfiles, LLMXGT, ngdftm, gdtlst, iret)
C
        END IF
C
	DO  i = 1, ngdftm
	    found = .false.
	    IF ( INDEX( gdclst, gdtlst (i)(:11) ) .ne. 0 )
     +						found = .true.
	    IF ( .not. found )  THEN
		ncyc = ncyc + 1
		IF ( ncyc .eq. 1 )  THEN
		    gdclst = gdtlst (i)(:11)
		ELSE
		    CALL ST_LSTR( gdclst, lg, ier )
		    gdclst = gdclst(:lg) // sep(1:1) // gdtlst (i)(:11)
		END IF
	    END IF
	END DO
C*
	RETURN
	END
