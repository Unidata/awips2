	SUBROUTINE	get_file_alias (alias, path, tmpl, iret)
C************************************************************************
C* GET_FILE_ALIAS                                                       *
C*                                                                      *
C* This subroutine returns the alias path and template.			*
C*                                                                      *
C* GET_FILE_ALIAS ( ALIAS, PATH, TMPL, IRET)				*
C* Input parameters:                                                    *
C*	ALIAS		CHAR*		Alias name			*
C*									*
C* Output parameters:                                                   *
C*	PATH		CHAR*		Alias path			*
C*	TMPL		CHAR*		Alias template			*
C*      IRET            INTEGER		Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* ???			?????	???					*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'

	CHARACTER*(*)	alias, path, tmpl

	CHARACTER*(255)	fpath, newpath, ftmpl, fnull*(MXFLSZ)
	LOGICAL		tplate

	iret = 0

	tplate = .true.
	CALL ST_NULL ( alias, fnull, nf, ier )
	fpath = ' '
	ftmpl = ' '
	CALL CTB_DTGET ( fnull, fpath, ftmpl, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	IF ( ier .ne. 0 )  tplate = .false. 
        CALL ST_RNUL ( fpath, fpath, lens, ier )
        CALL ST_RNUL ( ftmpl, ftmpl, lens, ier )
	IF ( .not. tplate ) THEN
	   iret = -1
	ELSE

	   CALL SS_ENVR ( fpath, newpath, ier)
	   IF (ier .eq. 0 ) fpath = newpath
	   CALL ST_NULL ( fpath, path, lens, ier)

C* Within templates, the following character combinations are reserved: *
C* comb - meaning               'metacharacters'           example(s)   *
C* YYYY - 4-digit year          '[0-9][0-9][0-9][0-9]'     1999,2000,...*
C* YY   - 2-digit year          '[0-9][0-9]'               98,99,00,... *
C* MMM  - 3-char  month         '[A-Za-z][A-Za-z][A-Za-z]' jan,...,dec  *
C* MM   - 2-digit month         '[0-9][0-9]'               01,02,...,12 *
C* DD   - 2-digit day           '[0-9][0-9]'               01,02,...    *
C* HH   - 2-digit hour          '[0-9][0-9]'               00,01,...    *
C* NN   - 2-digit minute        '[0-9][0-9]'               00,01,...,59 *
C* DWK  - 3-char  day of week   '[A-Za-z][A-Za-z][A-Za-z]' sun,...,sat  *
C* FFF  - 3-digit forecast hour '[0-9][0-9][0-9]'          000,...      *
C* FF   - 2-digit forecast hour '[0-9][0-9]'               00,...       *

	   CALL ST_RPST ( ftmpl, 'YYYY', '[0-2][0-9][0-9][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'YY', '[0-9][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'MMM', '[A-Za-z][A-Za-z][A-Za-z]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'MM', '[0-1][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'DD', '[0-3][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'HH', '[0-2][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'NN', '[0-6][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'DWK', '[A-Za-z][A-Za-z][A-Za-z]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'FFF', '[0-9][0-9][0-9]', 
     +			ipos, ftmpl, ier )
	   CALL ST_RPST ( ftmpl, 'FF', '[0-9][0-9]', 
     +			ipos, ftmpl, ier )

	   CALL ST_NULL ( ftmpl, tmpl, lens, ier)
	END IF

	RETURN
	END
