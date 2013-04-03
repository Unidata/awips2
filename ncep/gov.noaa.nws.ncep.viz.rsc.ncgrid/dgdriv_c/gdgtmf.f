	SUBROUTINE GD_GTMF  ( gdfile, gdatim, cycle, maxt, 
     +                        ngdftm, gdtlst, iret )
C************************************************************************
C* GD_GTMF								*
C*									*
C* This subroutine returns all the times present in a grid file or	*
C* grid file template.  Only the first times are returned.  		*
C* The times are sorted from most recent to oldest.			*
C*									*
C* GD_GTMF  ( GDFILE, GDATIM, CYCLE, MAXT, NGDFTM, GDTLST, IRET )	*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	GDATIM		CHAR*		Grid datetime			*
C*	CYCLE		CHAR*		Cycle (GEMPAK format)		*
C*	MAXT		INTEGER		Maximum number of times allowed	*
C*									*
C* Output parameters:							*
C*	NGDFTM		INTEGER		Number of times			*
C*	GDTLST(*)	CHAR*		List of GEMPAK times		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* D.W.Plummer/NCEP	11/96	Started with GD_GTIM; converted to 	*
C*				handle grid file name instead of number	*
C*				handle grid file name instead of number	*
C* D.W.Plummer/NCEP	 2/98	Added capability to handle templates	*
C* D.W.Plummer/NCEP	 8/98	Rewrote to improve algorithm		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Lee/GSC		 7/99	Added CYCLE to call seq of FL_MFLS	*
C* S. Jacobs/NCEP	 9/99	Changed call to FL_MDAT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdfile, gdtlst(*), gdatim, cycle
C
	CHARACTER	filnms(LLMXGT)*256, tmplt*64, gdtime*20
C-----------------------------------------------------------------------
	iret = 0
	ngdftm = 0
C
C*	Get files.
C
	CALL FL_MFLS  ( gdfile, gdatim, cycle, LLMXGT, filnms, nfiles, 
     +			tmplt, iret )
	IF ( iret .ne. 0 )  RETURN
C
C*	Check template (tmplt).  If it does not exist or if it
C*	does not contain the string 'fFF', open all the files to
C*	get all the times.  Otherwise, build all the times from 
C*	the filenames directly.
C
	IF ( tmplt .ne. ' ' .and. 
     +		INDEX ( tmplt, 'fFF' ) .ne. 0 )  THEN
C
C*	    Filenames are assumed to contain complete GEMPAK date
C*	    information - including the forecast hour... 
C*	    build the GEMPAK time strings from the filenames.
C
	    ngdftm = 0
	    DO  nf = 1, nfiles
C
		CALL FL_MDAT ( filnms(nf), tmplt, '000101/0000',
     +				gdtime, iret )
C
		ngdftm = ngdftm + 1
		gdtlst ( ngdftm ) = gdtime
C
	    END DO
C
	ELSE
C
            CALL GD_FLTM ( filnms, nfiles, maxt, ngdftm, gdtlst, iret )
C
	END IF
C*
	RETURN
	END
