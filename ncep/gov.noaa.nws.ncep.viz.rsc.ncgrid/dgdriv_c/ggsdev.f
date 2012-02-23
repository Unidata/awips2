	SUBROUTINE GG_SDEV ( device, iret )
C************************************************************************
C* GG_SDEV								*
C*									*
C* This subroutine sets the graphics device in GEMPLT.  If an 		*
C* error is returned from GEMPLT, an error message is written.		*
C*									*
C* GG_SDEV ( DEVICE, IRET )						*
C*									*
C* Input parameters:							*
C*	DEVICE		CHAR*		Device name 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				 	 -6 = invalid device specified	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 5/88	Eliminated saving old device name	*
C* K. Brill/NMC		 1/92	Replace GERROR with ER_WMSG		*
C* A. Chang/EAI		 4/94	Call GSFLNM if output file name is given*
C* C. Lin/EAI		 8/94	call GSFLNM even the file name is blank *
C* P. Bruehl/Unidata	 8/94	Added check for error from GSFLNM	*
C* M. Linda/GSC		 3/96	Added attributes to the DEVICE variable	*
C* S. Jacobs/NCEP	 4/96	Added default name for GIF file 	*
C* S. Jacobs/NCEP	 5/96	Added checks for PSC, PSP, PS1		*
C* S. Jacobs/NCEP	 9/96	Added check for XWP			*
C* S. Jacobs/NCEP	 9/96	Added check for VG			*
C* S. Jacobs/NCEP	 7/97	Added default file name for FAX		*
C* S. Jacobs/NCEP	 8/97	Added default file name for UTF		*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* S. Jacobs/NCEP	 2/99	Added default file name for TIFF	*
C* T. Lee/GSC		 7/00	Added default file name for GIF		*
C* S. Jacobs/NCEP	 9/00	Added special case for ctype for TIFF	*
C* A. Hardy/SAIC         5/02   Renamed RBK default filename		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
C*	CHARACTER*(*)	device
	CHARACTER*128	device
C
	CHARACTER	carr (4)*72, ddev*72, filnam*72, ctype*1
	REAL		xyarr (2)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Parse device string into components and translate into numbers.
C
	CALL ST_CLST ( device, '|', ' ', 4, carr, num, ier ) 
C
	CALL ST_LCUC ( carr (1), ddev, ier )
C
C*	Check for blank file/window name. Set a default based
C*	on the device type.
C
	filnam = carr (2)
	IF  ( filnam .eq. ' ' )  THEN
	    IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
		filnam = 'GEMPAK'
	      ELSE IF  ( ddev .eq. 'PS'  )  THEN
		filnam = 'ps.plt'
	      ELSE IF  ( ddev .eq. 'NC'  )  THEN
		filnam = 'Nmeta'
	      ELSE IF  ( ddev .eq. 'GF'  )  THEN
		filnam = 'gempak.gif'
	      ELSE IF  ( ddev .eq. 'GIF'  )  THEN
		filnam = 'gempak.gif'
	      ELSE IF  ( ddev .eq. 'PSC' )  THEN
		filnam = 'psc.plt'
	      ELSE IF  ( ddev .eq. 'PS1' )  THEN
		filnam = 'ps1.plt'
	      ELSE IF  ( ddev .eq. 'PSP' )  THEN
		filnam = 'psp.plt'
	      ELSE IF  ( ddev .eq. 'VG' )  THEN
		filnam = 'vgf.vgf'
	      ELSE IF  ( ddev .eq. 'FAX' )  THEN
		filnam = '999X;0167'
	      ELSE IF  ( ddev .eq. 'UTF' )  THEN
		filnam = 'T01'
	      ELSE IF  ( ddev .eq. 'RBK' )  THEN
		filnam = 'T01'
	      ELSE IF  ( ddev .eq. 'TIFF' )  THEN
		filnam = 'AAAA00'
	    END IF
	END IF
C
	CALL ST_RLST ( carr(3), ';', RMISSD, 2, xyarr, num, ier )
	IF  ( ddev .eq. 'PSP' )  THEN
	    xsize = 8.5
	    ysize = 11.0
	  ELSE IF  ( ( ddev .eq. 'PSC' ) .or.
     +		     ( ddev .eq. 'PS1' ) )  THEN
	    xsize = 11.0
	    ysize = 8.5
	  ELSE
	    xsize = xyarr (1)
	    ysize = xyarr (2)
	END IF
C
	IF  ( ddev .eq. 'PSC' )  THEN
	    ctype = 'C'
	  ELSE IF  ( ddev .eq. 'PS1' )  THEN
	    ctype = 'M'
	  ELSE IF  ( ddev .eq. 'PSP' )  THEN
	    ctype = 'G'
	  ELSE
	    CALL ST_LCUC ( carr(4), ctype, ier )
	END IF
C
	IF  ( ( ddev .eq. 'TIFF' ) .and. ( ctype .eq. ' ' ) )  THEN
	    ctype = 'M'
	END IF
C
	itype = 1
	IF  ( ctype .eq. 'M' )  THEN
	    itype = 0
	  ELSE IF  ( ctype .eq. 'C' )  THEN
	    itype = 2
	END IF
C
C*	Reset PSC, PS1 and PSP to PS.
C
	IF  ( ( ddev .eq. 'PSC' ) .or.
     +	      ( ddev .eq. 'PS1' ) .or.
     +	      ( ddev .eq. 'PSP' ) )  ddev = 'PS'
C
C*	Set device in GEMPLT.
C
	iunit = 1
	CALL GSDEVA ( ddev, iunit, filnam, itype, xsize, ysize, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', ier2 )
	    iret = -6
	END IF
C*
	RETURN
	END
