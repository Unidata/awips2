	SUBROUTINE GSDEVA ( device, iunit, filnam, itype, xsize, ysize,
     +			    iret )
C************************************************************************
C* GSDEVA								*
C* 									*
C* This subroutine sets the plot device to be used by GEMPLT.  If	*
C* another device is in use when it is called, GSDEVA terminates	*
C* plotting on that device, then starts the device subprocess for	*
C* the requested device.						*
C*									*
C* GSDEVA ( DEVICE, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, IRET )		*
C*									*
C* Input parameters:							*
C* 	DEVICE		CHAR*		Device name			*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		File name or window name	*
C*	ITYPE		INTEGER		Device color capability		*
C*	XSIZE		REAL		Width in inches or pixels	*
C*	YSIZE		REAL		Height in inches or pixels	*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 3/96	GSDEVA based on GSDEV			*
C* S. Jacobs/NCEP	 4/96	Added iunit to DSDATT			*
C* S. Jacobs/NCEP	 4/96	Reordered error checking for blank dev	*
C* S. Jacobs/NCEP	 9/96	Added checks for and processing of XWP	*
C* S. Jacobs/NCEP	11/96	Added check to reset colors		*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* C. Lin/EAI		 6/97	Modified the IF condition in setting    *
C*				device attribute section		*
C* S. Wang/GSC		03/97   Remove re_initializing sub-device color *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVWIN.CMN'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	device, filnam
C*
	CHARACTER	dev*12
	LOGICAL		newdv
C------------------------------------------------------------------------

	iret = NORMAL
C
	CALL ST_LCUC ( device, dev, ier )
	IF  ( ( iunit .lt. 1 ) .or. ( iunit .gt. 2 ) )  iunit = 1
C
C*	Check to see if the device has changed.
C
cc	print*, ' DEV = ', dev
cc	print*, '???? ddev????? = ', ddev
	IF ( ( dev .ne. ddev ) .or. ( ddev .eq. ' ' ) ) THEN
C
C*	    Check for a previous device or for the special XWP device.
C
	    IF  ( ( ddev .eq. ' ' ) .or.
     +		  ( ( curdev .eq. 'XWP' ) .and.
     +		    ( ( dev .eq. 'XWP' ) .or.
     +		      ( dev .eq. 'XW'  ) .or.
     +		      ( dev .eq. 'PS'  ) ) ) )  THEN
C
C*		If switching from PS to XW, close the plot file.
C
		IF  ( ( ddev .eq. 'PS' ) .and.
     +		      ( ( dev .eq. 'XWP' ) .or.
     +			( dev .eq. 'XW'  ) ) )  THEN
		    CALL DCLOSP ( ncurwn, ier )
		END IF
C
	      ELSE
C
C*		If there was a device installed, stop it.
C
		ieop = 1
		CALL DENDD ( ieop, ier )
	    END IF
C
C*	    Start the new device driver.
C
	    CALL DINITA ( dev, curdev, iunit, filnam, itype,
     +			  xsize, ysize, ncurwn, iret )
	    ncurwn = ncurwn + 1
	    newdv  = .true.
	  ELSE
C
C*	    Send possibly changed attributes to device.
C
	    CALL DSDATT ( iunit, filnam, itype, xsize, ysize,
     +			  ncurwn, iret )
	    ncurwn = ncurwn + 1
	    newdv  = .false.
	END IF
C
C*	Set the device characteristics and attributes.
C
	IF  ( ( iret .eq. NEWWIN ) .or. ( iret .eq. NWSIZE ) .or.
     +	      ( newdv .and. ( iret .eq. NORMAL ) ) )  THEN
C
C*	    If this is a new window, reset the margins.
C
	    IF  ( iret .eq. NEWWIN )  THEN
		CALL GSMMGN ( 0., 0., 0., 0., ier )
		CALL GSGMGN ( 0., 0., 0., 0., ier )
	    END IF
C
C*	    Reset iret to NORMAL return code.
C
	    iret = NORMAL
C
C*	    Get the information from /DEVCHR/.
C
	    CALL DQDCHR ( nncolr, ier )
C
C*	    Store the device name in /DEVCHR/.
C
	    IF  ( ( curdev .eq. 'XWP' ) .and.
     +		  ( ( dev .eq. 'XWP' ) .or.
     +		    ( dev .eq. 'XW'  ) .or.
     +		    ( dev .eq. 'PS'  ) ) )  THEN
		curdev = 'XWP'
	      ELSE
		curdev = dev
	    END IF
	    ddev   = dev
	    niunit = iunit
C
C*	    Set the drawing attributes and map/graph projections.
C
	    CALL GSATTR ( iret )
	  ELSE
	    IF  ( ( newdv ) .and. ( iret .ne. NORMAL ) )  THEN
		ddev = ' '
		IF  ( iret .eq. NOPROC )  iret = NODEVC
		RETURN
	    END IF
	END IF
C
C*	Flush the buffers and make the window appear.
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
