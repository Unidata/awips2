	SUBROUTINE CCPLOT  ( cval, clabel, ilabel, scflag, iret )
C************************************************************************
C* CCPLOT								*
C*									*
C* This subroutine applies a smoothing function and draws a line	*
C* connecting the array of points.  The points are specified in grid	*
C* coordinates.								*
C*									*
C* CCPLOT  ( CVAL, CLABEL, ILABEL, SCFLAG, IRET )			*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour level			*
C*	CLABEL		CHAR*		Contour Label			*
C*	ILABEL		INTEGER		Label type			*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* M. desJardins/GSFC	 6/89	Rewritten for GEMPAK 5			*
C* K. Brill/NMC          9/90   Added gaps for contour labels		*
C* J. Nielsen/SUNYA	 3/91	Simplified and fixed gap algorithm	*
C* K. Brill/NMC		01/92	CALL CLBBOX for label gap		*
C* S. Jacobs/EAI	10/93	Changed CLABEL --> GR_LABL		*
C* S. Jacobs/NMC	 9/94	Changed the check for number of points	*
C*				   from 23 to 7				*
C* S. Jacobs/NCEP	 1/99	Added groups and non-break for label	*
C* S. Jacobs/NCEP	10/00	Added call to CCLIP for latlon clipping	*
C* C. Bailey/HPC	 6/06	Added contour label as input parameter	*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'CONTUR.CMN'
C*
	CHARACTER*(*)	clabel
	LOGICAL		done, labflg, more, scflag
	CHARACTER	text*24
	REAL		xlcl (1024), ylcl (1024)
C------------------------------------------------------------------------
	iret = 0
	labflg = .false.
C
C*	Start a new group for the line and label text.
C*	Group type 8 is for LABELs.
C
	CALL GSGRP ( 8, ier )
C
C*	Apply the smoothing function and draw a line connecting
C*	the specified array of points on the plot in grid coordinates.
C
	IF  ( jsmoth .gt. 0 )  THEN
	    CALL CSMTHN  ( jsmoth, number, xval, yval, ier )
	END IF
C
C*	Transform the points to the actual grid coordindates.
C
	IF  ( ( offx .ne. 0. ) .or. ( offy .ne. 0. ) .or.
     +	      ( skip .gt. 0. ) )  THEN
	    CALL CTRANG  ( number, xval, yval, xval, yval, ier )
	END IF
C
C*	If the device is VG, then clip the line at the map bounds.
C
	more = .true.
	ipnt = 1
	DO WHILE  ( more )
	    IF  ( ddev .eq. 'VG' )  THEN
		CALL CCLIP ( number, xval, yval, ipnt,
     +			     nlcl, xlcl, ylcl, more, ier )
	      ELSE
		more = .false.
		nlcl = number
		DO  i = 1, nlcl
		    xlcl (i) = xval (i)
		    ylcl (i) = yval (i)
		END DO
	    END IF
C
C*	    Add label if requested.
C
	    IF  ( ilabel .gt. 0 .and. nlcl .gt. 7 )  THEN
		text = clabel
		CALL ST_LSTR  ( text, nchar, ier )
		labflg = .true.
		ihalf = nlcl / 2
		xl    = xlcl ( ihalf )
		yl    = ylcl ( ihalf )
C
C*	        Blank out a space in the contour line to make the label
C*              visible.  Allow for a blank rectangle surrounding the 
C*	        label with a margin of 0.1 characters on the top and
C*	        bottom and 0.2 characters on a side.
C
		CALL GTRANS ( 'G', 'N', 1, xlcl ( ihalf ),
     +                        ylcl ( ihalf ), x0, y0, ier )
C
C*	    	Set the bounds of the label box.
C
		CALL GQSYSZ ( wmk, zmk, c1, c2, blx, bly, ier )
		c1 = c1 * ( FLOAT ( nchar ) + 0.4 ) * .5
		c2 = c2 * .6
		xin = 0.
		yin = 0.
		xf = x0
		yf = y0
C
C*	        Start at the median point and step backward to find the
C*	        first point outside of the label box.
C
		iback = ihalf
		done = .false.
		DO WHILE ( .not. done )
		    iback = iback - 1
		    IF ( iback .lt. 1 ) THEN
			done = .true.
			iback = 1
		    ELSE
			CALL GTRANS ( 'G', 'N', 1, xlcl (iback), 
     + 		     	              ylcl (iback), xf, yf, ier )
			IF ( ABS (xf-x0) .gt. c1 ) done = .true.
			IF ( ABS (yf-y0) .gt. c2 ) done = .true.
			IF ( .not. done ) THEN
			    xin = xf - x0
			    yin = yf - y0
			END IF
		    END IF
		END DO
		xll = -c1
		yll = -c2
		xf = xf - x0
		yf = yf - y0
		CALL CLBBOX ( xll, yll, c1, c2, xin, yin, xf, yf, ier )
		xf = xf + x0
		yf = yf + y0
		istop = iback + 1
		CALL GTRANS ( 'N', 'G', 1, xf, yf, xlcl (istop),
     +			      ylcl (istop), ier )
C
C*	        Draw the contour to the edge of the label space.
C
		CALL GLINE ( 'G', istop, xlcl, ylcl, iret )
C*
		ifrwd = ihalf
		done = .false.
		xin = 0.
		yin = 0.
		xf = x0
		yf = y0
C
C*	        Start at the median point and step foreward to find the
C*	        first point outside of the label box.
C
		DO WHILE ( .not. done )
		    ifrwd = ifrwd + 1
		    IF ( ifrwd .gt. nlcl ) THEN
			done = .true.
			ifrwd = nlcl
		    ELSE
			CALL GTRANS ( 'G', 'N', 1, xlcl (ifrwd), 
     + 		     	              ylcl (ifrwd), xf, yf, ier )
			IF ( ABS (xf-x0) .gt. c1 ) done = .true.
			IF ( ABS (yf-y0) .gt. c2 ) done = .true.
			IF ( .not. done ) THEN
			    xin = xf - x0
			    yin = yf - y0
			END IF
		    END IF
		END DO
		xf = xf - x0
		yf = yf - y0
		CALL CLBBOX ( xll, yll, c1, c2, xin, yin, xf, yf, ier )
		xf = xf + x0
		yf = yf + y0
		istrt = ifrwd - 1
		CALL GTRANS ( 'N', 'G', 1, xf, yf, xlcl (istrt),
     +			      ylcl (istrt), ier )
		nfrwd = nlcl - istrt + 1
C
C*	        Draw the rest of the contour.
C
		CALL GLINE ( 'G', nfrwd, xlcl (istrt), ylcl (istrt),
     +                       iret )
C
C*	        Plot the label.
C
		ixoff = -nchar + 1
		CALL GTEXT  ( 'G', xl, yl, text, 0., ixoff, 0, ier )
C*
	      ELSE IF  ( ilabel .lt. 0 .and. nlcl .gt. 7 )  THEN
C
C*	    	If the label value is negative, plot the contour value,
C*	    	but DO NOT break the line.
C
		text = clabel
		CALL ST_LSTR  ( text, nchar, ier )
		labflg = .true.
		ihalf = nlcl / 2
		xl    = xlcl ( ihalf )
		yl    = ylcl ( ihalf )
C
C*	    	Draw the line.
C
		CALL GLINE  ( 'G', nlcl, xlcl, ylcl, iret )
C
C*	    	Plot the label.
C
		CALL GTEXT  ( 'G', xl, yl, text, 0., 0, 0, ier )
	    END IF
C
C*	    Draw the line. 
C
	    IF  ( ( scflag .and. nlcl .gt. 7 ) .or. .not. scflag) THEN
	        IF  ( nlcl .ge. 2 .and. .not. labflg )  THEN
		  CALL GLINE  ( 'G', nlcl, xlcl, ylcl, iret )
	        END IF
	    END IF
C
	END DO
C
C*	End the group for the line and label text.
C
	CALL GEGRP ( ier )
C*
	RETURN
	END
