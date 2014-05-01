	SUBROUTINE FILBOX  ( box, x1, y1, x2, y2, nlvl, clvl, lincol, 
     +			     lintyp, linwid, icornr, ilev, linflg, iret)
C************************************************************************
C* FILBOX								*
C*									*
C* This subroutine draws filled polygons through a single grid box.	*
C*									*
C* FILBOX  ( BOX, X1, Y1, X2, Y2, NLVL, CLVL, LINCOL, LINTYP, LINWID,	*
C*           ICORNR, ILEV, LINFLG, IRET )				*
C*									*
C* Input parameters:							*
C*	BOX (4)		REAL		Grid values (LL,LR,UR,UL)	*
C*	X1		REAL		X grid coordinate at LL		*
C*	Y1		REAL		Y grid coordinate at LL		*
C*	X2		REAL		X grid coordinate at UR		*
C*	Y2		REAL		Y grid coordinate at UR		*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL+1)	REAL		Contour levels			*
C*	LINCOL (NLVL+1)	INTEGER		Colors for contour levels	*
C*	LINTYP (NLVL+1)	INTEGER		Line type for contour levels	*
C*	LINWID (NLVL+1)	INTEGER		Line width for contour levels	*
C*	ICORNR		INTEGER		Lowest corner			*
C*	ILEV		INTEGER		Level at lowest corner		*
C*	LINFLG		LOGICAL		Flag to draw lines / fill	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Fulson-Woytek/GSFC 						*
C* M. desJardins/NMC	11/91	Adapted algorithm for GEMPAK 5.1	*
C* S. Jacobs/EAI	 3/93	Fixed typo cver --> dver		*
C************************************************************************
	REAL		box (4), clvl (*)
	INTEGER		lincol (*), lintyp (*), linwid (*)
	LOGICAL		linflg
C*
	LOGICAL		done, found, oppflg
	REAL		xpt (10), ypt (10), xptopp (5), yptopp (5)
C
C*	The following statement function interpolates x or y along a
C*	side of the box.
C
	RINTRP ( d1, d2, xy1, xy2 ) = xy1 + ( ( clev - d1 ) /
     +					      ( d2 - d1 ) ) *
     +					    ( xy2 - xy1 )
C----------------------------------------------------------------------
	iret = 0
	ilow = icornr
	dlow = box (ilow)
C
C*	Set lowest color and level.
C
	icol = lincol (ilev)
	ityp = lintyp (ilev)
	iwid = linwid (ilev)
	clev = clvl   (ilev)
C
C*	Compute value and coordinates at low, horizontal, vertical,
C*	and opposite points.
C
	IF  ( ilow .eq. 1 )  THEN
	    dlow = box (1)
	    xlow = x1
	    ylow = y1
	    dhor = box (2)
	    xhor = x2
	    yhor = y1
	    dopp = box (3)
	    xopp = x2
	    yopp = y2
	    dver = box (4)
	    xver = x1
	    yver = y2
	  ELSE IF  ( ilow .eq. 2 )  THEN
	    dlow = box (2)
	    xlow = x2
	    ylow = y1
	    dhor = box (1)
	    xhor = x1
	    yhor = y1
	    dopp = box (4)
	    xopp = x1
	    yopp = y2
	    dver = box (3)
	    xver = x2
	    yver = y2
	  ELSE IF  ( ilow .eq. 3 )  THEN
	    dlow = box (3)
	    xlow = x2
	    ylow = y2
	    dhor = box (4)
	    xhor = x1
	    yhor = y2
	    dopp = box (1)
	    xopp = x1
	    yopp = y1
	    dver = box (2)
	    xver = x2
	    yver = y1
	  ELSE
	    dlow = box (4)
	    xlow = x1
	    ylow = y2
	    dhor = box (3)
	    xhor = x2
	    yhor = y2
	    dopp = box (2)
	    xopp = x2
	    yopp = y1
	    dver = box (1)
	    xver = x1
	    yver = y1
	END IF
C
C*	Move low corner into XPT, YPT array of points in polygon.
C
	xpt (1) = xlow
	ypt (1) = ylow
	ipt     = 1
C
C*	Initialize counters for finding polygon.
C
	iside1 = 0
	iside2 = 0
	yq1    = yopp
	xq2    = xopp
	oppflg = .false.
	done   = .false.
C
C*	Loop through box until entire box is filled.
C
	DO WHILE  ( .not. done )
C
C*	    Find the first point.
C
	    found = .false.
	    DO WHILE  ( .not. found )
C*
		IF  ( ( iside1 .le. 1 ) .and. 
     +		      ( dhor .ge. clev ) )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = RINTRP ( dlow, dhor, xlow, xhor ) 
		    ypt (ipt) = ylow
		    iside1 = 1
		    IF  ( dhor .eq. clev )  iside1 = 2
		    found  = .true.
		  ELSE IF  ( iside1 .le. 1 )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = xhor
		    ypt (ipt) = yhor
		    iside1 = 2
		  ELSE IF  ( iside2 .eq. 2 )  THEN
		    done  = .true.
		    found = .true.
		  ELSE IF  ( ( iside1 .eq. 2 ) .and.
     +			     ( dopp .ge. clev ) )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = xhor
		    ypt (ipt) = RINTRP ( dhor, dopp, yhor, yopp )
		    IF  ( dopp .eq. clev )  iside1 = 3
		    found = .true.
		  ELSE IF  ( iside1 .eq. 2 )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = xhor
		    ypt (ipt) = yq1
		    iside1 = 3
		    IF  ( oppflg )  THEN
			ipt = ipt + 1
			xpt (ipt) = xq2
			ypt (ipt) = yopp
		    END IF
		  ELSE IF  ( iside2 .eq. 3 )  THEN
		    done  = .true.
		    found = .true.
		  ELSE IF  ( ( iside1 .eq. 3 ) .and.
     +			     ( dver .ge. clev ) )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = RINTRP ( dopp, dver, xopp, xver )
		    ypt (ipt) = yopp
		    IF  ( dver .eq. clev )  iside1 = 4
		    found = .true.
		  ELSE IF  ( iside1 .eq. 3 )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = xver
		    ypt (ipt) = yver
		    found = .true.
		    done  = .true.
		END IF
	    END DO
	    IF  ( iside1 .eq. iside2 )  done = .true.
C
C*	    Save KNT which tells us where the first point was
C*	    in the array.
C
	    knt = ipt
C
C*	    Now, find the second point.
C
	    IF  ( .not. done )  THEN
		IF  ( iside2 .eq. 0 )  iside2 = 4
		IF  ( ( iside2 .eq. 4 ) .and.
     +		      ( dver .ge. clev ) )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = xlow
		    ypt (ipt) = RINTRP ( dlow, dver, ylow, yver )
		    iside2 = 4
		    IF  ( dver .eq. clev )  iside2 = 3
		  ELSE IF  ( ( iside2 .ge. 3 ) .and.
     +			     ( dopp .ge. clev ) )  THEN
		    ipt = ipt + 1
		    xpt (ipt) = RINTRP ( dver, dopp, xver, xopp )
		    ypt (ipt) = yver
		    IF  ( iside2 .eq. 4 )  THEN
			ipt = ipt + 1
			xpt (ipt) = xver
			ypt (ipt) = yver
		    END IF
		    iside2 = 3
		    IF  ( dopp .eq. clev )  iside2 = 2
		  ELSE 
		    ipt = ipt + 1
		    xpt (ipt) = xopp
		    ypt (ipt) = RINTRP ( dopp, dhor, yopp, yhor )
		    IF  ( iside2 .ge. 3 )  THEN
			ipt = ipt + 1
			xpt (ipt) = xopp
			ypt (ipt) = yq1
			IF  ( oppflg )  THEN
			    ipt = ipt + 1
			    xpt (ipt) = xq2
			    ypt (ipt) = yopp
			END IF
		    END IF
		    IF  ( iside2 .eq. 4 )  THEN
			ipt = ipt + 1
			xpt (ipt) = xver
			ypt (ipt) = yver
		    END IF
		    iside2 = 2
		END IF
	    END IF
	    IF  ( iside1 .eq. iside2 )  done = .true.
C
C*	    Now set the color and draw polygon.
C
	    IF  ( icol .ne. 0 )  THEN
		CALL FFPLOT  ( ipt, xpt, ypt, linflg, icol, ityp, iwid,
     +			       ier )
	    END IF
C
C*	    Check for points from the opposite corner.
C
	    IF  ( ( .not. done ) .and. ( iside1 .le. 2 ) .and.
     +		  ( iside2 .ge. 3 ) .and. 
     +		  ( dopp .lt. clev ) )  THEN
		IF  ( oppflg )  THEN
		    xptopp (1) = xq2
		    yptopp (1) = yopp
		    xptopp (2) = xopp
		    yptopp (2) = yq1
		    xptopp (3) = xopp
		    yptopp (3) = RINTRP ( dopp, dhor, yopp, yhor )
		    xptopp (4) = RINTRP ( dver, dopp, xver, xopp )
		    yptopp (4) = yopp
		    ipt = 4
		    CALL FFPLOT  ( ipt, xptopp, yptopp, linflg, icol,
     +				   ityp, iwid, ier )
		    xq2 = xptopp (4)
		    yq1 = yptopp (3)
		  ELSE
		    xptopp (1) = xopp
		    yptopp (1) = yopp
		    xptopp (2) = xopp
		    yptopp (2) = RINTRP ( dopp, dhor, yopp, yhor )
		    xptopp (3) = RINTRP ( dver, dopp, xver, xopp )
		    yptopp (3) = yopp
		    ipt = 3
		    CALL FFPLOT  ( ipt, xptopp, yptopp, linflg, icol, 
     +				   ityp, iwid, ier )
		    xq2 = xptopp (3)
		    yq1 = yptopp (2)
		    oppflg = .true.
		END IF
	    END IF
C
C*	    If not done, increment level and loop.
C
	    IF  ( .not. done )  THEN
		ilev = ilev + 1
		clev = clvl   (ilev)
		icol = lincol (ilev)
		ityp = lintyp (ilev)
		iwid = linwid (ilev)
	    END IF
C
C*	    Retrieve the first two points for the next polygon.
C
	    IF  ( .not. done )  THEN
		xpt (1) = xpt (knt+1)
		ypt (1) = ypt (knt+1)
		xpt (2) = xpt (knt)
		ypt (2) = ypt (knt)
		ipt = 2
	    END IF
	END DO 
C*
	RETURN
	END
