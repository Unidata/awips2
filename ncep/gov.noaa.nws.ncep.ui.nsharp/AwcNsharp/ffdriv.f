	SUBROUTINE FFDRIV  ( nlvl, clvl, lincol, lintyp, linwid, linlbl,
     +			     linflg, iret )
C************************************************************************
C* FFDRIV								*
C*									*
C* This subroutine draws filled contours.				*
C*									*
C* FFDRIV  ( NLVL, CLVL, LINCOL, LINTYP, LINWID, LINLBL, LINFLG, IRET )	*
C*									*
C* Input parameters:							*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour levels			*
C*	LINCOL (NLVL)	INTEGER		Contour colors			*
C*	LINTYP (NLVL)	INTEGER		Contour line types		*
C*	LINWID (NLVL)	INTEGER		Contour line widths		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*	LINFLG		LOGICAL		Flag to draw lines 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91						*
C* D.W.Plummer/NCEP	 1/97	Remove call to GEPLOT prior to RETURN	*
C* T. Lee/SAIC		10/01	Query and restore fill attributes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		clvl (*)
	INTEGER		lincol (*), lintyp (*), linwid (*), linlbl (*)
	LOGICAL		linflg
C*
	REAL		box (4), xpt (250), ypt (250)
	LOGICAL		onelev
C------------------------------------------------------------------------
	iret = 0
C
C*	Query current fill type.
C
	CALL GQFILL ( szfil, iftyp, iret )
C
C*	Loop through all the grid boxes.
C
	DO  jll = 1, jsize - 1
	  DO  ill = 1, isize - 1
C
C*	    Get points on this box and check to see if the entire box
C*	    is one color.
C
	    CALL FGTBOX  ( ill, jll, box, xx1, yy1, xx2, yy2, iret ) 
C
C*	    If the return code is not 0, there is missing data.
C
	    IF  ( iret .eq. 0 )  THEN
		CALL FGTCOR  ( box, nlvl, clvl, icornr, levl, onelev, 
     +			       ier )
	    END IF
C
C*	    Do nothing if there is missing data.
C
	    IF  ( iret .ne. 0 )  THEN
C
C*	      Fill entire box if it is all one color.
C
	     ELSE IF  ( onelev .and. ( .not. linflg ) )  THEN
C
C*	      Plot the entire box.
C
	      ipt = 1
	      xpt (ipt) = xx1
	      ypt (ipt) = yy1
	      DO  jj = 1, numsub - 2
		ipt = ipt + 1
		xpt (ipt) = xx1
		ypt (ipt) = FLOAT ( jj ) * fincxy + yy1
	      END DO
	      ipt = ipt + 1
	      xpt (ipt) = xx1
	      ypt (ipt) = yy2
	      DO  ii = 1, numsub - 2
		ipt = ipt + 1
		xpt (ipt) = FLOAT ( ii ) * fincxy + xx1
		ypt (ipt) = yy2
	      END DO
	      ipt = ipt + 1
	      xpt (ipt) = xx2
	      ypt (ipt) = yy2
	      DO  jj = numsub - 2, 1, -1
		ipt = ipt + 1
		xpt (ipt) = xx2
		ypt (ipt) = FLOAT ( jj ) * fincxy + yy1
	      END DO
	      ipt = ipt + 1
	      xpt (ipt) = xx2
	      ypt (ipt) = yy1
	      DO  ii = numsub - 2, 1, -1
		ipt = ipt + 1
		xpt (ipt) = FLOAT ( ii ) * fincxy + xx1
		ypt (ipt) = yy1
	      END DO
	      CALL FFPLOT  ( ipt, xpt, ypt, linflg, lincol (levl),
     +			     lintyp (levl), linwid (levl), ier )
C
C*	      If box is not subdivided, fill in the whole box.
C
	     ELSE IF  ( numsub .le. 2 )  THEN
	      CALL FILBOX  ( box, xx1, yy1, xx2, yy2, nlvl, clvl,
     +			     lincol, lintyp, linwid, icornr, levl, 
     +			     linflg, ier )
C
C*	      Subdivide the box.
C
	     ELSE
C
C*	      Set up the coefficients for this box.
C
	      CALL CCOEFF  ( ill, jll, ier )
C
C*	      Loop through all the subboxes.
C
	      DO  jsub = 1, numsub - 1
		DO  isub = 1, numsub - 1
C
C*		  Get the points on this subbox and check for a single
C*		  color.
C
		  CALL FGTSUB  ( ill, jll, isub, jsub, box, xx1, yy1,
     +				 xx2, yy2, ier )
		  CALL FGTCOR  ( box, nlvl, clvl, icornr, levl, onelev,
     +				 ier )
C
C*		  Fill the entire box if it is all one color.
C
		  IF  ( onelev )  THEN
C
C*		    Plot the entire box.
C
		    xpt (1) = xx1
		    ypt (1) = yy1
		    xpt (2) = xx1
		    ypt (2) = yy2
		    xpt (3) = xx2
		    ypt (3) = yy2
	      	    xpt (4) = xx2
		    ypt (4) = yy1
		    ipt = 4
		    CALL FFPLOT  ( ipt, xpt, ypt, linflg, lincol (levl),
     +				   lintyp (levl), linwid (levl), ier )
C
C*		    Fill in this box.
C
		   ELSE
		    CALL FILBOX (box, xx1, yy1, xx2, yy2, nlvl, clvl,
     +				 lincol, lintyp, linwid, icornr, 
     +				 levl, linflg, ier )
		  END IF
		END DO
	      END DO
	    END IF
	  END DO
	END DO
999	continue
C
C*	Restore fill type setup.
C
	CALL GSFILL ( szfil, iftyp, iret )
C
C*	End plotting.
C
	RETURN
	END
