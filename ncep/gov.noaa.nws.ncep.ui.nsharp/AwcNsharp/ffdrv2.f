	SUBROUTINE FFDRV2  ( nlvl, clvl, lincol, lintyp, linwid, linlbl,
     +			     linflg, iret )
C************************************************************************
C* FFDRV2								*
C*									*
C* This subroutine draws filled contours.				*
C*									*
C* FFDRV2  ( NLVL, CLVL, LINCOL, LINTYP, LINWID, LINLBL, LINFLG, IRET )	*
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
C* M. Li/GSC		 1/00	Copied from FFDRIV			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		clvl (*)
	INTEGER		lincol (*), lintyp (*), linwid (*), linlbl (*)
	LOGICAL		linflg
C*
	REAL		box (4), xpt (250), ypt (250)
	LOGICAL		onelev, cont
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through all the grid boxes.
C
	DO  jll = 1, jsize - 1
	    DO  ill = 1, isize - 1
C
C*	    	Get points on this box and check to see if the entire
C*		box is one color.
C
		box (1) = z ( (jll-1) * isize + ill )
		box (2) = z ( (jll-1) * isize + ill + 1 )
		box (3) = z ( ( jll ) * isize + ill + 1 )
		box (4) = z ( ( jll ) * isize + ill )
C
C*	    	Check for missing data
C
		IF  ( ( ERMISS ( box (1) ) ) .or.
     +		      ( ERMISS ( box (2) ) ) .or.
     +		      ( ERMISS ( box (3) ) ) .or.
     +		      ( ERMISS ( box (4) ) ) )  THEN
		    cont = .false.
		  ELSE
		    cont = .true.
		    xx1  = ill
		    yy1  = jll
		    xx2  = xx1 + 1.0
		    yy2  = yy1 + 1.0
		END IF
C
C*	    	Continue if there is no missing data.
C
		IF  ( cont )  THEN
C
C*		    Find the corner with the lowest value.
C
		    ilow = 1
		    dlow = box (1)
		    DO  i = 2, 4
			IF  ( box (i) .lt. dlow )  THEN
			    ilow = i
			    dlow = box (i)
			END IF
		    END DO
C
C*		    Compute levl which is level at lowest corner.
C
		    levl = 1
		    DO  i = 1, nlvl
			IF  ( dlow .gt. clvl (i) )  levl = i + 1
		    END DO
		    clev = clvl (levl)
C
C*		    Check for solid box.
C
		    IF  ( ( box (1) .le. clev ) .and. 
     +			  ( box (2) .le. clev ) .and.
     +			  ( box (3) .le. clev ) .and.
     +			  ( box (4) .le. clev ) )  THEN
			onelev = .true.
		      ELSE
			onelev = .false.
		    END IF
C
C*		    Save the number of the lowest corner.
C
		    icornr = ilow
C
C*		    Fill entire box if it is all one color.
C
		    IF  ( onelev .and. ( .not. linflg ) )  THEN
C
C*	                Plot the entire box.
C
			ipt = 1
			xpt (ipt) = xx1
			ypt (ipt) = yy1
C
			ipt = ipt + 1
			xpt (ipt) = xx1
			ypt (ipt) = yy2
C
			ipt = ipt + 1
			xpt (ipt) = xx2
			ypt (ipt) = yy2
C
			ipt = ipt + 1
			xpt (ipt) = xx2
			ypt (ipt) = yy1
C
			CALL FFPLOT ( ipt, xpt, ypt, linflg,
     +				      lincol (levl), lintyp (levl),
     +				      linwid (levl), ier )
		      ELSE
C
C*			Fill in the whole box with appropriate divisions.
C
			CALL FILBOX ( box, xx1, yy1, xx2, yy2, nlvl,
     +				      clvl, lincol, lintyp, linwid,
     +				      icornr, levl, linflg, ier )
		    END IF
		END IF
	    END DO
	END DO
C
C*	End plotting.
C
	RETURN
	END
