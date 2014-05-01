	SUBROUTINE FGTCOR  ( box, nlvl, clvl, icornr, ilevel, onelev,
     +			     iret )
C************************************************************************
C* FGTCOR								*
C*									*
C* This subroutine finds the corner of the box or subbox which has the	*
C* lowest value.  The corner and lowest level are returned.  If the box	*
C* is all one level, ONELEV is set.					*
C*									*
C* FGTCOR  ( BOX, NLVL, CLVL, ICORNR, ILEVEL, ONELEV, IRET )		*
C*									*
C* Input parameters:							*
C*	BOX (4)		REAL		Grid values (LL,LR,UR,UL)	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL (NLVL+1)	REAL		Contour levels			*
C*									*
C* Output parameters:							*
C*	ICORNR		INTEGER		Corner with lowest value	*
C*	ILEVEL		INTEGER		Level at lowest corner		*
C*	ONELEV		LOGICAL		Flag for single level		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91				 		*
C************************************************************************
	REAL		box (4), clvl (*)
	LOGICAL		onelev
C----------------------------------------------------------------------
	iret = 0
C
C*	Find the corner with the lowest value.
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
C*	Compute ilevel which is level at lowest corner.
C
	ilevel = 1
	DO  i = 1, nlvl
	    IF  ( dlow .gt. clvl (i) )  ilevel = i + 1
	END DO
	clev = clvl (ilevel)
C
C*	Check for solid box.
C
	IF  ( ( box (1) .le. clev ) .and. 
     +	      ( box (2) .le. clev ) .and.
     +	      ( box (3) .le. clev ) .and.
     +	      ( box (4) .le. clev ) )  THEN
	    onelev = .true.
	  ELSE
	    onelev = .false.
	END IF
C
C*	Save the number of the lowest corner.
C
	icornr = ilow
C*
	RETURN
	END
