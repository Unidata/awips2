	SUBROUTINE CADJST ( kx, ky, nlvl, clvl, z, iret )
C************************************************************************
C* CADJST								*
C*									*
C* This subroutine adjusts the grid values so that contours do not pass	*
C* exactly through any grid points.					*
C*									*
C* Adjustment of the grid values prevents "holes" in the contour fills	*
C* and some bad contour lines.						*
C*									*
C* CADJST  ( KX, KY, NLVL, CLVL, Z, IRET )				*
C*									*
C* Input parameters:							*
C*									*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL (NLVL)	REAL		Contour level values		*
C*									*
C* Input and output parameters:						*
C*									*
C*	Z (KX,KY)	REAL		Grid data array			*
C*									*
C* Output parameters:							*
C*									*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 1/96	Adapted from WolfPlot Package		*
C* R. Tian/SAIC		 4/03	Modified calculation of adjustment value*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	REAL		clvl (*), z (*)
C*
	INCLUDE         'ERMISS.FNC'

C------------------------------------------------------------------------
C
C*	Find an appropriate adjustment value
C
	adj = ABS ( clvl ( nlvl ) - clvl ( 1 ) )
	DO  i = 1, nlvl -1
	    adj = MIN ( adj, ABS ( clvl ( i + 1 ) - clvl ( i ) ) )
	END DO
	IF  ( adj .eq. 0. ) THEN
	    IF  ( clvl ( 1 ) .eq. 0. ) THEN
		adj = .01
	      ELSE
		adj = clvl ( 1 )
	    END IF
	END IF
	adj = .0001 * adj 
	adjhlf = adj / 2.
C
C*	Adjust the grid value if it is within ADJ/2 of the
C*	contour value.
C
	DO  i = 1, kx * ky
	    DO  j = 1, nlvl
		diff = z(i) - clvl(j)
		IF  ( .not. ERMISS ( z ( i ) ) .and. 
     +		      ABS ( diff ) .lt. adjhlf ) THEN
     		    z(i) = z(i) + adj * SIGN ( 1., diff )
		END IF
	    END DO
	END DO
C*
	iret = 0
C*
	RETURN
	END
