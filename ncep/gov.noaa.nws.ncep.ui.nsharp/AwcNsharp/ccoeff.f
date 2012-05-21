	SUBROUTINE CCOEFF  ( ill, jll, iret )
C************************************************************************
C* CCOEFF								*
C*									*
C* This subroutine computes the Lagrangian coefficients for the		*
C* grid square whose lower left corner is ILL, JLL.			*
C*									*
C* CCOEFF  ( ILL, JLL, IRET )						*
C*									*
C* Input parameters:							*
C*	ILL		INTEGER		Lower left x coordinate		*
C*	JLL		INTEGER		Lower left y coordinate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* J. Whistler/SSAI	 6/91	Set internal grids to size LLMXGD	*
C* M. desJardins/NMC	12/91	Rename: COEFFS-->CCOEFF			*
C* S. Jacobs/EAI	 9/93	Moved ERMISS checks after the interps	*
C* M. desJardins/NMC	 3/94	Clean up all ERMISS checks		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		w (4,4)
C*
	PARAMETER	( COMP3 = 1./3., COMP6 = 1./6., COMP9 = 1./9. )
	PARAMETER	( COMP12 = 1./12., COMP18 = 1./18. )
	PARAMETER	( COMP36 = 1./36. )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Fill the W array.  This was originally done outside COEFFS.
C
	num = isize * jsize
	DO  ii = 1, 4
	    DO  jj = 1, 4
		knt = (jll+jj-2-1)*isize + (ill+ii-2)
		IF  ( ( knt .lt. 1 ) .or. ( knt .gt. num ) )  THEN
		    w ( ii, jj ) = RMISSD
		  ELSE
		    w ( ii, jj ) = z ( knt )
		END IF
	    END DO
	END DO
C
C*	The contours are being drawn through the grid box whose
C*	lower left corner is (ill, jll), i.e. points w(2,2)-
C*	w(2,3)-w(3,3)-w(3,2).  None of these points is missing.
C*	First, interpolate linearly to all points directly outside
C*	the grid box, but not outside the grid itself.
C
	IF  ( jll .ne. jsize - 1 )  THEN
	    IF ( ERMISS (w(2,4))) w(2,4) = 2.0 * w(2,3) - w(2,2)
	    IF ( ERMISS (w(3,4))) w(3,4) = 2.0 * w(3,3) - w(3,2)
	END IF
C*
	IF  ( ill .ne. 1 )  THEN
	    IF ( ERMISS (w(1,3))) w(1,3) = 2.0 * w(2,3) - w(3,3)
	    IF ( ERMISS (w(1,2))) w(1,2) = 2.0 * w(2,2) - w(3,2)
	END IF
C*
	IF  ( jll .ne. 1 )  THEN
	    IF ( ERMISS (w(2,1))) w(2,1) = 2.0 * w(2,2) - w(2,3)
	    IF ( ERMISS (w(3,1))) w(3,1) = 2.0 * w(3,2) - w(3,3)
	END IF
C*
	IF  ( ill .ne. isize -1 )  THEN
	    IF ( ERMISS (w(4,2))) w(4,2) = 2.0 * w(3,2) - w(2,2)
	    IF ( ERMISS (w(4,3))) w(4,3) = 2.0 * w(3,3) - w(2,3)
	END IF
C
C*	Now, interpolate the same points if they are at the edges
C*	of the grid.
C
	IF  ( jll .eq. jsize - 1 )  THEN
	    w (2,4) = w (2,1) + 3.0 * ( w(2,3)-w(2,2))
	    w (3,4) = w (3,1) + 3.0 * ( w(3,3)-w(3,2))
	END IF
C*
	IF  ( jll .eq. 1 )  THEN
	    w (2,1) = w (2,4) + 3.0 * ( w(2,2)-w(2,3))
	    w (3,1) = w (3,4) + 3.0 * ( w(3,2)-w(3,3))
	END IF
C*
	IF  ( ill .eq. isize - 1 )  THEN
	    w (4,2) = w (1,2) + 3.0 * ( w(3,2)-w(2,2))
	    w (4,3) = w (1,3) + 3.0 * ( w(3,3)-w(2,3))
	END IF
C*
	IF  ( ill .eq. 1 )  THEN
	    w (1,2) = w (4,2) + 3. * ( w(2,2)-w(3,2))
	    w (1,3) = w (4,3) + 3. * ( w(2,3)-w(3,3))
	END IF
C
C*	Finally, get the corners of the subgrid, i.e. w (1,1),
C*	w (1,4), w (4,1), and w (4,4).
C
	IF  ( ERMISS ( w (1,1) ) )
     +	    w (1,1) = 2.0*(w(1,2)+w(2,3)+w(3,2)+w(2,1))
     +		      - (4.0*w(2,2)+w(3,3)+w(3,1)+w(1,3))
C*
	IF  ( ERMISS ( w (1,4) ) )
     +	    w (1,4) = 2.0*(w(1,3)+w(2,4)+w(3,3)+w(2,2))
     +		      - (4.0*w(2,3)+w(3,2)+w(1,2)+w(3,4))
C*
	IF  ( ERMISS ( w (4,4) ) )
     +	    w (4,4) = 2.0*(w(4,3)+w(3,2)+w(2,3)+w(3,4))
     +		      - (4.0*w(3,3)+w(2,2)+w(4,2)+w(2,4))
C*
	IF  ( ERMISS ( w (4,1) ) )
     +	    w (4,1) = 2.0*(w(3,1)+w(2,2)+w(3,3)+w(4,2))
     +		      - (4.0*w(3,2)+w(2,3)+w(2,1)+w(4,3))
C
C*	Next compute the coefficients.
C
	C33 = COMP36*(w(1,1)-w(1,4)-w(4,1)+w(4,4)+3.0*(w(1,3)-w(1,2) -
     +	      w(2,1)+w(2,4)+w(3,1)-w(3,4)+w(4,2)-w(4,3)) +
     +	      9.0*(w(2,2)-w(2,3)-w(3,2)+w(3,3)))
	C32 = COMP12*(3.*(w(2,1)-w(3,1))-w(1,1)+w(4,1)+2.0*(w(1,2)
     +	      +3.0*(w(3,2)-w(2,2))-w(4,2))-w(1,3)+w(4,3)+3.0*(w(2,3)
     +	      -w(3,3)))
	C23 = COMP12*(3.0*(w(1,2)-w(1,3))+w(1,4)-w(1,1)+2.0*
     +	      (3.0*(w(2,3)-
     +	      w(2,2))+w(2,1)-w(2,4))-(w(3,1)-3.0*(w(3,2)-w(3,3))
     +	      -w(3,4)))
	C22 = w(2,2)-0.5*(w(1,2)+w(2,1)+w(2,3)+w(3,2))+0.25*(w(1,1)
     +	      +w(3,3)+w(1,3)+w(3,1))
	C31 = COMP18*(w(1,1)-w(4,1)+3.0*(w(3,1)+w(4,3)-w(1,3)-w(2,1))
     +	      +9.0*(w(2,3)-w(3,3)))+COMP36*(w(1,4)-w(4,4)+3.0*(w(1,2)
     +	      -w(4,2)-w(2,4)+w(3,4))+9.0*(w(3,2)-w(2,2)))
	C13 = COMP18*(w(1,1)-w(1,4)+3.0*(w(1,3)-w(1,2)-w(3,1)+w(3,4))
     +	      +9.0*(w(3,2)-w(3,3)))+COMP36*(w(4,1)-w(4,4)+3.0*(w(2,1)
     +	      -w(2,4)-w(4,2)+w(4,3))+9.0*(w(2,3)-w(2,2)))
	C21 = COMP12*(2.0*(w(2,4)-w(3,1)-w(1,1)+2.0*w(2,1))+3.*
     +	      (2.0*(w(1,3)+w(2,2)+w(3,3))-w(3,2)-w(1,2))-w(1,4)
     +	      -w(3,4))-w(2,3)
	C12 = 0.5*(w(2,2)+w(3,1)+w(3,3))-w(3,2)-COMP12*(w(4,1)+w(4,3))
     +	      +COMP6*(w(4,2)+2.0*w(1,2)-w(1,1)-w(1,3))-0.25*(w(2,1)+
     +	      w(2,3))
	C30 = COMP6*(w(4,2)-w(1,2)+3.0*(w(2,2)-w(3,2)))
	C03 = COMP6*(w(2,4)-w(2,1))+0.5*(w(2,2)-w(2,3))
	C11 = COMP9*w(1,1)+COMP6*(w(1,2)-w(4,3)-w(3,4)+w(2,1))-COMP3*
     +	      (w(3,1)+w(1,3))+COMP18*(w(1,4)+w(4,1))+0.25*w(2,2)
     +	      -0.5*(w(2,3)+w(3,2))+COMP12*(w(2,4)+w(4,2))+
     +	      COMP36*w(4,4)+w(3,3)
	C20 = 0.5*(w(1,2)+w(3,2))-w(2,2)
	C02 = 0.5*(w(2,1)+w(2,3))-w(2,2)
	C10 = w(3,2)-0.5*w(2,2)-COMP3*w(1,2)-COMP6*w(4,2)
	C01 = w(2,3)-0.5*w(2,2)-COMP3*w(2,1)-COMP6*w(2,4)
	C00 = w(2,2)
C*
	iret = 0
C*
	RETURN
	END
