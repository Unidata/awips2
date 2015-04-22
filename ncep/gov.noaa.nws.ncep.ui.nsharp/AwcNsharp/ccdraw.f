	SUBROUTINE CCDRAW  ( cval, clabel, ilabel, intror, ii, jj, 
     +			     idire, scflag, iret )
C************************************************************************
C* CCDRAW								*
C*									*
C* This subroutine draws contours.					*
C*									*
C* CCDRAW  ( CVAL, CLABEL, ILABEL, INTROR, II, JJ, IDIRE, SCFLAG,	* 
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	CVAL		REAL		Contour level			*
C*	CLABEL		CHAR*		Contour label			*
C*	ILABEL		INTEGER		Label type			*
C*	INTROR		LOGICAL		Interior start flag		*
C*	II		INTEGER		Lower left column of box	*
C*	JJ		INTEGER		Lower left row of box		*
C*	IDIRE		INTEGER		Direction entering box		*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C* 					 -5 = TROUBLE			*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* M. desJardins/GSFC	 1/88	Made max grid 125 * 125			*
C* M. desJardins/GSFC	 6/89	GEMPAK 5 changes			*
C* K. Brill/GSC          4/90   Put 2048 limit on DO WHILE loop		*
C* K. Brill/GSC          4/90   Renamed return code from CBOXIT		*
C* M. desJardins/NMC	12/91	Renamed: CDRAWC-->CCDRAW		*
C* C. Bailey/HPC	 6/06	Added contour label string		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	CHARACTER*24	clabel(LLCLEV)
	LOGICAL		intror, scflag
C*
	LOGICAL		start, edge, closed, revers
C------------------------------------------------------------------------
	iret   = 0
	jlabel = ilabel
C
C*	Initialize variables.
C
	number = 0
	ill    = ii
	jll    = jj
	idirc  = idire
	revers = .false.
	edge   = .false.
	closed = .false.
	start  = .true.
C
C*	Save initial points.
C
	ills  = ill
	jlls  = jll
	idirs = idirc
C
C*	Draw grid till the edge is reached.
C
	knt = 0
	DO WHILE  ( ( .not. edge ) .and. ( .not. closed ) )
	    knt = knt + 1
	    IF ( knt .gt. 2048 ) THEN
	      iret = - 5
              RETURN
            END IF
C*
	    CALL CCBOXT  ( cval, clabel, jlabel, start, ill, jll, 
     +			   idirc, scflag, iretn )
	    IF  ( start )  THEN
		x1save = xval (1)
		y1save = yval (1)
		start  = .false.
	    END IF
C
C*	    Check for closed curve.  In this case, set the last point
C*	    to be the same as the first point to ensure closure.
C
	    closed = ( idirc .eq. idirs ) .and. ( ills .eq. ill ) .and.
     +		     ( jlls .eq. jll )
	    IF  ( closed )  THEN
		number = number + 1
		xval ( number ) = x1save
		yval ( number ) = y1save
	    END IF
C
C*	    If an edge has been reached and the curve is not closed,
C*	    reverse directions and start again.
C
	    IF  ( ( iretn .eq. -3 ) .or. ( iretn .eq. -5 ) )
     +                                                edge = .true.
	    IF  ( edge .and. ( .not. revers ) .and. intror )  THEN
		revers = .true.
		idirc  = 4
		ill    = ills - 1
		jll    = jlls
		edge   = .false.
		CALL CCPLOT  ( cval, clabel, jlabel, scflag, ier )
		jlabel = 0
		number = 1
		xval (1) = x1save
		yval (1) = y1save
	    END IF
	END DO
C
C*	Contour has reached an edge so we are done.
C
	CALL CCPLOT  ( cval, clabel, ilabel, scflag, ier )
C*
	RETURN
	END
