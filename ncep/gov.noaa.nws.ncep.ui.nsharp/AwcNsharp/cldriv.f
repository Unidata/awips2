	SUBROUTINE CLDRIV  ( nlvl, clvl, clbl, lincol, lintyp, linwid, 
     +			     linlbl, scflag, iret )
C************************************************************************
C* CLDRIV								*
C*									*
C* This subroutine draws contours through a grid stored in common.	*
C*									*
C* CLDRIV  ( NLVL, CLVL, CLBL, LINCOL, LINTYP, LINWID, LINLBL, 		*
C*	     SCFLAG, IRET )						*
C*									*
C* Input parameters:							*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour levels			*
C*	CLBL   (NLVL)	CHAR*		Contour labels			*
C*	LINCOL (NLVL)	INTEGER		Contour colors			*
C*	LINTYP (NLVL)	INTEGER		Contour line types		*
C*	LINWID (NLVL)	INTEGER		Contour line widths		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*	SCFLAG		LOGICAL		Small contour suppress flag	*
C* Output parameters:							*
C*	IRET		INTEGER		Statrus				*
C**									*
C* Log:									*
C* WOLFPLOT			Original code for SACC			*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* I. Graffman/RDS	 6/86	Added line widths			*
C* M. desJardins/GSFC	 1/88	Made max grid isize * jsize		*
C* M. desJardins/GSFC	 6/89	Rewritten for GEMPAK 5			*
C* M. desJardins/GSFC	 8/89	Set hardware functions to no change	*
C* K. Brill/GSC          4/90   Change DO loops & check CDRAWC IRET	*
C* J. Whistler/SSAI	 6/91	Set internal grids to size LLMXDG	*
C* M. desJardins/NMC	11/91	Renamed from CONTSK			*
C* G. Krueger/EAI	12/94	CCGTBX call -> CCCRSS			*
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	REAL		clvl (*)
	INTEGER		lincol (*), lintyp (*), linwid (*), linlbl (*)
	CHARACTER*(*)	clbl (*)
	LOGICAL		scflag
C*
	CHARACTER*24	clabel
	LOGICAL		intror
C------------------------------------------------------------------------
	iret = 0
C
C*	Save current dashing status and color.
C
	CALL GQCOLR  ( isvcol, iret )
	CALL GQLINE  ( isltyp, islhw, islwid, islwhw, iret )
C
C*	Loop through each contour value.
C
	DO  jcntr = 1, nlvl
C
C*	    Initialize lines crossed.
C
	    DO  jj = 1, jsize
		DO  ii = 1, isize 
		    ihline ( (jj-1)*isize+ii ) = 0
		    ivline ( (jj-1)*isize+ii ) = 0
		END DO
	    END DO
C
C*	    Get variables for this plot.
C
	    cval   = clvl   (jcntr)
	    ilabel = linlbl (jcntr)
	    clabel = clbl   (jcntr)
	    itop   = isize - 1
	    jtop = jsize - 1
C
C*	    Set characteristics for this line.
C
	    iltyp  = lintyp (jcntr)
	    icolor = lincol (jcntr)
	    icwid  = linwid (jcntr)
	    CALL GSLINE  ( iltyp, 0, icwid, 0, iret )
	    IF ( icolor .ne. 0 )  THEN
		CALL GSCOLR ( icolor, iret )
C
C*		Check for contour line passing through each grid square.
C*		Search across the top.
C
		intror = .false.
		j = jsize - 1
		i = 0
		DO WHILE ( i .lt. itop )
		    i = i + 1
		    idire = 3
		    IF ( ihline ( (j)*isize+i ) .eq. 0 ) THEN
			CALL CCCRSS  ( 2, 3, i, j, cval, iret )
			IF  ( iret .eq. -6 ) THEN
			    CALL CCDRAW ( cval, clabel, ilabel, 
     +					  intror, i, j, idire, 
     +					  scflag, iret )
			    IF ( iret .ne. 0 ) THEN
				CALL ER_WMSG ( 'GEMPLT', NCTRLP, ' ',
     +					       ier )
			    END IF
			END IF
		    END IF
		END DO
C
C*		Search right edge of frame.
C
		i = isize - 1
		j = jtop + 1
		DO WHILE ( j .gt. 1 )
		    j = j - 1
		    idire = 4
		    IF ( ivline ( (j-1)*isize+i+1 ) .eq. 0 ) THEN
			CALL CCCRSS  ( 3, 4, i, j, cval, iret )
			IF  ( iret .eq. -6 ) THEN
			    CALL CCDRAW ( cval, clabel, ilabel, 
     +					  intror, i, j, idire, 
     +					  scflag, iret )
			    IF ( iret .ne. 0 ) THEN
				CALL ER_WMSG ( 'GEMPLT', NCTRLP, ' ',
     +					       ier )
			    END IF
			END IF
		    END IF
		END DO
C
C*		Search bottom of frame.
C
		j = 1
		i = 0
		DO WHILE ( i .lt. itop )
		    i = i + 1
		    idire = 1
		    IF ( ihline ( (j-1)*isize+i ) .eq. 0 ) THEN
			CALL CCCRSS  ( 1, 4, i, j, cval, iret )
			IF  ( iret .eq. -6 ) THEN
			    CALL CCDRAW ( cval, clabel, ilabel, 
     +					  intror, i, j, idire, 
     +					  scflag, iret )
			    IF ( iret .ne. 0 ) THEN
				CALL ER_WMSG ( 'GEMPLT', NCTRLP, ' ',
     +					       ier )
			    END IF
			END IF
		    END IF
		END DO
C
C*		Search left edge of frame.
C
		i = 1
		j = jtop + 1
		DO WHILE ( j .gt. 1 )
		    j = j - 1
		    idire = 2
		    IF ( ivline ( (j-1)*isize+i ) .eq. 0 ) THEN
			CALL CCCRSS  ( 1, 2, i, j, cval, iret )
			IF  ( iret .eq. -6 ) THEN
			    CALL CCDRAW ( cval, clabel, ilabel, 
     +					  intror, i, j, idire, 
     +					  scflag, iret )
			    IF ( iret .ne. 0 ) THEN
				CALL ER_WMSG ( 'GEMPLT', NCTRLP, ' ',
     +					       ier )
			    END IF
			END IF
		    END IF
		END DO
C
C*		Search for interior contours.
C
		intror = .true.
		jend   = 1
		ibgin  = 2
		j = jtop + 1

		DO WHILE ( j .gt. jend )
		    j = j - 1
		    i = ibgin - 1
		    DO WHILE ( i .lt. itop )
			i = i + 1
			idire = 2
			IF ( ivline ( (j-1)*isize+i ) .eq. 0 ) THEN
			    CALL CCCRSS  ( 1, 2, i, j, cval, iret )
			    IF ( iret .eq. -6 ) THEN
				CALL CCDRAW  ( cval, clabel, ilabel, 
     +					       intror, i, j, idire, 
     +					       scflag, iret )
				IF ( iret .ne. 0 ) THEN
				    CALL ER_WMSG ( 'GEMPLT', NCTRLP,
     +						   ' ', ier )
				END IF
			    END IF
			END IF
		    END DO
		END DO
	    END IF
	END DO
C
C*	Restore dashing and color setup.
C
	CALL GSCOLR  ( isvcol, iret )
	CALL GSLINE  ( isltyp, 0, islwid, 0, iret )
C*
	RETURN
	END
