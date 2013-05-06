	SUBROUTINE GD_RDAT  ( iacss, gdattm, level, ivcord, parm, 
     +			      grid, igx, igy, ighdr, iret )
C************************************************************************
C* GD_RDAT								*
C*									*
C* This subroutine reads the requested grid from a grid file.		*
C*									*
C* GD_RDAT  ( IACSS, GDATTM, LEVEL, IVCORD, PARM, GRID, IGX, IGY,	*
C*            IGHDR,  IRET )						*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C*					-12 = grid does not exist	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* M. desJardins/NMC	 3/94	Dont check levels,vcoord if vc is blank *
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (2), parm
	INTEGER		level (2), ighdr (*)
	REAL		grid (*)
C*
	INTEGER		ihdarr (10), keyloc (10), nvcarr (7),nvcloc (7)
C*
	DATA		keyloc / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
	DATA		nvcloc / 1, 2, 3, 4, 8, 9, 10 /
C------------------------------------------------------------------------
        iret = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Convert grid identifier to a column header.
C
	CALL GD_ITOH  ( gdattm, level, ivcord, parm, ihdarr, ier )
C
C*	Search for grid with this header.  Do not check vertical
C*	coordinate or level if ivcord is -1.
C
        IF  ( ivcord .lt. 0 )  THEN
           DO  i = 1, 7
              nvcarr (i) = ihdarr ( nvcloc (i) )
           END DO
           CALL DM_SRCH (igdfln, 'COL', 7, nvcloc, nvcarr, icol, ier)
        ELSE
           CALL DM_SRCH (igdfln, 'COL', 10, keyloc, ihdarr, icol, ier)
        END IF
C
C*	Check to see if the grid was found.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Read in the data.
C
	CALL DM_RDTR  ( igdfln, 1, icol, 'GRID', kbfhdr, grid, ikxy,
     +			iret )
C
C*	Read kx and ky from grid header.
C
	IF  ( iret .eq. 0 )  THEN
	    igx = kbfhdr (1)
	    igy = kbfhdr (2)
	    DO  i = 1, khdrln ( igdfln )
		ighdr (i) = kbfhdr (i+2)
	    END DO
	  ELSE
	    iret = -6
	END IF
C*
	RETURN
	END
