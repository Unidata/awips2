	SUBROUTINE FFPLOT  ( npt, xpt, ypt, linflg, icolor, iltype,
     +			     ilwdth, iret )
C************************************************************************
C* FFPLOT								*
C*									*
C* This subroutine plots a polygon for the contour fill routine.  If	*
C* linflg is set, the polygon is drawn as a series of lines.  Otherwise,*
C* the polygon is filled.						*
C*									*
C* FFPLOT  ( NPT, XPT, YPT, LINFLG, ICOLOR, ILTYPE, ILWDTH, IRET )	*
C*									*
C* Input parameters:							*
C*	NPT		INTEGER		Number of points in polygon	*
C*	XPT (NPT)	REAL		X coordinates			*
C*	YPT (NPT)	REAL		Y coordinates			*
C*	LINFLG		LOGICAL		Line flag			*
C*	ICOLOR		INTEGER		Color				*
C*	ILTYPE		INTEGER		Line type			*
C*	ILWDTH		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91				 		*
C* S. Jacobs/EAI	12/92	Added test for GSLINE			*
C* T. Lee/SAIC		10/01	Set contour fill types			*
C************************************************************************
	REAL		xpt (*), ypt (*)
	LOGICAL		linflg
C*
	REAL		xout (250), yout (250)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that there are enough points.
C
	IF  ( npt .le. 2 )  RETURN
C
C*	If this is color 0, there is no plotting to be done.
C
	IF  ( icolor .le. 0 )  RETURN
C
C*	Set color and line type.
C
	CALL GSCOLR  ( icolor, ier )
	IF  ( linflg )  CALL GSLINE  ( iltype, 0, ilwdth, 0, ier )
C
C*	Translate coordinates to actual grid coordinates.
C
	CALL CTRANG  ( npt, xpt, ypt, xout, yout, ier )
C
C*	Draw lines or filled polygons.
C
	IF  ( linflg )  THEN
	    mpt = npt + 1
	    xout (mpt) = xout (1)
	    yout (mpt) = yout (1)
	    CALL GLINE  ( 'G', mpt, xout, yout, ier )
	  ELSE
	    IF  ( iltype .le. 0 )  THEN
		CALL GSFILL ( 1., 1, ier )
	      ELSE
		CALL GSFILL ( 1., iltype, ier )
	    END IF
	    CALL GFILL  ( 'G', npt, xout, yout, ier )
	END IF
C*
	RETURN
	END
