	SUBROUTINE CLBBOX  ( xx1, yy1, xx2, yy2, x0, y0, xx, yy, iret )
C************************************************************************
C* CLBBOX								*
C*									*
C* This subroutine returns a point on the edge of a rectangle whose	*
C* vertices are given.  The point lies on the line from interior point  *
C* (x0, y0) to (xx, yy) outside the rectangle.  The point (xx, yy) is	*
C* effectively moved to the edge of the rectangle.			*
C*									*
C* Note:  It must be that xx1 < xx2 and yy1 < yy2.			*
C*									*
C* CLBBOX  ( XX1, YY1, XX2, YY2, X0, Y0, XX, YY, IRET )			*
C*									*
C* Input parameters:							*
C*	XX1		REAL		Lower left corner x coordinate  *
C*	YY1		REAL		Lower left corner y coordinate 	*
C*	XX2		REAL		Upper right corner x coordinate *
C*	YY2		REAL		Upper right corner y coordinate *
C*	X0		REAL		Interior point x coordinate	*
C*	Y0		REAL		Interior point y coordinate	*
C*									*
C* Input and Output parameters:						*
C*	XX		REAL		Input x coord. of exterior point*
C*					Output x coord. of edge point	*
C*	YY		REAL		Input y coord. of exterior point*
C*					Output y coord. of edge point	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC       	01/92						*
C* K. Brill/NMC		03/93	Treat cases of zero slopes		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Translate the origin to (x0, y0).
C
	x1 = xx1 - x0
	y1 = yy1 - y0
	x2 = xx2 - x0
	y2 = yy2 - y0
	x  = xx  - x0
	y  = yy  - y0
C
C*	Check special cases of zero slopes.
C
        IF ( x .eq. 0 ) THEN
            xx = x0
            IF ( y .ge. 0 ) THEN
                yy = yy2
            ELSE
                yy = yy1
            END IF
            RETURN
        END IF
        IF ( y .eq. 0 ) THEN
            yy = y0
            IF ( x .ge. 0 ) THEN
                xx = xx2
            ELSE
                xx = xx1
            END IF
            RETURN
        END IF
C
C*	Compute slopes of lines.
C
	ym = y / x
	xm = x / y
C
C*	Check each bounding edge for a crossing.
C
	IF ( x .lt. x1 ) THEN
	    yt = ym * x1
	    IF ( yt .ge. y1 .and. yt .le. y2 ) THEN
		xx = x1 + x0
		yy = yt + y0
		RETURN
	    END IF
	END IF
	IF ( x .gt. x2 ) THEN
	    yt = ym * x2
	    IF ( yt .ge. y1 .and. yt .le. y2 ) THEN
		xx = x2 + x0
		yy = yt + y0
		RETURN
	    END IF
	END IF
	IF ( y .lt. y1 ) THEN
	    xt = xm * y1
	    IF ( xt .ge. x1 .and. xt .le. x2 ) THEN
		xx = xt + x0
		yy = y1 + y0
		RETURN
	    END IF
	END IF
	IF ( y .gt. y2 ) THEN
	    xt = xm * y2
	    IF ( xt .ge. x1 .and. xt .le. x2 ) THEN
		xx = xt + x0
		yy = y2 + y0
	    END IF
	END IF
	RETURN
	END
