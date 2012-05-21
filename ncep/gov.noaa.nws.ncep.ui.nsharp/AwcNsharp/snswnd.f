	SUBROUTINE SNSWND  ( wintyp, nstn, ipsdat, nlvls, 
     +			     stndat, idtype, sloc, ybot, ytop, filtfc, 
     +			     vcord, nvlvl, vlvl, ivcolr, vflag, iret )
C************************************************************************
C* SNSWND								*
C*									*
C* This subroutine draws the observed winds on a cross section.		*
C*									*
C* SNSWND  ( WINTYP, NSTN, IPSDAT, NLVLS, STNDAT, IDTYPE,		*
C*           SLOC, YBOT, YTOP, FILTFC, VCORD, NVLVL, VLVL, IVCOLR,	*
C*	     VFLAG, IRET )						*
C*									*
C* Input parameters:							*
C*	WINTYP		CHAR*		Wind type			*
C*	NSTN		INTEGER		Number of stations		*
C*	IPSDAT (*)	INTEGER		Pointers to station data	*
C*	NLVLS  (*)	INTEGER		Number of levels		*
C*	STNDAT (*)	REAL		Station data buffer		*
C*	IDTYPE (LLMXLV, INTEGER		Level data type			*
C*		NSTN)			  1 = mandatory			*
C*					  2 = significant temp		*
C*					  3 = significant wind		*
C*	SLOC   (*)	REAL		Grid locations on x axis	*
C*	YBOT		REAL		Lower graph boundary in mb	*
C*	YTOP		REAL		Upper graph boundary in mb	*
C*	FILTFC		REAL		Wind filter factor		*
C*	VCORD		CHAR*		Vertical coordinate		*
C*	NVLVL		INTEGER		Number of wind colors		*
C*	VLVL   (*)	REAL		Wind speed range		*
C*	IVCOLR (*)	INTEGER		Wind colors			*
C*	VFLAG		LOGICAL		Flag to do color coded winds	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-18 = wind data not in file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/86						*
C* G. Huffman/GSC	11/88	GEMPAK4.1; doc; variable y axis		*
C* M. desJardins/GSFC	 1/90	Set color for winds			*
C* S. Schotz/GSC	 8/90	Plot winds at sig wind levels only	*
C* M. desJardins/GSFC	 9/90	Changes for GEMPAK 5			*
C* J. Nielsen/SUNYA	 2/91	Allow any vertical coordinate		*
C* J. Whistler/SSAI	 2/91	Separated the data check into SNSWWE	*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* J. Whistler/AWC	11/95	Added in color coded winds		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wintyp, vcord
	REAL		stndat (*), sloc (*), vlvl (*)
	INTEGER		ipsdat (*), nlvls (*), idtype (LLMXLV, * ),
     +			ivcolr (*)
	LOGICAL		vflag
C*
	CHARACTER	cdata (3)*8
	REAL		data (3), p (2000), d (2000), s (2000),
     +			xx (2000), vx (2000), vy (2000)
	LOGICAL		draw, found
	REAL            yold
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	yold = 0.0
C
C*	Get barb size and calculate filter value
C
	CALL GQSYSZ  ( rmkw, rmkh, rtxw, rtxh, brbx, brby, ier )
	brbftr = .6 * brbx * filtfc
C
C*	Extract data for each station.  Don't keep levels which are
C*	outside the plot area; this allows plotting in V coord.,
C*	with barbs extending outside the plot region.  Levels with
C*	missing data are also ignored.  Only winds at mandatory and
C*	significant wind levels are drawn.
C
	DO  i = 1, nstn
	    nlv = 0
	    CALL PC_SSTN  ( '    ',0,0,0,0,0,IMISSD, nlvls (i), iret )
	    DO  j = 1, nlvls (i)
		CALL PC_CMLV (j, stndat (ipsdat (i)), data, cdata, iret)
		data(1) = data(1) * 3.28
		IF  ( ( .not. ERMISS (data (1)) ) .and.
     *		      ( .not. ERMISS (data (2)) ) .and.
     *		      ( .not. ERMISS (data (3)) ) .and.
     *                ( idtype ( j, i ) .ne. 2  ) .and. 
     *		      ( ( ( vcord .eq. 'PRES' )   .and.
     *		          ( data (1) .le. ybot )  .and.
     *			  ( data (1) .ge. ytop ) ) .or.
     *			( ( vcord .ne. 'PRES' )   .and.
     *			  ( data (1) .ge. ybot )  .and.
     *			  ( data (1) .le. ytop ) ) ) ) THEN
		    nlv = nlv + 1
		    p (nlv) = data (1)
		    d (nlv) = data (2)
		    s (nlv) = data (3)
		    xx (nlv) = sloc (i)
		END IF
	    END DO
C
C*	    Transform pressures and x locations to V coordinates.
C
	    CALL GTRANS  ( 'M', 'V', nlv, xx, p, vx, vy, iret )
C
C*	    Loop though the winds, setting the colors if needed.
C
	    knt = 0
	    DO m = 1, nlv
C*
	      IF ( vflag ) THEN
		found = .false.
		j = 1
		DO WHILE ( j .le. nvlvl  .and. .not. found ) 
		  IF ( s(m) .le. vlvl (j) ) found = .true.
		  j = j + 1
		END DO
		IF ( found ) THEN
		  IF ( ivcolr(j-1) .ne. 0 ) THEN
		      CALL GSCOLR ( ivcolr(j-1), ier )
		      draw = .true.
		  ELSE
		      draw = .false.
		  END IF
		ELSE 
		  draw = .false.
		END IF
	      ELSE
		draw = .true.
	      END IF
	      IF ( draw ) THEN
	       knt = knt + 1
C
C*	       If wind arrows, draw them.
C
	       IF  ( wintyp .eq. 'A' )  THEN
		CALL GARRW   ( 'V', 1, vx(m), vy(m), s(m), d(m), iret )
	       ELSE
C
C*		If no filter then draw the barbs.
C
		IF  ( filtfc .eq. 0. )  THEN
		    CALL GBARB  ( 'V', 1, vx(m), vy(m), s(m), d(m), 
     + 				  ier )
		  ELSE
C
C*		    Draw the first barb
C
		    IF ( knt .eq. 1 ) THEN
		      yold = vy (m)
		      CALL GBARB  ( 'V', 1, vx(m), vy(m), s(m), d(m),
     +				    iret )
		    END IF
C
C*		    Check for overlap and draw if ok.
C
		    IF  ( vy (m) - yold .ge. brbftr )  THEN
		        CALL GBARB  ( 'V', 1, vx (m), vy (m), 
     +				      s (m), d (m), iret )
			yold = vy (m)
		    END IF
		END IF
	       END IF
	      END IF
	    END DO
	END DO
C*
	RETURN
	END
