	SUBROUTINE MAP_DRAW ( map, garea, proj, latlon, panel, text,
     +			      title, ititl, linttl, shrttl,
     +			      clear, iret )
C************************************************************************
C* MAP_DRAW								*
C*									*
C* This subroutine draws a map, lat/lon lines, and a title.		*
C*									*
C* MAP_DRAW ( MAP, GAREA, PROJ, LATLON, PANEL, TEXT, TITLE, 		*
C* 	      ITITL, LINTTL, SHRTTL, CLEAR, IRET )			*
C*									*
C* Input parameters:							*
C*	MAP		CHAR*	 	Map Color			*
C*	GAREA   	CHAR*	 	Graphics area			*
C*	PROJ		CHAR*	 	Map projection	name		*
C*	LATLON		CHAR*	 	Line color			*
C*	PANEL		CHAR*	 	Panel location			*
C*	TEXT		CHAR*	 	Text input			*
C*	TITLE		CHAR*		Title string			*
C*	ITITL		INTEGER		Title color			*
C*	LINTTL		INTEGER		Title line			*
C*	SHRTTL		CHAR*		Short title string		*
C*	CLEAR		LOGICAL	 	Clear screen flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* L. Williams/EAI	 4/94	Converted to a subroutine from GPMAP	*
C* S. Jacobs/NMC	 7/94	Copied for use with the NWX program	*
C* T. Piper/SAIC	 1/02	Initialized satfil			*	
C* S. Chiswell/Unidata	11/02	Fixed GG_MAPS call sequence		*
C************************************************************************
	CHARACTER*(*)	map, garea, proj, latlon, panel, text,
     +			title, shrttl
	LOGICAL		clear
C*
	CHARACTER	satfil*132
C------------------------------------------------------------------------
	iret = 0
	satfil = ' '
C
C*	Set the map file.
C
C--	CALL GSMFIL ( 'mepowo.gsf', ier )
c	CALL GSMFIL ( 'hipowo.cia', ier )
c	IF  ( ier .ne. 0 )  CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
C
C*      Set the projection.
C
	CALL GG_MAPS ( proj, garea, satfil, idrpflg, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the text attributes.
C
	CALL IN_TEXT ( text, ier )
C
C*	Clear the screen and write the short title.
C
	IF  ( clear )  CALL GCLEAR ( ier )
	IF  ( clear )  CALL GMESG ( shrttl, ier )
	CALL GG_PANL ( panel, ier )
C
C*	Display satellite image, if desired.
C
	IF  ( idrpflg .eq. 1 ) CALL IM_DROP ( ier )
C
C*	Draw map and lat/lon lines.
C
	CALL IP_PUTV  ( '$MAPFIL', 'base', ier )
	CALL GG_MAP  ( map, ier )
	CALL GG_LTLN ( latlon, ier )
C
C*	Draw the title.
C
	CALL GSCOLR  ( ititl, ier )
	CALL GG_WSTR ( title, linttl, ier )
C
C*      Flush the buffers.
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
