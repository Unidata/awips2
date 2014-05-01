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
C************************************************************************
	CHARACTER*(*)	map, garea, proj, latlon, panel, text,
     +			title, shrttl
        CHARACTER       garut*132, prjut*132
	LOGICAL		clear
C*
	CHARACTER	satfil*132
	LOGICAL		drpflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the map file.
C
C--	CALL GSMFIL ( 'mepowo.gsf', ier )
	CALL GSMFIL ( 'hipowo.cia', ier )
	IF  ( ier .ne. 0 )  CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
C
C*      Set the projection.
C
        CALL DG_FIXA ( garea, proj, garut, prjut, ier)
	CALL GG_MAPS ( proj, garea, satfil, drpflg, iret )
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
	IF  ( drpflg )  CALL GSATIM ( satfil, ier )
C
C*	Draw map and lat/lon lines.
C
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
C       ADD INTNL BNDRYS MAP (LJH)
C        CALL GSMFIL ( 'hifir.awc', ier)
        CALL GSMFIL ( 'intlbnd.gsf', ier)
       	IF  ( ier .ne. 0 )  CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
C
C*      Set the projection.
C
	CALL GG_MAPS ( proj, garea, satfil, drpflg, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the text attributes.
C
	CALL IN_TEXT ( text, ier )
C
C*	Draw map and lat/lon lines.
C
        map="21"
	CALL GG_MAP  ( map, ier )
C
C*      Flush the buffers.
C
	CALL GEPLOT ( ier )
        
	RETURN
	END
