#include "gui.h"


/************************************************************************
 * draw.c                                                               *
 *                                                                      *
 * This module contains the drawing functions.         			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      draw_map()        set the map attributes and draw the map.	*
 *      draw_stnmark()    draw station marker at station location.     	*
 *      draw_value()      plot values at station location .		*
 *      draw_cntr()       plot contouring lines.     			*
 *      draw_wbox()       draw watch box.            			*
 ***********************************************************************/

/*=====================================================================*/

void draw_map ( int num, struct maptype_list *map_info, int zoomflg, 
						mapbnd_t *mapb, int *iret )
/************************************************************************
 * draw_map								*
 *									*
 * This routine will set the map attributes and draw the map.		*
 *									*
 * draw_map ( num, map_info, zoomflg, mapb, iret )			*
 *									*
 * Input parameters:							*
 *	num		int		Index to map info array		*
 *	*map_info	struct maptype_list	Map projection info	*
 *	zoomflg		int		Zoom box flag			*
 *									*
 * Input/Output parameters:						*
 *	*mapb		mapbnd_t	Map boundary coordinates	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI	 	 8/95	add compute xin, yin from garea		*
 * C. Lin/EAI		 8/95   modified from nwx2.1			*
 * C. Lin/EAI		 2/97   use gqmprj				*
 * S. Jacobs/NCEP	11/97	Changed sizeof(savprj) to sizeof(prjtmp)*
 * I. Durham/GSC	 5/98   changed call for underscore		*
 * T. Lee/GSC		 1/01	checked CED/MER projection		*
 ***********************************************************************/
{
char	map[25];
char	proj[25];
char	garea[128];
char	latlon[25];
char	panel[13];
char	text[13];
char	shrttl[9];
char	title[73];
int	ititl;
int	linttl;
int	clear;

char    gflag;
float   angle[3];
int     len;
char    prjtmp[40]="\0";

static  char	savprj[128]; /* save the previous projection info */
/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	/*
 	 * Set the map attributes.
 	 */
	strcpy( map,    "31" );
	strcpy( latlon, "6/20/2" );
	strcpy( panel,  "0" );
	strcpy( text,   "1/22//hw" );
	strcpy( shrttl, " " );
	strcpy( title,  " " );
	ititl  =  1;
	linttl = -2;
	clear  = G_TRUE;

	/*
 	 * Check for a menu selection or a zoom box for garea.
 	 */
	gflag = 0;
	if  ( zoomflg ) {
	    sprintf( garea, "%f;%f;%f;%f",
		      mapb->x[0], mapb->y[0], mapb->x[1], mapb->y[1] );
	    strcpy( proj,   savprj );
	}
	else {

	    strcpy( garea, map_info[num].garea );
	    strcpy( proj,  map_info[num].proj  );

	    gflag = 1;
	    

	}

	/*
 	 * Draw the map.
 	 */
	map_draw( map, garea, proj, latlon, panel, text, title, &ititl,
		   &linttl, shrttl, &clear, iret,
		   strlen(map), strlen(garea), strlen(proj),
		   strlen(latlon), strlen(panel), strlen(text),
		   strlen(title), strlen(shrttl) );

	if ( gflag ) {

	    gqmprj( prjtmp, &angle[0], &angle[1], &angle[2],
				&mapb->x[0], &mapb->y[0], 
				&mapb->x[1], &mapb->y[1],
				iret, sizeof(prjtmp));
	    /* 
	     * remove trailing blank
	     */
	    prjtmp[39] = '\0';
	    cst_lstr(prjtmp, &len, iret);
	    prjtmp[len] = '\0';

	    if  ( strcmp ( prjtmp, "CED" ) == 0 ||
		  strcmp ( prjtmp, "MER" ) == 0 ||
		  strcmp ( prjtmp, "MCD" ) == 0 ) {
	        sprintf(savprj, "%s", prjtmp );
	    }
	    else {
	        sprintf(savprj, "%s/%5.2f;%5.2f;%5.2f", prjtmp, angle[0],
				angle[1], angle[2]);
	    }

	}

}

/*=====================================================================*/

void draw_stnmark ( int numpts, float *slat, float *slon, int jcolr, 
						int imrk, int *iret )
/************************************************************************
 * draw_stnmark								*
 *									*
 * This routine will set the marker attributes and plot the markers	*
 * at the station locations.						*
 *									*
 * draw_stnmark ( numpts, slat, slon, jcolr, imrk, iret )		*
 *									*
 * Input parameters:							*
 *	numpts		int		Number of stations		*
 *	*slat		float		Array of latitudes		*
 *	*slon		float		Array of longitudes		*
 *	jcolr		int		Requested marker color		*
 *	imrk		int		Requested marker type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 ***********************************************************************/
{
float  sizmrk;
int    ncolor, mrktyp, mrkwid, pltval, iposn;
	
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	/*
 	 * Set the marker attributes.
 	 */
	ncolor    = 1;
	mrktyp    = imrk;
	sizmrk    = 1.0F;
	mrkwid    = 2;
	pltval    = G_FALSE;
	iposn     =  0;

	/*
 	 * Draw the markers.
 	 */
	map_mark( &numpts, slat, slon, NULL, &ncolor, NULL, &jcolr,
		   &mrktyp, &sizmrk, &mrkwid, &pltval, &iposn, iret );

}

/*=====================================================================*/

void draw_value ( struct mrkv *markdata )
/************************************************************************
 * draw_value								*
 *									*
 * This routine will plot the markers based on the input data.		*
 *									*
 * draw_value ( markdata )						*
 *									*
 * Input parameters:							*
 *	*markdata	struct mrkv	structure for marker data	*
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * C. Lin/EAI	         9/95						*
 ***********************************************************************/
{
int iret;

        map_mark( &(markdata->nstn), markdata->lat, markdata->lon,
                        markdata->dvalues, &(markdata->ncolor),
                        markdata->breaks, markdata->icolrs, 
                        &(markdata->marktype), &(markdata->marksize),
                        &(markdata->markwdth), &(markdata->pltflag),
                        &(markdata->iposn),
                        &iret );
}

/*=====================================================================*/

void draw_cntr ( struct contour *contours )
/************************************************************************
 * draw_cntr 								*
 *									*
 * This routine plots the contour lines.				*
 *									*
 * draw_cntr(contours)							*
 *									*
 * Input parameters:							*
 *	*contours       struct contour	struct of contour data		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	01/95						*
 * C. Lin/EAI	        09/95						*
 * S. Jacobs/NCE	 8/98	Changed gtext to gtextc			*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
int             ltyhw, ltype, lwidth;
int             offset, j;
float           rotat;
int             iret;

/*---------------------------------------------------------------------*/

	if ( contours->nc == 0 ) 
		return;

	ltyhw  = 0;
	ltype  = 1;
	lwidth = 1;
	rotat  = 0.0F;
	offset = 0;

	/*
	 * set the line type, width... attributes
	 */
	gsline(&ltype, &ltyhw, &lwidth, &ltyhw, &iret);

	for ( j=0; j < contours->nc; ++j ) {

	      /*
	       * set line color
	       */	
	      gscolr(&(contours->line[j].color), &iret);

	      /*
	       * draw the line
	       */
	      gline(sys_M, &(contours->line[j].npt), 
			 contours->line[j].lat, 
			 contours->line[j].lon,
		         &iret, strlen(sys_M));

	      /*
	       * plot contour label
	       */
	      gtextc(sys_M, &(contours->line[j].lat[0]), 
			 &(contours->line[j].lon[0]),
		    	 contours->line[j].label, 
			 &rotat, &offset, &offset, &iret,
		    	 strlen(sys_M), strlen(contours->line[j].label));

	}

	/*
	 * flush the buffer
	 */
	geplot(&iret );

}

/*=====================================================================*/

void draw_wbox ( struct watchbox *wtchbox, int called_from )
/************************************************************************
 * draw_wbox 								*
 *									*
 * This routine draws a watch box.                                      *
 *									*
 * draw_wbox(wtchbox, called_from)					*
 *									*
 * Input parameters:							*
 *	*wtchbox        struct watchbox	struct of watchbox data		*
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * D. Kidwell/NCEP       8/98	Adapted from draw_cntr			*
 * S. Jacobs/NCEP	 4/99	Changed cfdate to css_date		*
 * D. Kidwell/NCEP       4/99	Fixed for Y2K; added gg_wlbl call; fixed*
 *                              watch cancel check; gtextc -> gtext     *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * T. Piper/SAIC	 6/02	Added 'No watches' message		*
 * A. Hardy/NCEP	 6/03   Added tmzn to CSS_DATE			*
 * R. Tian/SAIC         11/03   Added called_from arg                   *
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{

	int		ltype, ltyhw, lwidth, lwdhw,
			ltxfn, ltxhw, ltxwid, lbrdr, lrrotn, ljust;
	float		sztext;

	int		jtype, jtyhw, jwidth, jwdhw,
			jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
	float		siztx;

	float		rotat, xlat, ylon;
	int		j, k, ixoff, iyoff, iret;
	char		wlabel[50];
/*	char		found;  */

	int		itarr[5], ietar[5],
			minute, itype, isec, jday, jcolr, imrk;
	char		endtim[21], endtm2[21], expr[21]="\0", exptim[21], 
			vald[21]="\0", valtim[21], tmzn[4];

/*---------------------------------------------------------------------*/

	if  ( wtchbox->nb == 0 ) return;

/*	found  = 0;  NOT used */
	ltype  = 1;
	ltyhw  = 0;
	lwidth = 3;
	lwdhw  = 0;

	ltxfn  = 21;
	ltxhw  = 2;
	ltxwid = 1;
	lbrdr  = 111;
	lrrotn = 1;
	ljust  = 1;
	sztext = 1.0F;

	rotat  = 0.0F;
	ixoff  = 0;
	iyoff  = -4;

	/*
	 * Get the current time.
	 */
	itype = 1;
	css_date ( &itype, &itarr[0], &itarr[1], &itarr[2],
		   &itarr[3], &itarr[4], &isec, &jday, tmzn, &iret );

	/*
	 * Add 15 minutes to the current time to account for
	 * differences in system clocks, and use 4-digit year.
	 */
	minute = 15;
	ti_addm ( itarr, &minute, ietar, &iret );
	ti_itoc ( ietar, endtm2, &iret, sizeof(endtm2) );
	endtm2[20] = '\0';
	ti_dtm4 ( endtm2, endtim, &iret, sizeof(endtm2),
	 	  sizeof(endtim) );
	endtim[20] = '\0';

	/*
	 * set the line type, width... attributes
	 */
	gqline ( &jtype, &jtyhw, &jwidth, &jwdhw, &iret );
	gsline ( &ltype, &ltyhw, &lwidth, &lwdhw, &iret );

	gqtext ( &jtxfn, &jtxhw, &siztx, &jtxwid,
		 &jbrdr, &jrrotn, &jjust, &iret );
	gstext ( &ltxfn, &ltxhw, &sztext, &ltxwid,
		 &lbrdr, &lrrotn, &ljust, &iret );

	k = 0;

	/*
	 * Loop over all watches.
	 */
	for ( j = 0; j < wtchbox->nb; j++ ) {

	    xlat = -9999.0F;
	    ylon = -9999.0F;

	    /*
	     * Check for current watches.  Use 4-digit year for
	     * comparison.
	     */
	    strcpy ( expr, wtchbox->winfo[j].expire );
	    expr[20] = '\0';
	    ti_dtm4 ( expr, exptim, &iret,
		      sizeof(expr), sizeof(exptim) );
	    exptim[20] = '\0';
	    strcpy ( vald, wtchbox->winfo[j].valid );
	    vald[20] = '\0';
	    ti_dtm4 ( vald, valtim, &iret,
		      sizeof(vald), sizeof(valtim) );
	    valtim[20] = '\0';
	    if  ( ( ( strcmp ( endtim, exptim ) <= 0 ) &&
	            ( strcmp ( endtim, valtim ) >= 0 ) ) ) {
/*		found = 1;  NOT used */
		/*
		 * Set line color.
		 */	
		gscolr ( &(wtchbox->winfo[j].color), &iret );

		/*
		 * Draw the box.
		 */
		gline ( sys_M, &(wtchbox->winfo[j].npt), 
			     wtchbox->winfo[j].lat, 
			     wtchbox->winfo[j].lon,
			     &iret, strlen(sys_M) );

		/*
		 * Find the lower left corner for plotting the label.
		 */
		gg_wlbl ( &(wtchbox->winfo[j].npt),
                          wtchbox->winfo[j].lat,
                          wtchbox->winfo[j].lon,
			  &xlat, &ylon, &iret );

		/*
		 * Plot watch number, start and stop times
		 */
		wlabel[0] = '\0';

		strncpy ( wlabel, wtchbox->winfo[j].wtchnum, 4 );
		wlabel[4] = '\n';
		wlabel[5] = '\0';

		strncat ( wlabel, wtchbox->winfo[j].valid, 11 );
		wlabel[16] = '\n';
		wlabel[17] = '\0';

		strncat ( wlabel, wtchbox->winfo[j].expire, 11 );
		wlabel[28] = '\0';

		gtext ( sys_M, &xlat, &ylon, wlabel, &rotat,
			 &ixoff, &iyoff, &iret,
			 strlen(sys_M), strlen(wlabel) );

		jcolr = ALL_COL;
		imrk  = ALL_MRK;
		draw_stnmark ( 1, &xlat, &ylon, jcolr, imrk, &iret );

	    }

	    stnList.lat[k] = xlat;
	    stnList.lon[k] = ylon;
	    k++;

	}
/*
	if ( found == 0 && called_from == 1 ) {
	    XmTextSetString( textW, "No current watches found.");
	}
*/
	stnList.nstn = k;

	/*
	 * Reset the line and text attributes, and flush the graphics.
	 */
	gsline ( &jtype, &jtyhw, &jwidth, &jwdhw, &iret );

	gstext ( &jtxfn, &jtxhw, &siztx, &jtxwid,
		 &jbrdr, &jrrotn, &jjust, &iret );

	geplot ( &iret );

}

/*=====================================================================*/
