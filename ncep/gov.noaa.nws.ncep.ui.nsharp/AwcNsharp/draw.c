/* L. Hinson/AWC     11/02  Corrected coding errors */
#include <string.h>
#include <strings.h>
#include "gui.h"

void draw_map();
void draw_stnmark();
void draw_value();
void draw_cntr();

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
 ***********************************************************************/

/*=====================================================================*/

void draw_map ( num, map_info, zoomflg, mapb, iret )
int			num;
struct maptype_list	*map_info;
int			zoomflg;
mapbnd_t		*mapb;
int			*iret;

/************************************************************************
 * draw_map								*
 *									*
 * This routine will set the map attributes and draw the map.		*
 *									*
 * draw_map ( num, map_info, zoomflg, mapb, iret )			*
 *									*
 * Input parameters:							*
 *	num		int		Index to map info array		*
 *	*map_info	struct		Map projection info		*
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
 ***********************************************************************/

{
char	map[25];
char	garea[73], tmpgarea[73], *ptr;
char	proj[25];
char	latlon[25];
char	panel[13];
char	text[13];
char	shrttl[9];
char	title[73];
int	ititl;
int	linttl;
int	clear;

int	np;

float   cx, cy, dx, dy;
	
static  char	savprj[25]; /* save the previous projection info */
/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	/*
 	 * Set the map attributes.
 	 */
	strcpy( map,    "24" );
	strcpy( latlon, "1/10" );
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
	if  ( zoomflg ) {
	    sprintf( garea, "%f;%f;%f;%f",
		      mapb->x[0], mapb->y[0], mapb->x[1], mapb->y[1] );
	    strcpy( proj,   savprj );
	}
	else {

	    strcpy( garea, map_info[num].garea );
	    strcpy( proj,  map_info[num].proj  );
	    
	    /*
	     * set xin, yin based on garea
	     */
	    strcpy( tmpgarea, garea );
	    ptr = &tmpgarea[0];
	    if ( tmpgarea[0] == '#' ) {

		ptr ++;

		/*
		 * use center+delta format
		 */
		cx = atof(strtok(ptr, ";"));
		cy = atof(strtok(NULL,";"));
		dx = atof(strtok(NULL,";"));
		dy = atof(strtok(NULL," "));
		mapb->x[0] = cx - dx;
		mapb->x[1] = cx + dx;
		mapb->y[0] = cy - dy;
		mapb->y[1] = cy + dy;
	    }
	    else {
		/*
		 * use boundary format
		 */
		mapb->x[0] = atof(strtok(ptr, ";"));
		mapb->y[0] = atof(strtok(NULL, ";"));
		mapb->x[1] = atof(strtok(NULL, ";"));
		mapb->y[1] = atof(strtok(NULL, " "));
	    }

	}

	/*
 	 * Draw the map.
 	 */
	map_draw( map, garea, proj, latlon, panel, text, title, &ititl,
		   &linttl, shrttl, &clear, iret,
		   strlen(map), strlen(garea), strlen(proj),
		   strlen(latlon), strlen(panel), strlen(text),
		   strlen(title), strlen(shrttl) );

	strcpy( savprj, proj );

}


/*=====================================================================*/

void draw_stnmark ( numpts, slat, slon, jcolr, imrk, iret )
int		numpts;
float		*slat;
float		*slon;
int		jcolr;
int		imrk;
int		*iret;

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
	sizmrk    = 1.0;
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

void draw_value( markdata )
struct mrkv *markdata;

/************************************************************************
 * draw_value								*
 *									*
 * This routine will plot the markers based on the input data.		*
 *									*
 * draw_value ( markdata )						*
 *									*
 * Input parameters:							*
 *	*markdata	struct		structure for marker data	*
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

void draw_cntr(contours)
struct contour *contours;

/************************************************************************
 * draw_cntr 								*
 *									*
 * This routine plots the contour lines.				*
 *									*
 * draw_cntr(contours)							*
 *									*
 * Input parameters:							*
 *	*contours       struct		struct of contour data		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	01/95						*
 * C. Lin/EAI	        09/95						*
 ************************************************************************/

{
int             ltyhw, ltype, lwidth;
int             offset, j;
float           rotat;
char		sys[2];
int             iret;

/*---------------------------------------------------------------------*/

	if ( contours->nc == 0 ) 
		return;

	ltyhw  = 0;
	ltype  = 1;
	lwidth = 1;
	rotat  = 0.;
	offset = 0;
	strcpy (sys, "M");

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
	      gline(sys, &(contours->line[j].npt), 
			 contours->line[j].lat, 
			 contours->line[j].lon,
		         &iret, strlen(sys));

	      /*
	       * plot contour label
	       */
	      gtextc(sys, &(contours->line[j].lat[0]), 
			 &(contours->line[j].lon[0]),
		    	 contours->line[j].label, 
			 &rotat, &offset, &offset, &iret,
		    	 strlen(sys), strlen(contours->line[j].label));

	}

	/*
	 * flush the buffer
	 */
	geplot(&iret );

}

/*=====================================================================*/
