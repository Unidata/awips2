/*=========================================================================*/
/*                         FILE NAME:  show_overlays.c                     */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   show_states()                      */
/*                                      show_county()                      */
/*                                      show_cities_and_towns()            */
/*                                      show_basin_boundaries()            */
/*                                      show_rivers()                      */
/*   Modified by Jingtao Deng   02/24/2006 change LatLongToHrap()          */    
/*                                         to LatLongToHrapPproc()         */
/*                                                                         */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "hrap.h"
#include "stageiii_structures.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include "GeneralUtil.h"

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

char                   *color_list_overlays[30];
int                     dbg;

/*function prototype defination */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   show_states                                           */
/*       FUNCTION:   callback to toggle states overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (states) button

Functions called:
   add_overlays
   get_pixel_by_name

******************************************** BEGIN show_states *************/

void show_states(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, maxpts;
   XPoint      *points;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show states\n");

 dpy = XtDisplay(data->w);

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     if states are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

 if (data->states_on == 1)
    {
     data->states_on = 0;
     XCopyArea(dpy, data->pixbase, data->pix,
	    data->gc[0],0,0,width,height,0,0);
     post_analysis_add_overlays(data);
     XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
     return;
    }

 data->states_on = 1;

 /*--------------------------------------------------------------*/
 /*     determine number of points to be plotted & allocate space*/
 /*--------------------------------------------------------------*/

 maxpts = 0;
 for(i=0;i<numstates;i++)
      if (state[i]->npts > maxpts)
	  maxpts = state[i]->npts;

 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context for states overlay               */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(w,color_list_overlays[0]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     draw states                                              */
 /*--------------------------------------------------------------*/

 for (i=0;i<numstates;i++)
    {
    for (j=0;j<state[i]->npts;j++)
       {
       points[j].x = (state[i]->hrap[j].x - data->origin.x) * x;
       points[j].y = (data->maximum_rows - (state[i]->hrap[j].y - data->origin.y))*y;
       }
    XDrawLines(dpy, data->pix, gc, points, state[i]->npts, CoordModeOrigin);
    }

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);
}

/********************************************* END show_states *************/



/***************************************************************************/
/*  FUNCTION NAME:   show_county                                           */
/*       FUNCTION:   callback to toggle county overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (county) button

Functions called:
   add_overlays
   get_pixel_by_name

******************************************** BEGIN show_county *************/

void show_county(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, maxpts;
   XPoint      *points;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show states\n");

 dpy = XtDisplay(data->w);

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     if county are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

 if (data->county_on == 1)
    {
    data->county_on = 0;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0], 0, 0, width, height, 0, 0);
    post_analysis_add_overlays(data);
    XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
    return;
    }

 data->county_on = 1;

 /*--------------------------------------------------------------*/
 /*     determine number of points to be plotted & allocate space*/
 /*--------------------------------------------------------------*/

 maxpts = 0;
 for(i=0;i<numcounty;i++)
      if (county[i]->npts > maxpts)
	  maxpts = county[i]->npts;

 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context for states overlay               */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(w, color_list_overlays[7]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     draw states                                              */
 /*--------------------------------------------------------------*/

 for (i=0;i<numcounty;i++)
    {
    for (j=0;j<county[i]->npts;j++)
       {
       points[j].x = (county[i]->hrap[j].x - data->origin.x) * x;
       points[j].y = (data->maximum_rows - (county[i]->hrap[j].y - data->origin.y))*y;
       }
    XDrawLines(dpy, data->pix, gc, points, county[i]->npts, CoordModeOrigin);
    }

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);
}

/********************************************* END show_county *************/



/***************************************************************************/
/*  FUNCTION NAME:   show_cities_and_towns                                 */
/*       FUNCTION:   callback to toggle cities and towns overlay           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (cities) button

Functions called:
   add_overlays
   get_pixel_by_name
   LatLongToHrapPproc

******************************************** BEGIN show_cities_and_towns ***/

void show_cities_and_towns(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          j, n, len,xloc, yloc;
   HRAP         hpt;
   FILE        *cityfile;
   float        xlat, xlong;
   char         str[30], city[30],rfc[9],dir[100];
   char         *filename;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x,y;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show cities and towns\n");

 dpy = XtDisplay(data->w);
 filename = (char *)malloc(128*sizeof(char));

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     if cities are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

 if (data->cities_on == 1)
    {
    data->cities_on = 0;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0],0,0,width,height,0,0);
    post_analysis_add_overlays(data);
    XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
    return;
    }

 data->cities_on = 1;

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(w,color_list_overlays[4]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     read town or town_zoom file based on display size        */
 /*--------------------------------------------------------------*/

 len = strlen("geo_data");
 get_apps_defaults("geo_data",&len,dir,&len);
 len = strlen("st3_rfc");
 get_apps_defaults("st3_rfc",&len,rfc,&len);

 if (data->maximum_columns < 131)
    {
     sprintf(filename,"%s/%s/ascii/town_zoom.dat",dir,rfc);
     if (dbg)printf("file = %s\n",filename);
    }
 else
    {
     sprintf(filename,"%s/%s/ascii/town.dat",dir,rfc);
     if (dbg) printf("file = %s\n",filename);
    }

 /*--------------------------------------------------------------*/
 /*     display city names at appropriate location               */
 /*     a period is affixed to the beginning of the              */
 /*     name to locate the city on the display                   */
 /*--------------------------------------------------------------*/

     if((cityfile = fopen(filename,"r")) == NULL)
     {
       printf("warning: %s file not found \n",filename);
     }
     else
     {
       for(;;)
       {
         j = fscanf(cityfile, "%s %f %f", city, &xlat, &xlong);
         if (j == EOF) break;
         hpt = LatLongToHrapPproc(xlat, xlong);
         xloc = (hpt.x - data->origin.x) * x;
         yloc = (data->maximum_rows - (hpt.y - data->origin.y))*y;

         sprintf(str,".%s",city);
         XDrawString(XtDisplay(data->w),data->pix,gc,xloc,yloc,str,strlen(str));
       }
       fclose(cityfile);

 /*--------------------------------------------------------------*/
 /*     create expose event if appropriate                       */
 /*--------------------------------------------------------------*/

       if (XtIsRealized(data->w))
         XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
     }
 if (dbg) printf("leaving cities & towns\n");
}

/********************************************* END show_cities_and_towns ***/

/***************************************************************************/
/*  FUNCTION NAME:   show_basin_boundaries                                 */
/*       FUNCTION:   callback to toggle basin boundaries overlay           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (basins) button

Functions called:
   add_overlays
   get_pixel_by_name

******************************************** BEGIN show_basin_boundaries ***/

void show_basin_boundaries(w, data, call_data)
   Widget               w;
   draw_struct         *data;
   caddr_t             *call_data;
{
   int                  i, j, n, maxpts;
   XPoint              *points;
   GC                   gc;
   Display             *dpy;
   Dimension            width, height;
   Arg                  wargs[5];
   int                  x, y;
   int                  mask = GCForeground;
   XGCValues            gcv;
   int                  numbasin;
   overlay_struct      **basin;

 if (dbg) printf("in show basin boundaries\n");

 dpy = XtDisplay(data->w);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     if basins are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

 if (data->basins_on == 1)
    {
    data->basins_on = 0;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0],0,0,width,height,0,0);
    post_analysis_add_overlays(data);
    if (XtIsRealized(data->w))
       XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
    return;
    }

 data->basins_on = 1;

 /*--------------------------------------------------------------*/
 /*     determine whether to display map basins or forecast      */
 /*     group basins depending on size of display area           */
 /*--------------------------------------------------------------*/

 if (data->maximum_columns < 131) basin = mapbasin;
 else basin = fgbasin;

 /*--------------------------------------------------------------*/
 /*     determine maximum number of points to display & allocate */
 /*     space                                                    */
 /*--------------------------------------------------------------*/

 maxpts = 0;
 if (data->maximum_columns < 131)
 {
    numbasin = nummap;
    gcv.foreground = get_pixel_by_name(w, color_list_overlays[2]);
 }
 else
 {
    numbasin = numfg;
    gcv.foreground = get_pixel_by_name(w, color_list_overlays[3]);
 }

 for(i=0;i<numbasin;i++)
    if (basin[i]->npts > maxpts)
       maxpts = basin[i]->npts;

 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     display basin boundaries                                 */
 /*--------------------------------------------------------------*/

 for (i=0;i<numbasin;i++)
    {
    for (j=0;j<basin[i]->npts;j++)
       {
       points[j].x = (basin[i]->hrap[j].x - data->origin.x) * x;
       points[j].y = (data->maximum_rows - (basin[i]->hrap[j].y - data->origin.y))*y;
       }
    XDrawLines(dpy, data->pix, gc, points, basin[i]->npts, CoordModeOrigin);
    }

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already been   */
 /*     realized                                                 */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);
}

/********************************************* END show_basin_boundaries ***/

/***************************************************************************/
/*  FUNCTION NAME:   show_rivers                                           */
/*       FUNCTION:   callback to toggle rivers overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (rivers) button

Functions called:
   add_overlays
   get_pixel_by_name

******************************************** BEGIN show_rivers *************/

void show_rivers(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, maxpts;
   XPoint      *points;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y, minorder;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show rivers\n");

 dpy = XtDisplay(data->w);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     if basins are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

 if (data->rivers_on == 1)
    {
    data->rivers_on = 0;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0],0,0,width,height,0,0);
    post_analysis_add_overlays(data);
    XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
    return;
    }

 /*--------------------------------------------------------------*/
 /*     allocate space for number of points to display           */
 /*--------------------------------------------------------------*/

 data->rivers_on = 1;
 maxpts = 0;
 for(i=0;i<numrivers;i++)
    if (river[i]->npts > maxpts)
       maxpts = river[i]->npts;
 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(w,color_list_overlays[1]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     determine minimum stream order to display                */
 /*--------------------------------------------------------------*/

 minorder = 4;
 if (data->maximum_columns <= 131) minorder = 3;
 if (data->maximum_columns <= 75) minorder = 1;

 /*--------------------------------------------------------------*/
 /*     display rivers                                           */
 /*--------------------------------------------------------------*/

 for (i=0;i<numrivers;i++)
    {
    if (river[i]->order >= minorder)
       {
       for (j=0;j<river[i]->npts;j++)
	  {
	  points[j].x = (river[i]->hrap[j].x - data->origin.x) * x;
	  points[j].y = (data->maximum_rows - (river[i]->hrap[j].y - data->origin.y))*y;
	  }
       XDrawLines(dpy, data->pix, gc, points, river[i]->npts, CoordModeOrigin);
       }
    }

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already        */
 /*     been realized                                            */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(data->w))
    XClearArea(dpy,XtWindow(data->w),0,0,0,0,TRUE);


}

/********************************************* END show_rivers *************/
