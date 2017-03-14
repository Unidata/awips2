/*******************************************************************************
* FILENAME:     show_radar_rings.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:   show_radar_rings
*
* ORIGINAL AUTHOR:  
* CREATION DATE:    
* ORGANIZATION:     HSEB / OHD 
* MACHINE:          HP-UX / Redhat Dell Linux
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER          DESCRIPTION/REASON
*   1               5/21/2002    Moria Shebsovich    Revision
********************************************************************************
*/
#include <X11/cursorfont.h>

#include "drawa.h"
#include "map_resource.h"
#include "overlay.h"
#include "post_functions.h"
#include "stage3.h"
#include "stage3_interface.h"
#include "stage3_globals.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   show_radar_rings
* PURPOSE:  callback to toggle radar rings overlay on single site radar window 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  Widget      w           The widget in which the callback originated.
*   Input  draw_struct data        User-supplied calling data.
*   Input  caddr_t     call_data   Motif-supplied callback data.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                          HEADER FILE             DESCRIPTION
*   _get_map_display              map_resource.h          This routine returns the
*							  display. 
*   get_pixel_by_name             globals.h               Returns numeric color
*                                                         value by color name.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE                  NAME             DESCRIPTION
*
*   int          		i               The index of the panel as data array
*                                               ds_array.
*   int                         j               The number of the radars on one
*                                               panel.
*   int				n               The index in the ArgList array.
*   Arg                     wargs[5]            The argument list array.           
*   Display *                  dpy;             Specifies the display.
*   Dimension           width, height, diameter Dimension variables for location
*                                               and diameter of the radar ring.
*   short                   xpt, ypt            The coordinates of each point on
*						the radar ring.
*   int          	       x, y             The number of pixels per hrap bin.
*   GC                       gc1,gc2            The Graphic Context variable.
*   XGCValues               gcv1,gcv2;          Specifies the values for the fields 
*						in value_mask.
*
* DATA FILES AND/OR DATABASE:
*
* This routine reads radar locations from RadarLoc table of the database.
********************************************************************************

*********************************************** BEGIN show_radar_rings ********/

extern draw_struct * ds_array [ 4 ] ;

void show_radar_rings ( Widget w , XtPointer client_data, 
                        XtPointer call_data )
{
   int          i, j,  n;
   GC           gc1,gc2; /* gc1, gc3, */
   Display     *dpy;
   Dimension    width, height, diameter;
   short        xpt, ypt;
   Arg          wargs[5];
   int          x, y;
   int          mask = GCForeground;
   int          ss_number = ( int ) client_data ;
   XGCValues    gcv1,gcv2; /* gcv1, gcv3, */

 dpy = _get_map_display ( ) ;

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues ( ds_array [ MsData ] [ ss_number ].w, wargs, n);
 
 /*-------------------------------------------------------------------------*/
 /* Set up the scaling factor that will be used to plot the data.           */
 /*-------------------------------------------------------------------------*/
 
 x = (float)width/(float)ds_array [ MsData ] [ ss_number ].maximum_columns;
 y = (float)height/(float)ds_array [ MsData ] [ ss_number ].maximum_rows;
 
 /*-------------------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                             */
 /*-------------------------------------------------------------------------*/
 
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*-------------------------------------------------------------*/
 /*     create graphics contexts                                */
 /*     display legend                                          */
 /*-------------------------------------------------------------*/

    gcv1.foreground = get_pixel_by_name(w, "green"); /* 9 */
    gc1 = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv1);
  
    gcv2.foreground = get_pixel_by_name(w, "red"); /* 11 */
    gc2 = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv2);

 /*-------------------------------------------------------------*/
 /*     display rings                                           */
 /*     rings are color coded by radar rings/no radar data      */                                       
 /*-------------------------------------------------------------*/
 
 for ( i = MsData ; i <= St1iiData ; ++ i )
 {
    for ( j = 0 ; j < NRADARS ; ++j )
    {
       xpt = (nexrad[j].ctr.x - ds_array [ MsData ] [ ss_number ].origin.x 
             - nexrad[j].ngrd) * x;
       ypt = (ds_array [ i ] [ ss_number ].maximum_rows - 
    		(nexrad[j].ctr.y + nexrad[j].ngrd - 
                ds_array[i] [ ss_number ].origin.y))*y;
       diameter = nexrad[j].ngrd*2*x;
       
       /*-------------------------------------------------------*/
       /* draw radar rings where data is available              */
       /*-------------------------------------------------------*/
       
       if (datafile[j][2] == 0) 
       {
          XDrawArc(dpy, ds_array[i] [ ss_number ].pix, gc1, xpt, ypt, 
                   diameter, diameter, 0, 64*360);
       }
       
       /*-------------------------------------------------------*/
       /* draw radar rings no data                              */
       /*-------------------------------------------------------*/
       
       else 
       {
          XDrawArc(dpy, ds_array [i] [ ss_number].pix, gc2, xpt, ypt, 
                   diameter, diameter, 0, 64*360);
       }
    }
 }
 
}

/********************************************* END show_radar_rings ********/

