
/*******************************************************************************
* FILENAME:            plot_topography.c
* GENERAL INFORMATION: Contains routines for drawing the topography overlay.
*
* ORIGINAL AUTHOR:     Bryon Lawrence 
* CREATION DATE:       March 14, 2006
* ORGANIZATION:        OHD-11 HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   3/14/06      B. Lawrence       Original Coding   
********************************************************************************
*/

#include <errno.h>
#include <stdio.h>
#include <Xm/Xm.h>

#include "gageqc_defs.h"
#include "GeneralUtil.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "map_shapefile.h"
#include "mpe_topo.h"
#include "stage3_globals.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   _draw_topography
* PURPOSE:       Plots the topography overlay.  Assumes that the topography
*                data have been read in upon initialization of the application.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

#define NUM_TOPO_COLORS 35

/* Draws the topography overlay on the display. The information
   is drawn on the resolution of a HRAP grid. */

/* This routine has been modified from its original form in DailyQC
   to use the WHFS Map Library utilities. */
/* The color map for the topography overlay. */
static const char * tmap [ NUM_TOPO_COLORS ] = { "grey28",
                                                 "grey36",
                                                 "grey43",
                                                 "grey51",
                                                 "grey59",
                                                 "DarkGrey",
                                                 "grey74",
                                                 "grey25",
                                                 "grey31",
                                                 "grey38",
                                                 "burlywood4",
                                                 "wheat4",
                                                 "LightYellow4",
                                                 "burlywood3",
                                                 "grey19",
                                                 "grey25",
                                                 "salmon4",
                                                 "LightSalmon4",
                                                 "burlywood4",
                                                 "burlywood4",
                                                 "LightSalmon3",
                                                 "grey28",
                                                 "DarkOliveGreen",
                                                 "DarkOliveGreen4",
                                                 "LightGoldenrod4",
                                                 "khaki4",
                                                 "DarkOliveGreen3",
                                                 "DarkKhaki",
                                                 "DarkGreen",
                                                 "DarkGreen",
                                                 "DarkGreen",
                                                 "green4",
                                                 "green4",
                                                 "green4",
                                                 "green3" };

static unsigned long color_pixels [ NUM_TOPO_COLORS ];

static Pixmap topo_map = 0;

static Boolean topo_change = True;
static int prev_map_width = -1;
static int prev_map_height = -1;

Pixmap get_topo_pixmap ( )
{
   return topo_map;
}

void set_topo_change_flag ( )
{
   topo_change = True;
}
 
void _draw_topography ( int index , void * pData ,
                        Pixmap map , void * pUserData )
{
   Display * pDisplay = NULL;
  
   GC backgc;
   GC gc;
   static int first = 1;
   int i, j;
   Dimension width, height;
   XPoint point[5];
   XPoint * pPoints = NULL;
   XPoint prev_point_sw;
   XPoint prev_point_se;
   int maxi, maxj, mini, minj, dx, dy, di[4], dj[4];
   int x;
   int y;
   int                          mask = GCForeground;
   Pixmap pix;
   Widget map_widget;
   XGCValues                    gcv;

   /* Retrieve the topography data to plot. */
   const struct topo * topo = get_topo_coord ();

   if ( topo == NULL )
   {
      return;
   }

   /* Retrieve map library display parameters. */
   pDisplay = _get_map_display ( );
   gc = _get_map_gc ( );
   map_widget = _get_map_widget (0);
   pix = _get_map_pixmap ( );
   width = _get_map_width (0);
   height = _get_map_width (0);

   dx = 1;
   dy = 1;

   di[0] = 0;
   dj[0] = 0;
   di[1] = 1;
   dj[1] = 0;
   di[2] = 1;
   dj[2] = 1;
   di[3] = 0;
   dj[3] = 1;

   if ( width != prev_map_width ||
        height != prev_map_height || 
        topo_map == 0 )
   {
      topo_change = True;

      if ( topo_map != 0 )
      {
         XFreePixmap ( pDisplay, topo_map );
         topo_map = 0;
      }

      topo_map = XCreatePixmap ( pDisplay,
                 DefaultRootWindow ( pDisplay ),
                 width,
                 height,
                 DefaultDepthOfScreen ( XtScreen ( map_widget ) ) );

      if ( topo_map == 0 )
      {
         return;
      }

      prev_map_width = width;
      prev_map_height = height;
   }

   if ( topo_change == True )
   {
      topo_change = False;

      /* Fill the pixmap to clear it out. */
      gcv.foreground = get_pixel_by_name ( map_widget , "dim grey" ) ;
      backgc = XCreateGC ( pDisplay , 
                           DefaultRootWindow ( pDisplay ) , 
                           mask , 
                           & gcv ) ;
      XFillRectangle ( pDisplay , topo_map , backgc , 0 , 0 , 
                       width , height ) ;
      XFreeGC ( pDisplay , backgc ) ;
 

      mini = 0;
      minj = 0;
      maxi = topo->maxi;
      maxj = topo->maxj;

      /* Allocate an array of Points to contain the ne and se points in the
         current topography column. */
      pPoints = ( XPoint * ) malloc ( ( maxi - dx - mini + 1 ) 
                                      * sizeof ( XPoint ));

      if ( pPoints == NULL )
      {
         return;
      }

      /* Is this the first time this routine has been called?  If so,
         load the color pixel values for the named colors for this machine. */
      if ( first == 1 )
      {
         first = 0;
      
         for ( i = 0; i < NUM_TOPO_COLORS; ++i )
         {
            color_pixels [ i ] = _get_color ( ( char * ) tmap [ i  ] );
         }
      }

      /* Loop over each cell in the topography overlay.  Each
         cell is 1 arcminute in latitude by 1 arcminute in
         longitude. */
      for (i = mini; i < maxi - dx; i = i + dx)
      {
         for (j = minj; j < maxj - dy; j = j + dy)
         {
            /* Set the color of the elevation cell to be drawn.
               Using this routine instead of mSetColor so that 
               actual pixel values may be used. */
            XSetForeground ( pDisplay, gc,
                             color_pixels [ ( int ) topo->color[i][j] ] );


            /* Convert the lat/lon coordinates of the four corners of the
               elevation cell to pixel values. */

            if ( i == mini )
            {
               if ( j == minj )
               {
                  mConvertLatLon2XY ( topo->coord[i+di[0]][j+dj[0]].lat,
                                      topo->coord[i+di[0]][j+dj[0]].lon,
                                      & x,
                                      & y); 
                  point[0].x = (short)x;
                  point[0].y = (short)y;

                  mConvertLatLon2XY ( topo->coord[i+di[1]][j+dj[1]].lat,
                                      topo->coord[i+di[1]][j+dj[1]].lon,
                                      & x,
                                      & y); 
                  point[1].x = (short)x;
                  point[1].y = (short)y;
               }
               else
               {
                  point[0] = prev_point_sw;
                  point[1] = prev_point_se;
               }

               mConvertLatLon2XY ( topo->coord[i+di[2]][j+dj[2]].lat,
                                   topo->coord[i+di[2]][j+dj[2]].lon,
                                   & x,
                                   & y); 
               point[2].x = (short)x;
               point[2].y = (short)y;

               mConvertLatLon2XY ( topo->coord[i+di[3]][j+dj[3]].lat,
                                   topo->coord[i+di[3]][j+dj[3]].lon,
                                   & x,
                                   & y); 
               point[3].x = (short)x;
               point[3].y = (short)y;

               point[4] = point[0];
               prev_point_se = point[2];
               prev_point_sw = point[3];

            }
            else
            {
               if ( j == minj )
               {
                  point[0] = pPoints [ j ];

                  mConvertLatLon2XY ( topo->coord[i+di[1]][j+dj[1]].lat,
                                      topo->coord[i+di[1]][j+dj[1]].lon,
                                      & x,
                                      & y); 
                  point[1].x = (short)x;
                  point[1].y = (short)y;
               }
               else
               {
                  point[0] = prev_point_sw;
                  point[1] = prev_point_se;
               }

               mConvertLatLon2XY ( topo->coord[i+di[2]][j+dj[2]].lat,
                                   topo->coord[i+di[2]][j+dj[2]].lon,
                                   & x,
                                   & y); 
               point[2].x = (short)x;
               point[2].y = (short)y;

               point[3] = pPoints [ j + 1 ];
               point[4] = point[0];

               prev_point_se = point[2];
               prev_point_sw = point[3];

            }

            pPoints [ j ] = point [ 1 ];
            pPoints [ j + 1 ] = point [ 2 ];

            XFillPolygon ( pDisplay, topo_map, gc, point, 5, Nonconvex,
                           CoordModeOrigin );
         }
      }

      free ( pPoints );
      pPoints = NULL;
   }

   /* Copy the topo pixmap to the map library M_EXPOSE pixmap. */
   XCopyArea ( pDisplay, topo_map, pix, gc, 0, 0, width, height, 0 ,0 );
}

void _contour_topo (int index, void * pData, Pixmap map, void * pUserData )
{
   Display * pMapDisplay = NULL;
   float topo_scale = 1.0;
   Font font;
   extern int flf_on;
   extern int maxmin_on;
   extern int qpf_on;
   int h, i, j, k, m, l;
   int xpos, xpos1, ypos, ypos1;
   int yheight;
   const char * color = NULL;
   long nx[4], ny[4];
   XPoint point[5];
   int di[5], dj[5], dy, dx, pnum, kk, mm;
   float value[5];
   float x[5], y[5], xx[5], yy[5];
   int xint, yint, dup_flag[5];
   float telim[10];
   int maxtop, mintop;
   int offset;
   static XFontStruct * font_info = NULL;
   static char * fontname="*Adobe-Helvetica-Medium-r-normal-*-10-*";

   static char * elevation_strings [ ] = { "0", "2400", "4800", "7200", "9600", "12000" };

   const struct topo * topo = get_topo_coord ( );
   pMapDisplay = _get_map_display ( );

   /* Define the elevations to draw contours at.  These
      currently match the elevation breaks in the topography
      color legend.   These values are in decameters. */
   telim[1] = 73;
   telim[2] = 146;
   telim[3] = 219;
   telim[4] = 293;
   telim[5] = 366;

   /* Multiply the elevation delimeters by the topography scale. */
   for (i = 0; i < 6; i++)
   {
      telim[i] = telim[i] * topo_scale;
   }

   /* Define the i and j increments to be added to define the corners of
      on grid cell in the topography map. */
   dx = 1;
   dy = 1;

   di[0] = 0;
   dj[0] = 0;
   di[1] = 1;
   dj[1] = 0;
   di[2] = 1;
   dj[2] = 1;
   di[3] = 0;
   dj[3] = 1;
   di[4] = 0;
   dj[4] = 0;

   /* Draw the topography legend. */
   /* Test if the Temperature, Precipitation, or Freezing Level legends are displayed.*/
   if ( ( qpf_on == 1 ) || ( maxmin_on == 1 ) || ( flf_on == 1 ) )
   {
      /* Set the offset "y" to start drawing the elevation legend. */
     offset = MAX_GAGEQC_TYPE;
   }
   else
   {
      /* Set the offset "y" to start drawing the elevation legend. */
      offset = 0;
   }
   
   /* Initialize the font so that the font height may be determined for the
      topo legend. */
   if ( font_info == NULL )
   {
      font=XLoadFont(pMapDisplay,fontname);
      font_info=XQueryFont(pMapDisplay,font);
   }

   /* Set yheight ... needed for drawing the legend. */
   yheight = font_info->ascent;

   /* Draw the legend. */
   for ( i = 1; i < 6; ++i )
   {
      color = tmap [ 6 + 7 * ( i - 1 ) ];
      mSetColor ( ( char * ) color );
   
      mDrawLine (M_EXPOSE, index, 15, (i + offset) * yheight -
                 yheight / 2, 35,
                 (i + offset) * yheight - yheight / 2);

      mDrawText (M_EXPOSE, index, 35, (i + offset) * yheight,
                 elevation_strings [ i ]);
   }

   /* Loop over the topography map. */
   for (i = 0; i < topo->maxi - 1; i++)
   {
      for (j = 0; j < topo->maxj - 1; j++)
      {
         /* Retrieve the 4 topo values at each corner
            of the topo grid cell. */
	 value[0] = (float) topo->value[i][j];
	 value[1] = (float) topo->value[i + 1][j];
	 value[2] = (float) topo->value[i + 1][j + 1];
	 value[3] = (float) topo->value[i][j + 1];

	 maxtop = 0;
	 mintop = 9999;

         /* Determine the maximum and minimum topo value
            of the four topography values. */
	 for (mm = 0; mm < 4; mm++)
	 {
	    if (value[mm] < mintop)
            {
	       mintop = value[mm];
            }

	    if (value[mm] > maxtop)
            {
	       maxtop = value[mm];
            }
	 }

	 value[4] = value[0];

	 for (l = 1; l < 6; l++)
	 {
            /* Don't bother drawing black. */
	    if (tmap[3 + 7 * ( l - 1 )] == 0)
            {
	       continue;
            }


            /* The cell's value is not a max or min.  Don't draw it. */ 
	    if (telim[l] < mintop || telim[l] > maxtop)
            {
	       continue;
            }

	    pnum = 0;

	    for (m = 0; m < 4; m++)
	    {

	       if ((telim[l] >= value[m] &&
		    telim[l] < value[m + 1]) ||
		   (telim[l] < value[m] && telim[l] >= value[m + 1]))
	       {

		  if (value[m] == value[m + 1])
                  {
		     continue;
                  }

                  mConvertLatLon2XY (topo->coord[i+di[m+1]][j+dj[m+1]].lat,
                                     topo->coord[i+di[m+1]][j+dj[m+1]].lon,
                                     & xpos, & ypos ); 

                  mConvertLatLon2XY (topo->coord[i+di[m]][j+dj[m]].lat,
		                     topo->coord[i+di[m]][j+dj[m]].lon,
                                     & xpos1, & ypos1 );

		  x[pnum] = (float) xpos + (float) ( xpos1 - xpos ) /
		            (value[m] - value[m + 1]) * 
                            (telim[l] - value[m + 1]);
		  y[pnum] = (float) ypos + (float) ( ypos1 - ypos ) /
		            (value[m] - value[m + 1]) * 
                            (telim[l] - value[m + 1]);

		  for (h = 0; h < pnum; h++)
		  {
		     if (x[h] == x[pnum] && y[h] == y[pnum])
			goto hop;
		  }

		  if (pnum == 2)
		  {
		     for (kk = 0; kk < 3; kk++)
		     {
			dup_flag[kk] = 0;

			for (mm = 0; mm < 4; mm++)
			{
			   xint = (int) x[kk];
			   yint = (int) y[kk];

                         mConvertLatLon2XY(topo->coord[i+di[mm]][j+dj[mm]].lat,
                                           topo->coord[i+di[mm]][j+dj[mm]].lon,
                                             & xpos, & ypos );
			   if ( ( ( xint / 10 ) ==  (xpos / 10 ) ) && 
                                ( ( yint / 10 ) ==  (ypos / 10) ) )

                           {
			      dup_flag[kk] = 1;
                           }
			}
		     }
		  }

		  pnum++;

		hop:

		  pnum = pnum;

	       }

	    }

	    if ( pnum == 3 )
	    {

	       mm = 0;

	       for (kk = 0; kk < 3; kk++)
	       {

		  if (dup_flag[kk] == 0)
		  {
		     xx[mm] = x[kk];
		     yy[mm++] = y[kk];

		  }

	       }

	       for (kk = 0; kk < 2; kk++)
	       {

		  x[kk] = xx[kk];
		  y[kk] = yy[kk];

	       }

	       pnum = mm;

	    }

	    if (pnum != 2 && pnum != 4)
            {
	       continue;
            }

	    color = tmap [ 6 + 7 * ( l - 1 ) ];
            mSetColor ( ( char * ) color );

	    for (k = 0; k < pnum; k++)
	    {
	       nx[k] = x[k];
               ny[k] = y[k];
	    }

	    for (k = 0; k < pnum; k++)
	    {
	       point[k].x = nx[k] + 5;
	       point[k].y = ny[k] + 5;
	    }

	    if (pnum == 2)
            {
               mDrawLine ( M_EXPOSE, index, point[0].x, point[0].y,
                           point[1].x, point[1].y );
            }
	    else
	    {
	       if (telim[l] < value[1])
	       {
                  mDrawLine ( M_EXPOSE, index,  point[1].x, 
                              point[1].y, point[2].x, point[2].y );
                  mDrawLine ( M_EXPOSE, index, point[3].x, point[3].y,
                              point[0].x, point[0].y );
	       }
	       else
	       {
                  mDrawLine ( M_EXPOSE, index, point[0].x, point[0].y,
                              point[1].x, point[1].y );
                  mDrawLine ( M_EXPOSE, index, point[2].x, point[2].y,
                              point[3].x, point[3].y );
	       }

	    }
	 }

      }
   }

   return;

}
