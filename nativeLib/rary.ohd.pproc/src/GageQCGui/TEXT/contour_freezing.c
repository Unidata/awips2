
#include "convert_hrap.h"
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_convert.h"
#include "map_library.h"
#include "post_functions.h"

void contour_freezing (int h, int display_flag, char *file, int num)
{

   char * color = NULL;
   char ** gridded_freezing_colormap = NULL;
   extern double ** dqc_freezing_delim;
   extern int dqc_freezing_numcol;
   extern int pcp_in_use[200];
   extern struct pcp *pcp;
   extern int zscale;
   struct hrap_grid *hrap_grid;
   int i, j, k, m, l;
   int di[5], dj[5], dy, dx, pnum, kk, mm;
   float value[5];
   float x[5], y[5], xx[5], yy[5];
   double lat[5];
   double lon[5];
   int pixelx[5];
   int pixely[5];
   int xint, yint, dup_flag[5];
   float xfloat, yfloat;

   if (pcp_in_use[num] == -1)
   {
      return;
   }

   set_dqc_colordelimit();
   
   /* Retrieve the HRAP grid. */
   hrap_grid = get_hrap_grid ( );
   gridded_freezing_colormap = get_freezing_colors ( );

   /* Read the gridded field to be contoured. */
   read_file (file, num, pcp);

   dx = 1;
   dy = 1;

   /* Define the i and j increments to be added to define the corners of
      on grid cell in the map to be contoured. */
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


   /* Process every hrap grid cell except the last one
      in a row and the last one in a column. */
   for (i = 0; i < hrap_grid->maxi - 1; i++)
   {
      for (j = 0; j < hrap_grid->maxj - 1; j++)
      {

         /* Retrieve the values of the first square of four
	    cells. */
	 value[0] = (float) pcp->value[i][j] / 100;
	 value[1] = (float) pcp->value[i + 1][j] / 100;
	 value[2] = (float) pcp->value[i + 1][j + 1] / 100;
	 value[3] = (float) pcp->value[i][j + 1] / 100;

         /* If the value of these cells are too small, do not process
            this set of points! */
	 if (value[0] <= 0.001 && value[0] >= -0.001 &&
	     value[1] <= 0.001 && value[1] >= -0.001 &&
	     value[2] <= 0.001 && value[2] >= -0.001 &&
	     value[3] <= 0.001 && value[3] >= -0.001)
         {
	    continue;
         }

	 value[4] = value[0];

         /* Loop over each of the potential contour thresholds. */
	 for (l = 0; l < dqc_freezing_numcol; l++)
	 {
            /* Set the color of the contour. */
            color = gridded_freezing_colormap[l];

            /* Initialize the count of the number of points found to 0. */
	    pnum = 0;

            /* Loop over the each of the 4 sides of the square */
	    for (m = 0; m < 4; m++)
	    {

               /* Test to determine if a contour line intersects
	          this side of the box. */
	       if ((dqc_freezing_delim[zscale][l] >= value[m] &&
		    dqc_freezing_delim[zscale][l] < value[m + 1]) ||
		   (dqc_freezing_delim[zscale][l] < value[m] &&
		    dqc_freezing_delim[zscale][l] >= value[m + 1]))
	       {

                  /* A contour is not drawn between points of 
		     equal value. */
		  if (value[m] == value[m + 1])
                  {
		     continue;
                  }

                  /* Determine the x and y coordinates of the intersection point. */
		  x[pnum] =
		     (float) hrap_grid->coord[i + di[m + 1]][j +
							     dj[m + 1]].x +
		     (float) (hrap_grid->coord[i + di[m]][j + dj[m]].x -
			      hrap_grid->coord[i + di[m + 1]][j +
							      dj[m +
								 1]].x) /
		     (value[m] - value[m + 1]) * (dqc_freezing_delim[zscale][l] -
						  value[m + 1]);

		  y[pnum] =
		     (float) hrap_grid->coord[i + di[m + 1]][j +
							     dj[m + 1]].y +
		     (float) (hrap_grid->coord[i + di[m]][j + dj[m]].y -
			      hrap_grid->coord[i + di[m + 1]][j +
							      dj[m +
								 1]].y) /
		     (value[m] - value[m + 1]) * (dqc_freezing_delim[zscale][l] -
						  value[m + 1]);

                  /* If this point is the same as the one before it,
                     eliminate it ... it is a duplicate. */
		  for (h = 0; h < pnum; h++)
		  {

		     if (x[h] == x[pnum] && y[h] == y[pnum])
                     {
                        continue;
                     }
		  }

                  /* Increment the point count. */
		  pnum++;
	       }

	    }

            /* If there are three points, check for and eliminate duplicate points. */
            if ( pnum == 3 )
            {
               for (kk = 0; kk < pnum - 1; kk++)
               {
	          dup_flag[kk] = 0;

	          for (mm = 1; mm < pnum; mm++)
	          {
                     if ( x[kk] == x[mm] )
                     {
                        dup_flag [kk] = 1;
                     }
                  }
	       }

	       mm = 0;

	       for (kk = 0; kk < pnum; kk++)
	       {
		  if (dup_flag[kk] == 0)
		  {
		     x[mm] = x[kk];
		     y[mm++] = y[kk];
		  }
	       }

	       pnum = mm;
            }

            /* Need either two or 4 points for contouring. */
	    if (pnum != 2 && pnum != 4)
            {
	       continue;
            }

            /* Convert the points from HRAP to latitude and longitude
               to pixels. */
            for ( kk = 0; kk < pnum; ++kk )
            {
               HrapToLatLongByReference ( y[kk] + hrap_grid->hrap_miny, 
                               x[kk] + hrap_grid->hrap_minx, 
                               &lat[kk], 
                               &lon[kk] ); 
               lon[kk] *= -1;
               mConvertLatLon2XY ( lat[kk], lon[kk], &pixelx[kk], &pixely[kk] );
            }
   
            mSetColor ( color );

            /* Draw the contour line segment. */
	    if (pnum == 2)
            {
               mDrawLine ( M_EXPOSE, 0, pixelx[0], pixely[0],
			  pixelx[1], pixely[1]);
            }
	    else
	    {

	       if (dqc_freezing_delim[zscale][l] < value[1])
	       {
		  mDrawLine (M_EXPOSE, 0, pixelx[1], pixely[1],
			     pixelx[2], pixely[2]);
		  mDrawLine (M_EXPOSE, 0, pixelx[3], pixely[3],
			     pixelx[0], pixely[0]);
	       }
	       else
	       {
		  mDrawLine (M_EXPOSE, 0, pixelx[0], pixely[0],
			     pixelx[1], pixely[1]);
		  mDrawLine (M_EXPOSE, 0, pixelx[2], pixely[2],
			     pixelx[3], pixely[3]);
	       }
	    }
	 }

      }
   }

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
