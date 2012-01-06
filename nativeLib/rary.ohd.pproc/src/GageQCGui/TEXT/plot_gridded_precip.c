#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "post_functions.h"
#include "NamedColorSetGroup.h"
#include "rfcwide.h"

/***************************************************************************
plot_gridded_precip()

PURPOSE: plot image for callback function of "Points+Grids"/"Grids"
 buttons.
*****************************************************************/
void plot_gridded_precip ( int h, int display_flag, char *file, int num,
	                       int mnum)
{
  
   extern int dmvalue;
   extern int kscale;   
   extern int pcpn_time_step;
   extern int rsmode;
   extern int wfo_all;
   extern int wfo_in_use[20];
   extern struct pcp *pcp;
   extern struct pcp *spf;
   extern int pcp_in_use[500];
   extern int kscale;
   extern int dqc_precip_numcol;
   extern double ** dqc_precip_delim;
 
   struct hrap_grid *hrap_grid;
   int i, j, k;
   XPoint PolyPoints[5];
   char * color = NULL;
   char ** gridded_precip_colormap = NULL;
   int m;
   float value;
   int i1;
   int xpos;
   int ypos;
   const HRAP ** hrap_lat_lon = NULL;

   hrap_grid = get_hrap_grid ( );

   /* get latest color map */
   
   gridded_precip_colormap = get_precip_colors ( );
   
   /* get latest color limits */
   
   set_dqc_colordelimit();
      
   if (pcpn_time_step == 0 && rsmode != 1)
   {

      i1 = 1;

      if (num == 0)
      {
	 i1 = 0;
      }
      if (pcp_in_use[num + mnum] != -1 && pcp_in_use[num + mnum - i1] != -1)
      {
	 read_file (file, num + mnum, spf);
	 read_file (file, num + mnum - i1, pcp);

	 for (i = 0; i < hrap_grid->maxi - 1; i++)
	 {
	    for (j = 0; j < hrap_grid->maxj - 1; j++)
	    {
	       spf->value[i][j] = (spf->value[i][j] + pcp->value[i][j]) / 2;
	    }
	 }
      }
      else if (pcp_in_use[num + mnum] == 1)
      {
	 read_file (file, num + mnum, spf);
      }
      else if (pcp_in_use[num + mnum - i1] == 1)
      {
	 read_file (file, num + mnum - i1, spf);
      }
   }

   if (pcp_in_use[num] == -1)
   {
      return;
   }

   read_file (file, num, pcp);

   /* Retrieve the MPE LatLon Grid. */
   hrap_lat_lon = getLatLonGrid ( );

   if ( hrap_lat_lon == NULL )
   {
      return;
   }

   /* Color in the HRAP grid bins. */
   for (i = 0; i < hrap_grid->maxi - 1; i++)
   {
      for (j = 0; j < hrap_grid->maxj - 1; j++)
      {
	 if (hrap_grid->owner[i][j] == -1)
         {
	    continue;
         }

	 if ( wfo_all != 1 )
	 {

	    for (m = 0; m < 20; m++)
	    {
	       if (wfo_in_use[m] == -1)
               {
		  break;
               }

	       if (hrap_grid->owner[i][j] == wfo_in_use[m])
               {
		  break;
               }
	    }

	 }

	 if (wfo_all == 1 || wfo_in_use[m] != -1)
	 {

	    value = (float) pcp->value[i][j] / 100;

	    if (value <= 0.01 && value >= -0.01)
	       continue;
	    if (rsmode == 1)
	    {

	       for (k = 0; k < dqc_precip_numcol - 1; k++)
	       {

		  if (value >= dqc_precip_delim[kscale][k] &&
		      value < dqc_precip_delim[kscale][k + 1])
		  {
		     color = gridded_precip_colormap[k];
		     break;
		  }

	       }

	       if (k == (dqc_precip_numcol - 1))
		  color = gridded_precip_colormap[k];

	       if (value < -99.98)
		  color = gridded_precip_colormap[7];

	       if (color == 0)
		  continue;
	    }
	    else
	    {

	       for (k = 0; k < 4; k++)
	       {

		  if (value >= dqc_precip_delim[kscale][k] &&
		      value < dqc_precip_delim[kscale][k + 1])
		  {
		     color = gridded_precip_colormap[k];
		     break;
		  }
	       }

	       if (k == 4)
               {
		  color = gridded_precip_colormap[4];
               } 

	       if (value < -99.98)
               {
		  color = gridded_precip_colormap[7];
               }

	       if (color == 0)
               {
		  continue;
               }
	    }

	    if ( ( rsmode != 1 ) &&
		 ( pcp_in_use[mnum + num] == 1 ||
		   pcp_in_use[mnum + num - i1] == 1))
	    {
	       if ((spf->value[i][j] * 10 - dmvalue <
		    hrap_grid->elev[i][j]) && spf->value[i][j] >= 0)
               {
		  color = gridded_precip_colormap[k + 5];
               }
	    }

            mSetColor ( color  );

	 }


         /* Using the MPE Lat/Lon Grid, draw the point. */

         mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j ].y ,
                             hrap_lat_lon [ i ] [ j ].x ,
                             & xpos , & ypos ) ;

         PolyPoints[0].x = ( short ) xpos ;
         PolyPoints[0].y = ( short ) ypos ;


         mConvertLatLon2XY ( hrap_lat_lon [ i ] [ j + 1 ].y ,
                             hrap_lat_lon [ i ] [ j + 1 ].x ,
                             & xpos , & ypos ) ;

         PolyPoints [ 1 ].x = ( short ) xpos ;
         PolyPoints [ 1 ].y = ( short ) ypos ;

         mConvertLatLon2XY ( hrap_lat_lon [ i + 1 ] [ j + 1 ].y ,
                             hrap_lat_lon [ i + 1 ] [ j + 1 ].x ,
                             & xpos , & ypos ) ;

         PolyPoints [ 2 ].x = ( short ) xpos ;
         PolyPoints [ 2 ].y = ( short ) ypos ;

         mConvertLatLon2XY ( hrap_lat_lon [ i + 1 ] [ j ].y ,
                             hrap_lat_lon [ i + 1 ] [ j ].x ,
                             & xpos , & ypos ) ;

         PolyPoints [ 3 ].x = ( short ) xpos ;
         PolyPoints [ 3 ].y = ( short ) ypos ;

         PolyPoints [ 4 ].x = PolyPoints [ 0 ].x;
         PolyPoints [ 4 ].y = PolyPoints [ 0 ].y;

         if (wfo_all != 1 && wfo_in_use[m] == -1)
         {
            /* Set the color to missing. */
            mSetColor ( "Grey" );
         }
         mDrawFillPolygon ( M_EXPOSE , 0 , PolyPoints , 5 ,
                            Nonconvex , CoordModeOrigin ) ;
      }

   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

