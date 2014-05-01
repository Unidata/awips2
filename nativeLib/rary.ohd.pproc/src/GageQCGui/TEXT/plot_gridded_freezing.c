/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "mpe_log_utils.h"
#include "post_functions.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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

/***************************************************************************
plot_gridded_freezing()

PURPOSE: plot image for callback function of "Points+Grids"/"Grids"
 buttons.
*****************************************************************/
void plot_gridded_freezing (int h, int display_flag, char *file, int num)
{

   
   extern int dmvalue;
   extern int abmode;
   extern int wfo_all;
   extern int wfo_in_use[20];  
   extern struct pcp *pcp;
   extern int pcp_in_use[500];
   extern int zscale;
   extern int dqc_freezing_numcol;
   extern double ** dqc_freezing_delim; 
   extern struct hrap_grid *hrap_grid;
   
   int i, j, k;
   int xpos;
   int ypos;
   float value;
   XPoint PolyPoints[5];
   char * color = NULL;
   char ** gridded_freezing_colormap = NULL;
   const HRAP ** hrap_lat_lon = NULL;
   int m;
   
   hrap_grid = get_hrap_grid ( );

   /* get latest color map */
   
   gridded_freezing_colormap = get_freezing_colors ( );

   /* get latest color limits */
   
   set_dqc_colordelimit();

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
	    continue;

	 if (wfo_all != 1)
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

	    if (wfo_in_use[m] == -1)
            {
	       continue;
            }

	 }

	 value = (float) pcp->value[i][j] / 100;

	 if (value <= 0.01 && value >= -0.01)
         {
	    continue;
         }


	 for (k = 0; k < dqc_freezing_numcol - 1; k++)
	 {

	    if (( value >= dqc_freezing_delim[zscale][k] ) &&
	         ( value < dqc_freezing_delim[zscale][k + 1]))
	    {
	       
               color = gridded_freezing_colormap[k];
	      /*logMessage("value: %f color: %s", value, color);*/
	       	      
	       break;
	    }

	 }

	 if (k == (dqc_freezing_numcol - 1))	    
            color = gridded_freezing_colormap[dqc_freezing_numcol - 1]; 
	    	

	 if (value < -99.98)	    
            color = gridded_freezing_colormap[7];

	 if (color == 0)
	    continue;

	 if (abmode != 2)
	 {

	    if (abmode == 0 &&
		(pcp->value[i][j] * 10 - dmvalue < hrap_grid->elev[i][j]))
            {
	       continue;
            }


	    if (abmode == 1 &&
		(pcp->value[i][j] * 10 - dmvalue > hrap_grid->elev[i][j]))
            {
	       continue;
            }

	 }

         mSetColor ( color );

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/plot_gridded_freezing.c,v $";
 static char rcs_id2[] = "$Id: plot_gridded_freezing.c,v 1.2 2007/05/23 21:52:04 whfs Exp $";}
/*  ===================================================  */

}
