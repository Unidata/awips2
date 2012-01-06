#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "post_functions.h"

/****************************************************************
plot_gridded_temperature()

PURPOSE: plot image for callback function of "Points+Grids" /"Grids"
 buttons
*****************************************************************/
void plot_gridded_temperature(int h, int display_flag,char *file,int num)

{
  
   extern int wfo_all;
   extern int wfo_in_use[20];  
   extern struct pcp *pcp;
   extern int pcp_in_use[500];
   extern int tscale;
   extern int dqc_temp_numcol;
   extern double ** dqc_temp_delim;
 
 
   struct hrap_grid *hrap_grid;
   int i, j, k;
   XPoint PolyPoints[5];
   char * color = NULL;
   char ** gridded_temperature_colormap = NULL;
   int m;
   float value;
   int xpos;
   int ypos;
   const HRAP ** hrap_lat_lon = NULL;


hrap_grid = get_hrap_grid ( );

 /* get latest color map */
 
 gridded_temperature_colormap = get_temperature_colors ( );
 
 /* get latest color limits */
   
 set_dqc_colordelimit();

if(pcp_in_use[num]==-1)
   return;

read_file(file,num,pcp);


  hrap_lat_lon = getLatLonGrid ( );

   if ( hrap_lat_lon == NULL )
   {
      return;
   }


for(i=0;i  < hrap_grid->maxi - 1;i++) 
{

   for( j=0; j< hrap_grid->maxj - 1; j++) 
   {

             if(hrap_grid->owner[i][j]==-1)
                continue;
             if(wfo_all != 1)
	     {

                for(m=0;m<20;m++) 
		{

                   if(wfo_in_use[m] == -1)
                      break;

                   if(hrap_grid->owner[i][j]==wfo_in_use[m])
                      break;          

    	        }

                if(wfo_in_use[m]==-1)
                    continue;
             }
             
            value=(float)pcp->value[i][j]/100;

            if(value < dqc_temp_delim[tscale][0])
	       color=gridded_temperature_colormap[0];   
				 
            for(k = 0; k < dqc_temp_numcol - 1; k++) 
            {
                if( value >= dqc_temp_delim[tscale][k] &&
                        value <  dqc_temp_delim[tscale][k+1])
	        {

                   color=gridded_temperature_colormap[k];
                   break;

		}
                     
            }
            				 
            if( k == (dqc_temp_numcol - 1))
	       color=gridded_temperature_colormap[dqc_temp_numcol - 1]; 
           	   	   
 
            if(value < -99.98)
               color=0; 
              
            if(color==0)
               continue;


            mSetColor ( color  );

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




























































