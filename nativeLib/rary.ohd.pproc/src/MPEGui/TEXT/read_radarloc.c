/******************************************************************************** FILENAME:            read_radarloc.c
* NUMBER OF MODULES:  1
* GENERAL INFORMATION:
*   MODULE 1:  read_radarloc 
* DESCRIPTION:  This routine reads RadarLoc table and stores radar location 
*               information in nex_struct structure, it also calculates
*               the HRAP coordinates of the radar location and stores it
*               in the nex_struct structure.
* ORIGINAL AUTHOR:     
* CREATION DATE:       
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*       1          11/2/2004 Moria Shebsovich  Transition from .ec/.pgc.
*********************************************************************************/
 
#include <math.h>   /* For the M_PI manifest constant. */
#include <stdio.h>
#include <stdlib.h>

#include "RadarLoc.h"
#include "read_radarloc.h"
#include "stage3.h"

/****************************************************************************
* PURPOSE:       This routine reads RadarLoc table and stores radar location
*               information in nex_struct structure, it also calculates
*               the HRAP coordinates of the radar location and stores it
*               in the nex_struct structure.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  int *       number    A pointer  to number of radars found 
*                                in radarloc table with use_radar = 'T'.
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME               HEADER FILE     DESCRIPTION
* 
*
* DATA FILES AND/OR DATABASE:
*   Reads RadarLoc database table.
*
* ERROR HANDLING:
*   If there are no radars returned from the RadarLoc table, the error 
*   message is printed out.
* 
* CALLING SUBROUTINE:
*   calling function: main_rfcwide
*
*********************************************************************************/

void read_radarloc(int *nrad)
{
   int i ;
   double z ;
   HRAP   hrp ;
   char where_select[100] ;
   RadarLoc * pRadarLocNode = NULL ;
   RadarLoc * pRadarLoc = NULL ;

   sprintf ( where_select,"WHERE use_radar='T' order by radid" ) ;
   
   pRadarLoc = GetRadarLoc ( where_select ) ;
   if ( pRadarLoc == NULL )
   {
      printf("There are no radars in the RadarLoc table\n") ;
   }
   pRadarLocNode = ( RadarLoc * ) ListFirst ( & pRadarLoc->list ) ;

   * nrad = ListCount (& pRadarLoc->list ) ;

   /* malloc space and store information in structure */

   nexrad = ( nex_struct * ) malloc ( ( *nrad ) * sizeof ( nex_struct ) ) ;

   printf ( "HRAP coordinates of radar locations:\n" ) ;

   for(i = 0 ; i < * nrad ; i++ )
   {

     strcpy ( nexrad[i].id, pRadarLocNode->radid ) ;
     nexrad[i].xlat = pRadarLocNode->lat ;
     nexrad[i].xlong = pRadarLocNode->lon ;

     hrp = LatLongToHrapMpe ( nexrad[i].xlat , nexrad[i].xlong ) ;
    
     nexrad[i].ctr.x = ( int ) hrp.x ;
     nexrad[i].ctr.y = ( int ) hrp.y ;

     printf ("%s   %d   %d\n",nexrad[i].id, nexrad[i].ctr.x, nexrad[i].ctr.y ) ;

     /*--------------------------------------------------------------------*/
     /*   ngrd = number of HRAP bins in 230 km                             */
     /*   maximum value allowed = 65                                       */
     /*   SJU (San Juan) would have value = 69 without check on max value  */
     /*--------------------------------------------------------------------*/

               
     z=4.7625/((1+sin((double)(M_PI*60/180)))/
              (1+sin((double)(M_PI*nexrad[i].xlat/180.))));
     nexrad[i].ngrd = ( int )( 230/z + 1 ) ;
     if ( nexrad[i].ngrd > 65 ) 
        nexrad[i].ngrd = 65 ;
     
     pRadarLocNode = ( RadarLoc * ) ListNext ( & pRadarLocNode->node ) ; 
  }

  /* Free the memory used by the RadarLoc structures. */
  FreeRadarLoc ( pRadarLoc );
  pRadarLoc = NULL;

}  /* end read_radarloc function */
