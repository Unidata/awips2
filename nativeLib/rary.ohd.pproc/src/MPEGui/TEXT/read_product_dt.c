/*******************************************************************************
* FILENAME:            read_product_dt.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:          read_product_dt
* DESCRIPTION: This subroutine searches the DPARadar table for the 
*              record from a radar product with supplmess = 0 or 4.
*              The obstime is stored in the datetime_radar_prod array for
*              use in locating records in the DPARadar and DPAAdapt tables
*
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     November 3, 2004
* ORGANIZATION:      HSEB/OHD
* MACHINE:           Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/2/04      Bryon Lawrence    Converted to use DBgen
********************************************************************************
*/
#include <string.h>
#include <time.h>

#include "DPARadar.h"
#include "read_product_dt.h"
#include "rfcwide.h"
#include "time_convert.h"
#include "time_defs.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:  read_product_dt
* PURPOSE: This subroutine searches the DPARadar table for the 
*          record from a radar product with supplmess = 0 or 4.
*          The obstime is stored in the datetime_radar_prod array for
*          use in locating records in the DPARadar and DPAAdapt tables
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
* CALLING ROUTINE:
*    read_radar_grids
*
********************************************************************************
*/
void read_product_dt ( char rad [ 4 ], char * datetime, int * n )
{
   char where [ 100 ] ;

   int i, ihr, ihr1, ib, iwind = 10;
   char chr[3];
   char str[22],strp[22],str1[22];

   DPARadar * pDPARadarHead = NULL ;
   DPARadar * pDPARadarNode = NULL ;

   time_t strp_timet ;
   strcpy ( str, datetime );
   strcpy ( strp, datetime );

   /* Create the where clause. */ 
   sprintf ( where , "WHERE radid = '%s' AND obstime = '%s'" ,
             rad , str ) ; 

   /* Get the DPARadar record. */
   pDPARadarHead = GetDPARadar ( where ) ;

   if ( pDPARadarHead != NULL )
   {
      pDPARadarNode = ( DPARadar * ) ListFirst ( & pDPARadarHead->list ) ;

      if ( ( pDPARadarNode->supplmess == 0 ) ||
           ( pDPARadarNode->supplmess == 4 ) )
      {
         strcpy ( datetime_radar_prod [ * n ], datetime );
         FreeDPARadar ( pDPARadarHead );
         pDPARadarHead = NULL;
         return;
      }

      FreeDPARadar ( pDPARadarHead );
      pDPARadarHead = NULL;
   }

/*----------------------------------------------------------------------------*/
/*  search for non-top-of-hour record                                         */
/*  if searching in window around 00z, then need to use date of previous day  */
/*----------------------------------------------------------------------------*/

   sprintf ( chr, "%c%c", str [ 11 ], str [ 12 ] );
   ihr = atoi ( chr );
   ihr1 = ihr - 1;
   if(ihr1 == -1) ihr1 = 23;

   if(ihr1 == 23)
   {
      yearsec_ansi_to_timet ( strp, & strp_timet ) ;
      strp_timet -= SECONDS_PER_DAY ;
      timet_to_yearsec_ansi ( strp_timet , strp ) ;
   }

   for(i=1; i<iwind+1; i++)
   {

      /*-------------------------------------------------*/
      /*  search for record after the top-of-the-hour    */
      /*-------------------------------------------------*/

      sprintf ( str1,"%c%c%c%c-%c%c-%c%c %c%c:%02d:00",
                str[0],str[1],str[2],str[3],str[5],str[6],str[8],str[9],
                chr[0],chr[1],i);

      /* Build the where clause. */
      sprintf ( where , "WHERE radid = '%s' AND obstime = '%s'",
                rad , str1 ); 

      /* Retrieve the record from the DPARadar table. */
      pDPARadarHead = GetDPARadar ( where ) ; 

      if( pDPARadarHead != NULL )
      {
         pDPARadarNode = ( DPARadar * ) ListFirst ( & pDPARadarHead->list ) ; 

         if ( pDPARadarNode->supplmess == 0 || 
              pDPARadarNode->supplmess == 4 )
         {
            strcpy(datetime_radar_prod[*n],str1);
            FreeDPARadar ( pDPARadarHead );
            pDPARadarHead = NULL;
            return;
         }

         FreeDPARadar ( pDPARadarHead );
         pDPARadarHead = NULL;
      }

      /*-------------------------------------------------*/
      /*  search for record before the top-of-the-hour   */
      /*-------------------------------------------------*/

      ib = 60 - i;
      sprintf(str1,"%c%c%c%c-%c%c-%c%c %02d:%02d:00",
              strp[0],strp[1],strp[2],strp[3],strp[5],strp[6],strp[8],strp[9],
              ihr1,ib);

      /* Build the where clause. */
      sprintf ( where , "WHERE radid = '%s' AND obstime = '%s'",
                rad , str1 ); 

      /* Retrieve the record from the DPARadar table. */
      pDPARadarHead = GetDPARadar ( where ) ; 

      if( pDPARadarHead != NULL )
      {
         pDPARadarNode = ( DPARadar * ) ListFirst ( & pDPARadarHead->list ) ; 

         if ( pDPARadarNode->supplmess == 0 || 
              pDPARadarNode->supplmess == 4 )
         {
            strcpy(datetime_radar_prod[*n],str1);
            FreeDPARadar ( pDPARadarHead );
            pDPARadarHead = NULL;
            return;
         }

         FreeDPARadar ( pDPARadarHead );
         pDPARadarHead = NULL;
      }

   }  /* end for(i= ...) */

   return ;
}  /* end function read_product_dt  */
