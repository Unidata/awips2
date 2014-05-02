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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "DPARadar.h"
#include "time_convert.h"

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

void readDPARadar(const char * rad, 
                   const char * datetime, 
                   const int idpawind,
                   double * maxvald,
                   double * bias,
                   char * fname,
                   int * itim,
                   long int * irc)
{
/*
   this subroutine searches the DPARadar table for the
     bias, file name of gridded data file and maximum
     value (from data) from a radar product with supplmess = 0 or 4

   if no such top-of-hour record is found, then a search is done on either side
     of the hour up to idpawind minutes to look for a record from a product with
     supplmess value = 0 or 4

   itim = hhmm for non-top-of-hour product
        = 0 for top-of-hour product

   calling subroutine: getradar
*/
   char rrad [ RADAR_ID_LEN + 1 ];

   int found = 0;
   int i,ihr,ihr1,ib;
   char cday[3];
   char chr[3];
   char cmon[3];
   char cyear[5];
   char str [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char strp [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char str1 [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char where [ 256 ];
   DPARadar * pDPARadarHead = NULL ;
   struct tm * pTimeStruct = NULL;
   struct tm time_struct;
   time_t time_in_ticks;

   /* Initialize the radar identifier. */
   memset ( rrad,'\0',RADAR_ID_LEN + 1 );
   strncpy ( rrad,rad, RADAR_ID_LEN );
   strcpy ( fname, " " );

   /* Initialize the number of minutes the DPA product is offset from the 
      top of the hour. */
   *itim = 0;

   memset ( str , '\0' , ANSI_YEARSEC_TIME_LEN + 1 ) ;
   memset ( strp , '\0' , ANSI_YEARSEC_TIME_LEN + 1 ) ;
   memset ( str1 , '\0' , ANSI_YEARSEC_TIME_LEN + 1 ) ;

   /* Initialize the obstime. */
   strncpy ( str, datetime, ANSI_YEARSEC_TIME_LEN ) ;
   strcpy ( strp, str ) ;

   /*------------------------------------------------------------------------*/
   /*  search for top-of-hour record                                         */
   /*  if record is found with supplmess = 1,2,3 then continue search        */
   /*   around top-of-hour for valid product                                 */
   /*------------------------------------------------------------------------*/
   sprintf ( where , "WHERE radid='%s' AND obstime='%s' "
                     "AND (supplmess = 0 OR supplmess = 4)",
                     rrad, str ) ;

   pDPARadarHead = GetDPARadar ( where );

   if ( pDPARadarHead != NULL )
   {
      /* A record has been found. */
      * bias = pDPARadarHead->s1_bias_value;
      * maxvald = pDPARadarHead->maxvald;
      found = 1;
      strncpy ( fname, pDPARadarHead->grid_filename, 17);
   }
   else /* ( pDPARadarHead == NULL ) */
   {
      /*  Search for non-top-of-hour record.
          If searching in window around 00z, then need to use date 
           of previous day.  */
      sprintf(chr,"%c%c",str[11],str[12]);
      ihr = atoi ( chr );
      ihr1 = ihr - 1;

      if ( ihr1 == -1 )
      {
         /* Use the date of the previous day. */
         ihr1 = 23;

         sprintf ( cyear, "%c%c%c%c", str [ 0 ], str [ 1 ], str [ 2 ], 
                                      str [ 3 ] );
         sprintf ( cmon, "%c%c", str [ 5 ], str [ 6 ] ); 
         sprintf ( cday, "%c%c", str [ 8 ], str [ 9 ] );

         time_struct.tm_year = atoi ( cyear ) - 1900;
         time_struct.tm_mon = atoi ( cmon ) - 1; 
         time_struct.tm_mday = atoi ( cday );
         time_struct.tm_hour = ihr1;
         time_struct.tm_min = 0;
         time_struct.tm_sec = 0;
           
         time_in_ticks = gm_mktime ( & time_struct );
         time_in_ticks -= SECONDS_PER_DAY;
         pTimeStruct = gmtime ( & time_in_ticks ); 
         strftime ( strp, ANSI_YEARSEC_TIME_LEN, "%Y-%m-%d %H:00:00", 
                    pTimeStruct ) ;  
      }

      for ( i = 1; i < idpawind + 1; ++i )
      {
         sprintf(str1,"%c%c%c%c-%c%c-%c%c %02d:%02d:00",
                 str[0],str[1],str[2],str[3],str[5],str[6],str[8],str[9],
                 ihr,i);
          
         sprintf ( where , "WHERE radid='%s' AND obstime='%s' "
                           "AND (supplmess = 0 OR supplmess = 4)",
                           rrad, str1 ) ;

         pDPARadarHead = GetDPARadar ( where );

         if ( pDPARadarHead != NULL )
         {
            /* A record has been found. */
            * bias = pDPARadarHead->s1_bias_value;
            * maxvald = pDPARadarHead->maxvald;
            strncpy ( fname, pDPARadarHead->grid_filename, 17);
            * itim = ( ihr * 100 ) + i;
            found = 1;
            break ;
         }

         ib = 60 - i;
         sprintf ( str1, "%c%c%c%c-%c%c-%c%c %02d:%d:00",
                   strp[0],strp[1],strp[2],strp[3],strp[5],
                   strp[6],strp[8],strp[9],ihr1,ib);

         sprintf ( where , "WHERE radid='%s' AND obstime='%s' "
                           "AND (supplmess = 0 OR supplmess = 4)",
                            rrad, str1 ) ;

         pDPARadarHead = GetDPARadar ( where );

         if ( pDPARadarHead != NULL )
         {
            /* A record has been found. */
            * bias = pDPARadarHead->s1_bias_value;
            * maxvald = pDPARadarHead->maxvald;
            strncpy ( fname, pDPARadarHead->grid_filename, 17);
            * itim = (ihr1 * 100) + ib;
            found = 1 ;
            break;
         }
      }
   } /* ( pDPARadarHead == NULL ) */

   /*----------------------------------------------------------*/
   /*  no DPA product found after checking at top-of-hour      */
   /*   and around top-of-hour                                 */
   /*----------------------------------------------------------*/

   if ( found == 1 )
   {
      * irc = 0;

      if ( pDPARadarHead != NULL )
      {
         FreeDPARadar ( pDPARadarHead );
         pDPARadarHead = NULL; 
      }
   }
   else
   {
      * irc = 100;
   }

   return;

} /* end readDPARadar */
