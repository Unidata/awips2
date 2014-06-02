#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "DAARadar.h"
#include "time_convert.h"

void readDAARadar(const char * rad, 
                   const char * datetime, 
                   const int idpawind,
                   double * maxvald,
                   double * bias,
                   char * fname,
                   int * itim,
                   int * coverageDur,
                   int * nullProductFlag,
                   long int * irc)
{
/*
   this subroutine searches DAARadar table records for a top-of-hour record

   if no such top-of-hour record is found, then a search is done on either side
     of the hour up to idpawind minutes to look for a record

   if record has null_product_flag = 0, then product is read
   else missing field used

   itim = hhmm for non-top-of-hour product
        = 0 for top-of-hour product

   coverageDur = duration of coverage (minutes)
               -- if coverageDur >= min_coverage_dur minutes, then set radar field to all 0.0
               -- else DAA product is missing (search for DPA product)

   nullProductFlag = missing flag
               = 0 -- null_product_flag > 0, missing field used
               = 1 -- null_product_flag = 0, DAA decoded product read

   calling subroutine: readRDMosaicRadars
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
   DAARadar * pDAARadarHead = NULL ;
   struct tm * pTimeStruct = NULL;
   struct tm time_struct;
   time_t time_in_ticks;

   /* Initialize the radar identifier. */
   memset ( rrad,'\0',RADAR_ID_LEN + 1 );
   strncpy ( rrad,rad, RADAR_ID_LEN );
   strcpy ( fname, " " );

   * nullProductFlag = 0;
   * coverageDur = 0;

   /* Initialize the number of minutes the DAA product is offset from the 
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
   /*------------------------------------------------------------------------*/
   sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                     rrad, str ) ;

   pDAARadarHead = GetDAARadar ( where );

   if ( pDAARadarHead != NULL )
   {
      /* A record has been found. */
      * bias = pDAARadarHead->s1_bias_value;
      * maxvald = pDAARadarHead->maxvald;
      * nullProductFlag = pDAARadarHead->null_product_flag;
      * coverageDur = pDAARadarHead->coverage_dur;
      found = 1;
      strncpy ( fname, pDAARadarHead->grid_filename, 17);
   }
   else
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
          
         sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                           rrad, str1 ) ;

         pDAARadarHead = GetDAARadar ( where );

         if ( pDAARadarHead != NULL )
         {
            /* A record has been found. */
            * bias = pDAARadarHead->s1_bias_value;
            * maxvald = pDAARadarHead->maxvald;
            * coverageDur = pDAARadarHead->coverage_dur;
            * nullProductFlag = pDAARadarHead->null_product_flag;
            strncpy ( fname, pDAARadarHead->grid_filename, 17);
            * itim = ( ihr * 100 ) + i;
            found = 1;
            break ;
         }

         ib = 60 - i;
         sprintf ( str1, "%c%c%c%c-%c%c-%c%c %02d:%d:00",
                   strp[0],strp[1],strp[2],strp[3],strp[5],
                   strp[6],strp[8],strp[9],ihr1,ib);

         sprintf ( where , "WHERE radid='%s' AND obstime='%s' ",
                            rrad, str1 ) ;

         pDAARadarHead = GetDAARadar ( where );

         if ( pDAARadarHead != NULL )
         {
            /* A record has been found. */
            * bias = pDAARadarHead->s1_bias_value;
            * maxvald = pDAARadarHead->maxvald;
            * coverageDur = pDAARadarHead->coverage_dur;
            * nullProductFlag = pDAARadarHead->null_product_flag;
            strncpy ( fname, pDAARadarHead->grid_filename, 17);
            * itim = (ihr1 * 100) + ib;
            found = 1 ;
            break;
         }
      }
   }

   if ( found == 1 )
   {
      * irc = 0;

      if ( pDAARadarHead != NULL )
      {
         FreeDAARadar ( pDAARadarHead );
         pDAARadarHead = NULL; 
      }
   }
   else
   {

   /*-----------------------------------------------------*/
   /*  no record found                                    */
   /*-----------------------------------------------------*/

      * irc = 100;
   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/read_daaradar.c,v $";
 static char rcs_id2[] = "$Id: read_daaradar.c,v 1.2 2012/06/08 15:04:07 pst Exp $";}
/*  ===================================================  */

}
