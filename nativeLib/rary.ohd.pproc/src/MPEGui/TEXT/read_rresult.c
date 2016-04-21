#include <stdlib.h>
#include <string.h>

#include "List.h"
#include "mpe_log_utils.h"
#include "read_product_dt.h"
#include "read_rresult.h"
#include "RWRadarResult.h" 
#include "stage3.h"

void read_rresult ( char * datetime )

/*
   this function reads records from the rwradarresult table
  
   calling function: ReadStageiData

*/
{
   
   char * pComma = NULL ;
   char * pRadarNameSentence = NULL ;
   char * rid = NULL ;
   char where_clause [ BUFSIZ ] ;
   int comparison_result ;
   int n ;
   RWRadarResult * pRWRadarResult = NULL ;
   RWRadarResult * pRWRadarResultNode = NULL ;

   /* Check to make sure that the datetime is not null
      or an empty string. */
   if ( datetime == NULL || 
        * datetime == '\0' )
   {
      flogMessage ( stderr , "\nIn routine 'read_rresult':\n"
                         "A null or empty datetime string was passed\n"
                         "in.\n" ) ;
      return ;
   }

   /* Build the list of radars to be used in the query. */
   pRadarNameSentence = ( char * ) malloc ( ( NRADARS * 8 ) * 
                                            sizeof ( char ) ) ;

   if ( pRadarNameSentence == NULL )
   {
      flogMessage ( stderr , "In routine 'read_rresult':\n"
                         "Could not allocate %d bytes of memory for string\n"
                         "to contain %d radar names.\n" ,
                         NRADARS * 8 , NRADARS ) ;
      return ;
   }

   memset ( pRadarNameSentence , '\0' , NRADARS * 8 ) ;

   for ( n = 0 ; n < NRADARS ; ++n )
   {
      strcat ( pRadarNameSentence , "'" ) ;
      strcat ( pRadarNameSentence , nexrad[n].id ) ;
      strcat ( pRadarNameSentence , "', " ) ;
   }

   pComma = strrchr ( pRadarNameSentence , ',' ) ;
   * pComma = '\0' ;

   /* Build the where clause for the query to retrieve the radar specific
      information from the RWRadarResult table. */
   sprintf ( where_clause , "WHERE radid in ( %s ) "
                            "AND obstime= '%s' ORDER BY radid\n" ,
                            pRadarNameSentence , datetime ) ;

   free ( pRadarNameSentence ) ;
   pRadarNameSentence = NULL ;

   /* Retrieve the radar specific data from the RWRadarResult table. */
   pRWRadarResult = GetRWRadarResult ( where_clause ) ;

   if ( pRWRadarResult == NULL )
   {
      flogMessage ( stderr , "In routine 'RWRadarResult':\n"
                         "The query 'SELECT * from RWRadarResult\n"
                         "%s' has either failed or returned no rows.\n" ,
                         where_clause ) ;
   }
   else
   {
      pRWRadarResultNode = ( RWRadarResult * ) 
                           ListFirst ( & pRWRadarResult->list ) ;   
   }

   for ( n = 0 ; n < NRADARS ; n++ )
   {
      rid=nexrad[n].id ;
      datafile[n][1] = 1 ;

      if ( pRWRadarResultNode != NULL )
      {

         comparison_result = strcmp ( rid , pRWRadarResultNode->radid ) ;

         if ( comparison_result < 0 )
         {
           logMessage("Record not found in RWRadarResult table for %s %s \n",
                    nexrad[n].id,datetime);
            siibiasu[n] = 1.0;
            iflarad[n] = 1;
         }
         else if ( comparison_result == 0 )
         {
            siibiasu[n] = pRWRadarResultNode->rw_bias_val_used ;
            iflarad[n] = 1;

            if ( * ( pRWRadarResultNode->ignore_radar ) == 'n' )
            {
               iflign [ n ] = ( short ) DontIgnoreRadar ;
            }
            else
            {
               iflign [ n ] = ( short ) IgnoreRadar ;
            }

            if ( pRWRadarResultNode->num_gages > 0 ) datafile[n][1] = 0 ;

            if(strcmp(pRWRadarResultNode->rad_avail,"y") == 0) iflarad[n] = 0;
            if(strcmp(pRWRadarResultNode->rad_avail,"z") == 0) iflarad[n] = 2;
         }
         else
         {
            -- n ;
         }

         /* Read the datetime of the radar product. */
         if ( ( iflarad [ n ] == 0 ) ||
              ( iflarad [ n ] == 2 ) )
         {
            read_product_dt ( rid , datetime , 
                              & n ) ;
         }

         pRWRadarResultNode = ( RWRadarResult * ) 
                              ListNext ( & pRWRadarResultNode->node ) ;
      }
      else
      {
        logMessage("Record not found in RWRadarResult table for %s  %s \n",
         nexrad[n].id,datetime);
         siibiasu[n] = 1.0;
         iflarad[n] = 1;
      }
   }

   /* Free the linked list of RWRadarResult structures. */ 
   if ( pRWRadarResult != NULL )
   {
      FreeRWRadarResult ( pRWRadarResult ) ;
      pRWRadarResult = NULL ;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
