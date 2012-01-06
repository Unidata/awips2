/******************************************************************************** FILENAME:            read_adapt_param_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          read_adapt_param_RFCW
* DESCRIPTION:         This routine reads records from the RWBiasDyn table
*                      for the current hour. 
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       November 3, 2004
* ORGANIZATION:        OHD/HSEB
* OPERATING SYSTEM:    Red Hat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/3/2004    Moria Shebsovich    Converted to use DbGen.
*********************************************************************************/

#include <stdio.h>
#include "rfcwide.h"
#include "stage3.h"
#include "display_bias_table.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"
#include "RadarLoc.h"
#include "RWBiasDyn.h"
#include "RWBiasStat.h"

/******************************************************************************** MODULE NUMBER:  1
* MODULE NAME:    read_bias_table_param
* PURPOSE:        This routine reads records from the RWBiasDyn table. 
*                 values from the DpaAdapt table.  These values are the
*                 coefficient and power values, respectively, for the
*                 Z-R relationship.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Char *      rid                  The identifier of the radar
*                                           for which to retrieve the
*                                           field values.               
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*   NAME            HEADER FILE       DESCRIPTION
*   GetRWBiasDyn     GetRWBiasDyn.h Contains the GetRWBiasDyn DbGen routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME             DESCRIPTION
*   char [ ]   where            Contains the where clause to be passed into the
*                               GetRWBiasDyn DbGen routine.
*
* DATA FILES AND/OR DATABASE:
*   Needs an open connection to the IHFS database.  Needs the
*   RWBiasDyn table in the IHFS table.
*
* ERROR HANDLING:
*   No Error Handling
*
* CALLING SUBROUTINE:
*  display_bias_table   
*********************************************************************************/
const char * retrieve_fxa_local_site ( )
{
   static char fxa_local_site [ 20 ] = {'\0'};
   static int first = 1;
   int length;

   if ( first == 1 )
   {
      first = 0;
      length = strlen ( "FXA_LOCAL_SITE");

      get_apps_defaults ("FXA_LOCAL_SITE", &length,
                          fxa_local_site, &length );

      if ( length == 0 )
      {
         flogMessage ( stdout, "Could not retrieve a value for FXA_LOCAL_SITE\n");
         memset ( fxa_local_site, '\0', 20 );
      }
   }

   return fxa_local_site;
}

void read_bias_table_param ( const char *rid  )
{
   const char * pFxaLocalSite = NULL;
   char where [ 250 ] ;
   RWBiasDyn * pRWBiasDynHead = NULL ;
   RWBiasDyn * pRWBiasDynNode = NULL ;
   int irec = 0 ;
   double nnum_pairs ;

   pFxaLocalSite = retrieve_fxa_local_site ( );

   /* Build the where clause. */
   sprintf ( where , "WHERE radid='%s' and obstime='%s' " 
             " and office_id = '%s' ORDER BY  memspan_ind\n " ,
             rid , datetime, pFxaLocalSite ) ;

   /*-------------------------------------------------------*/
   /*   Retrieve data from RWBiasDyn table for the given    */
   /*   radar id and time.                                  */
   /*-------------------------------------------------------*/
   pRWBiasDynHead = GetRWBiasDyn ( where ) ;
 
   if ( pRWBiasDynHead == NULL )
   {
      flogMessage( stderr , " Could not retrieve data from RWBiasDyn table for "
                        " radar id %s and time %s.\n " , rid , datetime ) ;

      return;
   }

   else
   {
      if ( pRWBiasDynHead )
      { 
         pRWBiasDynNode = ( RWBiasDyn * ) ListFirst ( & pRWBiasDynHead->list ) ;
      
         while ( pRWBiasDynNode != NULL )
         {
            biasdata.mem_span[irec] = 
                            memspan_values [ pRWBiasDynNode->memspan_ind ];
            nnum_pairs = pRWBiasDynNode->numpairs ;
            biasdata.num_pairs[irec] = (float) nnum_pairs ; 
            biasdata.sumgag[irec] = pRWBiasDynNode->sumgag ;
            biasdata.sumrad[irec] = pRWBiasDynNode->sumrad ;
            biasdata.bias[irec] = pRWBiasDynNode->bias ;
            pRWBiasDynNode = ( RWBiasDyn * ) ListNext( & pRWBiasDynNode->node ) ;
            irec++ ;
         }    
      }
   }

   /* Free the RWBiasDyn data. */
   if ( pRWBiasDynHead != NULL )
   {
      FreeRWBiasDyn ( pRWBiasDynHead ) ;
      pRWBiasDynHead = NULL ;
   }

   return ;
}

int get_rfc_bias_value ( const char * rid, char * office_id, float * pBias )
{
   const char * pFxaLocalSite = NULL;
   char where[250];
   int bias_found = 0;
   int length;
   int status;
   RadarLoc * pRadarLoc = NULL;
   RWBiasStat * pRWBiasStat = NULL;
   RWBiasDyn * pRWBiasDyn = NULL;
   RWBiasDyn * pRWBiasDynNode = NULL;

   pFxaLocalSite = retrieve_fxa_local_site ( );
   length = strlen ( pFxaLocalSite );

   if ( length > 0 )
   {

      /* Get the corresponding office id for the rid. */
      sprintf ( where, "WHERE radid = '%s'", rid );
      pRadarLoc = GetRadarLoc ( where );

      if ( pRadarLoc != NULL )
      {
         status = strcasecmp ( pFxaLocalSite, pRadarLoc->office_id );

         if ( status != 0 )
         {
            sprintf ( where, "WHERE office_id = '%s'", 
                      pRadarLoc->office_id );
            /* Retrieve the record for this office from the RWBiasStat table. */
            pRWBiasStat = GetRWBiasStat ( where );

            if ( pRWBiasStat != NULL )
            {

               /* Retrieve the records for this office/radar from the
                  RWBiasDyn table. */
               sprintf ( where , "WHERE radid='%s' and obstime='%s' " 
                         " and office_id = '%s' ORDER BY  memspan_ind\n " ,
                         rid , datetime, pRadarLoc->office_id ) ;
               pRWBiasDyn = GetRWBiasDyn ( where );

               if ( pRWBiasDyn != NULL )
               {
                   /* There are entries in the RWBiasDyn table for the RFC
                      and obstime. Check for the bias which meets the 
                      number of gage/radar pairs requirement.  If this does
                      not exist, then set the bias to 1. */ 
                   bias_found = 1;
                   *pBias = 1.00;
                   strncpy ( office_id, pRadarLoc->office_id, RFC_LEN );
                   office_id [ RFC_LEN ] = '\0';

                   pRWBiasDynNode = (RWBiasDyn * ) 
                                    ListFirst (&pRWBiasDyn->list);

                   while ( pRWBiasDynNode != NULL )
                   {
                      if ( pRWBiasDynNode->numpairs >= 
                           pRWBiasStat->npair_bias_select )
                      {
                         * pBias = pRWBiasDynNode->bias;
                         break;
                      }

                      pRWBiasDynNode = (RWBiasDyn *) ListNext (&pRWBiasDynNode->node);
                   }

                  FreeRWBiasDyn ( pRWBiasDyn );
                  pRWBiasDyn = NULL;
               }

               FreeRWBiasStat ( pRWBiasStat );
               pRWBiasStat = NULL;
            }
         } 

         FreeRadarLoc ( pRadarLoc );
         pRadarLoc = NULL;
      }
   }

   return bias_found;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/read_bias_table_param.c,v $";
 static char rcs_id2[] = "$Id: read_bias_table_param.c,v 1.6 2007/10/18 18:07:24 lawrence Exp $";}
/*  ===================================================  */

}
