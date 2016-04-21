/******************************************************************************** FILENAME:            read_adapt_param_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          read_adapt_param_RFCW
* DESCRIPTION:         This routine reads the adaptable parameters from 
*                      the DPAadapt table. 
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       November 3, 2004
* ORGANIZATION:        OHD/HSEB
* OPERATING SYSTEM:    Red Hat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/2/2004    Moria Shebsovich  Converted to use DbGen.
*********************************************************************************/

#include "create_ss_interface_rfcwide.h"
#include "post_functions.h"
#include "mpe_log_utils.h"
#include "read_adapt_param_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "DPAAdapt.h"
/******************************************************************************** MODULE NUMBER:  1
* MODULE NAME:    read_adapt_param_RFCW
* PURPOSE:        This routine reads the adaptable parameters 
*                 values from the DPAAdapt table.  
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Char *      rid                  The identifier of the radar
*                                           for which to retrieve the
*                                           adaptable parameters.
*   Input  int         n                    The index into the
*                                           datetime_radar_prod array.
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*   NAME            HEADER FILE       DESCRIPTION
*   GetDPAAdapt     GetDPAAdapt.h     Contains the GetDPAAdapt DbGen routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME             DESCRIPTION
*   char [ ]   where            Contains the where clause to be passed into the
*                               GetDPAAdapt DbGen routine.
*   DPAAdapt * pDPAAdaptHead    Points to the head of the linked list of
*                               DPAAdapt data.
*   DPAAdapt * pDPAAdaptNode    Points to a node in the linked list of
*                               DPAAdapt data.
*
* DATA FILES AND/OR DATABASE:
*   Needs an open connection to the IHFS database.  Needs the
*   DPAAdapt table in the IHFS table.
*
* ERROR HANDLING:
*   No Error Handling
*
* CALLING SUBROUTINE:
*   create_ss_interface_rfcwide 
*********************************************************************************/

void read_adapt_param_RFCW ( const char * rid , const char * datetime ,
                             int * status )
{

   char where [ 50 ] ;
   DPAAdapt * pDPAAdaptHead = NULL;
   DPAAdapt * pDPAAdaptNode = NULL;

   *status = 0 ;

   /* Build the where clause. */
   sprintf ( where , "WHERE radid='%s' and obstime='%s'\n" ,
             rid , datetime );

   /*-------------------------------------------------------*/
   /*   Retrieve data from DPAAdapt table for the given     */
   /*   radar id and time.                                  */
   /*-------------------------------------------------------*/
   pDPAAdaptHead = GetDPAAdapt ( where );

   if ( pDPAAdaptHead == NULL )
   {
      flogMessage( stderr , "Could not retrieve data from DPAAdapt table for "
                        "radar id %s and time %s.\n", rid,
                        datetime );
      *status = -1 ;
      return;
   }
   else
   {
      pDPAAdaptNode = ( DPAAdapt * ) ListFirst ( & pDPAAdaptHead->list );
      ad_params[0] = pDPAAdaptNode->min_reflth ;
      ad_params[1]  = pDPAAdaptNode->max_reflth ;
      ad_params[2] = pDPAAdaptNode->ref_tltest ;
      ad_params[3] = pDPAAdaptNode->rng_tltin ;
      ad_params[4] = pDPAAdaptNode->rng_tltout ;
      ad_params[5] = pDPAAdaptNode->max_birng ;
      ad_params[6] = pDPAAdaptNode->min_birng ;
      ad_params[7] = pDPAAdaptNode->min_echoar ; 
      ad_params[8] = pDPAAdaptNode->min_awrefl ;
      ad_params[9] = pDPAAdaptNode->max_pctred ;
      ad_params[10] = pDPAAdaptNode->mlt_zrcoef ;
      ad_params[11] = pDPAAdaptNode->pwr_zrcoef ;
      ad_params[12] = pDPAAdaptNode->min_zrefl ;
      ad_params[13] = pDPAAdaptNode->max_zrefl ;
      ad_params[14] = pDPAAdaptNode->max_stmspd ;
      ad_params[15] = pDPAAdaptNode->max_timdif ;
      ad_params[16] = pDPAAdaptNode->min_artcon ;
      ad_params[17] = pDPAAdaptNode->tim_p1cont ;
      ad_params[18] = pDPAAdaptNode->tim_p2cont ;
      ad_params[19] = pDPAAdaptNode->max_ecarch ;
      ad_params[20] = pDPAAdaptNode->rng_cutoff ;
      ad_params[21] = pDPAAdaptNode->rng_e1coef ;
      ad_params[22] = pDPAAdaptNode->rng_e2coef ;
      ad_params[23] = pDPAAdaptNode->rng_e3coef ;
      ad_params[24] = pDPAAdaptNode->min_prate ;
      ad_params[25] = pDPAAdaptNode->max_prate ;
      ad_params[26] = pDPAAdaptNode->tim_restrt ;
      ad_params[27] = pDPAAdaptNode->max_timint ;
      ad_params[28] = pDPAAdaptNode->min_timprd ;
      ad_params[29] = pDPAAdaptNode->thr_hlyout ;
      ad_params[30] = pDPAAdaptNode->end_timgag ;
      ad_params[31] = pDPAAdaptNode->max_prdval ;
      ad_params[32] = pDPAAdaptNode->max_hlyval ;
      ad_params[33] = pDPAAdaptNode->tim_biest ;
      ad_params[34] = pDPAAdaptNode->thr_nosets ;
      ad_params[35] = pDPAAdaptNode->res_bias ;
      ad_params[36] = pDPAAdaptNode->longest_lag ;
      
      ad_param46[1] = '\0' ; 
      strcpy (ad_param46 , pDPAAdaptNode->bias_applied) ;
   }

   /* Free the DPAAdapt data. */
   if ( pDPAAdaptHead != NULL )
   {
      FreeDPAAdapt ( pDPAAdaptHead ) ;
      pDPAAdaptHead = NULL ;
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/read_adapt_param_RFCW.c,v $";
 static char rcs_id2[] = "$Id: read_adapt_param_RFCW.c,v 1.6 2007/09/11 13:20:16 varmar Exp $";}
/*  ===================================================  */

}
