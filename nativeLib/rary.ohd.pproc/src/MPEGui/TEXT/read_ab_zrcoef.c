/*******************************************************************************
* FILENAME:            read_ab_zrcoef.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:  
*   MODULE 1:          read_ab_zrcoef
* DESCRIPTION:         This routine reads the mlt_zrcoef and pwr_zrcoef field 
*                      values from the DpaAdapt table these values are power 
*                      and coefficient for the Z-R relation
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 3, 2004
* ORGANIZATION:        OHD/HSEB
* OPERATING SYSTEM:    Red Hat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/2/2004    Bryon Lawrence    Converted to use DbGen.
********************************************************************************
*/

#include <stdio.h>

#include "DPAAdapt.h"
#include "mpe_log_utils.h"
#include "read_ab_zrcoef.h"
#include "rfcwide.h"
#include "time_convert.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    read_ab_zrcoef
* PURPOSE:        This routine reads the mlt_zrcoef and pwr_zrcoef field 
*                 values from the DpaAdapt table.  These values are the
*                 coefficient and power values, respectively, for the 
*                 Z-R relationship.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Char *      rid                  The identifier of the radar
*                                           for which to retrieve the 
*                                           Z-R parameters.
*   Input  int         n                    The index into the 
*                                           datetime_radar_prod array.
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*   NAME            HEADER FILE       DESCRIPTION
*   GetDpaAdapt     GetDpaAdapt.h     Contains the GetDpaAdapt DbGen routine.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME             DESCRIPTION
*   char [ ]   where            Contains the where clause to be passed into the
*                               GetDpaAdapt DbGen routine.
*   DpaAdapt * pDpaAdaptHead    Points to the head of the linked list of
*                               DpaAdapt data.
*   DpaAdapt * pDpaAdaptNode    Points to a node in the linked list of 
*                               DpaAdapt data.
*
* DATA FILES AND/OR DATABASE:
*   Needs an open connection to the IHFS database.  Needs the 
*   DpaAdapt table in the IHFS table.
*
* ERROR HANDLING:
*   No Error Handling   
*
* CALLING SUBROUTINE:
*     display_bias_table
********************************************************************************
*/
void read_ab_zrcoef ( const char * rid, int n )
{
   char where [ 50 ];
   DPAAdapt * pDPAAdaptHead = NULL;
   DPAAdapt * pDPAAdaptNode = NULL;
   
   /* Build the where clause. */
   sprintf ( where , "WHERE radid='%s' and obstime='%s'\n" ,
             rid , datetime_radar_prod [ n ] );    
 
   /*-------------------------------------------------------*/
   /*   Retrieve data from DpaAdapt table for the given     */
   /*   radar id and time.                                  */
   /*-------------------------------------------------------*/
   pDPAAdaptHead = GetDPAAdapt ( where );

   if ( pDPAAdaptHead == NULL )
   {
      flogMessage( stderr , "Could not retrieve data from DPAAdapt table for "
                        "radar id %s and time %s.\n", rid,
                        datetime_radar_prod [ n ] );
      return;
   }
   else
   {
      /* There should not be multiple entries ... but just in case
         take the first one for the given radar id and obstime. */
      pDPAAdaptNode = ( DPAAdapt * ) ListFirst ( & pDPAAdaptHead->list );
      abzerocoef.mlt_zrcoef = pDPAAdaptNode->mlt_zrcoef;
      abzerocoef.pwr_zrcoef = pDPAAdaptNode->pwr_zrcoef;		
   }

   /* Free the DpaAdapt data. */
   if ( pDPAAdaptHead != NULL )
   {
      FreeDPAAdapt ( pDPAAdaptHead ) ;
      pDPAAdaptHead = NULL ;
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/read_ab_zrcoef.c,v $";
 static char rcs_id2[] = "$Id: read_ab_zrcoef.c,v 1.2 2007/09/11 13:35:44 varmar Exp $";}
/*  ===================================================  */

}
