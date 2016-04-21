/*******************************************************************************
* FILENAME:            read_default_bias.c 
* NUMBER OF MODULES:   1 
* GENERAL INFORMATION:
*   MODULE 1:          read_default_bias
* DESCRIPTION:         This function reads records from the RWBiasStat
*                      table in the IHFS database.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence    Updated to use DBGen
********************************************************************************
*/

#include "read_default_bias.h"
#include "RWBiasStat.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_default_bias
* PURPOSE:       Reads and stores the default bias value from the RWBiasStat 
*                table.
*                There should be only one record of information in this table.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE         DESCRIPTION
*   int               The bias value.
*
* APIs UTILIZED:
*   NAME             HEADER FILE    DESCRIPTION
*   FreeRWBiasStat   RWBiasStat.h   Frees the memory used by a linked list.
*   GetRWBiasStat    RWBiasStat.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME            DESCRIPTION
*   int          bias            The minimum number of gage/radar pairs needed
*                                before a memory span and its corresponding
*                                bias value are selected.
*   RWBiasStat * pRWBiasStatHead Points to the head of the RWBiasStat linked
*                                list.
*   RWBiasStat * pRWBiasStatNode Points to a node in the RWBiasStat linked
*                                list. 
*   
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.
*   Reads the RWBiasStat table record.  There should only be one
*   record in this table.
*
* ERROR HANDLING:
*   If the bias value cannot be found in the RWBiasStat table
*   then a bias value of 0 is returned.
********************************************************************************
*/
int read_default_bias ( )
{
   int          bias;
   RWBiasStat * pRWBiasStatHead = NULL ;
   RWBiasStat * pRWBiasStatNode = NULL ;
   
   bias = 0;
   
   /* There is no where clause.  Retrieve the data from the
      RWBiasStat table. */
   pRWBiasStatHead = GetRWBiasStat ( "" );
     
   if ( pRWBiasStatHead != NULL )
   {
      pRWBiasStatNode = ( RWBiasStat * ) ListFirst ( & pRWBiasStatHead->list );
      bias = pRWBiasStatNode->npair_bias_select;

      /* Free the memory used by the RWBiasStat linked list. */ 
      FreeRWBiasStat ( pRWBiasStatHead ) ;
      pRWBiasStatHead = NULL ;
   }

   return bias ;
}
