/*******************************************************************************
* FILENAME:            read_dpa_fname.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          read_dpa_fname
* DESCRIPTION:         Reads the filename of the decoded radar product 
*                      from the DPARadar table in the IHFS database.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhad Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence    Converted to use DBGen    
********************************************************************************
*/
#include <string.h>

#include "DPARadar.h"
#include "read_dpa_fname.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    read_dpa_fname
* PURPOSE:        Reads the name of the of the decoded radar product for
*                 a given radar and time.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  char [ ]    rad       The radar identifier
*   Input  char        datetime  The time to retrieve the radar product
*                                for.
*   Output char [ ]    fname     The name of the file containing the decoded
*                                radar product.
*   Output long int*   irc       Contains any error codes.
*
* RETURNS:
*   None.
*
* APIs UTILIZED:
*   NAME           HEADER FILE   DESCRIPTION
*   FreeDPARadar   DPARadar.h    Frees memory used by the linked list of
*                                DPARadar structures.
*   GetDPARadar    DPARadar.h    Retrieves rows from the DPARadar table. 
*                                Stores these in a linked list of
*                                DPARadar structures. 
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE     NAME               DESCRIPTION
*   char [ ]      where              The where clause is built in this
*                                    array.
*   DPARadar *    pDPARadarHead      Points to the head node of the DPARadar
*                                    linked list.
*   DPARadar *    pDPARadarNode      Points to a node in the DPARadar
*                                    linked list.
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.  Reads the
*   DPARadar table.
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/
void read_dpa_fname ( const char rad[4], const char * datetime, char fname[17], 
                      long int * irc )
{
   char where [ 50 ];
   DPARadar * pDPARadarHead = NULL;
   DPARadar * pDPARadarNode = NULL;

   strcpy(fname," ");
   * irc = 0;

   /* Build the where clause. */
   sprintf ( where, "WHERE radid='%s' AND obstime='%s'" ,
             rad , datetime );

   /* Get the data from the DPARadar table. */
   pDPARadarHead = GetDPARadar ( where );

   if ( pDPARadarHead != NULL )
   {
      pDPARadarNode = ( DPARadar * ) ListFirst ( & pDPARadarHead->list );
      strcpy ( fname , pDPARadarNode->grid_filename );

      FreeDPARadar ( pDPARadarHead ) ;
      pDPARadarHead = NULL ;
   }
   else
   {
      * irc = -1;
   }

   return ;
}
