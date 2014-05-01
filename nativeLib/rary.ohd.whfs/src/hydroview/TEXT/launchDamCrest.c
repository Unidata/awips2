/*******************************************************************************
* FILENAME:            locateDams.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION: 
*   MODULE 1:          launch_damcrest_cb 
* DESCRIPTION:         This routine is called when the user has clicked 
*                      the right mouse button and selectd DamCrest.  It
*                      then calls run_damcrest if a dam icon is found near
*                      the mouse pointer.
*   MODULE 2:          locate_dam
* DESCRIPTION:         This routine checks if the mouse pointer is near a
*                      dam icon.  If so, tt returns the dam id. 
*
* ORIGINAL AUTHOR:     Russell Erb
* CREATION DATE:       August 2004
* ORGANIZATION:        OHD / HSEB
* MACHINE:             Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <Xm/Xm.h>

#include "drawa.h"
#include "GeneralUtil.h"
#include "HvCallbacks.h"
#include "map.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "pointcontrol_mgr.h"
#include "PointDisplayControl.h"
#include "launchDamCrest.h"
#include "drawDams.h"
#include "damDisplayControl_show.h"

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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

void launch_damcrest_cb ( Widget map_widget, clicks  * mouse_clicks )
{
   int  x;
   int  y;
   char * nidid = NULL;
   char commandString[25];

   x = mouse_clicks->x ;
   y = mouse_clicks->y ;

   nidid = locateDam ( x , y ) ;
   if ( nidid != NULL )
   {
      /* Launch DamCrest */
      sprintf( commandString, "run_damcrest %s &", nidid);
      system ( commandString ) ;
   }
   else
   {
      /* Error message */
      ErrorDialog ( map_widget , "To launch DamCrest, you must click directly over a Dam's icon." ) ;
   }

   return;
}

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
*    ERROR CODE                            DESCRIPTION
********************************************************************************
*/

char * locateDam ( int x , int y )
{
   double mindistSq, distSq ;
   double delx, dely ;
   int num_dams ;
   static char damid[ NID_ID_LEN + 1] ;

   extern DamReportList * damreportlistHead ;
   DamReportList * pDamReportListNode = NULL ;

   mindistSq = 900;

   /* Are Dams being shown now? */
   if ( getDamDrawingState ( ) == 0 )
   {
      return NULL;
   }
   
   /* Determine if there are any dam reports */
   if ( damreportlistHead == NULL )
   {
      return NULL ;
   }

   /* Retrieve the number of dams in the linked list. */
   num_dams = ListCount ( & damreportlistHead->list ) ;

   if ( num_dams <= 0 )
   {
     return NULL ;
   }

   /* Walk through the linked list. */
   pDamReportListNode = ( DamReportList * ) ListFirst ( & damreportlistHead->list ) ;

   while ( pDamReportListNode != NULL )
   {
      if ( pDamReportListNode->use == 1 )
      {
          delx = abs ( x - pDamReportListNode->pos.x ) ;
         dely = abs ( y - pDamReportListNode->pos.y ) ;

         /* don't take square root of hypotenuse now (too slow) */
         distSq = (double) ((delx * delx) + (dely * dely));

         if (distSq < mindistSq)
         {
            mindistSq = distSq ;
            memset  ( damid , '\0', NID_ID_LEN + 1 ) ;
            strncpy ( damid , pDamReportListNode->nidid , NID_ID_LEN ) ;
         }
      }

      pDamReportListNode = ( DamReportList * ) ListNext ( & pDamReportListNode->node ) ;  
   }


   if ( mindistSq > CLOSE_ENOUGH_IN_PIXELS )
   {
      return NULL ;
   }
   else
   {
      return damid ;
   }
}
