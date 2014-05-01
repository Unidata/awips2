
/*******************************************************************************
* FILENAME:            set_single_site_overlays
* NUMBER OF MODULES:   1
* GENERAL INFORMATION: 
*   MODULE 1:          set_single_site_overlays
* DESCRIPTION:         Sets the display states of the single site radar window
*                      overlays from the overlay configuration file.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       March 24, 2004
* ORGANIZATION:        OHD-11 / HSEB
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        3/24/2004    Bryon Lawrence    Original Coding
********************************************************************************
*/

#include "map_defines.h"
#include "map_library.h"
#include "rfcwide.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    set_single_site_overlays
* PURPOSE:        Sets the initial display states (on or off) of the overlays
*                 displayed in the single site radar window.  These are set
*                 from the overlay configuration file which is the same source
*                 of overlay information used by the Hydroview/MPE main
*                 window.
*
* ARGUMENTS:
*    None.
*
* RETURNS:
*    Nothing.
*
* APIs UTILIZED:
*   NAME         HEADER FILE       DESCRIPTION
*   mGetOverlay  map_library.h     Returns a pointer to an overlay structure.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME     DESCRIPTION
*   int               i        Loop index.
*   struct _Overlay * pOverlay Points to the current overlay being processed.
*
* DATA FILES AND/OR DATABASE:
*    None.
*
* ERROR HANDLING:
*    None.
*
********************************************************************************
*/
void set_single_site_overlays ( )
{
   int i ;
   struct _Overlay * pOverlay = NULL ;

   /* The single site radar window gage overlay is always off
      by default. */
   overlay_def.igage = M_OFF ;

   for ( i = M_STATE ; i < M_OVERLAYS ; ++ i )
   {

      pOverlay = mGetOverlay ( ( enum MapOverlays ) i ) ;

      if ( pOverlay != NULL )
      {

         switch ( i )
         {
            case M_STATE :
   
               overlay_def.istate = pOverlay->status ;
               break ;

            case M_CITY_TOWN :

               overlay_def.icity = pOverlay->status ;
               break ;

            case M_RIVERS :

               overlay_def.iriver = pOverlay->status ; 
               break ;

            case M_COUNTY :

               overlay_def.icounty = pOverlay->status ;
               break ;

            case M_BASINS :

               overlay_def.ibasbound = pOverlay->status ;
               break ;

            case M_RADAR_RINGS :

               overlay_def.iradring = pOverlay->status ;
               break ;

            case M_RFC_BOUNDARY :

               overlay_def.irfc = pOverlay->status ; 
               break ;

            default :

               break ;
         }
      }
   } 

}
