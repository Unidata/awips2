#include <Xm/Xm.h>
#include "gageqc_gui.h"
/*#include "rfcwide_callbacks.h"*/

#include "map_library.h"

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
void change_plot ( Widget w,
		   XtPointer client_data,
		   XtPointer call_data )
{
   extern int plot_view ;

   plot_view=(int)client_data;
   send_expose();
   return ;
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
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void send_expose ( )
{
   Boolean split_screen = False;
   extern struct _Map map [ MAX_NUM_MAPS ];

   /* The call to mUpdate map calls the user-supplied redraw routine
      which redraws the MPE Editor screen. */ 

   if ( map [ 1 ].map != NULL )
   {
      split_screen = XtIsManaged ( map [ 1 ].map );
   }

   if ( split_screen == True )
   {
      mUpdateMap ( 1 );
   }
   else
   {
      mUpdateMap ( 0 );
   }
}

void send_legend_expose ( )
{
   Boolean split_screen = False;
   extern struct _Map map [ MAX_NUM_MAPS ];

   if ( map [ 1 ].legend != NULL )
   {
      split_screen = XtIsManaged ( map [ 1 ].legend );
   }
  
   if ( split_screen == True  )
   {
      mUpdateLegend ( 1 );
   }
   else
   {
      mUpdateLegend ( 0 );
   }

}
