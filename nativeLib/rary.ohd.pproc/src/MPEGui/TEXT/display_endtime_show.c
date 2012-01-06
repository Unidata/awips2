#include <Xm/Xm.h>
#include <Xm/Scale.h>

#include "display_accum_show.h"
#include "display_control_endtime.h"
#include "help.h"

static void accum_endtime_ok ( Widget w , XtPointer clientdata ,
                               XtPointer calldata )
{
    int value ;

    /* Read the slider bar value. */
    XmScaleGetValue ( accum_endtimeSC , & value ) ;
    set_multi_hour_endtime_val ( value ) ;

    /* Popdown the endtime slider GUI. */
    XtPopdown ( accum_endtimeControlDS ) ;
    
}

static void accum_endtime_cancel ( Widget w , XtPointer clientdata ,
                                   XtPointer calldata )
{
   if ( accum_endtimeControlDS != NULL )
   {
      XtPopdown ( accum_endtimeControlDS ) ; 
   }
}

static void accum_endtime_AddCallbacks ( )
{
   XtAddCallback ( accum_endtimeOkPB , XmNactivateCallback ,
                   accum_endtime_ok , NULL ) ;
   XtAddCallback ( accum_endtimeCancelPB , XmNactivateCallback ,
                   accum_endtime_cancel , NULL ) ;
   XtAddCallback ( accum_endtimeHelpPB , XmNactivateCallback ,
                   popup_help_window , NULL ) ;
}

void accum_endtime_show ( Widget w )
{
   if ( ( accum_endtimeControlDS == NULL ) || ( ! accum_endtimeControlDS ) )
   {
      /* Create all of the widgets. */
      create_accum_endtimeControlDS ( w ) ;
    
      /* Set up the callbacks. */
      accum_endtime_AddCallbacks ( ) ;
   }

   XtManageChild ( accum_endtimeControlFO ) ;
   XtPopup ( accum_endtimeControlDS , XtGrabNone ) ;
}
