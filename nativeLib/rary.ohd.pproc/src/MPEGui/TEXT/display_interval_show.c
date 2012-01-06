#include <Xm/Xm.h>
#include <Xm/Scale.h>

#include "display_accum_show.h"
#include "display_control_interval.h"
#include "help.h"

static void accum_interval_ok ( Widget w , XtPointer clientdata ,
                                XtPointer calldata )
{
    int value ;

    /* Read the slider bar value. */
    XmScaleGetValue ( accum_intervalSC , & value ) ;
    set_multi_hour_interval_val ( value ) ;

    /* Popdown the interval slider GUI. */
    XtPopdown ( accum_intervalControlDS ) ;
    
}

static void accum_interval_cancel ( Widget w , XtPointer clientdata ,
                                    XtPointer calldata )
{
   if ( accum_intervalControlDS != NULL )
   {
      XtPopdown ( accum_intervalControlDS ) ; 
   }
}

static void accum_interval_AddCallbacks ( )
{
   XtAddCallback ( accum_intervalOkPB , XmNactivateCallback ,
                   accum_interval_ok , NULL ) ;
   XtAddCallback ( accum_intervalCancelPB , XmNactivateCallback ,
                   accum_interval_cancel , NULL ) ;
//   XtAddCallback ( accum_intervalHelpPB , XmNactivateCallback ,
//                   popup_help_window , NULL ) ;
}

void accum_interval_show ( Widget w )
{
   if ( ( accum_intervalControlDS == NULL ) || ( ! accum_intervalControlDS ) )
   {
      /* Create all of the widgets. */
      create_accum_intervalControlDS ( w ) ;
    
      /* Set up the callbacks. */
      accum_interval_AddCallbacks ( ) ;
   }

   XtManageChild ( accum_intervalControlFO ) ;
   XtPopup ( accum_intervalControlDS , XtGrabNone ) ;
}
