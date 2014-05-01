#include <stdlib.h>
#include <Xm/Xm.h>

#include "create_ss_interface_rfcwide.h"
#include "drawa.h"
#include "map_library.h"
#include "read_rresult.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "update_flig_RFCW.h"

void ignore_radar_RFCW( Widget w , XtPointer client_data , 
                        XtPointer call_data )
{
    int ss_number = ( int ) client_data ;
    extern rfcwide_widget_struct * widget_struct_ss ;
    XmString xm_str ;

/*--------------------------------------------------*/
/*  update ignore_radar flag in RWRadarResult table */
/*--------------------------------------------------*/

    update_flig_RFCW ( iflign [ss_number ], nexrad [ ss_number ].id , 
                       datetime ) ;

    if ( iflign [ ss_number ] == IgnoreRadar ) 
    {
       iflign [ ss_number ] = DontIgnoreRadar ;
       xm_str = XmStringCreateLocalized ( "Ignore Radar" ) ;
       XtVaSetValues ( widget_struct_ss [ ss_number ].ignore_widget , 
                       XmNlabelString , xm_str , NULL ) ;
       XmStringFree ( xm_str ) ;
    }
    else
    {
       iflign [ ss_number ] = IgnoreRadar ;
       xm_str = XmStringCreateLocalized ( "Unignore Radar" ) ;
       XtVaSetValues ( widget_struct_ss [ ss_number ].ignore_widget , 
                       XmNlabelString , xm_str , NULL ) ;
       XmStringFree(xm_str);
    }

    /* Force the Hydroview/MPE map to be redrawn so that
       the radar rings may change color to indicate ignored
       and active radar sites. */ 
    mUpdateMap ( 0 ) ;
       

}  /* end function ignore_radar_RFCW */
