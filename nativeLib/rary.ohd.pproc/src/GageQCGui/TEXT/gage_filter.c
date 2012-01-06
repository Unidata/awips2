#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "map.h"
#include "map_library.h"


void new_filter ( Widget w, XtPointer data, XtPointer call_data )
	   
{
   extern float filter_value;

   XmScaleCallbackStruct *cbs= (XmScaleCallbackStruct *) call_data;
   filter_value=(float)cbs->value/100;
   send_expose ( );
}

void new_reverse_filter ( Widget w, XtPointer data, XtPointer call_data )
{

   extern float reverse_filter_value;

   XmScaleCallbackStruct *cbs= (XmScaleCallbackStruct *) call_data;
   reverse_filter_value=(float)cbs->value/100;
   send_expose ( );
}

void new_dm ( Widget w, XtPointer data, XtPointer call_data )
{

   extern int dmvalue;
   extern float pxtemp;

   XmScaleCallbackStruct *cbs= (XmScaleCallbackStruct *) call_data;
   pxtemp=(float)cbs->value/100;
   dmvalue=pxtemp*100*3.28/.55;
   send_expose ( );
}

void new_elevation_filter ( Widget w, XtPointer data, XtPointer call_data )
{
   extern int elevation_filter_value;

   XmScaleCallbackStruct *cbs= (XmScaleCallbackStruct *) call_data;
   elevation_filter_value=cbs->value;
   send_expose ( );
}

void temperature_value_filter ( Widget w, XtPointer client_data, XtPointer call_data )
{
   extern int temperature_filter_value;
   
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *)call_data;
   temperature_filter_value = cbs->value;
   send_expose ( );
}

void temperature_reverse_value_filter ( Widget w,
                                        XtPointer client_data,
                                        XtPointer call_data )
{
   extern int temperature_reverse_filter_value;

   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data;
   temperature_reverse_filter_value = cbs->value;
   send_expose ( );
}

void freezing_value_filter ( Widget w, XtPointer client_data, XtPointer call_data )
{
   extern float freezing_filter_value;
   
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *)call_data;
   freezing_filter_value = (float)cbs->value/10.0;
   send_expose ( );
}

void freezing_reverse_value_filter ( Widget w,
                                     XtPointer client_data,
                                     XtPointer call_data )
{
   extern float freezing_reverse_filter_value;

   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data;
   freezing_reverse_filter_value = (float)cbs->value/10.0;
   send_expose ( );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/gage_filter.c,v $";
 static char rcs_id2[] = "$Id: gage_filter.c,v 1.2 2007/10/18 16:08:58 lawrence Exp $";}
/*  ===================================================  */

}
