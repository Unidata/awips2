#include <Xm/Xm.h>

#include "gageqc_gui.h"

extern Widget z_widget;
extern int edit_stations_flag;
extern int flf_on;

int abmode = 2;

void change_z_edit_mode(Widget w,
                        XtPointer data,
                        XtPointer call_data)

{

   XtDestroyWidget(z_widget);

   z_widget=NULL;
   edit_stations_flag=-1;
   flf_on=-1;

   send_expose();
}

void change_abmode(Widget w,
		   XtPointer data,
		   XtPointer call_data )
{
   abmode=(int)data;
   send_expose();
}

