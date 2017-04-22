#include <Xm/Xm.h>

#include "gageqc_gui.h"

int gage_char[2];

void change_character ( Widget w,
                        XtPointer data,
                        XtPointer call_data)
{


   if((int)data==2) {

      gage_char[0]=1;
      gage_char[1]=1;

   }

   else if((int)data==0) {
      gage_char[0]=1;
      gage_char[1]=-1;

   }

   else if((int)data==1) {

      gage_char[0]=-1;
      gage_char[1]=1;

   }

   send_expose();

}
