#include <Xm/Xm.h>

#include "gageqc_gui.h"

int contour_flag = -1;
int points_flag = 1;
int grids_flag = -1;
int map_flag = -1;

void display_pcpn_options ( Widget w,
                            XtPointer data,
                            XtPointer client_data)
{

   /* Initialize the display flags. */
   points_flag=-1;
   grids_flag=-1;
   map_flag=-1;
   contour_flag=-1;

   if ( (int) data==0 ) 
   {
      points_flag=1;
   }

   else if ( (int) data==1 )
   {
      grids_flag=1;
   }
   else if ( (int) data==2 )
   {
      map_flag=1;
   }
   else if((int)data==3)
   {
      points_flag=1;
      grids_flag=1;
   }
   else if((int)data==4)
   {
      points_flag=1;
      map_flag=1;
   }
   else if ((int)data==5)
   {
      points_flag=-1;
      contour_flag=1;
   }
   else if ((int)data==6)
   {
      points_flag=1;
      contour_flag=1;
   }
   else if((int)data==7)
   {
      contour_flag=-1;
      points_flag=-1;
      grids_flag=-1;
      map_flag=-1;
   }

   send_expose ( );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
