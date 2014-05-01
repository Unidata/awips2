
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <stdio.h>
#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "map.h"
#include "map_resource.h"
#include "mpe_log_utils.h"


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
Pixmap logo[4];
unsigned int w_logo[4],h_logo[4];
int xh_logo[4],yh_logo[4];

Font font [ 10 ];
XFontStruct * info_font [ 10 ];

void initialize_gageqc_gui ( )
{
   char * fontname = NULL;
   Display * display = NULL;
   Widget map_shell;
   Window root_win;

   display = _get_map_display ( );
   map_shell = _get_map_shell ( );

   root_win = RootWindowOfScreen ( XtScreen ( map_shell ) );

   if(XReadBitmapFile(display,root_win,"/usr/include/X11/bitmaps/left_ptr",
      &w_logo[1],&h_logo[1],&logo[1],&xh_logo[1],&yh_logo[1])!=BitmapSuccess)
   {
     logMessage("could not find /usr/include/X11/bitmaps/left_ptr\n");
   }

   if(XReadBitmapFile(display,root_win,"/usr/include/X11/bitmaps/boxes",
      &w_logo[0],&h_logo[0],&logo[0],&xh_logo[0],&yh_logo[0])!=BitmapSuccess)
   {
     logMessage("could not find /usr/include/X11/bitmaps/boxes\n");
   }

   if(XReadBitmapFile(display,root_win,"/usr/include/X11/bitmaps/cross_weave",
      &w_logo[2],&h_logo[2],&logo[2],&xh_logo[2],&yh_logo[2])!=BitmapSuccess)
   {
     logMessage("could not find /usr/include/X11/bitmaps/cross_weave\n");
   }

   /* Initialize the DailyQC fonts here. */ 
  fontname="*Adobe-Helvetica-Medium-r-normal-*-10-*";
   font[0]=XLoadFont(display,fontname);
   info_font[0]=XQueryFont(display,font[0]);

   fontname="*Adobe-Helvetica-Medium-o-normal-*-10-*";
   font[1]=XLoadFont(display,fontname);
   info_font[1]=XQueryFont(display,font[1]);

   fontname="*Adobe-Helvetica-Medium-r-normal-*-14-*";
   font[2]=XLoadFont(display,fontname);
   info_font[2]=XQueryFont(display,font[2]);

   fontname="*Adobe-Helvetica-Medium-r-normal-*-12-*";
   font[3]=XLoadFont(display,fontname);
   info_font[3]=XQueryFont(display,font[3]);

   fontname="*Adobe-Helvetica-Medium-r-normal-*-10-*";
   font[4]=XLoadFont(display,fontname);
   info_font[4]=XQueryFont(display,font[4]);

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
