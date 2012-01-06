/*=========================================================================*/
/*                         FILE NAME:   post_gage_display.c               */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_gage()                     */
/*                                      close_gage()                       */
/*                                      get_reverse_color()                */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "postX.h"
#include "xs_create_menu_buttons.h"
#include "set_fields.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include <stdlib.h>
#include "post_stage3_interface.h"
#include "get_vip_level.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/**********************************************************************/
/*  FUNCTION NAME:   display_gage()                                   */
/*       FUNCTION:   create display shell to display 7x7 HRAP bins    */
/*                     from Hourly Summed MPE field centered          */
/*                     on gage closest to chosen point                */
/***********************************************************************

Function type:

Called by function:
   display_single_gage

Functions called:
   (callback) close_gage
   get_pixel_by_name
   get_vip_level
   get_reverse_color

******************************************** BEGIN display_gage ************/

void display_gage(num, data)
   int          num;
   draw_struct *data;
{
   Widget       gagerc, gageform, shell;
   Widget       mvlabel[7][7], gageid, gagevalue, gage_quit;
   int          i, j, n, ix, iy, level;
   float        x;
   short        y;
   Arg          wargs[10];
   char         mv[6], gval[12];
   char         *gage_popup_title="7x7_Summed_MPE_centered_on_gage";
   int          color, xwhite, fgcolor;

 /*-------------------------------------------------------------------------*/
 /*     create shell and form widget to display data                        */
 /*-------------------------------------------------------------------------*/

 shell = XtCreatePopupShell("gage_popup", transientShellWidgetClass, toplevel, NULL, 0);

 n=0;
 XtSetArg(wargs[n], XmNdialogTitle, XmStringCreate(gage_popup_title,XmSTRING_DEFAULT_CHARSET)); n++;

 gageform = XtCreateManagedWidget("gageform", xmFormWidgetClass, shell, wargs, n);

 /*------------------------------------------------------------------*/
 /*     create row-column widget to display summed hourly MPE   */
 /*       data                                                       */
 /*------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++;
 XtSetArg(wargs[n], XmNisAligned, TRUE); n++;
 XtSetArg(wargs[n], XmNentryAlignment, XmALIGNMENT_CENTER); n++;
 XtSetArg(wargs[n], XmNnumColumns, 7); n++;
 XtSetArg(wargs[n], XmNadjustLast, FALSE); n++;
 gagerc = XtCreateManagedWidget("gagerc", xmRowColumnWidgetClass, gageform, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     create widget displaying gage id                                    */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate(gage[num].id, XmSTRING_DEFAULT_CHARSET)); n++;
 gageid = XtCreateWidget("gageid", xmLabelWidgetClass, gageform, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     create widget displaying gage value                                 */
 /*-------------------------------------------------------------------------*/

 if(gage[num].gval == -9999.)
 {
   sprintf(gval, "Missing ");
 }
 else
 {
   sprintf(gval, "%.2f in.", gage[num].gval/25.4); /*in inch unit*/  
 }

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate(gval, XmSTRING_DEFAULT_CHARSET)); n++;
 gagevalue = XtCreateWidget("gagevalue", xmLabelWidgetClass, gageform, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     create quit button to close window and add callback                 */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Close", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XtNwidth, 100); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 gage_quit = XtCreateManagedWidget("gage_quit", xmPushButtonWidgetClass, gageform, wargs, n);
 XtAddCallback(gage_quit, XmNactivateCallback, close_gage, shell);
 XtSetKeyboardFocus(gageform, gage_quit);

 /*-------------------------------------------------------------------------*/
 /*     fill each point in row-column                                       */
 /*     background of label will be in color which is same as shown in      */
 /*       legend                                                            */
 /*     text will be in a complementary color (reverse color)               */
 /*     if any 7x7 HRAP bins are beyond rectangle defined in                */
 /*       coord_XXXXX.dat, then they are set to missing                     */
 /*-------------------------------------------------------------------------*/

 xwhite = get_pixel_by_name(data->w, "white");

 for (i=0;i<7;i++)
 for (j=0;j<7;j++)
    {
    ix = gage[num].hrap.x + i - 3;
    iy = gage[num].hrap.y - j + 3;

    if (ix >= MAXX || iy >= MAXY || ix < 0 || iy < 0)
    {
      x = -1.0;
      level = get_vip_level(data->num_levels, data->levels, x);
    }
    else
    {
      x = precip24[iy][ix]/2540.; /*in inch unit*/
      y = precip24[iy][ix]/10; /*in 10mm unit*/
      
      level = get_vip_level(data->num_levels, data->levels, y);
    }

    color = get_pixel_by_name(data->w, color_list_levels[level]);
    fgcolor = get_reverse_color(data->w, color);
    if (x >= 0)
       sprintf(mv, "%.2f", x);
    else
       strcpy(mv, "M");
    n=0;
    XtSetArg(wargs[n], XmNlabelString, XmStringCreate(mv, XmSTRING_DEFAULT_CHARSET)); n++;
    XtSetArg(wargs[n], XmNbackground, color);n++;
    XtSetArg(wargs[n], XmNforeground, fgcolor); n++;
    mvlabel[i][j] = XtCreateWidget("mosaic", xmLabelWidgetClass, gagerc, wargs, n);
    }

 /*-------------------------------------------------------------------------*/
 /*     manage various children and display shell                           */
 /*-------------------------------------------------------------------------*/

 XtManageChildren(*mvlabel, 49);
 XtManageChild(gageid);
 XtManageChild(gagevalue);
 XtPopup(shell, XtGrabNone);
}

/********************************************* END display_gage ************/



