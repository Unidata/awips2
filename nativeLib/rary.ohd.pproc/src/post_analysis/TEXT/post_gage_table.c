/*=========================================================================*/
/*               FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   create_gage_table()                */
/*                                      display_no_gage()                  */
/*                                      sort()                             */
/*                                      sort_by_gageval()                  */
/*                                      sort_by_diff()                     */
/*                                      sort_by_ratio()                    */
/*                                      sort_by_gageid()                   */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include <Xm/CascadeB.h>

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
#include "libXs.h"
#include "menus.h"

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

cb_struct       cb_data;

static xs_menu_struct Control_menu_struct[] =
   {
   {"Quit", un_map_shell, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"Recompute gage-only", redo_gageonly, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
   };

static xs_menu_struct Sort_menu_struct[] =
   {
   {"by Gage Value",       sort, (caddr_t)0,    TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"by Difference",       sort, (caddr_t)1,    TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"by Ratio",            sort, (caddr_t)2,    TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"by Gage ID",          sort, (caddr_t)4,    TRUE, PUSH_BUTTON, NULL, 0, NULL}
   };

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   create_gage_table()                                   */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   display_no_gage
   (callback) v_scrollbar_moved
   (callback) edit_gage_value

******************************************** BEGIN create_gage_table *******/

void create_gage_table(data)
   draw_struct *data;
{
   Widget       grtable, mainbb,  form_w, rc,
		scr_win, v_scrollbar;
   XmString     xmstr;
   Display     *dpy;
   Arg          wargs[10];
   int          i, j, n;
   Dimension    rc_width, rc_height, width, height;
   Dimension    v_scrollbar_width;
   Position     rc_y;
   Widget       Control_gtMenuItem, Sort_gtMenuItem, gt_cascade[2];
   Widget       menuBar;
   Widget       Help_mainMenuItem;

 if (ngages == 0)
    {
    display_no_gage();
    return;
    }

 /*-------------------------------------------------------------------------*/
 /*     Set overall size of arrays and visible portion to defaults          */
 /*-------------------------------------------------------------------------*/

 cb_data.visible_columns = DEFAULT_VISIBLE_COLS;
 cb_data.visible_rows = DEFAULT_VISIBLE_ROWS;
 /*cb_data.maximum_columns = DEFAULT_MAX_COLS;*/
 cb_data.maximum_columns = 6;
 cb_data.maximum_rows=ngages+1;

 /*-------------------------------------------------------------------------*/
 /*     Check for valid ranges of values                                    */
 /*-------------------------------------------------------------------------*/

 if(cb_data.visible_columns < 2) cb_data.visible_columns = 2;
 if(cb_data.visible_columns > cb_data.maximum_columns) cb_data.visible_columns = cb_data.maximum_columns;
 if(cb_data.visible_rows < 2) cb_data.visible_rows = 2;
 if(cb_data.visible_rows > cb_data.maximum_rows) cb_data.visible_rows = cb_data.maximum_rows;

 /*-------------------------------------------------------------------------*/
 /*     Allocate space for arrays in cb_data structure                      *    <>-<>-<>-<>-<>-<>-<>   */
 /*-------------------------------------------------------------------------*/

  cb_data.text_w =
     (Widget **)malloc(cb_data.maximum_columns * sizeof(Widget *));
  for(i=0; i<cb_data.maximum_columns; i++)
     cb_data.text_w[i] =
	  (Widget *)malloc(cb_data.maximum_rows * sizeof(Widget));

  cb_data.labels =
     (Widget **)malloc(cb_data.maximum_columns * sizeof(Widget *));
  for(i=0; i<cb_data.maximum_columns; i++)
     cb_data.labels[i] =
	  (Widget *)malloc(cb_data.maximum_rows * sizeof(Widget));

  cb_data.gage_table_children =
     (Widget *)malloc(cb_data.maximum_columns * cb_data.maximum_rows * sizeof(Widget));

  cb_data.data_array =
      (float **)malloc((cb_data.maximum_columns) * sizeof(int *));
  for(i=0; i<cb_data.maximum_columns; i++)
      cb_data.data_array[i] =
	  (float *)malloc((cb_data.maximum_rows) * sizeof(int));

  cb_data.headings =
     (char **)malloc((cb_data.maximum_columns) * sizeof(char *));
  for(i=0; i<cb_data.maximum_columns; i++)
      cb_data.headings[i] = (char *)malloc(14 * sizeof(char));

  cb_data.tdata =
	(char **)malloc(cb_data.maximum_rows * sizeof(char *));
  for(i=0; i<cb_data.maximum_columns; i++)
      cb_data.tdata[i] = (char *)malloc(10 * sizeof(char));

 /*-------------------------------------------------------------------------*/
 /*     All space in cb_data structure now allocated                        *    <>-<>-<>-<>-<>-<>-<>   */
 /*-------------------------------------------------------------------------*/

 /*-------------------------------------------------------------------------*/
 /*     Alloc space for horizontal label widgets                            */
 /*-------------------------------------------------------------------------*/

  cb_data.h_labels =
       (Widget *)malloc((cb_data.maximum_columns-1) * sizeof(Widget));

  strcpy(cb_data.headings[0], "GageID");
  strcpy(cb_data.headings[1], "Gage");
  strcpy(cb_data.headings[2], "Best");
  strcpy(cb_data.headings[3], "Low");
  strcpy(cb_data.headings[4], "High");
  strcpy(cb_data.headings[5], "Edit");

  cb_data.first_row_visible = 0;
  cb_data.first_col_visible = 0;


  grtable = XtCreatePopupShell("gage_table_shell",
			       transientShellWidgetClass,
			       toplevel, NULL, 0);

 /*-------------------------------------------------------------------------*/
 /*     Do not map the main shell when it is managed (initially realized)   */
 /*     because we then change the size based on the size of the            */
 /*     row column widget.  If we let it be mapped when realized and        */
 /*     then change size, the shell flashes twice on the screen, once       */
 /*     in the original and again in the changed size.                      */
 /*     Note that we map it just before calling XtMainLoop().               */
 /*-------------------------------------------------------------------------*/

  XtSetMappedWhenManaged(grtable, False);

  dpy = XtDisplay(grtable);

  mainbb = XtCreateManagedWidget("mainbb",
	      xmBulletinBoardWidgetClass, grtable, NULL, 0);


  n=0;
  form_w = XtCreateManagedWidget("form_w",
	      xmFormWidgetClass, mainbb, wargs, n);

   n=0;
   XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
   XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
   menuBar = XmCreateMenuBar(form_w, "gage_table_menuBar", wargs, n);
   XtManageChild(menuBar);

 XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET));
 Help_mainMenuItem = XmCreateCascadeButton(menuBar, "Help_mainMenuItem", wargs, 1);

 XtSetArg(wargs[0], XmNmenuHelpWidget,Help_mainMenuItem );
 XtSetValues(menuBar, wargs, 1);
 XtAddCallback(Help_mainMenuItem, XmNactivateCallback,
	   popup_help_window, "POSTGAGETABLE");

 XtManageChild(Help_mainMenuItem);

 /*-------------------------------------------------------------------------*/
 /*     Create the pulldown main menus...                                   */
 /*-------------------------------------------------------------------------*/

  Control_gtMenuItem = XmCreatePulldownMenu(menuBar,
	      "Control_gtMenuItem", NULL, 0);

  Sort_gtMenuItem = XmCreatePulldownMenu(menuBar,
		      "Sort_gtMenuItem", NULL, 0);

  XtSetArg(wargs[0], XmNsubMenuId, Control_gtMenuItem);
  XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Control",
		      XmSTRING_DEFAULT_CHARSET));
  gt_cascade[0] = XmCreateCascadeButton(menuBar,
		  "gtControl_cascade", wargs, 2);

  XtSetArg(wargs[0], XmNsubMenuId, Sort_gtMenuItem);
  XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Sort Gages",
				    XmSTRING_DEFAULT_CHARSET));
  gt_cascade[1] = XmCreateCascadeButton(menuBar,
		"gtSort_cascade", wargs, 2);

  XtManageChildren(gt_cascade,2);

  Control_menu_struct[0].data =  (caddr_t) grtable;
  Control_menu_struct[1].data =  (caddr_t) data;
  xs_create_menu_buttons("", Control_gtMenuItem,
	  Control_menu_struct, XtNumber(Control_menu_struct));
  xs_create_menu_buttons("", Sort_gtMenuItem,
	  Sort_menu_struct, XtNumber(Sort_menu_struct));


  n=0;
  XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(wargs[n], XmNsliderSize, cb_data.visible_rows-1); n++;
  XtSetArg(wargs[n], XmNincrement, 1); n++;
  XtSetArg(wargs[n], XmNmaximum, cb_data.maximum_rows-1); n++;
  XtSetArg(wargs[n], XmNpageIncrement, cb_data.visible_rows-1); n++;

  v_scrollbar = XtCreateManagedWidget("v_scrollbar",
		     xmScrollBarWidgetClass, form_w, wargs, n);

  XtAddCallback(v_scrollbar, XmNvalueChangedCallback,
		v_scrollbar_moved, &cb_data);
  XtAddCallback(v_scrollbar, XmNdragCallback,
		v_scrollbar_moved, &cb_data);
  XtAddCallback(v_scrollbar, XmNtoTopCallback,
		v_scrollbar_moved, &cb_data);
  XtAddCallback(v_scrollbar, XmNtoBottomCallback,
		v_scrollbar_moved, &cb_data);

  n=0;
  scr_win = XtCreateManagedWidget("scr_win",
		    xmScrolledWindowWidgetClass, mainbb, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;
  rc = XtCreateManagedWidget("rc",
	      xmRowColumnWidgetClass, form_w, wargs, n);
 /*-------------------------------------------------------------------------*/
 /*     Set the packing and number of columns for the rc widget             */
 /*-------------------------------------------------------------------------*/

  n=0;
  XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++;
  XtSetArg(wargs[n], XmNnumColumns, cb_data.visible_columns); n++;
  XtSetValues(rc, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     Set properties for the scrolled window                              */
 /*     Attach horizontal and vertical scrollbars, and                      */
 /*     make the row column widget the work window                          */
 /*-------------------------------------------------------------------------*/

  n=0;
  XtSetArg(wargs[n], XmNverticalScrollBar, v_scrollbar); n++;
  XtSetArg(wargs[n], XmNworkWindow, rc); n++;
  XtSetValues(scr_win, wargs, n);

  cb_data.rc = rc;

   cb_data.num_children = 0;

     for(i=0; i<cb_data.visible_columns; i++)
     {
       for (j=0; j<cb_data.visible_rows; j++)
	 {
	  if(j == 0)
	   {
 /*-------------------------------------------------------------------------*/
 /*     Fill zeroth (top) row with column identifiers                       */
 /*                                                                         */
 /*     First create frame widget as child or rc                            */
 /*     Then put label widget into frame                                    */
 /*-------------------------------------------------------------------------*/
	      n=0;
	      XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
	      cb_data.text_w[i][j] = XtCreateWidget("text",
				xmFrameWidgetClass, rc, wargs, n);
	      cb_data.gage_table_children[cb_data.num_children++] =
		       cb_data.text_w[i][j];
	      n=0;
	      xmstr = XmStringCreate(cb_data.headings[i],
			       XmSTRING_DEFAULT_CHARSET);
	      XtSetArg(wargs[n], XmNlabelString, xmstr); n++;
	      cb_data.h_labels[i-1] = XtCreateManagedWidget("text",
				xmLabelGadgetClass,
				cb_data.text_w[i][j], wargs, n);
	   }
	  else
	   {
	    if(i < cb_data.visible_columns-1)
	     {
 /*-------------------------------------------------------------------------*/
 /*     Fill row/column widget with gage structure values                   */
 /*                                                                         */
 /*     First create frame widget as child or rc                            */
 /*     Then put label widget into frame                                    */
 /*-------------------------------------------------------------------------*/
	      n=0;
	      XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
	      cb_data.text_w[i][j] = XtCreateWidget("text",
				xmFrameWidgetClass, rc, wargs, n);

	      cb_data.gage_table_children[cb_data.num_children++] =
		       cb_data.text_w[i][j];

	      n=0;
	      cb_data.tdata[i]=table_data(i,j-1);
	      XtSetArg(wargs[n], XmNlabelString,
	      XmStringCreate(
			     cb_data.tdata[i],
			     XmSTRING_DEFAULT_CHARSET)); n++;
	      cb_data.labels[i][j] = XtCreateManagedWidget("text",
				xmLabelGadgetClass,
				cb_data.text_w[i][j], wargs, n);
	   }
	   else
	   {
 /*-------------------------------------------------------------------------*/
 /*     Fill edit columns with text widget and data                         */
 /*-------------------------------------------------------------------------*/

	      n=0;

	      cb_data.text_w[i][j] = XtCreateWidget("gage_edit_text",
				xmTextWidgetClass, rc, wargs, n);
	      cb_data.gage_table_children[cb_data.num_children++] =
		       cb_data.text_w[i][j];
	      cb_data.tdata[i]=table_data(i,j-1);
	      XmTextSetString(cb_data.text_w[i][j], cb_data.tdata[i]);
	      XtAddCallback(cb_data.text_w[i][j], XmNvalueChangedCallback,
		  edit_gage_value, &cb_data);
	   }
	}
     }
  }

  XtManageChildren(cb_data.gage_table_children,cb_data.num_children);

  XtRealizeWidget(grtable);

 /*-------------------------------------------------------------------------*/
 /*     Get width and height of row/column widget                           */
 /*     Width and height aren't set until widget is realized                */
 /*-------------------------------------------------------------------------*/

  n=0;
  XtSetArg(wargs[n], XmNwidth, &rc_width); n++;
  XtSetArg(wargs[n], XmNheight, &rc_height); n++;
  XtSetArg(wargs[n], XmNy,&rc_y);n++;
  XtGetValues(rc, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     Set width and height of main shell widget based on rc size          */
 /*-------------------------------------------------------------------------*/

  n=0;
  XtSetArg(wargs[n], XmNwidth, rc_width+60); n++;
  XtSetArg(wargs[n], XmNheight, rc_height*1.6); n++;
  XtSetValues(grtable, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(grtable, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNwidth, width-5); n++;
  XtSetArg(wargs[n], XmNheight, height-20); n++;
  XtSetValues(form_w, wargs, n);
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(form_w, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     Get the width  of the vertical and                                  */
 /*             height of the horizontal scrollbars                         */
 /*     for use in positioning the scrollbars in the form widget            */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &v_scrollbar_width); n++;
 XtGetValues(v_scrollbar, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     Place the vertical scrollbar to the right of the row/column widget  */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNleftWidget, rc); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;
 XtSetArg(wargs[n], XmNtopOffset, rc_height/cb_data.visible_rows); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetValues(v_scrollbar, wargs, n);

 /*-------------------------------------------------------------------------*/
 /*     Map the main shell widget and all its children                      */
 /*     Didn't do this earlier because changing the size                    */
 /*     of the shell after it was realized caused it to be                  */
 /*     drawn twice.  This way it isn't visible until it has                */
 /*     the correct dimensions                                              */
 /*-------------------------------------------------------------------------*/

 /*
 XMapRaised(dpy, XtWindow(grtable));
 XMapSubwindows(dpy, XtWindow(grtable));
 */
 XtPopup(grtable,XtGrabNone);
}

/********************************************* END create_gage_table *******/



/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c                             */
/*  FUNCTION NAME:   display_no_gage()                                     */
/*       FUNCTION:   popup window signifying no gages reporting for        */
/*                   current hour                                          */
/***************************************************************************

Function type:
   void local

Called by function:

Functions called:
   (callback) close_shell

******************************************** BEGIN display_no_gage *********/

void display_no_gage()
{
   Widget       grtable, mainbb, msg_w, ok_button;
   int          n;
   char         str[40];
   XmString     msg;
   Arg          wargs[5];

 grtable = XtCreatePopupShell("no_gage_shell", transientShellWidgetClass, toplevel, NULL, 0);

 mainbb = XtCreateManagedWidget("mainbb", xmBulletinBoardWidgetClass, grtable, NULL, 0);

 n=0;
 strcpy(str, "No gage data available");
 msg = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 XtSetArg(wargs[n], XtNx, 20); n++;
 XtSetArg(wargs[n], XtNy, 20); n++;
 msg_w = XtCreateManagedWidget("msg_w", xmLabelWidgetClass, mainbb, wargs, n++);

 n = 0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 ok_button = XtCreateManagedWidget("gage_ok", xmPushButtonWidgetClass, mainbb, wargs, n);

 XtAddCallback(ok_button, XmNactivateCallback, close_shell, grtable);
 XtSetKeyboardFocus(mainbb, ok_button);

 XtPopup(grtable, XtGrabNone);
}

/********************************************* END display_no_gage *********/



/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   sort()                                                */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   qsort
   change_rc

******************************************** BEGIN sort ********************/

void sort(w, data, call_data)
   Widget       w;
   int          data;
   caddr_t     *call_data;
{

      if (data == 0)
    qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct), sort_by_gageval);

 else if (data == 1)
    qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct), sort_by_diff);

 else if (data == 2)
    qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct), sort_by_ratio);

 else if (data == 4)
    qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct), sort_by_gageid);

 change_rc(&cb_data);
}

/********************************************* END sort ********************/



/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   sort_by_gageval()                                     */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   integer

Called by function:

Functions called:

******************************************** BEGIN sort_by_gageval *********/

int sort_by_gageval(gage1, gage2)
   gage_struct *gage1, *gage2;
{
      if (gage1->gval > gage2->gval) return(-1);
 else if (gage1->gval < gage2->gval) return (1);
 return (0);
}

/********************************************* END sort_by_gageval *********/



/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   sort_by_diff()                                        */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   integer

Called by function:

******************************************** BEGIN sort_by_diff ************/

int sort_by_diff(gage1, gage2)
   gage_struct *gage1, *gage2;
{
   double       val1, val2;
   int          n;

 val1 = fabs((double)gage1->gval - (double)gage1->ms);
 val2 = fabs((double)gage2->gval - (double)gage2->ms);

      if (val1 > val2) n = -1;
 else if (val1 < val2) n = 1;
 else                  n = 0;

 return (n);
}

/********************************************* END sort_by_diff ************/



/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   sort_by_ratio()                                       */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   integer

Called by function:

Functions called:
   none

******************************************** BEGIN sort_by_ratio ***********/

int sort_by_ratio(gage1, gage2)
   gage_struct *gage1, *gage2;
{
   float        val1, val2;

 val1 = gage1->gval/gage1->ms;
 if (gage1->ms/gage1->gval > val1) val1 = gage1->ms/gage1->gval;
 if (gage1->ms < 0.01 && gage1->gval < 0.01) val1 = 0.0;
 val2 = gage2->gval/gage2->ms;
 if (gage2->ms/gage2->gval > val2) val2 = gage2->ms/gage2->gval;
 if (gage2->ms < 0.01 && gage2->gval < 0.01) val2 = 0.0;
 if (val1 > val2) return (-1);
 if (val1 < val2) return (1);
 return (0);
}

/********************************************* END sort_by_ratio ***********/


/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_gage_table.c               */
/*  FUNCTION NAME:   sort_by_gageid()                                      */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   integer

Called by function:

Functions called:
   none

******************************************** BEGIN sort_by_gageid **********/

int sort_by_gageid(gage1, gage2)
   gage_struct *gage1, *gage2;
{
 return(strncmp(gage1->id, gage2->id, strlen(gage1->id)));


}

/********************************************* END sort_by_gageid **********/
