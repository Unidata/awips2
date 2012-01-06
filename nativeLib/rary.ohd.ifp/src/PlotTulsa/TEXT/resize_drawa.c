/* File: resize_drawa.c
 *
 * Creates a wrist watch cursor if the process has not completed.
 *
 * Gets width of vertical scrollbar and height of horizontal scrollbar.
 *
 * Sets location and dimensions of main shell widget.
 *
 * Creates widgets, setting the locations of widgets in bulletin board.
 *
 * Manages all the children of the bulletin board.
 *
 * Sets resources for horizontal and vertical scrollbar based on the
 * number of visible rows and columns.
 *
 * Finds largest width and height of all of the label gadgets,
 * time series name label widgets with frames, date label widgets
 * with frames, and data text widgets.
 *
 */

#include "menus.h"
#include "ifp_struct.h"
#include <X11/cursorfont.h>
extern void  compute_num_vis_cols_rows(tables_cb_struct*);
void  resize_drawa(w, cb_data, call_data)

  Widget        w;                               /* widget data structure */
  combined_struct     *cb_data;                  /* tables and plot call back data structure pointer */
  XmDrawingAreaCallbackStruct       *call_data;
{
  Arg           wargs[10];         /* window resource data structure array */
  int           i, j, k, n;        /* counters */
  int           delta_t;           /* time series time interval*/
  /*int           multiple_factor; */  
  Dimension     width, height, max_width, max_height;    /* widget dimensions */
  Dimension     v_scrollbar_width, h_scrollbar_height;
  Dimension     shadowThickness;
  int           column, row;        /* data text widget positions */
  Position      current_popup_shell_x, current_popup_shell_y;
  Pixel         bb_foreground;      /* bulletin board foreground color */
  static Cursor wrist_watch = (Cursor)0;

if(w != (Widget)NULL)
  {
/*
 * Create a wrist watch cursor if not already done.
 */
   if(wrist_watch == (Cursor)0)
      wrist_watch = XCreateFontCursor(XtDisplay(w), XC_watch);
/*
 * Define the wrist watch cursor for the root window.
 */
   XDefineCursor(XtDisplay(w), DefaultRootWindow(XtDisplay(w)),
		       wrist_watch);
  }

/*
 * If shell and other widgets exist, popdown shell and
 *  unmanage all rc children.
 */
  if(cb_data->tables->tultable != NULL)
    {
     /*
      * Get current location of toplevel shell
      */
     n=0;
     XtSetArg(wargs[n], XmNx, &current_popup_shell_x); n++;
     XtSetArg(wargs[n], XmNy, &current_popup_shell_y); n++;
     XtGetValues(cb_data->tables->tultable, wargs, n);

     cb_data->tables->popup_shell_x = current_popup_shell_x - 11;
     cb_data->tables->popup_shell_y = current_popup_shell_y - 34;

     compute_num_vis_cols_rows(cb_data->tables);

     XtPopdown(cb_data->tables->tultable);

     XtRemoveCallback(cb_data->tables->drawa, XmNresizeCallback,
		resize_drawa, cb_data);
/*
 * Unmanage all widgets associated with form holding row columns
 */
     XtUnmanageChild(cb_data->tables->h_scrollbar);
     XtUnmanageChild(cb_data->tables->v_scrollbar);
     XtUnmanageChild(cb_data->tables->bb_label_gadget);

     XtUnmanageChildren(cb_data->tables->bb_date_frames,
			cb_data->tables->used_bb_dates);

     XtUnmanageChildren(cb_data->tables->bb_name_frames,
			cb_data->tables->used_bb_names);

     XtUnmanageChildren(cb_data->tables->bb_data,
			cb_data->tables->used_bb_data);

     XFlush(XtDisplay(cb_data->tables->tultable));
    }
/*
 * If shell and other widgets don't exist, create them.
 */
if(cb_data->tables->tultable == NULL)
  {
  n=0;
  cb_data->tables->tultable = XtCreatePopupShell("tultable",
					 transientShellWidgetClass,
					 cb_data->tables->toplevel, wargs, n);

  XtSetMappedWhenManaged(cb_data->tables->tultable, False);

  n=0;
  XtSetArg(wargs[n], XmNmarginWidth, 0); n++;
  XtSetArg(wargs[n], XmNmarginHeight, 0); n++;
  cb_data->tables->drawa = XtCreateManagedWidget("drawa",
	      xmDrawingAreaWidgetClass, cb_data->tables->tultable, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNmarginWidth, 0); n++;
  XtSetArg(wargs[n], XmNmarginHeight, 0); n++;
  cb_data->tables->mainbb = XtCreateManagedWidget("mainbb",
	      xmBulletinBoardWidgetClass, cb_data->tables->drawa, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNforeground, &bb_foreground); n++;
  XtGetValues(cb_data->tables->mainbb, wargs, n);

  cb_data->tables->bb_foreground = bb_foreground;

  n=0;
  cb_data->tables->h_scrollbar = XtCreateWidget("h_scrollbar",
		     xmScrollBarWidgetClass, cb_data->tables->mainbb, wargs, n);

  XtAddCallback(cb_data->tables->h_scrollbar, XmNvalueChangedCallback,
		h_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->h_scrollbar, XmNdragCallback,
		h_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->h_scrollbar, XmNtoTopCallback,
		h_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->h_scrollbar, XmNtoBottomCallback,
		h_scrollbar_moved, cb_data);

  n=0;
  cb_data->tables->v_scrollbar = XtCreateWidget("v_scrollbar",
		     xmScrollBarWidgetClass, cb_data->tables->mainbb, wargs, n);

  XtAddCallback(cb_data->tables->v_scrollbar, XmNvalueChangedCallback,
		v_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->v_scrollbar, XmNdragCallback,
		v_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->v_scrollbar, XmNtoTopCallback,
		v_scrollbar_moved, cb_data);
  XtAddCallback(cb_data->tables->v_scrollbar, XmNtoBottomCallback,
		v_scrollbar_moved, cb_data);

  }  /* end if(cb_data->tables->tultable == NULL) */
/*
 * Set resources for horizontal and vertical scrollbars
 *  based on number of visible rows and columns.
 */
  n=0;
  XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(wargs[n], XmNsliderSize, cb_data->tables->visible_columns-1); n++;
  XtSetArg(wargs[n], XmNincrement, 1); n++;
  XtSetArg(wargs[n], XmNmaximum, cb_data->tables->maximum_columns-1); n++;
  XtSetArg(wargs[n], XmNpageIncrement, cb_data->tables->visible_columns-1); n++;
  XtSetArg(wargs[n], XmNvalue, cb_data->tables->first_col_visible); n++;
  XtSetArg(wargs[n], XmNheight, 15); n++;
  XtSetValues(cb_data->tables->h_scrollbar, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(wargs[n], XmNsliderSize, cb_data->tables->visible_rows-1); n++;
  XtSetArg(wargs[n], XmNincrement, 1); n++;
  XtSetArg(wargs[n], XmNmaximum, cb_data->tables->maximum_rows-1); n++;
  XtSetArg(wargs[n], XmNpageIncrement, cb_data->tables->visible_rows-1); n++;
  XtSetArg(wargs[n], XmNvalue, cb_data->tables->first_row_visible); n++;
  XtSetArg(wargs[n], XmNwidth, 15); n++;
  XtSetValues(cb_data->tables->v_scrollbar, wargs, n);

  fill_tables_data(cb_data);
/*
 * Realize main shell so we can get sizes of widgets.
 */
  XtRealizeWidget(cb_data->tables->tultable);
/*
 * Find largest width and height of all of
 *  1. label gadget,
 *  2. time series name label widgets with frames,
 *  3. date label widgets with frames, and
 *  4. data text widgets.
 *   <<< NOTE: Don't use text widgets for now.
 *             Their default width is too wide.
 */
  max_width = max_height = 0;

  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;

  XtGetValues(cb_data->tables->bb_label_gadget, wargs, n);

  if(width > max_width) max_width = width;
  if(height > max_height) max_height = height;

  for(i=0; i<cb_data->tables->used_bb_names; i++)
    {
     XtGetValues(cb_data->tables->bb_name_frames[i], wargs, n);
     if(width > max_width) max_width = width;
     if(height > max_height) max_height = height;
    }

  for(i=0; i<cb_data->tables->used_bb_dates; i++)
    {
     XtGetValues(cb_data->tables->bb_date_frames[i], wargs, n);
     if(width > max_width) max_width = width;
     if(height > max_height) max_height = height;
    }
/*
 * for(i=0; i<cb_data->tables->used_bb_data; i++)
 *   {
 *    XtGetValues(cb_data->tables->bb_data[i], wargs, n);
 *    if(width > max_width) max_width = width;
 *    if(height > max_height) max_height = height;
 *   }
 */
/*
 * Set width and height of all of the above widgets
 *  equal to the max just found.
 * Set size of label widgets to max minus the frame
 *  thickness.  This will set the correct size for
 *  the frame widgets surrounding them.
 */
  n=0;
  XtSetArg(wargs[n], XmNshadowThickness, &shadowThickness); n++;
  XtGetValues(cb_data->tables->bb_date_frames[0], wargs, n);
/*
 * Store dimensions of bulletin board children for use
 *  in v & h scrollbar_moved and change_bb_data.
 */
  cb_data->tables->child_max_width = max_width;
  cb_data->tables->child_max_height = max_height;
  cb_data->tables->child_shadowThickness = shadowThickness;

  n=0;
  XtSetArg(wargs[n], XmNwidth, max_width); n++;
  XtSetArg(wargs[n], XmNheight, max_height); n++;

  XtSetValues(cb_data->tables->bb_label_gadget, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNwidth, max_width - 2*shadowThickness); n++;
  XtSetArg(wargs[n], XmNheight, max_height - 2*shadowThickness); n++;

  for(i=0; i<cb_data->tables->used_bb_names; i++)
    {
     XtSetValues(cb_data->tables->bb_name_labels[i], wargs, n);
    }

  for(i=0; i<cb_data->tables->used_bb_dates; i++)
    {
     XtSetValues(cb_data->tables->bb_date_labels[i], wargs, n);
    }

  n=0;
  XtSetArg(wargs[n], XmNwidth, max_width); n++;
  XtSetArg(wargs[n], XmNheight, max_height); n++;

  for(i=0; i<cb_data->tables->used_bb_data; i++)
    {
     XtSetValues(cb_data->tables->bb_data[i], wargs, n);
    }
/*
 * Get width of vertical scrollbar and height of horizontal scrollbar
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &v_scrollbar_width); n++;
  XtGetValues(cb_data->tables->v_scrollbar, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNheight, &h_scrollbar_height); n++;
  XtGetValues(cb_data->tables->h_scrollbar, wargs, n);
/*
 * Set location and dimensions of main shell widget
 *   based on previous location number of vis rows/columns
 */
  n=0;
  XtSetArg(wargs[n], XmNx, cb_data->tables->popup_shell_x); n++;
  XtSetArg(wargs[n], XmNy, cb_data->tables->popup_shell_y); n++;
  XtSetArg(wargs[n], XmNwidth,
	   cb_data->tables->visible_columns * max_width +
	   v_scrollbar_width); n++;
  XtSetArg(wargs[n], XmNheight,
	   cb_data->tables->visible_rows * max_height +
	   h_scrollbar_height); n++;
  XtSetValues(cb_data->tables->tultable, wargs, n);
/*
 * Set locations of widgets in bulletin board
 *
 * First, put label gadget in upper left corner.
 */
  n=0;
    XtSetArg(wargs[n], XmNx, 0); n++;
    XtSetArg(wargs[n], XmNy, 0); n++;
  XtSetValues(cb_data->tables->bb_label_gadget, wargs, n);
/*
 * Next attach the name and date frame widgets
 */
  for(i=0; i<cb_data->tables->used_bb_names; i++)
    {
     n=0;
     XtSetArg(wargs[n], XmNx, (i+1) * max_width); n++;
     XtSetArg(wargs[n], XmNy, 0); n++;
     XtSetValues(cb_data->tables->bb_name_frames[i], wargs, n);
    }
  for(i=0; i<cb_data->tables->used_bb_dates; i++)
    {
     n=0;
     XtSetArg(wargs[n], XmNx, 0); n++;
     XtSetArg(wargs[n], XmNy, (i+1) * max_height); n++;
     XtSetValues(cb_data->tables->bb_date_frames[i], wargs, n);
    }
/*
 * Next attach data text widgets
 */
  
  delta_t = cb_data->tables->delta_t[cb_data->tables->num_ts_menus-1];
  for(i=0; i<cb_data->tables->num_ts_menus; i++)
  {
     if (delta_t > cb_data->tables->delta_t[i])
     {
        delta_t = cb_data->tables->delta_t[i];
     }
  }
  for(i=0; i<cb_data->tables->used_bb_data; i++)
    {
       column = (i / (cb_data->tables->visible_rows-1)) + 1;
       row = (i % (cb_data->tables->visible_rows-1)) + 1;

       /* multiple_factor = cb_data->tables->delta_t[column-1] / delta_t; */

       n=0;
       XtSetArg(wargs[n], XmNx, column * max_width); n++;
       XtSetArg(wargs[n], XmNy, row * max_height); n++;
       XtSetValues(cb_data->tables->bb_data[i], wargs, n);
    }
/*
 * Place the horizontal scrollbar at the bottom of the row/column widget
 */
  n=0;
    XtSetArg(wargs[n], XmNx, max_width); n++;
    XtSetArg(wargs[n], XmNy, cb_data->tables->visible_rows * max_height); n++;
    XtSetArg(wargs[n], XmNwidth,
		 (cb_data->tables->visible_columns-1) * max_width); n++;
  XtSetValues(cb_data->tables->h_scrollbar, wargs, n);
/*
 * Place the vertical scrollbar to the right of the row/column widget
 */
  n=0;
    XtSetArg(wargs[n], XmNx, cb_data->tables->visible_columns * max_width); n++;
    XtSetArg(wargs[n], XmNy, max_height); n++;
    XtSetArg(wargs[n], XmNheight,
		 (cb_data->tables->visible_rows-1) * max_height); n++;
  XtSetValues(cb_data->tables->v_scrollbar, wargs, n);
/*
 * Manage all children of the bulletin board.
 */
  XtManageChild(cb_data->tables->bb_label_gadget);

  XtManageChildren(cb_data->tables->bb_date_frames,
		     cb_data->tables->used_bb_dates);

  XtManageChildren(cb_data->tables->bb_name_frames,
		     cb_data->tables->used_bb_names);

  XtManageChildren(cb_data->tables->bb_data,
		     cb_data->tables->used_bb_data);

  XtManageChild(cb_data->tables->h_scrollbar);
  XtManageChild(cb_data->tables->v_scrollbar);
/*
 * Popup the shell widget and all its children
 * Didn't do this earlier because changing the size
 * of the shell after it was realized caused it to be
 * drawn twice.  This way it isn't visible until it has
 * the correct dimensions.
 */
  XtPopup(cb_data->tables->tultable, XtGrabNone);

  XtAddCallback(cb_data->tables->drawa, XmNresizeCallback,
		resize_drawa, cb_data);

  if(w != NULL)
    XUndefineCursor(XtDisplay(w), DefaultRootWindow(XtDisplay(w)));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_drawa.c,v $";
 static char rcs_id2[] = "$Id: resize_drawa.c,v 1.3 2006/03/28 20:44:12 aivo Exp $";}
/*  ===================================================  */

}
