#include "menus.h"
#include "ifp_struct.h"

/* File: compute_num_vis_cols_rows.c
 *
 * Obtains width and height of shell widget.
 * Gets the margin width and height of drawing area widget.
 * Gets the margin width and height of the bulletin board widget.
 * Gets width of vertical scrollbar and height of horizontal scrollbar.
 * Computes how many columns and rows will fit in the shell.
 *
 */



void  compute_num_vis_cols_rows(cb_data)

  tables_cb_struct     *cb_data;      /* pointer to call back table structure */
{
  Arg           wargs[10];            /* window resource data structure array */
  int           i, n;                 /* counters   */
  Dimension     shell_width;          /* shell width */
  Dimension     shell_height;         /* shell height */
  Dimension     drawa_marginWidth;    /* drawing area margin width  */
  Dimension     drawa_marginHeight;   /* drawing area margin height */
  Dimension     bb_marginWidth;       /* bulletin board width       */
  Dimension     bb_marginHeight;      /* bulletin board height      */
  Dimension     v_scrollbar_width;    /* vertical scrollbar width   */
  Dimension     h_scrollbar_height;   /* horizontal scrollbar height */
  Dimension     width, height;        /* window width & height  */
/*
 * Get width and height of shell widget.
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &shell_width); n++;
  XtSetArg(wargs[n], XmNheight, &shell_height); n++;
  XtGetValues(cb_data->tultable, wargs, n);
/*
 * Get margin width and height of drawa widget.
 */
  n=0;
  XtSetArg(wargs[n], XmNmarginWidth, &drawa_marginWidth); n++;
  XtSetArg(wargs[n], XmNmarginHeight, &drawa_marginHeight); n++;
  XtGetValues(cb_data->drawa, wargs, n);
/*
 * Get margin width and height of bb widget.
 */
  n=0;
  XtSetArg(wargs[n], XmNmarginWidth, &bb_marginWidth); n++;
  XtSetArg(wargs[n], XmNmarginHeight, &bb_marginHeight); n++;
  XtGetValues(cb_data->mainbb, wargs, n);
/*
 * Get width of vertical scrollbar and height of horizontal scrollbar
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &v_scrollbar_width); n++;
  XtGetValues(cb_data->v_scrollbar, wargs, n);

  n=0;
  XtSetArg(wargs[n], XmNheight, &h_scrollbar_height); n++;
  XtGetValues(cb_data->h_scrollbar, wargs, n);
/*
 * Get width and height of label gadget (same for all)
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(cb_data->bb_label_gadget, wargs, n);
/*
 * Now compute how many columns and rows will fit in shell.
 */
  cb_data->visible_columns = 2;
  for(i=2; i<=cb_data->maximum_columns; i++)
     {
      if(i*width +
	 2*(drawa_marginWidth + bb_marginWidth) + v_scrollbar_width
	      > shell_width)
	 break;
      else
	 cb_data->visible_columns = i;
     }

  cb_data->visible_rows = 2;
  for(i=2; i<=cb_data->maximum_rows; i++)
     {
      if(i*height +
	 2*(drawa_marginHeight + bb_marginHeight) + h_scrollbar_height
	      > shell_height)
	 break;
      else
	 cb_data->visible_rows = i;
     }

  if(cb_data->first_col_visible + cb_data->visible_columns >
       cb_data->maximum_columns)
	 cb_data->visible_columns =
	   cb_data->maximum_columns - cb_data->first_col_visible;

  if(cb_data->first_row_visible + cb_data->visible_rows >
       cb_data->maximum_rows)
	 cb_data->visible_rows =
	   cb_data->maximum_rows - cb_data->first_row_visible;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/compute_num_vis_cols_rows.c,v $";
 static char rcs_id2[] = "$Id: compute_num_vis_cols_rows.c,v 1.1 1995/09/08 14:57:04 page Exp $";}
/*  ===================================================  */

}

