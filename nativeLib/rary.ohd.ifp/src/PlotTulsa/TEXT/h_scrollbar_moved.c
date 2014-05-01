/* File: h_scrollbar_moved.c
 *
 * Moves the scrollbar to the left or right of the scroll area.
 *
 * Puts proper time series names in zeroth row of rc widget.
 *
 * Resets dimensions of label widget to proper maximums so the
 * widgets remain lined up.
 */

#include "menus.h"
#include "ifp_struct.h"

void h_scrollbar_moved(w, cb_data, call_data)
   Widget       w;                           /* widget data structure */
/*   tables_cb_struct    *cb_data;              call back tables structure pointer */
     combined_struct     *cb_data;           /* call back combined structur pointer */
   XmScrollBarCallbackStruct  *call_data;    /* call back scrollbar structure pointer */

{
   int  i, n;           /* counters */
   Arg  wargs[5];       /* window resource data structure array */
/*
 * If a toTop or toBottom scrollbar event, move the scrollbar
 * to the left or right of the scroll area
 */
   if(call_data->reason == 7 || call_data->reason == 8)
     {
      n = 0;
      XtSetArg(wargs[n], XmNvalue, call_data->value); n++;
      XtSetValues(w, wargs, n);
     }

   cb_data->tables->first_col_visible = call_data->value;

 /*printf("call_data->value = %d, first_col_visible = %d\n", 
	  call_data->value, cb_data->tables->first_col_visible);*/
/*
 *  Put proper time series names in zeroth row of rc widget.
 *  Also, reset dimensions of label widget to proper maximums so
 *   widgets remain lined up.
 */
   for (i=0; i<cb_data->tables->visible_columns-1; i++)
   {
      n = 0;
      XtSetArg(wargs[n], XmNlabelString,
		XmStringCreate(
		      cb_data->tables->ts_menu_names[i+cb_data->tables->first_col_visible],
		      XmSTRING_DEFAULT_CHARSET)); n++;
      XtSetValues(cb_data->tables->bb_name_labels[i],
		  wargs, n);

      n = 0;
      XtSetArg(wargs[n], XmNwidth, cb_data->tables->child_max_width -
				2*cb_data->tables->child_shadowThickness); n++;
      XtSetArg(wargs[n], XmNheight, cb_data->tables->child_max_height -
				2*cb_data->tables->child_shadowThickness); n++;
      XtSetValues(cb_data->tables->bb_name_labels[i],
		  wargs, n);
   }
/*
 *  Move data into 1st through maximum rows and cols of rc widget
 */
   change_bb_data(cb_data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/h_scrollbar_moved.c,v $";
 static char rcs_id2[] = "$Id: h_scrollbar_moved.c,v 1.2 1997/04/04 15:33:52 page Exp $";}
/*  ===================================================  */

}
