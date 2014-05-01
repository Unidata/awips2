/* File: v_scrollbar_moved.c
 *
 * Moves the scrollbar to the top or bottom of the scroll area
 * if a toTop or toBottom scrollbar event occures.
 *
 * Puts proper dates into zeroth column of rc widget and
 * resets the  dimensions of label widget to the proper maximums
 * so that widgets remain lined up.
 *
 */

#include "menus.h"
#include "ifp_struct.h"

void v_scrollbar_moved(w, cb_data, call_data)
   Widget       w;
/* tables_cb_struct    *cb_data;          call back tables data structure pointer */
   combined_struct     *cb_data;        /*call back tables combined structure pointer */
   XmScrollBarCallbackStruct  *call_data;

{
   int  j, n;
   Arg  wargs[5];
/*
 * If a toTop or toBottom scrollbar event, move the scrollbar
 * to the top or bottom of the scroll area
 */
   if(call_data->reason == 7 || call_data->reason == 8)
     {
      n = 0;
      XtSetArg(wargs[n], XmNvalue, call_data->value); n++;
      XtSetValues(w, wargs, n);
     }

/*   printf(" call_data->value is %d\n",call_data->value); */
   cb_data->tables->first_row_visible = call_data->value;
/*
 *  Put proper dates into zeroth column of rc widget.
 *  Also, reset dimensions of label widget to proper maximums so
 *   widgets remain lined up.
 */
   for (j=0; j<cb_data->tables->visible_rows-1; j++)
   {
      n = 0;
      XtSetArg(wargs[n], XmNlabelString,
		XmStringCreate(
		      cb_data->tables->day_hour[j+cb_data->tables->first_row_visible+1],
		      XmSTRING_DEFAULT_CHARSET)); n++;
      XtSetValues(cb_data->tables->bb_date_labels[j],
		  wargs, n);

      n = 0;
      XtSetArg(wargs[n], XmNwidth, cb_data->tables->child_max_width -
				2*cb_data->tables->child_shadowThickness); n++;
      XtSetArg(wargs[n], XmNheight, cb_data->tables->child_max_height -
				2*cb_data->tables->child_shadowThickness); n++;
      XtSetValues(cb_data->tables->bb_date_labels[j],
		  wargs, n);
   }
/*
 *  Move data into 1st through maximum rows and cols of rc widget
 */
   change_bb_data(cb_data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/v_scrollbar_moved.c,v $";
 static char rcs_id2[] = "$Id: v_scrollbar_moved.c,v 1.2 1997/04/04 15:35:25 page Exp $";}
/*  ===================================================  */

}
