/* File: fill_tables_data.c
 *
 * Fills upper left widget with "  Day  Hour  "and zeroth (top) row with
 * time series names.
 *
 * Creates frame widget as child of main bulletin board then puts label
 * widget into the frame.
 *
 */

#include "menus.h"
#include "ifp_struct.h"

void fill_tables_data(cb_data)
  combined_struct     *cb_data;
{
  Arg           wargs[10];               /* window resource data structure array */
  int           i, j;                    /* counters */
  int           n;                       /* index    */
  char          data_text[15];           /* text used in labels */
  int           delta_t;                 /* time interval of time series */
  int           time_step;               /* time step */
  int           multiple_factor;

  cb_data->tables->used_bb_dates = 0;
  cb_data->tables->used_bb_names = 0;
  cb_data->tables->used_bb_data = 0;

  time_step = cb_data->tables->delta_t[cb_data->tables->num_ts_menus - 1];
  for (i=0; i<cb_data->tables->num_ts_menus; i++)
  {
     if (time_step > cb_data->tables->delta_t[i])
     {
        time_step = cb_data->tables->delta_t[i];
     } 
  }
  for (i=0; i<cb_data->tables->visible_columns; i++)
  {
     for(j=0; j<cb_data->tables->visible_rows; j++)
     {
	if(j == 0)
	{
	   if(i == 0)
	   {
/*
 *  Fill upper left widget with "  Day  Hour  "
 */
	      n=0;
	      XtSetArg(wargs[n], XmNlabelString,
		 XmStringCreate(
				"Day  Hour",
				XmSTRING_DEFAULT_CHARSET)); n++;

	      if(cb_data->tables->bb_label_gadget != NULL)
	       {                                  /* Gadget already exists */
		XtSetValues(cb_data->tables->bb_label_gadget, wargs, n);
	       }
	      else
	       {                                  /* Create label gadget   */
		cb_data->tables->bb_label_gadget = XtCreateWidget("text",
				xmLabelGadgetClass, cb_data->tables->mainbb, wargs, n);
	       }
	   }
	   else
	   {
/*
 *  Fill zeroth (top) row with time series names
 *
 *  First create frame widget as child of mainbb
 *  Then put label widget into frame
 *
 *  If there are not enough frame widgets in the frame widget list
 *   create a new frame and associated label widget.
 *  Otherwise, just use the next available frame widget.
 */
	      if(cb_data->tables->total_bb_names <= cb_data->tables->used_bb_names)
	       {                                  /* Create frame widget         */
		n=0;
		XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
		cb_data->tables->bb_name_frames[cb_data->tables->total_bb_names] =
		       XtCreateWidget("text",
				xmFrameWidgetClass, cb_data->tables->mainbb, wargs, n);
   /* Create associated label widget */
		cb_data->tables->bb_name_labels[cb_data->tables->total_bb_names] =
			XtCreateManagedWidget("text",
			xmLabelGadgetClass,
			cb_data->tables->bb_name_frames[cb_data->tables->total_bb_names],
			wargs, n);
		cb_data->tables->total_bb_names++;
	       }
 /*
  * Put correct text into label widget and increment number of names used.
  */
	      n=0;
	      XtSetArg(wargs[n], XmNlabelString,
		 XmStringCreate(
		  cb_data->tables->ts_menu_names[cb_data->tables->first_col_visible + i-1],
				 XmSTRING_DEFAULT_CHARSET)); n++;

	      XtSetValues(cb_data->tables->bb_name_labels[cb_data->tables->used_bb_names++], wargs, n);
	   }
	}
	else
	{
	   if(i == 0)
	   {
/*
 *  Fill zeroth (leftmost) column with dates
 *
 *  First create frame widget as child of bb_date_frames
 *  Then put label widget into frame
 *
 *  If there are not enough frame widgets in the frame widget list
 *   create a new frame and associated label widget.
 *  Otherwise, just use the next available frame widget.
 */
	      if(cb_data->tables->total_bb_dates <= cb_data->tables->used_bb_dates)
	       {                                  /* Create frame widget         */
		n=0;
		XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
		cb_data->tables->bb_date_frames[cb_data->tables->total_bb_dates] =
		       XtCreateWidget("text",
				xmFrameWidgetClass, cb_data->tables->mainbb, wargs, n);

   /* Create associated label widget */
		cb_data->tables->bb_date_labels[cb_data->tables->total_bb_dates] =
			XtCreateManagedWidget("text",
			xmLabelGadgetClass,
			cb_data->tables->bb_date_frames[cb_data->tables->total_bb_dates],
			wargs, n);
		cb_data->tables->total_bb_dates++;
	       }
 /*
  * Put correct text into label widget and increment number of dates used.
  */
	      n=0;
	      XtSetArg(wargs[n], XmNlabelString,
		 XmStringCreate(
				cb_data->tables->day_hour[cb_data->tables->first_row_visible + j-1 + 1],
				XmSTRING_DEFAULT_CHARSET)); n++;

	      XtSetValues(cb_data->tables->bb_date_labels[cb_data->tables->used_bb_dates++],
			      wargs, n);
	   }
	   else
	   {
/*
 *  Fill first through visible rows and columns with data
 *
 *  If there are not enough data widgets in the data widget list
 *   create a new data and associated label widget.
 *  Otherwise, just use the next available data widget.
 */
	      if(cb_data->tables->total_bb_data <= cb_data->tables->used_bb_data)
		{                                 /* Create data widget */
		 cb_data->tables->bb_data[cb_data->tables->total_bb_data++] =
		     XtCreateWidget("text",
				xmTextWidgetClass, cb_data->tables->mainbb, NULL, 0);
		}
    
              delta_t = cb_data->tables->delta_t[(i-1)+cb_data->tables->first_col_visible];
              multiple_factor = delta_t / time_step;
              if ((cb_data->tables->first_row_visible+j)%multiple_factor == 0)
              {
	         sprintf(data_text, "%.2f",
		         cb_data->tables->data_array[(i-1) + cb_data->tables->first_col_visible]
                                 [((j-1) + cb_data->tables->first_row_visible)/multiple_factor]);                                        

	         XmTextSetString(cb_data->tables->bb_data[cb_data->tables->used_bb_data++],
		     	         data_text);

	         if(cb_data->tables->changed[(i-1) + cb_data->tables->first_col_visible]
                                    [((j-1) + cb_data->tables->first_row_visible)/multiple_factor] == 0)			 
	         {
		    n=0;
		    XtSetArg(wargs[n], XmNforeground, cb_data->tables->bb_foreground);
		    n++;
		    XtSetValues(cb_data->tables->bb_data[cb_data->tables->used_bb_data-1],
			        wargs, n);
	         }
	         else
	         {
		    n=0;
		    XtSetArg(wargs[n], XmNforeground,
	              get_pixel_by_name(cb_data->tables->bb_data[cb_data->tables->used_bb_data-1],
					EDIT_TEXT_COLOR));
		    n++;
		    XtSetValues(cb_data->tables->bb_data[cb_data->tables->used_bb_data-1],
			        wargs, n);
	         }
	         
	         /* only add callback to new widgets */
	         if((cb_data->tables->total_bb_data-1) <= (cb_data->tables->used_bb_data-1))
	            XtAddCallback(cb_data->tables->bb_data[cb_data->tables->used_bb_data-1],
			          XmNactivateCallback, change_val, cb_data);
              }
              else  /* no data for this time period */
              {
                 sprintf(data_text, "%s", " ");
	         XmTextSetString(cb_data->tables->bb_data[cb_data->tables->used_bb_data],
		     	         data_text);
                 
                 /* need to make the widget uneditable when filled with a blank */
                 n=0;
                 XtSetArg(wargs[n], XmNeditable, FALSE); n++;
                 XtSetValues(cb_data->tables->bb_data[cb_data->tables->used_bb_data++],
                             wargs, n);
              }   
	   }
	}
     }
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/fill_tables_data.c,v $";
 static char rcs_id2[] = "$Id: fill_tables_data.c,v 1.2 1997/04/04 15:33:10 page Exp $";}
/*  ===================================================  */

}
