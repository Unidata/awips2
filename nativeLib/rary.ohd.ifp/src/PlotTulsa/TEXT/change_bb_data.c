#include "menus.h"
#include "ifp_struct.h"

/* File: change_bb_data.c
 *
 * Resets dimensions of text widget to proper maximums so that the widgets
 * remain lined up.
 *
 */

void change_bb_data(cb_data)
   combined_struct    *cb_data;  /* Call back data structure */
{
   int          i, j, n; /* counters  */
   int          delta_t;
   int          multiple_factor;
   char         text[14];   /* plot text */
   Arg          wargs[5];   /* window resource data structure array */

   for(i=0; i<cb_data->tables->visible_columns-1; i++)
   {
      delta_t = cb_data->tables->delta_t[cb_data->tables->num_ts_menus - 1];
      for (j=0; j<cb_data->tables->num_ts_menus; j++)
      {
         if (delta_t > cb_data->tables->delta_t[j])
         {
            delta_t = cb_data->tables->delta_t[j];
         }
      }
      multiple_factor = cb_data->tables->delta_t[i+cb_data->tables->first_col_visible] / delta_t;
      
      for (j=0; j<cb_data->tables->visible_rows-1; j++)
      {
         XtRemoveCallback(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
                          XmNactivateCallback, change_val, cb_data);
         
         if ((cb_data->tables->first_row_visible+j+1)%multiple_factor == 0)
         {
	    sprintf(text, "%.2f",
	            cb_data->tables->data_array[i+cb_data->tables->first_col_visible]
	             		      [(j+cb_data->tables->first_row_visible)/multiple_factor]
                    );
            XmTextSetString(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
	                    text);
                           
            XtAddCallback(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
		          XmNactivateCallback, change_val, cb_data);           
                  
	    /* check if the value has been changed, if yes, change color */            
	    if(cb_data->tables->changed[i+cb_data->tables->first_col_visible]
	 	               [(j+cb_data->tables->first_row_visible)/multiple_factor] == 0) 
	    {
	       n=0;
	       XtSetArg(wargs[n], XmNforeground, cb_data->tables->bb_foreground);
	       n++;
	       XtSetArg(wargs[n], XmNeditable, TRUE); n++;
	       XtSetValues(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
			   wargs, n);
	    }
            else
	    {
       	       n=0;
	       XtSetArg(wargs[n], XmNforeground,
		        get_pixel_by_name
			    (cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
		        EDIT_TEXT_COLOR));
	       n++;
	       XtSetArg(wargs[n], XmNeditable, TRUE); n++;
	       XtSetValues(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
			   wargs, n);
	    }
            /*
             *  Reset dimensions of text widget to proper maximums so
             *   widgets remain lined up.
             */
	    n = 0;
            XtSetArg(wargs[n], XmNwidth, cb_data->tables->child_max_width); n++;
	    XtSetArg(wargs[n], XmNheight, cb_data->tables->child_max_height); n++;
	    XtSetValues(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
		        wargs, n);
         }
         else
         {
            sprintf(text, "%s", " ");
            XmTextSetString(
                            cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
                            text
                           );
            n=0;
            XtSetArg(wargs[n], XmNeditable, FALSE); n++;
            XtSetValues(cb_data->tables->bb_data[j+(i*(cb_data->tables->visible_rows-1))],
                        wargs, n);
         }         
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/change_bb_data.c,v $";
 static char rcs_id2[] = "$Id: change_bb_data.c,v 1.2 1997/04/04 15:31:33 page Exp $";}
/*  ===================================================  */

}
