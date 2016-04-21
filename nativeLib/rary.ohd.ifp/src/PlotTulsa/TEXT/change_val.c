#include "menus.h"
#include "ifp_struct.h"

/* File: change_val.c
 *
 * change_val function is called when a text widget's displayed value
 * is changed.  It determines which widget in the display was changed,
 * checks the new value for validity, and puts the new value into the
 * data_array (ts_array).
 *
 */
extern void tschng_mod_made(combined_struct *);
void change_val(w, cb_data, call_data)
   Widget       w;                    /* widget data structure   */
   combined_struct  *cb_data;         /* tables and plot data structure array */
   XmAnyCallbackStruct  *call_data;   /* XmAny call back data structure pointer */

{
   Arg          wargs[5];             /* window resource data structure array */
   char         *new_string;          /* current window text string pointer   */
   char         old_string[14];       /* previous window text string          */
   int          i, j, n;              /* counters                             */
   int          current_column;       /* current plot column position         */
   int          current_row;          /* current plot row position            */
   int          current_row_data;
   int          multiple_factor;
   int          delta_t;

   new_string = XmTextGetString(w);

   if(strcmp(new_string, "q") == 0 || strcmp(new_string, "Q") == 0)
      exit(0);

   for(i=1; i<cb_data->tables->visible_columns; i++)
   {
      delta_t = cb_data->tables->delta_t[cb_data->tables->num_ts_menus - 1];
      for (j=0; j<cb_data->tables->num_ts_menus; j++)
      {
         if (delta_t > cb_data->tables->delta_t[j])
         {
            delta_t = cb_data->tables->delta_t[j];
         }
      }
      multiple_factor = cb_data->tables->delta_t[i-1+cb_data->tables->first_col_visible] / delta_t;
    
      for(j=1; j<cb_data->tables->visible_rows; j++)
      {
	 if(cb_data->tables->bb_data[(j-1) + (i-1)*(cb_data->tables->visible_rows-1)] == w)
         {
	    sprintf(old_string, "%.2f",
		    cb_data->tables->data_array[i+cb_data->tables->first_col_visible-1]
					       [(j+cb_data->tables->first_row_visible-1)/multiple_factor]);
	    if (is_valid_number(new_string) == TRUE
		 && strcmp(new_string, old_string) != 0)
	    {
	       current_column = i+cb_data->tables->first_col_visible;
	       current_row = (j+cb_data->tables->first_row_visible);
               current_row_data = current_row;
               if (multiple_factor != 1)
               {
                  current_row_data = (j+cb_data->tables->first_row_visible)/multiple_factor;
               }
             

	       n=0;
	       XtSetArg(wargs[n], XmNforeground,
			get_pixel_by_name(
		    cb_data->tables->bb_data[(j-1)+(i-1)*(cb_data->tables->visible_rows-1)],
					  EDIT_TEXT_COLOR)); n++;
	       XtSetValues(cb_data->tables->bb_data
				    [(j-1)+(i-1)*(cb_data->tables->visible_rows-1)],
			   wargs, n);
			   
	       sscanf(new_string, "%f",
		      &cb_data->tables->data_array[current_column-1]
                                                  [current_row_data-1]);                                                

	       cb_data->tables->changed[current_column-1][current_row_data-1] = 1;

	       XtFree(new_string);

	       /* call routine to update the plot */
	       update_plot(current_column, cb_data);

	       /* call to routine to reset flag for tschng_mod_made */
	       tschng_mod_made(cb_data);
	    }
	    else
	    {
	       XmTextSetString(
		    cb_data->tables->bb_data[(j-1)+(i-1)*(cb_data->tables->visible_rows-1)],
			       old_string);
	       printf("Number not changed--invalid or same number\n");
	    }
         }
         else 
         {
            if ((multiple_factor != 1) && 
                 ((cb_data->tables->first_row_visible+j)%multiple_factor != 0))
            {
               n=0;
               XtSetArg(wargs[n], XmNeditable, FALSE); n++;
               XtSetValues(cb_data->tables->bb_data
                         [(j-1)+(i-1)*(cb_data->tables->visible_rows-1)],
                          wargs, n);
            }
         }
      }
   }   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/change_val.c,v $";
 static char rcs_id2[] = "$Id: change_val.c,v 1.4 2007/05/16 16:41:43 aivo Exp $";}
/*  ===================================================  */

}
