/*  This is a test program to create the Tulsa plot menus using
 *     Motif widgets.
 */
#include "menus.h"
#include "ifp_atoms.h"
#include "ifp_struct.h"

void tultable(num_ts_menus, data_len, vis_cols_data, vis_rows_data,
	      ts_menu_names, day_hour, ts_array, orig_ts_array,
	      list_mask, num_ts, cb_data, ptm_data, end_obs_table)
   int          num_ts_menus;    /* number of time series menus */
   int          data_len;
   int          vis_cols_data;   /* visible columns data */
   int          vis_rows_data;   /* visible rows data */
   int          *list_mask;
   int          num_ts;          /* Number of time series used in the current forcast program */
   char         **ts_menu_names; /* time series menu names */
   char         **day_hour;
   float        **ts_array;      /* Address of pointer to the time series array  */
   float        **orig_ts_array; /* Address of pointe to the original time series array */
   tables_cb_struct  *cb_data;   /* call back tables data structure pointer */
   combined_struct   *ptm_data;  /* tables and plot data structure pointer */
{
   int          i, j;            /* counters */
/*
 * Set x,y location of popup shell
 */
  cb_data->popup_shell_x = DEFAULT_POPUP_SHELL_X;
  cb_data->popup_shell_y = DEFAULT_POPUP_SHELL_Y;

/*
 * Set overall size of arrays and visible portion to defaults or,
 * if not set from the call within cex25 program, add one to each
 * dimension for labels.
 */
  if(vis_cols_data < 1)
     cb_data->visible_columns = DEFAULT_VISIBLE_COLS;
  else
     cb_data->visible_columns = vis_cols_data + 1;

  if(vis_rows_data < 1)
     cb_data->visible_rows = DEFAULT_VISIBLE_ROWS;
  else
     cb_data->visible_rows = vis_rows_data + 1;

  if(num_ts_menus < 1)
     cb_data->maximum_columns = DEFAULT_MAX_COLS;
  else
     cb_data->maximum_columns = num_ts_menus + 1;

  if(data_len < 1)
     cb_data->maximum_rows = DEFAULT_MAX_ROWS;
  else
     cb_data->maximum_rows = data_len + 1;

  if(cb_data->popup_shell_x < 0) cb_data->popup_shell_x = 0;
  if(cb_data->popup_shell_x > 1280) cb_data->popup_shell_x = 0;
  if(cb_data->popup_shell_y < 0) cb_data->popup_shell_y = 0;
  if(cb_data->popup_shell_y > 1024) cb_data->popup_shell_y = 0;

/*
 * Allocate space for arrays in cb_data structure <>-<>-<>-<>-<>-<>-<>
 */
/*
 * Alloc space for rc children and set original number of children to zero.
 */
     cb_data->toplevel = NULL;
     cb_data->tultable = NULL;
     cb_data->drawa = NULL;
     cb_data->mainbb = NULL;
     cb_data->v_scrollbar = NULL;
     cb_data->h_scrollbar = NULL;
     cb_data->bb_label_gadget = NULL;
     cb_data->used_bb_names = cb_data->total_bb_names = 0;
     cb_data->bb_name_frames = NULL;
     cb_data->bb_name_labels = NULL;
     cb_data->used_bb_dates = cb_data->total_bb_dates = 0;
     cb_data->bb_date_frames = NULL;
     cb_data->bb_date_labels = NULL;
     cb_data->used_bb_data = cb_data->total_bb_data = 0;
     cb_data->bb_data = NULL;
     cb_data->ts_menu_names = NULL;
     cb_data->day_hour = NULL;
     cb_data->changed = NULL;
     cb_data->data_array = NULL;
     cb_data->first_col_visible = 0;
     if(end_obs_table > 13)
	cb_data->first_row_visible = end_obs_table - 13;
     else
	cb_data->first_row_visible = 0;

     cb_data->bb_name_frames =
	  (Widget *)malloc((cb_data->maximum_columns-1) *
			   sizeof(Widget));

     cb_data->bb_name_labels =
	  (Widget *)malloc((cb_data->maximum_columns-1) *
			   sizeof(Widget));

     cb_data->bb_date_frames =
	  (Widget *)malloc((cb_data->maximum_rows-1) *
			   sizeof(Widget));

     cb_data->bb_date_labels =
	  (Widget *)malloc((cb_data->maximum_rows-1) *
			   sizeof(Widget));

     cb_data->bb_data =
	  (Widget *)malloc((cb_data->maximum_rows-1) *
			   (cb_data->maximum_columns-1) *
			   sizeof(Widget));

     cb_data->data_array = (float **)malloc(num_ts * sizeof(float *));
/*
 * Allocate space for array of flags for changed values.
 */
  cb_data->changed =
     (int **)malloc(num_ts_menus * sizeof(int *));
  for(i=0; i<num_ts_menus; i++)
     cb_data->changed[i] =
          (int *)malloc(cb_data->end[i] * sizeof(int));
/*        (int *)malloc(data_len * sizeof(int)); */
  for(i=0; i<num_ts_menus; i++)
/*   for(j=0; j<data_len; j++) */
     for(j=0; j<cb_data->end[i]; j++)
	cb_data->changed[i][j] = 0;
  cb_data->num_ts_menus = num_ts_menus;
/*
 * All space in cb_data structure now allocated <>-<>-<>-<>-<>-<>-<>
 */
  j=0;
  for(i=0; i<num_ts; i++)
     if(list_mask[i] == 1)
     {
	cb_data->data_array[j++] = ts_array[i];
     }
  cb_data->ts_menu_names = ts_menu_names;
  cb_data->day_hour = day_hour;

/*
 * Create toplevel shell widget
 */
  cb_data->toplevel = global_toplevel;

/*
 * We don't want to see the toplevel shell, so don't map it.
 */
  XtSetMappedWhenManaged(cb_data->toplevel, False);

  cb_data->tultable = NULL;

  resize_drawa(cb_data->tultable, ptm_data, NULL);

  XSelectInput(XtDisplay(cb_data->tultable),
		DefaultRootWindow(XtDisplay(cb_data->tultable)),
				  PropertyChangeMask);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/tultable.c,v $";
 static char rcs_id2[] = "$Id: tultable.c,v 1.3 1997/04/04 15:34:44 page Exp $";}
/*  ===================================================  */

}
