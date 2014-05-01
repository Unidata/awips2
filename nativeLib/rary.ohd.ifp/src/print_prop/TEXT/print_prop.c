
#include <X11/Xatom.h>
#include "libXifp.h"
#include "mod_struct.h"
#include "ifp_atoms.h"
Atom TEMP_forecast_group_selected, TEMP_forecast_group_selected_type;

typedef struct
	{
	 char    segment_name[9];
	 int     status_id;
	}       seg_status;

void print_events(toplevel, atom)

Widget  toplevel;
Atom    atom;
{
	Atom    type;
	int     format, nitems, left;
	char    *property_data;
	Display         *dpy;
	Window          root;
	date    *date_property;
	int     *int_property;
	mod_limits_data     *mod_limits_data_property;
	seg_status      *flow_status;

	dpy = XtDisplay(toplevel);
	root = DefaultRootWindow(dpy);

 if(
    XGetWindowProperty
	(
	dpy,
	root,
	atom,
	0L,
	8192L,
	FALSE,
	AnyPropertyType,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&property_data
	) == Success
   )
    {
	/* if the data exists, display if in appropriate format */

     if(property_data == NULL)
       {
	printf("NULL property_data, atom = %d, type = %d\n", atom, type);
	   printf("Data for window property %s set to NULL\n",
		   XGetAtomName( dpy, atom));
       }
     else if(
	  atom == IFPA_run_start_date ||
	  atom == IFPA_run_end_date ||
	  atom == IFPA_run_end_obs_date ||

	  atom == IFPA_mods_start_date ||
	  atom == IFPA_mods_end_date ||
	  atom == IFPA_mods_end_obs_date ||

	  atom == IFPA_display_start_date ||
	  atom == IFPA_display_end_date ||
	  atom == IFPA_display_end_obs_date
	    )
	/*  date type property - print as month/day/year/hour/time_zone  */
       {
	date_property = (date *)property_data;

	printf("Window property %s, %s set to %2d %2d %4d %2d %s.\n",
			       XGetAtomName( dpy, atom),
			       XGetAtomName( dpy, type),
			       date_property->month,
			       date_property->day,
			       date_property->year,
			       date_property->hour,
			       date_property->time_zone);
       }
     else if(
	  atom == XA_CUT_BUFFER0 ||
	  atom == IFPA_run_segments ||
	  atom == IFPA_seg_selected ||
	  atom == IFPA_seg_deleted ||
	  atom == IFPA_status_change ||
	  atom == IFPA_current_segment ||
	  atom == IFPA_forecast_group ||
	  atom == IFPA_next_segment ||
	  atom == IFPA_rerun_segment ||
	  atom == IFPA_goto_upstream_segment ||
	  atom == IFPA_goto_downstream_segment ||
	  atom == IFPA_time_zone_code ||
	  atom == TEMP_forecast_group_selected
	    )
	/*  character type property - print as a string  */
       {
	printf("Window property %s, %s set to %s.\n",
			       XGetAtomName( dpy, atom),
			       XGetAtomName( dpy, type),
			       property_data);
       }
     else if(
	  atom == IFPA_display_selected ||
	  atom == IFPA_mods_quit_ok ||
	  atom == IFPA_begin_NWSRFS ||
	  atom == IFPA_quit_NWSRFS ||
	  atom == IFPA_entering_NWSRFS_eventLoop ||
	  atom == IFPA_current_mod_saved ||
	  atom == IFPA_no_Tulsa_plot ||
	  atom == IFPA_continue_to_next_operation ||
	  atom == IFPA_number_of_TulPlots ||
	  atom == IFPA_mods_API_units ||
	  atom == IFPA_mods_SAC_units ||
	  atom == IFPA_mods_general_units ||
	  atom == IFPA_general_units ||
	  atom == IFPA_number_of_mods_to_write ||
	  atom == IFPA_ok_to_write_mods ||
	  atom == IFPA_Start_IFP_is_running ||
	  atom == IFPA_IFP_NWSRFS_is_running ||
	  atom == IFPA_finished_making_mods ||
	  atom == IFPA_show_tulsa_plot ||
	  atom == IFPA_show_mods ||
	  atom == IFPA_show_tables ||
	  atom == IFPA_done_making_mods ||
	  atom == IFPA_show_operations_table ||
	  atom == IFPA_show_rating_curve ||
	  atom == IFPA_rating_curve_available ||
	  atom == IFPA_num_mods_to_delete_fromFile ||
	  atom == IFPA_tschng_mod ||
	  atom == IFPA_current_mod_saved ||
	  atom == IFPA_end_of_file_initializations ||
	  atom == IFPA_status_change ||
	  atom == IFPA_save_gif_file ||
	  atom == IFPA_save_gif_file_done ||
	  atom == IFPA_first_plot ||
	  atom == IFPA_tot_num_TulPlots ||
	  atom == IFPA_mod_files_updated ||
	  atom == IFPA_nts_mods_available ||
	  atom == IFPA_show_mods_viewer ||
	  atom == IFPA_activate_rerun
	    )
	/*  integer type property - print as an integer  */
       {
	int_property = (int *)property_data;

	printf("Window property %s, %s set to %d\n",
			       XGetAtomName( dpy, atom),
			       XGetAtomName( dpy, type),
			       *int_property);
       }
     else if(
	  atom == IFPA_segment_status
	    )
	/*  segment_status property - print the structure  */
       {
	flow_status = (seg_status *)property_data;

	printf("Window property %s, %s set to %s, %d\n",
			       XGetAtomName( dpy, atom),
			       XGetAtomName( dpy, type),
			       flow_status->segment_name,
			       flow_status->status_id);
       }
     else if(
	  atom == IFPA_mod_limits_data
	    )
	/*  mods_limit_data type property - print the structure  */
       {
	mod_limits_data_property = (mod_limits_data *)property_data;

	printf("Window property %s, %s\n",
			       XGetAtomName( dpy, atom),
			       XGetAtomName( dpy, type));
	printf("  lower_warning_limit = %f, upper_warning_limit = %f\n",
			       mod_limits_data_property->lower_warning_limit,
			       mod_limits_data_property->upper_warning_limit);
	printf("  lower_error_limit = %f, upper_error_limit = %f\n",
			       mod_limits_data_property->lower_error_limit,
			       mod_limits_data_property->upper_error_limit);
	printf("  lower_warning_inclusive = %d, upper_warning_inclusive = %d\n",
			       mod_limits_data_property->lower_warning_inclusive,
			       mod_limits_data_property->upper_warning_inclusive);
	printf("  lower_error_inclusive = %d, upper_error_inclusive = %d\n",
			       mod_limits_data_property->lower_error_inclusive,
			       mod_limits_data_property->upper_error_inclusive);
       }
     else
       {
	printf("Unknown window property, atom = %d, type = %d\n", atom, type);
	printf("Window property %s currently not known to this program.\n",
			       XGetAtomName( dpy, atom));
       }
    }                       /*  end if(XGetWindow...)  */
}

print_prop_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel;
	Window          root;
	XEvent          event;
	Display         *dpy;

toplevel = XtInitialize(argv[0], "Print_prop_events", NULL, 0, &argc, argv);

	dpy = XtDisplay(toplevel);
	root = DefaultRootWindow(dpy);

 intern_the_atoms(toplevel);

TEMP_forecast_group_selected =
	XInternAtom(dpy, "Forecast group selected", False);
TEMP_forecast_group_selected_type =
	XInternAtom(dpy, "Forecast group selected type", False);

/*  XtRealizeWidget(toplevel);  */

XSelectInput(dpy, root, PropertyChangeMask);

while(TRUE){
	XtNextEvent(&event);

	switch (event.type) {
	  case PropertyNotify:

	    print_events(toplevel, event.xproperty.atom);
	     XtDispatchEvent(&event);
	    break;

	default:
	     XtDispatchEvent(&event);
      }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/print_prop/RCS/print_prop.c,v $";
 static char rcs_id2[] = "$Id: print_prop.c,v 1.5 2006/04/07 13:51:21 aivo Exp $";}
/*  ===================================================  */

  }
