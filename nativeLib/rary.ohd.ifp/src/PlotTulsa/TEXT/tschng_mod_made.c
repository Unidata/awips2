/* File: tschng_mod_made.c
 *
 * tschng_mod_made compares the orig_ts_array and the ts_array to
 * see if a change was made.  If a change is made, change the window
 * property.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
#include "ifp_atoms.h"
extern void change_val(Widget, combined_struct *, XmAnyCallbackStruct*);

void tschng_mod_made(data)
   combined_struct      *data; /* tables and plot data structure pointer */
{
   int                  i, j;  /* counters */
   int                  tschng_mod_made;  /* status flag */

   tschng_mod_made = FALSE;
   for(i=0; i<data->plot->num_ts; i++)
   {
/*      for(j=0; j<*data->plot->num_pts-1; j++) */
     for(j=0; j<data->plot->end[i]-1; j++) 
	 if(data->plot->ts_array[i][j] != data->plot->orig_ts_array[i][j])
	 {
	    tschng_mod_made = TRUE;
	    XChangeProperty(
		    XtDisplay(global_toplevel),
		    DefaultRootWindow(XtDisplay(global_toplevel)),
		    IFPA_tschng_mod,
		    IFPA_tschng_mod_type,
		    8,
		    PropModeReplace,
		    (unsigned char *)&tschng_mod_made,
		    sizeof(int)
		    );
	    break;
	 }

      if(tschng_mod_made == TRUE) break;
   }

   /* if no changes, set property to false */
   if(tschng_mod_made == FALSE)
      XChangeProperty(
	      XtDisplay(global_toplevel),
	      DefaultRootWindow(XtDisplay(global_toplevel)),
	      IFPA_tschng_mod,
	      IFPA_tschng_mod_type,
	      8,
	      PropModeReplace,
	      (unsigned char *)&tschng_mod_made,
	      sizeof(int)
	      );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/tschng_mod_made.c,v $";
 static char rcs_id2[] = "$Id: tschng_mod_made.c,v 1.3 2006/03/28 20:44:33 aivo Exp $";}
/*  ===================================================  */

}
