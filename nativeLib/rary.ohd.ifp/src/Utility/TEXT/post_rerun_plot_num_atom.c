#include "libXifp.h"
#include "ifp_atoms.h"

Widget  global_toplevel;

/*----------------------------------------------------------
    post_rerun_plot_num_atom(int)
    
    Posts the IFPA_rerun_plot_num atom to tell which of
    the Tulsa Plots in a segment the user was in when they 
    chose to Rerun the segment.  Set to the incoming value 
    of *rerun_plot_num.
----------------------------------------------------------*/    
    
void post_rerun_plot_num_atom(int *rerun_plot_num)
{

 XChangeProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_rerun_plot_num,
	 IFPA_rerun_plot_num_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)rerun_plot_num,
	 sizeof(int)
	 );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_rerun_plot_num_atom.c,v $";
 static char rcs_id2[] = "$Id: post_rerun_plot_num_atom.c,v 1.2 2006/04/07 16:59:54 aivo Exp $";}
/*  ===================================================  */

}
