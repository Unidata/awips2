#include "libXifp.h"
#include "ifp_atoms.h"

Widget  global_toplevel;

/*----------------------------------------------------------
    post_first_plot_atom(int)
    
    Posts the IFPA_first_plot atom to tell if it is the
    first plot in the segment.  Set to the incoming
    value of *first_plot.
----------------------------------------------------------*/    
    
void post_first_plot_atom(int *first_plot)
{

 XChangeProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_first_plot,
	 IFPA_first_plot_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)first_plot,
	 sizeof(int)
	 );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_first_plot_atom.c,v $";
 static char rcs_id2[] = "$Id: post_first_plot_atom.c,v 1.2 2006/04/07 16:59:38 aivo Exp $";}
/*  ===================================================  */

}
