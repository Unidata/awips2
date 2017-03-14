#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"

post_no_tulsa_plot_atom()

{
  int           noTulsaPlot;
  char          *currentSegment, *goto_downstream_segment;
  Atom          type;
  int           format, nitems, left;
  long          atom_offset = 0;

/*
 * If goto_downstream_atom is on and the current segment
 *  is the downstream one, must turn off goto_downstream_atom.
 *
 *  See if goto_downstream_segment atom is set.
 */
  if(XGetWindowProperty
    (
    XtDisplay(global_toplevel),
    DefaultRootWindow(XtDisplay(global_toplevel)),
    IFPA_goto_downstream_segment,
    atom_offset,
    (long) 9,
    FALSE,
    IFPA_goto_downstream_segment_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&goto_downstream_segment
    ) == Success && type == IFPA_goto_downstream_segment_type)
	{
	/*
	 * If goto_downstream atom is set see if the current
	 *  segment is the downstream segment.
	 */
	 if(XGetWindowProperty
	    (
	    XtDisplay(global_toplevel),
	    DefaultRootWindow(XtDisplay(global_toplevel)),
	    IFPA_current_segment,
	    atom_offset,
	    (long) 9,
	    FALSE,
	    IFPA_current_segment_type,
	    (Atom *)&type,
	    (int *)&format,
	    (unsigned long *)&nitems,
	    (unsigned long *)&left,
	    (unsigned char **)&currentSegment
	    ) == Success && type == IFPA_current_segment_type)
	    {
	     if(strcmp(currentSegment,
		       goto_downstream_segment) == 0)
	       {
	     /*
	      * If so, remove the goto_downstream_segment atom
	      *  and continue.
	      */
		XDeleteProperty(XtDisplay(global_toplevel),
				DefaultRootWindow(XtDisplay(global_toplevel)),
				IFPA_goto_downstream_segment);
	       }
	    }
	}
 noTulsaPlot = TRUE;

 XChangeProperty(
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_no_Tulsa_plot,
	 IFPA_no_Tulsa_plot_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&noTulsaPlot,
	 sizeof(int)
	 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_no_Tulsa_plot_atom.c,v $";
 static char rcs_id2[] = "$Id: post_no_Tulsa_plot_atom.c,v 1.2 2006/04/07 16:59:44 aivo Exp $";}
/*  ===================================================  */

}
