#include <X11/Intrinsic.h>
#include "ifp_atoms.h"

/*
 * Argument next_segment_atom added to return whether the
 *  atom received was for the next segment or to goto an
 *  upstream segment.  This is needed when the last segment
 *  in a forecast group does not have a Tulsa plot.  In this
 *  case the only way go continue processing is to quit, 
 *  choose another forecast group, or select an upstream
 *  segment to goto.
 * Modified by gfs - hrl - 3 Oct 1994
 */

Widget  global_toplevel;

wait_for_next_segment_atom(next_segment_atom)

 int             *next_segment_atom;
{
 XEvent          event;
 int             *next_segment;

 XSelectInput(XtDisplay(global_toplevel),
	      DefaultRootWindow(XtDisplay(global_toplevel)),
	      PropertyChangeMask);

 while(TRUE)
   {
	XtNextEvent(&event);
	switch(event.type)
	{
/* **************************************************************************************************** */
	   case PropertyNotify:

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<<  next_segment  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>> */
	      if(event.xproperty.window ==
		 DefaultRootWindow(XtDisplay(global_toplevel)) &&
		 event.xproperty.atom == IFPA_next_segment)
		 {
                  *next_segment_atom = 0;
		  return;
		 }
      /*  <<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<<  goto_upstream_segment  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		      DefaultRootWindow(XtDisplay(global_toplevel)) &&
		      event.xproperty.atom == IFPA_goto_upstream_segment)
		 {
                  *next_segment_atom = 1;
		  return;
		 }

      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
      /*  <<<<<<<<<  quit_NWSRFS  >>>>>>>>>> */
      /*  <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
	      else if(event.xproperty.window ==
		 DefaultRootWindow(XtDisplay(global_toplevel)) &&
		 event.xproperty.atom == IFPA_quit_NWSRFS)
	      {
	         *next_segment_atom = 2;
		 return;
	      }
	      else
		 {
		  XtDispatchEvent(&event);
		 }
	      break;

/* **************************************************************************************************** */
	   default:
		   XtDispatchEvent(&event);
		   break;

	   }    /*  End of 'case' statement */
   }    /*  End of while loop */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/wait_for_next_segment_atom.c,v $";
 static char rcs_id2[] = "$Id: wait_for_next_segment_atom.c,v 1.2 1997/09/23 15:11:14 page Exp $";}
/*  ===================================================  */

}

