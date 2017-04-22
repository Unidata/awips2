#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"

post_start_ifp_atom()

{
  int           begin_NWSRFS;

 begin_NWSRFS = TRUE;

 XSync(XtDisplay(global_toplevel), 0);
 XChangeProperty(
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_begin_NWSRFS,
	 IFPA_begin_NWSRFS_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&begin_NWSRFS,
	 sizeof(int)
	 );
 XSync(XtDisplay(global_toplevel), 0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_start_ifp_atom.c,v $";
 static char rcs_id2[] = "$Id: post_start_ifp_atom.c,v 1.2 2006/04/19 21:12:00 aivo Exp $";}
/*  ===================================================  */

}
