#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"

post_nwsrfs_eventloop_atom()

{
 int   entering_eventLoop;

 entering_eventLoop = TRUE;

 XSync(XtDisplay(global_toplevel), 0);
 XChangeProperty(
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_entering_NWSRFS_eventLoop,
	 IFPA_entering_NWSRFS_eventLoop_type,
         8,
	 PropModeReplace,
	 (unsigned char *)&entering_eventLoop,
	 sizeof(int)
	 );
 XSync(XtDisplay(global_toplevel), 0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_nwsrfs_eventloop_atom.c,v $";
 static char rcs_id2[] = "$Id: post_nwsrfs_eventloop_atom.c,v 1.2 2006/04/07 16:59:51 aivo Exp $";}
/*  ===================================================  */

}
