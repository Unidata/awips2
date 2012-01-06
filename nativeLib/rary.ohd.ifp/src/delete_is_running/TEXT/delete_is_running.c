
#include <X11/Intrinsic.h>
#include "ifp_atoms.h"

delete_is_running_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel;
	Window          root;
	Display         *dpy;

toplevel = XtInitialize(argv[0], "Delete_is_running", NULL, 0, &argc, argv);

 dpy = XtDisplay(toplevel);
 root = DefaultRootWindow(dpy);

 intern_the_atoms(toplevel);

 XDeleteProperty(dpy, root, IFPA_Start_IFP_is_running);
 XSync(dpy, 0);
 XDeleteProperty(dpy, root, IFPA_IFP_NWSRFS_is_running);
 XSync(dpy, 0);
 XDeleteProperty(dpy, root, IFPA_goto_downstream_segment);
 XSync(dpy, 0);
 XFlush(dpy);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/delete_is_running/RCS/delete_is_running.c,v $";
 static char rcs_id2[] = "$Id: delete_is_running.c,v 1.1 1995/09/08 15:01:59 page Exp $";}
/*  ===================================================  */

}
