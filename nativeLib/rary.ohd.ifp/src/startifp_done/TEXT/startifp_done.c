

#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"




startifp_done_main (argc, argv)
	int             argc;
	char            **argv;
{

	Widget          toplevel;
	Display         *display;
	Window          root;
	int             initializationsDone;



 toplevel = XtInitialize (argv[0], "Start_IFP_done", NULL, 0, &argc, argv);

 intern_the_atoms(toplevel);

 display = XtDisplay(toplevel);
 root = DefaultRootWindow(display);

 initializationsDone = TRUE;

 XChangeProperty
	 (
	 display,
	 root,
	 IFPA_end_of_file_initializations,
	 IFPA_end_of_file_initializations_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&initializationsDone,
	 sizeof(int)
	 );

 XFlush(display);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/startifp_done/RCS/startifp_done.c,v $";
 static char rcs_id2[] = "$Id: startifp_done.c,v 1.2 2006/04/07 16:22:26 aivo Exp $";}
/*  ===================================================  */

}



