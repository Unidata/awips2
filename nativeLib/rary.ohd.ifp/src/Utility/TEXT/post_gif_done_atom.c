#include "libXifp.h"
#include "ifp_atoms.h"

Widget  global_toplevel;

/*------------------------------------------------------
     post_gif_done_atom(int)
     
     Posts the IFPA_save_gif_file_done atom based on 
     the incoming value of *gif_done.  Used to determine
     if ifp is done saving a gif file.
------------------------------------------------------*/

void post_gif_done_atom(int *gif_done)
{

 XChangeProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_save_gif_file_done,
	 IFPA_save_gif_file_done_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)gif_done,
	 sizeof(int)
	 );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_gif_done_atom.c,v $";
 static char rcs_id2[] = "$Id: post_gif_done_atom.c,v 1.2 2006/04/07 16:59:41 aivo Exp $";}
/*  ===================================================  */

}
