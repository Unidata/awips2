#include "libXifp.h"
#include "ifp_atoms.h"

Widget  global_toplevel;

/*----------------------------------------------------------
    post_fgmod_files_updated_atom(int)
    
    Posts the IFPA_mod_files_updated atom to tell if the
    mod files have been written during the run.  Set to the 
    incoming value of *fgmod_files_updated.  For new Mods
    interface.  dp - 4 Oct. 95
----------------------------------------------------------*/    
    
void post_fgmod_files_updated_atom(int *fgmod_files_updated)
{
 XChangeProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_fgmod_files_updated,
	 IFPA_fgmod_files_updated_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)fgmod_files_updated,
	 sizeof(int)
	 );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/post_fgmod_files_updated_atom.c,v $";
 static char rcs_id2[] = "$Id: post_fgmod_files_updated_atom.c,v 1.4 2006/04/18 15:29:29 aivo Exp $";}
/*  ===================================================  */

}

