#include "libXifp.h"
#include "ifp_atoms.h"

/*-------------------------------------------------------------------------
    post_show_mods_viewer_atom(Widget, int)
    
       posts the value of the IFPA_show_mods_viewer atom
-------------------------------------------------------------------------*/       

void post_show_mods_viewer_atom(Widget w, int value)
{
   /* post the IFPA_show_mods_viewer atom - 
      set to incoming value
    */

       XChangeProperty(
         XtDisplay(w),
         DefaultRootWindow(XtDisplay(w)),
         IFPA_show_mods_viewer,
         IFPA_show_mods_viewer_type,
         8,
         PropModeReplace,
         (unsigned char *)&value,
         sizeof(int)
         );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_show_mods_viewer_atom.c,v $";
 static char rcs_id2[] = "$Id: post_show_mods_viewer_atom.c,v 1.2 2006/04/07 17:00:03 aivo Exp $";}
/*  ===================================================  */

}
