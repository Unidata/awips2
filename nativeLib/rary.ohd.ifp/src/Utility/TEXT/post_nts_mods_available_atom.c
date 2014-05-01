#include "libXifp.h"
#include "ifp_atoms.h"

void post_nts_mods_available_atom(Widget w, int value)
{
   /* post the IFPA_nts_mods_available atom - 
      set to value
    */

       XChangeProperty(
         XtDisplay(w),
         DefaultRootWindow(XtDisplay(w)),
         IFPA_nts_mods_available,
         IFPA_nts_mods_available_type,
         8,
         PropModeReplace,
         (unsigned char *)&value,
         sizeof(int)
         );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_nts_mods_available_atom.c,v $";
 static char rcs_id2[] = "$Id: post_nts_mods_available_atom.c,v 1.2 2006/04/07 16:59:48 aivo Exp $";}
/*  ===================================================  */

}
