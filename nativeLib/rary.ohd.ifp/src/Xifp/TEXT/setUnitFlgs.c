/* File: setUnitFlgs.c
 *
 *  function to get the units window properties and put them into
 *  global variables
 *
 */

#include <X11/Intrinsic.h>
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "ifp_globals.h"
#include "techniques.h"

void set_unit_flags()
{
   Atom                    type;
   int                     format;   /* format of the stored data */
   int                     nitems;   /* number of bytes retrieved */
   int                     left;     /* remaining bytes stored in the window */
   long                    atom_offset = 0;
   univ_techniques_struct  *univ_tech;

   if(XGetWindowProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_univ_techniques,
	 atom_offset,
	 (long) sizeof(univ_techniques_struct),
	 FALSE,
	 (Atom)IFPA_univ_techniques_type,
	 (Atom *)&type,
	 (int *)&format,
	 (unsigned long *)&nitems,
	 (unsigned long *)&left,
	 (unsigned char **)&univ_tech
	 ) == Success && type == IFPA_univ_techniques_type)
   {
      NWSRFS_general_units = univ_tech->metric_units;
      mods_general_units = univ_tech->mod_units;
      mods_SAC_units = univ_tech->mod_sac_units;
      mods_API_units = univ_tech->mod_api_units;
   }
   else
   {
      NWSRFS_general_units = 0;     /* default = English units */
      mods_general_units = 0;       /* default = English units */
      mods_SAC_units = 1;           /* default = metric units */
      mods_API_units = 0;           /* default = English units */
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/setUnitFlgs.c,v $";
 static char rcs_id2[] = "$Id: setUnitFlgs.c,v 1.2 2006/04/07 14:11:02 aivo Exp $";}
/*  ===================================================  */

}
