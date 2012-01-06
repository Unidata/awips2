#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"

Widget           global_toplevel;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

void getfgroupid(fgroup_id, istat)
char    fgroup_id[9];
int     *istat;
{
long    offset = 0;
int     type, format, nitems, left;
char    *fgroup;

/*  Create global_toplevel widget so we can get window property         */

crwdgt();

/*  Get forecast group from root window properties        */
fgroup_id[9] = '\0'; 
if(XGetWindowProperty(
		     XtDisplay(global_toplevel),
		     DefaultRootWindow(XtDisplay(global_toplevel)),
		     IFPA_forecast_group,
		     offset,
		     8,
		     FALSE,
		     (Atom)IFPA_forecast_group_type,
		     (Atom *)&type,
		     (int *)&format,
		     (unsigned long *)&nitems,
		     (unsigned long *)&left,
		     (unsigned char **)&fgroup
		     ) == Success && type == IFPA_forecast_group_type)
  {
   strncpy(fgroup_id, fgroup, nitems);   
   *istat = 0;
  }
else
  {
   printf("forecast group property not found - run startifp first\n");
   strncpy(fgroup_id,"        ", 8);
   *istat = 1;
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/getfgroupid.c,v $";
 static char rcs_id2[] = "$Id: getfgroupid.c,v 1.1 2006/04/20 16:50:56 aivo Exp $";}
/*  ===================================================  */

}
