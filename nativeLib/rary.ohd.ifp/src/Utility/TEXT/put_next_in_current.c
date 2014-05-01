#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"

put_next_in_current()

{
  int           type, format, nitems, left;
  long          offset = 0;
  char          *segment_name;

  if (XGetWindowProperty
  (
  XtDisplay(global_toplevel),
  DefaultRootWindow(XtDisplay(global_toplevel)),
  IFPA_next_segment,
  offset,
  (long) 8,
  FALSE,
  IFPA_next_segment_type,
  (Atom *)&type,
  (int *)&format,
  (unsigned long *)&nitems,
  (unsigned long *)&left,
  (unsigned char **)&segment_name
  ) == Success && type == IFPA_next_segment_type)
	  {
	   XSync(XtDisplay(global_toplevel), 0);
	   XChangeProperty(
		   XtDisplay(global_toplevel),
		   DefaultRootWindow(XtDisplay(global_toplevel)),
		   IFPA_current_segment,
		   IFPA_current_segment_type,
		   8,
		   PropModeReplace,
		   (unsigned char *) segment_name,
		   strlen(segment_name)
		   );
	   XSync(XtDisplay(global_toplevel), 0);
	  }
  else
     {
      printf("next_segment window property not found - quit\n");
      exit(1);
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/put_next_in_current.c,v $";
 static char rcs_id2[] = "$Id: put_next_in_current.c,v 1.2 2006/04/07 17:00:08 aivo Exp $";}
/*  ===================================================  */

}
