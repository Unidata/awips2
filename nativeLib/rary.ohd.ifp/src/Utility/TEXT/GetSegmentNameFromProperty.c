#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"

getsegmentnamefromproperty(segnam)
	char    segnam[8];
{
  char  *segment_name;
  int   i, type, format, nitems, left;

  memset(segnam, '\0', 8);

  if(global_toplevel == NULL)
    {
     int     argc = 1;
     char    *argv[1];

     argc = 0;
     argv[0] = (char *)malloc(20);
     strcpy(argv[0], "");
/*
 *    printf("The global_toplevel widget is being initialized in ");
 *    printf("GetSegmentNameFromProperty.c\n");
 *    printf("The resources are from 'Forecast_Program'\n");
 */
     global_toplevel = XtInitialize("toplevel",
				    "Forecast_Program",
				    NULL, 0, &argc, argv);
     intern_the_atoms(global_toplevel);
    }

  if(XGetWindowProperty
	  (
	  XtDisplay(global_toplevel),
	  DefaultRootWindow(XtDisplay(global_toplevel)),
	  IFPA_current_segment,
	  (long) 0,
	  (long) 9,
	  FALSE,
	  IFPA_current_segment_type,
	  (Atom *)&type,
	  (int *)&format,
	  (unsigned long *)&nitems,
	  (unsigned long *)&left,
	  (unsigned char **)&segment_name
	  ) == Success && type == IFPA_current_segment_type
    )
     {
/*
 *     printf("Inside GetSegmentNameFromProperty, name = %s\n",
 *             segment_name);
 */
      i = 0;
      while((segment_name[i] != ' ') && (i < 8))
	 {
	  segnam[i] = segment_name[i];
	  i++;
	 }
     }
   else
     {
      printf("**Warning** in GetSegmentNameFromProperty, no segment name posted\n");
      exit(1);
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/GetSegmentNameFromProperty.c,v $";
 static char rcs_id2[] = "$Id: GetSegmentNameFromProperty.c,v 1.2 2006/04/07 16:59:14 aivo Exp $";}
/*  ===================================================  */

}
