/*
 * Function to post the current segment to an X-window property.
 * Function written by George Smith, February 1991 -- HRL.
 *
 * Segment name comes in as an 8 character Fortran variable.
 * Must remove any trailing blanks before posting property.
 */
#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"

Widget  global_toplevel;

post_current_segment(name, first_time)

char    *name;
int     *first_time;
{
int     i;
char    *posted_segment;

if((*first_time)++ < 1)crwdgt();

posted_segment = (char *)malloc(9 * sizeof(char));

memset(posted_segment, '\0', 9);

for (i = 0; i < 8; i++)
   {
    if(name[i] == ' ')
       break;
    else
       posted_segment[i] = name[i];
   }

/*if(strcmp(posted_segment, NULL) == 0)-- Changed by AV--*/
if(posted_segment == NULL)
   printf("Warning - Blank current segment name is being posted.\n");

XChangeProperty
	(
	XtDisplay(global_toplevel),
	DefaultRootWindow(XtDisplay(global_toplevel)),
	IFPA_current_segment,
	IFPA_current_segment_type,
	8,
	PropModeReplace,
	posted_segment,
	8
	);

/*  XFlush(XtDisplay(global_toplevel));  */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_curseg.c,v $";
 static char rcs_id2[] = "$Id: post_curseg.c,v 1.2 2002/02/12 15:26:21 dws Exp $";}
/*  ===================================================  */

}
