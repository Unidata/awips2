/* File: crwdgt.c
 *
 *  Establishes a connection with the X server and initializes the
 *  X resource manager resource database.
 *
 *  Opens the top level window, Run_time_mods.
 *
 *  Creates the global_toplevel atom.
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>

Widget  global_toplevel;

void crwdgt()
{
int     argc = 1;
char    *argv[1];

argc = 0;
argv[0] = (char *)malloc(20);
strcpy(argv[0], "");

global_toplevel = XtInitialize("toplevel", "Run_time_mods", NULL, 0,
			       &argc, argv);
intern_the_atoms(global_toplevel);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/crwdgt.c,v $";
 static char rcs_id2[] = "$Id: crwdgt.c,v 1.1 1995/09/08 15:00:06 page Exp $";}
/*  ===================================================  */

}
