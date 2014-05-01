#include <stdio.h>
#include <X11/Intrinsic.h>

Widget  global_toplevel;

turn_autorepeat_on()
{
 XAutoRepeatOn(XtDisplay(global_toplevel));
 XFlush(XtDisplay(global_toplevel));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/turn_autorepeat_on.c,v $";
 static char rcs_id2[] = "$Id: turn_autorepeat_on.c,v 1.1 1995/09/08 15:00:01 page Exp $";}
/*  ===================================================  */

}
