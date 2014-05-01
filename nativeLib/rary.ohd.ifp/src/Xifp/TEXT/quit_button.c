
/* *********************************************************************

 File: quit_button.c

	Quit button callbacks.

	Creates a 'Quit' button with a variable label with
	create_quit_button().

	Beginning of libXs from Douglas Young's:
		"Window Systems Programming and Applications with Xt"
			OSF/Motif edition

	Date:   7/31/90 - Thomas Adams NWS/HRL

   ********************************************************************* */



#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>






/* ************************ 'Quit' button Callbacks ******************** */

static void arm_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

*flag = FALSE;

}


static void activate_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

*flag = TRUE;

}

static void disarm_callback(w, flag, call_data)
	Widget                  w;
	int                     *flag;
	XmAnyCallbackStruct     *call_data;
{

if(*flag)
	{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
	}
}


/* **********************************************************************

	create_quit_button()
		creates a 'Quit' button with a variable label


   ********************************************************************** */

Widget Xifp_create_quit_button(label, parent)
	char            *label;
	Widget          parent;
{

	Widget          w;
	static int      really_quit;


w = XtCreateManagedWidget(label, xmPushButtonWidgetClass, parent, NULL, 0);

XtAddCallback(w, XmNarmCallback, arm_callback, &really_quit);
XtAddCallback(w, XmNdisarmCallback, disarm_callback, &really_quit);
XtAddCallback(w, XmNactivateCallback, activate_callback, &really_quit);

return(w);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/quit_button.c,v $";
 static char rcs_id2[] = "$Id: quit_button.c,v 1.1 1995/09/08 15:01:11 page Exp $";}
/*  ===================================================  */

}
