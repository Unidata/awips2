
/*******************************************************************************
	messageBox1.c

	Associated Header file: messageBox1.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>

#include <Xm/MessageB.h>

#include "callbacks.h"
#include "global_defs.h"

/*******************************************************************************
	The following is from the 'Includes, Defines, Global variables'
	area of the Declarations Editor.
*******************************************************************************/


/*******************************************************************************
	The following header file defines the context structure.
*******************************************************************************/

#include "messageBox1.h"


/*******************************************************************************
	The following are forward declarations of functions
	that are defined later in this file.
*******************************************************************************/

Widget	create_messageBox1();

/*******************************************************************************
	The following are callback functions.
*******************************************************************************/

Widget   build_messageBox1(shell, UxMessageBox1Context)
	Widget          shell;
	_UxCmessageBox1 *UxMessageBox1Context;
{
	Widget          messageBox1_shell;
	Widget          messageBox1;

	messageBox1_shell = XtVaCreatePopupShell( "messageBox1_shell",
			topLevelShellWidgetClass, shell,
			XmNx, 750,
			XmNy, 540,
			XmNwidth, 500,
			XmNheight, 110,
			NULL );

	messageBox1 = XtVaCreateManagedWidget( "messageBox1",
			xmMessageBoxWidgetClass, messageBox1_shell,
			XmNcancelLabelString, XmStringCreateLtoR("OK", XmSTRING_DEFAULT_CHARSET),
			XmNmessageString, XmStringCreateLtoR("The search could not find a help topic with the selected string.", XmSTRING_DEFAULT_CHARSET),
			XmNdialogTitle, XmStringCreateLtoR("Pattern not found", XmSTRING_DEFAULT_CHARSET),
			XmNdialogType, XmDIALOG_ERROR,
			XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON,
			XmNheight, 100,
			XmNwidth, 100,
			XmNunitType, XmPIXELS,
			NULL );

	UxMessageBox1Context->UxmessageBox1 = messageBox1;


	XtAddCallback( messageBox1, XmNcancelCallback,
			popdown_patternNotFound_dialog, (XtPointer) UxMessageBox1Context );



	return ( messageBox1 );
}

/*******************************************************************************
	The following function includes the code that was entered
	in the 'Initial Code' and 'Final Code' sections of the
	Declarations Editor. This function is called from the
	'Interface function' below.
*******************************************************************************/

Widget   _Uxcreate_messageBox1(shell)
	Widget          shell;
{
	Widget                  rtrn;
	_UxCmessageBox1         *UxContext;

	UxContext =
		(_UxCmessageBox1 *) XtMalloc( sizeof(_UxCmessageBox1) );

	rtrn = build_messageBox1(shell, UxContext);

	do_errorDialog_things(UxContext);
	return(rtrn);
}

/*******************************************************************************
	The following is the 'Interface function' which is the
	external entry point for creating this interface.
	This function should be called from your application or from
	a callback function.
*******************************************************************************/

Widget  create_messageBox1(shell)
	Widget          shell;
{
	Widget			_Uxrtrn;

	_Uxrtrn = _Uxcreate_messageBox1(shell);

	return ( _Uxrtrn );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/messageBox1.c,v $";
 static char rcs_id2[] = "$Id: messageBox1.c,v 1.1 1995/09/08 14:56:42 page Exp $";}
/*  ===================================================  */

}

/*******************************************************************************
	END OF FILE
*******************************************************************************/

