
/*******************************************************************************
	search_list.c

	Associated Header file: search_list.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>

#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <X11/Shell.h>

/*******************************************************************************
	The following is from the 'Includes, Defines, Global variables'
	area of the Declarations Editor.
*******************************************************************************/


/*******************************************************************************
	The following header file defines the context structure.
*******************************************************************************/

#include "search_list.h"
#include "callbacks.h"


/*******************************************************************************
	The following are forward declarations of functions
	that are defined later in this file.
*******************************************************************************/

Widget	create_transientShell1();

/*******************************************************************************
	The following are callback functions.
*******************************************************************************/

Widget   build_transientShell1(shell, UxTransientShell1Context)
	Widget                  shell;
	_UxCtransientShell1     *UxTransientShell1Context;
{

	Widget          transientShell1;
	Widget          form2;
	Widget          scrolledWindow3;
	Widget          scrolledList2;
	Widget          separator2;
	Widget          pushButton4;
	Widget          pushButton5;


	transientShell1 = XtVaCreatePopupShell( "transientShell1",
			transientShellWidgetClass, shell,
			XmNgeometry, "275x460+150+150",
			NULL );

	UxTransientShell1Context->UxtransientShell1 = transientShell1;

	form2 = XtVaCreateManagedWidget( "form2",
			xmFormWidgetClass, transientShell1,
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 60,
			XmNx, 40,
			XmNunitType, XmPIXELS,
			NULL );

	UxTransientShell1Context->Uxform2 = form2;

	scrolledWindow3 = XtVaCreateManagedWidget( "scrolledWindow3",
			xmScrolledWindowWidgetClass, form2,
			XmNbottomPosition, 85,
			XmNbottomAttachment, XmATTACH_POSITION,
			XmNtopAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNshadowThickness, 0,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNy, 40,
			XmNx, 30,
			XmNunitType, XmPIXELS,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			NULL );

	UxTransientShell1Context->UxscrolledWindow3 = scrolledWindow3;

	scrolledList2 = XtVaCreateManagedWidget( "scrolledList2",
			xmListWidgetClass, scrolledWindow3,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNheight, 10,
			XmNwidth, 100,
			NULL );

	UxTransientShell1Context->UxscrolledList2 = scrolledList2;

	separator2 = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass, form2,
			XmNbottomPosition, 90,
			XmNbottomAttachment, XmATTACH_POSITION,
			XmNtopWidget, scrolledWindow3,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 110,
			XmNx, 50,
			NULL );

	UxTransientShell1Context->Uxseparator2 = separator2;

	pushButton4 = XtVaCreateManagedWidget( "pushButton4",
			xmPushButtonWidgetClass, form2,
			XmNleftOffset, 10,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopPosition, 92,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNleftPosition, 10,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNbottomOffset, 10,
			XmNlabelString, XmStringCreateLtoR("Show", XmSTRING_DEFAULT_CHARSET),
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 280,
			XmNx, 10,
			NULL );

	UxTransientShell1Context->UxpushButton4 = pushButton4;

	pushButton5 = XtVaCreateManagedWidget( "pushButton5",
			xmPushButtonWidgetClass, form2,
			XmNtopPosition, 92,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNleftPosition, 50,
			XmNleftOffset, 10,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNbottomOffset, 10,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNlabelString, XmStringCreateLtoR("Close", XmSTRING_DEFAULT_CHARSET),
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 280,
			XmNx, 130,
			NULL );

	UxTransientShell1Context->UxpushButton5 = pushButton5;


	XtAddCallback( scrolledList2, XmNbrowseSelectionCallback,
			set_helpItem_selected, (XtPointer) UxTransientShell1Context );
	XtAddCallback( scrolledList2, XmNdefaultActionCallback,
			show_helpItem_selected, (XtPointer) UxTransientShell1Context );

	XtAddCallback( pushButton4, XmNactivateCallback,
			show_pattern_selected, (XtPointer) UxTransientShell1Context );

	XtAddCallback( pushButton5, XmNactivateCallback,
			close_search_popup, (XtPointer) UxTransientShell1Context );



	return ( transientShell1 );
}

/*******************************************************************************
	The following function includes the code that was entered
	in the 'Initial Code' and 'Final Code' sections of the
	Declarations Editor. This function is called from the
	'Interface function' below.
*******************************************************************************/

Widget   _Uxcreate_transientShell1(shell)
	Widget          shell;
{
	Widget                  rtrn;
	_UxCtransientShell1     *UxContext;

	UxContext =
		(_UxCtransientShell1 *) XtMalloc( sizeof(_UxCtransientShell1) );

	rtrn = build_transientShell1(shell, UxContext);

	fill_searchList(UxContext->UxscrolledList2);
	return(rtrn);
}

/*******************************************************************************
	The following is the 'Interface function' which is the
	external entry point for creating this interface.
	This function should be called from your application or from
	a callback function.
*******************************************************************************/

Widget  create_transientShell1(shell)
	Widget          shell;
{
	Widget			_Uxrtrn;

	_Uxrtrn = _Uxcreate_transientShell1(shell);

	return ( _Uxrtrn );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/search_list.c,v $";
 static char rcs_id2[] = "$Id: search_list.c,v 1.1 1995/09/08 14:56:44 page Exp $";}
/*  ===================================================  */

}

/*******************************************************************************
	END OF FILE
*******************************************************************************/

