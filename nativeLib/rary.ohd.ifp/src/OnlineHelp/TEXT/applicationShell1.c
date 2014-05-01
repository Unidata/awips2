
/*******************************************************************************
	applicationShell1.c

	Associated Header file: applicationShell1.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>

#include <Xm/Separator.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <X11/Shell.h>

#include "callbacks.h"
#include "global_defs.h"

/*******************************************************************************
	The following is from the 'Includes, Defines, Global variables'
	area of the Declarations Editor.
*******************************************************************************/


/*******************************************************************************
	The following header file defines the context structure.
*******************************************************************************/

#include "applicationShell1.h"


/*******************************************************************************
	The following are forward declarations of functions
	that are defined later in this file.
*******************************************************************************/

Widget  create_applicationShell1();


/*******************************************************************************
	The following are callback functions.
*******************************************************************************/

Widget   build_applicationShell1(shell, UxApplicationShell1Context)
	Widget                  shell;
	_UxCapplicationShell1   *UxApplicationShell1Context;
{

	Widget          applicationShell1;
	Widget          panedWindow1;
	Widget          form1;
	Widget          pushButton1;
	Widget          pushButton2;
	Widget          pushButton3;
	Widget          scrolledWindow1;
	Widget          scrolledWindow2;
	Widget          scrolledText1;
	Widget          scrolledList1 = (Widget)NULL;
	Widget          separator1;
	Widget          textField1;



	applicationShell1 = XtVaCreatePopupShell( "applicationShell1",
			transientShellWidgetClass, shell,
			XmNgeometry, "430x590+450+150",
			NULL );

	UxApplicationShell1Context->UxapplicationShell1 = applicationShell1;

	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass, applicationShell1,
			XmNheight, 10,
			XmNwidth, 100,
			XmNy, 90,
			XmNx, 70,
			XmNunitType, XmPIXELS,
			NULL );

	UxApplicationShell1Context->Uxform1 = form1;

	pushButton1 = XtVaCreateManagedWidget( "pushButton1",
			xmPushButtonWidgetClass, form1,
			XmNbottomPosition, 0,
			XmNrightAttachment, XmATTACH_NONE,
			XmNrecomputeSize, TRUE,
			XmNborderWidth, 6,
			XmNborderColor, XmStringCreateLtoR("#7E88AB", XmSTRING_DEFAULT_CHARSET),
			XmNshowAsDefault, 0,
			XmNresizable, FALSE,
			XmNlabelString, XmStringCreateLtoR("Find", XmSTRING_DEFAULT_CHARSET),
			XmNbottomOffset, 10,
			XmNtopPosition, 91,
			XmNtopOffset, 1,
			XmNrightOffset, 10,
			XmNleftOffset, 0,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNleftPosition, 13,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNheight, 10,
			XmNwidth, 100,
			XmNy, 3,
			XmNx, 108,
			NULL );

	UxApplicationShell1Context->UxpushButton1 = pushButton1;

	pushButton2 = XtVaCreateManagedWidget( "pushButton2",
			xmPushButtonWidgetClass, form1,
			XmNshowAsDefault, 1,
			XmNborderWidth, 0,
			XmNborderColor, XmStringCreateLtoR("Black", XmSTRING_DEFAULT_CHARSET),
			XmNresizable, FALSE,
			XmNlabelString, XmStringCreateLtoR("Close", XmSTRING_DEFAULT_CHARSET),
			XmNbottomOffset, 10,
			XmNtopPosition, 91,
			XmNtopOffset, 1,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNleftPosition, 40,
			XmNleftOffset, 0,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 530,
			XmNx, 220,
			NULL );

	UxApplicationShell1Context->UxpushButton2 = pushButton2;

	pushButton3 = XtVaCreateManagedWidget( "pushButton3",
			xmPushButtonWidgetClass, form1,
			XmNborderWidth, 6,
			XmNborderColor, XmStringCreateLtoR("#7E88AB", XmSTRING_DEFAULT_CHARSET),
			XmNresizable, FALSE,
			XmNleftOffset, 10,
			XmNtopPosition, 91,
			XmNtopOffset, 1,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNbottomOffset, 10,
			XmNlabelString, XmStringCreateLtoR("Help", XmSTRING_DEFAULT_CHARSET),
			XmNleftPosition, 65,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNheight, 10,
			XmNwidth, 100,
			XmNy, 3,
			XmNx, 3,
			NULL );

	UxApplicationShell1Context->UxpushButton3 = pushButton3;

	panedWindow1 = XtVaCreateManagedWidget( "panedWindow1",
			xmPanedWindowWidgetClass, form1,
			XmNtopAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNbottomPosition, 90,
			XmNbottomAttachment, XmATTACH_POSITION,
			XmNheight, 100,
			XmNwidth, 100,
			XmNy, 540,
			XmNx, 490,
			XmNunitType, XmPIXELS,
			NULL );

	UxApplicationShell1Context->UxpanedWindow1 = panedWindow1;

	scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
			xmScrolledWindowWidgetClass, panedWindow1,
			XmNheight, 600,
			XmNpaneMinimum, 300,
			XmNshadowThickness, 0,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNvisualPolicy, XmVARIABLE,
			XmNy, 0,
			XmNx, 0,
			XmNunitType, XmPIXELS,
			XmNscrollingPolicy, XmAUTOMATIC,
			NULL );

	UxApplicationShell1Context->UxscrolledWindow1 = scrolledWindow1;

	scrolledText1 = XtVaCreateManagedWidget( "scrolledText1",
			xmTextWidgetClass, scrolledWindow1,
			XmNeditMode, XmMULTI_LINE_EDIT,
			/*XmNy, -520,
			XmNx, 0, removed by AV, causing help text not display on scrolledText1*/
			XmNheight, 900,
			XmNwidth, 360,
			NULL );

	UxApplicationShell1Context->UxscrolledText1 = scrolledText1;
       
        
	scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
			xmScrolledWindowWidgetClass, panedWindow1,
			XmNshadowThickness, 0,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNy, 405,
			XmNx, 0,
			XmNunitType, XmPIXELS,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			NULL );

	UxApplicationShell1Context->UxscrolledWindow2 = scrolledWindow2;

	scrolledList1 = XtVaCreateManagedWidget( "scrolledList1",
			xmListWidgetClass, scrolledWindow2,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNheight, 100,
			XmNwidth, 100,
			NULL );

	UxApplicationShell1Context->UxscrolledList1 = scrolledList1;
       
	textField1 = XtVaCreateManagedWidget( "textField1",
			xmTextFieldWidgetClass, panedWindow1,
			XmNpaneMaximum, 60,
			XmNheight, 100,
			XmNwidth, 10,
			XmNy, 510,
			XmNx, 0,
			NULL );

	UxApplicationShell1Context->UxtextField1 = textField1;

	separator1 = XtVaCreateManagedWidget( "separator1",
			xmSeparatorWidgetClass, form1,
			XmNtopWidget, panedWindow1,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNbottomPosition, 92,
			XmNbottomWidget, pushButton1,
			XmNbottomAttachment, XmATTACH_POSITION,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNorientation, XmHORIZONTAL,
			XmNheight, 15,
			XmNwidth, 10,
			XmNy, 320,
			XmNx, 70,
			NULL );

	UxApplicationShell1Context->Uxseparator1 = separator1;


	XtAddCallback( pushButton1, XmNactivateCallback,
			find_selected, UxApplicationShell1Context );

	XtAddCallback( pushButton2, XmNactivateCallback,
			close_selected, UxApplicationShell1Context );

	XtAddCallback( pushButton3, XmNactivateCallback,
			help_selected, UxApplicationShell1Context );

	XtAddCallback( scrolledText1, XmNmotionVerifyCallback,
			set_selected_text, UxApplicationShell1Context );

	XtAddCallback( scrolledList1, XmNbrowseSelectionCallback,
			listItem_selected, UxApplicationShell1Context );



	return ( applicationShell1 );
}

/*******************************************************************************
	The following function includes the code that was entered
	in the 'Initial Code' and 'Final Code' sections of the
	Declarations Editor. This function is called from the
	'Interface function' below.
*******************************************************************************/

Widget   _Uxcreate_applicationShell1(shell, help_path)
	Widget          shell;
	char            *help_path;
{
	Widget                  rtrn;
	_UxCapplicationShell1   *UxContext;

	UxContext =
		(_UxCapplicationShell1 *) XtMalloc( sizeof(_UxCapplicationShell1) );

	rtrn = build_applicationShell1(shell, UxContext);

	do_app_specific_stuff(UxContext, shell, help_path);
	return(rtrn);
}

/*******************************************************************************
	The following is the 'Interface function' which is the
	external entry point for creating this interface.
	This function should be called from your application or from
	a callback function.
*******************************************************************************/

Widget  create_applicationShell1(shell, help_path)
	Widget          shell;
	char            *help_path;
{
	Widget			_Uxrtrn;

	_Uxrtrn = _Uxcreate_applicationShell1(shell, help_path);

	return ( _Uxrtrn );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/applicationShell1.c,v $";
 static char rcs_id2[] = "$Id: applicationShell1.c,v 1.2 2002/02/11 19:11:16 dws Exp $";}
/*  ===================================================  */

}

/*******************************************************************************
	END OF FILE
*******************************************************************************/

