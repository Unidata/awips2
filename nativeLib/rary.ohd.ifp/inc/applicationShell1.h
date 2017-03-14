
/*******************************************************************************
	applicationShell1.h
	This header file is included by applicationShell1.c

*******************************************************************************/

#ifndef applicationShell1_h
#define applicationShell1_h


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

/*******************************************************************************
	The following is the definition of the context structure.
	An instance of this structure is allocated for each instance
	of the interface. The context structure contains the swidget
	variables for the interface and any variables that were
	declared in the 'Instance Specific' area of the Declarations
	Editor. Any arguments to the Interface function are also
	recorded in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	UxapplicationShell1;
	Widget	Uxform1;
	Widget	UxpushButton1;
	Widget	UxpushButton2;
	Widget	UxpushButton3;
	Widget	UxpanedWindow1;
	Widget	UxscrolledWindow1;
	Widget	UxscrolledText1;
	Widget	UxscrolledWindow2;
	Widget	UxscrolledList1;
	Widget	UxtextField1;
	Widget	Uxseparator1;
} _UxCapplicationShell1;

/* ------------------------------------------------------------------------------------
#define applicationShell1       UxApplicationShell1Context->UxapplicationShell1
#define form1                   UxApplicationShell1Context->Uxform1
#define pushButton1             UxApplicationShell1Context->UxpushButton1
#define pushButton2             UxApplicationShell1Context->UxpushButton2
#define pushButton3             UxApplicationShell1Context->UxpushButton3
#define panedWindow1            UxApplicationShell1Context->UxpanedWindow1
#define scrolledWindow1         UxApplicationShell1Context->UxscrolledWindow1
#define scrolledText1           UxApplicationShell1Context->UxscrolledText1
#define scrolledWindow2         UxApplicationShell1Context->UxscrolledWindow2
#define scrolledList1           UxApplicationShell1Context->UxscrolledList1
#define textField1              UxApplicationShell1Context->UxtextField1
#define separator1              UxApplicationShell1Context->Uxseparator1
   ------------------------------------------------------------------------------------ */

#endif   /* applicationShell1 */
