
/*******************************************************************************
	search_list.h
	This header file is included by search_list.c

*******************************************************************************/

#ifndef search_list_h
#define search_list_h


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
	Widget	UxtransientShell1;
	Widget	Uxform2;
	Widget	UxscrolledWindow3;
	Widget	UxscrolledList2;
	Widget	Uxseparator2;
	Widget	UxpushButton4;
	Widget	UxpushButton5;
} _UxCtransientShell1;

/* ---------------------------------------------------------------
#define transientShell1         UxTransientShell1Context->UxtransientShell1
#define form2                   UxTransientShell1Context->Uxform2
#define scrolledWindow3         UxTransientShell1Context->UxscrolledWindow3
#define scrolledList2           UxTransientShell1Context->UxscrolledList2
#define separator2              UxTransientShell1Context->Uxseparator2
#define pushButton4             UxTransientShell1Context->UxpushButton4
#define pushButton5             UxTransientShell1Context->UxpushButton5
   --------------------------------------------------------------- */

#endif
