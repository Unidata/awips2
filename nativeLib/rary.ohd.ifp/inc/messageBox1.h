
/*******************************************************************************
	messageBox1.h
	This header file is included by messageBox1.c

*******************************************************************************/

#ifndef messageBox1_h
#define messageBox1_h


#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>

#include <Xm/MessageB.h>

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
	Widget	UxmessageBox1;
} _UxCmessageBox1;

/* ---------------------------------------------------------------
#define messageBox1             UxMessageBox1Context->UxmessageBox1
   --------------------------------------------------------------- */

#endif	
