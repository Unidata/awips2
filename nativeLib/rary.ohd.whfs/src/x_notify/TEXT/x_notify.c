/*
Name:		notify.c

Description:	X11-based notification utility.

Invocation:	notify <STRING> [<STRING>...]

		where;	<STRING> is one or more strings to output in the
			notify "dialog" (application actually).  Each
			STRING appears on its own line.

Return:		0 - Yes is pressed
		1 - No is pressed, or no string is supplied

Author:		Phil Perucci

Created:	3/8/1997
*/


/*
	Include files:
	-------------
*/

#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/MessageB.h>
#include <unistd.h>
#include "Xtools.h"

/*
	External definitions:
	--------------------
*/
extern void exit();
extern char *optarg;
extern int   optind, optopt;



/*
	Callbacks:
*/
void OK_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
	exit(0);
}

void Cancel_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
	exit(1);
}



/*
	Main program:
	------------
*/
int x_notify_main(int argc, const char ** argv)
{
	/*	Local storage:
		------------- */
	Widget toplevel;		/* Top-level widget */
	Widget mw;			/* Main window widget */
	Widget form;			/* Used to combine pushbuttons */
	Widget label;			/* Label widget */
	XtAppContext app;		/* Main app context */
	XmString xm_string;		/* Ye olde' Motif string */
	Widget main_rc;			/* Main row-column widget */
	Widget OK;			/* Yes/No buttons */
	Widget Cancel;
        Arg arg[10];			/* Arg passed to Xt */
	int ac = 0;
	Dimension width, height;	/* Widget width/height */
	char label_text[2048];		/* Buffer used for output label */
	int  i;				/* World-famous loop index */
	char window_title[500];		/* Window title */
	int c;				/* Infamous single character */
	int cancel_button=1;		/* 1 if "Cancel" button should be displayed */
	int title_supplied=0;		/* 1 if title is supplied on command line */

	Widget	horizSB;  /* for unmanaging horizontal scrollbar */
	Widget	vertSB;   /* for unmanaging vertical scrollbar */


	/*	Check input args:
		---------------- */
	if (argc < 2) {
		printf("\n");
		printf("usage:  x_notify [-n] [-t WINDOW_TITLE] <STRING> [<STRING>...]\n\n");
		printf("        where:  -n = no cancel button\n");
		printf("                -t = switch to add window title\n");
		printf("        (return code = 0 if user selects OK\n");
		printf("         return code = 1 if user selects Canceled or this message is "
					"displayed)\n\n");
		return 1 ;
	}


   	/*
   		Process command line argument[s]
   	*/
   	strcpy(window_title,"");

   	while ((c = getopt(argc, argv, "nt:")) != -1)
   	{
      		switch (c)
      		{
	 	case 't':
			/*
				Get window title
			*/
	    		strncpy(window_title, optarg, 490);
	    		title_supplied = 1;
	    		break;

	 	case 'n':
			/*
				no cancel button
			*/
	    		cancel_button = 0;
	    		break;

	 	default:
	    		break;
      		}
   	}


	/*	Set internationalization:
		------------------------ */
    	XtSetLanguageProc (NULL, NULL, NULL);


	/*	Initialize Xt:
		------------- */
	toplevel = XtVaAppInitialize (&app, "xdbw", NULL, 0,
        			      &argc, argv, NULL,
        			      NULL);


	/*	Create main window:
		------------------ */
    	mw = XtVaCreateManagedWidget ("main_w",
        	xmMainWindowWidgetClass, toplevel,
        	XmNwidth,           25,
        	XmNheight,          25,
        	XmNscrollingPolicy, XmAUTOMATIC,
        	NULL);

	/*
		Unmanage scrollbars to prevent warning messages
		----------------------------------------------- */
        XtVaGetValues(mw, XmNhorizontalScrollBar, &horizSB,
        		  XmNverticalScrollBar,   &vertSB, NULL);
        XtUnmanageChild(horizSB);
        XtUnmanageChild(vertSB);


    	/* 	Create main RowColumn:
		--------------------- */
    	main_rc = XtVaCreateWidget ("main_rc",
		xmRowColumnWidgetClass, mw, NULL);


	/*	Add label:
		--------- */
        label = XtVaCreateManagedWidget(
		"abc", xmLabelGadgetClass, main_rc,
            	XmNalignment,        XmALIGNMENT_CENTER,
            	NULL);

        if (0 != strncmp("-t", argv[1], 2))
        	strcpy(label_text,argv[1]);
        else
        	strcpy(label_text, "");

        for(i=2; i<argc; i++)
        {
	        if (0 != strncmp("-t", argv[i], 2))
	        {
	        	if ((i > 2) || (0 != strncmp("-t", argv[1], 2)))
	        	{
				strcat(label_text,"\n");
			}
			strcat(label_text,argv[i]);
		}
        }

        xm_string = XmStringCreateLtoR(label_text, "TAG");
        XtSetArg(arg[ac], XmNlabelString, xm_string);  ac++;
        XtSetValues(label, arg, ac);
	XmStringFree(xm_string);


	/*	Add separator:
		------------- */
        XtVaCreateManagedWidget(
		"sep",
		xmSeparatorWidgetClass, main_rc,
            	NULL);

	/*	Add buttons:
		----------- */
	form = XtVaCreateWidget(
		NULL, xmFormWidgetClass, main_rc,
		XmNfractionBase, 2,
		NULL);

        OK = XtVaCreateManagedWidget(
		"OK",
		xmPushButtonWidgetClass, form,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNleftPosition, 0,
		XmNrightAttachment, XmATTACH_POSITION,
		XmNrightPosition, 1,
            	NULL);
        Cancel = XtVaCreateManagedWidget(
		"Cancel",
		xmPushButtonWidgetClass, form,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNleftPosition, 1,
		XmNrightAttachment, XmATTACH_POSITION,
		XmNrightPosition, 2,
            	NULL);

	XtManageChild (form);


	/*	Set call-backs
		-------------- */
	XtAddCallback(OK, XmNactivateCallback, OK_CB, NULL);
	XtAddCallback(Cancel, XmNactivateCallback, Cancel_CB, NULL);


	/*	Pop-up window:
		------------- */
    	XtManageChild(main_rc);
    	XtRealizeWidget(toplevel);


	/*
		Set dimensions of main_rc & toplevel widgets
		-------------------------------------------- */
	ac = 0;
        XtSetArg(arg[ac], XmNwidth, &width); ac++;
        XtSetArg(arg[ac], XmNheight, &height); ac++;
        XtGetValues(main_rc, arg, ac);

        ac = 0;
        XtSetArg(arg[ac], XmNwidth, width+5); ac++;
        XtSetArg(arg[ac], XmNminWidth, width+5); ac++;
        XtSetArg(arg[ac], XmNmaxWidth, width+5); ac++;
        XtSetArg(arg[ac], XmNheight, height+5); ac++;
        XtSetArg(arg[ac], XmNminHeight, height+5); ac++;
        XtSetArg(arg[ac], XmNmaxHeight, height+5); ac++;
        XtSetValues(toplevel, arg, ac);



	/*	Set window title:
		---------------- */
	if (1 == title_supplied) {
		SetTitle(toplevel, window_title);
	}


	/*	Remove cancel button if required
		-------------------------------- */
	if (0 == cancel_button) {
		XtSetMappedWhenManaged(Cancel, False);
	}


	/* 	Main X loop...
		-------------- */
    	XtAppMainLoop(app);

        return 0 ;
}
