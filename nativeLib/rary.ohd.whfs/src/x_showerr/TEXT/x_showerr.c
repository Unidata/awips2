#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "xse.h"
#include "Xtools.h"

void exit_xseCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);

XtAppContext app_context;
Display *display;

/************************************************************************
   x_showerr.c
   main()

   PURPOSE
   Main driver for program.  Opens an X window shell that displays
   a string.  Useful for programs which have fatal errors that
   occur before the X mainloop is entered - this can then be called
   tp display a message to the user.

   ************************************************************************/

int x_showerr_main (int argc, const char ** argv)
{
   extern char	*optarg;
   extern int	optind, optopt;
   char 	*titlestr=NULL, *labelstr=NULL;
   int		titleset = 0, labelset = 0;
   int		c;

   /* process the command line arguments */

   while ((c = getopt(argc, argv, "t:l:")) != -1)
   {
      switch (c)
      {
	 case 't':
	    titlestr = optarg;
	    titleset = 1;
	    break;

	 case 'l':
	    labelstr = optarg;
	    labelset = 1;
	    break;

	 default:
	    fprintf(stderr, "usage: x_showerr -t -l\n");
	    exit(-1);
      }
   }

   /*----------------------------------------------------------------------*/
   /* open display */

   XtSetLanguageProc ((XtAppContext) NULL, (XtLanguageProc) NULL,
		      (XtPointer) NULL);
   XtToolkitInitialize ();
   app_context = XtCreateApplicationContext ();
   display = XtOpenDisplay (app_context, NULL, argv[0], "Rpf",
			    NULL, 0, &argc, argv);
   if (!display)
   {
      printf("%s: can't open display, exiting...\n", argv[0]);
      exit (-1);
   }
   /*----------------------------------------------------------------------*/
   /* create the application shell; add a callback, and
      realize the application shell widget */

   create_xseAS( display, argv[0], argc, argv );

   XtAddCallback(xse_cancelPB, XmNactivateCallback, exit_xseCB, NULL);
   XtRealizeWidget(xseAS);

   /* set label and title if defined */

   if (labelset == 1) SetLabel(xseLB, labelstr);
   if (titleset == 1) SetTitle(xseAS, titlestr);

   /* begin the main X loop */

   XtAppMainLoop(app_context);

   exit (0);
}

/************************************************************************
   exit_xseCB()

   PURPOSE
   Callback for exiting the program.

   ************************************************************************/
void exit_xseCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   exit(0);
}

