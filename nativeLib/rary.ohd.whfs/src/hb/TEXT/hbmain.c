/*
	File:		hbmain.c
	Purpose:	Main X-Windows Driver for HydroBase.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/SelectioB.h>
#include <Xm/ToggleB.h>

#include "DbmsAccess.h"
#include "GetOS.h"
#include "ParamDefs.h"
#include "Xtools.h"
#include "UserPrefs.h"
#include "hbAS.h"
#include "callbacks.h"
#include "Admin.h"
#include "GeneralUtil.h"
#include "user_prefs.h"
#include "Xtools.h"


int		Hb_entered_mainxloop = 0;
static char	*user_password = NULL;
static int	pswd_index = 0;

int hb_main(int argc,const char ** argv)
{
   	/*
   		External variables.
	*/
	extern char	*optarg;
	extern int	optind,
			optopt;


	/*
		X variables.
	*/
	XtAppContext	app_context;
	Display		*display;

	/*
		Local variables.
	*/
	char		*dbms;
	int		pos,
	   		c;
	UserPrefs	*up;
	char		where[MAX_WHERE_LEN],
	   		*userid;
	int		update;

        Admin		*admin;

	/*
		Try to open the display.
	*/
	XtSetLanguageProc((XtAppContext)NULL,
			  (XtLanguageProc)NULL,
			  (XtPointer)NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "Hb",
				NULL, 0, &argc, argv);
	if (!display)
	{
	   printf("%s: can't open display, exiting...\n", argv[0]);
	   exit(-1);
	}



	/*
		Check input args.
	*/
	if (argc < 2)
	{
		fprintf(stderr, "usage: hb -d[database]\n");
		exit(-1);
	}



	/*
		Process command line args.
	*/
	while ((c = getopt(argc, argv, "d:")) != -1)
	{
	   switch (c)
	   {
	      case 'd':
		 dbms = optarg;
		 if (OpenDbms(dbms) != Ok)
		 {
		    abort_hb("FATAL CONDITION...Failed to Open IHFS database");
		    exit(-2);
		 }
		 break;

	      default:
		    abort_hb("FATAL CONDITION...Failed to Open IHFS database");
		    exit(-3);
	   }
	}



	/*
		Create main application shell and add callbacks.
	*/
	create_hbAS (display, argv[0], argc, argv);
	AddCallbacks();

	/*
		Setup main window selectors.
	*/
	update = False;
	if ((userid = (char *)getlogin()) != NULL)
	{
	   	sprintf(where, " WHERE userid = '%s' ", userid);
	   	if ((up = GetUserPrefs(where)) != NULL)
		{
		   	update = True;
			FreeUserPrefs(up);
		}
	}
	if(update == False)
	{
	   set_title_preference(3);
	   set_sort_preference(0);
	   set_field_preference(7);
	}
	pos = get_sort_preference();
	SetMenuPos(hbsortOM, pos);
 	set_stat_list(NULL, NULL, NULL);
	set_main_window_title(hbAS, "HydroBase");

	/*
		Realize hbAS
	*/
	XtRealizeWidget(hbAS);
	XtAddEventHandler(hbAS, FocusChangeMask, False, (XtEventHandler) hbmain_setFocus, NULL);

	/*
		Get the password from the Admin table
		Put up the password dialog if a password exists.
	*/
        admin = GetAdmin("");

        if ( (admin != NULL) && (admin->hb_password != NULL) )
        {
            if (strncmp(admin->hb_password, "", 8) == 0)
                 ErrorDialog(hbAS, "Please set a password for HydroBase\n"
                                "  in the Setup/Administration dialog.");
            else
	      create_password_dialog ( NULL , NULL , NULL ) ;
        }

	/*
		Enter main loop
	*/
	Hb_entered_mainxloop = 1;
	XtAppMainLoop(app_context);

	/*
		Cleanup and exit.
	*/
	close_hb(NULL, NULL, NULL);
	return(0);
}



/***********************************************************************
   abort_hb()

   PURPOSE
   Displays an error dialog before aborting the HydroBase program.

***********************************************************************/
void	abort_hb(char *msg)
{
   char bindir[128];
   int  gad_token_len=0, gad_value_len=0;

   Widget abortDS, okPB;
   char systr[300];

   printf("ABORTING HydroBase.\n");
   printf("%s\n", msg);


   if (Hb_entered_mainxloop == 1)
   {
      abortDS = ErrorDialog(hbAS, msg);
      SetTitle(abortDS, "HydroBase Abort");

      okPB = XmMessageBoxGetChild(abortDS, XmDIALOG_OK_BUTTON);
      XtAddCallback(okPB, XmNactivateCallback, close_hb, NULL);
   }

   else
   {
      gad_token_len = strlen("whfs_bin_dir");
      get_apps_defaults("whfs_bin_dir", &gad_token_len, bindir, &gad_value_len);
      sprintf(systr, "%s/x_showerr.LX -t\"HydroBase Abort\" -l\"%s\"",
              bindir, msg);
      system(systr);
      exit(0);
   }
}


/***********************************************************************
   hbmain_setFocus()
***********************************************************************/
void	hbmain_setFocus(Widget w, XtPointer ptr, XEvent *event)
{

   if (event->type == FocusIn)
   {
      XmProcessTraversal(hbAS, XmTRAVERSE_CURRENT);
   }


   return;
}

/***********************************************************************
   create_password_dialog()
***********************************************************************/
void create_password_dialog( Widget w , XtPointer client_data ,
                             XtPointer call_data )
{
	Widget		passwordDS;
	XmString	xstring = XmStringCreateLocalized("Enter Password:");
	Arg		args[10];
	int		ac;

           ac = 0;


	/*
		create the password dialog shell
	*/
           XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
           XtSetArg(args[ac], XmNtitle, "Password Dialog"); ac++;
           XtSetArg(args[ac], XmNselectionLabelString, xstring); ac++;
           XtSetArg(args[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
           XtSetArg(args[ac], XmNnoResize, True); ac++;

	   passwordDS = XmCreatePromptDialog(hbAS, "hbpasswordPD", args, ac);

	   XmStringFree(xstring);
	/*
		keep only the OK button
	*/
	   XtDestroyWidget(XmSelectionBoxGetChild(passwordDS,
				                  XmDIALOG_HELP_BUTTON));
	   XtDestroyWidget(XmSelectionBoxGetChild(passwordDS,
				                  XmDIALOG_CANCEL_BUTTON));

	/*
		set callbacks for OK button and text field
	*/
	   XtAddCallback(passwordDS, XmNokCallback, check_password, NULL);
	   XtAddCallback(XmSelectionBoxGetChild(passwordDS, XmDIALOG_TEXT),
	                 XmNmodifyVerifyCallback, TextModifiedCallback, NULL);
	   XtAddCallback(XmSelectionBoxGetChild(passwordDS, XmDIALOG_TEXT),
	                 XmNmotionVerifyCallback, MotionCallback, NULL);

	   XtManageChild(passwordDS);
	   XtPopup(XtParent(passwordDS), XtGrabNone);
}


/***********************************************************************
   check_password()
***********************************************************************/
void    check_password(Widget w, XtPointer ptr, XtPointer cbs)
{
        Admin		*admin;
	static int	num_tries=1;
	Widget		pswd_errDS;

	/* get the password from the Admin table */

        admin = GetAdmin("");


	/* compare the 2 passwords; if they match then move on
           if they entered the wrong password and it has been less than
           three times then give them another chance
	   if they entered in the password wrong 3 times then abort */



        if ((user_password == NULL)||(strncmp(admin->hb_password,user_password, 8) != 0))
        {
           if (num_tries < 3)
           {
              pswd_errDS = ErrorDialog(hbAS, "Invalid password entered.\n"
                                             "      Please try again.");
	      XtAddCallback(pswd_errDS, XmNokCallback,
                             create_password_dialog, NULL ) ;

              num_tries++;
	      pswd_index = 0;
           }

           else
           {
              abort_hb("Three failed password attempts - exiting HydroBase.");
           }
        }

	return;
}


/***********************************************************************
   MotionCallback()
***********************************************************************/
void MotionCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
	XmTextVerifyCallbackStruct *tvcbs = (XmTextVerifyCallbackStruct *) cbs;


	/* don't allow backspacing */

	if (tvcbs->newInsert < tvcbs->currInsert)
	   tvcbs->doit = FALSE;


	return;
}


/***********************************************************************
   TextModifiedCallback()
***********************************************************************/
void TextModifiedCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

	XmTextVerifyCallbackStruct *tvcbs = (XmTextVerifyCallbackStruct *) cbs;

	/*
		Don't allow user to paste into the text field
	*/
	if (tvcbs->text->ptr == NULL ||
	    tvcbs->text->length > 1)
	{
	   tvcbs->doit = FALSE;
	}

	else
	{
	   user_password = XtRealloc (user_password, sizeof(char) * pswd_index + 2);
	   user_password[pswd_index++] = tvcbs->text->ptr[0];
	   user_password[pswd_index] = '\0';

	/*
		replace the charater in the text window with an *
	*/
	   tvcbs->text->ptr[0] = '*';
	}

	return;
}
