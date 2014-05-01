#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleBG.h>

#include <unistd.h>

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#include "DbmsAccess.h"
#include "latestrep_show.h"
#include "ldviewer.h"
#include "prodview_show.h"
#include "rejectdata_show.h"
#include "reviewqc_show.h"
#include "TechInfo.h"
#include "Xtools.h"


/***************************************************************************/
/* include of show functions */

/* Bryon L.  Modified on October 22, 2003 to pass a NULL value into
   set_pc_options routine. This argument gives the user the option
   to pass in a linked list of OptionValuePair structures which determine
   the initial settings on the Point Data Control GUI. */

#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "tsgen_info.h"

#include "preaccum_show.h"
#include "alertalarm_show.h"


/* include of xd functions */

#include "pointcontrol.h"

#include "reviewqc.h"

#include "preaccum.h"
#include "alertalarm.h"
#include "latestrep.h"
#include "prodview.h"


/* protos */

void add_ldv_callbacks();

void ldv_aboutCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_exitCB(Widget w, XtPointer ptr, XtPointer cbs);

void ldv_synopticCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_timeseriesCB(Widget w, XtPointer ptr, XtPointer cbs);

void ldv_qbCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_rejectedCB(Widget w, XtPointer ptr, XtPointer cbs);

void ldv_ppaCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_alertalarmCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_stationstatCB(Widget w, XtPointer ptr, XtPointer cbs);
void ldv_productsCB(Widget w, XtPointer ptr, XtPointer cbs);


/* fake functions to fool ldv so it won't use
   the real functions associated with point control */

void redrawMap ( ) ;

/************************************************************************/

XtAppContext app_context;
Display *display;       /*  Display  */


/* define a pointer for ther resulting linked list data
   and a structure for the pc_options data
   which is reference as external in the pointcontrol functions. */

ReportList   		*reportlistHead = NULL;
pc_options_struct	pc_options;


/************************************************************************
   main()

   PURPOSE
   Main driver for program.  Access database, open the display,
   get application data, create display, load application data into the
   display, and enter main X loop.

   ************************************************************************/

int ldv_main (int argc, const char ** argv)
{
   int status;

   extern char	*optarg;
   char		*dbms;
   char 	db_msg[60];
   int		c;


   /* process the command line arguments that specify the database. */

   if (argc < 2)
   {
      fprintf(stderr, "usage: ldv -d[database]\n");
      exit(-1);
   }

   status = 0;
   while ((c = getopt(argc, argv, "d:")) != -1)
   {
      switch (c)
      {
	 case 'd':
	    dbms = optarg;
	    if (OpenDbms(dbms) != Ok)
	    {
	       fprintf(stderr, "Unable to open dbms. %s\n", dbms);
	       exit(-2);
	    }
	    else
	    {
	       status = 1;
	       sprintf(db_msg, "Opened database: %s", dbms);
	    }
	    break;

	 default:
	    exit(-3);
      }
   }
   if (status == 0)
   {
      fprintf(stderr, "usage: ldv -d[database]\n");
      exit(-1);
   }


   /*----------------------------------------------------------------------*/
   /* open display */

   XtSetLanguageProc ((XtAppContext) NULL, (XtLanguageProc) NULL,
		      (XtPointer) NULL);
   XtToolkitInitialize ();
   app_context = XtCreateApplicationContext ();
   display = XtOpenDisplay (app_context, NULL, argv[0], "RFCWide_res",
			    NULL, 0, &argc, argv);
   if (!display)
   {
      printf("%s: can't open display, exiting...\n", argv[0]);
      exit (-1);
   }

   /*----------------------------------------------------------------------*/
   /*----------------------------------------------------------------------*/
   /* create the application shell; add the callbacks, and
      realize the application shell widget */

   create_ldvAS( display, argv[0], argc, argv );


   add_ldv_callbacks();

   XtRealizeWidget(ldvAS);

   XtAppMainLoop(app_context);

   exit (0);
}

/****************************************************************************/
void add_ldv_callbacks()
{
   Atom wmAtom;


   wmAtom = XmInternAtom(XtDisplay(ldvAS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(ldvAS, wmAtom, ldv_exitCB, NULL);


   /* callbacks for file cascade menu pushbuttons */

   XtAddCallback(ldv_aboutPB,        XmNactivateCallback, ldv_aboutCB,         NULL);
   XtAddCallback(ldv_exitPB,         XmNactivateCallback, ldv_exitCB,          NULL);

   XtAddCallback(ldv_synopticPB,     XmNactivateCallback, ldv_synopticCB,      NULL);
   XtAddCallback(ldv_timeseriesPB,   XmNactivateCallback, ldv_timeseriesCB,    NULL);

   XtAddCallback(ldv_qbPB,           XmNactivateCallback, ldv_qbCB,            NULL);
   XtAddCallback(ldv_rejectedPB,     XmNactivateCallback, ldv_rejectedCB,      NULL);

   XtAddCallback(ldv_ppaPB,          XmNactivateCallback, ldv_ppaCB,           NULL);
   XtAddCallback(ldv_alertalarmPB,   XmNactivateCallback, ldv_alertalarmCB,    NULL);
   XtAddCallback(ldv_stationstatPB,  XmNactivateCallback, ldv_stationstatCB,   NULL);
   XtAddCallback(ldv_productsPB,     XmNactivateCallback, ldv_productsCB,      NULL);


   return;
}


/****************************************************************************/
void ldv_synopticCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   /* Reference to the point control "Map Data" pushbutton widget. */
   extern Widget pc_mapPB ;

   /* call up point displayControl Window */
   if ( ( pointcontrolDS == 0 )  || ( ! XtIsManaged ( pointcontrolDS ) ) )
   {
      set_pc_options ( NULL ) ;
      show_pointcontrolDS ( w , NULL , app_context ) ;

      /* Desensitize the "Map Data" button in the Point Control GUI.
         This button has no real meaning in the realm of ldv. */
      DeSensitize ( pc_mapPB ) ;
      DeSensitize ( pc_clearPB ) ;
   }

   else
   {
      XtUnmapWidget ( pointcontrolDS ) ;
      XtMapWidget ( pointcontrolDS ) ;
   }

   return;
}


/***************************************************************************/

void ldv_timeseriesCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   TSGEN_INFO  tsgen;


   SetCursor(w, XC_watch);

   tsgen.standalone     = NON_STANDALONE;
   tsgen.pedtse_defined = 0;
   tsgen.nitems         = 1;

   tsgen.group_check = 0;
   sprintf(tsgen.lid, "%s", " ");

   show_TSControlDS(w, tsgen);

   UnsetCursor(w);

   return;
}


/***************************************************************************/

void ldv_qbCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (! reviewqcDS)
   {
      SetCursor(w, XC_watch);

      reviewqc_show(w, "");

      UnsetCursor(w);
   }

   else
      RemapWidget(reviewqcDS);


   return;
}


/***************************************************************************/

void ldv_rejectedCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   rejectdata_show(w, "");

   return;
}


/***************************************************************************/

void ldv_ppaCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (! preaccumDS)
   {
      SetCursor(w, XC_watch);

      preaccum_show(w);

      UnsetCursor(w);
   }

   else
   {
      RemapWidget(preaccumDS);
   }

   return;
}


/***************************************************************************/

void ldv_alertalarmCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   SetCursor(w, XC_watch);

   alertalarm_show(w);

   UnsetCursor(w);


   return;
}


/***************************************************************************/

void ldv_stationstatCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (! latestrepDS)
   {
      SetCursor(w, XC_watch);

      latestrep_show(w, "dummyLid");

      UnsetCursor(w);
   }

   else
   {
      RemapWidget(latestrepDS);
   }

   return;
}


/***************************************************************************/

void ldv_productsCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   if (! prodviewDS)
   {
      SetCursor(w, XC_watch);

      show_prodview(w, "dummylid");

      UnsetCursor(w);
   }

   else if (prodviewDS)
   {
      RemapWidget(prodviewDS);
   }


   return;
}


/******************************************************************************/

void ldv_aboutCB(Widget w, XtPointer ptr, XtPointer cbs)
{

   techinfo_show(w, "Live Data Viewer", "OB 8.1", "03/21/2007");

   return;
}


/******************************************************************************/

void ldv_exitCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   CloseDbms();

   exit(0);
}


/********************************************************************

   *****************************************************************/

void  redrawMap ( )
{
   ReportList	*rPtr = NULL ;
   char 	obstime_ansi[ANSI_TIME_LEN+1];
   char		abbrev_time[20];


   printf("** Data not mapped for this implementation\n");

   rPtr = (ReportList *) ListFirst(&reportlistHead->list);
   while (rPtr)
   {
      /* format the time */

      timet_to_yearsec_ansi(rPtr->validtime, obstime_ansi);
      memset(abbrev_time, 0, 20);
      strncpy(abbrev_time, &obstime_ansi[5], 11);

      printf("%s:%f:%s:%d\n", rPtr->lid, rPtr->value, abbrev_time, rPtr->use);
      rPtr = (ReportList *) ListNext(&rPtr->node);
   }

   printf("** End of tabular dump for map data.\n");

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
