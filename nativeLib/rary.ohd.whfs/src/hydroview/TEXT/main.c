/*******************************************************************************
* FILENAME:             main.c
* NUMBER OF MODULES:         1
* GENERAL INFORMATION:
*   MODULE 1:           main
* DESCRIPTION:          This is the main routine for the hydroview application.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        October 20, 2001
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/30/01     Bryon Lawrence    Created.
*
********************************************************************************
*/
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <X11/Intrinsic.h>
#include <Xm/MessageB.h>
#include <Xm/MainW.h>
#include <stdio.h>
#include <unistd.h>

#include "choose_rfcwide_date.h"
#include "DbmsAccess.h"
#include "gui_builder.h"
#include "stage3.h"
#include "stdlib.h"
#include "Xtools.h"   /* needed for set title */
#include "color_threshold_show.h"


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   main
* PURPOSE:       This is the main routine for the hydroview application.  It
*                expects to have one command line argument passed to it.  This
*                command line argument specifies which database to retrieve
*                the hydrological data from while running hydroview.
*
*                The useage of this program is as follows:
*
*                   hydroview -d[name of database]
*
*                If hydroview is unable to find or connect to the database,
*                then it will return with a return status of "1".  Otherwise,
*                it will return with a status of "0".
*
* ARGUMENTS:
*   DATA TYPE     NAME           DESCRIPTION/UNITS
*   int           argc           Specifies the number of command line arguments
*                                being passed to this routine.
*   char *        argv [ ]       Contains the argc command line arguments being
*                                passed to this routine.
*
* RETURNS:
*   DATA TYPE   NAME             DESCRIPTION
*   int         status           The return status for the
*                                application.  A return state of "0"
*                                indicates that this application
*                                functioned normally.
*
* APIs UTILIZED:
*   NAME               HEADER FILE          DESCRIPTION
*   draw_launch_gui    gui_builder.h        This routine creates the
*                                           launch gui from which hydroview
*                                           can be started or exited.
*   OpenDbms           DbmsAccess.h         This is the routine to open and
*                                           establish a connection with the
*                                           database containing the
*                                           hydrological information.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char [ ]   appname                      Contains the name of this
*                                           application for the purposes
*                                           of error logging.
*   char       c                            A character representing a command
*                                           line option as parsed from
*   char *     dbms                         Contains the name of the database
*                                           to try to open as retrieved from
*                                           the command line.
*   int        status                       Contains the numeric error code
*                                           that will be returned to the
*                                           operating system upon completion
*                                           of this program.
*
* DATA FILES AND/OR DATABASE:
*   This application depends upon the hydrological database.  The name of this
*   database must be supplied on the command line when running this
*   application.
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*             0                            This routine functioned normally.
********************************************************************************
*/

/* A macro which represents the maximum length of a database name
   supplied at the command line. */
#define MAX_DATABASE_NAME_LEN 25

/* A macro which represents the number of characters in the
   optional look up time string.  The format of this time string
   is YYYYMMDDHH, where YYYY is the year, MM is the month, DD is the day,
   and HH is the hour of the precipitation and gage data to retrieve. */
#define TIME_STRING_LEN 10

/* A macro that prints the useage of Hydroview to the standard output.
   This is useful when used in conjunction with error messaging and
   checking. */
#define USEAGE printf ( "Useage:\n"                                       \
                          " hydroview -d[name of hydrological database]\n"\
                          "           -t[Date of historic data in "       \
                          "YYYYMMDDHH format]\n" ) ;                      \


int hydroview_main ( int argc , const char ** argv)
{
   Arg          wargs [ 20 ] ;
   const char appname [ ] = "HydroView" ;
   char c ;
   char ctmp3 [ 3 ] ;
   char ctmp5 [ 5 ] ;
   char dbms [ MAX_DATABASE_NAME_LEN ] ;
   int          len ;
   int          n ;
   int          status = 0 ;

   setApplicationName("hydroview"); //used for color management


   Widget message_shell, working, button ;
   XtAppContext app ;

   /* Process the command line arguments. */
   if ( argc < 2 || argc > 3 )
   {
      printf ( "%s:  An incorrect number of command line arguments has been\n"
               "passed into this routine.\n" , appname ) ;
      USEAGE ;
      return 1 ;
   }

   date_supplied = 0 ;

   while ( ( c = getopt ( argc , argv , ":d:t:" ) ) != -1 )
   {
      switch ( c )
      {
         case 'd' :

            len = strlen ( optarg ) ;

            if ( len >= MAX_DATABASE_NAME_LEN )
            {
               fprintf ( stderr , "\nIn the main routine:\n"
                                  "The length of the database name supplied\n"
                                  "on the command line, \"%s\" , is %d\n"
                                  "characters long. This exceeds the space\n"
                                  "of %d characters allocated for it in\n"
                                  "the \"dbms\" character array.\n" , optarg ,
                                  len , MAX_DATABASE_NAME_LEN - 1 ) ;
               return 1 ;
            }

            strcpy ( dbms , optarg ) ;
            break ;

         case 't' :

            len = strlen ( optarg ) ;

            if ( len != TIME_STRING_LEN )
            {
               fprintf ( stderr , "\nIn the main routine:\n"
                                  "The time string supplied with the '-t'\n"
                                  "option does not contain the correct\n"
                                  "number of characters.  The format of the\n"
                                  "time string must be 'YYYYMMDDHH' where\n"
                                  "YYYY is the year, MM is the month, DD is\n"
                                  "the day, and HH is the hour of the Mpe\n"
                                  "data to process.\n" ) ;
               return 1 ;
            }

            strncpy ( ctmp5 , optarg , 4 ) ;
            ctmp5 [ 4 ] = '\0' ;
            argdate.tm_year = atoi ( ctmp5 ) ;

            strncpy ( ctmp3 , optarg + 4 , 2 ) ;
            ctmp3 [ 2 ] = '\0' ;
            argdate.tm_mon = atoi ( ctmp3 ) ;

            strncpy(ctmp3 , optarg + 6 , 2 );
            ctmp3[2] = '\0' ;
            argdate.tm_mday = atoi ( ctmp3 ) ;

            strncpy(ctmp3 , optarg + 8 , 2 ) ;
            ctmp3[2] = '\0' ;
            argdate.tm_hour = atoi ( ctmp3 ) ;
            date_supplied = 1 ;
            break ;

         case ':' :

            printf ( "%s:  A database name must be supplied with the \"-d\"\n"
                     "option.\n" , appname ) ;
            USEAGE ;
            status = 1 ;
            break ;

         case '?' :

            printf ( "%s: An invalid command line argument has been passed\n"
                     "into this routine.\n" , appname ) ;
            USEAGE ;
            status = 1 ;
            break ;

         default :
            printf ( "%s: An invalid character was retrieved from\n"
                     "\"getopt\" while parsing the command line options\n"
                     "passed into this routine.\n" , appname  ) ;
            USEAGE ;
            status = 1 ;
            break ;
      }
   }

   if ( dbms != NULL )
   {
      /* Attempt to open the database for testing. */
      status = OpenDbms ( dbms ) ;

      if ( status != Ok  )
      {
         printf ( "%s: The call to routine \"OpenDbms\" returned a\n"
                  "status code of %d.\n" , appname , status ) ;
         status = 1 ;
      }
      else
      {

         /* Launch the startup gui. */

         message_shell = XtVaAppInitialize ( &app, "hydroview_res", NULL, 0,
                                               &argc, argv, NULL, NULL );
         n=0;
         XtSetArg(wargs[n], XmNmessageString,
         XmStringCreateLtoR("Creating HydroView Display...",
                             XmSTRING_DEFAULT_CHARSET));n++;
         XtSetArg(wargs[n], XmNdefaultPosition, FALSE); n++;
	     XtSetArg(wargs[n], XtNx, 40); n++;
	     XtSetArg(wargs[n], XtNy, 40); n++;
	     XtSetArg(wargs[n], XmNheight, 80); n++;
    	 XtSetArg(wargs[n], XmNwidth, 280); n++;
    	 XtSetArg(wargs[n], XmNmaxHeight, 80); n++;
    	 XtSetArg(wargs[n], XmNmaxWidth, 280); n++;
    	 XtSetArg(wargs[n], XmNminHeight, 80); n++;
    	 XtSetArg(wargs[n], XmNminWidth, 280); n++;
    	 XtSetArg(wargs[n], XmNallowResize, FALSE); n++;

    	 working = XmCreateWorkingDialog(message_shell, "working", wargs, n);
    	 SetTitle(working, "Starting Hydroview");

         button = XmMessageBoxGetChild(working, XmDIALOG_OK_BUTTON);
     	 XtUnmanageChild(button);

     	 button = XmMessageBoxGetChild(working, XmDIALOG_HELP_BUTTON);
     	 XtUnmanageChild(button);

         button = XmMessageBoxGetChild(working, XmDIALOG_CANCEL_BUTTON);
     	 XtUnmanageChild(button);

         XtManageChild ( working ) ;

         set_parent_widget ( message_shell ) ;


         XtAppAddTimeOut ( app , 2000 ,
                        (XtTimerCallbackProc) draw_launch_gui ,
                        ( XtPointer ) & working ) ;

         XtAppMainLoop ( app ) ;
      }
   }

   return status ;
}

// ----------------------------------------------------------------------------------------------------------


