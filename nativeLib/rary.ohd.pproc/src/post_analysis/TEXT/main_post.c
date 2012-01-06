/*=========================================================================*/
/*                         FILE NAME:   main_post.c                        */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   main()                             */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <unistd.h>

#include "GeneralUtil.h"
#include "postX.h"
#include "siipp.h"
#include "version.h"
#include "postanalysis_functions.h"
#include "help.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTfinION NAME:   main()                                                */
/*       FUNCTION:   main driver for the Post Analysis program             */
/***************************************************************************

Functions called:
   print_version
   ReadParam
   get_dates
   display_date_window

Variables:
   app_name - application name used to get colors and levels from db tables

******************************************** BEGIN main ********************/

char app_name [ 21 ];

int post_analysis_main (int argc, const char **argv)
{
   int      len;
   long int  irc;
   char     *helpdir;
   char     ctmp[3],ctmp5[5];
   struct tm argdate;
   int      c;
   extern char *optarg;
   static int  first = 1;

   printf("Welcome to Post Analysis\n");
   printf("version number = %s\n", post_analysis_version_number);
   LOGNAME = getenv("LOGNAME");
   dbg = 0;
   numarg = argc;
   date_given = 0;

   /*-------------------------------------------------*/
   /*  define parameters for colorvalue table lookup  */
   /*-------------------------------------------------*/

   strcpy(app_name,"post_analysis");
   strcpy(cv_use,"PRECIP");
   cv_duration = 86400;

   /*--------------------------------------------------------------------*/
   /*     check to see if debug is specified on command                  */
   /*     check to see if date and time are specified on command line    */
   /*     date/time must be Z time                                       */
   /*--------------------------------------------------------------------*/

   if (argc > 3)
   {
      printf("Usage: post_analysis [-ddebug] [-t<date/time> in YYYYMMDDHH zulu time]\n");
      exit(0);
   }

   if (argc > 1)
   {
     while ((c=getopt(argc, argv, "d:t:")) != -1)
     {
      switch (c)
      {
       case 'd':
          /*get the option for debug*/

	  if (strcmp(optarg,"debug") == 0)
	     dbg = 1;
	  else
	     dbg = 0;
	  break;

       case 't':
          /*get the specified date time*/

	  if (strlen(optarg) == 10 )
	  {
	    strncpy(ctmp5,&optarg[0],4);
	    ctmp5[4] = '\0';
	    argdate.tm_year = atoi(ctmp5);
	    strncpy(ctmp,&optarg[4],2);
	    ctmp[2] = '\0';
	    argdate.tm_mon = atoi(ctmp);
	    strncpy(ctmp,&optarg[6],2);
	    ctmp[2] = '\0';
	    argdate.tm_mday = atoi(ctmp);
	    strncpy(ctmp,&optarg[8],2);
	    ctmp[2] = '\0';
	    argdate.tm_hour = atoi(ctmp);
	    date_given = 1;
	  }
	  else
	  {
	    printf("Invalid date/time %s, should be YYYYMMDDHH zulu time.\n",
	            optarg);
	    date_given = 0;
	  }
	  break;
        }
     }
   }

   toplevel = XtInitialize(argv[0], "S3Post_res", NULL, 0, &argc, argv);

   display = XtDisplay(toplevel);

/*-------------------------------------------------------------------*/
/*   set up online help                                              */
/*-------------------------------------------------------------------*/

   helpdir = (char *)malloc(200*sizeof(char));

   if (first == 1)
   {
     len = strlen("rfcwide_help_dir");
     get_apps_defaults("rfcwide_help_dir",&len,helpdir,&len);
     if (len == 0)
     {
        printf("Invalid token value for $(rfcwide_help_dir), program exit.\n");
        exit(1);
     }

     first = 0;
   }
   popupHelp_shell = create_applicationShell1(toplevel,helpdir);

 /*----------------------------------------*/
 /*   open Postgres database               */
 /*----------------------------------------*/

   startdb(&irc);
   if(irc !=0)
   {
     printf(" PostgreSQL error# %ld ",irc);
     printf(" occurred attempting to open database \n");
     exit(1);
   }

/*------------------------------------------------------------------*/
/*   read input parameters                                          */
/*------------------------------------------------------------------*/

   ReadParam();

/*-------------------------------------------------------------------*/
/*   define and display dates window                                 */
/*-------------------------------------------------------------------*/

   get_dates(argdate);

   display_date_window();

   XtMainLoop();

 /*-------------------------------------------*/
 /*   close database                          */
 /*-------------------------------------------*/

  closedb(&irc);
  if(irc !=0)
  {
    printf("PostgreSQL error# %ld ",irc);
    printf(" occurred attempting to close database \n");
  }

 return 0;
}

/********************************************* END main ********************/
