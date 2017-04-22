/*
	File:		geoline_show.c
	Purpose:	Provide support for GeoLine DS.
*/

/************************************************************************
   
   Functions to handle the setup control of geo line info.
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "geomanage.h"
#include "geoline_show.h"
#include "geolog_show.h"
#include "GetSuffix.h"

#include "DbmsUtils.h"
#include "Xtools.h"
#include "GeneralUtil.h"
#include "geoarea_show.h"

#define FILENAME_LEN 200

/* variables global to this file */

typedef enum
{
   RIVERS, STREAMS, HIWAYS, ROADS
} geoline_types;

char filename[FILENAME_LEN];

char geoline_datanames[4][20] =
   {"STREAM", "STREAM", "ROAD", "ROAD"};

char geoline_typenames[4][20] =
   {"RIVERS", "STREAMS", "HIWAYS", "ROADS"};

char geoline_filenames[4][30] =
   {"rivers.dat", "streams.dat", "hiways.dat", "roads.dat"};

char geoline_import_logfiles[4][30] =
   {"process_geoline_RIVERS.log", "process_geoline_STREAMS.log",
    "process_geoline_HIWAYS.log", "process_geoline_ROADS.log"};

geoline_types linetype_index;


/************************************************************************
   
   Load the geo line dialog shell.
   
   ***********************************************************************/

void geoline_show(Widget 	w)
{
   if (! geolineDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_geolineDS(GetTopShell(w));
      add_geoline_cbs();      
   }
   
   
   /* manage the windows now before doing the selections */ 
   
   if (! XtIsManaged(geolineDS))
   {
      XtManageChild(geolineFO);
      XtManageChild(geolineDS);

            
      /* use the callback to load the scrolled list of defined lines
	 and the import info, based on the selected line type */
      
      geoline_typeCB();
   }
         
   
   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_geoline_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callbacks on option menu pushbuttons selection */
   
   XtAddCallback(types_riversPB,  XmNactivateCallback, geoline_typeCB, NULL);
   XtAddCallback(types_streamsPB, XmNactivateCallback, geoline_typeCB, NULL);
   XtAddCallback(types_hiwaysPB,  XmNactivateCallback, geoline_typeCB, NULL);
   XtAddCallback(types_roadsPB,   XmNactivateCallback, geoline_typeCB, NULL);
   
   
   /* callback on import, edit, log view */
   
   XtAddCallback(line_editPB,   XmNactivateCallback, geoline_editCB,  NULL);
   
   XtAddCallback(line_importPB, XmNactivateCallback, geoline_importCB, NULL);
   
   XtAddCallback(line_readlog_importPB,   XmNactivateCallback,
		 geoline_readlog_importCB,  NULL);
      
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(geolineDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(geolineDS, wmAtom, ok_geolineCB, NULL);
   
   XtAddCallback(geoline_okPB, XmNactivateCallback, ok_geolineCB, NULL);
      
   return;
}

/************************************************************************
   
   Load the import filename for the geoline info.
   
   ***********************************************************************/

void load_line_import()
{
   
   XmTextSetString(line_fileTX, geoline_filenames[linetype_index]);
   
   return;
}



/************************************************************************
   
   
   
   **********************************************************************/

void geoline_importCB()
{
   Widget questDS;
   Widget okPB;
   char msgstr[500];
   int	default_match;
   int 	status;
   
   
   /* get the file name and other info including its accesibility
      and whether its name matches the default. the filename
      is passed via a global variable */
      
   getline_filename(READFILE, &default_match, &status);
   
   
   /* if the file was not accessible, don't continue with the import */
   
   if (status < 0) return;
  
     
   /* confirm import of info */
   
   if (default_match == 1)
      sprintf(msgstr,
	      "Importing %s data from the default file:\n  %s\n\n"
	      "This will DELETE all %s data before the import.\n\n", 
	      geoline_typenames[linetype_index], filename,
	      geoline_typenames[linetype_index]);
   else
      sprintf(msgstr,
	      "Importing %s data from a user-specified file:\n  %s\n\n"
	      "This will DELETE all %s data before the import.\n\n", 
	      geoline_typenames[linetype_index], filename,
	      geoline_typenames[linetype_index]);

   strcat(msgstr,
	  "(Note: Occasionally save backup file copies of the data\n"
	  "       to user-specified file - i.e. not the default filename.)\n\n");
   
   strcat(msgstr, "Are you sure you wish to import the data?");

   questDS = QuestionDialog(geolineDS, msgstr);
   SetTitle(questDS, "Geo line Import");
   
   
   /* add a callback, remove any leftover callbacks before adding it */
   
   okPB = XmMessageBoxGetChild(questDS, XmDIALOG_OK_BUTTON);
   remove_ok_callbacks(okPB);      
   XtAddCallback(okPB, XmNactivateCallback, import_geoline, NULL);
   
   
   return;
}


/************************************************************************
   
   Import the data.
   
   **********************************************************************/

void import_geoline()
{
   char		command[340];
   char		bindir[128];
   char		dbname[128];
   int          gad_token_len=0, gad_value_len=0;
   
   /* Build the command string to import the file */
   gad_token_len = strlen("whfs_bin_dir");
   get_apps_defaults("whfs_bin_dir", &gad_token_len, bindir, &gad_value_len);

   if (strlen(bindir) <= 0)
   {
      fprintf(stderr, "whfs_bin_dir undefined.  Import not performed.\n");
      return;
   }
   
   gad_token_len = strlen("db_name");
   get_apps_defaults("db_name", &gad_token_len, dbname, &gad_value_len);
   if (strlen(dbname) <= 0)
   {
      fprintf(stderr, "db_name undefined.  Import not performed.\n");
      return;
   }
   
   /* Construct the call to process_geoline based upon the
      vector type that has been chosen. */
   if ( linetype_index == RIVERS || linetype_index == HIWAYS)     
   {
      sprintf ( command , "%s/process_geoline -t%s -n%s %s" ,
	        bindir , geoline_datanames [ linetype_index ] ,
                geoline_typenames [ linetype_index ] , filename ) ;
   }
   else
   {
      sprintf ( command , "%s/process_geoline -t%s -n%s %s" ,
	        bindir, geoline_datanames[linetype_index],
                geoline_typenames [linetype_index ] , filename ) ;
   }

   /* Execute the command */
   printf("Command: %s\n", command);
   system(command);
   
   return;
}

/************************************************************************
   
   
   
   **********************************************************************/

void geoline_editCB ( )
{
   
   int		default_match;
   int		status;
   char 	command[300];
   char 	bindir [ 128 ] , editor_name [ 128 ] ;
   int          gad_token_len=0, gad_value_len=0;
   
   
   /* get the value of the file name to edit. the filename
      is passed via a global variable */
   
   getline_filename(READFILE, &default_match, &status);
   
   if (status < 0) return;
   
   /* get the directory for the edit script */
   gad_token_len = strlen("whfs_local_bin_dir");
   get_apps_defaults( "whfs_local_bin_dir" , & gad_token_len , bindir ,
                      & gad_value_len ) ;
   if ( gad_value_len <= 0 )
   {
      fprintf(stderr, "whfs_bin_dir undefined.  Edit not performed.\n");
      return;
   }
   
   /* Retrieve the name of the editor script. */
   gad_token_len = strlen("whfs_editor");
   get_apps_defaults("whfs_editor", &gad_token_len , editor_name , 
                     & gad_value_len);
   if ( gad_value_len <= 0 )
   {
      fprintf(stderr, "whfs_editor undefined.  Edit not performed.\n");
      return;
   }
   
   /* build the command line to perform the edit */
   
   sprintf( command , "%s/%s %s %s" ,
	    bindir , editor_name , "GeoLineEditor" , filename ) ;
   system(command);
   
   
   return;
}


/************************************************************************
   
   
   
   **********************************************************************/

void getline_filename(int  	readwrite_flag,
		      int	*default_match,
		      int	*status)
{
   char 	*namestr;
   char		geodir[128];
   FILE		*file_ptr;
   Widget	infoDS;
   char		msgstr[400];
   int          gad_token_len=0, gad_value_len=0;
   
   
   /* get the value of the filename */
   
   namestr = XmTextGetString(line_fileTX);
     
   
   /* check if the name is the default for this data set */
   
   
   if (strcmp(namestr, geoline_filenames[linetype_index]) == 0)
      *default_match = 1;
   else
      *default_match = 0;
   

   /* now append the directory onto the filename.
      insist that the directory be defined */
   
   memset(filename, 0, FILENAME_LEN);
   
   gad_token_len = strlen("whfs_geodata_dir");
   get_apps_defaults("whfs_geodata_dir", &gad_token_len, geodir, &gad_value_len);
   if (strlen(geodir) <= 0)
   {
      fprintf(stderr, "whfs_geodata_dir undefined.\n");
      sprintf(filename, "error: whfs_geodata_dir undefined.");
      *status = -1;
   }
   
   else
   {
      sprintf(filename, "%s/%s", geodir, namestr);      
      XtFree(namestr);
      
      
      /* check to make sure that the file can be accessed */
      
      if (readwrite_flag == READFILE)
	 file_ptr = fopen(filename, "r");
      else
	 file_ptr = fopen(filename, "a");
      
      
      /* set the status based on whether the file can be accessed. */
      
      if(file_ptr == NULL)
	 *status = -1;
      else
      {
	 *status = 0;
	 fclose(file_ptr);
      }
   }
   
   
   /* if could not access file, then issue message */
   
   if (*status < 0)
   {
      if (*default_match == 1)
	 sprintf(msgstr, "Could not access default %s file:\n%s\n",
		 geoline_typenames[linetype_index], filename);
      else
	 sprintf(msgstr, "Could not access specified %s file.\n%s\n",
		 geoline_typenames[linetype_index], filename);
      
      infoDS = InfoDialog(geolineDS, msgstr);
      SetTitle(infoDS, "File Access Warning");
   }
   
   return;
}


/************************************************************************
   
   
   **********************************************************************/

void geoline_typeCB()
{
   int menuitem;

   
   /* get the current value of the option menu indicating whether
      to list all, recommended, or just the included points */

   menuitem = GetMenuPos(geolineOM);

   if (menuitem == 0)
      linetype_index = RIVERS;
   else if (menuitem == 1)
      linetype_index = STREAMS;
   else if (menuitem == 3)
      linetype_index = HIWAYS;
   else if (menuitem == 4)
      linetype_index = ROADS;

   /* load the import info */

   load_line_import(linetype_index);
   
   
   return;
}


/************************************************************************
   
   
   **********************************************************************/

void geoline_readlog_importCB()
{
   char *textstr;
   char msgstr[120];
   int status;
   char logfilename[300];
   char geodir[128];
   int	gad_token_len=0, gad_value_len=0;
   
   
   /* build the file name */
   
   gad_token_len = strlen("whfs_util_log_dir");
   get_apps_defaults("whfs_util_log_dir", &gad_token_len, geodir, &gad_value_len);
   if (strlen(geodir) <= 0)
   {
      fprintf(stderr, "whfs_util_log_dir undefined.\n");
      sprintf(logfilename, "whfs_util_log_dir undefined.");
   }
   else
      sprintf(logfilename, "%s/%s",
	      geodir, geoline_import_logfiles[linetype_index]); 
   
   
   /* load the log file info into a string */
   
   textstr = (char *)NULL;
   status = load_log_text(logfilename, &textstr);
   
   
   /* build an info string */
   
   sprintf(msgstr, "%s : %s", geoline_typenames[linetype_index], logfilename); 
   
   
   /* display the text */
   
   if (status < 0)
      geolog_show(geolineDS, "Vector Definitions Import Log",
		  msgstr, "Log file not available.");
   else
      geolog_show(geolineDS, "Vector Definitions Import Log",
		  msgstr, textstr);
   
   
   free(textstr);
   
   return;
}



/************************************************************************
   remove_ok_callbacks()

   PURPOSE
   Remove any leftover callbacks on the ok (or cancel) button used for
   miscellaneous dialogs.  No error occurs if an attempt is
   made to remove a callback that doesn't exist.

   *********************************************************************/
void remove_ok_line_callbacks(Widget widgetPB)
{

   if (widgetPB != NULL)
   {
      XtRemoveCallback(widgetPB, XmNactivateCallback, import_geoline, NULL);
   }
   

   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_geolineCB()
{
   if (XtIsManaged(geolineDS))
   {
      XtDestroyWidget(geolineDS);
      geolineDS = NULL;
   }

   
   return;
}




