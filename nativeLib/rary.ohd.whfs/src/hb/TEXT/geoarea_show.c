/*
	File:		geoarea_show.c	
	Purpose:	Provide support for GeoArea DS.
*/


/************************************************************************
   
   Functions to handle the setup control of geo area info.
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "cvt_latlon.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "GeoArea.h" 
#include "geoarea_show.h"
#include "geolog_show.h"
#include "geomanage.h"
#include "GetSuffix.h"
#include "hybase_utils.h"
#include "Xtools.h"


/* variables global to this file */

typedef enum
{
   ZONES, COUNTIES, BASINS, RESERVOIRS
} geoarea_types;

char geoarea_datanames[4][20] =
   {"ZONE", "COUNTY", "BASIN", "RESRVR"};

char geoarea_filenames[4][30] =
   {"zones.dat", "counties.dat", "basins.dat", "resvrs.dat"};

char		geoarea_format_string[] =
"%-8s %-40s %9s  %9s";

char geoarea_import_logfiles[4][30] =
   {"process_geoarea_ZONE.log",  "process_geoarea_COUNTY.log",
    "process_geoarea_BASIN.log", "process_geoarea_RESRVR.log"};

GeoArea	*areaHead;
geoarea_types areatype_index;

#define FILENAME_LEN 200
char filename[FILENAME_LEN];


/************************************************************************
   
   Load the geo area dialog shell.
   
   ***********************************************************************/

void geoarea_show(Widget 	w)
{
   if (! geoareaDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_geoareaDS(GetTopShell(w));
      add_geoarea_cbs();
   }
   
   
   /* manage the windows now before doing the selections */   
   if (! XtIsManaged(geoareaDS))
   {
      XtManageChild(geoareaFO);
      XtManageChild(geoareaDS);
      
      
      /* initialize data memory */
      
      areaHead = (GeoArea *)(NULL);
      
      
      /* use the callback to load the scrolled list of defined areas
	 and the import info, based on the selected area type */
      
      geoarea_typeCB();
      
   }
   
   
   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_geoarea_cbs()
{
   
   Atom	wmAtom;
   
   /* callbacks on option menu pushbuttons selection */
   
   XtAddCallback(types_zonesPB,      XmNactivateCallback, geoarea_typeCB, NULL);
   XtAddCallback(types_countiesPB,   XmNactivateCallback, geoarea_typeCB, NULL);
   XtAddCallback(types_basinsPB,     XmNactivateCallback, geoarea_typeCB, NULL);
   XtAddCallback(types_reservoirsPB, XmNactivateCallback, geoarea_typeCB, NULL);
   
   
   /* callback on import, edit, log view */
   
   XtAddCallback(area_editPB,   XmNactivateCallback, geoarea_editCB,  NULL);
   
   XtAddCallback(area_importPB, XmNactivateCallback, geoarea_importCB, NULL);
   
   XtAddCallback(area_readlog_importPB,   XmNactivateCallback,
		 geoarea_readlog_importCB,  NULL);
      
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(geoareaDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(geoareaDS, wmAtom, ok_geoareaCB, NULL);
   
   XtAddCallback(geoarea_okPB, XmNactivateCallback, ok_geoareaCB, NULL);
   
   return;
}

/************************************************************************
   
   Load the scrolled list for the GeoArea info.
   
   ***********************************************************************/

void load_area_list()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt;
   char			liststr[80];
   GeoArea		*areaPtr = NULL;
   char 		where[140];
   
   char			lat[11];
   char			lon[11];

   
   /* free any memory allocated */
   
   free_geoarea();
   
   
   /* load the list of geo areas from the database */
   
   sprintf(where, " WHERE boundary_type = '%s' ", 
	   geoarea_datanames[areatype_index]); 
   strcat(where, " ORDER BY area_id ");  
   
   areaHead = GetGeoArea(where);
   
   if (areaHead != NULL)
   {
      cnt = ListCount(&areaHead->list);
      areaPtr = (GeoArea *) ListFirst(&areaHead->list);
   }   
   else
   {
      cnt = 0;
      areaPtr = NULL;
   }   
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings */
   
   if (areaPtr != NULL)
   {
     for (i = 0; i < cnt; i++)
     {
      strcpy(lat, cvt_latlon_from_double(areaPtr->interior_lat));
      strcpy(lon, cvt_latlon_from_double(areaPtr->interior_lon));
      
      sprintf(liststr, geoarea_format_string,
	      areaPtr->area_id,  areaPtr->name, lat, lon);
      xmStr[i] = XmStringCreateSimple(liststr);
      
      areaPtr = (GeoArea *) ListNext(&areaPtr->node);
     }
   
   }
  
   /* load the list in the scrolled list */
   
     ac = 0;
     XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
     XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
     XtSetValues(geoareaLS, arg, ac);
   
      
   /* free the memory */
   
     for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
      XtFree((char *)xmStr);
       
   
   return;
}

/************************************************************************
   
   Load the import filename for the GeoArea info.
   
   ***********************************************************************/

void load_area_import()
{
   
   XmTextSetString(area_fileTX, geoarea_filenames[areatype_index]);
   
   return;
}


/************************************************************************
   
   
   
 **********************************************************************/

void geoarea_importCB()
{
   Widget questDS;
   Widget okPB;
   char msgstr[500];
   int	default_match;
   int 	status;
   
   
   /* get the file name and other info including its accesibility
      and whether its name matches the default. the filename
      is passed via a global variable */
      
   getarea_filename(READFILE, &default_match, &status);
   
   
   /* if the file was not accessible, don't continue with the import */
   
   if (status < 0) return;
  
     
   /* confirm import of info */
   
   if (default_match == 1)
      sprintf(msgstr,
	      "Importing %s data from the default file:\n  %s\n\n"
	      "This will DELETE all %s data before the import.\n\n", 
	      geoarea_datanames[areatype_index], filename,
	      geoarea_datanames[areatype_index]);
   else
      sprintf(msgstr,
	      "Importing %s data from a user-specified file:\n  %s\n\n"
	      "This will DELETE all %s data before the import.\n\n", 
	      geoarea_datanames[areatype_index], filename,
	      geoarea_datanames[areatype_index]);

   strcat(msgstr,
	  "(Note: Occasionally save backup file copies of the data\n"
	  "       to user-specified file - i.e. not the default filename.)\n\n");
   
   strcat(msgstr, "Are you sure you wish to import the data?");

   questDS = QuestionDialog(geoareaDS, msgstr);
   SetTitle(questDS, "Geo Area Import");
   
   
   /* add a callback, remove any leftover callbacks before adding it */
   
   okPB = XmMessageBoxGetChild(questDS, XmDIALOG_OK_BUTTON);
   remove_ok_callbacks(okPB);      
   XtAddCallback(okPB, XmNactivateCallback, import_geoarea, NULL);
   
   
   return;
}


/************************************************************************
   
   Import the data.
   
   **********************************************************************/

void import_geoarea()
{
   char		bindir[128];
   char		command[340];
   char		dbname[128];
   int		gad_token_len=0, gad_value_len=0;
   
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

   /* Call the process_geoarea script to load the geoarea from the ascii
      file into the geoarea database table. */
   sprintf ( command , "%s/process_geoarea -l -d%s -t%s %s" ,
	     bindir , dbname , geoarea_datanames [ areatype_index ] , 
             filename ) ;
   
   /* now execute the command */
   printf("Command: %s\n", command);
   system(command);
   
   /* update the list of items since the import affects the database.
      don't update the file name as it may be user-specified */
   
   load_area_list(areatype_index);
   
   XmListSetPos(geoareaLS, 1);
   XmListSelectPos(geoareaLS,   1, 1);
   
   
   return;
}

/************************************************************************
   

    **********************************************************************/

void geoarea_editCB()
{
   
   int		default_match;
   int		status;
   char 	command[300];
   char		bindir[128] , editor_name[128] ;
   int		gad_token_len=0, gad_value_len=0;
   
   
   /* get the value of the file name to edit. the filename
      is passed via a global variable */
   
   getarea_filename(READFILE, &default_match, &status);
   
   if (status < 0) return;
   
   /* get the directory for the edit script */
   gad_token_len = strlen ( "whfs_local_bin_dir" ) ;
   get_apps_defaults ( "whfs_local_bin_dir" , & gad_token_len , bindir ,
                       & gad_value_len ) ;
   if ( gad_value_len <= 0 )
   {
      fprintf(stderr, "whfs_bin_dir undefined.  Edit not performed.\n");
      return;
   }

   /* Retrieve the name of the editor script. */
   gad_token_len = strlen ( "whfs_editor" ) ;
   get_apps_defaults ( "whfs_editor" , & gad_token_len , editor_name ,
                       & gad_value_len ) ;
   if ( gad_value_len <= 0 )
   {
      fprintf(stderr, "whfs_editor undefined.  Edit not performed.\n");
      return;
   }

   /* build the command line to perform the edit */
   sprintf ( command , "%s/%s %s %s" ,
	     bindir , editor_name , "GeoAreaEditor" , filename ) ;
   system(command);
   
   return;
}


/************************************************************************
   
   
   
   **********************************************************************/

void getarea_filename(int  	readwrite_flag,
		      int	*default_match,
		      int	*status)
{
   char 	*namestr;
   char		geodir[128];
   FILE		*file_ptr;
   Widget	infoDS;
   char		msgstr[400];
   int		gad_token_len=0, gad_value_len=0;
   
   
   /* get the value of the filename */
   
   namestr = XmTextGetString(area_fileTX);
     
   
   /* check if the name is the default for this data set */
   
   
   if (strcmp(namestr, geoarea_filenames[areatype_index]) == 0)
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
	 sprintf(msgstr, "Could not access default %s file:\n  %s\n",
		 geoarea_datanames[areatype_index], filename);
      else
	 sprintf(msgstr, "Could not access user-specified %s file.\n  %s\n",
		 geoarea_datanames[areatype_index], filename);
      
      infoDS = InfoDialog(geoareaDS, msgstr);
      SetTitle(infoDS, "File Access Warning");
   }
   
   return;
}


/************************************************************************
   
   Select the entry currently being considered.
   The format of this string must agree with how the list
   was loaded.
   
   **********************************************************************/

void geoarea_select(GeoArea *areaPtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;
   
   char			lat[11];
   char			lon[11];

   
   strcpy(lat, cvt_latlon_from_double(areaPtr->interior_lat));
   strcpy(lon, cvt_latlon_from_double(areaPtr->interior_lon));
   
   sprintf(liststr, geoarea_format_string,
	   areaPtr->area_id,  areaPtr->name, lat, lon);
   
   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(geoareaLS, item))
   {
      pos = XmListItemPos(geoareaLS, item);
      XmListSetPos(geoareaLS, pos);
      XmListSelectPos(geoareaLS, pos, True);
   }
   
   else
   {
      XmListSetPos(geoareaLS, 1);
      XmListSelectPos(geoareaLS, 1, True);
   }
   XmStringFree(item);

   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_geoarea()
{
   
   if (areaHead != NULL)
   {
      FreeGeoArea(areaHead);
      areaHead = (GeoArea *)NULL;
   }
   
   
   return;
}


/************************************************************************
   
   
   **********************************************************************/

void geoarea_typeCB()
{
   int menuitem;

   /* get the current value of the option menu indicating whether
      to list all, recommended, or just the included points */

   menuitem = GetMenuPos(geoareaOM);

   if (menuitem == 0)
      areatype_index = ZONES;
   else if (menuitem == 1)
      areatype_index = COUNTIES;
   else if (menuitem == 2)
      areatype_index = BASINS;
   else if (menuitem == 3)
      areatype_index = RESERVOIRS;


   /* reload the list */

   load_area_list(areatype_index);
   
   /* select the first item in the list with the callback */
   
   XmListSetPos(geoareaLS, 1);
   XmListSelectPos(geoareaLS,   1, 1);

   
   /* load the import info */

   load_area_import(areatype_index);
   
   
   return;
}


/************************************************************************
   
   
   **********************************************************************/

void geoarea_readlog_importCB()
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
	      geodir, geoarea_import_logfiles[areatype_index]); 
   
   
   /* load the log file info into a string */
   
   textstr = (char *)NULL;
   status = load_log_text(logfilename, &textstr);
   
   
   /* build an info string */
   
   sprintf(msgstr, "%s : %s", geoarea_datanames[areatype_index], logfilename); 
   
   
   /* display the text */
   
   if (status < 0)
      geolog_show(geoareaDS, "Areal Definitions Import Log",
		  msgstr, "Log file not available.");
   else
      geolog_show(geoareaDS, "Areal Definitions Import Log",
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
void remove_ok_callbacks(Widget widgetPB)
{

   if (widgetPB != NULL)
   {
      XtRemoveCallback(widgetPB, XmNactivateCallback, import_geoarea, NULL);
   }
   

   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_geoareaCB()
{
   /* BryonL - November 27, 2003 - Removed the linesegs computation from 
      this routine.  The load_linesegs routine is now called from the
      process_geoarea script. */ 
   
   /* free allocated memory */
   
   free_geoarea();
   
   
   /* remove the window */
   
   if (XtIsManaged(geoareaDS))
   {
      XtDestroyWidget(geoareaDS);
      geoareaDS = NULL;
   }
   
   return;
}




