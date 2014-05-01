/*
	File:		textrept_show.c
	Date:		April 11, 1997
	Author:		Paul Taylor

	Purpose:	Provides support for the Text Reports DS.

        Modification History: Updated the TextReports_AddCallbacks routine so
                              that ordering by HSA is possible.  Also, removed
                              the primary and secondary backup office
                              columns.   February 10, 2004  Bryon Lawrence
                              
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <datetime.h>
#include <time.h>
#include <string.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "GeneralUtil.h"
#include "GetSuffix.h"

#include "Admin.h"


#include "e19_show.h"
#include "e19a_show.h"
#include "b44a_show.h"
#include "stalist_show.h"
#include "stnclass_show.h"
#include "servbkup_show.h"

#include "textrept.h"
#include "textrept_show.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */


/*
	Globals.
*/
static char			textrept_lid[LOC_ID_LEN + 1];
static textrept_bufinfo		bufInfo;
static char			textrept_email_subject[BUFSIZ];


/*
	Functions.
*/

void	ShowTextReportsDs(Widget w, char *lid)
{ 
    int lines_per_page_token_len = 0;
    int lines_per_page_value_len = 0;
    char buf[100];

    memset(buf, '\0', 100);
    TEXTREPORTS_MAX_LINES_PER_PAGE = -1;
    
    lines_per_page_token_len = strlen("whfs_lines_per_page");
    get_apps_defaults("whfs_lines_per_page", 
                      &lines_per_page_token_len, 
                      buf, 
		      &lines_per_page_value_len);

    if(strlen(buf) > 0)
    {
       TEXTREPORTS_MAX_LINES_PER_PAGE = atoi(buf);
    }
    if(TEXTREPORTS_MAX_LINES_PER_PAGE == -1)
    {
       printf("whfs_lines_per_page token not set...continuing with default value of 60\n");
       TEXTREPORTS_MAX_LINES_PER_PAGE = 60;
    }
    
    FOOTER_POSITION = TEXTREPORTS_MAX_LINES_PER_PAGE - 4; 

#if DEBUG_MODE
printf("in ShowTextReportDs...textreptDS, textreptpeDS, textreptsaveDS == "
       "<%p>,<%p>,<%p>\n", textreptDS, textreptpeDS, textreptsaveDS);
#endif

   if (! textreptDS)
   {
      bufInfo.e19_buf = NULL;
      bufInfo.e19a_buf = NULL;
      bufInfo.b44a_buf = NULL;
      
      bufInfo.stalist_buf = NULL;
      bufInfo.stnclass_buf = NULL;
      bufInfo.servbkup_buf = NULL;

      CreateTextReportsDs(w, lid);
      TextReports_GenerateReport(textreptOM);
   }

#if DEBUG_MODE
printf("after TextReports_GenerateReport\n");
#endif

   if (! XtIsManaged(textreptDS))
   {
      XtManageChild(textreptFO);
      XtManageChild(textreptDS);
/* hide the Help push button */
      XtUnmanageChild(textrepthelpPB);
/* hide the Email push button */
      XtUnmanageChild(textreptpeemailPB);
/* hide the Email address form */
      XtUnmanageChild(textreptaddrFO);
  }
   
   return;
}
//---------------------------------------------------------


void	CreateTextReportsDs(Widget w, char *lid)
{
   Atom			wmAtom;

   
   if (! textreptDS)
   {
      create_textreptDS(GetTopShell(w));
      wmAtom = XmInternAtom(XtDisplay(textreptDS), "WM_DELETE_WINDOW", False);
      XmAddWMProtocolCallback(textreptDS, wmAtom, TextReports_Close, NULL);
      
      strcpy(textrept_lid, lid);
      /*
      		Get static database info.
		
		Keep this data around until the user closes the
		TextReports DS as text is copied directly from the
		Text widget for Print/Email & Save operations.
      */
      set_E19_Info_Ptr (get_E19_Info(True));
      set_E19A_Info_Ptr(get_E19A_Info(True));
      set_B44A_Info_Ptr(get_B44A_Info(True));
      set_StnClass_Info_Ptr(get_StnClass_Info(True));
            
      SetMenuPos(textreptOM, 0);	/* (set "Report" OM to E-19) */
      SetMenuPos(e19reptOM, E19_COVER);	/* (set "Page" OM to Cover) */
      
      TextReports_AddCallbacks();
   }
   
#if DEBUG_MODE
printf("in CreateTextReportsDs...textreptDS, textreptpeDS, textreptsaveDS == "
       "<%p>,<%p>,<%p>\n", textreptDS, textreptpeDS, textreptsaveDS);
#endif

   /*
   	Create other supporting dialogs.
   */
   CreateTextReportsPrintEmailDs(w);
   CreateTextReportsSaveDs(w);

   
   return;
}
//---------------------------------------------------------


void	CreateTextReportsPrintEmailDs(Widget w)
{
   Atom		wmAtom;

   
   if (! textreptpeDS)
   {
      create_textreptpeDS(GetTopShell(w));
      wmAtom = XmInternAtom(XtDisplay(textreptpeDS), "WM_DELETE_WINDOW", False);
      XmAddWMProtocolCallback(textreptpeDS,  wmAtom, TextReports_PE_Close, NULL);

      
      /*
	      Add callbacks.
      */
      XtAddCallback(textreptpeokPB, XmNactivateCallback, TextReports_PE_Close, NULL);
      XmToggleButtonSetState(e19allTB, True, False);
      XmToggleButtonSetState(stalistallTB, True, False);
   }

#if DEBUG_MODE
printf("in CreateTextReportsPrintEmailDs...textreptDS, textreptpeDS, textreptsaveDS == "
       "<%p>,<%p>,<%p>\n", textreptDS, textreptpeDS, textreptsaveDS);
#endif

   return;
}
//---------------------------------------------------------


void	CreateTextReportsSaveDs(Widget w)
{
   Atom		wmAtom;


   if (! textreptsaveDS)
   {
      create_textreptsaveDS(GetTopShell(w));
      wmAtom = XmInternAtom(XtDisplay(textreptsaveDS), "WM_DELETE_WINDOW", False);
      XmAddWMProtocolCallback(textreptsaveDS,  wmAtom, TextReports_SAVE_Close, NULL);

      
      /*
		Add callbacks.
      */
      XtAddCallback(textreptsokPB, XmNactivateCallback, TextReports_SAVE_Save, NULL);
      XtAddCallback(textreptscancelPB, XmNactivateCallback, TextReports_SAVE_Close, NULL);
   }

#if DEBUG_MODE
printf("in CreateTextReportsSaveDs...textreptDS, textreptpeDS, textreptsaveDS == "
       "<%p>,<%p>,<%p>\n", textreptDS, textreptpeDS, textreptsaveDS);
#endif

   return;
}
//---------------------------------------------------------


void	ShowTextReportsPrintEmailDs(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (! XtIsManaged(textreptpeDS))
   {
      XtManageChild(textreptpeFO);
      XtManageChild(textreptpeDS);
   }
   else
   {
      RemapWidget(textreptpeDS);
   }
   
   return;
}
//---------------------------------------------------------


void	ShowTextReportsSaveDs(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (! XtIsManaged(textreptsaveDS))
   {
      XtManageChild(textreptsaveFO);
      XtManageChild(textreptsaveDS);
   }
   else
   {
      RemapWidget(textreptsaveDS);
   }
   
   return;
}
//---------------------------------------------------------


void	ShowTextReportsHelpDs(Widget w, XtPointer ptr, XtPointer cbs)
{
   return;
}
//---------------------------------------------------------


void	TextReports_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	reportOM = (Widget) ptr;

   SetCursor(textreptFO, XC_watch);
   
   
   if (GetMenuHistory(reportOM) == textrept_stalistPB)
   {
      SetMenuPos(stalistOM, STALIST_SORTBY_LID);
   }
   
   TextReports_FreeBufferMemory();
   TextReports_GenerateReport(reportOM);
   
   
   UnsetCursor(textreptFO);
   
   return;
}
//---------------------------------------------------------


void	TextReports_GenerateReport(Widget reportOM)
{
   Widget	reportPB = GetMenuHistory(reportOM);
   
   /*
   	Perform miscellaneous setup operations, to include:
	setting of titles on the Print/Email & Save windows,
	visibility of rowColumn widget on the Print/Email window, and the
	setting of the label widget on the Save window.
   */
   TextReports_MiscSetupOperations(reportPB);

#if DEBUG_MODE
printf("after TextReports_MiscSetupOperations\n");
#endif


   /*
   	Load the appropriate text into the main Text widget,
	depending on what Report has been selected.
   */
   TextReports_LoadTextWidget(reportPB);

   
   return;
}
//---------------------------------------------------------


void	TextReports_MiscSetupOperations(Widget reportPB)
{
   Widget	w = reportPB;
   char		*report_type = GetLabel(reportPB);
   int		menu_pos;

   char		newstring[BUFSIZ];
   char		rpt_dir[128];
   char		fn_ptr[BUFSIZ];
   char		file[BUFSIZ];
   int          gad_token_len=0, gad_value_len=0;

   
   /*
   	Handle the swapping in & out of the "E-19 Page"
	and "StaList SortBy" option menus.
   */
   TextReports_HideAuxilliaryOMs();
   
   if (reportPB == textrept_e19PB)
      TextReports_ShowAuxilliaryOM(e19reptOM);
   
   if (reportPB == textrept_stalistPB)
      TextReports_ShowAuxilliaryOM(stalistOM);

   if (reportPB == textrept_servbkupPB)
      TextReports_ShowAuxilliaryOM(servbkupOM);
   
   /*
   	Set up title for main Text Reports window.
	Set up title for Text Reports - Print window.
	Set up title for Text Reports - Save window.
   */
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report", report_type);
   if ((w == textrept_stnclassPB) || (w == textrept_stalistPB) || (w == textrept_servbkupPB))
      SetTitle(textreptDS, newstring);
   else
   {
      strcat(newstring, TextReports_getCurrentLid());
      SetTitle(textreptDS, newstring);
   }

   
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   if ((w == textrept_stnclassPB) || (w == textrept_stalistPB) || (w == textrept_servbkupPB))
      sprintf(newstring, "%-s - Print", report_type);
   else
      sprintf(newstring, "%-s - Print - %-s",
	      report_type, TextReports_getCurrentLid());
   SetTitle(textreptpeDS, newstring);
   

   memset ( (void * ) newstring, '\0', sizeof(newstring));
   if ((w == textrept_stnclassPB) || (w == textrept_stalistPB) || (w == textrept_servbkupPB))
      sprintf(newstring, "%-s - Save", report_type);
   else
      sprintf(newstring, "%-s - Save - %-s",
	      report_type, TextReports_getCurrentLid());
   SetTitle(textreptsaveDS, newstring);

   
   /*
   	Set up labels for Text Reports - Print window.
	Set up labels for Text Reports - Save window.
   */
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report Contents:", report_type);
   SetLabel(e19includeLA, newstring);
   
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report:", report_type);
   SetLabel(e19aincludeLA, newstring);
   SetLabel(e19ainclude2LA, "A single-page Summary Report");
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "for station %-s",
	   TextReports_getCurrentLid());
   SetLabel(e19ainclude3LA, newstring);
   
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report:", report_type);
   SetLabel(b44aincludeLA, newstring);
   SetLabel(b44ainclude2LA, "An Unofficial Cooperative Report");
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "for station %-s\n",
	   TextReports_getCurrentLid());
   SetLabel(b44ainclude3LA, newstring);
   
   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report, sort by:", report_type);
   SetLabel(stalistincludeLA, newstring);

   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report:", report_type);
   SetLabel(stnclassincludeLA, newstring);
   SetLabel(stnclassinclude2LA, "Station Class Information");
   SetLabel(stnclassinclude3LA, "for ALL stations\n");

   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "%-s Report, sort by:", report_type);
   SetLabel(servbkupincludeLA, newstring);
   SetLabel(servbkupinclude2LA, "Service Backup Information");
   SetLabel(servbkupinclude3LA, "for ALL stations\n");

   memset ( (void * ) newstring, '\0', sizeof(newstring));
   sprintf(newstring, "Save %-s Report to the following location:",
	   report_type);
   SetLabel(textreptsaveLA, newstring);


   /*
   	Set up certain PE ChildCallbacks.
	
   	Unmap all row column widgets from Print window,
	then map the ONE row column widget we want to see.
   */
   TextReports_Set_PE_ChildCallbacks();
   
   TextReports_unmap_all_PE_RCs();
   if (w == textrept_e19PB)	TextReports_map_one_PE_RC(e19reptpeRC);
   if (w == textrept_e19aPB)	TextReports_map_one_PE_RC(e19apeRC);
   if (w == textrept_b44aPB)	TextReports_map_one_PE_RC(b44apeRC);
   if (w == textrept_stalistPB)	TextReports_map_one_PE_RC(stalistpeRC);
   if (w == textrept_stnclassPB)TextReports_map_one_PE_RC(stnclasspeRC);
   if (w == textrept_servbkupPB)TextReports_map_one_PE_RC(servbkuppeRC);
   
   
   /*
	Get the environment variables for the reports direcotry,
	create the filename for the E19 Report, and save.
	
	Then, set the filename.
   */
   gad_token_len = strlen("whfs_report_dir");
   get_apps_defaults("whfs_report_dir", &gad_token_len, rpt_dir, &gad_value_len);
   memset ( (void * ) fn_ptr, '\0', sizeof(fn_ptr));
   strcpy(fn_ptr, rpt_dir);
   strcat(fn_ptr, "/");


   memset ( (void * ) file, '\0', sizeof(file));
   if ((w != textrept_stnclassPB) && (w != textrept_stalistPB) && (w != textrept_servbkupPB))
   {
      strcat(file, (char *) TextReports_getCurrentLid());
   }

   
   if (w == textrept_e19PB)		strcat(file, ".e19");
   if (w == textrept_e19aPB)		strcat(file, ".e19a");
   if (w == textrept_b44aPB)		strcat(file, ".b44a");
   if ((w == textrept_stalistPB) && (strcat(file, "station_list.")))
   {
      menu_pos = GetMenuPos(stalistOM);
      
      switch(menu_pos)
      {
	 case STALIST_SORTBY_LID:	strcat(file, "lid");	break;
	 case STALIST_SORTBY_NAME:	strcat(file, "name");	break;
	 case STALIST_SORTBY_COUNTY:	strcat(file, "county");	break;
	 case STALIST_SORTBY_BASIN:	strcat(file, "basin");	break;
	 case STALIST_SORTBY_OBSERVER:	strcat(file, "observer");	break;
	    
	 default:	strcat(file, "rpt");
			fprintf(stderr, "WARNING: a default value was used...\n");
      }
   }
   if (w == textrept_stnclassPB)	strcpy(file, "stnclass.rpt");
   if ((w == textrept_servbkupPB) && (strcat(file, "service_backup.")))
   {
      menu_pos = GetMenuPos(servbkupOM);
      
      switch(menu_pos)
      {
	 case SERVBKUP_SORTBY_LID:	strcat(file, "id");		break;
	 case SERVBKUP_SORTBY_WFO:	strcat(file, "wfo");		break;
	 case SERVBKUP_SORTBY_HSA:	strcat(file, "hsa");	        break;
	    
	 default:	strcat(file, "rpt");
			fprintf(stderr, "WARNING: a default value was used...\n");
      }
   }
   
   strcat(fn_ptr, file);
   XtVaSetValues(textreptsaveTE, XmNvalue, fn_ptr, NULL);


   
   /*
	Just in case we're emailing.
	stalist_show.c uses a different email subject though...
	(see that code for details).
   */
   TextReports_setEmailSubject(file);

   
   
   /*
   	Free & return.
   */
   if (report_type != NULL)
      free(report_type);
   
   return;
}
//---------------------------------------------------------


void	TextReports_HideAuxilliaryOMs(void)
{
   XtSetMappedWhenManaged(e19reptOM, False);
   XtSetMappedWhenManaged(stalistOM, False);
   XtSetMappedWhenManaged(servbkupOM, False);
   
   return;
}

//---------------------------------------------------------

void	TextReports_ShowAuxilliaryOM(Widget om)
{
   XtSetMappedWhenManaged(om, True);
   
   return;
}

//---------------------------------------------------------

void	TextReports_LoadTextWidget(Widget reportPB)
{
   Widget w = reportPB;

#if DEBUG_MODE
printf("in TextReports_LoadTextWidget...\n");
#endif

   if (w == textrept_e19PB)
   {
      SetMenuPos(e19reptOM, E19_COVER);   /* (set "Page" OM to Cover) */

      E19_LoadTextWidget();	  /* (Loads "E-19" data into Text widget) */
      E19_checkPB(e19coverPB, NULL, NULL);	/* Finds the "Cover" page */
   }

   
   if (w == textrept_e19aPB)
   {
      E19A_LoadTextWidget();    /* (Loads "E-19A" data into Text widget) */
   }
   
   
   if (w == textrept_b44aPB)
   {
      B44A_LoadTextWidget();    /* (Loads "B-44A" data into Text widget) */
   }
   
   
   if (w == textrept_stalistPB)
   {
#if DEBUG_MODE
printf("setting stalistOM...\n");
#endif

      SetMenuPos(stalistOM, STALIST_SORTBY_LID);
      StaList_LoadTextWidget(STALIST_SORTBY_LID);
   }
   
   
   if (w == textrept_stnclassPB)
   {
      StnClass_LoadTextWidget();/* (Loads "StnClass" data into Text widget) */
   }
   
   if (w == textrept_servbkupPB)
   {
      SetMenuPos(servbkupOM, SERVBKUP_SORTBY_LID);
      ServBkup_LoadTextWidget(SERVBKUP_SORTBY_LID);
   }
   
   
   return;
}
//---------------------------------------------------------


void	TextReports_Close(Widget w, XtPointer ptr, XtPointer cbs)
{
   e19_info_type 	*e19Ptr  = get_E19_Info(False);
   e19a_info_type	*e19aPtr = get_E19A_Info(False);
   b44a_info_type	*b44aPtr = get_B44A_Info(False);
   stnclass_info_type	*stnclassPtr = get_StnClass_Info(False);

   
   TextReports_FreeBufferMemory();

   /*
   	Free static database info.
   */
   E19_FreeInfo(&e19Ptr);
   E19A_FreeInfo(&e19aPtr);
   B44A_FreeInfo(&b44aPtr);
   /* There is no need for a StaList_FreeInfo() function. */
   StnClass_FreeInfo(&stnclassPtr);

   
   XtDestroyWidget(textreptpeDS);	/* Destroy Print/Email DS. */
   textreptpeDS = NULL;
   
   XtDestroyWidget(textreptsaveDS);	/* Destroy Save DS. */
   textreptsaveDS = NULL;
   
   XtDestroyWidget(textreptDS);		/* Destroy TextReports DS. */
   textreptDS = NULL;
   
   
   return;  
}
//---------------------------------------------------------


void	TextReports_PE_Close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(textreptpeDS))
   {
      XtUnmanageChild(textreptpeFO);
      XtUnmanageChild(textreptpeDS);
   }
   
   return;
}
//---------------------------------------------------------

void	TextReports_P_OutputBuffer(char* buf)
{
        char *  pFileTemplate = "XXXXXX\0";
	char	cmd_str[BUFSIZ];
	char	tmp_dir[128];
	char	lpr_print[128];
	char	bin_dir[128];
        const char * pSuffix = NULL ;  /* Will point to a constant "C" string
                                          containing the executable suffix
                                          to use. */
        int     file_descriptor;
	int     gad_token_len=0, gad_value_len=0;	
        int     string_len;

	char	verify_str[BUFSIZ];

	char e19PrintCommand[128];
	int e19_print_command_token_len = 0;
	int e19_print_command_value_len = 0;
	
	/*
		Get temporary filename,
		and write the buffer to it.
	*/
	gad_token_len = strlen("whfs_report_dir");
	get_apps_defaults("whfs_report_dir", &gad_token_len, tmp_dir, &gad_value_len);
        /* Jan 24, 2006, Bryon L., Modified to use mkstemp instead of
           tempnam.  Tempnam is deprecated. */
        strcat ( tmp_dir, "/" );
        strcat ( tmp_dir, pFileTemplate );

        file_descriptor = mkstemp ( tmp_dir );

	if( file_descriptor >= 0 )
	{
           string_len = strlen ( buf );
           write ( file_descriptor, buf, string_len );
           fsync ( file_descriptor );
	   close ( file_descriptor );
	}
	else
	{
	   fprintf(stderr, "ERROR: Could not create temporary file...\n");
	   return;
	}


	/*
		Print the file to local printer (verify first).
		Remove the temporary file.
	*/
	memset ( (void * ) verify_str, '\0', sizeof(verify_str));
	gad_token_len = strlen("whfs_bin_dir");
	get_apps_defaults("whfs_bin_dir", &gad_token_len, bin_dir, &gad_value_len);
        /* Determine the suffix that needs to be appended onto the end of the
           executable name. */
        pSuffix = GetSuffix ( ) ;
	sprintf(verify_str, "%-s/x_notify%s -t \"Print Confirmation\""
		" \"Are you sure you want to print this document?\"" ,
                bin_dir ,
                pSuffix ) ;
	if (system(verify_str) == 0)
	{
	   memset(e19PrintCommand, '\0', 128);
	   e19_print_command_token_len = strlen("whfs_e19_print_command");
	   get_apps_defaults("whfs_e19_print_command", 
                   &e19_print_command_token_len, 
                   e19PrintCommand, 
		   &e19_print_command_value_len);
	   if(strlen(e19PrintCommand) > 0)
	   {
	      strcpy(lpr_print, e19PrintCommand);   
	   }
	   else
	   {
	      printf("Warning: whfs_e19_print_command token not set\n");
	      gad_token_len = strlen("whfs_printcommand"); /*token for the print command */
	      get_apps_defaults("whfs_printcommand", &gad_token_len, lpr_print, &gad_value_len);
	   }
	   
	   if(strlen(lpr_print) > 0)
	   {
	      strcpy(cmd_str, lpr_print);
	      strcat(cmd_str, " ");
	   }
	   else
	   {
	      printf("Warning: whfs_printcommand token not set\n");
	      strcpy(cmd_str, "lp ");
	   }
	   strcat(cmd_str, tmp_dir);
	   system(cmd_str);		/* print the temporary file */
	}
	

	strcpy(cmd_str, "rm ");
	strcat(cmd_str, tmp_dir);
	system(cmd_str);		/* remove the temporary file */
	
	return;
}
//---------------------------------------------------------


void	TextReports_E_OutputBuffer(char* buf)
{
        char*   pFileTemplate = "XXXXXX\0";
	char	cmd_str[BUFSIZ];
	char	tmp_dir[128];
	char*	addr = NULL;
	char	mail_cmd[128];
	char	ff[10];

	char	verify_str[BUFSIZ];
        int     file_descriptor;
	int     gad_token_len=0, gad_value_len=0;
        int     string_len;
	char	bin_dir[128];
        const char * pSuffix = NULL ;  /* Will point to a constant "C" string
                                          containing the executable suffix
                                          to use. */

	/*
		Get temporary filename,
		and write the buffer to it.
	*/
	gad_token_len = strlen("whfs_report_dir");
	get_apps_defaults("whfs_report_dir", &gad_token_len, tmp_dir, &gad_value_len);

        /* Jan 24, 2006.  The call to tempnam has been replaced by a call
           to mkstemp.  Tempnam is deprecated. */
        strcat ( tmp_dir, "/" );
        strcat ( tmp_dir, pFileTemplate );
        file_descriptor = mkstemp ( tmp_dir );

	if ( file_descriptor >= 0 )
	{
	   memset ( (void * ) ff, '\0', sizeof(ff));

           string_len = strlen ( ff );
           write ( file_descriptor, buf, string_len ); 
           string_len = strlen ( buf );
           write ( file_descriptor, buf, string_len ); 

	   close ( file_descriptor );
	}
	else
	{
	   fprintf(stderr, "ERROR: Could not create temporary file...\n");
	   return;
	}
	
	/*
		Get the destination email address.
	*/
	addr = (char *) XmTextGetString(textreptpeTE);
	if (TextReports_E_EmptyAddress(addr))
	{
	   ErrorDialog(textreptpeDS, "You must enter a valid email address...\n");
	   return;
	}



	/*
		Email the file to the specified address (verify first).
		Remove the temporary file.
	*/
	memset ( (void * ) verify_str, '\0', sizeof(verify_str));
	gad_token_len = strlen("whfs_bin_dir");
	get_apps_defaults("whfs_bin_dir", &gad_token_len, bin_dir, &gad_value_len);
        pSuffix = GetSuffix ( ) ;
	sprintf(verify_str, "%-s/x_notify%s -t \"Email Confirmation\""
		" \"Are you sure you want to email this document?\"" ,
                bin_dir ,
                pSuffix ) ;
	if (system(verify_str) == 0)
	{
	   gad_token_len = strlen("MAILCMD");
	   get_apps_defaults("MAILCMD", &gad_token_len, mail_cmd, &gad_value_len);
	   memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	   if(strlen(mail_cmd) > 0)
	   {
	      strcpy(cmd_str, mail_cmd);
	      strcat(cmd_str, " ");
	   }
	   else
	   {
	      sprintf(cmd_str, "mailx -s '%-s' ", textrept_email_subject);
	   }
	   
	   strcat(cmd_str, addr);
	   strcat(cmd_str, " < ");
	   strcat(cmd_str, tmp_dir);
	   system(cmd_str);		/* email the temporary file */
	}
	
	
	strcpy(cmd_str, "rm ");
	strcat(cmd_str, tmp_dir);
	system(cmd_str);		/* remove the temporary file */

	XtFree(addr);
	
	return;
}
//---------------------------------------------------------


int	TextReports_E_EmptyAddress(char* addr)
{
   int	 flag;
   int	 i, j;

   flag = True;
   if(strlen(addr) > 0)
   {
      for(i=0, j=0;  i<strlen(addr);  i++, j++)
      {
	 if (addr[j] != ' ')
	 {
	    flag = False;
	    break;
	 }
      }
   }
   
   return(flag);
}
//---------------------------------------------------------


void	TextReports_setEmailSubject(char *subject)
{
   memset ( (void * ) textrept_email_subject, '\0', sizeof(textrept_email_subject));
   strcpy(textrept_email_subject, subject);

   return;
}


//---------------------------------------------------------
void	TextReports_SAVE_Save(Widget w, XtPointer ptr, XtPointer cbs)
{
   char*	fn_ptr = NULL;
   FILE*	fp;
   char		message[BUFSIZ];
   
   char*	text = NULL;
   int		reason = TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE;
   
   
   SetCursor(textreptFO, XC_watch);
   
   switch(GetMenuPos(textreptOM))
   {
      case 0:	text = E19_GetText(E19_ALLPAGES, reason);	break;
      case 1:	text = E19A_GetText(E19A_ALLPAGES, reason);	break;
      case 2:	text = B44A_GetText(B44A_ALLPAGES, reason);	break;
	 /* case 3 is the separator */
      case 4:	text = StaList_GetText(GetMenuPos(stalistOM), reason);	break;
      case 5:	text = StnClass_GetText(STNCLASS_ALLPAGES, reason);	break;
      case 6:	text = ServBkup_GetText(GetMenuPos(servbkupOM), reason);break;
   }
   
   
   /*
   	Get the filename for the report
   	and save the buffer in it.
   */
   fn_ptr = XmTextGetString(textreptsaveTE);
   if((fp = fopen(fn_ptr, "w")) != NULL)
   {
      fputs(text, fp);
      fclose(fp);
   }
   else
   {
      UnsetCursor(textreptFO);
      memset ( (void * ) message, '\0', sizeof(message));
      sprintf(message, "ERROR: Unable to save %-s Report...\n",
	      TextReports_getCurrentLid());
      ErrorDialog(textreptDS, message);
      
      return;
   }
   
   
   if(fn_ptr != NULL)
      free(fn_ptr);
 
  /* for stalist and servbkup, assign text to bufInfoPtr->stalist_buf, 
     bufInfoPtr->servbkup_buf, will free two times. so comment out    
   if (text != NULL)
      free(text);
  */ 
   /*
   	Close window & return.
   */
   TextReports_SAVE_Close(NULL, NULL, NULL);
   UnsetCursor(textreptFO);
   return;
}
//---------------------------------------------------------


void	TextReports_SAVE_Close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(textreptsaveDS))
   {
      XtUnmanageChild(textreptsaveFO);
      XtUnmanageChild(textreptsaveDS);
   }

   return;
}

//---------------------------------------------------------

void	TextReports_SaveAll_Confirm(Widget w, XtPointer ptr, XtPointer cbs)
{
   char		message[BUFSIZ];

   
   strcpy(message,
	  "Click OK to generate & save a copy of all text reports to disk\n"
	  "(this includes reports for all Location Identifiers as well as\n"
	  "the five Sorted Station List reports).  Please be patient as this\n"
	  "may take several minutes to complete.\n\n"
	  "If you do not wish to save reports at this time, click CANCEL\n"
	  "to abort and close this dialog.");
   
   QuestionDialogWithCallbacks(textreptFO, message,
			       "Save All Text Reports - Confirmation Dialog",
			       TextReports_SaveAll, NULL);

   return;
}
//---------------------------------------------------------


void	TextReports_SaveAll(Widget w, XtPointer ptr, XtPointer cbs)
{
   Location	*loc = NULL,
      		*locPtr = NULL;

   
   SetCursor(textreptFO, XC_watch);
   
   
   /*
   	Obtain list of lids.
   */
   if ((loc = (Location *) GetLocation(" ORDER BY lid ")) != NULL)
   {
      locPtr = (Location *) ListFirst(&loc->list);
      
      /*
      	  Generate E-19, E-19A, & B-44A text reports for all lids in database.
      */
      while(locPtr != NULL)
      {
	 TextReports_writeTextIntoFile(TEXTREPORTS_E19_TEXT,  locPtr->lid);
	 TextReports_writeTextIntoFile(TEXTREPORTS_E19A_TEXT, locPtr->lid);
	 TextReports_writeTextIntoFile(TEXTREPORTS_B44A_TEXT, locPtr->lid);
	 
	 locPtr = (Location *) ListNext(&locPtr->node);
      }
      
      /*
      	  Generate all five Sorted Station List reports.
      */
      TextReports_writeTextIntoFile(TEXTREPORTS_STALIST_TEXT, NULL);
      TextReports_writeTextIntoFile(TEXTREPORTS_STNCLASS_TEXT, NULL);
      TextReports_writeTextIntoFile(TEXTREPORTS_SERVBKUP_TEXT, NULL);
      
      
      FreeLocation(loc);
   }
   else
   {
      fprintf(stderr, "ERROR encountered while trying to save reports...\n");
   }

   
   UnsetCursor(textreptFO);
   
   return;
}
//---------------------------------------------------------


void	TextReports_writeTextIntoFile(int report_type, char *newLid)
{
   char		fn_ptr[BUFSIZ];
   char		new_fn_ptr[BUFSIZ];
   char		cmd_str[BUFSIZ];
   
   FILE*	fp;
   
   char*	text = NULL;
   int		reason = TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE;
   
   char		rpt_dir[128];
   long		fileLength;
   int		gad_token_len=0, gad_value_len=0;
   
   
   memset ( (void * ) fn_ptr, '\0', sizeof(fn_ptr));
   gad_token_len = strlen("whfs_report_dir");
   get_apps_defaults("whfs_report_dir", &gad_token_len, rpt_dir, &gad_value_len);

   
   if (report_type == TEXTREPORTS_STALIST_TEXT)
   {
      fileLength = 0;
      StaList_GenerateReport(STALIST_SORTBY_LID, reason, fn_ptr, 
                             &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/station_list.lid", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }
      
      fileLength = 0;
      StaList_GenerateReport(STALIST_SORTBY_NAME, reason, fn_ptr, 
                             &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/station_list.name", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      fileLength = 0;
      StaList_GenerateReport(STALIST_SORTBY_COUNTY, reason, fn_ptr, 
                             &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/station_list.county", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      fileLength = 0;
      StaList_GenerateReport(STALIST_SORTBY_BASIN, reason, fn_ptr, 
                             &fileLength);

      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/station_list.basin", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      fileLength = 0;
      StaList_GenerateReport(STALIST_SORTBY_OBSERVER, reason, fn_ptr, 
                             &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/station_list.observer", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      return;
   }

   if (report_type == TEXTREPORTS_STNCLASS_TEXT)
   {
      text = StnClass_GetText(STNCLASS_ALLPAGES, reason);
      sprintf(fn_ptr, "%-s/stnclass.rpt", rpt_dir);
   }
   
  
   if (report_type == TEXTREPORTS_SERVBKUP_TEXT)
   {
      fileLength = 0;
      ServBkup_GenerateReport(SERVBKUP_SORTBY_LID, reason, fn_ptr,
                              &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/service_backup.id", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }
      
      fileLength = 0;
      ServBkup_GenerateReport(SERVBKUP_SORTBY_WFO, reason, fn_ptr,
                              &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/service_backup.wfo", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      fileLength = 0;
      ServBkup_GenerateReport(SERVBKUP_SORTBY_HSA, reason, fn_ptr, 
                              &fileLength);
      if (fileLength > 0)
      {
	 memset ( (void * ) new_fn_ptr, '\0', sizeof(new_fn_ptr));
	 memset ( (void * ) cmd_str, '\0', sizeof(cmd_str));
	 sprintf(new_fn_ptr, "%-s/service_backup.hsa", rpt_dir);
	 sprintf(cmd_str, "mv -f %-s %-s", fn_ptr, new_fn_ptr);
	 system(cmd_str);
      }

      return;
   }

   
   
   /*
   	If not for STALIST, STNCLASS, or SERVBKUP, Must be one of other types.
	Build fn_ptr, and send text to newly-opened file.  Then, free text.
   */
   if (report_type == TEXTREPORTS_E19_TEXT)
   {
      text = E19_GetText(E19_ALLPAGES, reason);
      sprintf(fn_ptr, "%-s/%-s.e19", rpt_dir, newLid);
   }
   if (report_type == TEXTREPORTS_E19A_TEXT)
   {
      text = E19A_GetText(E19A_ALLPAGES, reason);
      sprintf(fn_ptr, "%-s/%-s.e19a", rpt_dir, newLid);
   }
   if (report_type == TEXTREPORTS_B44A_TEXT)
   {
      text = B44A_GetText(B44A_ALLPAGES, reason);
      sprintf(fn_ptr, "%-s/%-s.b44a", rpt_dir, newLid);
   }

   if((fp = fopen(fn_ptr, "w")) != NULL)
   {
      fputs(text, fp);
      fclose(fp);
   }
   else
   {
      fprintf(stderr, "ERROR: Unable to save report under filename %-s\n",
	      fn_ptr);
      return;
   }
   
   if (text != NULL)
      free(text);
   
   return;
}
//---------------------------------------------------------

void	TextReports_FreeBufferMemory(void)
{
   textrept_bufinfo*	bufInfoPtr  = NULL;
   
   if ((bufInfoPtr=TextReports_GetBufInfo()) != NULL)
   {
#if DEBUG_MODE
printf("TextReports: Attempting to free buffer memory...\n");
#endif
      
      
        Text_FreeString(&bufInfoPtr->e19_buf);
        Text_FreeString(&bufInfoPtr->e19a_buf);
        Text_FreeString(&bufInfoPtr->b44a_buf);
        Text_FreeString(&bufInfoPtr->stalist_buf);
        Text_FreeString(&bufInfoPtr->stnclass_buf);
        Text_FreeString(&bufInfoPtr->servbkup_buf);
    }  
      return;
}
//---------------------------------------------------------


textrept_bufinfo*	TextReports_GetBufInfo(void)
{
   textrept_bufinfo*	bufInfoPtr = &bufInfo;
      
   return(bufInfoPtr);
}
//---------------------------------------------------------


void	TextReports_unmap_all_PE_RCs(void)
{
   XtSetMappedWhenManaged(e19reptpeRC, False);
   XtSetMappedWhenManaged(e19apeRC,    False);
   XtSetMappedWhenManaged(b44apeRC,    False);
   XtSetMappedWhenManaged(stalistpeRC, False);
   XtSetMappedWhenManaged(stnclasspeRC,False);
   XtSetMappedWhenManaged(servbkuppeRC,False);
   
   return;
}
//---------------------------------------------------------


void	TextReports_map_one_PE_RC(Widget w)
{
   XtSetMappedWhenManaged(w, True);
   
   return;
}
//---------------------------------------------------------


void	TextReports_AddCallbacks(void)
{
   /*
   	Add callbacks for TextReports DS.
   */
   XtAddCallback(textreptpePB,   XmNactivateCallback, ShowTextReportsPrintEmailDs, NULL);
   XtAddCallback(textreptsavePB, XmNactivateCallback, ShowTextReportsSaveDs, NULL);
   XtAddCallback(textrepthelpPB, XmNactivateCallback, ShowTextReportsHelpDs, NULL);

   XtAddCallback(textreptokPB,   XmNactivateCallback, TextReports_Close, NULL);
   
   XtAddCallback(textreptsaveallPB, XmNactivateCallback, TextReports_SaveAll_Confirm, NULL);
   

   /*
   	Support for Report OM.
   */
   XtAddCallback(textrept_e19PB,     XmNactivateCallback, TextReports_checkPB, textreptOM);
   XtAddCallback(textrept_e19aPB,    XmNactivateCallback, TextReports_checkPB, textreptOM);
   XtAddCallback(textrept_b44aPB,    XmNactivateCallback, TextReports_checkPB, textreptOM);
   XtAddCallback(textrept_stalistPB, XmNactivateCallback, TextReports_checkPB, textreptOM);
   XtAddCallback(textrept_stnclassPB,XmNactivateCallback, TextReports_checkPB, textreptOM);
   XtAddCallback(textrept_servbkupPB,XmNactivateCallback, TextReports_checkPB, textreptOM);
   
   
   /*
   	Support for E-19 page OM.
   */
   XtAddCallback(e19coverPB,    XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19mapPB,      XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19benchPB,    XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19gagesPB,    XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19histPB,     XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19crestsPB,   XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19lwPB,       XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19condPB,     XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19damagePB,   XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19staffPB,    XmNactivateCallback, E19_checkPB, NULL);
   XtAddCallback(e19contactsPB, XmNactivateCallback, E19_checkPB, NULL);

   
   /*
   	Support for StationList sort-by OM.
   */
   XtAddCallback(stalist_lidPB,      XmNactivateCallback, StaList_checkPB, 
                 (XtPointer) STALIST_SORTBY_LID);
   XtAddCallback(stalist_namePB,     XmNactivateCallback, StaList_checkPB, 
                 (XtPointer) STALIST_SORTBY_NAME);
   XtAddCallback(stalist_countyPB,   XmNactivateCallback, StaList_checkPB, 
                 (XtPointer) STALIST_SORTBY_COUNTY);
   XtAddCallback(stalist_basinPB,    XmNactivateCallback, StaList_checkPB, 
                 (XtPointer) STALIST_SORTBY_BASIN);
   XtAddCallback(stalist_observerPB, XmNactivateCallback, StaList_checkPB, 
                 (XtPointer) STALIST_SORTBY_OBSERVER);

   /*
        Support for ServiceBackup sort-by OM.
   */
   XtAddCallback(servbkup_lidPB,       XmNactivateCallback, ServBkup_checkPB, 
                 (XtPointer) SERVBKUP_SORTBY_LID);
   XtAddCallback(servbkup_wfoPB,       XmNactivateCallback, ServBkup_checkPB, 
                 (XtPointer) SERVBKUP_SORTBY_WFO);

   /* Removed the callbacks for the primary and secondary backup offices.
      These are no longer used.  Added the call back for the option to
      sort by HSA. */
   XtAddCallback(servbkup_hsaPB, XmNactivateCallback, ServBkup_checkPB, 
                 (XtPointer) SERVBKUP_SORTBY_HSA);
   
   
   return;
}
//---------------------------------------------------------


void	TextReports_Set_PE_ChildCallbacks(void)
{
#if DEBUG_MODE
printf("Setting PE Child Callbacks...\n");
#endif

   /*
   	Remove all "child dialog callbacks",
	then call the SINGLE child callback function we are
	interested in adding callbacks for
	(i.e. for either E19, E19A, B44A, or StaList).
   */
   TextReports_Remove_PE_ChildCallbacks();

   switch(GetMenuPos(textreptOM))
   {
      case 0:	TextReports_Add_E19_PE_Callbacks();	break;
      case 1:	TextReports_Add_E19A_PE_Callbacks();	break;
      case 2:	TextReports_Add_B44A_PE_Callbacks();	break;
	 /* case 3 is the separator */
      case 4:	TextReports_Add_StaList_PE_Callbacks();	break;
      case 5:	TextReports_Add_StnClass_PE_Callbacks();break;
      case 6:   TextReports_Add_ServBkup_PE_Callbacks();break;
	 
      default:	fprintf(stderr, "ERROR: Option Menu not defined properly...\n");
   }

   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_PE_ChildCallbacks(void)
{
#if DEBUG_MODE
printf("Removing all PE Child Callbacks...\n");
#endif

   TextReports_Remove_E19_PE_Callbacks();
   TextReports_Remove_E19A_PE_Callbacks();
   TextReports_Remove_B44A_PE_Callbacks();
   TextReports_Remove_StaList_PE_Callbacks();
   TextReports_Remove_StnClass_PE_Callbacks();
   TextReports_Remove_ServBkup_PE_Callbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Add_E19_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Adding E-19 Print/Email Callbacks...\n");
#endif

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, E19_PE_Print, NULL);
   XtAddCallback(textreptpeemailPB, XmNactivateCallback, E19_PE_Email, NULL);
   
   E19_AddCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_E19_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Removing E-19 Print/Email Callbacks...\n");
#endif

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, E19_PE_Print, NULL);
   XtRemoveCallback(textreptpeemailPB, XmNactivateCallback, E19_PE_Email, NULL);
   
   E19_RemoveCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Add_E19A_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Adding E-19A Print/Email Callbacks...\n");
#endif

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, E19A_PE_Print, NULL);
   XtAddCallback(textreptpeemailPB, XmNactivateCallback, E19A_PE_Email, NULL);
   
   E19A_AddCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_E19A_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Removing E-19A Print/Email Callbacks...\n");
#endif

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, E19A_PE_Print, NULL);
   XtRemoveCallback(textreptpeemailPB, XmNactivateCallback, E19A_PE_Email, NULL);
   
   E19A_RemoveCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Add_B44A_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Adding B-44A Print/Email Callbacks...\n");
#endif

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, B44A_PE_Print, NULL);
   XtAddCallback(textreptpeemailPB, XmNactivateCallback, B44A_PE_Email, NULL);
   
   B44A_AddCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_B44A_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Removing B-44A Print/Email Callbacks...\n");
#endif

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, B44A_PE_Print, NULL);
   XtRemoveCallback(textreptpeemailPB, XmNactivateCallback, B44A_PE_Email, NULL);
   
   B44A_RemoveCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Add_StaList_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Adding StaList Print/Email Callbacks...\n");
#endif

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, StaList_PE_Manager, (XtPointer)STALIST_OPTION_PRINT);
   XtAddCallback(textreptpeemailPB, XmNactivateCallback, StaList_PE_Manager, (XtPointer)STALIST_OPTION_EMAIL);
   
   StaList_AddCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_StaList_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Removing StaList Print/Email Callbacks...\n");
#endif

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, StaList_PE_Manager, (XtPointer)STALIST_OPTION_PRINT);
   XtRemoveCallback(textreptpeemailPB, XmNactivateCallback, StaList_PE_Manager, (XtPointer)STALIST_OPTION_EMAIL);
   
   StaList_RemoveCallbacks();
   
   return;
}
//---------------------------------------------------------


void	TextReports_Add_StnClass_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Adding StnClass Print/Email Callbacks...\n");
#endif

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, StnClass_PE_Print, NULL);
   XtAddCallback(textreptpeemailPB, XmNactivateCallback, StnClass_PE_Email, NULL);

   StnClass_AddCallbacks();

   return;
}
//---------------------------------------------------------


void	TextReports_Remove_StnClass_PE_Callbacks(void)
{
#if DEBUG_MODE
printf("Removing StnClass Print/Email Callbacks...\n");
#endif

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, StnClass_PE_Print, NULL);
   XtRemoveCallback(textreptpeemailPB, XmNactivateCallback, StnClass_PE_Email, NULL);

   StnClass_RemoveCallbacks();

   return;
}
//---------------------------------------------------------


void	TextReports_Add_ServBkup_PE_Callbacks(void)
{

   XtAddCallback(textreptpeprintPB, XmNactivateCallback, ServBkup_PE_Print, NULL);
   
   return;
}
//---------------------------------------------------------


void	TextReports_Remove_ServBkup_PE_Callbacks(void)
{

   XtRemoveCallback(textreptpeprintPB, XmNactivateCallback, ServBkup_PE_Print, NULL);
   
   return;
}
//---------------------------------------------------------


char*	TextReports_getCurrentLid(void)
{
   return(textrept_lid);
}
//---------------------------------------------------------


char*	TextReports_GetDate(void)
{
   static char	date[15];
   static char	*date_ptr = NULL;
   
   time_t	curr_time;
   struct tm	*tm_ptr;
   
   if (date_ptr == NULL)
   {
      memset ( (void * ) date, '\0', sizeof(date));
      time(&curr_time);
      tm_ptr = localtime(&curr_time);
      strftime(date, 14+1, "%m/%d/%Y", tm_ptr);
      date_ptr = &date[0];
   }

   return(date_ptr);
}
//---------------------------------------------------------


char*	TextReports_GetHydrologistName(void)
{
   static char	name[BUFSIZ];
   Admin	*admin = NULL;
   
   memset ( (void * ) name, '\0', sizeof(name));
   if ((admin = (Admin *) GetAdmin(NULL)) != NULL)
   {
      strcpy(name, admin->focalpoint);
      FreeAdmin(admin);
   }
   
   return(name);
}
//---------------------------------------------------------


char*	TextReports_GetUserName(void)
{
   char *       pFileTemplate = "XXXXXX\0";
   char		tmp_dir[128],
                tmp1[BUFSIZ],
      		tmp2[BUFSIZ],
		tmp3[BUFSIZ],
		tmp4[BUFSIZ];
   
   static char	name[128];
   
   FILE*	fp = NULL;
   int          file_descriptor;

   
   memset ( (void * ) tmp1, '\0', sizeof(tmp1));
   memset ( (void * ) tmp2, '\0', sizeof(tmp2));
   memset ( (void * ) tmp3, '\0', sizeof(tmp3));
   memset ( (void * ) tmp4, '\0', sizeof(tmp4));
   memset ( (void * ) name, '\0', sizeof(name));

   /*
   	Build the whoami command & run it.
	Then, execute a finger command to find the name InRealLife.
   */
   /* Replaced the call to tempnam with a call to mkstemp. */
   strcpy ( tmp_dir, "/usr/tmp/" );
   strcat ( tmp_dir, pFileTemplate );
   file_descriptor = mkstemp ( tmp_dir );

   if ( file_descriptor >= 0 )
   {
      /* The temporary file has been successfully created. */ 
      close ( file_descriptor );

      sprintf(tmp1, "/usr/bin/whoami > %-s", tmp_dir);
      system(tmp1);
      
      if ((fp = fopen(tmp_dir, "r")) != NULL)
      {
	 fgets(tmp2, 128, fp);
	 tmp2[strlen(tmp2)-1] = '\0';
	 
	 fclose(fp);

	 sprintf(tmp3, "/usr/bin/finger %-s > %-s 2> /dev/null", tmp2, tmp_dir);
	 system(tmp3);

	 if ((fp = fopen(tmp_dir, "r")) != NULL)
	 {
	    strcpy(name, TextReports_InRealLife(fp));	/* get the name */
	    fclose(fp);					/* close the fp */

	    sprintf(tmp4, "rm %-s", tmp_dir);
	    system(tmp4);	    		/* remove the temporary file */

	    return(name);
	 }
      }
   }


   /*
   	Failure.
   */
   fprintf(stderr, "ERROR: Could not create temporary file...\n");
   return(NULL);
}
//---------------------------------------------------------


char*	TextReports_InRealLife(FILE *fp)
{
   static char	search[1005];
   char		tmp1[200],
      		tmp2[200],
		tmp3[200];
   
   char		*search_str = "In real life: ";

   
   memset ( (void * ) search, '\0', sizeof(search));
   if (fp != NULL)
   {
      fscanf(fp, "%s%s%s",  tmp1, tmp2, tmp3);
      fscanf(fp, "%s%s%s ", tmp1, tmp2, tmp3);

      sprintf(search, "%s %s %s ", tmp1, tmp2, tmp3);
      if (strcmp(search, search_str) == 0)
      {
	 fgets(search, 128, fp);
	 search[strlen(search)-1] = '\0';
      }
   }

   
   if (strlen(search) > 0)
   {
      return(search);
   }
   else
   {
      return(NULL);  /* failure */
   }
}
//---------------------------------------------------------




void	HPPrinterCodes_StandardCompressed(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s%c%s%c%s%c%s",
	   esc, "(0U", esc, "(s0P", esc, "&k2S", esc, "(s0B");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_StandardPitch10(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s%c%s%c%s%c%s",
	   esc, "(0U", esc, "(s0P", esc, "&k0S", esc, "(s0B");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_StandardPitch12(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s%c%s%c%s%c%s",
	   esc, "(0U", esc, "(s0P", esc, "&k4S", esc, "(s0B");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_Bold(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s%c%s",
	   esc, "(0U", esc, "(s3B");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_Normal(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s",
	   esc, "(s0B");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_Italic(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s",
	   esc, "(s1S");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_Upright(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s",
	   esc, "(s0S");
   
   strcat(buf, lprcode);
   return;
}
//---------------------------------------------------------


void	HPPrinterCodes_FormFeed(char *buf)
{
   char		esc='\x1B';
   char		lprcode[BUFSIZ];
   
   memset ( (void * ) lprcode, '\0', sizeof(lprcode));
   sprintf(lprcode, "%c%s",
	   esc, "&l0H");	/* & - ell - zero - H */
   
   strcat(buf, lprcode);
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
//---------------------------------------------------------


