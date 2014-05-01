/****************************************************************************
   File:           pointcontrol_tabulate.c
   
   History:
      Bryon Lawrence   03/10/02    Updated this file to reduce the possibility
                                   of NULL pointer reads.  This is being done
                                   in response to DR 10530.

   **************************************************************************/

#include <stdio.h>

#include "GeneralUtil.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "pointdata_table.h"
#include "rating_util.h"
#include "get_loc_info.h"

/* need a persistent local variable for the report list
   for use in the time series invoke function and file save */

static ReportList *rep_globalHead;


/* global variables for print and save operations */

static char	temp_print_file[110];
Widget pc_tableFileSB = (Widget) NULL;


/******************************************************************************
   Point Control Table Dialog    
   ***************************************************************************/

void show_pointcontrol_tableDS(Widget 		w,
			       ReportList	*tablerepHead)
{
   
   /* create the window */
   
   if ( ( pc_tableDS == NULL ) || ( ! XtIsManaged ( pc_tableDS ) ) )
   {      
      create_pc_tableDS(GetTopShell(w));
                 
      /* add the callbacks */
      
      pc_table_callbacks();  
                
   }
   
   /* copy the value of the report list pointer to a variable global to this file */
   
   rep_globalHead = tablerepHead;
   
   
   /* load the list with the data */
   
   load_pointtable(tablerepHead);
   
   
   /* manage the form and shell */
  
   XtManageChild(pc_tableFO);
   XtManageChild(pc_tableDS);
   
   
   /* raise the window to the top of the display stack */
   
   XRaiseWindow(XtDisplay(pc_tableDS), XtWindow(pc_tableDS));
   
   
   return;   
}


/***************************************************************************** 
   pc_table_callbacks() 
   add callbacks 
   **************************************************************************/

void pc_table_callbacks()
{

   XtAddCallback(pc_table_tabtsPB,   XmNactivateCallback, pc_invoke_timeseries,
                 ( XtPointer ) TABULAR_TS);
   XtAddCallback(pc_table_graphtsPB, XmNactivateCallback, pc_invoke_timeseries,
                 ( XtPointer ) GRAPH_TS);
   
   XtAddCallback( pc_table_printPB , XmNactivateCallback , pc_table_print ,
                  NULL ) ;
   XtAddCallback( pc_table_savePB ,  XmNactivateCallback , 
                  pc_table_get_filename , NULL ) ;
   
   XtAddCallback( pc_table_closePB , XmNactivateCallback , pc_table_CloseCB ,
                  ( XtPointer ) NULL ) ;

   return;
}


/****************************************************************************

   **************************************************************************/
void load_pointtable(ReportList	*tablerepHead)
{
   int		rep_count = 0 ;
   int		list_count;
   int		alloc_count = 1 ;
   int          is_file = 0;
   char		buf[100];
   ReportList	*rPtr = NULL ;
   int		i;
   XmStringTable  xmStr;
   Arg         arg[40];   
   int         ac;
   
   /* allocate memory for the list. this may over allocate
      the list since some reports may be filtered out. */
   if ( tablerepHead != NULL ) 
   {
      rep_count = ListCount(&tablerepHead->list);
     
      if ( rep_count > 0 ) alloc_count = rep_count ;
   }
   
   xmStr  = ( XmStringTable ) XtMalloc(alloc_count * sizeof(XmString *));
   
   /* load the observed items into the scrolled lists */
   list_count = 0;
   
   if (rep_count > 0)
   {
      rPtr = (ReportList *) ListFirst(&tablerepHead->list);
      
      while (rPtr)
      {
         if ( rPtr->use )
         {
            build_pointdata_table_entry ( rPtr , buf, is_file );	 
            xmStr [ list_count ++ ] = XmStringCreate ( buf , 
                                                     XmFONTLIST_DEFAULT_TAG ) ;
         }

	 rPtr = (ReportList *) ListNext(&rPtr->node);
      }
   }
   
   
   /* if no report then display message */
   
   if (list_count == 0)
   {
      sprintf(buf, "No data for the specified request.");
      xmStr[0] = XmStringCreate ( buf , XmFONTLIST_DEFAULT_TAG ) ;
      
      list_count = 1;
   }
      
     
   /* load XmList widget  */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, list_count);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(pc_tableLS, arg, ac);
        
   
   /* cleanup and return */
   
   for (i = 0; i < list_count; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   
   return;
} 

/*********************************************************************
   pc_invoke_timeseries()
   
   PURPOSE
   Invoke the time series control window.
   
   *********************************************************************/

void pc_invoke_timeseries(Widget w, XtPointer ptr, XtPointer call_data )
{
   int		itemnum = 0 ;
   TSGEN_INFO  	ts_invoke;
   int		*poslist = NULL , count;
   int 		display_mode;
   ReportList	*rPtr = NULL ;
   
   /* get the graph or table choice as passed through the callback.
      this argument is then used below when invoking the time series. */
   
   display_mode = ( int ) ptr;
   
   
   /* get currently selected list position and determine the 
      forecast point it is for */
   
   XmListGetSelectedPos(pc_tableLS, &poslist, &count);

   if ( poslist != NULL )
   {
      itemnum = poslist[0];
      free(poslist);
   }
   
   if (itemnum < 1)
   {
      fprintf(stderr, "No item selected - time series not invoked\n");
      return;
   }

   if (rep_globalHead == NULL)
   {
      fprintf(stderr, "No data in list - time series not invoked\n");
      return;
   }
   
   
   /* get lid, pe, ts, extremum, and time from selected item.
      because the displayed list only shows the stations that passed the
      station filter, and the linked list of reports has all stations,
      then we need to sort through the list*/
   
   count = 0;
   rPtr = (ReportList *) ListFirst(&rep_globalHead->list);
   
   while (rPtr)
   {	 
      if (rPtr->use)
      {
	 if (count == (itemnum - 1))
	 {
	    break;
	 }
	 count++;	    
      }
      
      rPtr = (ReportList *) ListNext(&rPtr->node);
   }
   
   
   /* load in the structure that controls the time series 
      invocation */
   
   ts_invoke.display_mode   = display_mode;
   ts_invoke.group_check    = FALSE;      
   ts_invoke.standalone     = NON_STANDALONE;  
   ts_invoke.pedtse_defined = TRUE;
   
   strcpy(ts_invoke.lid,rPtr->lid);     
   
   ts_invoke.Begin_time = rPtr->validtime - 3600*48;
   ts_invoke.End_time   = rPtr->validtime + 3600*24;
         
   ts_invoke.nitems = 1;
   
   strcpy(ts_invoke.pedtse[0].pe, rPtr->pe);
   strcpy(ts_invoke.pedtse[0].ts, rPtr->ts);
   strcpy(ts_invoke.pedtse[0].extremum, rPtr->extremum);
   ts_invoke.pedtse[0].dur = 0;
     
   
   /* lastly invokve the timeseries control window with the
      proper argument information */
   
   show_TSControlDS(w, ts_invoke);
   
   
   return;
}


/*************************************************************************
   
*************************************************************************/
void pc_table_print(Widget w, XtPointer ptr, XtPointer cbs)
{
   char 	cmd_str[200];
   char         lpr_print[128];
   char         command[200];
   char 	report_dir[128];
   int          gad_token_len=0, gad_value_len=0;
   int		status;
   
   
   /* get the name of the report directory and set the temp file name */
   gad_token_len = strlen("whfs_report_dir");
   status = get_apps_defaults("whfs_report_dir", &gad_token_len,
			      report_dir, &gad_value_len);
      
   if (status == 0)
      sprintf(temp_print_file, "%s/temp_file", report_dir);
   else
   {
      fprintf(stderr,
		 "whfs_report_dir token not defined; using current directory.\n");
      sprintf(temp_print_file, "%s/temp_file", ".");
   }
   
   
   /* create the file */
   
   if (create_pctable_file() != 0)
   {    
      printf("printing report file: %s\n", temp_print_file); 
      
      
      /* print and remove the file */
       
       gad_token_len = strlen("whfs_printcommand"); /*token for the print command*/
       get_apps_defaults("whfs_printcommand", &gad_token_len,lpr_print, &gad_value_len);
       if(strlen(lpr_print) > 0)
       {
          strcpy(command,lpr_print);
       }
       else
       {
          strcpy(command, "lpr");
       }
      
      
      sprintf(cmd_str, "%s %s", command,temp_print_file);
      system(cmd_str);
      
      sprintf(cmd_str, "rm  %s", temp_print_file);
      system(cmd_str);
   }
   
   return;
   
}


/*************************************************************************
   
*************************************************************************/

int create_pctable_file(void)
{
   char		msg[BUFSIZ];
   FILE		*pfilePtr = NULL;
   int		rep_count = 0 ;
   ReportList	*rPtr;
   char 	obstime_ansi[ANSI_TIME_LEN+1]; 
   char 	buf[120];
   time_t	curtimet;
   int          is_file = 1;
   int		report_found = 0;
   
   /* open print file for writing */
   
   pfilePtr = fopen(temp_print_file, "w");
   if (pfilePtr == NULL)
   {
      sprintf(msg, "File: %s\nUser does NOT have write access.\n",
	      temp_print_file);
      ErrorDialog(pc_tableDS, msg);
      return(0);
   }
   
   
   /* write to the file */
   
   time(&curtimet);
   timet_to_yearsec_ansi(curtimet, obstime_ansi);	 
   fprintf(pfilePtr, "Listing of IHFS synoptic data created: %s\n\n", obstime_ansi);
   
   
   /* load the observed items into the scrolled lists */
   
   if ( rep_globalHead != NULL )
   {
      rep_count = ListCount(&rep_globalHead->list);
   }
   
   if (rep_count > 0)
   {      
      rPtr = (ReportList *) ListFirst(&rep_globalHead->list);
      
      while ( rPtr )
      {
	 /* only display the value if it passed the filter */
	 
	 if ( rPtr->use )
	 {
            /* Check if the header has already been printed. */
            if ( report_found == 0 )
            {
               fprintf ( pfilePtr, "%-7s %-15s %-6s %-8s %-13s " 
                                   "%-2s %-2s %-1s %-1s [%6s %5s]\n\n",
                                   "Id", "Name", "Value", "Stg/Flw", "Time",
                                   "PE", "TS", "D", "E", "Fld", "Dep" ); 
            }

            report_found = 1;
            build_pointdata_table_entry ( rPtr , buf, is_file );	 
	    fprintf(pfilePtr, "%s", buf);
	 }  /* end of if on whether report is used */	 
	 
	 rPtr = (ReportList *) ListNext(&rPtr->node);
      }  /* end of while on report data */
      
      
      /* it is possible that records were retrieved, but were
         filtered out. indicate this special case as necessary */
      
      if (!report_found)
      {
         fprintf(pfilePtr, "\nNo data for specified request.\n\n");
      }
      
   }  /* end of check if any records retrieved */
   
   
   /* if no records retreived, indicate this  */
   
   else
      fprintf(pfilePtr, "\nNo data for the specified request.\n\n");
      
   
   fprintf(pfilePtr, "\n** End of listing **\n");
     
   
   /* close the print file */
   
   fclose(pfilePtr);

   return(1);
}


/*************************************************************************
   
*************************************************************************/
void pc_table_get_filename(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char         report_dir[128];
   int          gad_token_len=0, gad_value_len=0;
   int		status;
   
   
   if(! pc_tableFileSB)
   {
      /* get the name of the report directory and
	 set the temp file name */
      
      gad_token_len = strlen("whfs_report_dir");
      status = get_apps_defaults("whfs_report_dir", &gad_token_len,
			report_dir, &gad_value_len);
      
      if (status == 0)
	 sprintf(temp_print_file, "%s/temp_file", report_dir);
      else
      {
	 fprintf(stderr,
		 "whfs_report_dir token not defined; using current directory.\n");
	 sprintf(temp_print_file, "%s/temp_file", ".");
      }

      
      /* create the file selection window */
      
      pc_tableFileSB = XmCreateFileSelectionDialog(GetTopShell(w),
						   "pc_tableFileSB", NULL, 0);
            
      /* set the necessary X args */
      
      ac = 0;
      
      str = XmStringCreateSimple("*");
      XtSetArg(args[ac], XmNpattern, str); ac++;
      
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
            
      str = XmStringCreateSimple(report_dir);      
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      
      XtSetValues(pc_tableFileSB, args, ac);
      
      XmStringFree(str);
      
      
      /* add the callbacks to the file selection box */
      
      XtAddCallback ( pc_tableFileSB , XmNcancelCallback , 
                      pc_table_close_filesb , NULL ) ;
      XtAddCallback ( pc_tableFileSB , XmNokCallback , pc_table_save_table ,
                      NULL) ;
      
      list = XmFileSelectionBoxGetChild(pc_tableFileSB, XmDIALOG_LIST);
      XtAddCallback(list, XmNdefaultActionCallback, pc_table_save_table, NULL);
   }   
   
   SetTitle(pc_tableFileSB, "Save File Selection");
   
   if(! XtIsManaged(pc_tableFileSB))
      XtManageChild(pc_tableFileSB);
   
   return;
}


/*************************************************************************
   
***********************************************************************/
void pc_table_save_table(Widget 				w,
			 XtPointer 				ptr,
                         XtPointer                          call_data )
{ 
   XmFileSelectionBoxCallbackStruct * cbs =
          ( XmFileSelectionBoxCallbackStruct * ) call_data ;
   XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
   char *name = NULL;
   char cmd_str[300];
   
   if(XtIsManaged(pc_tableFileSB))
   {
      XtDestroyWidget(pc_tableFileSB);
      pc_tableFileSB = NULL;
   }
   
   if (! XmStringGetLtoR(cbs->value, charset, &name))
      return;
   
   if (! *name)
   {
      XtFree(name);
      return;
   }
   
   
   if (create_pctable_file() != 0)
   {
      printf("saving report file: %s\n", name);
      sprintf(cmd_str, "mv %s %s", temp_print_file, name);
      system(cmd_str);
   }
   
   XtFree(name);
   
   return;   
}


/*************************************************************************
   
   **********************************************************************/
void pc_table_close_filesb(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(pc_tableFileSB))
   {
      XtDestroyWidget(pc_tableFileSB);
      pc_tableFileSB = NULL;
   }
   
   return;
}


/****************************************************************************
   close callback for  Point Display Control TableDialog
   **************************************************************************/
void pc_table_CloseCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   XtDestroyWidget(pc_tableDS);
   pc_tableDS = NULL;
   
   return;
}
