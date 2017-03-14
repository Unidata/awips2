/******************************************************************
File:		tabular_show.c
Date:		September 1995, January 2000, June 2006
Author:		Sung Vo,Russell Erb,MarkG	
******************************************************************/

#include "CurPC.h"
#include "CurPP.h"

#include "GeneralUtil.h"
#include "load_maxfcst.h"
#include "tabular_show.h"
#include "TSControl_show.h"
#include "TSutils.h"
#include "get_precip_settings.h"    /* for SHEFHOUR_IN_MINUTES */
#include "gpp_input.h"
#include "tabular.h"

#define MAX_TS_ON_LIST 120

static float 	oldValue = 0;
static char  	product_id[11] = "CCCCNNNXXX";
static char  	issue_script[110];
static char  	edit_script[110];
static char  	current_shef_file[110];
static char  	temp_print_file[110];
static char  	Begintime_ANSI[ANSI_TIME_LEN+1];
static char  	Endtime_ANSI[ANSI_TIME_LEN+1];
static RussText modified_ts_list[MAX_TS_ON_LIST];
static TimeSeriesType current_timeseries;
static TAB_INFO current_tab_info;
Widget tabularFileSB = (Widget) NULL;

#define UNDEFINED_TYPESOURCE "??"
static char  InsertProdID[11] = "CCCCWRKXXX";
static char  InsertProdTime_ANSI[ANSI_TIME_LEN+1];
static char  InsertBasisTime_ANSI[ANSI_TIME_LEN+1];
static char  InsertTypeSource[SHEF_TS_LEN + 1] = UNDEFINED_TYPESOURCE;

#define MAX_FCST_TYPESOURCE 50
static char  forecast_typesource_string[MAX_FCST_TYPESOURCE * SHEF_TS_LEN + 1] = "";
static int   num_forecast_typesource = 0;

int gpp_enable = 0;

/*************************************************************************/

FcstStruct *getFcstStruct(void)
{
   static  FcstStruct fcstStruct;

   return &fcstStruct;
}

/*************************************************************************/

void initFcstStruct(FcstStruct *fcstStruct)
{
     if (fcstStruct->fcstHead)
     {
	  FreeForecast(fcstStruct->fcstHead);
	  fcstStruct->fcstHead = NULL;   		  
     }

     return;
}

/*************************************************************************/

ObsStruct * getObsStruct(void)
{
     static ObsStruct obsStruct;
         
     return &obsStruct;
}

/*************************************************************************/

void initObsStruct(ObsStruct *obsStruct)
{
     if (obsStruct->obsHead)
     {
	  FreeObservation(obsStruct->obsHead);
	  obsStruct->obsHead = NULL;   		  
     }

     return;
}


/*************************************************************************/

void	tabular_show(Widget w, TAB_INFO tab_info)
{	  
     char	buf[BUFSIZ];
     char	labeltext[BUFSIZ];
     char	gad_value [ 128 ] , edit_script_dir [ 128 ] ;
     long	pidval;
     int	ctr, gad_token_len=0, gad_value_len=0, rcfad=0;
     time_t     currentTime = 0;
     char	shef_issue_prodid[128];


     /* load the info about the available time series */
     
     if(current_tab_info.buf)
	free(current_tab_info.buf);

     current_tab_info.buf = NULL;
     current_tab_info.buf = (RussText *)malloc(sizeof(RussText)*(tab_info.nitems));

     current_tab_info.nitems       = tab_info.nitems;
     current_tab_info.selected_pos = tab_info.selected_pos;
     current_tab_info.Begin_time   = tab_info.Begin_time;
     current_tab_info.End_time     = tab_info.End_time;
     
     for (ctr = 0; ctr < tab_info.nitems; ctr++)
	strcpy(current_tab_info.buf[ctr], tab_info.buf[ctr]);


     /* set the file names for WHFS editor, SHEF issue script
        and the current SHEF product file for possible later use. */
     
     gad_token_len  =  strlen ("whfs_local_bin_dir");
     rcfad = get_apps_defaults("whfs_local_bin_dir", &gad_token_len, 
                               gad_value, &gad_value_len);
     if (rcfad == 0)
	sprintf(issue_script, "%s/shef_issue", gad_value);
     else
	sprintf(issue_script, "%s/shef_issue", ".");


     /* retrieve the directory where the whfs editor script is located. */
     
     gad_token_len  =  strlen ( "whfs_local_bin_dir" ) ;
     rcfad = get_apps_defaults ( "whfs_local_bin_dir" , & gad_token_len , 
                                  gad_value , & gad_value_len ) ;

     if (rcfad == 0 )
        sprintf ( edit_script_dir , "%s" , gad_value ) ;
     else
        sprintf ( edit_script_dir , "%s" , ".") ;


     /* retrieve the name of the whfs editor script to use. */
     
     gad_token_len  =  strlen ( "whfs_editor" ) ;
     rcfad = get_apps_defaults ( "whfs_editor" , & gad_token_len ,
                                  gad_value , & gad_value_len ) ;
     if ( rcfad == 0 )
	sprintf( edit_script , "%s/%s" , edit_script_dir , gad_value ) ;
     else
	sprintf( edit_script , "%s/%s" , edit_script_dir , "whfs_editor" ) ;

     pidval = (long )getpid();
     gad_token_len  =  strlen ("whfs_product_dir");
     rcfad = get_apps_defaults("whfs_product_dir", &gad_token_len, 
                               gad_value, &gad_value_len);
     if (rcfad == 0)
	sprintf(current_shef_file,  "%s/shef_product.%ld", gad_value, pidval);
     else
	sprintf(current_shef_file,  "%s/shef_product.%ld", ".", pidval);

     gad_token_len  =  strlen ("whfs_report_dir");
     rcfad = get_apps_defaults("whfs_report_dir", &gad_token_len, 
                               gad_value, &gad_value_len);
     if (rcfad == 0)
	sprintf(temp_print_file, "%s/temp_print_file", gad_value);
     else     
	sprintf(temp_print_file, "%s/temp_print_file", ".");


     /* check if gpp enable is turned on */

     gad_token_len  =  strlen ( "gage_pp_enable" ) ;
     rcfad = get_apps_defaults ( "gage_pp_enable" , & gad_token_len ,
                                  gad_value , & gad_value_len ) ;
     if ( rcfad == 0 )
     {
        if ( strncmp(gad_value, "ON", 2) == 0 )
           gpp_enable = 1;
        else
           gpp_enable = 0;
     }
     else
     {
        gpp_enable = 0;
     }


     /* currently runs every time, since dialog is destroyed on close */
     
     if (! tabularDS)
     {
	  create_tabularDS(GetTopShell(w));
	  
	  /* Set up the callbacks */
	  
	  tabular_callbacks();
     }
     
     
     if (! XtIsManaged(tabularDS))
     {
	  /* Set the Title of the dialog */
	  
	  sprintf(buf, "Tabular Time Series");
	  SetTitle(tabularDS, buf);
	  
	  
	  /* Set the column headings for the scrolled list */
	  
	  SetLabel(tabularfloodLA, "Flood Stg/Flow: N/A");
	  SetLabel(tabularheaderLA, HDRDEFAULT);
	  
	  
	  /* Set productID default for out going SHEFencoded products */
	  
	  gad_token_len = strlen("shefencode_prodid");
	  get_apps_defaults("shefencode_prodid", &gad_token_len, 
	                    shef_issue_prodid, &gad_value_len);
	  if (strlen(shef_issue_prodid) > 0)
	  {
		strncpy(product_id, shef_issue_prodid, 10);
		strcat (product_id, "\0");
	  }

	  XmTextSetString(tabularpilTE, product_id);


	  /* set insert product id, time, basis time info.  */
	  
	  time(&currentTime);
	  timet_to_yearsec_ansi(currentTime, &InsertProdTime_ANSI[0]);
	  SetLabel(tabularInsertProdTimeLA, InsertProdTime_ANSI);

	  strcpy(InsertProdID, "CCCCWRKXXX");
	  SetLabel(tabularInsertProdIDLA, InsertProdID);	  
	
          timet_to_yearsec_ansi(currentTime, &InsertBasisTime_ANSI[0]);
          SetLabel(tabularInsertBasisLA, InsertBasisTime_ANSI);
	  

          /* load the type source. */
    
          SetLabel(tabularInsertTypSrcLA, InsertTypeSource);


	  /* Manage dialog shell and form */
	  
	  XtManageChild(tabularFM);
	  XtManageChild(tabularDS);

	  XmToggleButtonSetState(tabularlistallTB,False,False);
     }


     /* set Begin-End Time label, truncating the info a bit. */
	
     timet_to_yearsec_ansi(current_tab_info.Begin_time, Begintime_ANSI);
     sprintf(buf, "%s", Begintime_ANSI);
     buf[16] = '\0';
     sprintf(labeltext, "%s - ", buf);
     
     timet_to_yearsec_ansi(current_tab_info.End_time, Endtime_ANSI);
     sprintf(buf, "%s", Endtime_ANSI);
     buf[16] = '\0';
     strcat(labeltext, buf);
	
     XtUnmanageChild(tabularportimeLA);
     SetLabel(tabularportimeLA, labeltext);
     XtManageChild(tabularportimeLA);

      
     /* load the list of difference time series sets.
        this also loads the type source string for use in the forecast
	copy operation. */
	
     tabular_load_timeseries();
     

     /* retrieve the data from the dbms.*/
	
     tabular_retrieve_data(w, NULL, NULL);


     /* stvo added 01/14/2000 */
	
     XRaiseWindow(XtDisplay(tabularDS), XtWindow(tabularDS));
     
     
     return;
}

/*************************************************************************/

void	tabular_callbacks(void)
{
     /* window manager callbacks. */

     Atom   atom;
     atom = XmInternAtom(XtDisplay(tabularDS), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(tabularDS, atom, tabular_close, NULL);
     
     
     /* push button callbacks */
     
     XtAddCallback(tabularApplyPB,  XmNactivateCallback, tabular_save,        NULL);
     XtAddCallback(tabularMsgPB,    XmNactivateCallback, tabular_setmissing,  NULL);
     XtAddCallback(tabularSetQCPB,  XmNactivateCallback, tabular_setqc,       NULL);
     XtAddCallback(tabularDeletePB, XmNactivateCallback, tabular_del_conf,    NULL);
    
     XtAddCallback(tabularCancelPB, XmNactivateCallback, tabular_close,       NULL);
     XtAddCallback(tabularEncodePB, XmNactivateCallback, tabular_shef_encode, NULL);
     XtAddCallback(tabularSendPB,   XmNactivateCallback, tabular_send_conf,   NULL);
     XtAddCallback(tabularClearPB,  XmNactivateCallback, tabular_clear_conf,  NULL);
     XtAddCallback(tabularEditPB,   XmNactivateCallback, tabular_edit_shef,   NULL);
     XtAddCallback(tabularIssuePB,  XmNactivateCallback, tabular_edit_issue,  NULL);
     XtAddCallback(tabularPrintPB,  XmNactivateCallback, tabular_print_table, NULL);
     XtAddCallback(tabularSavePB,   XmNactivateCallback, tabular_get_filename,NULL);
     
     XtAddCallback(tabularInsertEditPB,XmNactivateCallback,tabular_insert_edit,NULL);
     XtAddCallback(tabularInsertCopyPB,XmNactivateCallback,tabular_copy_ts, NULL);

     XtAddCallback(tabularlistallTB, XmNvalueChangedCallback ,tabularLatestBasis, NULL);
     
     
     /* list callbacks */
     
     XtAddCallback(tabularLI, XmNextendedSelectionCallback,
		   TS_data_RowSelection, NULL);
     
     XtAddCallback(tabulartsLI, XmNsingleSelectionCallback,
		   TS_type_RowSelection, NULL);
     
     
     /* add TextFilter callbacks. */

     XtAddCallback(tabularvalTE, XmNmodifyVerifyCallback,
                   (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_SIGN);

     XtAddCallback(tabularpilTE, XmNmodifyVerifyCallback,
                   (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);


     return;
}


/*************************************************************************/

void	tabular_retrieve_data(Widget w, XtPointer ptr, XtPointer cbs)
{  
     ObsStruct * obsStruct = getObsStruct();
     FcstStruct * fcstStruct = getFcstStruct();

     char	tablename[BUFSIZ];
     char       where[BUFSIZ];
     char       fcst_where[BUFSIZ];
     
     
     /* get the applicable table name */
     
     getTableName(current_timeseries.pe, current_timeseries.ts, tablename);
     
     
     /* clear out the type source since the available type-sources
        may have changed */
	
     strcpy(InsertTypeSource, UNDEFINED_TYPESOURCE);
     SetLabel(tabularInsertTypSrcLA, InsertTypeSource);
     
     
     /* clear the form */
     
     clearForm(tabularElemFO);
          
	  
     /* free any old forecast information */
     
     if (fcstStruct->fcstHead)
     {
	FreeForecast(fcstStruct->fcstHead);
	fcstStruct->fcstHead = NULL;
     }
     
     
     /* free any old observation information */
     
     if (obsStruct->obsHead)
     {
	FreeObservation(obsStruct->obsHead);
	obsStruct->obsHead = NULL;
     }
     
     
     /* Load the XmList with forecasts or observations. */
     
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
	createGetWhereFcst(fcst_where);

	/* get current forecasts */ 
	  
	if (strcmp(current_timeseries.basistime, "No Data") != 0)
        {
	   fcstStruct->fcstHead = GetForecast(fcst_where, tablename);
        }

	loadTabularListFcst(fcstStruct->fcstHead);
	
	
	/* now that we know what specific time series is loaded, load a list
	   of type sources that match the selected pe, dur, extremum 
	   for possible use later in the copy forecast operation */
	   
        load_forecast_typesource(current_timeseries.pe, 
	                         current_timeseries.dur,
                                 current_timeseries.extremum);
     }
     
     else
     {
	createGetWhereObs(where);

	/* get current observations */ 
	  
	obsStruct->obsHead = GetObservation(where, tablename);

	loadTabularListObs(obsStruct->obsHead);
     }
	
     return;  
}


/*************************************************************************/

void	createGetWhereObs(char *where)
{	
     char	buf[BUFSIZ];

     /* clear memory buffer. */	
     
     sprintf(where, " WHERE lid='%s' ", current_timeseries.lid);
      
     /* PEDTSE filter. */
     
     sprintf(buf, "AND (pe='%s') ", current_timeseries.pe); 
     strcat(where, buf);	
     sprintf(buf, "AND (dur=%d) ", current_timeseries.dur); 
     strcat(where, buf);	
     sprintf(buf, "AND (ts='%s') ", current_timeseries.ts); 
     strcat(where, buf);	
     sprintf(buf, "AND (extremum='%s') ", current_timeseries.extremum); 
     strcat(where, buf);	

     sprintf(buf, "AND (obstime >= '%s') ", Begintime_ANSI); 
     strcat(where, buf);	
     sprintf(buf, "AND (obstime <= '%s') ", Endtime_ANSI); 
     strcat(where, buf);	

     strcat(where, " ORDER BY obstime DESC ");
     
     return;   
}


/*************************************************************************/

void	createGetWhereFcst(char *fcst_where)
{	
     char	buf[BUFSIZ];

     
     /* clear memory buffer. */	
     
     sprintf(fcst_where, " WHERE lid='%s' ", current_timeseries.lid);
           
     /* PEDTSE filter. */
     
     sprintf(buf, "AND (pe='%s') ", current_timeseries.pe); 
     strcat(fcst_where, buf);	
     sprintf(buf, "AND (dur=%d) ", current_timeseries.dur); 
     strcat(fcst_where, buf);	
     sprintf(buf, "AND (ts='%s') ", current_timeseries.ts); 
     strcat(fcst_where, buf);	
     sprintf(buf, "AND (extremum='%s') ", current_timeseries.extremum); 
     strcat(fcst_where, buf);
     
     sprintf(buf, "AND (basistime='%s') ", current_timeseries.basistime); 
     strcat(fcst_where, buf);
     sprintf(buf, "AND (validtime >= '%s') ", Begintime_ANSI); 
     strcat(fcst_where, buf);	
     sprintf(buf, "AND (validtime <= '%s') ", Endtime_ANSI); 
     strcat(fcst_where, buf);	

     strcat(fcst_where, " ORDER BY validtime DESC ");

     return;   
}


/*************************************************************************/
/* Loads the data retrieved from the database query into
   the tabularLI (tabular data ListBox) */

void	loadTabularListObs(Observation *obsHead)
{  
     Observation        *obsPtr;
     XmStringTable	xmStr;
     char	     	obstime_ansi[ANSI_TIME_LEN];
     char               prodtime_ansi[ANSI_TIME_LEN];
     char               posttime_ansi[ANSI_TIME_LEN];
     char	        buf[BUFSIZ];	
     char	        where[BUFSIZ];	
     struct         	tm *tmptr;
     time_t		obstime, prodtime, posttime;
     int		cnt;
     int		i;
     char	     	range_check[MAXLEN_QCSYMBOL];
     char	     	revision;
     float		derived_value=0;
     int		rating_curve_exists=0;
     Riverstat  	*rHead,	*rPtr;
     char		floodFlow[25], floodStage[25], floodLabel[50];
     char		river_name[SHOW_RIVER_LEN + 1];
     
     memset(floodFlow,  '\0', sizeof(floodFlow));     
     memset(floodStage, '\0', sizeof(floodStage));     
     memset(floodLabel, '\0', sizeof(floodLabel));     

     /* Delete all XmList items. */
     
     XmListDeleteAllItems(tabularLI);
     
     
     /* create where clause to find records for this lid */
     
     sprintf(where, " WHERE lid='%s' ", current_timeseries.lid);

     strcpy(floodFlow,  " ");
     strcpy(floodStage, " ");
     strcpy(floodLabel, " ");
     
     rHead = GetRiverstat(where);
     if (rHead)
     {
		rPtr = (Riverstat *) ListFirst(&rHead->list);	
		while (rPtr) 
		{	
			if( ! IsNull ( FLOAT, &rPtr->fs) )
				sprintf(floodStage, "%.1f/", rPtr->fs);
				
			if( ! IsNull ( FLOAT, &rPtr->fq) )
				sprintf(floodFlow, "%.0f", rPtr->fq);
			
			if( ! IsNull ( CHAR, &rPtr->stream))
			{
				sprintf(river_name, "%s", rPtr->stream);
				break;
			}
          		else
			{
				sprintf(river_name, "%s", "UNDEFINED");	
				break;
			}	
		}
		FreeRiverstat(rHead);
     }
     
     if((strcmp(river_name, "UNDEFINED") == 0) || (!rHead))
     {
    	strcpy(floodLabel,  " ");
     }
     else
     { 
     	strcpy(floodLabel, "Flood Stg/Flow: ");
     	strcat(floodLabel, floodStage);
     	strcat(floodLabel, floodFlow);
     }
     
     XtUnmanageChild(tabularfloodLA);
     SetLabel(tabularfloodLA, floodLabel);
     XtManageChild(tabularfloodLA);

     /* determine which header to display. */
 
     XtUnmanageChild(tabularheaderLA);
     if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
	  (strncmp(current_timeseries.pe, "HT", 2) == 0))
	 && (recordCount("Rating", where) > 1))
     {
	  SetLabel(tabularheaderLA, HDRSTAGE);
	  rating_curve_exists=1;
     }
     else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
	      (recordCount("Rating", where) > 1))
     {
	
	  SetLabel(tabularheaderLA, HDRFLOW);
	  rating_curve_exists=1;
     }
     else
     {
	  SetLabel(tabularheaderLA, HDRDEFAULT);
	  rating_curve_exists=0;
     }  
   
     XtManageChild(tabularheaderLA);
      
     
     /* if no obs data, don't allow certain actions */
     
     if (obsHead == NULL)
     {
        DeSensitize(tabularApplyPB);
        DeSensitize(tabularDeletePB);
        DeSensitize(tabularSetQCPB);
        DeSensitize(tabularMsgPB);
	
	XmTextSetString(tabularqcdescTE, "");

        xmStr  = (XmStringTable) XtMalloc(sizeof(XmString *));
        xmStr[0] = XmStringCreateSimple("No Data");
        XmListAddItems(tabularLI, xmStr, 1, 1);
        XmStringFree(xmStr[0]);
        XtFree((char *) xmStr);
	
        Sensitize(tabularUseProductTB);
        DeSensitize(tabularUseBasisTB);
        DeSensitize(tabularUseTypSrcTB);

        XmToggleButtonSetState(tabularUseProductTB, True, False);
        XmToggleButtonSetState(tabularUseBasisTB,   False, False);
        XmToggleButtonSetState(tabularUseTypSrcTB,  False, False);
	
        return;
     }
     
     else
     {
	/* get count of number of observations in linked list. */
	
	cnt = ListCount(&obsHead->list);
	
        Sensitize(tabularUseProductTB);
        DeSensitize(tabularUseBasisTB);
        DeSensitize(tabularUseTypSrcTB);
	
        XmToggleButtonSetState(tabularUseProductTB, True, False);
        XmToggleButtonSetState(tabularUseBasisTB,   False, False);
        XmToggleButtonSetState(tabularUseTypSrcTB,  False, False);
     }     

     
     xmStr  = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
     obsPtr = (Observation *) ListFirst(&obsHead->list);


     /* Loop through the list of observations.*/
     
     for (i = 0; i < cnt; i++)
     {
	  yearsec_dt_to_timet(obsPtr->obstime, &obstime);
	  tmptr = gmtime(&obstime);
	  strftime(obstime_ansi, sizeof(obstime_ansi), "%m/%d %H:%M", tmptr);

	  yearsec_dt_to_timet(obsPtr->producttime, &prodtime);
	  tmptr = gmtime(&prodtime);
	  strftime(prodtime_ansi, sizeof(prodtime_ansi), "%m/%d %H:%M", tmptr);

	  yearsec_dt_to_timet(obsPtr->postingtime, &posttime);
	  tmptr = gmtime(&posttime);
	  strftime(posttime_ansi, sizeof(posttime_ansi), "%m/%d %H:%M", tmptr);

	  build_qc_symbol(obsPtr->quality_code, range_check);

	  if (obsPtr->revision == 1)
	  	revision = 'T';
	  else
	  	revision = 'F';
	  
	  if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
	       (strncmp(current_timeseries.pe, "HT", 2) == 0))
	      && (rating_curve_exists == 1))
	  {
		if (obsPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = stage2discharge(current_timeseries.lid, 
			                                obsPtr->value);

		sprintf(buf, "%8.2f %8.0f %11s %c %1s %1s %10s %11s %11s",
			obsPtr->value, derived_value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);
	  }
	  else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
		   (rating_curve_exists == 1))
	  {
		if (obsPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = discharge2stage(current_timeseries.lid, 
			                                obsPtr->value);

		sprintf(buf, "%8.2f %8.2f %11s %c %1s %1s %10s %11s %11s",
			obsPtr->value, derived_value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);
	  }
	  else
		sprintf(buf, "%8.2f %11s %c %1s %1s %10s %11s %11s",
			obsPtr->value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);
	  
	  
	  xmStr[i] = XmStringCreateSimple(buf);
	  obsPtr = (Observation *) ListNext(&obsPtr->node);  
     }
     
     
     /* load the list. */
     
     XmListAddItems(tabularLI, xmStr, cnt, 1);
     XmListSelectPos(tabularLI, 1, True);
     
     
     /* cleanup and return. */
     
     for (i = 0; i < cnt; i++)
	  XmStringFree(xmStr[i]);
     XtFree((char *) xmStr);
     
     
     return;  
}

/*************************************************************************/
/* Loads the data retrieved from the database query into
   the tabularLI (tabular data ListBox) */

void	loadTabularListFcst(Forecast *fcstHead)
{  
     Forecast		*fcstPtr;

     XmStringTable	xmStr;
     char	     	validtime_ansi[ANSI_TIME_LEN];
     char	     	prodtime_ansi[ANSI_TIME_LEN];
     char	     	posttime_ansi[ANSI_TIME_LEN];
     char	        buf[BUFSIZ];	
     char	        where[BUFSIZ];	
     struct         	tm *tmptr;
     time_t		validtime, prodtime, posttime;
     int		cnt;
     int		i;
     char		range_check[MAXLEN_QCSYMBOL];
     char		revision;
     float		derived_value=0;
     int		rating_curve_exists=0;
     Riverstat  	*rHead = NULL,	*rPtr;
     char		floodFlow[25], floodStage[25], floodLabel[50];    
     char		river_name[30];
     
     memset(river_name,  '\0', sizeof(river_name)); 
     
     memset(floodFlow,  '\0', sizeof(floodFlow));     
     memset(floodStage, '\0', sizeof(floodStage));     
     memset(floodLabel, '\0', sizeof(floodLabel));     


     /* delete all XmList items.*/
     
     XmListDeleteAllItems(tabularLI);
     
     
     /* create where clause to find records for this lid */
     
     sprintf(where, " WHERE lid='%s' ", current_timeseries.lid);

     strcpy(floodFlow,  " ");
     strcpy(floodStage, " ");
     strcpy(floodLabel, " ");
     
     rHead = GetRiverstat(where);
     if (rHead != NULL)
     {
		rPtr = (Riverstat *) ListFirst(&rHead->list);	
		while (rPtr) 
		{
		   if( ! IsNull ( FLOAT, &rPtr->fs) )
			sprintf(floodStage, "%.1f/", rPtr->fs);
				
		   if( ! IsNull ( FLOAT, &rPtr->fq) )
			sprintf(floodFlow, "%.0f", rPtr->fq);
				
		   if( ! IsNull ( CHAR, &rPtr->stream))
		   {
			sprintf(river_name, "%s", rPtr->stream);
			break;
		   }
         	   else
		   {
			sprintf(river_name, "%s", "UNDEFINED");	
			break;
		   }	
		}
		FreeRiverstat(rHead);
     }
     
     if((strcmp(river_name, "UNDEFINED") == 0) || (!rHead))
     {
    	  strcpy(floodLabel,  " ");
     }
     
     else
     { 
     	  strcpy(floodLabel, "Flood Stg/Flow: ");
     	  strcat(floodLabel, floodStage);
     	  strcat(floodLabel, floodFlow);
     }
     
     XtUnmanageChild(tabularfloodLA);
     SetLabel(tabularfloodLA, floodLabel);
     XtManageChild(tabularfloodLA);


     /* determine which header to display. */
  
     XtUnmanageChild(tabularheaderLA);
     if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
	  (strncmp(current_timeseries.pe, "HT", 2) == 0))
	 && (recordCount("Rating", where) > 1))
     {
	  SetLabel(tabularheaderLA, HDRSTAGE);
	  rating_curve_exists=1;
     }
     else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
	      (recordCount("Rating", where) > 1))
     {
	  SetLabel(tabularheaderLA, HDRFLOW);
	  rating_curve_exists=1;
     }
     else
     {
	  SetLabel(tabularheaderLA, HDRDEFAULT);
	  rating_curve_exists=0;
     }  
   
     XtManageChild(tabularheaderLA);
     
     
     /* if no fcst data, don't allow certain actions */
     
     if (fcstHead == NULL)
     {
        DeSensitize(tabularApplyPB);
        DeSensitize(tabularDeletePB);
        DeSensitize(tabularSetQCPB);
        DeSensitize(tabularMsgPB);
	XmTextSetString(tabularqcdescTE, "");
	  
        xmStr  = (XmStringTable) XtMalloc(sizeof(XmString *));
        xmStr[0] = XmStringCreateSimple("No Data");
        XmListAddItems(tabularLI, xmStr, 1, 1);
        XmStringFree(xmStr[0]);
        XtFree((char *) xmStr);

        Sensitize(tabularUseProductTB);
        Sensitize(tabularUseBasisTB);
        Sensitize(tabularUseTypSrcTB);

        XmToggleButtonSetState(tabularUseProductTB, True, False);
        XmToggleButtonSetState(tabularUseBasisTB,   True, False);
        XmToggleButtonSetState(tabularUseTypSrcTB,  True, False);
	
        return;
     }

     else
     {
	/* get a count of the number of forecasts in the linked list. */
	
	cnt = ListCount(&fcstHead->list);
	
        Sensitize(tabularUseProductTB);
        Sensitize(tabularUseBasisTB);
        Sensitize(tabularUseTypSrcTB);
	
        XmToggleButtonSetState(tabularUseProductTB, True, False);
        XmToggleButtonSetState(tabularUseBasisTB,   False, False);
        XmToggleButtonSetState(tabularUseTypSrcTB,  False, False);
     }

     
     xmStr  = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
     fcstPtr = (Forecast *) ListFirst(&fcstHead->list);


     /* Loop through the list of forecasts. */
     
     for (i = 0; i < cnt; i++)
     {	  
	  yearsec_dt_to_timet(fcstPtr->validtime, &validtime);
	  tmptr = gmtime(&validtime);
	  strftime(validtime_ansi, sizeof(validtime_ansi), "%m/%d %H:%M", tmptr);

	  yearsec_dt_to_timet(fcstPtr->producttime, &prodtime);
	  tmptr = gmtime(&prodtime);
	  strftime(prodtime_ansi, sizeof(prodtime_ansi), "%m/%d %H:%M", tmptr);

	  yearsec_dt_to_timet(fcstPtr->postingtime, &posttime);
	  tmptr = gmtime(&posttime);
	  strftime(posttime_ansi, sizeof(posttime_ansi), "%m/%d %H:%M", tmptr);

	  build_qc_symbol(fcstPtr->quality_code, range_check);

	  if (fcstPtr->revision == 1)
	  	revision = 'T';
	  else
	  	revision = 'F';
	  	  
	  if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
	       (strncmp(current_timeseries.pe, "HT", 2) == 0))
	      && (rating_curve_exists == 1))
	  {
		if (fcstPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = stage2discharge(current_timeseries.lid, 
			                                fcstPtr->value);

		sprintf(buf, "%8.2f %8.0f %11s %c %1s %1s %10s %11s %11s",
			fcstPtr->value, derived_value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	  }
	  else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
		   (rating_curve_exists == 1))
	  {
		if (fcstPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = discharge2stage(current_timeseries.lid,
			                                fcstPtr->value);

		sprintf(buf, "%8.2f %8.2f %11s %c %1s %1s %10s %11s %11s",
			fcstPtr->value, derived_value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	  }
	  else
		sprintf(buf, "%8.2f %11s %c %1s %1s %10s %11s %11s",
			fcstPtr->value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	  
	  xmStr[i] = XmStringCreateSimple(buf);
	  fcstPtr = (Forecast *) ListNext(&fcstPtr->node);  
     }
     
     
     /* load the list.*/
     
     XmListAddItems(tabularLI, xmStr, cnt, 1);
     XmListSelectPos(tabularLI, 1, True);


     /* cleanup and return. */
     
     for (i = 0; i < cnt; i++)
	 XmStringFree(xmStr[i]);
     XtFree((char *) xmStr);
     
     return;  
}


/*************************************************************************/

void	tabular_close(Widget w, XtPointer ptr, XtPointer cbs)
{

     if (tabularDS != NULL)
	XtUnmanageChild(tabularDS);

     if (tabinsertDS != NULL)
	if (XtIsManaged(tabinsertDS))
	   XtUnmanageChild(tabinsertDS);
    
     return;
}


/*************************************************************************/

void 	tabular_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget		  delete_questDS;
     Widget		  ok_button;
     Widget	  	  cancel_button;				
     char		  question[BUFSIZ];
     int		  count = 0;
     int		  *posList;
     
    
     XmListGetSelectedPos(tabularLI, &posList, &count);
     if (count == 1)
     {
	   sprintf(question,"Do you wish to delete this record?"); 
     }
     else if (count > 1)
     {
	   sprintf(question,"Do you wish to delete these %d records?", count);
     }
     else
     	   return;
	   
     
     if (posList)
	  free(posList);
	  
     
     delete_questDS = QuestionDialog(tabularDS, question);			
     SetTitle(delete_questDS,"Delete Confirmation");
     
     ok_button = XmMessageBoxGetChild(delete_questDS, XmDIALOG_OK_BUTTON);	
     XtAddCallback(ok_button, XmNactivateCallback, tabular_delete, delete_questDS);
     
     cancel_button = XmMessageBoxGetChild(delete_questDS, XmDIALOG_CANCEL_BUTTON);
     XtAddCallback(cancel_button, XmNactivateCallback, tabular_question_cancel,
		   delete_questDS);
     
     return;
}


/*************************************************************************/

void	tabular_question_cancel(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	questionDialog = (Widget) ptr;
     
     
     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}


/*************************************************************************/

void	tabular_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	  questionDialog = (Widget) ptr;
     char	  where[BUFSIZ];
     char         command[BUFSIZ];
     char	  tablename[TABLE_NAME_LEN];
     char	  *message;
     
     int	  *poslist = NULL;
     int	  count = 0;
     int	  pos = 0;
     int	  manResult;
     int	  execResult;
     
     long	  result;
     
     ObsStruct		*obsStruct = getObsStruct();
     Observation	*obsHead = obsStruct->obsHead;
     Observation 	*obsPtr;

     FcstStruct		*fcstStruct = getFcstStruct();
     Forecast		*fcstHead = fcstStruct->fcstHead;
     Forecast	 	*fcstPtr;

     RejectedData	rejectedData;
     int		i;
     int		success = 0;
     char		fcst_where[BUFSIZ];
     
     /*  Find the selected position in the list widget and
         go to that position in the linked list. */
     
     XmListGetSelectedPos(tabularLI, &poslist, &count);

     /********** This part is for OBSERVED or PROCCESSED data **********/

     if (current_timeseries.ts[0] == 'R' || current_timeseries.ts[0] == 'P')
     {
	if (obsHead == NULL)
		return;
	/*  Delete all rows that have been selected */
	for (i = 0; i < count; i++)
	{
	  pos = poslist[i];

	  if ( (obsPtr = (Observation *) ListNth(&obsHead->list, pos)) )
	  {
	       createUpdDelWhereObs(where, obsPtr);
	       getTableName(obsPtr->pe, obsPtr->ts, tablename);		       

	       result = DeleteObservation(where, tablename);
	       
	       if (result < 0)  /* error */
	       {
		    message = DbErrorString(result);
		    ErrorDialog(tabularDS, message);
		    if (message)
		    {
			 free(message);	    
			 message = NULL;
		    }
		    
		    break;
	       } 
	       else /* successful delete */
	       {
		    success = 1;

		    /* 	if precip then delete from curprecip table as well  */
		    if ((obsPtr->pe[0] == 'P' ) && (obsPtr->pe[1] != 'A' &&
						    obsPtr->pe[1] != 'D' &&
						    obsPtr->pe[1] != 'E' &&
						    obsPtr->pe[1] != 'L'))
		    {
                       if ( obsPtr->pe[1] == 'P' )
                       {
		          DeleteCurPP(where) ;
                       }
                       else
                       {
                          DeleteCurPC(where) ;
                       }
		    }
		    
		    /* copy the deleted record to RejectedData */
		    
		    setRejectedDataObs(obsPtr, &rejectedData, obsPtr->value);
		    
		    manResult = PutRejectedData(&rejectedData);
		    if (manResult != 0)
		    {
			 message = DbErrorString(manResult);
			 ErrorDialog(GetTopShell(tabularLI), message);
			 if (message)
			 {
			      free(message);
			      message = NULL;
			 }   
		    }
		    
	       } /* successful delete */

	  } /* obsPtr */

	} /* for count */


	if ( success )
	{
	    /* if height or discharge then calculate new RiverStatus as well */
	    
	    if (current_timeseries.pe[0] == 'H' || 
	        current_timeseries.pe[0] == 'Q' )
	    { 
	         sprintf(command, "load_obs_river('%s', '%s', '%s')",
	                 current_timeseries.lid, current_timeseries.pe, 
			 current_timeseries.ts); 

	         execResult = execFunction(command);
	         if (execResult != 0)
	         {   
		      message = DbErrorString(execResult);
		      ErrorDialog(GetTopShell(tabularLI), message);
		      if (message)
		      {
		           free(message);
		           message = NULL;
		      }   
	         }
	    }
	}
	
     } /* typ_src starts with R or P */


     /********** This part is for FORECAST data **********/
     
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
	if (fcstHead == NULL)
		return;
		
		
	/*  Delete all rows that have been selected */
	
	for (i = 0; i < count; i++)
	{
	  pos = poslist[i];

	  if ( (fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos)) )
	  {
	       createUpdDelWhereFcst(fcst_where, fcstPtr);
	       getTableName(fcstPtr->pe, fcstPtr->ts, tablename);
		       
	       result = DeleteForecast(fcst_where, tablename);

	       if (result < 0)  /* error */
	       {
		    message = DbErrorString(result);
		    ErrorDialog(tabularDS, message);
		    if (message)
		    {
			 free(message);	    
			 message = NULL;
		    }
		    
		    break;
	       } 
	       else /* successful delete */
	       {
		    success = 1;

		    /* copy the deleted record to RejectedData */
		    
		    setRejectedDataFcst(fcstPtr, &rejectedData, fcstPtr->value);
		    
		    manResult = PutRejectedData(&rejectedData);
		    if (manResult != 0)
		    {
			 message = DbErrorString(manResult);
			 ErrorDialog(GetTopShell(tabularLI), message);
			 if (message)
			 {
			      free(message);
			      message = NULL;
			 }   
		    }    

	       } /* successful delete */

	  } /* fcstPtr */

	} /* for count */


	if ( success )
	{
	     /* Call Load Max Forecast if delete a stage or discharge */
	     
	     if (current_timeseries.pe[0] == 'H' || 
	         current_timeseries.pe[0] == 'Q')
	     	    load_maxfcst_item(current_timeseries.lid, 
		                      current_timeseries.pe,
				      current_timeseries.ts);
	}

     } /* typ_src starts with F */


     tabular_retrieve_data(w, NULL, NULL);
     
     if (poslist)
	  free(poslist);
     
     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}


/*************************************************************************/

void	tabular_shef_encode(Widget w, XtPointer ptr, XtPointer cbs)
{
     int	*poslist = NULL;
     int	count;
     int	pos;
     int	ctr;
     char	msg[BUFSIZ];
     FILE	*shef_file_ptr = NULL;

     ObsStruct		*obsStruct = getObsStruct();
     Observation	*obsHead = obsStruct->obsHead;
     Observation 	*obsPtr;

     FcstStruct		*fcstStruct = getFcstStruct();
     Forecast		*fcstHead = fcstStruct->fcstHead;
     Forecast		*fcstPtr;

     char	timebuf[BUFSIZ], timebuf2[BUFSIZ];
     struct     tm *tmptr;
     time_t	otime, vtime, basisTime = 0;
     char	fmt_type[4];
     char	txt_value[10];
     ShefDur	*durHead=NULL, *durPtr=NULL;
     char	*dur_symbol;
     char	qualityCodeSymbol[MAXLEN_QCSYMBOL];
     char	data_qualifier = ' ';


     /* open SHEF encode file for appending */
     
     shef_file_ptr = fopen(current_shef_file, "a");
     if (shef_file_ptr == NULL)
     {
	sprintf(msg, "File: %s\nUser does NOT have write permission.",
	        current_shef_file);
	ErrorDialog(tabularDS, msg);
        return;
     }


     /* Get ALL of the SHEF duration Code records and assign the pointer
        of the first one to DurPtr to be passed to the getDurCode function  */
	
     durHead = GetShefDur("");
     if (durHead)
	durPtr = (ShefDur *) ListFirst(&durHead->list);


     /* Find the selected position in the list widget and
        go to that position in the linked list. */
     
     XmListGetSelectedPos(tabularLI, &poslist, &count);

     
     /*	SHEF encode (.A or .AR format) all rows that have been selected */

     for (ctr = 0; ctr < count; ctr++)
     {
	pos = poslist[ctr];	  

	if (obsHead != (Observation *) NULL)
	  if ( (obsPtr = (Observation *) ListNth(&obsHead->list, pos)) )
	  {
	     /* convert INFORMIX time to ASCII GMT */

	     yearsec_dt_to_timet(obsPtr->obstime, &otime);
	     tmptr = gmtime(&otime);
	     strftime(timebuf, sizeof(timebuf), "%Y%m%d Z DH%H%M", tmptr);


	     /* if manually edited data then format as .AR message */

	     if (obsPtr->shef_qual_code[0] == 'M')
	        sprintf(fmt_type, ".AR");
	     else
	        sprintf(fmt_type, ".A ");


	     /* convert '-9999' to missing symbol 'M' */
	     
	     if (obsPtr->value == -9999.)
	        sprintf(txt_value, "M");
	     else if ((obsPtr->pe[0] == 'Q') &&
		      (obsPtr->pe[1] != 'B')  &&
		      (obsPtr->pe[1] != 'E') &&
		      (obsPtr->pe[1] != 'F'))
	        sprintf(txt_value, "%.3f", (obsPtr->value/1000.0));
	     else
	        sprintf(txt_value, "%.2f", obsPtr->value);


	     /* get the internal QC code and set the SHEF data Qualifier */
	     
	     build_qc_symbol(obsPtr->quality_code, qualityCodeSymbol);
	     if (qualityCodeSymbol[0] == 'B')
		data_qualifier = 'B';
	     else if (qualityCodeSymbol[0] == 'Q')
	     {
	     	if (obsPtr->shef_qual_code[0] == 'F')
			data_qualifier = 'F';
		else
			data_qualifier = 'Q';
	     }
	     else
		data_qualifier = ' ';


	     /* get durcode and format the outgoing SHEF message */

	     dur_symbol = getDurCode(durPtr, obsPtr->dur);
	     if (dur_symbol != NULL)
	     {
		fprintf(shef_file_ptr, "%s %s %s/%s%s%s%s %-s%c\n",
			fmt_type, obsPtr->lid, timebuf,
			obsPtr->pe, dur_symbol, obsPtr->ts, obsPtr->extremum,
  			txt_value, data_qualifier);
	     }
	     else
	     {
		fprintf(shef_file_ptr, 
		        ": SHEF Duration letter code NOT be found for %d\n",
			obsPtr->dur);
	     }
	  }	  

	if (fcstHead != (Forecast *) NULL)
	  if ( (fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos)) )
	  {
	     /* convert INFORMIX time to ASCII GMT */

	     yearsec_dt_to_timet(fcstPtr->validtime, &vtime);
	     tmptr = gmtime(&vtime);
	     strftime(timebuf, sizeof(timebuf), "%Y%m%d Z DH%H%M", tmptr);


	     /* if manually edited data then format as .AR message */

	     if (fcstPtr->shef_qual_code[0] == 'M')
	        sprintf(fmt_type, ".AR");
	     else
	        sprintf(fmt_type, ".A ");
		

	     /* convert '-9999' to missing symbol 'M' */

	     if (fcstPtr->value == -9999.)
	        sprintf(txt_value, "M");
	     else if ((fcstPtr->pe[0] == 'Q') &&
		      (fcstPtr->pe[1] != 'B') &&
		      (fcstPtr->pe[1] != 'E') &&
		      (fcstPtr->pe[1] != 'F'))
	        sprintf(txt_value, "%.3f", (fcstPtr->value/1000.0));
	     else
	        sprintf(txt_value, "%.2f", fcstPtr->value);


	     /* get the internal QC code and set the SHEF data Qualifier */

	     build_qc_symbol(fcstPtr->quality_code, qualityCodeSymbol);
	     if (qualityCodeSymbol[0] == 'B')
		data_qualifier = 'B';

	     else if (qualityCodeSymbol[0] == 'Q')
	     {
	     	if (fcstPtr->shef_qual_code[0] == 'F')
			data_qualifier = 'F';
		else
			data_qualifier = 'Q';
	     }
	     else
		data_qualifier = ' ';


	     /* get basis time to set SHEF Creation Date (DC) */
		yearsec_dt_to_timet(fcstPtr->basistime, &basisTime);
		
	     tmptr = gmtime(&basisTime);
	     strftime(timebuf2, sizeof(timebuf2), "%y%m%d%H%M", tmptr);


	     /* get durcode and format the outgoing SHEF message */
	     
	     dur_symbol = getDurCode(durPtr, fcstPtr->dur);
	     if (dur_symbol != NULL)
	     {
		fprintf(shef_file_ptr, "%s %s %s/DC%s/%s%s%s%s %-s%c\n",
			fmt_type, fcstPtr->lid, timebuf, timebuf2,
			fcstPtr->pe, dur_symbol, fcstPtr->ts, fcstPtr->extremum,
  			txt_value, data_qualifier);
	     }
	     else
	     {
		fprintf(shef_file_ptr, 
		        ": SHEF Duration letter code NOT found for duration %d\n",
			fcstPtr->dur);
	     }

	  } /* if obs ptr or fcst ptr exists */

     } /* for loop */


     /* free ShefDur linked list if it exists */

     if (durHead)
     {
	FreeShefDur(durHead);
	durHead = NULL;
     }
     
     
     /* close SHEF encode file */

     fclose(shef_file_ptr);

     if (poslist)
	  free(poslist);


     return;
}


/*************************************************************************/

void 	tabular_send_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	shef_questDS;
     Widget	ok_button;
     Widget	cancel_button;				
     char	question[BUFSIZ];
     char	*buf;
     
     if ( (buf = XmTextGetString(tabularpilTE)) )
     {
        if (IsNull(CHAR, buf) == NOTNULL)
        {
	   strcpy(product_id, buf);
	   if (strcmp(product_id, "CCCCNNNXXX") == 0)
	   {
	      ErrorDialog(tabularDS, "Invalid Product ID: CCCCNNNXXX");
	   }
	   
	   else
	   {
	      sprintf(question,"Do you wish to send product %s?", product_id); 

	      shef_questDS = QuestionDialog(tabularDS, question);			
	      SetTitle(shef_questDS,"SHEF Send Confirmation");
     
	      /* OK button pressed so call function to issue SHEF product */
	      
	      ok_button = XmMessageBoxGetChild(shef_questDS, XmDIALOG_OK_BUTTON);
	      XtAddCallback(ok_button, XmNactivateCallback, tabular_shef_send,
	                    shef_questDS);
     
	      cancel_button = XmMessageBoxGetChild(shef_questDS, XmDIALOG_CANCEL_BUTTON);
	      XtAddCallback(cancel_button, XmNactivateCallback, tabular_question_cancel,
			    shef_questDS);
	   }
	}
	
	else
	   ErrorDialog(tabularDS, "Product ID can not be blank.");
	
	
	XtFree(buf);
     }
     
     return;
}


/*************************************************************************/

void	tabular_shef_send(Widget w, XtPointer ptr, XtPointer cbs)
{
	Widget	questionDialog = (Widget) ptr;
	char	command[240];


	/* call shef_issue script passing in name of
	   SHEF encoded file and product ID */
	   
	sprintf(command, "%s %s %s",
		issue_script, current_shef_file, product_id);
	system(command);
     
     
	/* make sure previous question dialog is destroyed */
	
	if (questionDialog)
	   XtDestroyWidget(questionDialog);
     
     return;
}


/*************************************************************************/

void 	tabular_clear_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
	Widget		  shef_questDS;
	Widget		  ok_button;
	Widget	  	  cancel_button;				
	char		  question[BUFSIZ];
     

	sprintf(question,"Do you wish to remove the SHEF encoded product file?"); 

	shef_questDS = QuestionDialog(tabularDS, question);			
	SetTitle(shef_questDS,"SHEF Clear Confirmation");
     
     
	/* OK button pressed so call function to delete SHEF encode file */
	
	ok_button = XmMessageBoxGetChild(shef_questDS, XmDIALOG_OK_BUTTON);	
	XtAddCallback(ok_button, XmNactivateCallback, 
	              tabular_shef_clear, shef_questDS);
     
	cancel_button = XmMessageBoxGetChild(shef_questDS, 
	                                     XmDIALOG_CANCEL_BUTTON);	
	XtAddCallback(cancel_button, XmNactivateCallback, 
	              tabular_question_cancel, shef_questDS);
     
     return;
}


/*************************************************************************/

void	tabular_shef_clear(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget  questionDialog = (Widget) ptr;
     char    command[240];


     /*	try to remove SHEF encode file weather it exists or not */
     sprintf(command, "rm %s", current_shef_file);
     system(command);
     
     /* make sure previous question dialog is destroyed */
     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}


/*************************************************************************/

void	tabular_edit_shef(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	command[240];
	char	msg[BUFSIZ];
	FILE    *shef_file_ptr = NULL;
	

	/* try to open SHEF encode file for reading */
	
	shef_file_ptr = fopen(current_shef_file, "r");
	if (shef_file_ptr == NULL)
	{
	   sprintf(msg, "Unable to open file:\n%s", current_shef_file);
	   ErrorDialog(tabularDS, msg);
	}
	else
	{
	   /* close SHEF encode file pointer for reading */
	   
	   fclose(shef_file_ptr);
	   
	   
	   /* open SHEF encode file in an editor if it exists */
	   
	   sprintf(command, "%s %s %s %s &",
		   edit_script, "SHEF_encode_Editor", current_shef_file,
		   "SHEFENCODE");
	   system(command);
	}
	 
	return;
}


/*************************************************************************/

void	tabular_edit_issue(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	command[240];
	char	msg[BUFSIZ];
	FILE    *issue_file_ptr = NULL;


	/*  try to open shef_issue script for reading */
	
	issue_file_ptr = fopen(issue_script, "r");
	if (issue_file_ptr == NULL)
	{
	   sprintf(msg, "Unable to open file:\n%s", issue_script);
	   ErrorDialog(tabularDS, msg);
	}
	
	else
	{
	   /* close shef_issue script file pointer for reading */
	   
	   fclose(issue_file_ptr);
	   
	   
	   /* open shef_issue script in an editor if it exists */
	   
	   sprintf(command, "%s %s %s &",
		   edit_script, "SHEF_issue_Editor", issue_script);
	   system(command);
	}
	
	return;
}


/*************************************************************************/

void  TS_data_RowSelection(Widget w, XtPointer ptr, XtPointer cbs)
{       
     int		*poslist = NULL;
     int	  	count;
     int		pos;
     
     
     /* find the selected position in the list widget and
        go to that position in the linked list. */
     
     XmListGetSelectedPos(tabularLI, &poslist, &count);
     
     if (count <= 1)
     {      	  
	  if (count == 1)
	  {
	       Sensitize(tabularDeletePB);
	       
	       Sensitize(tabularSetQCPB);
	       Sensitize(tabularMsgPB);
	       Sensitize(tabularApplyPB);
	       
	       Sensitize(tabularvalLA);
	       Sensitize(tabularvalTE);
	       Sensitize(tabulartimeLA);
	       Sensitize(tabulartimeTE);
	       
	       pos = poslist[0];
	       loadRowIntoWidgets(pos);
	  }
     }
     
     else /* (count > 1) */
     {
	  DeSensitize(tabularApplyPB);
	  
	  DeSensitize(tabularvalLA);
	  DeSensitize(tabularvalTE);
	  DeSensitize(tabulartimeLA);
	  DeSensitize(tabulartimeTE);
     }
     

     if (poslist)
     	free(poslist);
     
     return;		
}

/*************************************************************************/
 
void  loadRowIntoWidgets(int pos)
{
     ObsStruct		*obsStruct = getObsStruct();
     Observation	*obsHead = obsStruct->obsHead;
     Observation	*obsPtr;

     FcstStruct		*fcstStruct = getFcstStruct();
     Forecast		*fcstHead = fcstStruct->fcstHead;
     Forecast		*fcstPtr;

     char		buf[BUFSIZ];	
     char		current_qc_desc[MAXLEN_QCDESCR];
     char		qualityCodeSymbol[MAXLEN_QCSYMBOL];
   
     
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
	if (fcstHead == (Forecast *) NULL)
	{
	  XmTextSetString(tabulartimeTE, InsertProdTime_ANSI);
	  XmTextSetString(tabularvalTE, "-9999.0");
	  XmTextSetString(tabularqcdescTE, "");
	  strcpy(qualityCodeSymbol, "G");
	}
	
	else if ( (fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos)) )
	{
	  yearsec_dt_to_ansi(fcstPtr->validtime, buf);
	  XmTextSetString(tabulartimeTE, (char *)buf);
	  
	  oldValue = fcstPtr->value;
	  sprintf(buf, "%8.2f", fcstPtr->value);
	  XmTextSetString(tabularvalTE, buf);

	  build_qc_descr(fcstPtr->quality_code, current_qc_desc);
	  XmTextSetString(tabularqcdescTE, current_qc_desc);

	  build_qc_symbol(fcstPtr->quality_code, qualityCodeSymbol);
	}
     }
     
     else
     {
	if (obsHead == (Observation *) NULL)
	{
	  XmTextSetString(tabulartimeTE, InsertProdTime_ANSI);
	  XmTextSetString(tabularvalTE, "-9999.0");
	  XmTextSetString(tabularqcdescTE, "");
	  strcpy(qualityCodeSymbol, "G");
	}
	
	else if ( ( obsPtr = (Observation *) ListNth(&obsHead->list, pos) ) )
	{
	  yearsec_dt_to_ansi(obsPtr->obstime, buf);
	  XmTextSetString(tabulartimeTE, (char *)buf);
	  	  
	  oldValue = obsPtr->value;
	  sprintf(buf, "%8.2f", obsPtr->value);
	  XmTextSetString(tabularvalTE, buf);

	  build_qc_descr(obsPtr->quality_code, current_qc_desc);
	  XmTextSetString(tabularqcdescTE, current_qc_desc);

	  build_qc_symbol(obsPtr->quality_code, qualityCodeSymbol);
	}
     }

     if (qualityCodeSymbol[0] == 'B')
	SetMenuHistory(tabularQcOM, "Bad");
     else if (qualityCodeSymbol[0] == 'Q')
	SetMenuHistory(tabularQcOM, "Quest.");
     else
	SetMenuHistory(tabularQcOM, "Good");

 /*
     XmTextSetString(tabularpilTE, product_id);
 */ 
}


/*************************************************************************/

void	tabular_save(Widget w, XtPointer ptr, XtPointer cbs)
{      
   char	  *buf;
   char	  where[BUFSIZ];
   char	  *error_message;       
   char	  tablename[TABLE_NAME_LEN];
     
   ObsStruct	*obsStruct = getObsStruct();
   Observation  *obsHead = obsStruct->obsHead;
   Observation  *obsPtr = NULL;

   FcstStruct	  *fcstStruct = getFcstStruct();
   Forecast	  *fcstHead = fcstStruct->fcstHead;
   Forecast	  *fcstPtr = NULL;

   Observation  obsRow;       
   Forecast	fcstRow;       
   RejectedData rejectedData;

   long	rv;
   char	fcst_where[BUFSIZ];
   int	*poslist = NULL;
   int	count=0, cnt=0;
   int	pos=0;
   time_t     currentTime = 0;
   dtime_t    postingTime;
   int	return_code, within_hour;
   char	qualityCodeSymbol[MAXLEN_QCSYMBOL];

   Widget	widget_returned = NULL;
   
          
   /* find the selected position in the list widget and
      go to that position in the linked list. */
     
   XmListGetSelectedPos(tabularLI, &poslist, &count);
   if (count >= 1 )
      pos = poslist[0];
   else
      return;

   getTableName(current_timeseries.pe, current_timeseries.ts, tablename);


   /* set postingtime to current time */
     
   time(&currentTime);
   timet_to_yearsec_dt(currentTime, &postingTime);
   
     
   /* code to insert or update an observation */
   
   if (current_timeseries.ts[0] == 'R' || current_timeseries.ts[0] == 'P')
   {
       
     /* set the update/add structure with data which doesn't change */
     
     strcpy(obsRow.lid, current_timeseries.lid);
     strcpy(obsRow.pe, current_timeseries.pe);
     obsRow.dur = current_timeseries.dur;
     strcpy(obsRow.ts, current_timeseries.ts);
     strcpy(obsRow.extremum, current_timeseries.extremum);
     obsRow.postingtime = postingTime;

     if (obsHead == NULL)
	cnt = 0;
     else
	cnt = ListCount(&obsHead->list);

     if (cnt > 0)
     {
	/* get the original data from the entry selected */
	
	obsPtr = (Observation *) ListNth(&obsHead->list, pos);
	
	
	/* set the update/add structure with data from the original entry */
	
	obsRow.quality_code = obsPtr->quality_code;
	
	strcpy(obsRow.product_id, obsPtr->product_id);
	obsRow.producttime = obsPtr->producttime;
     }
     
     
     /* if no data in list, set defaults for the product info. */
     
     else
     {
	strcpy(obsRow.product_id, InsertProdID);
	yearsec_ansi_to_dt(InsertProdTime_ANSI, &obsRow.producttime);
     }
     
     
     /* read data from the value and time widgets and replace in structue */
     
     if ( ( buf = XmTextGetString(tabularvalTE) ) )
     {
	obsRow.value = atof(buf);
	XtFree(buf);
     }
     if ( ( buf = XmTextGetString(tabulartimeTE)) )
     {
	return_code = yearsec_ansi_to_dt(buf, &obsRow.obstime);
	XtFree(buf);
	if (return_code < 0)
	{
		ErrorDialog(tabularDS, "Invalid date/time entered.");
		return;
	}
     }
     
   
     /* always set the shef_qual_code with a "M" for Manual edit */
     
     strcpy(obsRow.shef_qual_code, "M");
     
     createUpdDelWhereObs(where, &obsRow); 

     if ( (recordCount(tablename, where) == 0) )
     {   
	  set_qccode(QC_MANUAL_NEW, &obsRow.quality_code);

	  widget_returned = GetMenuHistory(tabularQcPDM);
	  if (widget_returned != NULL)
	  	(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	  else
	  	(void) strcpy(qualityCodeSymbol, "G");

	  if (qualityCodeSymbol[0] == 'B')
		set_qccode(QC_MANUAL_FAILED, &obsRow.quality_code);
	  else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &obsRow.quality_code);
	  else
		set_qccode(QC_MANUAL_PASSED, &obsRow.quality_code);

	  obsRow.revision = 0;
	  
	  if (XmToggleButtonGetState(tabularUseProductTB))
	  {
		strcpy(obsRow.product_id, InsertProdID);
		yearsec_ansi_to_dt(InsertProdTime_ANSI, &obsRow.producttime);
	  }

	  rv = PutObservation(&obsRow, tablename);
	  if ( rv != 0)  /* error */
	  {
	       error_message = DbErrorString(rv);
	       ErrorDialog(tabularDS, error_message); 
	       if (error_message)
	       {
		    free(error_message);
		    error_message = NULL;
	       }
	  }
	  
          else /* check if precip and needs to be sent to GPP server */
          {
             if ( gpp_enable )
             {
                if ((strcmp(obsRow.pe, "PC") == 0) ||
                    (strcmp(obsRow.pe, "PP") == 0 &&
                    (obsRow.dur == SHEFHOUR_IN_MINUTES ||
                     obsRow.dur == SHEFHOUR_IN_HOURS)))	
	        {
                   within_hour = check_precip_window(obsRow.pe, obsRow.obstime);
                   if (within_hour)
	              gpp_hourly_precip(&obsRow);
	        }
           }
        }    
     }
     
     else  /* already a record with same key do an update */
     {
	/* place a copy of this record in the Rejected Data table */
	
	setRejectedDataObs(&obsRow, &rejectedData, oldValue);
	       
	rv = PutRejectedData(&rejectedData);
	if ( rv != 0)
	{
	    error_message = DbErrorString(rv);
		    
	    ErrorDialog(tabularDS, error_message);
	    if (error_message)
	    {
		 free(error_message);
		 error_message = NULL;
	    }
	}

	widget_returned = GetMenuHistory(tabularQcPDM);
	if (widget_returned != NULL)
		(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	else
		(void) strcpy(qualityCodeSymbol, "G");

	if (qualityCodeSymbol[0] == 'B')
		set_qccode(QC_MANUAL_FAILED, &obsRow.quality_code);
	else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &obsRow.quality_code);
	else
		set_qccode(QC_MANUAL_PASSED, &obsRow.quality_code);

	obsRow.revision = 1;

        rv = UpdateObservation(&obsRow, where, tablename);
        if (rv != 0) /* error */
        {
           error_message = DbErrorString(rv);
           ErrorDialog(tabularDS, error_message);
           if (error_message)
           {
              free(error_message);
              error_message = NULL;
           }
        }
     
        else /* check if precip and needs to be sent to GPP server */
        {
           if ( gpp_enable )
           {
              if ((strcmp(obsRow.pe, "PC") == 0) ||
                   (strcmp(obsRow.pe, "PP") == 0 &&
                    (obsRow.dur == SHEFHOUR_IN_MINUTES ||
                     obsRow.dur == SHEFHOUR_IN_HOURS)))	
	      {
                 within_hour = check_precip_window( obsRow.pe, obsRow.obstime );
                 if (within_hour)
	              gpp_hourly_precip(&obsRow);
	      }
           }
        }
     
       } /* if insert or update */
   } /* if observed data */


   /* code to insert or update a forecast ----------------------------- */
   
   if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
   {
     /* set the update/add structure with data which doesn't change.
        although the type-source may be changed... */
     
     strcpy(fcstRow.lid, current_timeseries.lid);
     strcpy(fcstRow.pe,  current_timeseries.pe);
     fcstRow.dur =       current_timeseries.dur;
     strcpy(fcstRow.ts,  current_timeseries.ts);
     strcpy(fcstRow.extremum, current_timeseries.extremum);
     fcstRow.postingtime = postingTime;

     if (fcstHead == NULL)
	cnt = 0;
     else
	cnt = ListCount(&fcstHead->list);

     if (cnt > 0)
     {
	/* get the original data from the entry selected */
	
	fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos);
	
	
	/* set the update/add structure with data from the original entry.
	   note that the basistime may be changed below... */
	
	fcstRow.quality_code = fcstPtr->quality_code;

	fcstRow.probability = fcstPtr->probability;
	fcstRow.basistime   = fcstPtr->basistime;
	
	strcpy(fcstRow.product_id, fcstPtr->product_id);
	fcstRow.producttime = fcstPtr->producttime;
     }
     
     
     /* if no data in list, set defaults for product info 
        and for the type source */
     
     else
     {
	strcpy(fcstRow.product_id, InsertProdID);
	yearsec_ansi_to_dt(InsertProdTime_ANSI,  &fcstRow.producttime);	
	yearsec_ansi_to_dt(InsertBasisTime_ANSI, &fcstRow.basistime);
     }


     /* read data from the value and time widgets and replace in structue */
     
     if ( ( buf = XmTextGetString(tabularvalTE) ) )
     {
	fcstRow.value = atof(buf);
	XtFree(buf);
     }
     if ( ( buf = XmTextGetString(tabulartimeTE) ) )
     {
	return_code = yearsec_ansi_to_dt(buf, &fcstRow.validtime);
	XtFree(buf);
	if (return_code < 0)
	{
		ErrorDialog(tabularDS, "Invalid Date/Time entered.");
		return;
	}
     }
     
     
     /* always set the shef_qual_code with a "M" for Manual edit */
     
     strcpy(fcstRow.shef_qual_code, "M");


     /* use the info for BasisTime and Type Source if button pressed.
        these two fields are part of the data key.
        use a type source if a valid one has been selected. */
     
     if (XmToggleButtonGetState(tabularUseBasisTB))
     {
	yearsec_ansi_to_dt(InsertBasisTime_ANSI, &fcstRow.basistime);
     }
	
     if (XmToggleButtonGetState(tabularUseTypSrcTB))
     {
	if (strcmp(InsertTypeSource, UNDEFINED_TYPESOURCE) != 0)
 	   strcpy(fcstRow.ts, InsertTypeSource);
     }


     /* check if the record exists already */
     
     createUpdDelWhereFcst(fcst_where, &fcstRow); 

     if ( (recordCount(tablename, fcst_where) == 0) )
     {   
	  fcstRow.probability = -1.0;

	  set_qccode(QC_MANUAL_NEW, &fcstRow.quality_code);

	  widget_returned = GetMenuHistory(tabularQcPDM);
	  if (widget_returned != NULL)
	  	(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	  else
	  	(void) strcpy(qualityCodeSymbol, "G");

	  if (qualityCodeSymbol[0] == 'B')
		set_qccode(QC_MANUAL_FAILED, &fcstRow.quality_code);
	  else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &fcstRow.quality_code);
	  else
		set_qccode(QC_MANUAL_PASSED, &fcstRow.quality_code);

	  fcstRow.revision = 0;


	  /* use the info for product ID & Time if button pressed */
	  
	  if (XmToggleButtonGetState(tabularUseProductTB))
	  {
		strcpy(fcstRow.product_id, InsertProdID);
		yearsec_ansi_to_dt(InsertProdTime_ANSI, &fcstRow.producttime);
	  }

	  rv = PutForecast(&fcstRow, tablename);
	  if ( rv != 0)  /* error */
	  {
	       error_message = DbErrorString(rv);
	       ErrorDialog(tabularDS, error_message); 
	       if (error_message)
	       {
		    free(error_message);
		    error_message = NULL;
	       }
	  }   
	  else /* successful add of a new forecast record */
	  {
		tabular_load_timeseries();
          }
     }
     
     else  /* already a record with same key */
     {
	/* place a copy of this record in the Rejected Data table */
	
	setRejectedDataFcst(&fcstRow, &rejectedData, oldValue);
	       
	rv = PutRejectedData(&rejectedData);
	if ( rv != 0)
	{
	    error_message = DbErrorString(rv);
		    
	    ErrorDialog(tabularDS, error_message);
	    if (error_message)
	    {
		 free(error_message);
		 error_message = NULL;
	    }
	}

	widget_returned = GetMenuHistory(tabularQcPDM);
	if (widget_returned != NULL)
		(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	else
		(void) strcpy(qualityCodeSymbol, "G");

	if (qualityCodeSymbol[0] == 'B')
		set_qccode(QC_MANUAL_FAILED, &fcstRow.quality_code);
	else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &fcstRow.quality_code);
	else
		set_qccode(QC_MANUAL_PASSED, &fcstRow.quality_code);

	fcstRow.revision = 1;

	rv = UpdateForecast(&fcstRow, fcst_where, tablename);
	if (rv != 0) /* error */
	{
	       error_message = DbErrorString(rv);
	       ErrorDialog(tabularDS, error_message);
	       if (error_message)
	       {
		    free(error_message);
		    error_message = NULL;
	       }
	}
     } /* if insert or update */
    
    
     /* call Load Max Forecast if update or insert of H or Q PE's */
     
     if (current_timeseries.pe[0] == 'H' || current_timeseries.pe[0] == 'Q')
     	    load_maxfcst_item(current_timeseries.lid, current_timeseries.pe,
			      current_timeseries.ts);
    
   } /* if forecast data */


   tabular_retrieve_data(w, NULL, NULL);
          
   return;
}


/*************************************************************************/
/* sets just the value of the selected row(s) to missing */

void tabular_setmissing(Widget w, XtPointer ptr, XtPointer cbs)
{     
     char	  where[BUFSIZ];
     char	  *error_message;       
     char	  tablename[TABLE_NAME_LEN];
     
     ObsStruct	  *obsStruct = getObsStruct();
     Observation  *obsHead = obsStruct->obsHead;
     Observation  *obsPtr = NULL;

     FcstStruct	  *fcstStruct = getFcstStruct();
     Forecast	  *fcstHead = fcstStruct->fcstHead;
     Forecast	  *fcstPtr = NULL;

     Observation  obsRow;       
     Forecast	  fcstRow;       
     RejectedData rejectedData;

     long	rv;
     int	i;
     int	success = 0;
     char	fcst_where[BUFSIZ];
     int	*poslist = NULL;
     int	count=0, cnt=0;
     int	pos=0;
     time_t     currentTime = 0;
     dtime_t    postingTime;
     float      oldVal = 0;
     int	within_hour = 0;
     
          
     /* find the selected positions in the list widget.*/
     
     XmListGetSelectedPos(tabularLI, &poslist, &count);
     if (count == 0 )
        return;
	
     getTableName(current_timeseries.pe, current_timeseries.ts, tablename);


     /* set postingtime to current time */
     
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
               
	       
     /* code to update an observation to MISSING */
     
     if (current_timeseries.ts[0] == 'R' || current_timeseries.ts[0] == 'P')
     {
       /* set the update structure with data which doesn't change */
       
       strcpy(obsRow.lid, current_timeseries.lid);
       strcpy(obsRow.pe,  current_timeseries.pe);
       obsRow.dur = current_timeseries.dur;
       strcpy(obsRow.ts,  current_timeseries.ts);
       strcpy(obsRow.extremum, current_timeseries.extremum);
       obsRow.postingtime = postingTime;
     
       if (obsHead == NULL)
	  cnt = 0;
       else
	  cnt = ListCount(&obsHead->list);

       if (cnt == 0)
          return;
	  
	
       /* use a loop since there may be more than one row selected */
       
       for (i = 0; i < count; i++)
       {
          pos = poslist[i];
	  
	  /* get the original data from the entry selected */
	  
	  obsPtr = (Observation *) ListNth(&obsHead->list, pos);
	  
	  
	  /* set the update structure with data from the original entry */
	  
	  strcpy(obsRow.product_id, obsPtr->product_id);
	  obsRow.producttime  = obsPtr->producttime;
	  obsRow.obstime      = obsPtr->obstime;
     
     
          /* set value to MISSING */
	  
	  oldVal = obsPtr->value;
	  obsRow.value = MISSING;
     
     
          /* set the shef_qual_code with a "M" for Manual edit */
	  
          strcpy(obsRow.shef_qual_code, "M");
	  
	  obsRow.quality_code = obsPtr->quality_code;
     	  set_qccode(QC_MANUAL_PASSED, &obsRow.quality_code);
	  
	  obsRow.revision = 1;
	  

          /* already a record with same key, do an update */
	  
          createUpdDelWhereObs(where, &obsRow); 
	  rv = UpdateObservation(&obsRow, where, tablename);
	  
         if (rv != 0) /* error */
         {
            error_message = DbErrorString(rv);
            ErrorDialog(tabularDS, error_message);
            if (error_message)
            {
              free(error_message);
              error_message = NULL;
            }
         }
	 
         else /* successful update of observation */
         {
           success = 1;
	       
           setRejectedDataObs(&obsRow, &rejectedData, oldVal);
	       
           rv = PutRejectedData(&rejectedData);
           if ( rv != 0)
           {
              error_message = DbErrorString(rv);

              ErrorDialog(tabularDS, error_message);
              if (error_message)
              {
                 free(error_message);
                 error_message = NULL;
              }
	   }

           /* check if precip and needs to be sent to GPP server */
	
           if ( gpp_enable )
           {
              if ((strcmp(obsRow.pe, "PC") == 0) ||
                  (strcmp(obsRow.pe, "PP") == 0 &&
                  (obsRow.dur == SHEFHOUR_IN_MINUTES ||
                   obsRow.dur == SHEFHOUR_IN_HOURS)))	
	      {
                 within_hour = check_precip_window( obsRow.pe, obsRow.obstime );
                 if (within_hour)
	            gpp_hourly_precip(&obsRow);
	      }
            }
        
          } /* successful update of an observation */
        } /* for count */
     } /* if observed data */


     /* code to update a forecast */
     
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
       /* set the update structure with data which doesn't change */
       
       strcpy(fcstRow.lid, current_timeseries.lid);
       strcpy(fcstRow.pe, current_timeseries.pe);
       fcstRow.dur = current_timeseries.dur;
       strcpy(fcstRow.ts, current_timeseries.ts);
       strcpy(fcstRow.extremum, current_timeseries.extremum);
       fcstRow.postingtime = postingTime;
       
       if (fcstHead == NULL)
	  cnt = 0;
       else
	  cnt = ListCount(&fcstHead->list);

       if (cnt == 0)
	  return;
	  
	  
       /* use a loop since there may be more than one row selected */
       
       for (i = 0; i < count; i++)
       {
          pos = poslist[i];
    
          /* get the original data from the entry selected */
	  
	  fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos);
	  
	
	  /* set the update structure with data from the original entry */
	  
	  fcstRow.probability  = fcstPtr->probability;
	  fcstRow.basistime    = fcstPtr->basistime;
	  strcpy(fcstRow.product_id, fcstPtr->product_id);
	  fcstRow.producttime  = fcstPtr->producttime;
	  fcstRow.validtime    = fcstPtr->validtime;
	  
	  
	  /* set value to MISSING */

	  oldVal = fcstPtr->value;
          fcstRow.value = MISSING;


          /* always set the shef_qual_code with a "M" for Manual edit */

          strcpy(fcstRow.shef_qual_code, "M");

	  fcstRow.quality_code = fcstPtr->quality_code;
          set_qccode(QC_MANUAL_PASSED, &fcstRow.quality_code);
	  
	  fcstRow.revision = 1;
	  
	  
	  /* always do an update */
	  
          createUpdDelWhereFcst(fcst_where, &fcstRow); 

	  rv = UpdateForecast(&fcstRow, fcst_where, tablename);
	  if (rv != 0) /* error */
	  {
	       error_message = DbErrorString(rv);
	       ErrorDialog(tabularDS, error_message);
	       if (error_message)
	       {
		    free(error_message);
		    error_message = NULL;
	       }
	  }
	  
	  else /* successful update of forecast record */
	  {
               success = 1;
	       
	       setRejectedDataFcst(&fcstRow, &rejectedData, oldVal);
	       
	       rv = PutRejectedData(&rejectedData);
	       if ( rv != 0)
	       {
		    error_message = DbErrorString(rv);
		    
		    ErrorDialog(tabularDS, error_message);
		    if (error_message)
		    {
			 free(error_message);
			 error_message = NULL;
		    }
	       }
	  } /* successful update */
       } /* for count */
    
    
       /* call Load Max Forecast if update of H or Q PE's */
     
       if (current_timeseries.pe[0] == 'H' || current_timeseries.pe[0] == 'Q')
     	    load_maxfcst_item(current_timeseries.lid, current_timeseries.pe,
			      current_timeseries.ts);
    
     } /* if forecast data */


     tabular_retrieve_data(w, NULL, NULL);
     
     if (poslist)
     {
          free(poslist);
	  poslist = NULL;
     }
	  
     return;
}

/*************************************************************************/
/* sets just the QC fields for the selected row(s) */

void tabular_setqc(Widget w, XtPointer ptr, XtPointer cbs)
{     
     char	  where[BUFSIZ];
     char	  *error_message;       
     char	  tablename[TABLE_NAME_LEN];
     
     ObsStruct	  *obsStruct = getObsStruct();
     Observation  *obsHead = obsStruct->obsHead;
     Observation  *obsPtr = NULL;

     FcstStruct	  *fcstStruct = getFcstStruct();
     Forecast	  *fcstHead = fcstStruct->fcstHead;
     Forecast	  *fcstPtr = NULL;

     Observation  obsRow;       
     Forecast	  fcstRow;       

     long	rv;
     int	i;
     int	success = 0;
     char	fcst_where[BUFSIZ];
     int	*poslist = NULL;
     int	count=0, cnt=0;
     int	pos=0;
     time_t     currentTime = 0;
     dtime_t    postingTime;
     Widget     widget_returned = NULL;  
     char	qualityCodeSymbol[MAXLEN_QCSYMBOL];
        
          
     /* Find the selected positions in the list widget.*/
     
     XmListGetSelectedPos(tabularLI, &poslist, &count);
     if (count == 0 )
        return;
	
     getTableName(current_timeseries.pe, current_timeseries.ts, tablename);


     /* set postingtime to current time */
     
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
               
	       
     /* code to update an observation qc info */
     
     if (current_timeseries.ts[0] == 'R' || current_timeseries.ts[0] == 'P')
     {
       /* set the update structure with data which doesn't change */
       
       strcpy(obsRow.lid, current_timeseries.lid);
       strcpy(obsRow.pe,  current_timeseries.pe);
       obsRow.dur = current_timeseries.dur;
       strcpy(obsRow.ts,  current_timeseries.ts);
       strcpy(obsRow.extremum, current_timeseries.extremum);
       obsRow.postingtime = postingTime;    
     
       if (obsHead == NULL)
	  cnt = 0;
       else
	  cnt = ListCount(&obsHead->list);

       if (cnt == 0)
          return;
	  
	
       /* use a loop since there may be more than one row selected */
       
       for (i = 0; i < count; i++)
       {
          pos = poslist[i];
	  
	  
	  /* get the original data from the entry selected */
	  
	  obsPtr = (Observation *) ListNth(&obsHead->list, pos);
	  
	  
	  /* set the update structure with data from the original entry */
	  
	  strcpy(obsRow.product_id, obsPtr->product_id);
	  obsRow.producttime      = obsPtr->producttime;
	  obsRow.value            = obsPtr->value;
	  obsRow.obstime          = obsPtr->obstime;
     
     
          /* set the shef_qual_code with a "M" for Manual edit */
	  
          strcpy(obsRow.shef_qual_code, "M");
	  obsRow.revision = 1;
	  
	  obsRow.quality_code = obsPtr->quality_code;	  
	  
	  widget_returned = GetMenuHistory(tabularQcPDM);
	  if (widget_returned != NULL)
		(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	  else
		(void) strcpy(qualityCodeSymbol, "G");

	  if (qualityCodeSymbol[0]      == 'B')
		set_qccode(QC_MANUAL_FAILED, &obsRow.quality_code);
	  else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &obsRow.quality_code);
	  else
		set_qccode(QC_MANUAL_PASSED, &obsRow.quality_code);
	  
	  
	  /* do the update */

          createUpdDelWhereObs(where, &obsRow); 
	  
	  rv = UpdateObservation(&obsRow, where, tablename);
          if (rv != 0) /* error */
          {
             error_message = DbErrorString(rv);
             ErrorDialog(tabularDS, error_message);
             if (error_message)
             {
                free(error_message);
                error_message = NULL;
             }
          }
	  
          else /* successful update of observation */
          {
             success = 1;	               
          } 
       }  /* for count */
     }    /* if observed data */
   

     /* code to update a forecast */
     
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
       /* set the update structure with data which doesn't change */
       
       strcpy(fcstRow.lid, current_timeseries.lid);
       strcpy(fcstRow.pe,  current_timeseries.pe);
       fcstRow.dur = current_timeseries.dur;
       strcpy(fcstRow.ts,  current_timeseries.ts);
       strcpy(fcstRow.extremum, current_timeseries.extremum);
       fcstRow.postingtime = postingTime;
       
       if (fcstHead == NULL)
	  cnt = 0;
       else
          cnt = ListCount(&fcstHead->list);

       if (cnt == 0)
	  return;
	  
	  
       /* use a loop since there may be more than one row selected */
       
       for (i = 0; i < count; i++)
       {
          pos = poslist[i];
    
          /* get the original data from the entry selected */
	  
	  fcstPtr = (Forecast *) ListNth(&fcstHead->list, pos);
	  
	
	  /* set the update structure with data from the original entry */
	  
	  fcstRow.probability = fcstPtr->probability;
	  fcstRow.basistime   = fcstPtr->basistime;
	  strcpy(fcstRow.product_id, fcstPtr->product_id);
	  fcstRow.producttime = fcstPtr->producttime;
	  fcstRow.value     = fcstPtr->value;
	  fcstRow.validtime = fcstPtr->validtime;


          /* always set the shef_qual_code with a "M" for Manual edit */

          strcpy(fcstRow.shef_qual_code, "M");
	  fcstRow.revision = 1;

	  fcstRow.quality_code = fcstPtr->quality_code;
	  
	  widget_returned = GetMenuHistory(tabularQcPDM);
	  if (widget_returned != NULL)
		(void) strncpy(qualityCodeSymbol, GetLabel(widget_returned), 1);
	  else
		(void) strcpy(qualityCodeSymbol, "G");

	  if (qualityCodeSymbol[0] == 'B')
		set_qccode(QC_MANUAL_FAILED, &fcstRow.quality_code);
	  else if (qualityCodeSymbol[0] == 'Q')
		set_qccode(QC_MANUAL_QUEST,  &fcstRow.quality_code);
	  else
		set_qccode(QC_MANUAL_PASSED, &fcstRow.quality_code);


          /* do the update */
	  
          createUpdDelWhereFcst(fcst_where, &fcstRow); 
	  
	  rv = UpdateForecast(&fcstRow, fcst_where, tablename);
	  if (rv != 0) /* error */
	  {
	     error_message = DbErrorString(rv);
	     ErrorDialog(tabularDS, error_message);
	     if (error_message)
	     {
		 free(error_message);
		 error_message = NULL;
	     }
	  }
	  
	  else /* successful update of forecast record */
	  {
             success = 1;	       
	  } 
	  
       } /* for count */
    
   
     } /* if forecast data */


     /* reload the data list */
     
     tabular_retrieve_data(w, NULL, NULL);
     
     
     /* free memory */
     
     if (poslist)
     {
          free(poslist);
	  poslist = NULL;
     }
	  
     return;
}


/*************************************************************************/
/* copy the current forecast time series in its entirety to a new
   type source and/or basis time */
   
void tabular_copy_ts(Widget w, XtPointer ptr, XtPointer cbs)
{      
   FcstStruct	  *fcstStruct = getFcstStruct();
   Forecast	  *fcstHead = fcstStruct->fcstHead;
   Forecast	  *fcstPtr = NULL;
   Forecast	  fcstRow; 
   int		  insert_cnt = 0, duplicate_cnt = 0;
   int 	          cnt = 0;
   time_t         currentTime = 0;
   dtime_t        postingTime;
   char	          fcst_where[BUFSIZ];
   char	          tablename[TABLE_NAME_LEN];
   int            rv;
   char	          *error_message;       
   int		  key_changed;
            

   /* get the count of the records in the list */

   if (fcstHead == NULL)
      cnt = 0;
   else
      cnt = ListCount(&fcstHead->list);
      
   /* check the the type source or basis time is being changed, otherwise
      it has nothing to copy the forecast to */

   if (XmToggleButtonGetState(tabularUseBasisTB) ||
       XmToggleButtonGetState(tabularUseTypSrcTB))
       key_changed = 1;
   else
   {
       key_changed = 0;
       printf("Copy request ignored - specify use of basistime or typesource.\n");
   }
       
       
   /* only do something if we have forecast or contingency data and
      a valid type source has been selected.  */

   if ((current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C') &&
       (cnt > 0) && (key_changed) && 
       (strcmp(InsertTypeSource, UNDEFINED_TYPESOURCE) != 0) )
   {
     /* set postingtime to current time */
     
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
     
     
     /* load the structure with the general data */
     
     strcpy(fcstRow.lid, current_timeseries.lid);
     strcpy(fcstRow.pe,  current_timeseries.pe);
     fcstRow.dur = current_timeseries.dur;
     strcpy(fcstRow.extremum, current_timeseries.extremum);
     fcstRow.postingtime = postingTime;
     
     
     /* load the type source key field specific to the copy request. */
     
     if (XmToggleButtonGetState(tabularUseTypSrcTB))
        strcpy(fcstRow.ts, InsertTypeSource);
     else
        strcpy(fcstRow.ts, current_timeseries.ts);
	        
     
     /* set up the first record for use in defining some fields
        outside the loop. this must be done before loading
	the basistime, productid, and productime below.  */
     
     fcstPtr = (Forecast *) ListFirst(&fcstHead->list);	
     
     
     /* load the basistime key field specific to the copy request */

     if (XmToggleButtonGetState(tabularUseBasisTB))
        yearsec_ansi_to_dt(InsertBasisTime_ANSI, &fcstRow.basistime);
     else
        fcstRow.basistime = fcstPtr->basistime;
      
      
     /* load the product id and time data fields accordingly */
          
     if (XmToggleButtonGetState(tabularUseProductTB))
     {
	strcpy(fcstRow.product_id, InsertProdID);
	yearsec_ansi_to_dt(InsertProdTime_ANSI, &fcstRow.producttime);
     }
     else
     {
        strcpy(fcstRow.product_id, fcstPtr->product_id);
        fcstRow.producttime = fcstPtr->producttime;
     }
      
      
     /* define the table name to insert data into */
      
     getTableName(fcstRow.pe, fcstRow.ts, tablename);


     /* loop on each of the entries in the list and copy them. */

     while (fcstPtr)
     {	
	/* load the data specific to the record */
	
	fcstRow.probability = fcstPtr->probability;
	fcstRow.value       = fcstPtr->value;
	fcstRow.validtime   = fcstPtr->validtime;
        strcpy(fcstRow.shef_qual_code, "M");
        set_qccode(QC_MANUAL_NEW, &fcstRow.quality_code);
	fcstRow.revision = 0;
	
	
	/* build the where clause */
	
        createUpdDelWhereFcst(fcst_where, &fcstRow); 

        if ((recordCount(tablename, fcst_where) == 0))
        {   
	   rv = PutForecast(&fcstRow, tablename);
	   if ( rv != 0)  /* error */
	   {
	       error_message = DbErrorString(rv);
	       ErrorDialog(tabularDS, error_message); 
	       if (error_message)
	       {
		    free(error_message);
		    error_message = NULL;
	       }
	   }   
	   else /* successful add of a new forecast record */
	   {
	       insert_cnt++;
           }
        }
     
        else  /* already a record with same key */
        {
           duplicate_cnt++;
        } 
    
	/* get the next record */
	
	fcstPtr = (Forecast *) ListNext(&fcstPtr->node);  
     }


     /* report on any duplicates which are ignored */
     
     if (insert_cnt > 0)
     {
        printf("%d duplicate records detected in copy operation.\n", 
	       duplicate_cnt);
     }
     
     
     /* if at least one value inserted,  */

     if (insert_cnt > 0)
     {
        printf("Inserted %d new records in copy operation.\n", insert_cnt);
	
	
	/* reload list of timeseries */
	
        tabular_load_timeseries();
      

        /* update the timeseries data itself */
   
        tabular_retrieve_data(w, NULL, NULL);
	

        /* reload the max forecast info if height or discharge data 
           since there is a new typesource or new basis time */
     
        if (current_timeseries.pe[0] == 'H' || current_timeseries.pe[0] == 'Q')
     	      load_maxfcst_item(current_timeseries.lid, 
	                        current_timeseries.pe,
			        fcstRow.ts);
      }
   } 

     
   return;
}


/*************************************************************************/

char *  getDurCode(ShefDur *durPtr, int dur)
{			
	/* loop through linked list of SHEF duration codes and if the dur
	   number passed in matches one in the table then return code else
	   move on down the list. */
	   
	while(durPtr)
	{
		if (durPtr->dur == dur)
		   return(durPtr->durcode);
		else
                   durPtr = (ShefDur *) ListNext(&durPtr->node);
	}

	/* if NO match found then return a NULL pointer */
	
	return(NULL);
}


/*************************************************************************/
/* load the list of time series */
   
void	tabular_load_timeseries()
{

	int      ctr1, ctr2, count, entry_number=0, highlight_pos=0;
        RussText temp_buf;
	char	 tablename[BUFSIZ];
	char     fcst_where[BUFSIZ];
	
	char     buf[100];
	char 	 lid_buf[LOC_ID_LEN + 1];
	
  	char 	 stn_name[LOC_NAME_LEN + 1];
	char 	 river_name[STREAM_NAME_LEN + 1];
	
	char     stn_buf[SHOW_LOC_LEN + 1];
	char     stream_buf[SHOW_RIVER_LEN + 1];
		
	char	 *location_id; 
	char     *physical_element;
	char	 *duration;
	char     *type_source;
	char     *extremum;
	
	UniqueList *ulHead=NULL, *ulPtr=NULL;
        
	
	
	/* loop on the unique time series defined from the parent info */
	
	for (ctr1 = 0; ctr1 < current_tab_info.nitems; ctr1++)
	{
	   if ((ctr1 + 1) == current_tab_info.selected_pos)
	      highlight_pos = entry_number + 1;

	   /* parse out variables from timeseries passed in */
	   
	   strcpy(temp_buf, current_tab_info.buf[ctr1]);   
	   location_id      = strtok(temp_buf, " ");	
	   physical_element = strtok(NULL, " ");
	   duration         = strtok(NULL, " ");
	   type_source      = strtok(NULL, " ");
	   extremum         = strtok(NULL, " ");

           sprintf(lid_buf, "%s", location_id);
	   memset(buf, '\0', sizeof(buf));
	   	
	
	   /* append the river name info */
	   
	   getStnRiverName(lid_buf, stn_name, river_name);
		   
	   memset(stn_buf,'\0',sizeof(stn_buf));
	   strncat(stn_buf, stn_name, SHOW_LOC_LEN);  
	   strcat(buf, stn_buf);	
           if(strcmp(river_name,"UNDEFINED") == 0)
	   {
		sprintf(buf, " (%s)", stn_buf); 
	   }
	   else
	   {
		memset(stream_buf,'\0',sizeof(stream_buf)); 
		strncat(stream_buf, river_name, SHOW_RIVER_LEN);
		sprintf(buf, " (%s - %s)", stn_buf, stream_buf);
	   }

	    
	   /* if a forecast timeseries then find all basis times */
	   
	   if (type_source[0] == 'F' || type_source[0] == 'C')
	   {
	      sprintf(fcst_where, " WHERE lid='%s' "
     				 	" AND pe='%s'"
	                                " AND dur='%s'"
	                                " AND ts='%s'"
	                                " AND extremum='%s'"
	                                " AND validtime >= '%s'"
	                                " AND validtime <= '%s'"
	                                " ORDER BY basistime DESC ",
				location_id,
				physical_element, duration, type_source,
				extremum, Begintime_ANSI, Endtime_ANSI);
                                    
	       getTableName(physical_element, type_source, tablename);
	   
	       ulHead = LoadUnique("basistime", tablename, fcst_where, &count);
	       if (ulHead != NULL)
	       {
		   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
		   
		   /* loop though number of unique basis times */
		   /* if list ALL basis TB is not pressed then loop only ONCE */
		
		   if (! XmToggleButtonGetState(tabularlistallTB))
		      count = 1;
		      
		   for (ctr2 = 0; ctr2 < count; ctr2++)
		   {
                       if (entry_number < MAX_TS_ON_LIST)
                       {
			   sprintf(modified_ts_list[entry_number],
			           "%-5s %2s %4s %2s %s ",
				   location_id,
	  			   physical_element,
				   duration,
				   type_source,
				   extremum);
			   strncat(modified_ts_list[entry_number], 
			           ulPtr->uchar, 19);
			   entry_number++;
			   ulPtr = (UniqueList *) ListNext(&ulPtr->node);
                       }
		   }
		   FreeUnique(ulHead);
		   ulHead = NULL;
	       }
	       
	       else /* if NO basis times found */
	       {
                    if (entry_number < MAX_TS_ON_LIST)
                    {
		  	sprintf(modified_ts_list[entry_number],
			        "%-5s %2s %4s %2s %s ",
  					location_id,
	  				physical_element,
	  				duration,
	  				type_source,
	  				extremum);
			strcat(modified_ts_list[entry_number], "No Data");
			entry_number++; 
                    }         
	       }
	   }
	    
	   else /* if an observed timeseries then just store in modified list */
	   {
               if (entry_number < MAX_TS_ON_LIST)
               {
  			sprintf(modified_ts_list[entry_number],
			        "%-5s %2s %4s %2s %s ",
	  				location_id,
	  				physical_element,
	  				duration,
	  				type_source,
	  				extremum);
			entry_number++;
               }
	   }

	} /* ctr1 for loop, number of pedtseps */
	
		
	/* load the list of modified times series and select the first one */
	
	loadXmList100(tabulartsLI, modified_ts_list, entry_number);
	XmListSelectPos(tabulartsLI, highlight_pos, True);
	XmListSetPos(tabulartsLI, highlight_pos);
	
	
       
	return;   
}
   
   
/*************************************************************************/

void TS_type_RowSelection(Widget w, XtPointer ptr, XtPointer cbs)
{
	int		*poslist = NULL;
	int	  	count;
	int		entry;
         
	RussText	temp_buf;
	char	*location_id; 
	char	*physical_element; 
	char	*duration;
	char	*type_source;
	char	*extremum;
	char	*basistime_date;
	char	*basistime_time;
        char	dur_buf[5];
	char    cur_ts_label[50];
	char    cur_nm_label[80];
	char    buf[100];
	char    lid_buf[LOC_ID_LEN + 1];
	
	char     stn_buf[SHOW_LOC_LEN + 1];
	char     stream_buf[SHOW_RIVER_LEN + 1];
	
  	char 	stn_name[LOC_NAME_LEN + 1];
	char 	river_name[STREAM_NAME_LEN + 1];
	

	
	/* find the selected position in the list widget and
	   go to that position in the modified times series array. */
	
	XmListGetSelectedPos(tabulartsLI, &poslist, &count);
	if (count == 1)
	{
	    current_tab_info.selected_pos = poslist[0];
	    
	    entry = poslist[0] - 1;


	    /* parse out variables from modified_ts_list */
	    
	    strcpy(temp_buf, modified_ts_list[entry]);
	    location_id      = strtok(temp_buf, " ");
	    physical_element = strtok(NULL, " ");
	    duration         = strtok(NULL, " ");
	    type_source      = strtok(NULL, " ");
	    extremum         = strtok(NULL, " ");
	    basistime_date   = strtok(NULL, " ");
	    basistime_time   = strtok(NULL, " ");     
	    
	    
	    /* save the current lid selected */
	    
	    memset(current_timeseries.lid, '\0', sizeof(current_timeseries.lid));
	    strcpy(current_timeseries.lid, location_id);
	    
	    sprintf(lid_buf, "%s", current_timeseries.lid);
	    memset(buf, '\0', sizeof(buf));
	    
	    
            /* get Location name and River  */

	    getStnRiverName(lid_buf, stn_name, river_name);
		    
	    
	    memset(stn_buf,'\0',sizeof(stn_buf));
	    strncat(stn_buf, stn_name, SHOW_LOC_LEN);  
	    strcat(buf, stn_buf);	
	    if(strcmp(river_name,"UNDEFINED") == 0)
	    {
			sprintf(buf, " (%s)", stn_buf); 
	    }
	    else
	    {
			memset(stream_buf,'\0',sizeof(stream_buf)); 
			strncat(stream_buf, river_name, SHOW_RIVER_LEN);
			sprintf(buf, " (%s - %s)", stn_buf, stream_buf);
	    }
	    
	    
	    /* save the current pe selected */
	    
	    memset(current_timeseries.pe, '\0', sizeof(current_timeseries.pe));
	    strcpy(current_timeseries.pe, physical_element);


	    /* save the current dur selected */
	    
	    memset(dur_buf, '\0', sizeof(dur_buf));
	    strcpy(dur_buf, duration);
	    current_timeseries.dur = atoi(dur_buf);


	    /* save the current ts selected */
	    
	    memset(current_timeseries.ts, '\0', sizeof(current_timeseries.ts));
	    strcpy(current_timeseries.ts, type_source);


	    /* save the current ex selected */

	    memset(current_timeseries.extremum, '\0', 
	           sizeof(current_timeseries.extremum));
	    strcpy(current_timeseries.extremum, extremum);


	    /* save the basistime to be returned */

	    memset(current_timeseries.basistime, '\0', 
	           sizeof(current_timeseries.basistime));
	    
	    if (basistime_date != NULL)
	    	strcpy(current_timeseries.basistime, basistime_date);
	    strcat(current_timeseries.basistime, " ");
	   
	    if (basistime_time != NULL)
		strcat(current_timeseries.basistime, basistime_time);
	    sprintf(cur_nm_label,"%s %s", current_timeseries.lid,
				buf); 
	    sprintf(cur_ts_label, "%s %s %s %s %s",
				current_timeseries.pe,
				conv_dur2text(current_timeseries.dur),
				current_timeseries.ts,
				current_timeseries.extremum,
				current_timeseries.basistime);
	    		
    	    XtUnmanageChild(tabularnamestreamLA);
	    SetLabel(tabularnamestreamLA, cur_nm_label);
	    XtManageChild(tabularnamestreamLA);	    
	    
	    XtUnmanageChild(tabular_pedtseLA);
	    SetLabel(tabular_pedtseLA, cur_ts_label);
	    XtManageChild(tabular_pedtseLA);

	    tabular_retrieve_data(w, NULL, NULL);
	     
	}
     
	if (poslist)
	  free(poslist);
     
	return;		
}

/*************************************************************************/

int	create_table_file(void)
{
     char	msg[BUFSIZ];
     FILE	*print_file_ptr = NULL;
     char	obstime_ansi[ANSI_TIME_LEN],
     		validtime_ansi[ANSI_TIME_LEN],
     		prodtime_ansi[ANSI_TIME_LEN],
     		posttime_ansi[ANSI_TIME_LEN];
     struct     tm *tmptr;
     time_t	obstime, validtime, prodtime, posttime;
     char	range_check[MAXLEN_QCSYMBOL];
     char       revision;
     float      derived_value=0;
     int	rating_curve_exists=0;
     char       where[BUFSIZ];

     ObsStruct		*obsStruct = getObsStruct();
     Observation	*obsHead = obsStruct->obsHead;
     Observation 	*obsPtr;

     FcstStruct		*fcstStruct = getFcstStruct();
     Forecast		*fcstHead = fcstStruct->fcstHead;
     Forecast		*fcstPtr;


     /* open print file for writing */

     print_file_ptr = fopen(temp_print_file, "w");
     if (print_file_ptr == NULL)
     {
	sprintf(msg, "File: %s\nUser does NOT have write permission.",
	        temp_print_file);
	ErrorDialog(tabularDS, msg);
        return(0);
     }

     fprintf(print_file_ptr, "Beginning Time(Z):   %s\nEnding Time(Z):     %s\n",
		  Begintime_ANSI, Endtime_ANSI);
     
     fprintf(print_file_ptr,   "Station Identifier: %s\n"
				"Physical Element:   %s\n"
				"Duration:           %s\n"
				"SHEF Type Source:   %s\n"
				"SHEF Extremum:      %s\n",
				current_timeseries.lid,
				current_timeseries.pe,
				conv_dur2text(current_timeseries.dur),
				current_timeseries.ts,
				current_timeseries.extremum);
     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
	  fprintf(print_file_ptr, "Basis Time(Z):      %s\n",
		  current_timeseries.basistime);

     sprintf(where, " WHERE lid='%s' ", current_timeseries.lid);

     if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
          (strncmp(current_timeseries.pe, "HT", 2) == 0))
         && (recordCount("Rating", where) > 1))
     {
	  fprintf(print_file_ptr, "\n         Derived              R S Q\n");
	  fprintf(print_file_ptr, "  Value    Flow     Time(Z)   V Q C  Product      Time       Posted\n");
	  fprintf(print_file_ptr, "-------- -------- ----------- - - - ---------- ----------- -----------\n");
	  rating_curve_exists=1;
     }
     else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
              (recordCount("Rating", where) > 1))
     {
	  fprintf(print_file_ptr, "\n         Derived              R S Q\n");
	  fprintf(print_file_ptr, "  Value   Stage     Time(Z)   V Q C  Product      Time       Posted\n");
	  fprintf(print_file_ptr, "-------- -------- ----------- - - - ---------- ----------- -----------\n");
	  rating_curve_exists=1;
     }
     else
     {
	  fprintf(print_file_ptr, "\n                     R S Q\n");
	  fprintf(print_file_ptr, "  Value    Time(Z)   V Q C  Product      Time       Posted\n");
	  fprintf(print_file_ptr, "-------- ----------- - - - ---------- ----------- -----------\n");
	  rating_curve_exists=0;
     }  

     if (current_timeseries.ts[0] == 'F' || current_timeseries.ts[0] == 'C')
     {
       if ( fcstHead != (Forecast *) NULL )
       {
	if ( ( fcstPtr = (Forecast *) ListFirst(&fcstHead->list) ) )
	{
	  while(fcstPtr)
	  {
	     /* convert INFORMIX time to ASCII GMT */
	     yearsec_dt_to_timet(fcstPtr->validtime, &validtime);
	     tmptr = gmtime(&validtime);
	     strftime(validtime_ansi, sizeof(validtime_ansi), "%m/%d %H:%M", 
	              tmptr);

	     yearsec_dt_to_timet(fcstPtr->producttime, &prodtime);
	     tmptr = gmtime(&prodtime);
	     strftime(prodtime_ansi, sizeof(prodtime_ansi), "%m/%d %H:%M",
	              tmptr);

	     yearsec_dt_to_timet(fcstPtr->postingtime, &posttime);
	     tmptr = gmtime(&posttime);
	     strftime(posttime_ansi, sizeof(posttime_ansi), "%m/%d %H:%M",
	              tmptr);

	     build_qc_symbol(fcstPtr->quality_code, range_check);

	     if (fcstPtr->revision == 1)
                revision = 'T';
	     else
                revision = 'F';

	     if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
		  (strncmp(current_timeseries.pe, "HT", 2) == 0))
		 && (rating_curve_exists == 1))
	     {
		if (fcstPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = stage2discharge(current_timeseries.lid, 
			                                fcstPtr->value);

		fprintf(print_file_ptr, 
		        "%8.2f %8.0f %11s %c %1s %1s %10s %11s %11s\n",
			fcstPtr->value, derived_value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	     }
	     else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
		      (rating_curve_exists == 1))
	     {
		if (fcstPtr->value == -9999.0)
			derived_value = -9999.0;
		else
	                derived_value = discharge2stage(current_timeseries.lid, 
			                                fcstPtr->value);

		fprintf(print_file_ptr, 
		        "%8.2f %8.2f %11s %c %1s %1s %10s %11s %11s\n",
			fcstPtr->value, derived_value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	     }
	     else
		fprintf(print_file_ptr, 
		        "%8.2f %11s %c %1s %1s %10s %11s %11s\n",
			fcstPtr->value, validtime_ansi,
			revision, fcstPtr->shef_qual_code, range_check,
			fcstPtr->product_id, prodtime_ansi, posttime_ansi);
	     

	     fcstPtr = (Forecast *) ListNext(&fcstPtr->node);  
	  } /* while */
	}	  

	else
	  fprintf(print_file_ptr, 
	          "\nThere is NO data for the parameters defined above.\n");
       }
      }

      else /* TS is NOT F* so therfore NOT forecast data */
      {
       if ( obsHead != (Observation *) NULL )
       {
	if ( ( obsPtr = (Observation *) ListFirst(&obsHead->list) ) )
	{
	  while(obsPtr)
	  {
	     /* convert INFORMIX time to ASCII GMT */
	     yearsec_dt_to_timet(obsPtr->obstime, &obstime);
	     tmptr = gmtime(&obstime);
	     strftime(obstime_ansi, sizeof(obstime_ansi), "%m/%d %H:%M", tmptr);

	     yearsec_dt_to_timet(obsPtr->producttime, &prodtime);
	     tmptr = gmtime(&prodtime);
	     strftime(prodtime_ansi, sizeof(prodtime_ansi), "%m/%d %H:%M", tmptr);

	     yearsec_dt_to_timet(obsPtr->postingtime, &posttime);
	     tmptr = gmtime(&posttime);
	     strftime(posttime_ansi, sizeof(posttime_ansi), "%m/%d %H:%M", tmptr);

	     build_qc_symbol(obsPtr->quality_code, range_check);

	     if (obsPtr->revision == 1)
                revision = 'T';
	     else
                revision = 'F';

	     if (((strncmp(current_timeseries.pe, "HG", 2) == 0) ||
		  (strncmp(current_timeseries.pe, "HT", 2) == 0))
		 && (rating_curve_exists == 1))
	     {
		if (obsPtr->value == -9999.0)
			derived_value = -9999.0;
		else
			derived_value = stage2discharge(current_timeseries.lid,
			                                obsPtr->value);

		fprintf(print_file_ptr, 
		        "%8.2f %8.0f %11s %c %1s %1s %10s %11s %11s\n",
			obsPtr->value, derived_value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);
	     }
	     else if ((strncmp(current_timeseries.pe, "QR", 2) == 0) &&
		      (rating_curve_exists == 1))
	     {
		if (obsPtr->value == -9999.0)
			derived_value = -9999.0;
		else
	                derived_value = discharge2stage(current_timeseries.lid,
			                                obsPtr->value);

		fprintf(print_file_ptr, 
		        "%8.2f %8.2f %11s %c %1s %1s %10s %11s %11s\n",
			obsPtr->value, derived_value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);
             }
             else
		fprintf(print_file_ptr, 
		        "%8.2f %11s %c %1s %1s %10s %11s %11s\n",
			obsPtr->value, obstime_ansi,
			revision, obsPtr->shef_qual_code, range_check,
			obsPtr->product_id, prodtime_ansi, posttime_ansi);

	  	  	  
	     obsPtr = (Observation *) ListNext(&obsPtr->node);  
	  }
	}	  
	else
	  fprintf(print_file_ptr, "\nNO data for the parameters defined above.\n");
       }
      }
     
     /* close print timeseries file */
     
     fclose(print_file_ptr);

     return(1);
}


/*************************************************************************/

void	tabularLatestBasis(Widget w, XtPointer ptr, XtPointer cbs)
{  

	/* load the times series list widget. */
	
	tabular_load_timeseries();
	
	
	return;
}


/*************************************************************************/
void    disableCloseFunc( Widget w )
{
	 Arg    args[2];
	 XtSetArg (args[0], XmNdeleteResponse, XmDO_NOTHING);
	 XtSetValues ( w ,args, 1);
	 
	 return;
}


/*************************************************************************/
void	tabular_get_filename(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char         report_dir[128];
   int          gad_token_len=0, gad_value_len=0;
   
   
   if(! tabularFileSB)
   {
      tabularFileSB = XmCreateFileSelectionDialog(GetTopShell(w), 
                                                  "tabularFileSB", NULL, 0);
      
      
      /* set XmNpattern & XmNdialogStyle */
      
      str = XmStringCreateSimple("*");
      ac = 0;
      XtSetArg(args[ac], XmNpattern, str); ac++;
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;

      gad_token_len = strlen("whfs_report_dir");
      get_apps_defaults("whfs_report_dir", &gad_token_len, report_dir, 
                        &gad_value_len);
      str = XmStringCreateSimple(report_dir);
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      XtSetValues(tabularFileSB, args, ac);
      XmStringFree(str);
      
      
      XtAddCallback(tabularFileSB, XmNcancelCallback, tabular_close_filesb, NULL);
      XtAddCallback(tabularFileSB, XmNokCallback, tabular_save_table, NULL);
      
      list = XmFileSelectionBoxGetChild(tabularFileSB, XmDIALOG_LIST);
      XtAddCallback(list, XmNdefaultActionCallback, tabular_save_table, NULL);
   }   
   
   SetTitle(tabularFileSB,"Save File Selection");
   if(! XtIsManaged(tabularFileSB))
      XtManageChild(tabularFileSB);
   
   return;
}


/*************************************************************************/
void	tabular_save_table( Widget w , XtPointer ptr , 
                            XtPointer call_data )
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
	char *name = NULL;
	char cmd_str[300];

        /* Cast the call_data variable to a pointer of type 
           XmFileSelectionBoxCallbackStruct. */
	   
        XmFileSelectionBoxCallbackStruct * cbs = 
               ( XmFileSelectionBoxCallbackStruct * ) call_data ;
	
	if(XtIsManaged(tabularFileSB))
	{
	   XtDestroyWidget(tabularFileSB);
	   tabularFileSB = NULL;
	}
	
	if (! XmStringGetLtoR(cbs->value, charset, &name))
		return;
		
	if (! *name)
	{
		XtFree(name);
		return;
	}


	if (create_table_file() != 0)
	{
	   sprintf(cmd_str, "mv %s %s", temp_print_file, name);
	   system(cmd_str);
	}

	XtFree(name);	

}

/*************************************************************************/
void	tabular_close_filesb(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(tabularFileSB))
   {
      XtDestroyWidget(tabularFileSB);
      tabularFileSB = NULL;
   }
   
   return;
}


/*************************************************************************/
void	tabular_print_table(Widget w, XtPointer ptr, XtPointer cbs)
{
	char cmd_str[200];
	char lpr_print[128];
	char command[200];
	int gad_token_len=0, gad_value_len=0;
	
	if (create_table_file() != 0)
	{
           gad_token_len = strlen("whfs_printcommand"); 
           get_apps_defaults("whfs_printcommand", &gad_token_len, 
	                     lpr_print, &gad_value_len);
           if(strlen(lpr_print) > 0)
           {
             strcpy(command, lpr_print);
           }
           else
           {
             strcpy(command, "lp");
           }

	   sprintf(cmd_str, "%s %s", command,temp_print_file);
	   system(cmd_str);
	   sprintf(cmd_str, "rm  %s", temp_print_file);
	   system(cmd_str);
	}
	
}


/*************************************************************************/
void	tabular_insert_edit(Widget w, XtPointer ptr, XtPointer cbs)
{
	tabinsert_show(w);
}


/*************************************************************************/
/* load a packed list of unique forecast type source codes 
   for the given pe, dur, extremum combination, filtering from 
   the full list of time series. */

void load_forecast_typesource(char current_pe[],
                              int  current_duration,
			      char current_extremum[])
{  
   int i, j, pos;
   int match_found;
   char	 *location_id; 
   char  *physical_element;
   char	 *duration;
   char  *type_source;
   char  *extremum;
   RussText temp_buf;
   int	num_duration;
   
 	
   /* re-initialize the list of type sources */
	
   memset(forecast_typesource_string, '\0', MAX_FCST_TYPESOURCE * SHEF_TS_LEN + 1);
   num_forecast_typesource = 0;
   
  
   for (i = 0; i < current_tab_info.nitems; i++)
   {
       /* parse out variables from timeseries passed in */
	   
       strcpy(temp_buf, current_tab_info.buf[i]);   
       location_id      = strtok(temp_buf, " ");	
       physical_element = strtok(NULL, " ");
       duration         = strtok(NULL, " ");
       type_source      = strtok(NULL, " ");
       extremum         = strtok(NULL, " ");
       
       num_duration = atoi(duration);
	 
	 
       /* only consider forecast type sources, and only consider those that 
          match the ingest key fields. ignore the lid, since they are all
	  for the same id. */
       
       if ((type_source[0] == 'F' || type_source[0] == 'C') &&
           (strcmp(physical_element, current_pe)       == 0) &&
           (strcmp(extremum,         current_extremum) == 0) &&
           (num_duration ==          current_duration))
       {
       
          /* load this type source entry if it is new to the list */

          match_found = 0;
   
          if (num_forecast_typesource < MAX_FCST_TYPESOURCE)
          {
             for (j = 0; j < num_forecast_typesource; j++)
             {
                pos = j * SHEF_TS_LEN;
                if (strncmp(type_source, 
		    &forecast_typesource_string[pos], SHEF_TS_LEN) == 0)
	        {
	           match_found = 1; 
	           break;
	        }
             }
      
             if (!match_found)
             {
	        strcat(forecast_typesource_string, type_source);
                num_forecast_typesource++;
	     }
          }       
       }
   }   


   /* printf("%d:%s:\n",
         num_forecast_typesource, forecast_typesource_string); */

   return;
}


/*************************************************************************/

void tabinsert_show(Widget w)
{	  
     if (! tabinsertDS)
     {
	create_tabinsertDS(GetTopShell(w));
	  
	/* Set up the callbacks */
	
	XtAddCallback(tabinsertSavePB,   XmNactivateCallback, tabinsert_save,  NULL);
	XtAddCallback(tabinsertCancelPB, XmNactivateCallback, tabinsert_cancel, NULL);

        XtAddCallback(tabinsertPidTE, XmNmodifyVerifyCallback,
                     (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
     }
     
     
     if (! XtIsManaged(tabinsertDS))
     {
	/* Manage dialog shell and form */
	
	XtManageChild(tabinsertFO);
	XtManageChild(tabinsertDS);
     }


     /* load the text box strings */
     
     XmTextSetString(tabinsertPidTE, InsertProdID);
     XmTextSetString(tabinsertPtimeTE, InsertProdTime_ANSI);
     XmTextSetString(tabinsertBtimeTE, InsertBasisTime_ANSI);


     /* load the scrolled list with type-source codes */

     load_insert_ts();


     /* raise window */
     
     XRaiseWindow(XtDisplay(tabinsertDS), XtWindow(tabinsertDS));
     
     return;
}


/************************************************************/
void load_insert_ts()
{
   int          i;
   int          pos;
   char		typesource[SHEF_TS_LEN + 1];
   XmStringTable xmStr;
   Arg		arg[MAX_FCST_TYPESOURCE];   
   int		ac;
   
     
   printf("fcst typesources: %s\n", forecast_typesource_string);
   
   
   /* allocate */
   
   xmStr = (XmStringTable) XtMalloc(num_forecast_typesource * sizeof(XmString *));
      
   
   /* loop on the entries */
   
   for (i = 0; i < num_forecast_typesource; i++)
   {     
	memset(typesource, '\0', SHEF_TS_LEN + 1);
        pos = i * SHEF_TS_LEN;
	strncpy(typesource, &forecast_typesource_string[pos], SHEF_TS_LEN);
	
	xmStr[i] = XmStringCreateSimple(typesource);
   }
      
        	   		
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, num_forecast_typesource);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(tabinsert_tsLI, arg, ac);
    
   
   /* free the memory */
   
   for (i = 0; i < num_forecast_typesource; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
    
   return;
} 


/*************************************************************************/
void tabinsert_save(Widget w, XtPointer ptr, XtPointer cbs)
{	  
	char	*buf;
	int	return_code;
        dtime_t	checktime;
	int	select_count;
	int     *poslist = NULL;
	int     pos = 0;
	int     string_pos;
		

	/* read data from the prod ID, Prod Time and Basis Time widgets */
		
	if ( (buf = XmTextGetString(tabinsertPidTE)) )
	{
	  strcpy(InsertProdID, buf);
	  XtFree(buf);
	}

	if ( (buf = XmTextGetString(tabinsertPtimeTE)) )
	{
	    return_code = yearsec_ansi_to_dt(buf, &checktime);
	    if (return_code < 0)
	    {
		ErrorDialog(tabinsertDS, "Invalid Product Date/Time entered.");

		XtFree(buf);
		return;
            } 
	    else
	    {
		strcpy(InsertProdTime_ANSI, buf);
		XtFree(buf);
	    }
	}

	if ( (buf = XmTextGetString(tabinsertBtimeTE)) )
	{
	    return_code = yearsec_ansi_to_dt(buf, &checktime);
	    if (return_code < 0)
	    {
		ErrorDialog(tabinsertDS, "Invalid Basis Date/Time entered.");

		XtFree(buf);
		return;
            } 
	    else
	    {
		strcpy(InsertBasisTime_ANSI, buf);
		XtFree(buf);
	    }
	}


        /* get type-source. */	

	if (num_forecast_typesource >= 1)
	{
	   XmListGetSelectedPos(tabinsert_tsLI, &poslist, &select_count);
	   if (select_count > 0)
	   {
	       pos = poslist[0];
	       
	       /* the first position is one, not zero, but check just in case */
	       
	       if (pos > 0)
   	         string_pos = (pos - 1) * SHEF_TS_LEN;
	       else
	         string_pos = 0;
	   
	       memset(InsertTypeSource, '\0', SHEF_TS_LEN + 1);
	       strncpy(InsertTypeSource, 
	               &forecast_typesource_string[string_pos], SHEF_TS_LEN);
	   }
	   else
	       printf("No type-source selected.\n");
	}
	
	
	/* set Product ID, Time, Basistime, TypeSource labels for inserted data */
	
	SetLabel(tabularInsertProdIDLA,   InsertProdID);
	SetLabel(tabularInsertProdTimeLA, InsertProdTime_ANSI);	
	SetLabel(tabularInsertBasisLA,    InsertBasisTime_ANSI);	
	SetLabel(tabularInsertTypSrcLA,   InsertTypeSource);
	  
	  
	/* unmanage the window. */
	  
	XtUnmanageChild(tabinsertDS);
     
	return;
}


/*************************************************************************/
void	tabinsert_cancel(Widget w, XtPointer ptr, XtPointer cbs)
{
     /* unmanage the window. */ 
     
     XtUnmanageChild(tabinsertDS);
     
     return;
}
