
/****************************************************************************

   File:           pointcontrol_show.c 
   Purpose:        Provide support for Point Control Dialog   


   History:

   Bryon Lawrence      03/10/02   Tried to minimize the possibility of a
                                  NULL pointer read resulting from missing
                                  data in the database.  This should lead
                                  to fewer occurrences of segmentation
                                  violations resulting from trying to
                                  read unallocated memory.  This is
                                  being done in response to DR 10530.
				  
   Jingtao Deng       02/25/03    Modified function get_serv_bkup_info(),
                                  only keep arg responsible_wfo_list.     

   Bryon Lawrence     02/20/04    Modified this routine during the split
                                  of the pointcontrol library into
                                  an engine and a GUI portion.  Also,
                                  changed the wfo_list member of the
                                  pc_options structure to hsa_list since
                                  service backup support is now based on hsa.
                                  
   Chip Gobs          2006        Many and varied changes throughout for the 
                                  implementation of the Time-Step mode.
   **************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "map.h"
#include "pointcontrol.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_presets.h"
#include "pointcontrol_riverstatus.h"
#include "pointcontrol_show.h"
#include "PointDataPresets.h"
#include "pointservice_backup.h"
#include "pointservice_backup_show.h"
#include "preset_saveupdate.h"
#include "preset_saveupdate_show.h"


/* Contains the application context of the application which 
   is launching the Point Data Control GUI. */
static XtAppContext app_context ;


/* The user-supplied display routine.  It is up to the user to supply this
   routine by calling pc_SetDisplay. */
static void ( * display ) ( ) = NULL ;
static int		 retrieval_required;
static int disableDrawing = 0; //used to avoid redrawing when using SetGUIFromOptions (or whatever it's called)

/* Contains the map widget to plot point data on. */
static Widget map_widget = 0 ;


static char timeStepDataElementStringArray[][50] = {
    										 // RIVER
    										"STAGE/POOL",
    									    "FLOW/STORAGE", 
    									    "DEPTH ABOVE FLOOD STAGE",  
    									    "PERCENT OF FLOOD FLOW",
    									     
										    // RAIN
										    "INSTANTANEOUS",
										    "1-HOUR PRECIP TOTAL",
										    "3-HOUR PRECIP TOTAL",
										    "6-HOUR PRECIP TOTAL",
										    "24-HOUR TOTAL (12Z)",
   
   											// SNOW
  											"SNOW WATER EQUIVALENT",
   											"SWE - 24 HOUR CHANGE",
   
   											// TEMPERATURE
   											"TEMPERATURE",
 										    "TEMP. 24 HOUR CHANGE",
  											"MAX TEMP",
										    "MIN TEMP",
   
  											 //HUMIDITY
   											"DEWPOINT",
  											"DEWPT - 24 HR CHANGE",
  											"RELATIVE HUMIDITY",
   
   											// WIND
   											"WIND SPEED",
   											"WIND DIRECTION" 									    
    									   };
    				

/*****************************************************************************
   pc_SetDisplay()
   Sets the display routine to be used in the callback from the "Map Data"
   button.
 **************************************************************************/
void pc_SetDisplay ( void ( * display_routine ) ( ) )
{
   display = display_routine ;
}

// --------------------------------------------------------------------
void pc_drawMapIfNotDisabled(Widget widget, Boolean forceRetrieveData)
{
	if (! disableDrawing)
	{
	    pc_drawMap(widget, forceRetrieveData);
	}
}
// --------------------------------------------------------------------
void pc_drawMap(Widget widget, Boolean forceRetrieveData)
{
	
	//set the widget to lock the cursor up against
	if (widget == NULL)
	{
	   widget = pointcontrolDS;	
	}
	
	// set the retrieval_required variable only if 
	// forceRetrieveData is set to true,
	// retrieval_required might already be true, though
    if (forceRetrieveData)
	{
	   retrieval_required = True;	
	}
	
	
	 /* Enable the plotting of station data. Inform the hv areal routines
      that they should plot the station data on the screen. */
    enableStationDrawing ( ) ;

    /* get and set the time fields */
    set_timefields();
   
    /* now map the data */
    pc_RetrieveAndMapData ( widget , retrieval_required ) ;
    retrieval_required = False;
	
    
    return;
}


// --------------------------------------------------------------------

void pc_RetrieveAndMapData ( Widget w , int retrieval_required_arg)
{
	
   char header[] = "pc_RetrieveAndMapData(): ";
   	
   printf("%s  retrieval_required_arg = %d \n", header, retrieval_required_arg);	
   int drawStationFlag ;

   drawStationFlag = getStationDrawingState ( ) ;

   /* set the cursor to a watch while the request is being processed */
   SetCursor ( w , XC_watch ) ;

   /* Process the user request.  Only process this request if
      the option to map data is "on". */

   if ( drawStationFlag == 1 )
   {
      pc_process_request ( retrieval_required_arg ) ;
   }

   /* Display the retrieved data.  This is done by
      calling the display routine that the USER
      supplied via the pc_SetDisplay routine. */
   if ( display != NULL )
   {
      display ( ) ;
   }

   /* Clear the watch cursor. */
   UnsetCursor ( w ) ;

   return ;
}

/******************************************************************************
   pc_TabulateData()
   callback for Table Data pushbutton
   ***************************************************************************/
static void pc_TabulateData ( Widget     w ,
                              int        retrieval_required )
{
   /* set the cursor to a watch while the request is being processed */
   SetCursor(pointcontrolFO, XC_watch);

   /* proces the user request */
   pc_process_request(retrieval_required);
   UnsetCursor(pointcontrolFO);

   /* display the retrieved data */
   show_pointcontrol_tableDS(w, getReportListHead());
   
   return;
}

/******************************************************************************
   The callback for the "Clear Data" button.  This simply informs the drawing
   routines in the hv areal library whether or not to plot the point data.
   The outside world should have no reason to call this routine.  So, it has
   been made "static" which limits its scope to this file.
   ***************************************************************************/
void pc_ClearCB ( Widget w , XtPointer clientdata , XtPointer calldata ) 
{
   disableStationDrawing ( ) ;

   /* Disable the station highlighting feature. */
   pc_RetrieveAndMapData ( w , retrieval_required ) ;

   /* Free the memory used by the river status list which is used in 
      deriving the river threat index values for each of the data points. */
   FreeRiverStatusList ( ) ;
}
   
/******************************************************************************
   Load Point Display Control Dialog    
   ***************************************************************************/

void show_pointcontrolDS ( Widget w , Widget map , 
                           XtAppContext app )
{
	
 //  char header[] = "show_pointcontrolDS(): ";	
	
   int preset_count = 0;	
	
   if ( ( pointcontrolDS == ( Widget ) NULL ) ||
       ! XtIsManaged ( pointcontrolDS ) )
   {
      /* Set variables necessary for drawing to the map and listening
         for a double mouse click in the pointdata presets box. */
      map_widget = map ;
      app_context = app ;

      create_pointcontrolDS ( GetTopShell ( w ) ) ;
      
           /* By default, this GUI is being made MODAL. */
      XtVaSetValues ( pointcontrolDS , XmNdialogStyle ,
                      XmDIALOG_FULL_APPLICATION_MODAL , NULL ) ;
        
      pc_loadElementTypeCBX();
      
      /* add the callbacks */
      pointcontrol_add_callbacks();
       
      /* load and save pe-ts info from Ingestfilter table, then use
	 the information to load the entries in the pe and ts lists */ 
      save_PeTsIngestfilter();
      count_PhysElemTypes();
      
      /* load entries into DataSource list widget */
      load_DatasrcList ( ) ;
  
        /* load entries into the Predefined Option Sets scrolled list widget. */ 
      preset_count = load_presetOptionList ( True , NULL , False ) ;
      
      
      /* desensitize the type source list widget as default */
 //     desensitize_type_source_widgets();
 
    //  DeSensitize(pc_typeSourcePB);
      
 //     DeSensitize(pc_timeStepTB);
      
       
      /* initialize a variable that is used to indicate whether
	 data needs to be retrieved because it is currently not
	 buffered.  this spares the need to retrieve and derive
	 data when only a post-processing option has changed, and
	 the buffered data is adequate to perform the 
	 requested operation */
      
      retrieval_required = 1;      
   }
   
   XtManageChild(pointcontrolFO);
   XtManageChild(pointcontrolDS);
      
   /* set the default values of the options */
   pc_SetGUIFromOptionsStruct();            	 
   
   if (preset_count > 0)
   {
       selectFirstPreset();
   }
   
   return;   
}

// ---------------------------------------------------------------------------------------------

void selectFirstPreset()
{
    XmListSelectPos ( pc_presetsLI , 1 , True ) ;
}

/***************************************************************************** 
   pointcontrol_callbacks()
   adds all the callbacks.   
   for the radio buttons, not the hardcoded values for the callbacks.
   **************************************************************************/
void pc_add_adhoc_callbacks()
{
	 /* callbacks on the operations which require a re-retrieval of data */
   
   XtAddCallback(pc_elementTypeCBX, XmNselectionCallback, pc_AdHocElementTypeCB, 
                 (XtPointer) NULL);
   
    
   XtAddCallback(pc_physicalElementCBX, XmNselectionCallback,
                 pc_AdHocPhysElementCB, (XtPointer) NULL);
                 
                 
   XtAddCallback(pc_dataSourceTB, XmNvalueChangedCallback, pc_dataSourceToggleButtonCB, 
                 (XtPointer) NULL);
                 
   XtAddCallback(pc_dataSourcePB, XmNactivateCallback, pc_AdHocDataSourceItemChooserCB, 
                 (XtPointer) 1);      
                 
                 
   // Type Source filtering 
        			  
   XtAddCallback(pc_typeSourceTB, XmNvalueChangedCallback,    pc_TypeSrcToggleCB,
   			    (XtPointer) NULL);			 
   			    
   XtAddCallback(pc_typeSourcePB, XmNactivateCallback, 
    			  pc_AdHocTypeSourceItemChooserCB, NULL);	     
                 
}


void pc_remove_adhoc_callbacks()
{
	
	char header[] = "pc_remove_adhoc_callbacks(): ";
	printf("%s inside ...\n", header);
	
	XtRemoveAllCallbacks(pc_elementTypeCBX, XmNselectionCallback);
	XtRemoveAllCallbacks(pc_physicalElementCBX, XmNselectionCallback);
	
	XtRemoveAllCallbacks(pc_dataSourceTB, XmNvalueChangedCallback);
	XtRemoveAllCallbacks(pc_dataSourcePB, XmNactivateCallback);	
	
	XtRemoveAllCallbacks(pc_typeSourceTB, XmNvalueChangedCallback);
	XtRemoveAllCallbacks(pc_typeSourcePB, XmNactivateCallback);	
	

}

void pc_add_timestep_callbacks()
{
	
   XtAddCallback(pc_elementTypeCBX, XmNselectionCallback, pc_TimeStepElementTypeListCB, 
                 (XtPointer) NULL);
   
    
   XtAddCallback(pc_physicalElementCBX, XmNselectionCallback,
                 pc_TimeStepElementListCB, (XtPointer) NULL);
                 
                 
   XtAddCallback(pc_dataSourceTB, XmNvalueChangedCallback, pc_dataSourceToggleButtonCB, 
                 (XtPointer) NULL);
                 
   XtAddCallback(pc_dataSourcePB, XmNactivateCallback, pc_TimeStepTypeSourceItemChooserCB, 
                 (XtPointer) 1);



   //Instantaneous Precip SimpleSpinBox
   XtAddCallback(pc_precipHoursSSB, XmNvalueChangedCallback, pc_instPrecipSimpleSpinBoxCB,
                    (XtPointer) NULL); 

 
   // River Station Filtering     
   XtAddCallback(pc_allStationsPB, XmNactivateCallback, pc_riverStationFilterCB, 
                 (XtPointer) ALL_STATIONS_RSF);
                 
   XtAddCallback(pc_streamOnlyPB, XmNactivateCallback, pc_riverStationFilterCB, 
                 (XtPointer) STREAM_STATIONS_RSF);
                  
   XtAddCallback(pc_reservoirOnlyPB, XmNactivateCallback, pc_riverStationFilterCB, 
                 (XtPointer) RESERVOIR_STATIONS_RSF);
     
     
   // Precip Pe Filtering  
   XtAddCallback(pc_precipPcAndPpPB, XmNactivateCallback, pc_precipPeFilterCB, 
                 (XtPointer) PC_AND_PP_PPF);   
                 
   XtAddCallback(pc_precipPcOnlyPB, XmNactivateCallback, pc_precipPeFilterCB, 
                 (XtPointer) PC_ONLY_PPF);         
                 
   XtAddCallback(pc_precipPpOnlyPB, XmNactivateCallback, pc_precipPeFilterCB, 
                 (XtPointer) PP_ONLY_PPF);                                      
                       	      
  
    // Type Source filtering     			  
   XtAddCallback(pc_typeSourceTB, XmNvalueChangedCallback,    pc_TypeSrcToggleCB,
   			    (XtPointer) NULL);			 
   			    
   // this is not a typo, but I will want to get rid of the other button in this mode.			    
   XtAddCallback(pc_typeSourcePB, XmNactivateCallback, 
    			  pc_TimeStepTypeSourceItemChooserCB, NULL);	                          
         
}

void pc_remove_timestep_callbacks()
{
	
	char header[] = "pc_remove_timestep_callbacks(): ";
	printf("%s inside ...\n", header);
	
	XtRemoveAllCallbacks(pc_elementTypeCBX, XmNselectionCallback);
	XtRemoveAllCallbacks(pc_physicalElementCBX, XmNselectionCallback);
	
	XtRemoveAllCallbacks(pc_dataSourceTB, XmNvalueChangedCallback);
	XtRemoveAllCallbacks(pc_dataSourcePB, XmNactivateCallback);
	
	XtRemoveAllCallbacks(pc_typeSourceTB, XmNvalueChangedCallback);
	XtRemoveAllCallbacks(pc_typeSourcePB, XmNactivateCallback);		
	
	XtRemoveAllCallbacks(pc_precipHoursSSB, XmNentryCallback);
                    
}
	

void pc_add_common_callbacks()
{
	 
    /* callbacks for predefined settings */
    
    /* ComboBox item selection */
   XtAddCallback(pc_presetsCBX, XmNselectionCallback,  pc_PredefinedOptionSelectCB , 
                 (XtPointer) NULL) ;
                 
                 
                 
    /* Preset Save button. */             
    XtAddCallback(pc_savePresetsPB, XmNactivateCallback, pc_SaveAsNewCB, 
                 (XtPointer) NULL ) ;

   /* Preset Delete button. */
    XtAddCallback ( pc_deletePresetsPB, XmNactivateCallback , pc_DeleteCB ,
                   ( XtPointer ) NULL ) ; 
   
   
   /*  Add Query Mode callbacks */
    XtAddCallback( pc_adhocTB,  XmNvalueChangedCallback, 
    		       pc_QueryModeCB, (XtPointer) NULL);
    		       			    
    XtAddCallback( pc_timeStepTB,  XmNvalueChangedCallback,
    			   pc_QueryModeCB, (XtPointer) NULL);		
             
 
   
   		
   /* Value/Time Control  callbacks */
   
   XtAddCallback(pc_latestValuePB,   XmNactivateCallback, pc_TimeModeCB,
                 (XtPointer) LATEST );
    
   XtAddCallback(pc_selectedValuePB,   XmNactivateCallback, pc_TimeModeCB,
                 (XtPointer) SETTIME);              
   
   XtAddCallback(pc_minValuePB,   XmNactivateCallback, pc_TimeModeCB,
                 (XtPointer) MINSELECT);              
   
   XtAddCallback(pc_maxValuePB,   XmNactivateCallback, pc_TimeModeCB, 
                 (XtPointer) MAXSELECT);              
   
   
  XtAddCallback(pc_valueChangePB,   XmNactivateCallback, pc_TimeModeCB, 
                 (XtPointer) VALUE_CHANGE);              
   
  
   /* arrow button callbacks */  

   XtAddCallback(pc_dayupAB,    XmNactivateCallback, increaseEndDayCB,  NULL);
   XtAddCallback(pc_daydownAB , XmNactivateCallback, decreaseEndDayCB,  NULL);
   XtAddCallback(pc_hourupAB,   XmNactivateCallback, increaseEndHourCB, NULL);
   XtAddCallback(pc_hourdownAB, XmNactivateCallback, decreaseEndHourCB, NULL);
   
   
   	XtAddCallback(pc_hrsoldTX, XmNvalueChangedCallback, pc_hoursTextFilterCB,
                 (XtPointer) NULL);


    Widget map_widget = _get_map_widget ( 0 ) ;
   //listen to keys to up and down arrow actions
   XtAddEventHandler ( map_widget , KeyPressMask , FALSE , arrow_key_press_handler ,
                     NULL ) ;
  
   XtInsertEventHandler (pc_hourupAB , KeyPressMask , FALSE , arrow_key_press_handler ,
                     NULL, XtListHead ) ;
  
   XtInsertEventHandler (pointcontrolFO , KeyPressMask , FALSE , arrow_key_press_handler ,
                     NULL, XtListHead ) ;
  
  
 
   /* callbacks for filtering, controlling how the retrieved
      data are presented. some callbacks have hardcoded values
      passed to callback function - egad! */
    
                 
  
   XtAddCallback(pc_showMissingTB, XmNvalueChangedCallback, pc_showMissingToggleButtonCB, 
                 (XtPointer) NULL);
                           
                 
   XtAddCallback(pc_showNonFcstPntsTB, XmNvalueChangedCallback, 
   				  pc_showForecastPointsToggleButtonCB, 
                 (XtPointer) NULL);
      
      
      
      
   XtAddCallback(pc_serviceAreaTB, XmNvalueChangedCallback, pc_hsaToggleCB, 
                 (XtPointer) 0);
   XtAddCallback(pc_serviceAreaPB, XmNactivateCallback, pc_hsaButtonCB, 
                 (XtPointer) 1);           
   
   
   
     
   
   /* OB7 Change - these toggle callbacks now have the button passed in with them instead of an
    * arbitrary constant
    * */
   
   XtAddCallback(pc_valueTB,  XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_valueTB);
   XtAddCallback(pc_iconTB,   XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_iconTB);
   XtAddCallback(pc_idTB,     XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_idTB);
   XtAddCallback(pc_nameTB,   XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_nameTB);
      
      
   XtAddCallback(pc_timeTB,   XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_timeTB);
              
    /* OB7 Change - new toggles  */             
   XtAddCallback(pc_elevTB,   XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_elevTB);
   XtAddCallback(pc_paramCodeTB, XmNvalueChangedCallback, pc_ShowCB, 
                 (XtPointer) pc_paramCodeTB);
                 
          
   /* OB7 - widget name change */                 
   XtAddCallback(pc_colorRiverIconTB, XmNvalueChangedCallback, pc_ShowCB,
                 (XtPointer) pc_colorRiverIconTB);
   
   
   
   /* Added for OB7  */
    XtAddCallback(pc_showAllValuesPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_ALL);             
    XtAddCallback(pc_showValuesEqPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_EQUAL);           
    XtAddCallback(pc_showValuesNeqPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_NOT_EQUAL);         
    XtAddCallback(pc_showValuesGeqPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_GREATER_EQUAL);
    XtAddCallback(pc_showValuesLeqPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_LESS_EQUAL);
    XtAddCallback(pc_showValuesGtPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_GREATER);
    XtAddCallback(pc_showValuesLtPB, XmNactivateCallback, pc_valueFilterOperationCB,
                 (XtPointer) SHOW_LESS);
                 
   /* Added for OB7  */
    XtAddCallback(pc_showAllElevPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_ALL);
    XtAddCallback(pc_showElevEqPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_EQUAL);        
    XtAddCallback(pc_showElevNeqPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_NOT_EQUAL);
    XtAddCallback(pc_showElevGeqPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_GREATER_EQUAL);
    XtAddCallback(pc_showElevLeqPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_LESS_EQUAL);
    XtAddCallback(pc_showElevGtPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_GREATER);
    XtAddCallback(pc_showElevLtPB, XmNactivateCallback, pc_elevFilterOperationCB,
                 (XtPointer) SHOW_LESS);


	XtAddCallback(pc_filterByValueTF, XmNvalueChangedCallback, pc_valueTextFilterCB,
                 (XtPointer) NULL);


 	XtAddCallback(pc_filterByElevTF, XmNvalueChangedCallback, pc_elevTextFilterCB,
                 (XtPointer) NULL);

 
   
    /* OB7 - passing in the button instead of magic numbers */    
   XtAddCallback(pc_valPB,        XmNactivateCallback, pc_StageDataToDisplayCB, 
                 (XtPointer) pc_valPB );
   XtAddCallback(pc_val_fldPB,    XmNactivateCallback, pc_StageDataToDisplayCB, 
                 (XtPointer) pc_val_fldPB);
   XtAddCallback(pc_val_stageflowPB, XmNactivateCallback , pc_StageDataToDisplayCB, 
                 (XtPointer) pc_val_stageflowPB);
   XtAddCallback(pc_departPB,     XmNactivateCallback, pc_StageDataToDisplayCB, 
                 (XtPointer) pc_departPB);
   XtAddCallback(pc_depart_fldPB, XmNactivateCallback, pc_StageDataToDisplayCB, 
                 (XtPointer) pc_depart_fldPB);
   
   XtAddCallback(pc_obsrivPB,   XmNactivateCallback, pc_StageBasisCB, 
                 (XtPointer) BASIS_OBS);
   XtAddCallback(pc_fcstrivPB,  XmNactivateCallback, pc_StageBasisCB, 
                 (XtPointer) BASIS_FCST);
   XtAddCallback(pc_maxrivPB,   XmNactivateCallback, pc_StageBasisCB, 
                 (XtPointer) BASIS_MOFO);
   
                  
   
   
/*  Removed for OB7 pc_predefineLS eventhandler replaced with
   XtAddEventHandler ( pc_predefineLS , ButtonPressMask , False ,
                       process_clicks , NULL ) ;
*/




  /* callbacks for getting, deriving, and presenting the data */
   
   XtAddCallback(pc_mapPB,   XmNactivateCallback, pc_MapButtonCB,   
                 (XtPointer) NULL);
   XtAddCallback(pc_tablePB, XmNactivateCallback, pc_TableCB, 
                 (XtPointer) NULL);
   XtAddCallback(pc_clearPB, XmNactivateCallback, pc_ClearCB, 
                 (XtPointer) NULL);
   
   
   /* callback for closing the window */
   XtAddCallback(pc_closePB, XmNactivateCallback, pc_CloseCB, 
                 (XtPointer) NULL);
   
   return; 
	
}



void pc_setup_mode_related_callbacks(QueryMode queryMode)
{	
	char *header = "pc_setup_mode_related_callbacks(): ";
	printf("%s queryMode = %d\n", header, queryMode);
	
	if (queryMode == TIME_STEP_MODE)
	{
		pc_remove_adhoc_callbacks();
		pc_add_timestep_callbacks();
	}
	else if (queryMode == AD_HOC_MODE)
	{
		pc_remove_timestep_callbacks();
		pc_add_adhoc_callbacks();	
	}
}


void pointcontrol_add_callbacks()
{
   pc_options_struct * pc_options = get_pc_options();
	
   pc_add_common_callbacks();
   
   pc_setup_mode_related_callbacks(pc_options->query_mode);
    
   
   return; 
}


/*****************************************************************************
   pc_SetGUIFromOptionsStruct()
   set default values for toggle buttons.  use tokens later...
   **************************************************************************/

void pc_SetGUIFromOptionsStruct()
{
   pc_options_struct * pc_options = get_pc_options();
   char header[] = "pc_SetGUIFromOptionsStruct(): ";
   char valueString[BUFSIZ];
   int position = 0;


 //  printf("%s  starting ********************\n", header); 

  /* set the query mode radio button */

   if (pc_options->query_mode == AD_HOC_MODE)
   {
        XmToggleButtonSetState(pc_adhocTB,  True,  True);
   }
   else
   {
   	
  //	    if (! DISABLED_TIME_STEP)
   	    {
   	        XmToggleButtonSetState(pc_timeStepTB,  True,  True);
   	    }
   	//    else
   	//    {
   	//    	XmToggleButtonSetState(pc_adhocTB,  True,  True);
   	//    }
   } 	    
   	
   	/*
   	 * Set the element Type
   	 * 
   	 */
//   printf("%s  pc_options->element_type=%d\n", header, pc_options->element_type); 
   
   position = pc_options->element_type + 1;
  
   XmListSelectPos(pc_elementTypeLI, position, True);
  
 	
   /* Note: The element is set when the pc_TimeStepElementTypeListCB
    * callback is activated, not directly in here.
    * 
    * In turn, it will call pc_loadTimeStepElementXmListByElementType(),
    * which will call pc_loadTimeStepElementCBX, which will  select the correct item, if any
    * 
    */	
          
   printf("%s  pc_options->lsminmax = %d\n", header, pc_options->time_mode);
   
    SetMenuPos(pc_valueFunctionOM, pc_options->time_mode);
   /* set the time mode */
   
     
   /* set the endtime and the duration */
   pc_load_timeinfo();
   
   
   // set the options for the Instantaneous precip accumulation time
   // Only is visible when in time-step mode, and instantaneous precip has been
   // selected 
    int selection = pc_options->inst_precip_accum_time_selection;
    if (  (selection < 0) || (selection > PRECIP_TIME_24_HOURS) )
    {
    	pc_options->inst_precip_accum_time_selection = PRECIP_TIME_30_MINUTES;    	
    }
    //XmSetSp(pc_precipHoursSSB , pc_options->inst_precip_accum_time_selection);
    XtVaSetValues (pc_precipHoursSSB, XmNposition, pc_options->inst_precip_accum_time_selection, NULL);
   
  
  
   // set the toggle button that corresponds to the filter by typesource option
   XmToggleButtonSetState(pc_typeSourceTB, pc_options->filter_by_typesource,  True);
  
   
    
   /* Set the service backup according to the value in the pc_options
      structure.  */
   if ( ( pc_options->filter_by_hsa == 1 ) &&
        ( pc_options->hsa_list [ 0 ] != '\0' )
      )
   {
      XmToggleButtonSetState ( pc_serviceAreaTB , True ,  False ) ;
   }
   else
   {
      XmToggleButtonSetState ( pc_serviceAreaTB , False ,  False ) ;
   }

   

   /* set the datasource toggle button */
    XmToggleButtonSetState(pc_dataSourceTB,
                           pc_options->filter_by_datasource,  False);
   

   /* set the filter options */
   if (pc_options->suppress_missing) //reversed polarity!
      XmToggleButtonSetState(pc_showMissingTB, False,  False);
   else
      XmToggleButtonSetState(pc_showMissingTB, True, False);
   
   
   /* set the pc_pc_riverStationFilterOM based on river_station_filter */  
   SetMenuPos(pc_riverStationFilterOM, pc_options->river_station_filter );
   
    /* set the pc_precipPeOM based on precip_pe_filter */  
   SetMenuPos(pc_precipPeOM, pc_options->precip_pe_filter );
   
   
   /* set the pc_filterElevationOM */  
   SetMenuPos(pc_filterElevationOM, pc_options->elevFilterOperation );
   
   enableDisableValueFilterText(pc_options->elevFilterOperation, pc_filterByElevTF);
     
    /* Set the text field that holds the associated filter value */
   sprintf(valueString, "%-6.2lf", pc_options->elevFilterValue);
   
  // printf("%s before setting the elev. XmText valueString = :%s: \n", header, valueString); 
   XmTextSetString(pc_filterByElevTF, valueString);
 //  printf("%s after setting the elev. XmText valueString = :%s:\n", header, valueString); 
  
     
   /* set the pc_filterValueOM, */  
   SetMenuPos(pc_filterValueOM, pc_options->valueFilterOperation );
   enableDisableValueFilterText(pc_options->valueFilterOperation, pc_filterByValueTF);
 
     
   /* Set the text field that holds the associated filter value */
   sprintf(valueString, "%-6.2lf", pc_options->valueFilterValue);
   
  // printf("%s before setting the value XmText valueString = :%s: \n", header, valueString); 
   XmTextSetString(pc_filterByValueTF, valueString);
 //  printf("%s after setting the value XmText valueString = :%s: \n", header, valueString); 
   
   
   
   if (pc_options->fcstpts_only) //reversed polarity!
      XmToggleButtonSetState(pc_showNonFcstPntsTB, False,  False);
   else
      XmToggleButtonSetState(pc_showNonFcstPntsTB, True, False);
   
   
   /* set the map display options */
   

   XmToggleButtonSetState(pc_valueTB,  pc_options->value,  False);
   
   XmToggleButtonSetState(pc_idTB,  pc_options->id,  False);
  
   XmToggleButtonSetState(pc_nameTB,  pc_options->name,  False);
   
   XmToggleButtonSetState(pc_timeTB,  pc_options->time,  False);
   
   XmToggleButtonSetState(pc_iconTB,  pc_options->icon,  False);
   
   XmToggleButtonSetState(pc_elevTB,  pc_options->elevation,  False);
   	    
   XmToggleButtonSetState(pc_paramCodeTB,  pc_options->paramCode,  False);
 	    
   /* set the river options. */
   
   SetMenuPos(pc_basisOM, pc_options->stagebasis);
  
  
   RiverValueDisplayMode  mode = getRiverValueDisplayMode(pc_options);
  
   SetMenuPos(pc_rivvalsOM, mode);
   
   /* Set the state of the display riverstatus toggle button. */
   /*  name of pc_riverstatTB changed to pc_colorRiverIconTB*/
   XmToggleButtonSetState(pc_colorRiverIconTB,  pc_options->riverstatus,  False);
  
   
 //  printf("%s  ending \n", header); 
   
   return;
}


RiverValueDisplayMode getRiverValueDisplayMode(pc_options_struct * pc_options)
{
    RiverValueDisplayMode mode = RAW_VALUE_ONLY;	
	
   if (pc_options->valuetype == TYPE_VALUE &&  
       pc_options->fldlevel  == 0 && 
       pc_options->derive_stage_flow == 0)   
   {       
      mode = RAW_VALUE_ONLY;
   }   
   else if (pc_options->valuetype == TYPE_VALUE && 
	    pc_options->fldlevel  == 1)   
   {     
      mode = 	RAW_VALUE_FLOOD_LEVEL;   
   }
      
   else if (pc_options->valuetype == TYPE_VALUE && 
	    pc_options->derive_stage_flow == 1) 
   {     
      mode =  RAW_VALUE_STAGE_FLOW	;    
   }
   
   else if (pc_options->valuetype == TYPE_DEPART && 
	    pc_options->fldlevel  == 0)  
   {    
      mode = FLOOD_DEPARTURE;   
   }
   
   else if (pc_options->valuetype == TYPE_DEPART && 
	    pc_options->fldlevel  == 1)      
      mode = FLOOD_DEPARTURE_FLOOD_LEVEL;
      
      
   return mode;
	
}
  
  

/******************************************************************************
   pc_load_timeinfo()
   ***************************************************************************/
void pc_load_timeinfo()
{
	
   pc_options_struct * pc_options = get_pc_options();
   char header[] = "pc_load_timeinfo(): ";
	
   printf("%s ----------- \n", header);	
	
   if (pc_options->query_mode == AD_HOC_MODE)
   {	
	   /* Set the date time text field on the PDC GUI. */
	   set_pcoptions_timestring ( ) ;
	   XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
	   
	   /* Set the hours duration field on the PDC GUI. */
	   char pc_hour_str[MAXLEN_HOUR];
	   sprintf(pc_hour_str, "%d", pc_options->dur_hours);
	   XmTextSetString(pc_hrsoldTX, pc_hour_str);
   }
   else //TIME_STEP_MODE
   {
   	    time_t current_time;
   	    time(&current_time);
   	    time_t valid_time = current_time;
   	    long roundingHours = 1;
   	    long additionalHours = 0;
   	    
   	    if (pc_options->element_type == RAIN_TIME_STEP_TYPE)
   	    {
   	        if (pc_options->selectedTimeStepElement == INSTANTANEOUS_PRECIP_TSDE)
   	        {
   	          //do nothing	
   	        }
   	    	else if (pc_options->selectedTimeStepElement ==  HOURLY_PRECIP_TSDE)
   	    	{
   	    		roundingHours = 1;
   	    		printf("%s  hourly precip \n", header);	
   	    	}
   	    	else if  (pc_options->selectedTimeStepElement ==  THREE_HOUR_PRECIP_TSDE)
   	    	{
   	    		roundingHours = 3;
   	    		printf("%s 3 hourly precip- \n", header);	
   	    	}
   	    	else if  (pc_options->selectedTimeStepElement ==  SIX_HOUR_PRECIP_TSDE)
   	    	{
   	    		roundingHours = 6;
   	    		printf("%s 6 hourly precip- \n", header);	
   	    	}
   	    	else if (pc_options->selectedTimeStepElement ==  DAILY_PRECIP_TSDE)
   	    	{
   	    		roundingHours = 24;
   	    		additionalHours = 12;		//daily periods end at 12Z
   	    		printf("%s 24 hourly precip \n", header);	
   	    	}
   	    	else
   	    	{
   	    		printf("%s ERROR - I don't know what kind of precip I am dealing with. Examine code for errors. \n", header);	
   	    	}
   	    	
   	    	valid_time /= roundingHours * SECONDS_PER_HOUR;
   	    	valid_time *= roundingHours * SECONDS_PER_HOUR;
   	    	
   	    	valid_time += additionalHours * SECONDS_PER_HOUR;	    	
   	    }
        
        else if ( 
                       (pc_options->selectedTimeStepElement ==  TEMP_MAX_TSDE) ||
                       (pc_options->selectedTimeStepElement ==  TEMP_MIN_TSDE)
                )
        {
            
            if (pc_options->selectedTimeStepElement ==  TEMP_MAX_TSDE)
            {
                roundingHours = 24;
                additionalHours = 12;       //daily periods end at 12Z
            }
            else if (pc_options->selectedTimeStepElement ==  TEMP_MIN_TSDE)
            {
                roundingHours = 24;
                additionalHours = 12;       //daily periods end at 12Z
            }
            
            valid_time /= roundingHours * SECONDS_PER_HOUR;
            valid_time *= roundingHours * SECONDS_PER_HOUR;
            
            valid_time += additionalHours * SECONDS_PER_HOUR;       
        }
        
   	    else  // other types don't need rounding
   	    {
            valid_time = pc_options->valid_timet;
            valid_time /= SECONDS_PER_HOUR;
            valid_time *= SECONDS_PER_HOUR;
   	        // do nothing
   	    }
   	
   	    pc_options->valid_timet = valid_time;
   	
   	    set_pcoptions_timestring ( ) ;
	    XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
	    
	    printf("%s pc_options->pc_time_str = %s\n",
	    	   header, pc_options->pc_time_str);
   }
   
   
   return;
}

/******************************************************************************
   pc_LoadElementTypeCBX
   Loads the Element Type ComboBox with data, depending on the query mode
   ****************************************************************************/
void pc_loadElementTypeCBX( )
{	
	XmString  xmstring = NULL;
	int i = 0;	
    pc_options_struct * pc_options = get_pc_options();
  
	pc_engine_LoadElementTypes(pc_options->query_mode); 
	
    pc_pets_struct *pc_petsdata = get_pc_petsdata ( ) ;

    XmListDeleteAllItems ( pc_elementTypeLI) ;

 	for (i = 0; i < pc_petsdata->element_type_count ; i++)
	{ 
 	    xmstring = XmStringGenerate ( (XtPointer) pc_petsdata->elementTypeTextArray[i], 
 	     							  XmFONTLIST_DEFAULT_TAG,
 	     							  XmCHARSET_TEXT,
 	     							  NULL );
	    XmComboBoxAddItem(pc_elementTypeCBX,  xmstring, 0, False);
	    XmStringFree (xmstring);   
	}	
	
	//gets rid of Processed Entry if shef_proc_obs is true
	checkOtherList();
   
}

/******************************************************************************
   load_DatasrcList
   Loads the data source list widget with the data sources contained in
   the pc_petsdata.datasrcbuf buffer.
   ****************************************************************************/
void load_DatasrcList ( )
{
   int datasource_count ;
   pc_pets_struct * pc_petsdata = NULL ;

   pc_petsdata = get_pc_petsdata ( ) ;

   datasource_count = get_AdHocDataSourceList( ) ; 

   if ( datasource_count > 0 )
   {
   	/* removed for OB7 */
  /*    loadXmList100(pc_datasrcLS, pc_petsdata->datasrcbuf, datasource_count);
   */  
   }
}
   
/******************************************************************************
   checkOtherList()
   Checks the setting of the shef_procobs token and sees whether the
   last item in the scrolled list which contains the "Processed" entry,
   should be deleted.  This function is only called when the window
   is created.
   ***************************************************************************/

void checkOtherList()
{ 
   int shef_proc_obs_on ;

   /* Find out if the procobs token is on */
   shef_proc_obs_on = check_ShefProcObs ( ) ;
   
   /* If treating processed data as data comingled in the obs tables,
      as opposed to segregated in its own table, then remove the 
      entry in the Other list for the Processed table.
      Note constant number for the position of the Processed entry !!! */
   if ( shef_proc_obs_on == 1 )
   {
     /* REMOVED FOR OB7
      XmListDeletePos ( pc_otherelemLS , LIST_LOCATION_OF_PROCESSED_ENTRY ) ;
      */
          
      //delete the "Processed" entry which is the last entry, 
	  // position 0 in the list indicates the last entry  
      
       XmListDeletePos (pc_elementTypeLI, 0 ); 
   }
   
   return;
}

/* ========================================================================
   ======= Beginning of callbacks controlling the data retrieval ==========
   ========================================================================*/
/****************************************************************************** 
   pc_PointTypeCB()
   callback for datatype selection such as River/Rain/Snow/Temp/Other 
   ***************************************************************************/
#ifdef	OB6
void old_pc_PointTypeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   pc_pets_struct * pc_petsdata = NULL ;
   pc_petsdata = get_pc_petsdata ( ) ;

   /* assign the datatype based on which toggle button
      is selected */
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
      
   set_pcoptions_datatype ( ( int ) ptr ) ;
   
   /* ignore the toggled off events */
   
   if (tb_state->set== 0) return;
   
   /* now load the element list accordingly */
   
   if (pc_options->element_type == RIVER_AD_HOC_TYPE) 
   {
      pc_loadAdHocElementXmList(pc_physelemLS, pc_petsdata->riverbuf, 
                        pc_petsdata->nriver);
   }
   
   if (pc_options->element_type == RAIN_AD_HOC_TYPE) 
   {
      pc_loadAdHocElementXmList(pc_physelemLS, pc_petsdata->rainbuf, 
                        pc_petsdata->nrain);
   }
   
   if (pc_options->element_type == SNOW_AD_HOC_TYPE)
   {
      pc_loadAdHocElementXmList(pc_physelemLS, pc_petsdata->snowbuf, 
                        pc_petsdata->nsnow);
   }
   
   if (pc_options->element_type == TEMP_AD_HOC_TYPE)
   {
      pc_loadAdHocElementXmList(pc_physelemLS, pc_petsdata->tempbuf, 
                        pc_petsdata->ntemp);
   }
   
   if (pc_options->element_type == OTHER_AD_HOC_TYPE)
   {
      Sensitize_otherList();
      pc_OtherListCB(pc_otherelemLS, NULL, NULL);
   }
   
   else
      deSensitize_otherList();
      
   
   /* being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   
   retrieval_required = 1;  
   
   
   return;   
}
#endif

/****************************************************************************** 
   pc_TimeStepElementTypeListCB()
   callback for datatype selection such as River/Rain/Snow/Temp/Other 
   ***************************************************************************/
void pc_TimeStepElementTypeListCB(Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_pets_struct * pc_petsdata = NULL ;
   pc_petsdata = get_pc_petsdata ( ) ;
  //  pc_options_struct * pc_options = get_pc_options();
   int *positionArray = NULL;
  
   int posCount = 0;
   int position = 0;

   /* assign the datatype based on which toggle button
      is selected */
  //  XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
   char header[] = "pc_TimeStepElementTypeCB()";
   
   printf("%s  called \n", header);
   
   /*  
   XmComboBoxCallbackStruct *cbs_struct = (XmComboBoxCallbackStruct *)cbs;
   */
   
   int elementType = 0;
   
   /*Determine the selected Position and set the elementType based on that */
   XmListGetSelectedPos(pc_elementTypeLI, &positionArray, &posCount);
   if ((positionArray != NULL) && (posCount > 0))
   {
       position = positionArray[0];	
       elementType = position - 1;
       XtFree((XtPointer) positionArray);
       positionArray = NULL;
   }
   
   if (elementType == RIVER_TIME_STEP_TYPE)
   {
      XtManageChild(pc_riverStationFilterOM);	
   }
   else
   {
   	  XtUnmanageChild(pc_riverStationFilterOM);	
   }
   
  
        
   set_pcoptions_datatype ( ( int ) elementType ) ;   
   
   // load the element combobox with the types I just retrieved 
 
   pc_loadTimeStepElementXmListByElementType(elementType);
   
   // pick the first element 
   //XmListSelectPos(pc_physicalElementLI, 1, True);
   
   
   ///being in this callback function means that a setting has changed
   //   which controls the data retrieval, so set flag to indicate this.
   
   retrieval_required = 1;  
   
   printf("%s  exiting \n", header);
   
   return;   
}

/* ------------------------------------------------------------------------------------- */	
void pc_loadTimeStepElementXmListByElementType(TimeStepDataElementType elementType)
{
	
   int *timeStepElementArray = NULL;
   int elementCount = 0;
   
   timeStepElementArray = loadTimeStepElementArrayByElementType(elementType, &elementCount);
   
   pc_loadTimeStepElementCBX(timeStepElementArray, elementCount);
	
   return;
}	



/* ------------------------------------------------------------------------------------- */

void pc_AdHocElementTypeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
	
   pc_options_struct * pc_options = get_pc_options();
   pc_pets_struct * pc_petsdata = NULL ;
   pc_petsdata = get_pc_petsdata ( ) ;
   int *positionArray = NULL;
   int posCount = 0;
   int position = 0;

 
   int pointType = 0;
   
   // determine which element type was selected in the combobox.
   // Note: It is easier to get the answer from the List inside the combox
   // so, it is done that way  
   XmListGetSelectedPos(pc_elementTypeLI, &positionArray, &posCount);
   if ((positionArray != NULL) && (posCount > 0))
   {
       position = positionArray[0];	
       pointType = position - 1;
       XtFree((XtPointer) positionArray);
       positionArray = NULL;
   }
   

      
   // tell the pc_options what the type is      
   set_pcoptions_datatype ( ( int ) pointType ) ;  
   
   XmTextSetString(pc_physicalElementText, "");
   
   // based on the type, load the list  
   if (pc_options->element_type == RIVER_AD_HOC_TYPE) 
   {   
      pc_loadAdHocElementXmList(pc_physicalElementLI, pc_petsdata->riverbuf, 
                        pc_petsdata->nriver);       
   }
   
   else if (pc_options->element_type == RAIN_AD_HOC_TYPE) 
   {
      pc_loadAdHocElementXmList(pc_physicalElementLI, pc_petsdata->rainbuf, 
                        pc_petsdata->nrain);
   }
   
   else if (pc_options->element_type == SNOW_AD_HOC_TYPE)
   {
      pc_loadAdHocElementXmList(pc_physicalElementLI, pc_petsdata->snowbuf, 
                        pc_petsdata->nsnow);
   }
   
   else if (pc_options->element_type == TEMP_AD_HOC_TYPE)
   {
   	  	   	
      pc_loadAdHocElementXmList(pc_physicalElementLI, pc_petsdata->tempbuf, 
                        pc_petsdata->ntemp); 
   }
   
   else if (pc_options->element_type >= OTHER_AD_HOC_TYPE)
   {
      pc_OtherListCB(pc_physicalElementLI, NULL, NULL); 
   }
   
  // else
  //    deSensitize_otherList();
      
   
   /* being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   
   retrieval_required = 1;  
   
   
   return;   
}

#ifdef CHIP

void floodrept_fill_hsa_CBX()
{
    XmString   xmStr;
    FloodReportWorkArea	*frwa = getFrWorkArea();
    int i = 0;
    int hsaCount = 0;
    int tableEntryCount = 0;
    
    
    /* add the callback for the always-there button */   
    XtAddCallback(fr_hsaCBX, XmNselectionCallback, (XtCallbackProc) floodrept_hsa_cbxCB, NULL);
    
    /*  create the dynamic HSA buttons from what is in the database */  
    hsaCount = frwa->hsaCount;
    tableEntryCount = hsaCount + 1;
   
   
    /* add the special ALL HSAs item*/ 
    xmStr = XmStringCreateSimple(ALL_HSA_TEXT);
    XmListAddItem(fr_hsaLI, xmStr, 0); 
    XmStringFree(xmStr);
    xmStr = NULL;
    
       
    /*  create the XM Strings from the actual db-read HSAs */ 
    for (i = 0; i < hsaCount; i++)
    {
         xmStr = XmStringCreateSimple(frwa->availableHsaArray[i]);
	
	 XmListAddItem(fr_hsaLI, xmStr, 0);
	 
	 XmStringFree(xmStr);
	 xmStr = NULL;
    }
        	
     
    // select the first position and thereby invoke the callback
    
    XmListSelectPos(fr_hsaLI, 1, True);    
    XmProcessTraversal(fr_hsaLI, XmTRAVERSE_CURRENT);
      
    return;  
}
#endif

/****************************************************************************
   getPointTypeFromString - given a String - returns the type of point it is
   **************************************************************************/
int getElementTypeFromString(char * elementTypeString)   
{
	int pointType = -1;

	
    if (strcasecmp(elementTypeString, RIVER_STRING) == 0)
    {       	
    	pointType = RIVER_AD_HOC_TYPE;
    }	
    else if (strcasecmp(elementTypeString,  RAIN_STRING) == 0)
    {
        pointType = RAIN_AD_HOC_TYPE;	
    }
   
    else if (strcasecmp(elementTypeString,  SNOW_STRING) == 0)
    {
        pointType = SNOW_AD_HOC_TYPE;	
    }
    
    else if (strcasecmp(elementTypeString,  TEMP_STRING) == 0)
    {
        pointType = TEMP_AD_HOC_TYPE;	
    }
    
    else if (strcasecmp(elementTypeString,  OTHER_STRING) == 0)
    {
         pointType = OTHER_AD_HOC_TYPE;	
    }
    else
    {
         pointType = OTHER_AD_HOC_TYPE;	
    }
    
    /*
    printf("%s pointType = %d\n", "getPointTypeFromString():  ", pointType);
	*/
	
	return pointType;
	
}

/****************************************************************************
   pc_OtherListCB() - callback for otherPE list widget 
   **************************************************************************/

void pc_OtherListCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Boolean any_selected_items ;
   int cnt = 0 ;
   int pos ;
   int * poslist = NULL ;
   static int previous_position = 0 ;
   pc_options_struct * pc_options = get_pc_options();
   
   /* if the current option is not 'other', then don't bother doing anything */
   if (pc_options->element_type < OTHER_AD_HOC_TYPE)
      return;
   
   /* get the selected position in the 'other' list */
   any_selected_items = XmListGetSelectedPos ( pc_elementTypeLI , & poslist , & cnt ) ;

   if ( any_selected_items == False )
   {
      /* There are no items selected in the "other" elements list box.
         Check to see if there was a previously selected item.  If there
         was, then highlight it.  Otherwise, highlight the first element
         in the list box. */
      if ( previous_position != 0 )
      {
         fprintf ( stdout , "\nNothing selected. "
                            "The value of previous_position is %d\n" ,
                            previous_position ) ;

         pos = previous_position ;
        
         /* Highlight the item in the list. */
         XmListSelectPos ( pc_elementTypeLI , pos , False ) ;

         /* No additional processing is necessary. */
         return ;
      }
      else
      {
         previous_position = 1 ;
         pos = previous_position ;
      }
   }
   else
   {
      pos = * poslist;
      previous_position = pos ;
   }
   
   /* pos = position selected or highlighted 
      from other list widget */
      
   pos = pos - 4;    /* 4 is the offset to map 
  					 from the oldstyle "otherList"
   			   	     to the new way of doing business */
   pc_LoadOtherXmListByPos ( pos ) ; 	
   
   
   /* Being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   
   retrieval_required = 1;
   return;   
}



/***************************************************************************** 
   callbacks() for physical element list 
   **************************************************************************/

void pc_AdHocPhysElementCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   pc_options_struct * pc_options = get_pc_options();
   int	* posList = NULL;
   int	listCount = 0 ; 
   int  pos  = 0;
   pc_pets_struct * pc_petsdata = NULL ;
   char header[] = "pc_AdHocPhysElemListCB";
   
 
   pc_petsdata = get_pc_petsdata ( ) ;
    
   /* get selected position from list */
  
	/*
	printf("%s null cbs_struct-------------------------------  \n", header);
	*/
	XmListGetSelectedPos(pc_physicalElementLI, &posList, &listCount);
	if (posList != NULL)
	{
	      pos = posList[0];	
	      printf("%s null------------------------------------------------ pos = %d \n", header, pos);
	     
	      XtFree((XtPointer) posList);
	}
   	  
   
   /* initialize */
   pc_options->PCandPP = 0;
   pc_options->Primary = 0;
   
   
   /* if PE is selected from physical element list widget,
      then load the corresponding type-source list and 
      then save it as current PE. */   
   
 //  printf("%s ------------------------------------------------ pos = %d \n", header, pos);
   
 //  printf("%s before call to load_TypeSrcListPos(pos) \n", header);
  
    
   load_adhoc_typesource_list ( pos ) ;
 
  // pc_loadAdHocTypeSrcListByElementPos(pos);
 //  printf("%s ------------------------------------------------ \n", header);
            
   pc_set_selected_pe ( pc_petsdata->element_buffer[pos - 1] ,
   					pc_options->element_type ,
                    pos  ) ; 
     
   if (pc_options->Primary)
   {
   	   //deselect the filter option and make both widgets insensitive
   	   XmToggleButtonSetState(pc_typeSourceTB, False, True);
       
      // printf("pc_AdHocPhysElementCB() - calling desensitize_type_source_widgets() \n");
   	  // desensitize_type_source_widgets();
   }   
   else
   {
   	   sensitize_type_source_widgets();
   }
   
   
      
   /* being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   
   retrieval_required = 1; 
   
   return;   
}

/* ---------------------------------------------------------------------------------------*/
void pc_instPrecipSimpleSpinBoxCB(Widget w, XtPointer ptr, XtPointer cbs)
{
	 char header[] = "pc_instPrecipSimpleSpinBoxCB(): ";
	 XmSpinBoxCallbackStruct * cb_struct = (XmSpinBoxCallbackStruct *) cbs;
		  
	 int pos = cb_struct->position;
		 
	 pc_options_struct * pc_options =  get_pc_options(); 
	 pc_options->inst_precip_accum_time_selection = pos;
	 
	 
	 printf("%s ^^^^^^^^^^^^^^^^^^^^^^^^----------------------------------\n", header);
	 
	  pc_drawMapIfNotDisabled(w, True);
	 
	return;
	
	/*
	 * 
	 * typedef struct
{
    int          reason;
    XEvent       *event;
    Widget       widget;
    Boolean      doit;
    int          position;
    XmString     value;
    Boolean      crossed_boundary;
} XmSpinBoxCallbackStruct;
 */

}

/* ---------------------------------------------------------------------------------------*/
	
void pc_TimeStepElementListCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   pc_options_struct * pc_options = get_pc_options();
   int	* posList = NULL;
   int	listCount = 0 ; 
   int  pos  = 0;
   pc_pets_struct * pc_petsdata = NULL ;
   char header[] = "pc_TimeStepElementListCB";
   
   printf("%s at start \n", header);
 
   pc_petsdata = get_pc_petsdata ( ) ;
    
   /* get selected position from list */
  
	/*
	printf("%s null cbs_struct-------------------------------  \n", header);
	*/
   XmListGetSelectedPos(pc_physicalElementLI, &posList, &listCount);
   if (posList != NULL)
   {
	      pos = posList[0];	
	      printf("%s null------------------------------------------------ pos = %d \n", header, pos);
	     
	      XtFree((XtPointer) posList);
   }
   	  
   	  
    TimeStepDataElement element =  
           pc_getTimeStepElement(pc_options->element_type, pos -1 );
     
    pc_options->selectedTimeStepElement = element; 
     
    char * elementString = pc_getTimeStepElementStringFromElement(element);
    printf("%s element = %d elementString = :%s: \n", header, element, elementString);   
     
   TimeStepDataElementType elementType = (TimeStepDataElementType) pc_options->element_type;
    
    
    
   //handle the precip PC/PP filter
   // start by showing it always for Rain
   // turn it off when it is instantaneious  
   if (elementType == RAIN_TIME_STEP_TYPE)
   {
       if (element == INSTANTANEOUS_PRECIP_TSDE)
       {
           //can't filter PE for INSTANTANEOUS, always PC
           XtUnmanageChild(pc_precipPeOM); 
       }
       else //any other precip mode
       {
           XtManageChild(pc_precipPeOM);
       }
   }
   else //if not rain, then the filter is invisible
   {
       XtUnmanageChild(pc_precipPeOM);
   }
   
   
     
   // When the element is for instaneous precip, display the
   // option menu, otherwise, don't
   if (element == INSTANTANEOUS_PRECIP_TSDE)
   {   
        //can't pick the time for INSTANTANEOUS, always most current
        XtUnmanageChild(pc_timespecFO); 
        
   	    // need to pick the accumulation period
   	    XtManageChild(pc_precipHoursLA);	
   	    XtManageChild(pc_precipHoursSSB);
   }
   else //any other element for Rain or other elementType
   {
   	    XtManageChild(pc_timespecFO);
   	    XtUnmanageChild(pc_precipHoursLA);
   	    XtUnmanageChild(pc_precipHoursSSB);
   }
   
  
   
     
   /* initialize */
   pc_options->PCandPP = 0;
   pc_options->Primary = 0;
   
   
   /* if PE is selected from physical element list widget,
      then load the corresponding type-source list and 
      then save it as current PE. */   
   
 //  printf("%s ------------------------------------------------ pos = %d \n", header, pos);
   
  // printf("%s before call to pc_set_selected_pe \n", header);
  //   printf("%s ------------------------------------------------ \n", header);
   
  // printf("%s  before pc_load_timeinfo------------------------------- \n", header);
   pc_load_timeinfo();        
  // printf("%s  after  pc_load_timeinfo------------------------------- \n", header);   
      
   /* being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   pc_drawMapIfNotDisabled(w, True);
   
   printf("%s at end \n", header);
 
   
   return;   
}
// -------------------------------------------------------------------------------------------

TimeStepDataElement pc_getTimeStepElement(TimeStepDataElementType elementType, 
									  int arrayPosition)
{
	// This function depends on the values being used as the elementOffset to be the first value in 
	// That elementType's set of elements.
	
	char header[] = "pc_getTimeStepElement(): ";
	TimeStepDataElement element = 0;
	
	int elementOffset = 0;
	
	switch (elementType)
	{
	    case RIVER_TIME_STEP_TYPE:   
	       elementOffset = STAGE_POOL_TSDE;
	    
	    break;	
		
		case RAIN_TIME_STEP_TYPE:
		   elementOffset = INSTANTANEOUS_PRECIP_TSDE;
		   
		break;
		   
		case SNOW_TIME_STEP_TYPE:
		   elementOffset = SNOW_WATER_EQUIV_TSDE;
		   
		break;
		
		
		case TEMPERATURE_TIME_STEP_TYPE:
		   elementOffset = TEMPERATURE_TSDE;
		   
		break;
		
		case HUMIDITY_TIME_STEP_TYPE:
		   elementOffset = DEWPOINT_TSDE;
		   
		break;
		
		case WIND_TIME_STEP_TYPE:
		   elementOffset = WIND_SPEED_TSDE;
		   
		break;
		
		default:
		    fprintf(stderr, "%s - ERROR invalid TimeStepElementType = %d \n", header, elementType);
		 
	}	
	
	element = elementOffset + arrayPosition;
	
	
	 
	return element;
	
} 

/******************************************************************************
   pc_TypeSrcCB()
   callbacks for typesource toggle,   
   ****************************************************************************/

void pc_TypeSrcToggleCB(Widget w, XtPointer ptr, XtPointer cbs)
{  
	
   char header[] = "pc_TypeSrcToggleCB()";
   	
   printf("%s calling \n", header);
	
   pc_options_struct * pc_options = get_pc_options();
 //  char header[] = "pc_TypeSrcCB(): ";
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
   pc_pets_struct *pets_data = get_pc_petsdata();
   /* set the option */
   
   pc_options->filter_by_typesource = tb_state->set;
   
   printf("%s filter_by_typesource = %d \n", header, pc_options->filter_by_typesource);
   
   // if type source button toggled on then sensitize the type source
   //   list widget; else desensitize the type source list widget
          
   Sensitize(pc_typeSourcePB);
       
   //When no items are selected and you press the toggle, make the toggle unset again,
   // so that it does not give you the illusion of filtering   
   
   if (pc_options->query_mode == AD_HOC_MODE)
   {
       if (  pets_data->adhoc_type_source_count == 0)
       {
   	       printf("%s I am setting the type source toggle button to False and invoking this callback again \n", header);
           XmToggleButtonSetState(pc_typeSourceTB, False, True);
          // DeSensitize(pc_typeSourcePB);
       }
   }    
       
   
   // being in this callback function means that a setting has changed
   //   which controls the data retrieval, so set flag to indicate this.
     
   pc_drawMapIfNotDisabled(w, True);
  
   return;   
}

/******************************************************************************
   pc_QueryModeCB()
   callbacks for typesource toggle,   
   ****************************************************************************/

void pc_QueryModeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
	 char header[] = "pc_QueryModeCB(): ";
	XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
	pc_options_struct * pc_options = get_pc_options();
	
	pc_options->query_mode = -1; 
	
	if (w == pc_timeStepTB)
	{
		if (tb_state->set)
		{
			
		   pc_set_query_mode(TIME_STEP_MODE);

		   printf("%s TIME_STEP_MODE\n", header);
		}	
	}
	else if (w == pc_adhocTB)
	{
		if (tb_state->set)
		{
		   pc_set_query_mode(AD_HOC_MODE);
		
		   printf("%s AD_HOC_MODE\n", header);	
		}		
	}
	
	
	
	
	
}



/******************************************************************************
   pc_set_query_mode
   Handles the ramifications of setting the queryMode (AD_HOC_MODE or TIME_STEP_MODE
   ***************************************************************************/

void pc_set_query_mode(QueryMode queryMode)
{
	char header[] = "pc_set_query_mode(): ";  
	pc_options_struct * pc_options = get_pc_options();
	
    printf("%s  at start, queryMode arg = %d\n", header, queryMode);
  
    
    
	int previousQueryMode = pc_options->query_mode;
	int modeChanged = 0;
	
	
    pc_options->query_mode = queryMode;	
    
    
  //  if (DISABLED_TIME_STEP)
  //  {
 //   	pc_options->query_mode = AD_HOC_MODE;
 //   }
    
    
    if (previousQueryMode != queryMode)
    {
        modeChanged = 1;
    }
    
    
    //printf("%s  after setting pc_options->query_mode, it = %d\n", header, pc_options->query_mode);
    
    //printf("%s  before calling pc_setup_mode_related_callbacks() \n", header);
    pc_setup_mode_related_callbacks(queryMode);    
    //printf("%s  after calling pc_setup_mode_related_callbacks(), pc_options->query_mode  = %d\n", header, pc_options->query_mode);
   
    
    /* disable type source ComboBox in timestep mode */
    if (queryMode == TIME_STEP_MODE)
    {
      
        SetLabel(pc_basisLA, "River Icon Color Based On:");
      
        XtUnmanageChild(pc_hrsoldLA);
        XtUnmanageChild(pc_hrsoldTX);
        
        XtUnmanageChild(pc_valueFunctionOM);
        XtUnmanageChild(pc_rivvalsOM);    
        
        XtUnmanageChild(pc_dataSourcePB);
        XtUnmanageChild(pc_dataSourceTB);
        
        
        
   //   XtUnmanageChild(pc_timespecFO); //handled by timestepElementListCB      
   //   XtManageChild(pc_precipHoursSSB); //handled by timestepElementListCB
  //  	XtManageChild(pc_riverStationFilterOM); //handled by elementtypeListCB
            
    }
    else /* in ADHOC MODE */ 
    {
    	SetLabel(pc_basisLA, "River Color/Value Based On:");
    
        XtManageChild(pc_hrsoldLA);
        XtManageChild(pc_hrsoldTX);
        
        XtManageChild(pc_valueFunctionOM);
        XtManageChild(pc_rivvalsOM);
        
        XtManageChild(pc_dataSourcePB);
        XtManageChild(pc_dataSourceTB);
        
        XtManageChild(pc_timespecFO);
   
        XtUnmanageChild(pc_precipHoursSSB);
        XtUnmanageChild(pc_riverStationFilterOM);
    
    }
   
    
  
    if (modeChanged)
    {
       pc_loadElementTypeCBX();
       pc_setElementTypeByOppositeModeElementType(previousQueryMode, queryMode);	
    }
    
    printf("%s  at end, queryMode = %d\n", header, pc_options->query_mode);
   
}

/******************************************************/

void pc_setElementTypeByOppositeModeElementType(QueryMode previousMode, QueryMode newMode)
{
	
	pc_options_struct * pc_options = get_pc_options();
	
	
	int old_element_type = pc_options->element_type;
	int new_element_type = 0;
	
	if (previousMode == TIME_STEP_MODE)
	{
		 //Note, the element types in TimeStepDataElementType and 
		 // AdHocDataElementType need to match each other in order so
		 // that their values are equal
		 
		 if (old_element_type >=0 && old_element_type < HUMIDITY_TIME_STEP_TYPE)
		 {
		     new_element_type = old_element_type;	 	
		 }
		 else
		 {
		 	 // I am stuck, because there are no standard ad_hoc types that
		 	 // go beyond TEMPERATURE
		     new_element_type = RIVER_AD_HOC_TYPE;	
		 }
		 
	}
	else //previousMode == AD_HOC_MODE
	{
		if (old_element_type >=0 && old_element_type < OTHER_AD_HOC_TYPE)
		{
		     new_element_type = old_element_type;	 	
		}
		
	}
	
	pc_setElementType(new_element_type);
	
}


/******************************************************/
void pc_setElementType(int element_type)
{
	 printf("pc_setElementType(): element_type = %d\n", element_type);
	
	 //this will cause an invocation of the callback, which will
	 // then cause this elementType to be set in the pc_options structure
     XmListSelectPos(pc_elementTypeLI, element_type + 1, 1);	
}

/******************************************************/

/*****************************************************************************
   pc_TimeModeCB()
   toggle callback() for Latest/SetTime/Min/Max options 
   ***************************************************************************/

void pc_TimeModeCB(Widget w, XtPointer ptr, XtPointer cbs)
{      
  pc_options_struct * pc_options = get_pc_options();
   char header[] = "pc_TimeModeCB(): ";
   /* save the current option for the time mode. */
   int intValue = (int) ptr;
   
   printf("%s intValue = %d\n", header, intValue);
   
   pc_options->time_mode = intValue;
   
   
   /* being in this callback function means that a setting has changed
      which controls the data retrieval, so set flag to indicate this. */
   
   retrieval_required = 1; 
   
   
   return;   
}


/***************************************************************************
   increaseEndDayCB()
   
   being in any of these 4 callback functions means that the time has
   changed, which controls the data retrieval.  unlike the other settings
   which may be changed, for the time fields, a change is checked
   noted when the map or table is requested as there is no callback
   on the hours text field.  this means that one could change the time
   then change it right back, and the data will not be re-retreived.
   
   In time-step mode, the data is remapped automatically
   
    *************************************************************************/
void increaseEndDayCB(Widget w, XtPointer ptr, XtPointer cbs )
{ 
   pc_options_struct * pc_options = get_pc_options();
   
   read_end_pctimestr();   
   adjust_pctimestr(pc_options->pc_time_str, +1, 24*3600);   
   XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
       pc_drawMap(w, False);
   }
   
   
   return;   
}


/****************************************************************************
   decreaseEndDayCB()
   **************************************************************************/
void decreaseEndDayCB(Widget w, XtPointer ptr, XtPointer cbs )
{
   
   pc_options_struct * pc_options = get_pc_options();
   
   read_end_pctimestr();   
   adjust_pctimestr(pc_options->pc_time_str, -1, 24*3600);   
   XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
        pc_drawMap(w, False);
   }
   
   return;     
}


// --------------------------------------------------------------------------

 void arrow_key_press_handler ( Widget w , XtPointer clientdata ,
                         XEvent * event , 
                         Boolean * continue_to_dispatch_event )
 {
    
     char header[] = "arrow_key_press_handler(): ";
     static int first = 1;
     static KeyCode upKeyCode;
     static KeyCode rightKeyCode;
         
     static KeyCode downKeyCode;
     static KeyCode leftKeyCode;
     
     KeyCode pressedKeyCode;
     if (first)
     {
         first = 0;
         Display * display = XtDisplay ( w ) ;
         
         //forward in time
         upKeyCode = XKeysymToKeycode(display, XK_Up);
         rightKeyCode = XKeysymToKeycode(display, XK_Right);
      
      
         //backward in time
         downKeyCode = XKeysymToKeycode(display, XK_Down);
         leftKeyCode = XKeysymToKeycode(display, XK_Left);
     }
     
     printf("%s in function\n", header);
    
     pressedKeyCode = event->xkey.keycode ;
    
     if ((pressedKeyCode == upKeyCode) || (pressedKeyCode == rightKeyCode) )
     {
         increaseEndHourCB(pc_hourupAB, NULL, NULL);
	     *continue_to_dispatch_event = False;
     }
     else if ((pressedKeyCode == downKeyCode) || (pressedKeyCode == leftKeyCode) )
     {
         decreaseEndHourCB(pc_hourdownAB, NULL, NULL); 
	     *continue_to_dispatch_event = False;
     }
     
     return;
 
  }
 

/*****************************************************************************
   increaseEndHourCB()
   **************************************************************************/
void increaseEndHourCB (Widget w, XtPointer ptr, XtPointer cbs )
{
   pc_options_struct * pc_options = get_pc_options();
   
   
   int adjustment_hours = getAdjustmentHours(pc_options);
   read_end_pctimestr();   
   adjust_pctimestr(pc_options->pc_time_str, 1, adjustment_hours * 3600);   
   XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
   	    pc_drawMap(w, False);
   }
   
   return;   
}


/******************************************************************************
   decreaseEndHourCB()
   ***************************************************************************/
void decreaseEndHourCB(Widget w, XtPointer ptr, XtPointer cbs )
{
   pc_options_struct * pc_options = get_pc_options();
   
   int adjustment_hours = getAdjustmentHours(pc_options);
   read_end_pctimestr();   
   adjust_pctimestr(pc_options->pc_time_str, -1, adjustment_hours * 3600);   
   XmTextSetString(pc_timespecTX, pc_options->pc_time_str);
   
   if (pc_options->query_mode == TIME_STEP_MODE)
   {
   	    pc_drawMap(w, False);
   }
   
   return;   
}


int getAdjustmentHours(pc_options_struct * pc_options)
{
	int adjustment_hours = 1;
	
	if (pc_options->query_mode == TIME_STEP_MODE)
	{
		
		TimeStepDataElement element = pc_options->selectedTimeStepElement;	
			
	    if (element == THREE_HOUR_PRECIP_TSDE)
        {
	        adjustment_hours = 3;	
	    }		
	    else if (element == SIX_HOUR_PRECIP_TSDE)
        {
	        adjustment_hours = 6;	
	    }
	    else if (element == DAILY_PRECIP_TSDE)
	    {
	    	adjustment_hours = 24;		
  	    }
        else if (element ==  TEMP_MAX_TSDE)
        {
            adjustment_hours = 24;      
        }
        else if (element == TEMP_MIN_TSDE)
        {
            adjustment_hours = 24;      
        }
  	    // has no meaning for INSTANTANEOUS
  	    // same answer for hourly as default 
	}

	
	return adjustment_hours;
	
}


/* ========================================================================
   ============== End of callbacks controlling retrieval ==================
   ========================================================================*/

/* ========================================================================
   ========== Beginning of callbacks for display options ==================
   ========================================================================*/

// --------------------------------------------------------------------------------------
void pc_riverStationFilterCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	RiverStationFilter filterMode = (RiverStationFilter) ptr;
	pc_options_struct * pc_options = get_pc_options();
	
	//printf("pc_riverStationCB: \n");
	pc_options->river_station_filter = filterMode;
	
	
	pc_drawMapIfNotDisabled(w, False);
}

// --------------------------------------------------------------------------------------

void pc_precipPeFilterCB (Widget w, XtPointer ptr, XtPointer cbs)
{
    PrecipPeFilter filterMode = (PrecipPeFilter) ptr;
    pc_options_struct * pc_options = get_pc_options();
    
    //printf("pc_precipPeFilterCB: \n");
    pc_options->precip_pe_filter = filterMode;
    
    printf("pc_precipPeFilterCB: value = %d \n", pc_options->precip_pe_filter);
    
    pc_drawMapIfNotDisabled(w, False);
    
}

// --------------------------------------------------------------------------------------

void pc_valueFilterOperationCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	FilterOperation operation = (FilterOperation) ptr;
	pc_options_struct * pc_options = get_pc_options();
	
	printf("pc_valueFilterOperationCB: operation = %d\n", operation);
	pc_options->valueFilterOperation = operation;
	
	
	enableDisableValueFilterText(pc_options->valueFilterOperation, pc_filterByValueTF);
	    
   if (pc_options->valueFilterOperation == SHOW_ALL)
   {
      DeSensitize(pc_filterByValueTF); 	
   }  
   else
   {
      Sensitize(pc_filterByValueTF);	
   }  
   
	
	pc_drawMapIfNotDisabled(w, False);
}

void enableDisableValueFilterText(FilterOperation filterOperation, Widget textField)
{
	if (filterOperation == SHOW_ALL)
   {
      DeSensitize(textField); 	
   }  
   else
   {
      Sensitize(textField);	
   }  
	
}


void pc_elevFilterOperationCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_options_struct * pc_options = get_pc_options();
	FilterOperation operation = (FilterOperation) ptr;
	
	printf("pc_elevFilterOperationCB: operation = %d\n", operation);
	pc_options->elevFilterOperation = operation;
	
	
	enableDisableValueFilterText(pc_options->elevFilterOperation,
								 pc_filterByElevTF);
	
	pc_drawMapIfNotDisabled(w, False);
}


void pc_valueTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_options_struct * pc_options = get_pc_options();
	pc_options->valueFilterValue = filterTextAndGetDecimal(w);
	
	pc_drawMapIfNotDisabled(w, False);
	
}


void pc_elevTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_options_struct * pc_options = get_pc_options();
	pc_options->elevFilterValue = filterTextAndGetDecimal(w);

	pc_drawMapIfNotDisabled(w, False);

}

void pc_hoursTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_options_struct * pc_options = get_pc_options();
	pc_options->dur_hours = (int) filterTextAndGetLong(w);
	
}


/******************************************************************************/
long filterTextAndGetLong(Widget textField)
{
	char * valueText = NULL;
	char * newValueText = NULL;
	
    valueText = XmTextGetString(textField);
    newValueText = removeNonDigits(valueText);	
	
	long value = atoi(valueText);	
    
    if (strcmp(valueText, newValueText) != 0)
	{
	    XmTextSetString(textField, newValueText);	
	}	
    
    if (valueText != NULL)
	{
	   XtFree(valueText);	
	}
	
	if (newValueText != NULL)
    {
       free(newValueText);
    }
    
    return value;		
}

/******************************************************************************/
double filterTextAndGetDecimal(Widget textField)
{
	char * valueText = NULL;
	char * newValueText = NULL;
	
    valueText = XmTextGetString(textField);
	
    newValueText = removeNonNumericChars(valueText);	
	double value = atof(newValueText);	
   
	/*
	  If the strings are different, change the contents to the new string
	 */
	if (strcmp(valueText, newValueText) != 0)
	{
	    XmTextSetString(textField, newValueText);	
	}	
	
	/* free the strings */
	if (valueText != NULL)
	{
	   XtFree(valueText);	
	}
	
    if (newValueText != NULL)
    {
       free(newValueText);
    }
    
    return value;	
}
/******************************************************************************/
char * removeNonDigits(char * text)
{
    char * newText = NULL;
    char c;
    
    int length = strlen(text) + 1;
    newText = (char *) malloc(length);
    
    memset(newText, '\0', length); 
    
    int i = 0;
    int j = 0;
    
    if (newText != NULL)
    {
	    for (i = 0; text[i]; i++)
	    {
	    	c = text[i];
	    	
	        if (isNumeric(c))
	        {
	            newText[j] = c;
	            j++;
	        }	  
	    }
    }
    
    if (strcmp(text, newText) != 0)
    {
        printf("------changed--------------------------------text = %s  newText = %s \n", text, newText);	
    }
    else
	{
		printf("------same-----------------------------------text = %s  newText = %s \n", text, newText);
	}
    
    return newText;
		
}

/******************************************************************************/
char * removeNonNumericChars(char * text)
{
    char * newText = NULL;
    char c;
    
    int length = strlen(text) + 1;
    newText = (char *) malloc(length);
    
    memset(newText, '\0', length); 
    
    int i = 0;
    int j = 0;
    int dotCount = 0;
    
    if (newText != NULL)
    {
	    for (i = 0; text[i]; i++)
	    {
	    	c = text[i];
	    	
	        if (isNumeric(c))
	        {
	            newText[j] = c;
	            j++;
	        }	
	        else if (c == '-')
	        {
	        	if (j == 0)  /* first character in newText can be a sign*/
	        	{
	        		newText[j] = c;
	        		j++;
	        	}
	        }
	        else if (c == '.')
	        {
	            if (dotCount == 0)
	            {
	               newText[j] = c;
	               j++;
	               dotCount++;	
	            }
	        }
	        
	        
	    }
    }
    
    if (strcmp(text, newText) != 0)
    {
       // printf("------changed--------------------------------text = %s  newText = %s \n", text, newText);	
    }
    else
	{
		//printf("------same-----------------------------------text = %s  newText = %s \n", text, newText);
	}
    
    return newText;
	
}


/******************************************************************************
   toggle callbacks() for Data Source/ Misssing/ Zeros options 
   ***************************************************************************/
void pc_dataSourceToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
   pc_options_struct * pc_options = get_pc_options();
     
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
       
   pc_options->filter_by_datasource       = tb_state->set; 

   pc_drawMapIfNotDisabled(w, False);
   
   return;
}

void pc_showMissingToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
   pc_options_struct * pc_options = get_pc_options();
     
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
       
       
   //button is now labelled show_missing    
   pc_options->suppress_missing = reversePolarity(tb_state->set); 


   pc_drawMapIfNotDisabled(w, False);
   
   return;
}


void pc_showForecastPointsToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
   pc_options_struct * pc_options = get_pc_options();
     
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
           
   //button is now labelled showNonFcstPoints
   pc_options->fcstpts_only = reversePolarity(tb_state->set);

   pc_drawMapIfNotDisabled(w, False);
   
   return;
}


void pc_ToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
	pc_options_struct * pc_options = get_pc_options();
   Widget button;
   
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
   
   button = (Widget) ptr;
   
   /* save data source selected options for
      Data Source/Missing/Zeros/Fore Cast Points.
      Corresponding values are [0,1,2,3,4]  */
   
   if (button == pc_dataSourceTB)
      pc_options->filter_by_datasource       = tb_state->set; 

   else if (button == pc_showMissingTB)
      pc_options->suppress_missing = reversePolarity(tb_state->set); //button is now labelled show_missing
 /*  
   else if (button == 2)
      pc_options->suppress_zeros   = tb_state->set;
 */  
   else if (button == pc_showNonFcstPntsTB)
      pc_options->fcstpts_only     = reversePolarity(tb_state->set);//button is now labelled showNonFcstPoints
   
   pc_drawMapIfNotDisabled(w, False);
   
   return;
}

/******************************************************************************
   Change a True To False and False to True 
   ***************************************************************************/

int reversePolarity(int booleanValue)
{
	 int returnValue = 0;
	 if (booleanValue)
	 {
	     returnValue = 0;
	 }
	 else
	 {
	     returnValue = 1;	
	 }
	 
	 return returnValue;
	
}

/******************************************************************************
   callback for service backup toggle
   ***************************************************************************/

void pc_hsaToggleCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
   pc_options_struct * pc_options = get_pc_options();
   XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;
      
   pc_options->filter_by_hsa = tb_state->set; 
   
   pc_drawMapIfNotDisabled(w, False);
   
   return;
}


/******************************************************************************
   callback for service backup selections 
   ***************************************************************************/

void pc_hsaButtonCB (Widget w, XtPointer ptr, XtPointer cbs)
{
   
   /* display the window allowing the user control of the filter
      by service backup */
   
   show_serv_bkup(w);
   
   return;
}

/***********************************************************************************/


void pc_AdHocDataSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs)
{
    /*
     * This function is invoked as a callback, which then
     * creates an ItemChooser and even sets up ITS own callback for its apply button.
     * This function translates between the application information and the information
     * known to the ItemChooser Dialog
     * 
     * */
    
    pc_options_struct * pc_options = get_pc_options();
    char header[] = "pc_AdHocDataSourceItemChooserCB(): ";
    int i = 0;
    int j = 0;
    LongText * itemStringArray = NULL;
    int *selectedPositionsArray;
    int itemCount = 0;
    int allowMultipleSelection = 1;
   
    printf("%s starting.\n", header);
   
    pc_pets_struct * pc_petsdata = NULL ;
    pc_petsdata = get_pc_petsdata ( ) ;    
    
    /* cause the data source list to be loaded from the database */
    //itemCount = get_AdHocAndTimeStepDataSourceLists ( ) ; 
    itemCount = get_AdHocDataSourceList();
        
    itemStringArray = (LongText *) malloc(sizeof(LongText) * itemCount);
    if (itemStringArray == NULL)
    { 
        fprintf(stderr, "ERROR -- malloc failure in %s  for LongText itemStringArray\n", header);
        return; 
    }

    printf("%s before item loop\n", header);
  
    /* copy the item string array, so that it can be safely passed to the "show" function */
    for (i = 0; i < itemCount; i++)
    {
        memset(itemStringArray[i], '\0', BUFSIZ);
        strncpy(itemStringArray[i],pc_petsdata->adhoc_datasrcbuf[i],  BUFSIZ);
        printf("%s itemStringArray[%d] = %s\n", header, i,  itemStringArray[i]);
    }
    
   printf("%s after item loop\n", header);
    
    
    /* allocate the selectedPositionsArray */
    selectedPositionsArray = (int *) malloc(sizeof(int) * itemCount);
        
    if (selectedPositionsArray == NULL)
    {
        fprintf(stderr, "ERROR -- malloc failure in %s for selectedPositionsArray  \n", header);
        return; 
    }
    
    /* init selected Positions array based on the pc_options->sources_chosen  */
    
    for (i = 0; i < itemCount; i++)
    {
        selectedPositionsArray[i] = False;
        for (j = 0; j < pc_options->data_sources_chosen_count; j++)
        {
            if (strcmp(itemStringArray[i], pc_options->data_sources_chosen_array[j]) == 0)
            {
                selectedPositionsArray[i] = True;
                printf("%s %s is selected BEFORE the dialog has been shown\n", header, itemStringArray[i]);
                break;  
            }       
        }   
    }
       
    show_item_chooser(w,
                      "Data Source",
                      itemStringArray, 
                      selectedPositionsArray,
                      itemCount,
                      allowMultipleSelection,
                      pc_AdHocDataSourceApplyCB,
                      pc_AdHocDataSourceCloseCB);

  
  
    if (itemStringArray != NULL)
    {
        free(itemStringArray);    
    }
                                                                          
    return;
}


/***********************************************************************************/

void pc_AdHocTypeSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs)
{
    /*
     * This function is invoked as a callback, which then
     * creates an ItemChooser and even sets up ITS own callback for its apply button.
     * This function translates between the application information and the information
     * known to the ItemChooser Dialog
     * 
     * */
     
    pc_options_struct * pc_options = get_pc_options();
     
    char header[] = "pc_AdHocTypeSourceItemChooserCB(): ";
    int i = 0;
    int j = 0;
    LongText  * itemStringArray = NULL;
    int *selectedPositionsArray = NULL;
    int itemCount = 0;
    int allowMultipleSelection = 1;
     
    pc_pets_struct * pc_petsdata = NULL ;
    pc_petsdata = get_pc_petsdata ( ) ;    
    
    
    printf("%s inside\n", header);
      
    /* cause the type source list to be loaded from the database */
    itemCount = pc_petsdata->adhoc_type_source_count;
    
    itemStringArray = (LongText *) malloc(sizeof(LongText) * itemCount);
    if (itemStringArray == NULL)
    { 
        fprintf(stderr, "ERROR -- malloc failure in %s for LongText itemStringArray\n", header);
        return; 
    }
  
    
    printf(" ^^^^^^^^^^^^^^^^^^^^^^^^^%s pc_petsdata->adhoc_type_source_count = %d\n",
            header, pc_petsdata->adhoc_type_source_count);
 
    /* copy the item string array, so that it can be safely passed to the "show" function */
    for (i = 0; i < itemCount; i++)
    {
        memset(itemStringArray[i], '\0', BUFSIZ );
        strncpy(itemStringArray[i],pc_petsdata->adhoc_typeSourceBuffer[i], BUFSIZ);
   //     printf("%s itemStringArray[%d] = %s\n", header, i,  itemStringArray[i]);
    }
    
    /* allocate the selectedPositionsArray */
    selectedPositionsArray = (int *) malloc(sizeof(int) * itemCount);
        
    if (selectedPositionsArray == NULL)
    {
        fprintf(stderr, "ERROR -- malloc failure in %s() \n", header);
        return; 
    }
    
    /* init selected Positions array based on the pc_options->sources_chosen  */
    
    /* for each Item in the list to be displayed */
    for (i = 0; i < itemCount; i++)
    {
        selectedPositionsArray[i] = False;
        
        /*  compare the item against each of the items that have been selected */
        for (j = 0; j < pc_options->type_source_chosen_count; j++)
        {
        //  printf("%s pc_options->type_source_chosen_array[%d] = %s \n", header, j, pc_options->type_source_chosen_array[j]);
        //  printf("%s itemStringArray = %s \n", header, itemStringArray[i]);
            if (strcmp(itemStringArray[i], pc_options->type_source_chosen_array[j]) == 0)
            {
                selectedPositionsArray[i] = True;
      //          printf("%s  %s is selected BEFORE the dialog has been shown\n", header, itemStringArray[i]);
                break;  
            }       
        }   
    }
    
    show_item_chooser(w,
                      "Type/Source Selection Dialog",
                      itemStringArray, 
                      selectedPositionsArray,
                      itemCount,
                      allowMultipleSelection,
                      pc_TypeSourceApplyCB,
                      pc_TypeSourceCloseCB);
                      
    if (itemStringArray != NULL)
    {
        free(itemStringArray);    
    }

                                                                      
    return;
}



/***********************************************************************************/


void pc_TimeStepTypeSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs)
{
    /*
     * This function is invoked as a callback, which then
     * creates an ItemChooser and even sets up ITS own callback for its apply button.
     * This function translates between the application information and the information
     * known to the ItemChooser Dialog
     * 
     * */
    char header[] = "pc_TimeStepTypeSourceItemChooserCB(): ";
   
    
    pc_options_struct * pc_options = get_pc_options();
    int i = 0;
    int j = 0;
    LongText * itemStringArray = NULL;
    int *selectedPositionsArray;
    int itemCount = 0;
    int allowMultipleSelection = 1;
   
   
    pc_pets_struct * pc_petsdata = NULL ;
    pc_petsdata = get_pc_petsdata ( ) ;    
    
     
    /* cause the data source list to be loaded from the database */
    // get_AdHocAndTimeStepDataSourceLists ( ) ; //old way that Chip realized was dumb
    int dummyCount =  load_TimeStepTypeSourceArray();  
     
    itemCount = pc_petsdata->timestep_type_source_count;
    
    if (dummyCount != itemCount)
    {
        printf("%s dummyCount = %d and itemCount = %d - WHY? \n", header, dummyCount, itemCount);    
    }
    
    itemStringArray = (LongText *) malloc(sizeof(LongText) * itemCount);
    if (itemStringArray == NULL)
    { 
        fprintf(stderr, "ERROR -- malloc failure in %s for LongText itemStringArray\n", header);
        return; 
    }
  
    printf("^^^^^^^^^^^^%s itemCount = %d \n", header, itemCount);
 
    
    /* copy the item string array, so that it can be safely passed to the "show" function */
    for (i = 0; i < itemCount; i++)
    {
        memset(itemStringArray[i], '\0', BUFSIZ );
        strncpy(itemStringArray[i],pc_petsdata->timestep_typeSourceBuffer[i], BUFSIZ);
        printf("%s itemStringArray[%d] = %s\n", header,  i,  itemStringArray[i]);
    }
    
    /* allocate the selectedPositionsArray */
    selectedPositionsArray = (int *) malloc(sizeof(int) * itemCount);
        
    if (selectedPositionsArray == NULL)
    {
        fprintf(stderr, "ERROR -- malloc failure in %s for selectedPositionArray \n", header);
        return; 
    }
    
    /* init selected Positions array based on the pc_options->sources_chosen  */
    
    for (i = 0; i < itemCount; i++)
    {
        selectedPositionsArray[i] = False;
        for (j = 0; j < pc_options->type_source_chosen_count; j++)
        {
            printf("%s itemStringArray[%d] = :%s:  pc_options->type_source_chosen_array[%d] = :%s: \n", 
            header, i, itemStringArray[i], j, pc_options->type_source_chosen_array[j] );
            
            if (strcmp(itemStringArray[i], pc_options->type_source_chosen_array[j]) == 0)
            {
                selectedPositionsArray[i] = True;
                printf("%s %s is selected BEFORE the dialog has been shown\n", header, itemStringArray[i]);
                break;  
            }       
        }   
    }
    
    show_item_chooser(w,
                      "Reporting Type/Source Dialog",
                      itemStringArray, 
                      selectedPositionsArray,
                      itemCount,
                      allowMultipleSelection,
                      pc_TimeStepTypeSourceApplyCB,
                      pc_TimeStepTypeSourceCloseCB);


 
    if (itemStringArray != NULL)
    {
        free(itemStringArray);    
    }

                                                                          
    return;
}



/***********************************************************************************/

void pc_AdHocDataSourceCloseCB(int * selectedPositionArray)
{
	 if (selectedPositionArray != NULL)
     {
   	     free(selectedPositionArray);
   	     selectedPositionArray = NULL;
     }	
     
     return;
}

/***********************************************************************************/
void pc_AdHocDataSourceApplyCB(int * selectedPositionArray, int itemCount)
{
	pc_options_struct * pc_options = get_pc_options();
	int i = 0;
    int j = 0;	
   
    pc_pets_struct * pc_petsdata = NULL ;

    pc_petsdata = get_pc_petsdata ( ) ;    
    
    /* cause the data source list to be loaded from the database */
    itemCount = get_AdHocDataSourceList( ) ; 
 /*
   * If an item is selected, reflect that in the pc_options structure
   */

    j = 0;
    for (i = 0; i < itemCount; i++)
    {
        printf("pc_DataSourceApplyCB() after the dialog: selectedPositionArray[%d] = %d ", i, selectedPositionArray[i]);
   	
 	    if (selectedPositionArray[i])
 	    {
            memset(pc_options->data_sources_chosen_array[j], 0, sizeof(DataSource));
            strncpy(pc_options->data_sources_chosen_array[j],
                    pc_petsdata->adhoc_datasrcbuf[i],
                     sizeof(DataSource) - 1);
          
            printf("pc_DataSourceApplyCB() after the dialog: pc_options->sources_chosen[%d] = :%s: is selected AFTER the dialog has been  close\n",
                i, pc_options->data_sources_chosen_array[j]);
                
            j++;
 	     }
 	       
     }
     pc_options->data_sources_chosen_count = j;
  	
     pc_drawMap( pointcontrolDS , False);
  		
     return;
}


/***********************************************************************************/
void pc_TimeStepTypeSourceCloseCB(int * selectedPositionArray)
{
	 if (selectedPositionArray != NULL)
     {
   	     free(selectedPositionArray);
   	     selectedPositionArray = NULL;
     }	
     
     return;
}
/***********************************************************************************/
void pc_TimeStepTypeSourceApplyCB(int * selectedPositionArray, int itemCount)
{
	
	pc_options_struct * pc_options = get_pc_options();
	int i = 0;
    int j = 0;	
   
    pc_pets_struct * pc_petsdata = NULL ;

    pc_petsdata = get_pc_petsdata ( ) ;    
    
   
 /*
   * If an item is selected, reflect that in the pc_options structure
   */

    j = 0;
    for (i = 0; i < itemCount; i++)
    {
        printf("pc_TimeStepTypeSourceApplyCB() after the dialog: selectedPositionArray[%d] = %d \n", i, selectedPositionArray[i]);
   	
 	    if (selectedPositionArray[i])
 	    {
            memset(pc_options->type_source_chosen_array[j], 0, sizeof(TypeSourceString));
            strcpy(pc_options->type_source_chosen_array[j], pc_petsdata->timestep_typeSourceBuffer[i]);
          
            printf("pc_TimeStepTypeSourceApplyCB() after the dialog: pc_options->sources_chosen[%d] = :%s: is selected AFTER the dialog has been applied\n",
                i, pc_options->type_source_chosen_array[j]);
                
            j++;
 	     }
 	       
     }
     pc_options->type_source_chosen_count = j;
 
     pc_drawMap(pc_typeSourcePB, False);
	
     return;
}

/****************************************************************************************************/
void pc_TypeSourceCloseCB(int * selectedPositionArray)
{
	 if (selectedPositionArray != NULL)
     {
   	     free(selectedPositionArray);
   	     selectedPositionArray = NULL;
     }	
     
     return;
}

/****************************************************************************************************/

void pc_TypeSourceApplyCB(int * selectedPositionArray, int itemCount)
{
	// char header[] = "pc_TypeSourceApplyCB(): ";
	 int i = 0;
     int j = 0;	
     pc_options_struct * pc_options = get_pc_options();
     pc_pets_struct * pc_petsdata = get_pc_petsdata ( ) ;
    
    /*
    * If an item is selected, reflect that in the pc_options structure
    */

     j = 0;
     for (i = 0; i < itemCount; i++)
     { 	
 	     if (selectedPositionArray[i])
 	     {
             memset(pc_options->type_source_chosen_array[j], '\0', sizeof(TypeSourceString));
             strncpy(pc_options->type_source_chosen_array[j],
                        pc_petsdata->adhoc_typeSourceBuffer[i],
                        sizeof(TypeSourceString)-1);            
             j++;
 	     }
 	       
     }
     pc_options->type_source_chosen_count = j;
   
	 pc_drawMap( pointcontrolDS, True);
	
     return;
}


/******************************************************************************
   toggle callbacks() for Value/Time/Icon/Id/Name/FloodLevel 
   ***************************************************************************/

void pc_ShowCB (Widget w, XtPointer ptr, XtPointer cbs)
{   
   Widget button = NULL;
   pc_options_struct * pc_options = get_pc_options();
   
   XmToggleButtonCallbackStruct * tb_state = 
                                ( XmToggleButtonCallbackStruct * ) cbs ;
   
   button = (Widget) ptr;
 //  char header[] = "pc_ShowCB";
   
   /* save options under Show such as Value/Time/Icon/Id/Name/Flood Level
      value = 1 if toggle is set else value = 0 */
   
   if (button == pc_valueTB)
   {
      pc_options->value = tb_state->set;
     // printf("%s value \n", header);
   }
   
   else if (button == pc_iconTB)
   {
      pc_options->icon  = tb_state->set;
     // printf("%s icon \n", header);
   }
   else if (button == pc_idTB)
   {
      pc_options->id    = tb_state->set;
     // printf("%s id \n", header);
   }
   else if (button == pc_nameTB)
   {
      pc_options->name  = tb_state->set;
      //printf("%s name \n", header);
   }
    /* New addition as of 3/20/2003 - Monitor the state of the
      display riverstatus toggle button. Bryon L. */
   else if (button == pc_colorRiverIconTB)
   {
      pc_options->riverstatus = tb_state->set ;
       // printf("%s riverstatus \n", header);
   }
   /*
    * radio buttons
    * */
   else if (button == pc_timeTB)
   {
   	  if (pc_options->time)
   	  {
   	      XmToggleButtonSetState(pc_timeTB, False, True); 	
   	  }
      pc_options->time  = tb_state->set;
      //printf("%s time \n", header);
   }
      
   else if (button == pc_elevTB)
   {
      if (pc_options->elevation)
   	  {
   	      XmToggleButtonSetState(pc_elevTB, False, True); 	
   	  }
   	  pc_options->elevation = tb_state->set;
     //printf("%s elevation \n", header);
   }
  
   else if (button == pc_paramCodeTB)
   {
   	  if (pc_options->paramCode)
   	  {
   	      XmToggleButtonSetState(pc_paramCodeTB, False, True); 	
   	  }
      pc_options->paramCode  = tb_state->set;
    //  printf("%s paramCode \n", header);
   }   

   pc_drawMapIfNotDisabled(w, False);
  
   return;   
}


/******************************************************************************
   pc_StageShowCB() 
   ***************************************************************************/
void pc_StageDataToDisplayCB (Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget button = NULL;
   pc_options_struct * pc_options = get_pc_options();
   
   button = (Widget) ptr;
   
   /* save the settings for which data value(s) to display.
      0 = value only
      1 = value and flood level
      2 = value and derived stage/flow
      3 = departure
      4 = departure and flood level */
   
   if (button == pc_valPB)
   {
      pc_options->valuetype = TYPE_VALUE; 
      pc_options->fldlevel  = 0;
      pc_options->derive_stage_flow = 0;
   }
   
   else if (button == pc_val_fldPB)
   {
      pc_options->valuetype = TYPE_VALUE; 
      pc_options->fldlevel  = 1;
      pc_options->derive_stage_flow = 0;
   }
   
   else if ( button == pc_val_stageflowPB )
   {
      pc_options->valuetype = TYPE_VALUE ;
      pc_options->fldlevel  = 0;
      pc_options->derive_stage_flow = 1;

   }
   
   else if (button == pc_departPB)
   {
      pc_options->valuetype = TYPE_DEPART; 
      pc_options->fldlevel  = 0;
      pc_options->derive_stage_flow = 0;
   }
   
   else if (button == pc_depart_fldPB)
   {
      pc_options->valuetype = TYPE_DEPART; 
      pc_options->fldlevel  = 1; 
      pc_options->derive_stage_flow = 0;
   }
   
   
   pc_drawMapIfNotDisabled(w, True);
   
   return;
}


/******************************************************************************
   pc_StageBasisCB() 
   Save value for which type of stage value to use
   0 = obs
   1 = fcst
   2 = max of obs and fcst.
   note that symbolic constants are used elsewhere to 
   determine the meanings of these values. 
   ***************************************************************************/
void pc_StageBasisCB (Widget w, XtPointer ptr, XtPointer cbs)
{
	pc_options_struct * pc_options = get_pc_options();
   int button;
   
   button = (int) ptr;
   pc_options->stagebasis = button;
 
   pc_drawMapIfNotDisabled(w, True);
 
   return;
}

/* ========================================================================
   ============== End of callbacks for display options   ==================
   ========================================================================*/

/*****************************************************************************
   pc_LoadXmOtherListPos()
   load physical element list according to data type 
   that is selected from "other" list widget 
   **************************************************************************/

void pc_LoadOtherXmListByPos ( int pos )
{
   
   int		count;   
   pc_pets_struct * pc_petsdata = NULL ;

   pc_petsdata = get_pc_petsdata ( ) ;

   /* Retrieve a count of the "Other" PEs to load into the 
      Physical Element List Box. This routine automatically
      loads the other array in the pc_petsdata structure. */
   count = count_OtherTypes ( pos ) ; 

   /* load pe into physical element list widget
      according to datatype selected from other list widget */
   
   
   pc_loadAdHocElementXmList(pc_physicalElementLI, pc_petsdata->otherbuf, count);
   /* REMOVED FOR OB7
    * pc_LoadXmElemList(pc_physelemLS, pc_petsdata->otherbuf, kount);
   */
   return;
   
}


/****************************************************************************** 
   read_end_pctimestr()
   ***************************************************************************/
void read_end_pctimestr()
{
   pc_options_struct * pc_options = get_pc_options();
   char *str;
   
   str = XmTextGetString(pc_timespecTX);
   strcpy(pc_options->pc_time_str, str);
   XtFree(str);
   
   return;
}

/******************************************************************************
   load data into physical element list in general 
   The textbuf argument contains a list of the those PE-TS combinations
   for the given datatype (e.g. River, Rain, etc.)
   ***************************************************************************/

/*  ********************************************************************** */

void pc_loadAdHocElementXmList(Widget w, PhysTypeSrcText *textbuf, int nitems)
{
   XmStringTable	xmStr = NULL ;
   int          	i ;
   int			selected_pos ;
   pc_pets_struct * pc_petsdata = NULL ;
 //  char header[] = "pc_loadAdHocElementXmList";


   pc_options_struct * pc_options = get_pc_options();
  
   pc_petsdata = get_pc_petsdata ( ) ;
   
  
   /* Load the element list in the pc_petsdata structure. */
   load_physicalelement_list ( textbuf , nitems ) ;

   /* clear the physical element list and type source list widgets,
      which depends on the selected PE item. */
   XmListDeleteAllItems ( w ) ;
  
   
   /* load unique pe into physical element list widget and also
      append name field from ShefPe table that matched. */
   xmStr = (XmStringTable) XtMalloc ( pc_petsdata->element_count *
                                      sizeof(XmString * ) ) ;


   for ( i = 0 ; i < pc_petsdata->element_count ; ++ i )
   {
      xmStr [ i ] = XmStringCreateSimple ( pc_petsdata->element_buffer [ i ] ) ;
   }
   
   /* put the items in the list */
   XmListAddItems ( w , xmStr , pc_petsdata->element_count , 1 ) ;
   
   /* loop on the items placed in the list, and find the
      position of the currently selected item */
   
   selected_pos = 1;

 
   for ( i = 0 ; i < pc_petsdata->element_count ; ++ i )
   {
      if ( strncmp ( pc_petsdata->element_buffer[i] , pc_options->selectedAdHocElementString , 2 ) == 0 )
      {
		 selected_pos = i + 1;
		 break;
      }      	  
   }
   
 
   
   /* select the position in the physical element list;
      its callback results in selecting from the 
      type-source list*/
   
   XmListSelectPos ( w , selected_pos , True ) ;     
   
   for ( i = 0 ; i < pc_petsdata->element_count ; ++ i )
   {
      XmStringFree ( xmStr [ i ] ) ;
   }
   
   if ( xmStr != NULL )
   {
      XtFree ( ( char * ) xmStr ) ;
      xmStr = NULL ;
   }
   
   
   return ;
}


/*  ********************************************************************** */


void pc_loadTimeStepElementCBX(int * elementArray, int elementCount)
{
	
// Note elementArray maps from numbers like 0 to 4 to the actual positions
// in the enumerated type	
   XmStringTable	xmStr = NULL ;
   int          	i ;
   int			selected_pos ;
   pc_pets_struct * pc_petsdata = NULL ;
 //  char header[] = "pc_loadTimeStepElementCBX";
   char *elementString = NULL;
   pc_options_struct * pc_options = get_pc_options();

   //printf("%s start \n", header);
  
   pc_petsdata = get_pc_petsdata ( );
   
 
  /* clear the physical element list and type source list widgets,
      which depends on the selected PE item. */
   XmListDeleteAllItems(pc_physicalElementLI);
  
   
   xmStr = (XmStringTable) XtMalloc ( elementCount *
                                      sizeof(XmString * ) ) ;


   for ( i = 0 ; i < elementCount ; ++ i )
   {
   	  elementString = pc_getTimeStepElementStringFromElement(elementArray[i]);
   //  printf("%s elementString = %s\n", header, elementString);
      xmStr [ i ] = XmStringCreateSimple ( elementString ) ;
   }
   
   /* put the items in the list */
   XmListAddItems ( pc_physicalElementLI , xmStr , elementCount , 1 ) ;
   
   /* loop on the items placed in the list, and find the
      position of the currently selected item */
   
 //  selected_pos =  pc_options->selectedTimeStepElement + 1;

   char  * selectedString =
        pc_getTimeStepElementStringFromElement(pc_options->selectedTimeStepElement);
 
  // printf("%s selectedString = %s\n", header, selectedString );
 
   selected_pos = -1;
   for ( i = 0 ; i < elementCount ; ++ i )
   {
   	
   	  elementString = pc_getTimeStepElementStringFromElement(elementArray[i]);
   	  
   	  //printf("%s selectedString = %s\n", header, selectedString );
   	
      if ( strncmp ( selectedString, 
                     elementString ,
                     BUFSIZ) == 0 )
      {
		 selected_pos = i + 1;
		// printf("%s selected_pos = %d\n", header, selected_pos );
		// printf("%s selectedTimeStepElement index = %d\n",
		//          header, pc_options->selectedTimeStepElement );
		 break;
      }      	  
   }
   
   
   /*
    * make sure that the alleged selected_pos is at least 1
    * */
    
   if (selected_pos <= 0)
   {
   	 // printf("%s corrected selected_pos to 1 \n", header);
   	  selected_pos = 1;
   }
   
   /* select the position in the physical element list;
      its callback results in selecting from the 
      type-source list*/
   
   XmListSelectPos ( pc_physicalElementLI , selected_pos , True ) ;     
 //  printf("%s selected_pos = %d\n", header, selected_pos );
   
   for ( i = 0 ; i < elementCount ; ++ i )
   {
      XmStringFree ( xmStr [ i ] ) ;
   }
   
   if ( xmStr != NULL )
   {
      XtFree ( ( char * ) xmStr ) ;
      xmStr = NULL ;
   }
   
   
  //printf("%s end \n", header);
 
   
   return ;
}

/*  ********************************************************************** */

/*
 * pc_loadComboBox
 * 
 * */
 void pc_loadComboBox(Widget comboBoxList, char** stringArray, int itemCount)
 {
 	XmString   xmStr;
 	int i = 0;
 	/* now load the element list accordingly */
     /*  create the XM Strings from the strings in stringArray*/ 
    for (i = 0; i < itemCount; i++)
    {
         xmStr = XmStringCreateSimple(stringArray[i]);
	
	     XmListAddItem(comboBoxList, xmStr, 0);
	  
	     XmStringFree(xmStr);
	     xmStr = NULL;
    }
   
 }


/*************************************************************************
   desensitize_type_source_widgets()
   ***********************************************************************/
void desensitize_type_source_widgets()
{

   DeSensitize(pc_typeSourcePB);
   DeSensitize(pc_typeSourceTB);
   
   return;
}

/*************************************************************************
   sensitize_type_source_widgets()
   ***********************************************************************/
void sensitize_type_source_widgets()
{
   Sensitize(pc_typeSourcePB); 
   Sensitize(pc_typeSourceTB);
   
   return;
}

/****************************************************************************
   set_timefields()
   Read the absolute time and time window from the user controlled
   window and set the variables to their value.
   **************************************************************************/
#define LENGTH_OF_TIME_STRING 20 /* This is the size of "str" as set in 
                                    X-designer (16 chars) plus an additional
                                    4 characters to accomodate the ":00" 
                                    second string and a terminating '\0'. */
void set_timefields()
{
    
    
   char header[] = "set_timefields(): "; 
   pc_options_struct * pc_options = get_pc_options();
   time_t	settime_t;
   int		hours;
   int		status;
   char 	*str;   
   char         time_string [ LENGTH_OF_TIME_STRING ] = { '\0' } ;
   static time_t prev_settime_t = 0;
   static int	 prev_hours     = -1;
   
   printf("%s in function \n", header);
     
   /* extract the values */
   str = XmTextGetString(pc_timespecTX);
   strncpy ( time_string , str , 16 ) ;
   time_string[16] = '\0';
   XtFree(str);
   strcat ( time_string , ":00" ) ; 

   status = yearsec_ansi_to_timet(time_string, &settime_t);
   if (status < 0)
      fprintf(stderr, "ERROR reading time: %s\n", time_string);
   
   str = XmTextGetString(pc_hrsoldTX);
   hours = atoi(str);   
   XtFree(str);
   
   /* set the values */
   
   pc_options->valid_timet = settime_t;
   pc_options->dur_hours   = hours;
      
   
   /* check if this time has changed from the last time 
      in order to determine whether a retrieval is needed */
   
   if (settime_t != prev_settime_t || hours != prev_hours)
   {
   	
   	  if (pc_options->query_mode == AD_HOC_MODE)
   	  {
          retrieval_required = 1;
   	  }      
      prev_settime_t = settime_t;
      prev_hours     = hours;
   }
   
   
   return;   
}
 

/*****************************************************************************
   pc_MapCB()
   callbacks() for Map Data push button 
   **************************************************************************/

// --------------------------------------------------------------------
void pc_MapButtonCB(Widget w, XtPointer ptr, XtPointer cbs)
{
  
   pc_drawMap(w,False);
   
   return;   
}


/******************************************************************************
   pc_Table()
   callback for Table Data push button 
   ***************************************************************************/

void pc_TableCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   /* get and set the time fields in the structure */
   set_timefields();
   
   pc_TabulateData(w, retrieval_required);
   
   retrieval_required = 0;
   
   return;
}


/****************************************************************************
   close callback for Point Display Control Dialog
   **************************************************************************/
void pc_CloseCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   XtUnmanageChild ( pointcontrolDS ) ;
   
   /* Free the linked list of PointDataPresets. */
   free_PointDataPresets ( ) ;

   /* Free the memory used by the service backup list. */
   free_service_backup_list ( ) ;

   return;
}

/****************************************************************************
callback for "Delete" button in Point Data Predefined Setting. 
*****************************************************************************/
void pc_DeleteCB ( Widget w , XtPointer ptr , XtPointer cbs )
{
   Boolean isItemSelected ;
   char where_clause [ 200 ] ; 
   int num_selected_items ;
   int * position_list = NULL ;
   int status ;
   PointDataPresets * pPresetHead = NULL ;
   PointDataPresets * pPresetNode = NULL ;

   /* Determine the currently selected preset group. */
   
   isItemSelected = XmListGetSelectedPos ( pc_presetsLI , & position_list , 
                                           & num_selected_items ) ;
   /*
   isItemSelected = XmListGetSelectedPos ( pc_predefineLS , & position_list , 
                                           & num_selected_items ) ;
    */
     
   if ( isItemSelected == True )
   {
      pPresetHead = get_PointDataPresetsHead ( ) ;

      if ( pPresetHead != NULL )
      {
         pPresetNode = ( PointDataPresets * ) ListNth ( & pPresetHead->list , 
                                                        position_list [ 0 ] ) ;

         if ( pPresetNode != NULL )
         {
            sprintf ( where_clause , "WHERE preset_id = '%s' " , 
                      pPresetNode->preset_id ) ;
         }

         XtFree ( ( char * ) position_list ) ;
         position_list = NULL ;

         status = DeletePointDataPresets ( where_clause ) ;

         if ( status != 0 )
         {
            fprintf ( stderr , "\nIn routine 'pc_DeleteCB':\n"
                               "Could not delete the record from\n"
                               "the PointDataPresets table. SQLCODE = %d\n" ,
                               status ) ;
         }

         load_presetOptionList ( True , NULL , True ) ;
      }
   }
}

/****************************************************************************
callback for "Save as New" button in Point Data Predefined Setting.
*****************************************************************************/
void pc_SaveAsNewCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Boolean isItemSelected ;
   static int saveasnew_created = 0 ;
   int num_selected_items ;
   int * position_list = NULL ;
   PointDataPresets * pPresetHead = NULL ;
   PointDataPresets * pPresetNode = NULL ;
   
   pPresetHead = get_PointDataPresetsHead ( ) ;

   /* Determine the currently selected preset group. */
   isItemSelected = XmListGetSelectedPos ( pc_presetsLI , & position_list , 
                                           & num_selected_items ) ;
     
   if ( isItemSelected == True )
   {
      pPresetNode = ( PointDataPresets * ) ListNth ( & pPresetHead->list , 
                                                     position_list [ 0 ] ) ;
   }

   if ( ! saveasnew_created )
   {
      create_saveasnewDS ( GetTopShell ( w ) ) ;
      add_saveasnew_callbacks ( ) ; 
      saveasnew_created = 1 ;
   }

   if ( ! XtIsManaged ( saveasnewDS ) )
   {
      XtManageChild ( saveasnewFO ) ;
      XtManageChild ( saveasnewDS ) ;
   }
   
   load_saveasnew ( pPresetHead , pPresetNode ) ;

   return ;
}

/****************************************************************************
Callback for the selection of a predefined option from the predefined 
option list.
*****************************************************************************/
void pc_PredefinedOptionSelectCB ( Widget w , XtPointer ptr , XtPointer cbs )
{
	/*
   Boolean double_click = * ( Boolean * ) ptr ;
   */
   
   int item_position ;
   int status ;
   
   char header[] = "pc_PredefineOptionSelectCB";
   
   
   
   
   /* Indicate to the pointcontrol manager routines that new data must
      be retrieved from the IHFS database. */
   retrieval_required = 1 ;

   /* Retrieve the selected row from the scrolled list displaying the
      predefined option sets. */
      
  XmComboBoxCallbackStruct *cbStruct = (XmComboBoxCallbackStruct *) cbs;
 
   /*
   isItemSelected = XmListGetSelectedPos ( pc_presetsLI , & position_list , 
                                           & num_selected_items ) ;
   */
   
   if (cbStruct == NULL)
   {
   	   printf("%s cbStruct is NULL \n", header);
       return;	
   }
   if (cbStruct->event == NULL) /* just browsing - XmComboBoxCallbackStruct documentation */
   {
   	    printf("%s event is NULL \n", header);
   	   return;
   }
 
   item_position = cbStruct->item_position;
   
   status = set_pc_options_using_presets ( item_position + 1 ) ;
  
   if ( status == 0 )
   {
   	   //Avoid several redraws while the options are being changed
   	   // What happens without this is that the individual callbacks call
   	   // pc_drawMap each time their contents are changed, resulting in
   	   // multiple unneed callbacks.  pc_drawMapConditionally now checks for a certain
   	   // the disable remapping variable
   	   disableDrawing = True;
       
       pc_SetGUIFromOptionsStruct();
       
       disableDrawing = False;
       
       //force a retrieve of data, since a lot of stuff could have
       // changed when selecting a new preset list.     
       pc_drawMap(map_widget, True);
   }
}

/****************************************************************************
Routine to load predefined option sets into the option set scrolled list
on the bottom of the PointControl GUI.
*****************************************************************************/
int load_presetOptionList ( Boolean refresh_preset_option_list ,
                             char * preset_id , 
                             Boolean update_pcoptions )
                             
{
    
   char header[] = "load_presetOptionList() ";
    
   int i ;
   int highlight_position = 1 ;
   int number_of_sets = 0;
   int status ;
   PointDataPresets * pPresetHead = NULL ;
   PointDataPresets * pPresetNode = NULL ;
   Char100Array * pOptionNameRankId = NULL ;


   printf("%s preset_id = %s \n", header, preset_id);

   /* Delete all of the list items. */
   XmListDeleteAllItems ( pc_presetsLI ) ;

   pPresetHead = get_PointDataPresetsHead ( ) ;

   if ( pPresetHead == NULL || refresh_preset_option_list == True)
   {
      if ( pPresetHead != NULL )
      {
         free_PointDataPresets ( ) ; 
      }

      get_PointDataPresetsList ( ) ;    
      pPresetHead = get_PointDataPresetsHead ( ) ;
   }

   if ( pPresetHead != NULL )
   {
      /* Retrieve a count of the number of option sets retrieved
         from the PointDataPresets table. */
      number_of_sets = ListCount ( & pPresetHead->list ) ;
      
      if ( number_of_sets > 0 )
      {
         /* Allocate an array of Char100Array data types based on the number
            of option sets in the PointDataPresets table. */
         pOptionNameRankId = ( Char100Array * ) 
                              malloc ( sizeof ( Char100Array ) *
                                       number_of_sets ) ; 

         if ( pOptionNameRankId == NULL )
         {
            fprintf ( stderr , "\nIn routine 'load_PredefinedOptionList':\n"
                               "Could not allocate %d bytes for the\n"
                               "buffered option set name, rank, and id\n"
                               "information.\n" , sizeof ( Char100Array ) *
                               number_of_sets ) ;

         }

         pPresetNode = ( PointDataPresets * ) 
                       ListFirst ( & pPresetHead->list ) ;

         for ( i = 0 ; i < number_of_sets ; ++ i )
         {
            if ( preset_id != NULL )
            { 
               status = strcmp ( preset_id , pPresetNode->preset_id ) ;

               if ( status == 0 )
               {
                  highlight_position = i + 1 ;
               }
                  
            }

            sprintf ( pOptionNameRankId [ i ] , "%-30s (%d,%s)" ,
                      pPresetNode->descr , pPresetNode->preset_rank ,
                      pPresetNode->preset_id ) ;
            pPresetNode = ( PointDataPresets * ) 
                          ListNext ( & pPresetNode->node ) ;
         } //end for

         /* Load the option sets into the option set scrolled list on the
            pointcontrol GUI. */
         loadXmList100 ( pc_presetsLI , pOptionNameRankId ,
                         number_of_sets ) ;

         /* Deallocate the memory used to create the buffered option set
            description, id, and rank info. */
         if ( pOptionNameRankId != NULL )
         {
            free ( pOptionNameRankId ) ;
            pOptionNameRankId = NULL ;
         }

         if ( preset_id != NULL )
         {
            XmListSelectPos ( pc_presetsLI , highlight_position ,
                              update_pcoptions ) ;
         }
         else
         {
            XmListSelectPos ( pc_presetsLI , 1 ,
                              update_pcoptions ) ;
         }
         
        
         XmComboBoxCallbackStruct cbStruct;
         XEvent * xEvent = malloc(sizeof(XEvent));
         cbStruct.event = xEvent;
         cbStruct.item_position = highlight_position - 1; //This ComboBox starts at 0, even if the list does not
         
         pc_PredefinedOptionSelectCB ( NULL, NULL, &cbStruct );
         
         if (xEvent != NULL)
         {
             free(xEvent);
             xEvent = NULL;   
         }
         
         
      } //end if number of sets

   } //end present Head != NULL
   
   
   return number_of_sets;
   
} //end load_PredefinedOptionList



/*  ********************************************************************** */
TimeStepDataElement pc_getTimeStepElementFromString(const char * timeStepElementString)
{
	/*  
	 * Note the dataElementStringArray needs to be in the SAME order as the
	 * TimeStepDataElement enum
	 * 
	 * */
    TimeStepDataElement element = -1;
    int i = 0;
    char * dataElementString = NULL;
//    char header[] = "pc_getTimeStepElementFromString(): ";
    
 //   printf("%s --------------- timeStepElementString arg = %s\n", header, timeStepElementString);
    
    for (i = 0; i < TIME_STEP_DATA_ELEMENT_COUNT; i++)
    {
    	
    	
    	dataElementString = pc_getTimeStepElementStringFromElement(i);
    	
    //	printf("%s dataElementString = %s\n", header, dataElementString);
 
  
        if 	(strcmp(dataElementString, timeStepElementString) == 0)
        {
            element = i;
            break;	
        }
    }
    
  
    return element;	
}


/*  ********************************************************************** */
char * pc_getTimeStepElementStringFromElement(TimeStepDataElement element)
{
	/*  
	 * Note the dataElementStringArray needs to be in the SAME order as the
	 * TimeStepDataElement enum
	 * 
	 * */
    char * returnString = NULL;
  
    									   
    if ( (element >= 0 ) && (element < TIME_STEP_DATA_ELEMENT_COUNT))
    {
         returnString = timeStepDataElementStringArray[element];
    }
    return returnString; 	

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}




