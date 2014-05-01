#include <Xm/Xm.h>

#include "color_threshold_show.h"
#include "damDisplayControl_show.h"
#include "display_best_qpe_show.h"
#include "tsgen_info.h"
#include "map_defines.h"
#include "map_library.h"
#include "TSControl_show.h"
#include "alertalarm_show.h"
#include "reviewqc_show.h"
#include "rejectdata_show.h"
#include "latestrep.h"
#include "latestrep_show.h"
#include "statprof_show.h"
#include "rate_cbs.h"
#include "stachar_show.h"
#include "textrept.h"
#include "textrept_show.h"
#include "contact_show.h"
#include "crest_show.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "map.h"
#include "map_resource.h"
#include "preaccum_show.h"
#include "prodview_show.h"
#include "HvDisplayControlDefs.h"
#include "HvDisplayControlProto.h"
#include "reviewqc.h"
#include "gui_builder.h"
#include "PointDisplayControl.h"


/* Added these prototypes to get this code to successfully compile
   without warnings.  Bryon Lawrence  June 6, 2001. */
#include "HvDisplayControlDefs.h"
void showStaffGage(Widget parent, char *lid);
void ShowFstmtDs(Widget w, char *lid, int isEditable);
void ShowLwstmtDs(Widget w, char *lid, int isEditable);

extern Widget locationShiftCascadeMenuItem; //used to sensitize and desensitize the menu items


/* Added these global variables on July 16, 2001 to enable the use
   of the "point control" utilities. */

void pointdisplayCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   static XtAppContext app_context = 0 ;
   HvDisplayControl * hdc = NULL ;
   static int first = 1 ;
   int is_displayed ;
   static Widget top_widget = 0 ;

   if ( first == 1 )
   {
      first = 0 ;
      
      Sensitize(locationShiftCascadeMenuItem );

      /* Determine if this is being run in suppressed mode.
         If it is, then initialize the HV display control variables.
         Note that the automatic refresh is not activated here. */
      is_displayed = is_riverstatus_displayed ( ) ;
      top_widget = _get_map_widget ( 0 ) ;
      app_context = _get_map_context ( ) ;

      if ( is_displayed == 0 )
      {
         hdc = getHvDisplayControl ( ) ;
         initHvDisplayControl ( top_widget , hdc , redrawMap , 0 ) ;
      }
   }

   /* Set the pointer to indicate that the program is "busy". */
   mSetCursor ( M_WATCH ) ;

   /* Launch the point display control window. */
   show_pointcontrolDS ( w , top_widget , app_context ) ;

   /* Set the pointer back to "normal" to indicate that the program
      is "idle". */ 
   mSetCursor ( M_NORMAL ) ;
   
} 

void bestQpeCallback ( Widget w, XtPointer clientdata, XtPointer calldata )
{
   /* Set the watch cursor to "work" mode. */
   mSetCursor ( M_WATCH );

   show_bestQpeDS ( w );

   /* Set the watch cursor to "done" mode. */
   mSetCursor ( M_NORMAL );
}

void damDisplayCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{

   /* Set the pointer to indicate that the program is "busy". */
   mSetCursor ( M_WATCH ) ;

   /* Launch the dam display control window. */
   dam_display_show ( w ) ;

   /* Set the pointer back to "normal" to indicate that the program
      is "idle". */ 
   mSetCursor ( M_NORMAL ) ;
} 



void riverMonitorCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   char command [ BUFSIZ ] ;
   char binDir [ 128 ] ;
   int rv = 1 ;
   int gad_token_len = 0 ;
   int gad_value_len = 0 ;
 
 
   mSetCursor ( M_WATCH ) ;
   gad_token_len = strlen ( "whfs_bin_dir" ) ;
   get_apps_defaults ( "whfs_bin_dir" , & gad_token_len , binDir , 
                       & gad_value_len ) ;
  
   if ( strlen ( binDir ) > 0 )
   {
        sprintf ( command , "%s/start_rivermonitor &" , binDir );
      
        rv = system(command); 
   }
   else
   {
      fprintf ( stderr , "whfs_bin_dir env variable not available\n" ) ;
   }

   mSetCursor ( M_NORMAL ) ;
}



void siteSpecificCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   char command [ BUFSIZ ] ;
   char binDir [ 128 ] ;
   int rv = 1 ;
   int gad_token_len = 0 ;
   int gad_value_len = 0 ;
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   mSetCursor ( M_WATCH ) ;
   gad_token_len = strlen ( "whfs_bin_dir" ) ;
   get_apps_defaults ( "whfs_bin_dir" , & gad_token_len , binDir , 
                       & gad_value_len ) ;
  
   if ( strlen ( binDir ) > 0 )
   {
      if ( pCurrentStation != NULL )
      {
         sprintf ( command , "%s/run_SiteSpecific %s &" , binDir , 
                   pCurrentStation->lid ) ;
      }
      else
      {
         sprintf ( command , "%s/run_SiteSpecific &" , binDir ) ;
      }

      rv = system(command); 
   }
   else
   {
      fprintf ( stderr , "whfs_bin_dir env variable not available\n" ) ;
   }

   mSetCursor ( M_NORMAL ) ;
}

void tsControlCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   Station * pCurrentStation = NULL ;

   TSGEN_INFO tsgen ;
   tsgen.standalone     = NON_STANDALONE ;
   tsgen.pedtse_defined = 0 ;
   tsgen.nitems         = 1 ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      tsgen.group_check = 1 ;
      strcpy ( tsgen.lid , pCurrentStation->lid ) ;
   }
   else
   {
      tsgen.group_check = 0 ;
      strcpy ( tsgen.lid , " " ) ;
   }

   mSetCursor( M_WATCH ) ;
   show_TSControlDS ( w , tsgen ) ;
   mSetCursor ( M_NORMAL ) ;

}


/* *************************************************************** */
void alertalarmCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
	mSetCursor ( M_WATCH ) ;
	alertalarm_show ( w ) ;
	mSetCursor ( M_NORMAL ) ;
}

/* *************************************************************** */
void outOfRangeCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   /* Check to determine if the GUI has already been created. */
   if ( reviewqcDS == NULL )
   {
      mSetCursor ( M_WATCH ) ;
      
      if ( pCurrentStation != NULL )
      {
         reviewqc_show ( w , pCurrentStation->lid ) ;
      }
      else
      {
         reviewqc_show ( w , " " ) ;
      }

      mSetCursor ( M_NORMAL ) ;

   }
   else
   {
      RemapWidget ( reviewqcDS ) ;
   }
}

/*******************************************************************/
void rejectedDataCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;
   pCurrentStation = get_current_station ( ) ;


   mSetCursor ( M_WATCH ) ;

   if ( pCurrentStation != NULL )
   {
      rejectdata_show ( w , pCurrentStation->lid ) ;
   }
   else
   {
      rejectdata_show ( w , " " ) ;
   }

   mSetCursor ( M_NORMAL ) ;

}

/* *************************************************************** */
void preaccumCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   mSetCursor ( M_WATCH ) ;
   preaccum_show ( w ) ;
   mSetCursor ( M_NORMAL ) ;
}

/* *************************************************************** */
void stationReportCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;  

   pCurrentStation = get_current_station ( ) ; 

   /* Test to determine if the GUI has already been constructed. */
   if ( latestrepDS == NULL )
   {
      mSetCursor ( M_WATCH ) ;

      if ( pCurrentStation != NULL )
      {
         latestrep_show ( w,  pCurrentStation->lid ) ;
      }
      else
      {
         latestrep_show ( w, " ") ;
      }

      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      RemapWidget ( latestrepDS ) ;
   }
}


/* *************************************************************** */
void stationProfileCallback(Widget w, XtPointer ptr, XtPointer cbs)
{ 
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor ( M_WATCH ) ;
      statprof_show ( w , pCurrentStation->lid ) ;  
      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      ErrorDialog ( w , "Please select a station first." ) ;	
   }
}

/* *************************************************************** */
void hydrobriefCallback(Widget w, XtPointer ptr, XtPointer cbs)
{ 
	 char command[BUFSIZ];
	 char *binDir;
	 int rv = 1;

	 printf("hydrobriefCallback........\n");

	 mSetCursor( M_WATCH);
	 binDir = getenv("WHFS_BIN_DIR");
	 if (binDir)
	 {
		sprintf(command, "%s/run_hydrobrief &",binDir);
		 rv = system(command);
 	 }
	 else
	 	fprintf(stderr,"WFO_BIN_DIR env variable not available\n");


	 mSetCursor(M_NORMAL);

}


/* *************************************************************** */
void staffGageCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor ( M_WATCH ) ;
      showStaffGage ( w , pCurrentStation->lid ) ;
      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      ErrorDialog( w , "Please select a station first." ) ;	
   }

}


/* *************************************************************** */
void impactStatementCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
    Station * pCurrentStation = NULL ;

    pCurrentStation = get_current_station ( ) ;

    if ( pCurrentStation != NULL )
    {
       mSetCursor( M_WATCH ) ;
       ShowFstmtDs( w , pCurrentStation->lid , 0 ) ;
       mSetCursor ( M_NORMAL ) ;
    }
    else
    {
       ErrorDialog( w , "Please select a station first." ) ;
    }
}

/* *************************************************************** */
void lowStatementCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
    Station * pCurrentStation = NULL ;

    pCurrentStation = get_current_station ( ) ;

    if ( pCurrentStation != NULL )
    {
       mSetCursor( M_WATCH ) ;
       ShowLwstmtDs( w , pCurrentStation->lid , 0 ) ;
       mSetCursor ( M_NORMAL ) ;
    }
    else
    {
       ErrorDialog( w , "Please select a station first." ) ;
    }
}

/* *************************************************************** */
void ratingCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
    Station * pCurrentStation = NULL ;

    pCurrentStation = get_current_station ( ) ;

    if ( pCurrentStation != NULL )
    {
       mSetCursor ( M_WATCH ) ;
       ShowRateDs ( w , pCurrentStation->lid , False ) ;
       mSetCursor ( M_NORMAL ) ;
    }
    else
    {
       ErrorDialog ( w , "Please select a station first." ) ;
    }

}


/* *************************************************************** */
void dataSourcesCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor( M_WATCH ) ;
      ShowStaCharDs( w , pCurrentStation->lid , False ) ;
      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      ErrorDialog( w , "Please select a station first." ) ;
   }
}

/* *************************************************************** */
void contactCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation  = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor ( M_WATCH ) ;
      contact_show ( w , pCurrentStation->lid , False ) ;
      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      ErrorDialog ( w , "Please select a station first." ) ;
   }
}


/* *************************************************************** */
void crestHistoryCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor ( M_WATCH ) ;
      crest_show ( w , pCurrentStation->lid , False ) ;
      mSetCursor ( M_NORMAL ) ;
   }
   else 
   {
      ErrorDialog ( w , "Please select a station first." ) ;
   }

}

/* *************************************************************** */
void textreportsCallback ( Widget w , XtPointer ptr , XtPointer cbs )
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( textreptDS == NULL )
   {
      if ( pCurrentStation != NULL )
      {
         mSetCursor ( M_WATCH ) ;
         ShowTextReportsDs ( w , pCurrentStation->lid ) ;
         mSetCursor ( M_NORMAL ) ;
      }
      else
      {
         ErrorDialog ( w , "Please select a station first." ) ;
      }
   }
   else
   {
      RemapWidget ( textreptDS ) ;
   }
 
}

/* *************************************************************** */
void productViewerCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;

   pCurrentStation = get_current_station ( ) ;

   if ( pCurrentStation != NULL )
   {
      mSetCursor ( M_WATCH ) ;
      show_prodview ( w , pCurrentStation->lid ) ;
      mSetCursor ( M_NORMAL ) ;
   }
   else
   {
      ErrorDialog ( w , "Please select a station first." ) ;
   }
}

/* *************************************************************** */

void launchColorManager ( Widget w , XtPointer ptr , XtPointer cbs )
{
	mSetCursor ( M_WATCH ) ;
	color_threshold_show ( w ) ;
	mSetCursor ( M_NORMAL ) ;
}

