#include <stdio.h>
#include <stdlib.h>

#include "BinarySearch.h"
#include "CurPC.h"
#include "CurPP.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "get_loc_info.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_addmissing.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_derive.h"
#include "pointcontrol_findbasis.h"
#include "pointcontrol_getdata.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_riverstatus.h"
#include "pointcontrol_value.h"
#include "pointcontrol_report.h"
#include "pointcontrol_timestep.h"
#include "pointcontrol_options.h"
#include "pointcontrol_loc_shift.h"
#include "rating_util.h"
#include "RiverStatus.h"
#include "time_convert.h"

#define DEBUG 1

static int drawStationFlag = 1 ; /* By default stations are plotted. The user
                                    may use the routines "enableStationDrawing"
                                    and "disableStationDrawing" to manipulate
                                    this flag.  A value of "0" means that
                                    the stations are not plotted. */
static int pointDataUpdateFlag = 0 ; /* This flag tells the outside world that
                                        the point data has been changed
                                        and any processing such as copying
                                        to a locally maintained structure
                                        of point data must be performed. */




static char responsible_hsa [ MAXLEN_AREALIST ] = "" ;

/****************************************************************************
   File:           pointcontrol_mgr.c 
   
   This file drives the functions related to the point
   control operations.
   
   
   *****  HELPFUL DOCUMENTATION **************************
   
   The primary steps in the overall process are:
   
   0. Provide a GUI to the user that allows them to specify
   which data to retrieve present.  The details on which
   data to retrieve is contained within the pc_options
   structure. (pointcontrol_show.c)
   
   This gui calls this manager function, which manages
   the procesing of the remaining steps.
   
   1. Retrieve the data from the relational database and store it
   in the linked list structure(s).  This retrieval will typically
   get multiple values for a given location. (pointcontrol_getdata.c)
   
   2. Derive the specific data value for each location, so that there
   is one and only one value for the location. (pointcontrol_derive.c)
   
   Steps 2 and 3 are only performed when the options are changed
   that require a retrieval of data.
   
   3. For requests that get/derive multiple values per station 
   (e.g. RiverStatus mofo), then determine which value to use
   for the station (pointcontrol_findbasis.c)
   
   4. Allow for "missing" reports to be added as per user instructions.
   (pointcontrol_addmissing.c)
   
   5. Filter the data according to the data source, service backup assignments,
   or other user-selected filters. (pointcontrol_filter.c)
   
   At this point, the data are ready for display in either
   a tabulated list or a geographical display.   
   
  -----------------
  
  There are two "variables" that are shared between the pointcontrol
  software and the external software which controls the retrieval
  and then uses the data, presumably for a map display.
  One variable controls the retrieval, this is type pc_options_struct.
  The second variable contains the resulting data, this is of type ReportList. 
   
      
   
   **************************************************************************/


/* linked list head pointers for DERIVED report data */

static ReportList	*obsrepHead  = NULL;  /* derived report list for obs */
static ReportList	*fcstrepHead = NULL;  /* derived report list for fcast */


/* linked list head pointers for FINAL report data */
//static ReportList       *repHead = NULL;   /* could be mix of obs and fcast */

/* use a global external pointer to the linked list;
   this is declared by the external calling functions */


static ReportList *reportListHead = NULL;
static ReportList *iconReportListHead = NULL;

void setIconReportListHead(ReportList * newReportListHead)
{
  iconReportListHead = newReportListHead;
 
  return;
    
}


ReportList * getIconReportListHead()
{
  
  return iconReportListHead;
    
}



void setReportListHead(ReportList * newReportListHead)
{
  reportListHead = newReportListHead;
 
  return;
	
}


ReportList * getReportListHead()
{
  
  return reportListHead;
	
}



void pc_process_request(int	retrieval_required_arg)
{	
    char header[] = " pc_process_request(): ";
    static int first_time = 1;
  
    if (first_time)
    {
        printf("%s *** first call *** \n", header);
    }
    else
    {
        printf("%s \n", header);
    }
    
    pc_options_struct * pc_options = get_pc_options();
    
    
    if( first_time ) //load the location shift values
    {
        loadShiftValues();
        first_time = 0;       
    } 
    
  	
	if (pc_options->query_mode == AD_HOC_MODE)
	{
	    pc_adhoc_process_request(retrieval_required_arg);
	}
	else
	{
		pc_timestep_process_request(retrieval_required_arg);
	}
    
    applyShiftValues();
}

void pc_adhoc_process_request(int retrieval_required_arg)
{
    
   char header[]  = "pc_adhoc_process_request(): ";
    
   /* linked list head pointers for RAW retrieved data */
   ReportList       *repHead = NULL;
   
   CurPP     *ppHead = NULL;	 /* precip PP */
   CurPC     *pcHead = NULL;	 /* precip PC */   
   LatestObsValue	*lHead   = NULL; /* Latestobs (snow/temp/other) */
   Observation		*obsHead = NULL; /* non-latest obs */
   Observation	*obshHead = NULL;	 /* observed height    */
   Observation	*obsdHead = NULL;	 /* observed discharge */
   RiverStatus	*rsHead   = NULL;	 /* river status  */   
   pc_options_struct * pc_options = get_pc_options();
   
   static int 		first_gad = 1;
   static long 		minutes = 15;
   static time_t	prev_retrievaltime = 0;
   char 		minute_string[128];
   int  		gad_token_len, gad_value_len, rv;
   time_t		curtimet;
   
    if (DEBUG) printf("***** pc_adhoc_process_request *****\n");
   
   /* check if the time since the the last retrieval is greater than
      the limit defined by a token.  if so, then re-retrieve the data
      regardless of the retrieval_required setting */
   time(&curtimet);

   /* Indicate to the outside world that the point data is being
      updated. */
   set_pointdata_update_flag ( ) ;

   if (!retrieval_required_arg)
   {
      if (first_gad)
      {
		 gad_token_len = strlen("hv_refresh_minutes");
		 get_apps_defaults("hv_refresh_minutes", &gad_token_len, 
				   minute_string, &gad_value_len);
		 if (strlen(minute_string) > 0)
		 {
		    rv = sscanf(minute_string, "%ld", &minutes);
		    
		    if ((rv < 1) || (minutes < 1) || (minutes > 1440) )
		    {
		       fprintf(stderr, "hv_refresh_minutes token not defined.\n"
			       "Using default refresh time of %ld minutes \n", minutes);
		    }
		 }
		 first_gad = 0;
      }
      
      if (curtimet > prev_retrievaltime + (minutes * 60))
      {
		 printf("%s Performing full data retrieval since refresh time exceeded\n", header);
		 retrieval_required_arg = 1;
      }
   }
   else
      prev_retrievaltime = curtimet;
   
   
   if (DEBUG) 
   {
       printf("%s retrieval_required=%d\n", header, retrieval_required_arg);
   }
   
   /* only retrieve and derive the data if the data has
      not already been retrieved */
   
   if (retrieval_required_arg)
   {           
      /* always free any linked lists allocated for derived data */
      
      free_derivedPtrs();
      free_finalPtr();
      
      /* get the RAW data, then derive the DERIVED data based on the
	 user instructions. */
      
      switch (pc_options->element_type)
      {
		 case RIVER_AD_HOC_TYPE:
		    
		    /* get the data from the appropriate data set, whether it
		       be from the height, discharge, or riverstatus table. 
		       then process the data to get a single value for each location;
		       one list is for observed data and one is for forecast data */
		    
		    getRiverData(pc_options,
				 &obshHead, &obsdHead, &rsHead);
	                	    
		    derive_reportsRiver(pc_options, obshHead, obsdHead, rsHead,
					&obsrepHead, &fcstrepHead);
		          
                      
             printReportObs(obsrepHead, header, "after derived_reportsRiver(), obsrepHead ");
             printRiverStatus(rsHead, header, "after derived_reportsRiver(), rsHead ");
      
            
		    break;
		    
		 case RAIN_AD_HOC_TYPE:
		    
	            /* Do not process rain for the value change option.  Showing
	               a change in precipitation amount over a period of time 
	               generally does not make sense. */ 
	            if ( pc_options->time_mode != VALUE_CHANGE )
	            {
			       /* any "rain" data other than PC, PP, or both, is
			          treated like generic data. then process the data
			          to get a single value for each location */
		
			       if (!pc_options->PCandPP &&
				   strncmp(pc_options->selectedAdHocElementString, "PC", 2) != 0 &&
				   strncmp(pc_options->selectedAdHocElementString, "PP", 2) != 0)
			       {	    
			          getSnowTempOtherData(pc_options,
				   		       &obsHead, &lHead);
			          obsrepHead = derive_reportsOther(pc_options, obsHead, lHead);
			       }
			    
			       /* get PC and/or PP data. then process the data to get a
			          single value for each location */
			    
			       else
			       {	    
			          getRainData(pc_options,
				   	      &pcHead, &ppHead);
			          obsrepHead = derive_reportsRain(pc_options, pcHead, ppHead);
			       }
	            }
		    
		    break;
		    
		    
		 case SNOW_AD_HOC_TYPE:
		 case TEMP_AD_HOC_TYPE:
		 default:
		    
		    /* get snow, temperature or other data, including
		       processed data, in a general fashion. then process
		       the data to get a single value for each location */
		    
		    getSnowTempOtherData(pc_options,
					 &obsHead, &lHead);
		    
		    obsrepHead = derive_reportsOther(pc_options, obsHead, lHead);
		    
		    break;
		   	    
		    
      } //end switch
      
      
      /* free any linked list head pointers for the raw data */
      
      free_rawPtrs(obshHead, obsdHead, rsHead,
		   ppHead, pcHead, lHead, obsHead);  
      
   }  /* end of if (retrieval_required) block */


   /* if getting the latest River data, then the stage basis selected by
      the user must be applied.  if getting the max of the obs or forecast,
      then loop thru the observed and forecast lists and assign the
      appropriate value. otherwise simply copy the list. */
   
   if  ( (pc_options->element_type == RIVER_AD_HOC_TYPE) && 
         (pc_options->time_mode == LATEST)
       )
   {
      if (DEBUG)
      {
         printf("setting value as per stage basis...\n");
      }
      
      if (pc_options->stagebasis == BASIS_OBS)
      {
       
         repHead = copy_replist(obsrepHead);
         
         printReportObs(repHead, header, "in BASIS_OBS section, repHead ");
      
      }
      else if (pc_options->stagebasis == BASIS_FCST)
      {
       
		 repHead = copy_replist(fcstrepHead);
         printReportObs(repHead, header, "in BASIS_FCST section, repHead ");
      
      }
      
      else //BOTH OBS and FCST
      {
		 repHead = determine_rs_mofo(obsrepHead, fcstrepHead);	
         printReportObs(repHead, header, "in BASIS_BOTH section, repHead ");
      }  
         
    
         
   } // end if RIVER_AD_HOC_TYPE and LATEST mode
   
   else
   {
      repHead = copy_replist(obsrepHead);
   }
   
   /* Note that copy_replist must set the new elements for stage,
      discharge, and departure to missing. These elements are being
      included so that they don't have to be recalculated each time
      a request is made for which data retrieval is not required. That
      would be time consuming. */

   /* if the user has requested that a missing report be shown for
      each station being considered, then add them as necessary.
      the report list is passed in AND returned to handle the case
      where the report list may be empty, and therefore this function
      must assign a new Head pointer.
      
      missing data are only added if requested, since it takes
      a finite time to determine which stations are missing. */
      
   if (!pc_options->suppress_missing)
   {
      if (DEBUG) 
      {
          printf("adding missing reports...\n");
      }
      
      printReportObs(repHead, header, "before add_missing_reports section, repHead ");
      
  
      
      add_missing_reports (pc_options, & repHead ) ;
      
      printReportObs(repHead, header, "after add_missing_reports section, repHead ");
      
  
      
   }

   /* Check the "Values" pulldown option menu.  Process the options
      according to the selected value which may be one of the following:
      Value
      Value / Flood Level
      Value / Derived Stage (or Derived Flow)
      Flood Departure
      Flood Departure / Flood Level 

      Only do this for river PE's or the PRIMARY derived type. */
      
     
   process_value_option_menu ( repHead , pc_options , 
                               retrieval_required_arg ) ;

   /* Check to determine if the user wants the displayed river icons
      to be colored according to their river status value.  This is done
      by checking the toggle button corresponding to the display of
      riverstatus information on the point data control gui. */
   process_river_threat_index ( repHead , retrieval_required_arg ) ;

   /* Filter the data as needed.  This should be the last step in the
      process of preparing the point data to be displayed.  This routine
      also adds key information to the reports such as the latitude and 
      longitude of the station, its display class, its name, and its
      stream name.  */
      
             
      if (repHead != NULL)
      {   
        printf("%s before filter_reports_and_add_info(), repHead list count = %d\n",
                header, ListCount(&repHead->list));
      }   
      else
      {
         printf("%s before filter_reports_and_add_info(), repHead = NULL\n",
                header);
      }   
  
      
      filter_reports_and_add_info ( pc_options , repHead ) ;
   
   
      if (repHead != NULL)
      {   
        printf("%s after filter_reports_and_add_info(), repHead list count = %d\n",
                header, ListCount(&repHead->list));
      }   
      else
      {
         printf("%s after filter_reports_and_add_info(), repHead = NULL\n",
                header);
      }   
   
  
   
   /* assign the result to a static file variable. */
   setReportListHead(repHead);
   
   return;
}


/*****************************************************************************
   THIS FUNCTION IS INTENDED TO OPERATE EXACTLY THE SAME AS 
   pc_process_request().  IN FACT, IT SHOULD BE EXACTLY THE
   SAME EXCEPT...THE DIFFERENCE IS THAT THIS FUNCTION PASSES THE
   INFORMATION AS CONVENTIONAL ARGUMENTS.  THE INPUT OPTIONS ARE PASSED IN 
   AND THE RETRIEVED DATA ARE PASSED OUT.  THIS FUNCTION ALLOCATES
   THE MEMORY AND PROVIDES THE DATA TO THE CALLING FUNCTION,
   WHICH IS RESPONSIBLE FOR FREEING THE RETURNED MEMORY.
   
   The specific differences between this function and pc_process_request
   is in how the data are passed in and out, and that this function
   always retrieves data (i.e. there is no retrieval_required) argument.
   Lastly, its report lists memory is local to this function, and not global.

*****************************************************************************/

ReportList * pc_process_onetime(pc_options_struct pc_options_onetime)
{
   /* linked list head pointers for RAW retrieved data */
   Observation	  * obshHead = NULL;	/* observed height    */
   Observation	  * obsdHead = NULL;	/* observed discharge */
   RiverStatus	  * rsHead   = NULL;	/* river status  */
   CurPP          * ppHead = NULL;		/* precip PP */
   CurPC          * pcHead = NULL;		/* precip PC */
   LatestObsValue * lHead   = NULL;	/* Latestobs (snow/temp/other) */
   Observation	  * obsHead = NULL;	/* non-latest obs */
      
   /* linked list head pointers for DERIVED report data */
   ReportList	   * obsrep2Head  = NULL;  /* derived report list for obs */
   ReportList	   * fcstrep2Head = NULL;  /* derived report list for fcast */
   
   /* linked list head pointers for FINAL report data */
   ReportList	*repHead = NULL;   /* could be mix of obs and fcast */
      
   if (DEBUG) printf("***** pc onetime request *****\n");
   
   /* get the RAW data, then derive the DERIVED data based on the
      user instructions. */
   
   switch (pc_options_onetime.element_type)
   {
      case RIVER_AD_HOC_TYPE:
	 
	 /* get the data from the appropriate data set, whether it
	    be from the height, discharge, or riverstatus table. 
	    then process the data to get a single value for each location;
	    one list is for observed data and one is for forecast data */
	 
	 getRiverData ( & pc_options_onetime ,
		        & obshHead , & obsdHead , & rsHead ) ;
	 
	 derive_reportsRiver( & pc_options_onetime , obshHead , obsdHead , 
                              rsHead , & obsrep2Head , & fcstrep2Head ) ;
	 
         break ;
	 
      case RAIN_AD_HOC_TYPE:
	 
	 /* any "rain" data other than PC, PP, or both, is
	    treated like generic data. then process the data
	    to get a single value for each location */
	 
	 if (!pc_options_onetime.PCandPP && 
	     strncmp(pc_options_onetime.selectedAdHocElementString, "PC", 2) != 0 &&
	     strncmp(pc_options_onetime.selectedAdHocElementString, "PP", 2) != 0)
	 {	    
	    getSnowTempOtherData(&pc_options_onetime,
				 			 &obsHead, &lHead);
	    
	    obsrepHead = derive_reportsOther(&pc_options_onetime, obsHead, 
                                             lHead);
	 }
	 
	 
	 /* get PC and/or PP data. then process the data to get a
	    single value for each location */
	 
	 else
	 {	    
	    getRainData(&pc_options_onetime,
					&pcHead, &ppHead); 
	    
	    obsrepHead = derive_reportsRain(&pc_options_onetime,
	                                    pcHead, ppHead);
	 }
	 
	 break;
	 
	 
      case SNOW_AD_HOC_TYPE:
      case TEMP_AD_HOC_TYPE:	
      default:	
	 
	 /* get snow, temperature or other data, including
	    processed data, in a general fashion. then process
	    the data to get a single value for each location */
	 
	 getSnowTempOtherData(&pc_options_onetime,
			      		 &obsHead, &lHead);
	 
	 obsrepHead = derive_reportsOther(&pc_options_onetime, 
	 								  obsHead, lHead);
	 
	 break;
   }
   
   
   /* free any linked list head pointers for the raw data 
      and the derived data */
   
   free_rawPtrs(obshHead, obsdHead, rsHead,
		ppHead, pcHead, lHead, obsHead); 
         
   
   /* if getting the latest River data, then the stage basis selected by
      the user must be applied.  if getting the max of the obs or forecast,
      then loop thru the observed and forecast lists and assign the
      appropriate value. otherwise simply copy the list. */
   
   if (pc_options_onetime.element_type == RIVER_AD_HOC_TYPE && 
       pc_options_onetime.time_mode == LATEST)
   {
      if (DEBUG) printf("setting value as per stage basis...\n");
      
      if ( pc_options_onetime.stagebasis == BASIS_OBS &&
           obsrepHead != NULL )
         repHead = copy_replist(obsrep2Head);
      
      else if ( pc_options_onetime.stagebasis == BASIS_FCST &&
                fcstrepHead != NULL )
	 repHead = copy_replist(fcstrep2Head);
      
      else
	 repHead = determine_rs_mofo(obsrep2Head, fcstrep2Head);	 
   }
   
   else
      repHead = copy_replist(obsrep2Head);
   
  
   /* if the user has requested that a missing report be shown for
      each station being considered, then add them as necessary.
      the report list is passed in AND returned to handle the case
      where the report list may be empty, and therefore this function
      must assign a new Head pointer.
      
      missing data are only added if requested, since it takes
      a finite time to determine which stations are missing. */
      
   if ( ! pc_options_onetime.suppress_missing && repHead != NULL )
   {
      if (DEBUG) printf("adding missing reports...\n");
      
      add_missing_reports ( & pc_options_onetime, & repHead);     
   }
    
   /* Don't bother filtering the data here.  For riverstatus
      we want everything. */
   
   /* free the reports */
   
   if ( obsrep2Head != NULL )
      FreeReportList(obsrep2Head);
   if ( fcstrep2Head != NULL )
      FreeReportList(fcstrep2Head);
   
   return(repHead);
}


/*****************************************************************************
   free any linked list memory allocated to raw Pointers
   **************************************************************************/
void free_rawPtrs(Observation	*obshHead,
		  Observation	*obsdHead,
		  RiverStatus	*rsHead,		  
		  CurPP         *ppHead,
		  CurPC         *pcHead,		  
		  LatestObsValue *lHead,
		  Observation	 *obsHead)
{

   if (obsdHead)
   {
      FreeObservation(obsdHead);
      obsdHead = NULL;
   }
   
   if (obshHead)
   {
      FreeObservation(obshHead);
      obshHead = NULL;
   }
   
   if (rsHead)
   {
      FreeRiverStatus(rsHead);
      rsHead = NULL;
   }
   
   if (ppHead)
   {
      FreeCurPP(ppHead);
      ppHead = NULL;
   
   }
   
   if (pcHead)
   {
      FreeCurPC(pcHead);
      pcHead = NULL;
   }
   
   if (lHead)
   {
      FreeLatestObsValue(lHead);
      lHead = NULL;
   }
   
   if (obsHead)
   {
      FreeObservation(obsHead);
      obsHead = NULL;
   }
   
   return;
}


/*****************************************************************************
   free any linked list memory allocated to derived data
   **************************************************************************/
void free_derivedPtrs()
{

   if (obsrepHead)
   {
      FreeReportList(obsrepHead);
      obsrepHead = NULL;
   }
   
   if (fcstrepHead)
   {
      FreeReportList(fcstrepHead);
      fcstrepHead = NULL;
   }
  
   
   return;
}


/*****************************************************************************
   free any linked list memory allocated to the final linked list data
   **************************************************************************/
void free_finalPtr()
{  
   ReportList *repHead = getReportListHead();
   	
   if (repHead)
   {
      FreeReportList(repHead);
      repHead = NULL;
      setReportListHead(repHead);
   }
   
   return;
}



void enableStationDrawing ( )
{
   drawStationFlag = 1 ;
}

/***********************************************************************/

void disableStationDrawing ( )
{
   drawStationFlag = 0 ;
}

/***********************************************************************/

int getStationDrawingState ( )
{
   return drawStationFlag ;
}

/***********************************************************************/

inline void set_pointdata_update_flag ( )
{
   pointDataUpdateFlag = 1 ;
}

inline int get_pointdata_update_flag ( )
{
   return pointDataUpdateFlag ;
}

inline void clear_pointdata_update_flag ( )
{
   pointDataUpdateFlag = 0 ;
}

#define RIVERSTATUS_REPLY_LEN 100

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:    is_riverstatus_displayed
* PURPOSE:        Returns the a value indicating the state of the
*                 hv_pointdata_display token.  This token takes two values: 
*                 "ON" or "OFF".  If it is ON, then this routine will
*                 return a value of 1.  If it is OFF, then this routine
*                 will return a value of 0. 
*
* ARGUMENTS:
*    None.
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int is_riverstatus_displayed ( )
{
   static char * pPointDisplayTokenName = "hv_pointdata_display" ;
   char reply [ RIVERSTATUS_REPLY_LEN ] ;
   static int first = 1 ;
   int reply_len ;
   int request_len ;
   int status ;
   static int display_pointdata = 1 ;

   if ( first == 1 )
   {
      first = 0 ;

      /* Retrieve the value of the token specifying whether or not
         the display of the point data is supressed. */
      request_len = strlen ( pPointDisplayTokenName ) ;

      status = get_apps_defaults ( pPointDisplayTokenName ,
                                   & request_len ,
                                   reply ,
                                   & reply_len ) ;

      if ( status == 0 )
      {
         if ( strcmp ( reply , "OFF" ) == 0 )
         {
            display_pointdata = 0 ; 
         }  
      }
   }

   return display_pointdata ;
}


/****************************************************************************/




void build_pointdata_table_entry ( ReportList * rPtr , char * buf, 
		                   int is_file )
{
	
   pc_options_struct * pc_options = get_pc_options();
	
   float       flow ;
   float       stage ;
   char        obstime_ansi[ANSI_TIME_LEN+1];
   char        abbrev_time[20];
   char        file_name [LENGTH_OF_FILE_NAME + 1];
   float       flood_level, action_level, flood_depart = 0. ;
   char        action_msg[] = " >ACTION !!";
   char        action_text[ACTION_TEXT_LENGTH];
   char        stageflow_text[ACTION_TEXT_LENGTH];
   int         length;
   int         returnvalue;
   double      dlat, dlon;
   char        name[ALLOWED_NAME_LEN + 1];

   if ( rPtr != NULL && buf != NULL )
   {
      if (rPtr->use)
      {
         /* Null out strings used to create an entry in the
            pointdata tabular GUI. */
         memset ( action_text , '\0' , ACTION_TEXT_LENGTH ) ;
         memset ( stageflow_text , '\0' , ACTION_TEXT_LENGTH ) ;

         if ( ( rPtr->value != MISSING_VAL ) &&
              ( pc_options->time_mode != VALUE_CHANGE ) )
         {
            /* get the riverstat info if a river station
               and use it to set certain fields */
            get_floodinfo ( rPtr->lid , rPtr->pe , & flood_level , 
                            & action_level ) ;

            if (action_level != MISSING_VAL)
            {
               if ( rPtr->value >= action_level )
               {
                  strcpy ( action_text , action_msg ) ;
               }
            }

            if (flood_level != MISSING_VAL)
               flood_depart = rPtr->value - flood_level;

            /* Check if the PE is of type "HG" or "QR".  If it is, then
               attempt to derive a flow value if the PE is "HG" or
               a stage value if the PE is "QR". */
            if ( ( ( rPtr->pe [ 0 ] == 'H' ) &&
                   ( rPtr->pe [ 1 ] == 'G' ) ) ||
                 ( ( rPtr->pe [ 0 ] == 'Q' ) &&
                   ( rPtr->pe [ 1 ] == 'R' ) ) )
            {
               switch ( rPtr->pe [ 0 ] )
               {
                  case 'H' :

                     flow = stage2discharge ( rPtr->lid , rPtr->value ) ;

                     if ( flow != RATING_CONVERT_FAILED )
                     {
                        sprintf ( stageflow_text, "[%#-6.0f]" , flow ) ;
                     }

                     break ;

                 case 'Q' :

                     stage = discharge2stage ( rPtr->lid , rPtr->value ) ;

                     if ( stage != RATING_CONVERT_FAILED )
                     {
                        sprintf ( stageflow_text, "[%-6.2f]" , stage ) ;
                     }

                     break ;

                 default :

                     fprintf ( stderr , "\nIn routine \"load_pointtable\":\n"
                               "Error in switch statement which attempts\n"
                               "to find a derived stage/flow value.\n"
                               "Value %c is invalid.\n" , rPtr->pe [ 0 ] ) ;
                     strcpy ( stageflow_text, "m" ) ;
                     break ;
               }
           }

         }

         /* format the time */
         timet_to_yearsec_ansi(rPtr->validtime, obstime_ansi);
         memset(abbrev_time, 0, 20);
         strncpy(abbrev_time, &obstime_ansi[5], 11);

         /*get name for the lid */
	 memset ( name, '\0', ALLOWED_NAME_LEN + 1 );

         if (rPtr->lid != NULL)
         {
            returnvalue= get_loc_info(rPtr->lid,&dlat,&dlon,name);
         }

         if ( is_file == 1 )
         {
            memset ( file_name, '\0', LENGTH_OF_FILE_NAME + 1);
            strncpy ( file_name, name, LENGTH_OF_FILE_NAME);
         }

         /* write the information */


         if (rPtr->value == MISSING_VAL)
         {
            /* Test if this is for the tabular gui or if it is for the
               the file or printer. Format accordingly. */
            if ( is_file == 1 )
            {
               sprintf(buf, "%-7s %-15s %-6s \n", rPtr->lid, file_name,"m");
            }
            else
            {
               sprintf(buf, "%-7s %-20s %-6s ", rPtr->lid, name,"m");
            }
         }
         else
         {
            if ( ( flood_level != MISSING_VAL ) && 
                 ( pc_options->time_mode != VALUE_CHANGE ) )
            {
               if ( is_file == 1 )
               {
                  sprintf(buf, "%-7s %-15s %-6.2f %-8s %-13s "
                               "%-2s %-2s %ld %s [%6.1f %+5.1f]\n" ,
                               rPtr->lid, file_name,rPtr->value, 
			       stageflow_text, abbrev_time, rPtr->pe, 
			       rPtr->ts, rPtr->dur, rPtr->extremum , 
			       flood_level , flood_depart );

                  length = strlen ( action_text );
    
                  if ( length > 0 )
                  {
                     strcat ( buf, action_text );
                     strcat ( buf, "\n" );
                  }
               }
               else
               {
                  sprintf(buf, "%-7s %-20s %-6.2f %-8s %s %-13s "
                               "%-2s %-2s %ld %s [%6.1f %+5.1f]" ,
                               rPtr->lid, name,rPtr->value, stageflow_text,
                               action_text, abbrev_time, rPtr->pe, rPtr->ts,
                               rPtr->dur, rPtr->extremum , flood_level ,
                               flood_depart );
               }
            }
            else
            {
               if ( is_file == 1 )
               {
                  sprintf(buf, "%-7s %-15s %-6.2f %-8s %-13s %-2s %-2s "
                               "%ld %s\n" ,
                               rPtr->lid, file_name,rPtr->value, 
			       stageflow_text, abbrev_time, rPtr->pe, 
			       rPtr->ts, rPtr->dur, rPtr->extremum);

                  length = strlen ( action_text );
    
                  if ( length > 0 )
                  {
                     strcat ( buf, action_text );
                     strcat ( buf, "\n" );
                  }
               }
               else
               {
                  sprintf(buf, "%-7s %-20s %-6.2f %-8s %s %-13s %-2s %-2s "
                               "%ld %s" ,
                               rPtr->lid, name,rPtr->value, stageflow_text,
                               action_text, abbrev_time, rPtr->pe, rPtr->ts,
                               rPtr->dur, rPtr->extremum);
               }
            }
         }
      }
   }
}


void get_serv_bkup_info ( char * responsible_hsa_list ) 
{
   strcpy ( responsible_hsa_list , responsible_hsa ) ;
   return ;
}

void set_serv_bkup_info ( const char * responsible_hsa_list )
{
   pc_options_struct * pc_options = get_pc_options();
	
   strcpy ( responsible_hsa , responsible_hsa_list ) ;
   strcpy ( pc_options->hsa_list , responsible_hsa_list ) ;
   return ;
}


// -------------------------------------------------------------------------------------------- 

void printReportObs(ReportList * repPtr, char *header, char * message)
{
    if (repPtr != NULL)
    {
        printf("%s %s ListCount = %d \n", header, message, ListCount(&repPtr->list));              
    }     
    else
    {
        printf("%s %s is NULL \n", header, message);   
    }
    
    return;
}
 
// -------------------------------------------------------------------------------------------- 


void printRiverStatus(RiverStatus * rsPtr, char *header, char * message)
{
    if (rsPtr != NULL)
    {
        printf("%s %s ListCount = %d \n", header, message, ListCount(&rsPtr->list));              
    } 
    else
    {
        printf("%s %s is NULL \n", header, message);   
    }    
    
    return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
 
// -------------------------------------------------------------------------------------------- 

 

