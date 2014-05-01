/*******************************************************************************
 *
* FILENAME:             pointcontrol_derive.c
* NUMBER OF MODULES:    18
* GENERAL INFORMATION:
*   MODULE 1:  derive_reportsRiver
* DESCRIPTION: Processes a list of river data.  Use typesource ranking to
*              eliminate multiple typesources.
*   MODULE 2:  process_lid_rs
* DESCRIPTION: For a block of RiverStatus reports with duplicate lids
*              the "best" report is retrieved and inserted into the
*              ReportList linked list.  This best report is selected using
*              the user-specified PE and TS and IngestFilter table TS-ranking.
*   MODULE 3:  process_lid_obsriv
* DESCRIPTION: For a block of Observation reports with duplicate lids
*              the "best" report is retrieved and inserted into the
*              ReportList linked list.  This best report is selected using
*              the user-specified PE and Ts and IngestFilter table TS-ranking. 
*   MODULE 4:  derive_report_rs
* DESCRIPTION: To select the best RiverStatus report out of a block
*              of RiverStatus reports with duplicate lids.  TS ranking
*              is used to determine which report is the best one.
*   MODULE 5:  derive_report_obsriv
* DESCRIPTION: To select the best Observation out of a block of river 
*              observations with the same lid.  The TS rank from the
*              IngestFilter table is used to do this.  For certain time
*              modes (SETTIME, CHANGE), the observation closest to the
*              set time or change interval begin/end times are searched for. 
*   MODULE 6:  compare_rs_info 
* DESCRIPTION: This routine is supplied to the binary search program.  It
*              compares the lid the user is looking for with the lids in
*              the RiverStat linked list.  It enables the binary search to
*              quickly locate the lid the user wants.
*
*   MODULE 7:  get_rs_info
* DESCRIPTION: Retrieves the RiverStat IHFS database table entry for a given
*              station.
*
*   MODULE 8:  derive_reportsRain
* DESCRIPTION: For a pe of PC or PP, this routine computes rain amounts over
*              the user-specified duration. Precipitation may come from
*              several different typesources.
*
*   MODULE 9:  derive_pointcontrol_precip
* DESCRIPTION: This routine is called by derive_reportsRain. For a given
*              lid, pe, duration, ts, combination it tallies a rainfall
*              amount. It also tracks of the number of hours in the duration
*              which actually had data.
*   MODULE 10: load_precip_report
* DESCRIPTION:
*   MODULE 11: derive_reportsOther
* DESCRIPTION:
*   MODULE 12: process_lid_obs
* DESCRIPTION:
*   MODULE 13: process_lid_lat
* DESCRIPTION:
*   MODULE 14: derive_report_obs
* DESCRIPTION:
*   MODULE 15: derive_report_lat
* DESCRIPTION:
*   MODULE 16: FreeReportList 
* DESCRIPTION:
*   MODULE 17: compare_tsrank
* DESCRIPTION:
*   MODULE 18: get_lidts_precip_count
* DESCRIPTION:
*
* ORIGINAL AUTHOR:    Unknown
* CREATION DATE:      January 6, 2004
* ORGANIZATION:       HSEB / OHD
* MACHINE:            Redhat Linux
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER      DESCRIPTION/REASON
*     1-18          1/6/2004     Bryon Lawrence  Updated documentation.  This
*                                                is the result of an 
*                                                effort to separate the 
*                                                pointcontrol library into
*                                                the data processing and
*                                                gui components. 
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "BinarySearch.h"
#include "CurPC.h"
#include "CurPP.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "get_total_precip.h"
#include "IngestFilter.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_derive.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_report.h"
#include "QualityCode.h"
#include "RiverStatus.h"
#include "Riverstat.h"
#include "time_convert.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   derive_reportsRiver
* PURPOSE:       This routine process river data from the RiverStatus table
*                or the Height and Discharge tables.  It looks for blocks of
*                river reports with the same lid.  It eliminates these 
*                multiple reports by using type source ranking from the
*                Ingest table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME        DESCRIPTION/UNITS
*
*   I      pc_options_struct * pc_options  Contains the user-selected options
*                                          on the PDC GUI.  
*   I      Observation 	     * obshHead    Contains observed height data.
*   I      Observation	     * obsdHead    Contains observed discharge data.
*   I      RiverStatus	     * rsHead      Contains river height/discharge data
*                                          read from the RiverStatus table
*                                          (for queries retrieving the latest
*                                           river data).
*   O      ReportList	    ** obsrepHeadDPtr  Processed river height/discharge
*                                          observations are returned 
*                                          in this linked list.
*   O      ReportList	    ** fcstrepHead Processed river height/discharge
*                                          forecasts are returned in this
*                                          linked list.
*
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*  NAME                   HEADER FILE           DESCRIPTION
*  get_change_hour_window pointcontrol_derive.h  Returns the number of hours
*                                                to use to search for 
*                                                a valid report when in 
*                                                the "change" time mode.
*  ListFirst              List.h                 Returns the first node in a
*                                                linked list.
*  ListLast               List.h                 Returns the last node in a
*                                                linked list. 
*  ListNext               List.h                 Returns the next node in a
*                                                linked list
*  ListPrev               List.h                 Returns the previous node in
*                                                a linked list.
*  process_lid_rs         pointcontrol_derive.h  When multiple river reports
*                                                are present for a lid,
*                                                this routine uses TS ranking
*                                                to return only one report.
*                                                This routine is for the
*                                                RiverStatus table. It
*                                                considers both observed
*                                                and forecast river reports.
*  process_lid_obsriv     pointcontrol_derive.h  When multiple river reports
*                                                are present for a lid,
*                                                this routine uses TS ranking
*                                                to return only one report.
*                                                It considers only observed
*                                                river reports. 
*                                           
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME          DESCRIPTION
*   char	  curlid[]      Current lid being processed.
*   char          use_ts[]      Indicates whether to retrieve forecast
*                               or observed data when querying the
*                               riverstatus table.
*   int           change_window The number of hours to search around end times
*                               for  valid reports in the change hour option.
*   int		  lid_count     The number of duplicate lids in the forecast
*                               or observed data list.
*   ReportList	* repfcstPtr    A pointer to the linked list of forecast
*                               data.
*   ReportList	* repobsPtr     A pointer to the liked list of observed data.
*   RiverStatus	* end_rsPtr     The last report in a block of RiverStatus
*                               reports with duplicate lids.
*   RiverStatus	* rsPtr         Used to "walk" through a linked list 
*                               of RiverStatus reports.
*   RiverStatus	* start_rsPtra  The first report in a block of RiverStatus
*                               reports with duplicate lids.
*   Observation	* end_obsPtr    The last report in a block of Observation
*                               reports with duplicate lids.
*   Observation	* obsPtr        Used to "walk" through a linked list of
*                               Observation reports.
*   Observation	* start_obsPtr  The first report in a block of Observation
*                               reports with duplicate lids.
*
* DATA FILES AND/OR DATABASE:
*  Requires the IngestFilter table in the IHFS database.
*
* ERROR HANDLING:
*  None
*
********************************************************************************
*/


 
void derive_reportsRiver(const pc_options_struct * pc_options,
			 Observation 		*obshHead,
			 Observation		*obsdHead,
			 RiverStatus		*rsHead,
			 ReportList		**obsrepHeadDPtr,
			 ReportList		**fcstrepHead)
{
 //  char header[] = "derive_reportsRiver():  ";
   ReportList	* repobsPtr = NULL;
   ReportList * repfcstPtr = NULL;
   char		   use_ts[3];
   RiverStatus	*rsPtr = NULL;
   RiverStatus	*start_rsPtr = NULL;
   RiverStatus  *end_rsPtr = NULL;
   Observation	*obsPtr = NULL;
   Observation	*start_obsPtr = NULL;
   Observation  *end_obsPtr = NULL;
   char		curlid[LOC_ID_LEN + 1] = "";
   int      change_window = 0 ;
   int		lid_count;
   
   /* initialize. for sanity sake, use local variables for these
      pointers, then assign them at the end. */
   
   *obsrepHeadDPtr  = NULL;
   *fcstrepHead = NULL;
   
   /* if no lists have data, return now */
   
   if (!obsdHead && !obshHead && !rsHead)
      return;  
   
   /* if riverstatus has data, then it must be because we are looking
      for the latest data. */
   
   if ( rsHead )
   {      
      /* loop thru the list and process each lid. */
      rsPtr = (RiverStatus *) ListFirst(&rsHead->list);
      
      /* initialize the start of the current block
	 to the first record */
      strcpy(curlid, rsPtr->lid);
      lid_count = 1;
      start_rsPtr = rsPtr;
      end_rsPtr   = NULL;

     // printf("%s  1st one - curlid = %s, rsPtr->lid = %s\n", header, curlid, rsPtr->lid);   

      /* get the next record and start the looping */
      rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
      
      while (rsPtr)
      {
		 /* if the lid does not match, then we have a block of data to
		    process */
            
       //  printf("%s curlid = %s, rsPtr->lid = %s\n", header, curlid, rsPtr->lid);   
		 
		 if (strcmp(curlid, rsPtr->lid) != 0)
		 {	    
		    /* set the end pointer */
		    
		    end_rsPtr = (RiverStatus *) ListPrev(&rsPtr->node);
		    
		    
		    /* for each lid, use the primary pe or the specified pe. 
		       when the pe is found, get the appropriate ts data.
		       if more than one type-source's value for the location, use the
		       ingestfilter type source rank to determine which one to use. */
	   	    
		    /* printf("lid=%s:%d\n", start_rsPtr->lid, lid_count); */
		    
		    strcpy(use_ts, "RP");
		    repobsPtr = process_lid_rs(pc_options, use_ts, 
					       start_rsPtr, end_rsPtr, lid_count,
					       repobsPtr);
                           
                           
           // printReportObs(repobsPtr, header, "repObsPtr ");
		    
		    strcpy(use_ts, "F");
		    repfcstPtr = process_lid_rs(pc_options, use_ts,
						start_rsPtr, end_rsPtr, lid_count,
						repfcstPtr);
		    
		    
		    /* set up data for next pass through the input 
		       linked list */
		    
		    strcpy(curlid, rsPtr->lid);
		    lid_count = 1;
		    start_rsPtr  = rsPtr;
		    end_rsPtr    = NULL;
		 }
		 
		 
		 /* the lids do match so increment the count */
		 
		 else
		 {
		    lid_count++;
		 }
		 
		 
		 /* keep on looping */
		 
		 rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
      } //end while
      
      /* process the leftover block of data at the end of the list */
      
      if (lid_count > 0)
      {
		 end_rsPtr = (RiverStatus *) ListLast(&rsHead->list);
		 
		 strcpy(use_ts, "RP");
		 repobsPtr = process_lid_rs(pc_options, use_ts,
					    start_rsPtr, end_rsPtr, lid_count,
					    repobsPtr);
                        
                        
      //   printReportObs(repobsPtr, header, " in processesing left over block section, repObsPtr ");               
                        
		 strcpy(use_ts, "F");
		 repfcstPtr = process_lid_rs(pc_options, use_ts,
					     start_rsPtr, end_rsPtr, lid_count,
					     repfcstPtr);
            
     //    printReportObs(repobsPtr, header, "after process_lid_rs");
       
      }
      
   } //end if riverstatus has data
 
   /* if riverstatus does not have data, then the height and/or
      discharge lists have the data.  if getting data for the primary pe,
      then data may be loaded in both the Height list and the Discharge
      list, so need to consider both lists. if not for the primary pe,
      only one list will have data. */
   
   else
   {
    
                
   //    printReportObs(repobsPtr, header, "in selected time section after process_lid_rs");
      
    
      /* Get the change hour window if the selected time mode is
         VALUE_CHANGE. */
      if ( pc_options->time_mode == VALUE_CHANGE )
      {
         change_window = get_change_hour_window ( ) ;
         change_window *= 3600 ;
         change_window /= 2 ;
      } 

      if (obshHead)
      {
	 /* loop thru the list and process each lid. */
	 obsPtr = (Observation *) ListFirst(&obshHead->list);
	 
	 /* initialize the start of the current block
	    to the first record */
	 
	 strcpy(curlid, obsPtr->lid);
	 lid_count = 1;
	 start_obsPtr = obsPtr;
	 end_obsPtr   = NULL;
	 
	 /* get the next record and start the looping */
	 
	 obsPtr = (Observation *) ListNext(&obsPtr->node);
	 
	 while (obsPtr)
	 {
	    /* if the lid does not match, then we have a block of data to
	       process */
	    if (strcmp(curlid, obsPtr->lid) != 0)
	    {
	       /* set the end pointer */
	       
	       end_obsPtr = (Observation *) ListPrev(&obsPtr->node);
	       
	       
	       /* for each lid, use the primary pe or the specified pe. 
		  when the pe is found, get the appropriate ts data.
		  if more than one type-source's value for the location, use the
		  ingestfilter type source rank to determine which one to use. */
	       
	       /* printf("lid=%s:%d\n", start_obsPtr->lid, lid_count); */
	       
	       repobsPtr = process_lid_obsriv(pc_options,
					      start_obsPtr, end_obsPtr,
					      lid_count,
                          change_window,
					      repobsPtr);
	       
	       
	       /* set up data for next pass through the input 
		  linked list */
	       
	       strcpy(curlid, obsPtr->lid);
	       lid_count = 1;
	       start_obsPtr  = obsPtr;
	       end_obsPtr    = NULL;
	    }
	    
	    
	    /* the lids do match so increment the count */
	    
	    else
	    {
	       lid_count++;
	    }
	    
	    
	    /* keep on looping */
	    
	    obsPtr = (Observation *) ListNext(&obsPtr->node);
	 }
	 
	 /* process the leftover block of data at the end of the list */
	 
	 if (lid_count > 0)
	 {
	    end_obsPtr = (Observation *) ListLast(&obshHead->list);
	    
	    repobsPtr = process_lid_obsriv(pc_options,
					   start_obsPtr,
                                           end_obsPtr,
                                           lid_count,
                                           change_window,
					   repobsPtr);
	 }
      }  /* end of if block on obsh Head list */
      
      
      /* repeat same process for observed discharge data */
      
      if (obsdHead)
      {
    	 /* loop thru the list and process each lid. */
    	 
    	 obsPtr = (Observation *) ListFirst(&obsdHead->list);
    	 
    	 /* initialize the start of the current block
    	    to the first record */
    	 
    	 strcpy(curlid, obsPtr->lid);
    	 lid_count = 1;
    	 start_obsPtr = obsPtr;
    	 end_obsPtr   = NULL;
    	 
    	 
    	 /* get the next record and start the looping */
    	 
    	 obsPtr = (Observation *) ListNext(&obsPtr->node);
	 
    	 while (obsPtr)
    	 {
    	    /* if the lid does not match, then we have a block of data to
    	       process */
    	    
    	    if (strcmp(curlid, obsPtr->lid) != 0)
    	    {	    
    	       /* set the end pointer */
    	       
    	       end_obsPtr = (Observation *) ListPrev(&obsPtr->node);
    	       
    	       
    	       /* for each lid, use the primary pe or the specified pe. 
    		  when the pe is found, get the appropriate ts data.
    		  if more than one type-source's value for the location, use the
    		  ingestfilter type source rank to determine which one to use. */
    	       
    	       /* printf("lid=%s:%d\n", start_obsPtr->lid, lid_count); */
    	       
    	       repobsPtr = process_lid_obsriv(pc_options,
    					      start_obsPtr, end_obsPtr,
    					      lid_count,
                              change_window,
    					      repobsPtr);
    	       
    	       
    	       /* set up data for next pass through the input 
    		  linked list */
    	       
    	       strcpy(curlid, obsPtr->lid);
    	       lid_count = 1;
    	       start_obsPtr  = obsPtr;
    	       end_obsPtr    = NULL;
    	    }
    	    
    	    
    	    /* the lids do match so increment the count */
    	    
    	    else
    	    {
    	       lid_count++;
    	    }
    	    
    	    
    	    /* keep on looping */
    	    
    	    obsPtr = (Observation *) ListNext(&obsPtr->node);
    	 }
	 
	 
    	 /* process the leftover block of data at the end of the list */
    	 
    	 if (lid_count > 0)
    	 {
    	    end_obsPtr = (Observation *) ListLast(&obsdHead->list);
    	    
    	    repobsPtr = process_lid_obsriv(pc_options, 
    					   start_obsPtr, end_obsPtr, lid_count,
    					   change_window, repobsPtr);
    	 }
      }  /* end of if block on obsd Head list */
      
   } /* end of else block */
   
   /* now assign the returned variables */
   
   *obsrepHeadDPtr  = repobsPtr;
   *fcstrepHead = repfcstPtr;
   
   
   return;  
}


/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   process_lid_rs
* PURPOSE:       This routine searches a block of RiverStatus river reports
*                that have duplicate lids.  It attempts to find the best
*                report out of the group using TS-ranking information
*                in the IngestFilter table.  This best report is inserted
*                into the output report list which is ultimately used
*                to display and tabulate pointdata.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME       DESCRIPTION/UNITS
*   I      pc_options_struct * pc_options Contains the user-selected options
*                                         from the Point Data Control GUI.
*   I      char		     * use_ts     Contains the typesource to use
*                                         in selecting the best RiverStatus
*                                         report.
*   I      RiverStatus       * startPtr   Points to the beginning of the
*                                         block of RiverStatus reports
*                                         with duplicate lids.
*   I      RiverStatus 	     * endPtr     Points to the end of the block
*                                         of RiverStatus reports with
*                                         duplicate lids.
*   I      int		       lid_count  This is the number of RiverStatus
*                                         reports contained in the block.
*   I/O    ReportList	     * inputPtr   Points to the head of the list
*                                         of reports in which the bestPtr
*                                         will be inserted.
*                                         
* RETURNS:
*   DATA TYPE   NAME    DESCRIPTION
*   ReportList  bestPtr The best report in the block of RiverStatus 
*                       reports.
*
* APIs UTILIZED:
*   NAME             HEADER FILE           DESCRIPTION
*   derive_report_rs pointcontrol_derive.h Finds the best report with the
*                                          best TS for a group of PEs. 
*   ListAdd          List.h                Add a node to a linked list.
*   ListInit         List.h                Initialize a linked list.
*
* LOCAL DATA ELEMENTS:
*  DATA TYPE      NAME         DESCRIPTION
*  int            status       Contains the exit code of the call to routine
*                              yearsec_dt_to_timet
*  ReportList	* onereportPtr The contents of the best RiverStatus report
*                              are copied into this dynamically created
*                              ReportList structure.  This structure is
*                              then added to the the list of report structures
*                              pointed to by inputPtr.   
*  ReportList	* outputPtr    Either points to the best report or, if one
*                              could not be found, the head node in the linked
*                              list of reports.
*  RiverStatus  * bestPtr      Points to the best report.
*
* DATA FILES AND/OR DATABASE:
*  This routine uses the IngestFilter table in the IHFS database.
*
* ERROR HANDLING:
*   If a best report cannot be found, then this routine will return a pointer
*   to the head of the unmodified linked list of river reports. 
********************************************************************************
*/

ReportList * process_lid_rs(const pc_options_struct *pc_options,
			    char		*use_ts,
			    RiverStatus		*startPtr, 
			    RiverStatus 	*endPtr,
			    int			lid_count,
			    ReportList		*inputPtr)
{
   RiverStatus  * bestPtr = NULL ;
   ReportList	* onereportPtr = NULL;
   ReportList	* outputPtr = NULL;
   int		status;
  // char header[] = "process_lid_rs(): ";
  
   
   /* get the best value for this location from the
      set of records.  The pointer returned is of 
      the type RiverStatus. it is possible that no data are found
      because this functions handles requests for both
      observed and forecast data, and usually forecast data are
      not provided. */
   
   bestPtr = derive_report_rs(pc_options, use_ts,
			      startPtr, endPtr, lid_count);
   
   
   /* if data was found, then copy the best report into the
      linked list of Reports */
   
   if (bestPtr)
   {
    
     //  printf("%s bestPtr is not NULL for startPtr lid = %s, pe = %s ts = %s\n",
     //            header, startPtr->lid, startPtr->pe, startPtr->ts);
  
    
      if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
      {      
	     /* copy fields */
	 
    	 strcpy(onereportPtr->lid, bestPtr->lid);
    	 strcpy(onereportPtr->pe,  bestPtr->pe);
    	 onereportPtr->dur =       bestPtr->dur;
    	 strcpy(onereportPtr->ts,  bestPtr->ts);
    	 strcpy(onereportPtr->extremum, bestPtr->extremum);
    	 onereportPtr->probability =    bestPtr->probability;
    	 
    	 strcpy(onereportPtr->shef_qual_code, "Z");
    	 onereportPtr->quality_code =         DEFAULT_QC_VALUE;
    	 
    	 onereportPtr->value = bestPtr->value;
    	 status = yearsec_dt_to_timet(bestPtr->validtime,
    				      &onereportPtr->validtime);
    	 status = yearsec_dt_to_timet(bestPtr->basistime,
    				      &onereportPtr->basistime);
    	 
	 
    	 /* initialize the list */
    	 
    	 if (inputPtr == NULL)
    	 {
    	    outputPtr = onereportPtr;
    	    ListInit(&outputPtr->list);
            
            
        //    printf("%s outputPtr reinitialized when bestPtr->lid = %s \n",
        //            header, bestPtr->lid);
    	 }
    	 
    	 else
         {
    	    outputPtr = inputPtr;
         }
    	 
    	 /* add the data to the list */
    	 
    	 ListAdd(&outputPtr->list, &onereportPtr->node);
          
      }
      
      else
      {
	      fprintf(stderr, "Error allocating for ReportList riverstatus.\n");
      }
   }
   
   /* no data was found, so assign pointer and return */
   else
   {
      
      outputPtr = inputPtr;
   }
   
   return outputPtr ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   process_lid_obsriv
* PURPOSE:       Processes the river observations.  For a block of river
*                observations with the same lid, determine which is the best
*                using the TS ranking from the Ingest Table.
*          
*                The best river observation is then copied into the list
*                of Reports.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME           DESCRIPTION/UNITS
*    I     pc_options_struct * pc_options     Contains the options 
*                                             selected by the user
*                                             on the point data control
*                                             GUI.
*    I     Observation	     * startPtr       This points to the first
*                                             observation in the block of
*                                             observations with duplicate lids.
*    I     Observation 	     * endPtr         This points to the last 
*                                             observation in the block of
*                                             observations with duplicate
*                                             lids.
*    I     int		       lid_count      The number of observations in
*                                             the block with the same lid.
*    I     int                 change_window  The number of hours around an
*                                             end time to search for 
*                                             observations in - used in the
*                                             change hour mode.
*    I/O   ReportList        * inputPtr       Points to the report list into
*                                             which the best observation will
*                                             be copied.
*
* RETURNS:
*   DATA TYPE    NAME       DESCRIPTION
*   ReportList * outputPtr  A pointer to the head node in the ReportList. 
*
* APIs UTILIZED:
*   NAME                  HEADER FILE           DESCRIPTION
*   derive_report_obsriv  pointcontrol_derive.h Find the best observation
*                                               for a given lid and store
*                                               it in the report list.
*   ListAdd               List.h                Add a node to a linked list.
*   ListInit              List.h                Initialize a node in a linked 
*                                               list.
*  yearsec_dt_to_timet                          Convert an Informix datetime
*                                               to a UNIX datetime.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME         DESCRIPTION
*   double        change       In the change time mode, this is the change
*                              in the river value over the user-specified
*                              duration.
*   int		  status       The return status of the yearsec_dt_to_timet
*                              routine.
*   Observation * bestPtr      A pointer to the best observation out of the
*                              block of observations with duplicate lids.
*   ReportList	* onereportPtr A pointer to a newly allocate Report structure
*                              to contain the best observation.
*   ReportList	* outputPtr    A pointer to the head of the report list with
*                              the new observation added to it.
*
* DATA FILES AND/OR DATABASE:
*   This routine relies on the IngestFilter database table.
*
* ERROR HANDLING:
*   If an error is encountered, this routine will return the report list
*   back to the caller unchanged.  If memory could not be allocated for the
*   new report list node (onereportPtr) then a diagnostic is printed to the
*   standard error stream.
********************************************************************************
*/
ReportList * process_lid_obsriv ( const pc_options_struct * pc_options,
				  Observation		* startPtr, 
				  Observation 		* endPtr,
				  int			    lid_count,
                 int                change_window,
				  ReportList		* inputPtr)
{
   double       change = MISSING_VAL ;
   int		status;
   Observation  *bestPtr = NULL ;
   ReportList	*onereportPtr = NULL;
   ReportList	*outputPtr = NULL;

   /* get the best value for this location from the
      set of records.  The pointer returned is of 
      the type Observation. It is possible that no data are found
      because this functions handles requests for both
      observed and forecast data, and usually forecast data are
      not provided. */
   bestPtr = derive_report_obsriv(pc_options, startPtr, endPtr, lid_count,
                                  change_window , & change ) ;
                                  
                                  
                                  
                                  
   
   /* If data were found, then copy the best report into the
      linked list of Reports */
   if ( bestPtr )
   {
    //  printf("%s bestPtr is not NULL for startPtr->lid = %s\n", header,startPtr->lid);
    
      if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
      {      
    	 /* copy fields */
    	 
    	 strcpy(onereportPtr->lid, bestPtr->lid);
    	 strcpy(onereportPtr->pe,  bestPtr->pe);
    	 onereportPtr->dur =       bestPtr->dur;
    	 strcpy(onereportPtr->ts,  bestPtr->ts);
    	 strcpy(onereportPtr->extremum, bestPtr->extremum);
    	 onereportPtr->probability =    -1;
    	 
    	 strcpy(onereportPtr->shef_qual_code, "Z");
    	 onereportPtr->quality_code =         DEFAULT_QC_VALUE;
	 
         if ( pc_options->time_mode == VALUE_CHANGE )
         {
	        onereportPtr->value = change;
         }
         else
         {
	         onereportPtr->value = bestPtr->value;
         }
         
    	 status = yearsec_dt_to_timet(bestPtr->obstime,
    				      &onereportPtr->validtime);
    	 onereportPtr->basistime = 0;
	 
    	 /* initialize the list */
    	 if (inputPtr == NULL)
    	 {
    	    outputPtr = onereportPtr;
    	    ListInit(&outputPtr->list);
    	 }
    	 
    	 else
    	    outputPtr = inputPtr;
    	 
    	 
    	 /* add the data to the list */
    	 
    	 ListAdd(&outputPtr->list, &onereportPtr->node);
     }
      
     else
     {
	      fprintf(stderr, "Error allocating for ReportList riverstatus.\n");
     }
  } //end if bestPtr
   
   
   /* no data was found, so assign pointer and return */
   
  else
  {
     // printf("%s bestPtr is  NULL for startPtr->lid = %s\n", header,startPtr->lid);
    
      outputPtr = inputPtr;
   
  }
  return(outputPtr);
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   derive_report_rs
* PURPOSE:       To select the best RiverStatus report out of a block
*                of RiverStatus reports with duplicate lids.  TS ranking
*                is used to determine which report is the best one.
*
* ARGUMENTS:
*   TYPE   DATA TYPE            NAME       DESCRIPTION/UNITS
*   I      pc_options_struct  * pc_options Contains the PDC GUI settings.
*   I      char	              * use_ts     Contains the TS to use when 
*                                          TS ranking.
*   I      RiverStatus        * startPtr   Points to the start of the block
*                                          of RiverStatus records. 
*   I      RiverStatus        * endPtr     Points to the end of the block
*                                          of RiverStatus records.
*   I      int	                lid_count  The number of RiverStatus records
*                                          in the block.
*
* RETURNS:
*   DATA TYPE     NAME     DESCRIPTION
*   RiverStatus * bestPtr  A pointer to the best RiverStatus record in the
*                          block bounded by the startPtr and endPtr pointers. 
*
* APIs UTILIZED:
*   NAME             HEADER FILE           DESCRIPTION
*   compare_tsrank   pointcontrol_derive.h Compares ts ranks from the Ingest
*                                          Filter table to determine the
*                                          highest ranking report.
*   GetIngestFilter  IngestFilter.h        Retrieves records from the 
*                                          IngestFilter table for a given
*                                          PE. 
*
*   get_rs_info      pointcontrol_derive.h
*   ListNext         List.h
* 
* LOCAL DATA ELEMENTS:
*   DATA TYPE        NAME             DESCRIPTION
*   char             best_ts [ ]      Keeps a copy of the best TS found so far.
*   char             cur_ts [ ]       Keeps a copy of the current TS being
*                                     examined.
*   char             prev_lid [ ]     Keeps track of the last station 
*                                     riverstat information was retrieved
*                                     for.  If it is the same as the lid in
*                                     the current report being processed,
*                                     then do not get the riverstat information
*                                     again.
*   char             use_pe [ ]       The physical element use in thinning
*                                     out reports with duplicate lids.
*   char             where [ ]        The where clause for the query to the
*                                     ingest filter table is built in this
*                                     array.
*   IngestFilter *   ingestHead       The results of the query to the ingest
*                                     filter are returned in this linked list.
*   int		     i                A loop index variable.
*   int              record_found     Keeps track of whether or not a matching
*                                     pe was found in the report list.
*   int		     result           Keeps track of the results of the
*                                     compare_tsrank routine. 
*   RiverStatus *    bestPtr          A pointer to the best RiverStatus
*                                     report in the block of RiverStatus
*                                     reports with duplicate lids.
*   RiverStatus	*    rsPtr            Used to examine each node of the
*                                     linked list of RiverStatus reports.
*   rs_info_struct * rs_info          Contains RiverStat information
*                                     for the river station being processed.
*                                     This info includes the primary pe.      
*
* DATA FILES AND/OR DATABASE:
*   This routine uses the IngestFilter table in the IHFS database.
*
* ERROR HANDLING:
*   If an error is encountered, this routine will return the report list
*   back to the caller unchanged.  If memory could not be allocated for the
*   new report list node (onereportPtr) then a diagnostic is printed to the
*   standard error stream.
*
********************************************************************************
*/

RiverStatus * derive_report_rs(const pc_options_struct  * pc_options,
			       char			* use_ts,
			       RiverStatus 		* startPtr,
			       RiverStatus 		* endPtr,
			       int			lid_count)
{
  // char header[] = "derived_report_ts() :";
   char		use_pe[SHEF_PE_LEN + 1];
   char		cur_ts[SHEF_TS_LEN + 1];
   char		best_ts[SHEF_TS_LEN + 1];
   static char  prev_lid [ 9 ] = "\0" ;
   char		where[100];
   IngestFilter	* ingestHead = NULL;
   int		result;   
   int		i, record_found;
   RiverStatus	* rsPtr = NULL ;
   RiverStatus 	* bestPtr = NULL;
   static rs_info_struct	*rs_info = NULL ;
         
   /* determine which physical element to use for this location */
   
   if ( pc_options->Primary )
   {
      /* default primary pe to HG */
      
      strcpy(use_pe, "HG") ;
      
      /* Determine if the rs information was already retrieved
         for this station by a previous call. */
      if ( strcmp ( startPtr->lid , prev_lid ) != 0 )
      {
         strcpy ( prev_lid , startPtr->lid ) ;

         /* try and find the pe specified for this location */
         rs_info = get_rs_info(startPtr->lid);
      }
      
      if (rs_info != NULL)
	 strcpy(use_pe, rs_info->pe);
   }
   
   
   /* explicit pe specified so use it */
   
   else
   {
      memset(use_pe, '\0', SHEF_PE_LEN+1);
      strncpy(use_pe, pc_options->selectedAdHocElementString, SHEF_PE_LEN);
    //  printf("%s use_pe = %s \n", header, use_pe);
      
   }   
   
   /* loop thru the records and find the best type-source to use for the
      physical element. */
    
   record_found = 0;

   rsPtr = startPtr;   

   if ( rsPtr != NULL )
   {
      for (i = 0; i < lid_count; i++)
      {
         /* need to check that the pe matches and that the proper type of
	    type-source is found - i.e. either observed/processed or forecast */
      
         if (strcmp(use_pe, rsPtr->pe) == 0 &&
	     strchr(use_ts, rsPtr->ts[0]) != NULL)
         {
	 
		    /* now that we have a matching record, initialize the best ts */
		    if (!record_found)
		    {
		       strcpy(best_ts, rsPtr->ts);
		       strcpy(cur_ts,  rsPtr->ts);
		       record_found = 1;
		    }
		 	 
		    if (strcmp(rsPtr->ts, cur_ts) != 0)
		    {
		       /* since there is more than one type-source, go get the ingest
		          filter entries for this location for later use.  do not filter
		          the request by duration or extremum, since that is done via
		          the original query, if at all */
		    
		       if (ingestHead == NULL)
		       {
		          sprintf(where, " WHERE lid='%s' AND pe ='%s' ", 
	                          rsPtr->lid, rsPtr->pe);
		          ingestHead = GetIngestFilter(where);
		       
		          if (ingestHead == NULL)
		          {
				     fprintf(stderr, "No ingest filter defined %s\n", where);
				     return(rsPtr);
		          }
		       
		        //  if (DEBUG) printf("multi-ts for rs %s\n", where);
		       }
		    
		    
		       /* compare the ts just found with the best one and
		          set it to the best ts if it has a higher rank */
		    
		       result = compare_tsrank(rsPtr->ts, best_ts, ingestHead);
		       if (result < 0)
		          strcpy(best_ts, rsPtr->ts);
		    
		    
		       /* update for subsequent passes */
		    
		       strcpy(cur_ts, rsPtr->ts);	 
		    }
         }
      
         /* get the next item in the list */
      
         rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
      }
   }
   
   if (ingestHead)
   {
      FreeIngestFilter(ingestHead);
      ingestHead = NULL;
   }
   
   
   /* if the pe was not found, then return with nothing */
   
   if (!record_found)
      return(bestPtr);
   
   
   /* now at this point, we know what pe to use and what 
      the best ts is in the linked list.  loop thru the list
      and get the value for the lid,pe, ts. */
   rsPtr = startPtr;   

   for (i = 0; i < lid_count; i++)
   {      
      if (strcmp(use_pe,  rsPtr->pe) == 0 &&
	  strcmp(best_ts, rsPtr->ts) == 0)
      {
	 /* since the RiverStatus key is lid, pe, ts, once we found a
	    match, we can leave this function.f */
	 
	    bestPtr = rsPtr;
	    break;	 
      }
      
      
       /* get the next item in the list */
      
       rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
   }
   
   return(bestPtr);  
}


/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   derive_report_obsriv
* PURPOSE:       For a list of river observations retrieved from the Height
*                or Discharge tables in the IHFS database, processes a block
*                of observations with the same lid.   The best observation
*                from this block is selected through the use of TS ranking.
*                The TS ranking information is taken from the IngestFilter
*                table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME          DESCRIPTION/UNITS
*   I      pc_options_struct * pc_options    The settings on the Point Data 
*                                            control GUI.
*   I      Observation 	     * startPtr      Points to the start of the 
*                                            block of observations with
*                                            duplicate lids.
*   I      Observation       * endPtr        Points to the end of the block
*                                            of observations with duplicate
*                                            lids.
*   I      int		       lid_count     The number of observations in the
*                                            block.
*   I      int                 change_window The number of hours to search
*                                            around the beginning and ending
*                                            times in the "Change" mode.
*   I      double            * change        The actual change in the value
*                                            over the user specified duration.
*
* RETURNS:
*   DATA TYPE       DESCRIPTION
*   Observation *   Points to the best observation.   If an error occurred,
*                   this will point to the start of the block of observations
*                   with duplicate lids. 
* APIs UTILIZED:
*   NAME                HEADER FILE            DESCRIPTION
*   compare_tsrank      pointcontrol_derive.h
*   FreeIngestFilter    IngestFilter.h
*   GetIngestFilter     IngestFilter.h
*   get_rs_info         pointcontrol_derive.h
*   ListNext            List.h
*   yearsec_dt_to_timet time_convert.h 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE        NAME              DESCRIPTION
*   char	     use_pe [ ]        The PE to use in finding the best
*                                      observation.
*   char	     cur_ts [ ]        The current TS being examined.
*   char	     best_ts [ ]       The best TS found so far.
*   char	     where [ ]         Used to supply the where clause
*                                      when querying the IngestTable for
*                                      typesource information.
*   IngestFilter *   ingestHead        Points to the IngestFilter entries
*                                      for the station lid being processed.
*   int              first_obs_found   Indicates if the first observation of 
*                                      two required for computing a change 
*                                      has been found.
*   int              i                 Loop index.
*   int              last_obs_found    Indicates if the last observation of
*                                      two required for computing a change
*                                      has been found.
*   int              record_found      Indicates if a match for the PE in
*                                      "use_pe" has been found in the
*                                      linked list of Observations.
*   int		     result            Contains the result of the call to
*                                      compare_tsrank.  Determines which of
*                                      two TS values in the "best" based on
*                                      IngestFilter TS ranking.
*   int		     status            Contains the result of the
*                                      conversion of the Informix-formatted
*                                      year to second observation time to 
*                                      a UNIX ticks (time_t) value.
*   time_t           best_diff         Used in the SETTIME and CHANGE time
*                                      modes to find which observation 
*                                      obstime is closest to the set time.
*   time_t	     cur_diff          The absolute difference
*                                      between the current observation obstime
*                                      and the set time. 
*   time_t	     curtimet          Contains the Unix ticks representation
*                                      of the observations obstime.  
*   Observation	*    obsPtr            Used to "walk" through a linked list
*                                      of Observations. 
*   Observation *    bestPtr           Points to the best observation.
*   Observation	*    bestPtr2          When computing a change in a value,
*                                      points to the ending time best 
*                                      observation.
*   rs_info_struct * rs_info           Used to reference the river stat
*                                      information for a station.
*
* DATA FILES AND/OR DATABASE:
*   Uses the IngestFilter table in the IHFS database for TS ranking
*   information.  Uses the RiverStat table in the IHFS database for
*   primary pe information.
*   
*
* ERROR HANDLING:
*    ERROR CODE                        DESCRIPTION
*    If an error occurs,  a pointer to the the observation pointed to by
*    startPtr is returned to the caller.  Otherwise, the "best" observation
*    is returned to the caller based on TS ranking.
********************************************************************************
*/
Observation * derive_report_obsriv(const pc_options_struct * pc_options,
				   Observation 		* startPtr, 
				   Observation 		* endPtr,
				   int			  lid_count,
                   int            change_window,
                   double         * change )
{
   int          first_obs_found = 0 ;
   int          last_obs_found = 0 ;
   char		use_pe[SHEF_PE_LEN + 1];
   char		cur_ts[SHEF_TS_LEN + 1];
   char		best_ts[SHEF_TS_LEN + 1];
   char		where[100];
   IngestFilter	*ingestHead = NULL;
   int		result;   
   time_t	curtimet;
   time_t	cur_diff, best_diff;  
   int		i, status, record_found;
   Observation	*obsPtr = NULL ;
   Observation 	*bestPtr = NULL ;
   Observation 	*bestPtr2 = NULL ;
   rs_info_struct	*rs_info = NULL ;
         
   /* Initialize the change value to be missing. This prevents subsequent
      passes through this routine from using the same value for different
      stations. */ 
   * change = MISSING_VAL ;

   /* determine which physical element to use for this location */
   if (pc_options->Primary)
   {
      /* default primary pe to HG */
      strcpy(use_pe, "HG");
      
      
      /* try and find the pe specified for this location */
      rs_info = get_rs_info(startPtr->lid);
      if (rs_info != NULL)
	 strcpy(use_pe, rs_info->pe);
   }
   
   
   /* explicit pe specified so use it */
   
   else
   {
      memset(use_pe, '\0', SHEF_PE_LEN + 1);
      strncpy(use_pe, pc_options->selectedAdHocElementString, SHEF_PE_LEN);
      
   }
   
   /* loop thru the records and find the best type-source to use for the
      physical element. */
    
   record_found = 0;

   obsPtr = startPtr;   

   if ( obsPtr != NULL )
   {
       
      for (i = 0; i < lid_count; i++)
      {
         /* need to check that the pe matches */
      
         if (strcmp(use_pe, obsPtr->pe) == 0)
         {
	 
    	    /* now that we have a matching record, initialize the best ts */
    	    
    	    if (!record_found)
    	    {
    	       strcpy(best_ts, obsPtr->ts);
    	       strcpy(cur_ts,  obsPtr->ts);
    	       record_found = 1;
    	    }
	 	 
    	    if (strcmp(obsPtr->ts, cur_ts) != 0)
    	    {
    	       /* since there is more than one type-source, go get the ingest
    	          filter entries for this location for later use.  do not 
    	          filter the request by duration or extremum, since that is 
    	          done via the original query, if at all */
    	    
    	       if (ingestHead == NULL)
    	       {
    	          sprintf(where, " WHERE lid='%s' AND pe ='%s' ", 
    	      	          obsPtr->lid, obsPtr->pe);
    	          ingestHead = GetIngestFilter(where);
    	       
    	          if (ingestHead == NULL)
    	          {
        		     fprintf(stderr, "No ingest filter defined %s\n", where);
        		     return(obsPtr);
    	          }
    	       
    	       //   if (DEBUG) printf("multi-ts for obsriv %s\n", where);
    	       }
    	    
    	    
    	       /* compare the ts just found with the best one and
    	          set it to the best ts if it has a higher rank */
    	    
    	       result = compare_tsrank(obsPtr->ts, best_ts, ingestHead);
    	       if (result < 0)
    	          strcpy(best_ts, obsPtr->ts);
    	    
    	    
    	       /* update for subsequent passes */
    	    
    	       strcpy(cur_ts, obsPtr->ts);	 
    	    }
         }
      
         /* get the next item in the list */
      
         obsPtr = (Observation *) ListNext(&obsPtr->node);
      }
   }
   
   if (ingestHead)
   {
      FreeIngestFilter(ingestHead);
      ingestHead = NULL;
   }
   
   /* if the pe was not found, then return with nothing */
   if (!record_found)
   {
      return(bestPtr);
   }
   
   /* now at this point, we know what pe to use and what 
      the best ts is in the linked list.  loop thru the list,
      considering only those that match this pe and ts */

   best_diff = MISSING_VAL;
   
   obsPtr = startPtr;

   for (i = 0; i < lid_count; i++)
   {      
      if (strcmp(use_pe,  obsPtr->pe) == 0 &&
	  strcmp(best_ts, obsPtr->ts) == 0)
      {
         switch ( pc_options->time_mode )
         {
            case LATEST :
            case MAXSELECT :
            case MINSELECT :

               /* for all but the SETTIME and VALUE_CHANGE modes, use the first
                  value, which is the latest. */
               return obsPtr ;
               break ;

            case SETTIME :

    	       status = yearsec_dt_to_timet ( obsPtr->obstime, &curtimet ) ;
    	       cur_diff = abs ( pc_options->valid_timet - curtimet ) ;
    	    
    	       if ( best_diff == MISSING_VAL || cur_diff < best_diff )
    	       {
    	          best_diff = cur_diff;
    	          bestPtr = obsPtr;
    	       }
    	    
    	       if ( best_diff == 0 )
               {
                   return bestPtr ;
               }
    
               break ;

            case VALUE_CHANGE :
               
               /* Find the best observation in the upper time window. */
               if ( first_obs_found == 0 )
               {
    	          status = yearsec_dt_to_timet(obsPtr->obstime, &curtimet);
    	          cur_diff = abs(pc_options->valid_timet - curtimet);

                  /* Is the observation inside the window? */
                  if ( cur_diff <= change_window )
                  { 
                     /* Is the observation the closest to the center
                        time so far? */
                     if ( ( best_diff == MISSING_VAL ) || 
                          ( cur_diff <= best_diff ) )
                     {
                        best_diff = cur_diff ;
                        bestPtr = obsPtr ;
                     }
                     else
                     {
                        first_obs_found = 1 ;
                        best_diff = MISSING_VAL ;

            			/* Retain the current observation so that it
            			   can be tested against the lower time bound of
            			   the change interval. */ 
            			-- i ;
                        continue ;
                     }
                  }
                  else
                  {

                     if ( best_diff != MISSING_VAL )
                     {
                        first_obs_found = 1 ;
                        best_diff = MISSING_VAL ;

                        /* Retain the current observation so that it can
                           be tested against the lower time bound of
                           the change interval. */ 
                        -- i ;
                        continue ;
                     }
                  }
               }
               else if ( last_obs_found == 0 )
               {
    	          cur_diff = abs ( pc_options->valid_timet - curtimet ) ;
                      /* Find the best observation in the lower time window. */
    	          status = yearsec_dt_to_timet(obsPtr->obstime, &curtimet);
    	          cur_diff = abs ( ( pc_options->valid_timet -
                                       ( pc_options->dur_hours * 3600 ) -
                                       curtimet ) ) ;

                  /* Is the observation inside the window? */
                  if ( cur_diff <= change_window )
                  { 
                     /* Is the observation the closest to the center
                        time so far? */
                     if ( ( best_diff == MISSING_VAL ) || 
                          ( cur_diff <= best_diff ) )
                     {
                        best_diff = cur_diff ;
                        bestPtr2 = obsPtr ;
                     }
                     else
                     {
                        last_obs_found = 1 ;
                     }
                  }
                  else
                  {
                     if ( best_diff != MISSING_VAL )
                     {
                        last_obs_found = 1 ;
                     }
                  } 

               }
               else
               {
                  /* Both observations necessary to compute the change
                     have been found. */
                  * change = bestPtr->value - bestPtr2->value ;
                  return bestPtr ;
               }
                
               break ;

            default : 
               break ;
         }

      }
      
      /* get the next item in the list */
      
      obsPtr = (Observation *) ListNext(&obsPtr->node);
   }
   
   if ( pc_options->time_mode == VALUE_CHANGE )
   {
      if ( first_obs_found == 1 &&
           best_diff != MISSING_VAL )
      {
         * change = bestPtr->value - bestPtr2->value ;
      }
   } 
   
   
   return bestPtr ;  
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   compare_rs_info
* PURPOSE:       Supplied to for the use of the Binary Search routines.
*                It compares the user-supplied lid with the lids in the
*                linked list of RiverStat information.  It enables the
*                Binary Search algorithm to quickly locate the lid and
*                its associated RiverStat information.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   Input  void *      search_value A pointer to the value being searched for.
*   Input  void *      array_value  A pointer to the array element being
*                                   checked.
*
* RETURNS:
*   DATA TYPE  DESCRIPTION
*   int        A value indicating the following:
*              < 0 : The search value is smaller than the array value. 
*              0   : The search value equals the array value.
*              > 0 : The search value is greater that the array value.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE        NAME                DESCRIPTION
*   char *           pSearchValue        The value being searched for. 
*   rs_info_struct * pArrayValue         The value in current binary search
*                                        array element.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static int compare_rs_info ( void * search_value , void * array_value )
{
   char * pSearchValue = ( char * ) search_value ;
   rs_info_struct * pArrayValue = ( rs_info_struct * ) array_value ;

   return ( strcmp ( pSearchValue , pArrayValue->lid) ) ;
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   get_rs_info
* PURPOSE:       Retrieves the RiverStat IHFS database table entry for a given
*                station.
*
*                The first time this routine is called, it buffers the
*                RiverStat table entries into an array of rs_info_structs
*                in memory.  This array is available for subsequent calls to
*                this routine, saving time lost in querying the database.
*
*                The user passes in the lid of the station that RiverStat data 
*                is desired for.  Since the elements in the array of
*                rs_info_structs are ordered by lid in ascending order from
*                A to Z, an efficient binary search can be used to quickly
*                locate the river stat data corresponding to the user-supplied
*                lid.
*
*                This routine will be especially slow the first time it is
*                called because it is retrieving large amounts of data from
*                the database.   Subsequent calls should be very fast since
*                a binary search operates at a big O notation of log N, where
*                N is the number of items in the array.  This routine
*                will especially benefit users who need to retrieve river stat
*                data for a large number of stations.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME  DESCRIPTION/UNITS
*   Input  char *      lid   The location identifier of the station
*                            the user wants to retrieve river stat
*                            data for.
* RETURNS:
*   DATA TYPE         NAME           DESCRIPTION
*   rs_info_struct *  rs_info_return Contains the river stat data for
*                                    the user-supplied lid.
* APIs UTILIZED:
*   NAME            HEADER FILE     DESCRIPTION
*   binary_search   BinarySearch.h  Uses the binary search algorithm
*                                   to quickly find an item in an array
*                                   of order elements. 
*   FreeRiverstat   RiverStat.h     Deallocates the memory used for the 
*                                   linked list of river stat data 
*                                   retrieved from the RiverStat table in the
*                                   IHFS database.
*   GetRiverstat    RiverStat.h     Retrieves a linked list of river stat
*                                   data from the RiverStat table in the
*                                   IHFS database.
*   ListCount       List.h          Returns a count of the nodes in a linked
*                                   list.
*   ListFirst       List.h          Returns a reference to the first node in
*                                   a linked list.
*   ListNext        List.h          Returns a reference to the next node in
*                                   a linked list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int			first = 1 ;
*   int				i;
*   int			rs_count;
*   Riverstat			*stationHead, *stationPtr;
*   rs_info_struct	*rs_info = NULL ;
*   rs_info_struct		*rs_info_return = NULL;
*
* DATA FILES AND/OR DATABASE:
*   Reads the RiverStat table in the IHFS database.
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

rs_info_struct * get_rs_info ( char * lid )
{
   static int			first = 1;
   static rs_info_struct	*rs_info = NULL;
   static int			rs_count;
   
   rs_info_struct		*rs_info_return = NULL;
   Riverstat			*stationHead, *stationPtr;
   int				i;
   
   
   /* buffer the lookup info the first time thru */
   
   if (first)
   {
      /* get the RiverStat records and extract the lid and primary_pe
	 fields from it. copying into a local structure is done to save
	 some space since the RiverStat record is 550 bytes, but we
	 only need 26 bytes. */
      
      stationHead = GetRiverstat(" ORDER BY lid ") ;
      
      if (stationHead)
      {
	 rs_count = ListCount(&stationHead->list);
	 if (DEBUG) printf("read %d riverstat records.\n", rs_count);
	 
	 rs_info = (rs_info_struct *)
	    malloc(sizeof(rs_info_struct) * rs_count);
	 stationPtr = (Riverstat *) ListFirst(&stationHead->list);	    

	 for (i = 0; i < rs_count; i++)
	 {
	    strcpy(rs_info[i].lid, stationPtr->lid);
	    strcpy(rs_info[i].pe,  stationPtr->primary_pe);
	    
	    if (IsNull(DOUBLE, &stationPtr->fq) == NOTNULL && stationPtr->fq != 0.0)
	       rs_info[i].fq = stationPtr->fq;
	    else
	       rs_info[i].fq = MISSING_VAL;
	    
	    if (IsNull(DOUBLE, &stationPtr->fs) == NOTNULL && stationPtr->fs != 0.0)
	       rs_info[i].fs = stationPtr->fs;
	    else
	       rs_info[i].fs = MISSING_VAL;
	       
	    if (IsNull(DOUBLE, &stationPtr->action_flow) == NOTNULL &&
		stationPtr->action_flow != 0.0)
	       rs_info[i].aq = stationPtr->action_flow;
	    else
	       rs_info[i].aq = MISSING_VAL;
	    
	    if (IsNull(DOUBLE, &stationPtr->wstg) == NOTNULL && stationPtr->wstg != 0.0)
	       rs_info[i].as = stationPtr->wstg;
	    else
	       rs_info[i].as = MISSING_VAL;
	    
	    stationPtr = (Riverstat *) ListNext(&stationPtr->node);
	 }
	 
	 FreeRiverstat(stationHead);
	 stationHead = NULL;
      }

      first = 0;
   }
   
   
   /* now get the info for the requested station */
   
   rs_info_return = ( rs_info_struct * ) 
                    binary_search ( rs_info ,
                                    lid ,
                                    rs_count ,
                                    sizeof ( rs_info_struct) ,
                                    compare_rs_info ) ;
   return ( rs_info_return ) ;   
   
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*________________________________________________________________________*/
/*___ Processing Precip Data in Two Precip Linked Lists __________________*/
/*________________________________________________________________________*/

/*****************************************************************************
   derive_reportsRain()
   ***************************************************************************/

ReportList * derive_reportsRain(const pc_options_struct 	*pc_options,
				CurPC 		        *pcHead,
				CurPP		        *ppHead)
{
   static char * min_dur_token = "hv_min_dur_filled";
   char           reply [ 20 ];
   CurPC        * pcPtr = NULL ; 
   CurPP        * ppPtr = NULL ;
   double       elapsed_time ;
   static int   first = 1 ; 
   int          num_pc_records ;
   int          num_pp_records ;
   int          reply_len ;
   int          request_len ;
   int          status ;
   static float min_percent = 0.0 ;
   ReportList	*reportPtr = NULL;
   short int    advance = 1 ;
   short int    exact_match_window = EXACT_ENDINGTIME_MATCH ;
   struct timeval btime ;
   struct timeval etime ; 
   struct tm    *data_tm = NULL ;
   struct total_precip total_precip ;
   time_t	begin_timet, end_timet;	
   time_t	tnow;
   unsigned char precip_settings = PRECIP_PE_BEST | PRECIP_TS_BEST |
                                   REPORT_MISSING_BELOW_MIN_PERCENT ;
   
   /* if neither list has data, return now */
   if ( !pcHead && !ppHead )
   {
      return ( reportPtr ) ;
   }

   if ( first == 1 )
   {
      first = 0;
      request_len = strlen ( min_dur_token ); 
      status = get_apps_defaults ( min_dur_token, & request_len,
                                   reply, & reply_len );

      if ( ( status == 0 ) && ( reply_len > 0 ) ) 
      {
         min_percent = atof ( reply );

         if ( ( min_percent < 0.0 ) || ( min_percent > 1.0 ) )
         {
            min_percent = 0.0;
         }
      }
   }
     
   /* initialize */
   pcPtr = pcHead;
   ppPtr = ppHead; 
   
   /* set the time window accordingly */
   if (pc_options->time_mode == LATEST)
   {
     /* convert the current time_t to tm struct, set the minutes to zero,
        convert back to time_t to get the top of the latest hour. */

      time(&tnow);
      data_tm = gmtime(&tnow);
      data_tm->tm_min = 0;
      data_tm->tm_sec = 0;
      tnow = gm_mktime(data_tm);

      begin_timet = tnow - (pc_options->dur_hours *3600);
      end_timet   = tnow;   
   }
   
   else
   {    
      begin_timet = pc_options->valid_timet - (pc_options->dur_hours *3600);
      end_timet   = pc_options->valid_timet;
   }
 
   
   /* Loop until we are at the end of both lists.  Note that the
      get_total_raw_precip routine uses colooping to advance the 
      pcPtr and ppPtr pointers. */
   gettimeofday ( & btime , NULL ) ;

   while ( ( pcPtr != ( CurPC * ) NULL ) || ( ppPtr != ( CurPP * ) NULL ) )
   {
      /* determine which is the current id to work on.  this is 
	 the id that is first alphanumerically when looking
	 at both of the lists */
	 
	  CurPC **curPCDoublePtr =  & pcPtr;
	  CurPP **curPPDoublePtr =  & ppPtr;
	  	  
      total_precip = get_total_raw_precip ( ( RawPC ** ) curPCDoublePtr ,
                                            ( RawPP ** ) curPPDoublePtr ,
                                            begin_timet ,
                                            end_timet ,
                                            exact_match_window ,
                                            min_percent ,
                                            precip_settings ,
                                            advance ,
		                            & num_pc_records ,
		                            & num_pp_records )  ;
      
      /* load in the record if a valid value was found.
	 some of the fields are derived - i.e. best_hr_filled, best_value,
	 while others are passed in the available */
     
      if ( ( total_precip.percent_filled > 0.0 ) && 
           ( total_precip.value >= 0.0 ) )
      {
	 reportPtr = load_precip_report ( & total_precip , end_timet , 
			                  reportPtr ) ;
      }
      
   }  /* end of while loop of ptrs != NULL */

   gettimeofday ( & etime , NULL ) ;

   elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                  ( double ) ( etime.tv_usec - btime.tv_usec ) / 1000000. ;

   fprintf ( stdout , "   Time to compute rainfall totals: %5.3f\n" ,
                      elapsed_time ) ;
        
   return(reportPtr);
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   load_precip_report()
   
 ***************************************************************************/
ReportList * load_precip_report( const struct total_precip * pTotalPrecip ,
                                 time_t end_timet , ReportList * inputPtr )
{
   ReportList	*onereportPtr = NULL;
   ReportList	*outputPtr = NULL;
         
   /* copy the best report into the linked list of Reports */
   if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
   {      
      /* copy fields */
      
      strcpy(onereportPtr->lid, pTotalPrecip->lid ) ;
      strcpy(onereportPtr->pe,  pTotalPrecip->PE ) ;
      onereportPtr->dur =       ( long ) pTotalPrecip->hours_covered + 1000 ;
      strcpy(onereportPtr->ts,  pTotalPrecip->TS ) ;
      strcpy(onereportPtr->extremum, "Z" ) ;
      onereportPtr->probability = -1;
      
      onereportPtr->shef_qual_code [ 0 ] = pTotalPrecip->qc ;
      onereportPtr->shef_qual_code [ 1 ] = '\0' ;
      onereportPtr->quality_code = DEFAULT_QC_VALUE;
      onereportPtr->value     = pTotalPrecip->value;
      onereportPtr->validtime = end_timet;
      onereportPtr->basistime = 0;
      
      /* initialize the list */
      if (inputPtr == NULL)
      {
	 outputPtr = onereportPtr;
	 ListInit(&outputPtr->list);
      }
      else
	 outputPtr = inputPtr;
      
      
      /* add the data to the list */
      
      ListAdd(&outputPtr->list, &onereportPtr->node);
   }
   
   else
   {
      fprintf(stderr, "Error allocating for ReportList curprecip.\n");
   }
   
   
   return(outputPtr);
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*________________________________________________________________________*/
/*___ Processing "Other" Data in Observation or LatestObsValue List ______*/
/*________________________________________________________________________*/

/*****************************************************************************
   derive_reportsOther()
   Takes a linked list of data, given in either a list of Observation
   records or LatestObsValue records, which contains multiple
   records for each location.  This function runs through this list
   and finds the best value for each location, then returns
   a linked lists of reports.
   
   
   derive_reportsOther
   
   calls either:
   
   --->process_lid_obs
   ------->derive_report_obs
   
      OR
      
   --->process_lid_lat
   ------->derive_report_lat
   
   ***************************************************************************/

ReportList * derive_reportsOther(const pc_options_struct  *	pc_options,
				 Observation 		*obsHead,
				 LatestObsValue		*latHead)
{
   ReportList	*reportPtr = NULL;
   
   Observation	*obsPtr = NULL ;
   Observation	*start_obsPtr = NULL , *end_obsPtr = NULL ;
   
   LatestObsValue	*latPtr = NULL ;
   LatestObsValue	*start_latPtr = NULL , *end_latPtr = NULL ;
   
   char		curlid[LOC_ID_LEN + 1] = "";
   int          change_window = 0 ;
   int		lid_count;
   
   /* if neither list has data, return now */
   
   if (!obsHead && !latHead)
      return(reportPtr);
   
   /* initialize */
   lid_count = 0;
      
   /* If data stored in Observation type linked list.
      loop thru Observation list.  Find beginning and end of each
      set of records associated with a given location. */
   
   if ( obsHead )
   {
      /* Get the change hour window if the selected time mode is
         VALUE_CHANGE. */
      if ( pc_options->time_mode == VALUE_CHANGE )
      {
         change_window = get_change_hour_window ( ) ;
         change_window *= 3600 ;
         change_window /= 2 ;
      } 

      obsPtr = (Observation *) ListFirst(&obsHead->list);
      
      /* initialize the start of the current block
	 to the first record */
      
      strcpy(curlid, obsPtr->lid);
      lid_count = 1;
      start_obsPtr = obsPtr;
      end_obsPtr   = NULL;
      
      
      /* get the next record and start the looping */
      
      obsPtr = (Observation *) ListNext(&obsPtr->node);
      
      while (obsPtr)
      {
	 /* if the lid does not match, then we have a block of data to
	    process */
	 
	 if (strcmp(curlid, obsPtr->lid) != 0)
	 {	    
	    /* set the end pointer */
	    
	    end_obsPtr = (Observation *) ListPrev(&obsPtr->node);
	    
	    
	    /* now proces this set of data for the single lid */
	    
	    reportPtr = process_lid_obs(pc_options, start_obsPtr, end_obsPtr,
					lid_count, change_window, reportPtr);
	    
	    
	    /* set up data for next pass through the input 
	       linked list */
	    
	    strcpy(curlid, obsPtr->lid);
	    lid_count = 1;
	    start_obsPtr  = obsPtr;
	    end_obsPtr    = NULL;
	 }
	 
	 
	 /* the lids do match so increment the count */
	 
	 else
	 {
	    lid_count++;
	 }
	 
	 
	 /* keep on looping */
	 
	 obsPtr = (Observation *) ListNext(&obsPtr->node);
      }
      
      
      /* process the leftover block of data at the end of the list */
      
      if (lid_count > 0)
      {
	 end_obsPtr = (Observation *) ListLast(&obsHead->list);
	 
	 reportPtr = process_lid_obs(pc_options, start_obsPtr, end_obsPtr,
				     lid_count, change_window, reportPtr);
      }
   }
  
   
   
   /* repeat for data stored in LatestObsValue linked list.
      process in a manner identical to the Observation list, although
      in this case, there will be far fewer records per location 
      since only the latest data is being considered. */
   
   if (latHead)
   {
      latPtr = (LatestObsValue *) ListFirst(&latHead->list);
      
      /* initialize the start of the current block
	 to the first record */
      
      strcpy(curlid, latPtr->lid);
      lid_count = 1;
      start_latPtr = latPtr;
      end_latPtr   = NULL;
      
      
      /* get the next record and start the looping */
      
      latPtr = (LatestObsValue *) ListNext(&latPtr->node);
      
      while (latPtr)
      {
	 /* if the lid does not match, then we have a block of data to
	    process */
	 
	 if (strcmp(curlid, latPtr->lid) != 0)
	 {	    
	    /* set the end pointer */
	    
	    end_latPtr = (LatestObsValue *) ListPrev(&latPtr->node);
	    
	    
	    /* now proces this set of data for the single lid */
	    
	    reportPtr = process_lid_lat(pc_options, start_latPtr, end_latPtr,
					lid_count, reportPtr);
	    
	    
	    /* set up data for next pass through the input 
	       linked list */
	    
	    strcpy(curlid, latPtr->lid);
	    lid_count = 1;
	    start_latPtr  = latPtr;
	    end_latPtr    = NULL;
	 }
	 
	 
	 /* the lids do match so increment the count */
	 
	 else
	 {
	    lid_count++;
	 }
	 
	 
	 /* keep on looping */
	 
	 latPtr = (LatestObsValue *) ListNext(&latPtr->node);
      }
      
      
      /* process the leftover block of data at the end of the list */
      
      if (lid_count > 0)
      {
	 end_latPtr = (LatestObsValue *) ListLast(&latHead->list);
	 
	 reportPtr = process_lid_lat(pc_options, start_latPtr, end_latPtr,
				     lid_count, reportPtr);
      }
   }

   
   return(reportPtr);  
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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

/*****************************************************************************
   process_lid_obs()
   
   The process_lid_lat() and proces_lid_obs() functions
   are identical, except that they process two different
   types of linked list information.
   ***************************************************************************/
ReportList * process_lid_obs(const pc_options_struct * pc_options,
			     Observation 	*startPtr, 
			     Observation 	*endPtr,
			     int		lid_count,
                             int                change_window,
			     ReportList		*inputPtr)
{
   double       change = MISSING_VAL ;
   Observation  *bestPtr = NULL ;
   ReportList	*onereportPtr = NULL ;
   ReportList	*outputPtr = NULL ;
   int		status ;
  
   
   /* get the best value for this location from the
      set of records.  The pointer returned is of 
      the type Observation. */
   
   bestPtr = derive_report_obs(pc_options, startPtr, endPtr, 
                               change_window, lid_count, & change);
   
   
   /* copy the best report into the linked list of Reports */
   if ( bestPtr != NULL )
   {
      if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
      {      
         /* copy fields */
      
         strcpy(onereportPtr->lid, bestPtr->lid);
         strcpy(onereportPtr->pe,  bestPtr->pe);
         onereportPtr->dur =       bestPtr->dur;
         strcpy(onereportPtr->ts,  bestPtr->ts);
         strcpy(onereportPtr->extremum, bestPtr->extremum);
         onereportPtr->probability = -1;
      
         strcpy(onereportPtr->shef_qual_code, bestPtr->shef_qual_code);
         onereportPtr->quality_code =         bestPtr->quality_code;
      
         if ( pc_options->time_mode != VALUE_CHANGE )
         {
            onereportPtr->value = bestPtr->value;
         }
         else
         {
            onereportPtr->value = change ;
         }

         status = yearsec_dt_to_timet(bestPtr->obstime,
				   &onereportPtr->validtime);
         onereportPtr->basistime = 0;
      
      
         /* initialize the list */
      
         if (inputPtr == NULL)
         {
	    outputPtr = onereportPtr;
	    ListInit(&outputPtr->list);
         }
      
         else
	    outputPtr = inputPtr;
      
      
         /* add the data to the list */
      
         ListAdd(&outputPtr->list, &onereportPtr->node);
      }
   
      else
      {
         fprintf(stderr, "Error allocating for ReportList observation.\n");
      }
   }
   /* no data was found, so assign pointer and return */
   
   else
   {
      outputPtr = inputPtr;
   }
   
   return(outputPtr);
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   process_lid_lat()
   
   The process_lid_lat() and proces_lid_obs() functions
   are identical, except that they process two different
   types of linked list information.
   ***************************************************************************/
ReportList * process_lid_lat(const pc_options_struct * pc_options,
			     LatestObsValue 	*startPtr, 
			     LatestObsValue 	*endPtr,
			     int		lid_count,
			     ReportList		*inputPtr)
{
   LatestObsValue  	*bestPtr = NULL ;
   ReportList		*onereportPtr = NULL;
   ReportList		*outputPtr = NULL;
   int			status;
  
   /* get the best value for this location from the
      set of records.  The pointer returned is of 
      the type LatestObsValue. */
   
   bestPtr = derive_report_lat(pc_options, startPtr, endPtr, lid_count);

   if ( bestPtr != NULL )
   {
   
      /* copy the best report into the linked list of Reports */
   
      if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
      {      
         /* copy fields */
      
         strcpy(onereportPtr->lid, bestPtr->lid);
         strcpy(onereportPtr->pe,  bestPtr->pe);
         onereportPtr->dur =       bestPtr->dur;
         strcpy(onereportPtr->ts,  bestPtr->ts);
         strcpy(onereportPtr->extremum, bestPtr->extremum);
         onereportPtr->probability = -1;
      
         strcpy(onereportPtr->shef_qual_code, bestPtr->shef_qual_code);
         onereportPtr->quality_code =         bestPtr->quality_code;
      
         onereportPtr->value = bestPtr->value;
         status = yearsec_dt_to_timet(bestPtr->obstime,
   				      &onereportPtr->validtime);
         onereportPtr->basistime = 0;
      
      
         /* initialize the list */
      
         if (inputPtr == NULL)
         {
	   	    outputPtr = onereportPtr;
		    ListInit(&outputPtr->list);
         }
      
         else
	    outputPtr = inputPtr;
      
      
         /* add the data to the list */
      
         ListAdd(&outputPtr->list, &onereportPtr->node);
      }
      else
      {
         fprintf(stderr, "Error allocating for ReportList latestobsvalue.\n");
      }
   }
   
   return(outputPtr);
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   derive_report_obs()
   given a linked list of data, and the start and end position of the 
   data for a single location and the number of rows inclusive in this
   block of records, this function determines the "best" value for the 
   location.
   ***************************************************************************/
Observation * derive_report_obs(const pc_options_struct	* pc_options,
				Observation 		*startPtr, 
				Observation 		*endPtr,
                                int                     change_window,
				int			lid_count,
                                double                  * change )
{
   int          first_obs_found = 0 ;
   int          last_obs_found = 0 ;
   char		cur_ts[SHEF_TS_LEN + 1];
   char		best_ts[SHEF_TS_LEN + 1];
   char		where[100];
   IngestFilter	*ingestHead = NULL;
   int		result;   
   time_t	curtimet;
   time_t	cur_diff, best_diff;  
   int		i, status;
   Observation	*obsPtr = NULL ;
   Observation 	*bestPtr = NULL ;
   Observation  *bestPtr2 = NULL ;

   * change = MISSING_VAL ;
     
   /* loop thru the records and find the best type-source to use.  there is
      no need to worry about data from multiple physical element for the
      location, but we do need to check for data possibly from multiple
      type-sources.  check if there is more than one type-source for this
      data. if so find out which is the one we should be using */
     
   if ( startPtr == NULL ) return bestPtr ;

   strcpy(best_ts, startPtr->ts);
   strcpy(cur_ts,  startPtr->ts);

   obsPtr = startPtr ;

   for (i = 0; i < lid_count; i++)
   {
   
      /* anytime a new ts is noted, then see if it the best one */
       
      if (strcmp(obsPtr->ts, cur_ts) != 0)
      {
	 /* since there is more than one type-source, go get the ingest
	    filter entries for this location for later use.  do not filter
	    the request by duration or extremum, since that is done via the 
	    original query, if at all */
	 
		 if ( ingestHead == NULL )
		 {
		    sprintf(where, " WHERE lid='%s' AND pe ='%s' ", obsPtr->lid, 
	                    obsPtr->pe);
		    ingestHead = GetIngestFilter(where);
		    
		    if (ingestHead == NULL)
		    {
			    fprintf(stderr, "No ingest filter defined %s\n", where);
			    return(obsPtr);
		    }
		    
		    if (DEBUG) 
		    {
			   // printf("multi-ts for obs %s\n", where);
		    }
		 }
		 
	 
		 /* compare the ts just found with the best one and
		    set it to the best ts if it has a higher rank */
		 
		 result = compare_tsrank(obsPtr->ts, best_ts, ingestHead);
		 if (result < 0)
		    strcpy(best_ts, obsPtr->ts);
		 
		 
		 /* update for subsequent passes */
		 
		 strcpy(cur_ts, obsPtr->ts);	 
      }
      
      /* get the next item in the list */
      
      obsPtr = (Observation *) ListNext(&obsPtr->node);
   }
   
   
   if (ingestHead)
   {
      FreeIngestFilter(ingestHead);
      ingestHead = NULL;
   }
   
   
   /* now at this point, we know what the best ts is in the linked
      list.  loop thru the list, considering only those that match
      this ts, and find the appropriate value to return based on
      the time mode. */

   best_diff = MISSING_VAL;
   
   obsPtr = startPtr;   

   for (i = 0; i < lid_count; i++)
   {
      if (strcmp(best_ts, obsPtr->ts) == 0)
      {
         switch ( pc_options->time_mode )
         {
            case LATEST :
            case MAXSELECT :
            case MINSELECT :

	       /* for all but the SETTIME and VALUE_CHANGE modes, 
                  use the first value, which is the latest */
	       return obsPtr ;
	       break;
	 
            case SETTIME :

               status = yearsec_dt_to_timet ( obsPtr->obstime, &curtimet ) ;
               cur_diff = abs ( pc_options->valid_timet - curtimet ) ;

               if ( best_diff == MISSING_VAL || cur_diff < best_diff )
               {
                  best_diff = cur_diff;
                  bestPtr = obsPtr;
               }

               if ( best_diff == 0 )
               {
                  return bestPtr ;
               }

               break ;

            case VALUE_CHANGE :

               /* Find the best observation in the upper time window. */
               if ( first_obs_found == 0 )
               {
                  status = yearsec_dt_to_timet(obsPtr->obstime, &curtimet);
                  cur_diff = abs(pc_options->valid_timet - curtimet);

                  /* Is the observation inside the window? */
                  if ( cur_diff <= change_window )
                  {
                     /* Is the observation the closest to the center
                        time so far? */
                     if ( ( best_diff == MISSING_VAL ) ||
                          ( cur_diff <= best_diff ) )
                     {
                        best_diff = cur_diff ;
                        bestPtr = obsPtr ;
                     }
                     else
                     {
                        first_obs_found = 1 ;
                        best_diff = MISSING_VAL ;
                        -- i ;
			continue ;
                     }
                  }
                  else
                  {


                     if ( best_diff != MISSING_VAL )
                     {
                        first_obs_found = 1 ;
                        best_diff = MISSING_VAL ;
                        -- i ;
		        continue ;
                     }
		     
                  }
               }
               else if ( last_obs_found == 0 )
               {
                  /* Find the best observation in the lower time window. */
                  status = yearsec_dt_to_timet(obsPtr->obstime, &curtimet);
                  cur_diff = abs ( ( pc_options->valid_timet -
                                   ( pc_options->dur_hours * 3600 ) -
                                   curtimet ) ) ;

                  /* Is the observation inside the window? */
                  if ( cur_diff <= change_window )
                  {
                     /* Is the observation the closest to the center
                        time so far? */
                     if ( ( best_diff == MISSING_VAL ) ||
                          ( cur_diff <= best_diff ) )
                     {
                        best_diff = cur_diff ;
                        bestPtr2 = obsPtr ;
                     }
                     else
                     {
                        last_obs_found = 1 ;
                     }
                  }
                  else
                  {
                     if ( best_diff != MISSING_VAL )
                     {
                        last_obs_found = 1 ;
                     }
                  }

               }
               else
               {
                  /* Both observations necessary to compute the change
                     have been found. */
                  * change = bestPtr->value - bestPtr2->value ;
                  return bestPtr ;
               }

               break ;

            default :

               break ;
         }          
      }
      
      /* get the next item in the list */
      obsPtr = (Observation *) ListNext(&obsPtr->node);
   }

   if ( pc_options->time_mode == VALUE_CHANGE )
   {
      if ( first_obs_found == 1 &&
           best_diff != MISSING_VAL )
      {
         * change = bestPtr->value - bestPtr2->value ;
      }
   } 
   
   return bestPtr ;  
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   derive_report_lat()
   This function is identical to the derive_report_lat() function
   except that it operates on LatestObsValue data.
   ***************************************************************************/
LatestObsValue * derive_report_lat(const pc_options_struct *	pc_options,
				   LatestObsValue 	*startPtr, 
				   LatestObsValue 	*endPtr,
				   int			lid_count)
{
   char		cur_ts[SHEF_TS_LEN + 1];
   char		best_ts[SHEF_TS_LEN + 1];
   char		where[100];
   IngestFilter	*ingestHead = NULL;
   int		result;   
   time_t	curtimet;
   time_t	cur_diff, best_diff;  
   int		i, status;
   LatestObsValue	*latPtr = NULL ;
   LatestObsValue 	*bestPtr = NULL;
     
   
   /* loop thru the records and find the best type-source to use.  there is
      no need to worry about data from multiple physical element for the
      location, but we do need to check for data possibly from multiple
      type-sources.  check if there is more than one type-source for this
      data. if so find out which is the one we should be using */
     
   if ( startPtr == NULL ) return bestPtr ;

   strcpy(best_ts, startPtr->ts);
   strcpy(cur_ts,  startPtr->ts);

   latPtr = startPtr;   
   for (i = 0; i < lid_count; i++)
   {
       
      if (strcmp(latPtr->ts, cur_ts) != 0)
      {
	 /* since there is more than one type-source, go get the ingest
	    filter entries for this location for later use.  do not filter
	    the request by duration or extremum, since that is done via the 
	    original query, if at all */
	 
	 if (ingestHead == NULL)
	 {
	    sprintf(where, " WHERE lid='%s' AND pe ='%s' ", latPtr->lid, latPtr->pe);
	    ingestHead = GetIngestFilter(where);
	    
	    if (ingestHead == NULL)
	    {
	       fprintf(stderr, "No ingest filter defined %s\n", where);
	       return(latPtr);
	    }
	    
	   // if (DEBUG) printf("multi-ts for latestobsvalue %s\n", where);
	 }
	 
	 
	 /* compare the ts just found with the best one and
	    set it to the best ts if it has a higher rank */
	 
	 result = compare_tsrank(latPtr->ts, best_ts, ingestHead);
	 if (result < 0)
	    strcpy(best_ts, latPtr->ts);
	 
	 
	 /* update for subsequent passes */ 
	 
	 strcpy(cur_ts, latPtr->ts);	 
      }
      
      /* get the next item in the list */
      
      latPtr = (LatestObsValue *) ListNext(&latPtr->node);
   }
   
   
   if (ingestHead)
   {
      FreeIngestFilter(ingestHead);
      ingestHead = NULL;
   }
   
   
   /* now at this point, we know what the best ts is in the linked
      list.  loop thru the list, considering only those that match
      this ts, and find the appropriate value to return based on
      the time mode. */

   best_diff = MISSING_VAL;
   
   latPtr = startPtr;   
   for (i = 0; i < lid_count; i++)
   {      
      if (strcmp(best_ts, latPtr->ts) == 0)
      {
	 /* for all but SETTIME mode, use the first value, which is the latest */
	 
	 if (pc_options->time_mode != SETTIME)
	 {
	    bestPtr = latPtr;
	    break;
	 }
	 
	 else
	 {
	    status = yearsec_dt_to_timet(latPtr->obstime, &curtimet);
	    cur_diff = abs(pc_options->valid_timet - curtimet);
	    
	    if (best_diff == MISSING_VAL || cur_diff < best_diff)
	    {
	       best_diff = cur_diff;
	       bestPtr = latPtr;
	    }
	    
	    if (best_diff == 0)
	       break;
	    
	 }
      }
      
      
      /* get the next item in the list */
      
      latPtr = (LatestObsValue *) ListNext(&latPtr->node);
   }
   
   
   return(bestPtr);  
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   FreeReportList()
   ***************************************************************************/
void FreeReportList ( ReportList * sp )
{
   ReportList * nextPtr = NULL ;

   if ( sp != NULL )
   {
      sp = ( ReportList * ) ListFirst ( & sp->list ) ;

      while ( sp != NULL )
      {
         nextPtr = ( ReportList * ) ListNext ( & sp->node ) ;
         free ( ( void * ) sp ) ;
         sp = nextPtr ;
      }
   }
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
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
/*****************************************************************************
   compare_tsrank()
   ***************************************************************************/
int compare_tsrank(char 	*ts1,
		   char		*ts2,
		   IngestFilter	*ingestHead)
{
   IngestFilter *ingestPtr = NULL ;
   int		rank1, rank2;
   int		result;
   
   
   /* initialize */
   
   rank1 = rank2 = 0;
   ingestPtr = (IngestFilter*) ListFirst(&ingestHead->list);

   while (ingestPtr )
   {
      if (strcmp(ingestPtr->ts, ts1) == 0)
	 rank1 = ingestPtr->ts_rank;
      
      if (strcmp(ingestPtr->ts, ts2) == 0)
	 rank2 = ingestPtr->ts_rank;
      
      ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
   }
   
   if (rank1 > rank2)
      result = 1;
   else if (rank1 < rank2) 
      result = -1;
   else 
      result = 0;
   
   return(result);
}
