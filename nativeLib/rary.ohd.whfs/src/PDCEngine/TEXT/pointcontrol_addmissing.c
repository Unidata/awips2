
/*******************************************************************************
* FILENAME:            pointcontrol_addmissing.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION:
*   MODULE 1:          add_missing_reports
* DESCRIPTION:         Adds mulitplie missing reports to a linked list of
*                      report structures containing observed/forecast data.
*                      This is done by repetitively calling the
*                      add_missing_report routine.
*                      
*   MODULE 2:          add_missing_report
* DESCRIPTION:         Adds a single missing report to a linked list of report
*                      structures containing observed/forecast data.
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        OHD-11 HSEB
* MACHINE:             HP-UX / Redhat Linux
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1,2      12/31/2003   Bryon Lawrence    Added documentation, split
*                                                  the pointcontrol library
*                                                  into the PdcGui and
*                                                  PdcEngine libraries.
********************************************************************************
*/

#include <stdio.h>

#include "LoadUnique.h"
#include "pointcontrol_addmissing.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_getdata.h"
#include "QualityCode.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   add_missing_reports
* PURPOSE:       Adds multiple missing reports to a linked list of report 
*                structures containing observed/forecast data.  This is
*                done using repetitive calls to the add_missing_report
*                routine.
*
*                A list of unique Lids is loaded from the IngestFilter table
*                based on the PE and TS elements selected by the user
*                in the Point Data Control GUI.   The list of observed/forecast
*                reports is compared against this list of unique lids to
*                determine if reports for one or more lids are missing. 
*                If there are any missing reports, then they are inserted into
*                to the observed/forecast data report list.
*
*                At the beginning of this routine, the observed/forecast report
*                list contains only reports with data.  At the end of this
*                routine, the list contains both reports with data and 
*                reports with missing data.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME        DESCRIPTION/UNITS
*   I      pc_options_struct * pc_options  Reflects the pointcontrol options
*                                          the user has selected.
*   I/O    ReportList **       obsrepHead  Contains any observed/forecast
*                                          data retrieved from the database.
*                                          Missing reports are added to this
*                                          list.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                 HEADER FILE                DESCRIPTION
*   add_missing_report   pointcontrol_addmissing.h  Adds a missing report to
*                                                   a list of observed/forecast
*                                                   data.
*   FreeUnique           LoadUnique.h               Deallocates list containing
*                                                   unique query data.
*   LoadUnique           LoadUnique.h               Loads unique data from 
*                                                   user-specified table
*                                                   for user-specfied columns.
*   ListCount            List.h                     Returns the number of nodes
*                                                   in a linked list.
*   ListFirst            List.h                     Returns the head pointer
*                                                   of a linked list.
*   ListNext             List.h                     Returns the next node in
*                                                   a linked list, NULL if
*                                                   there are no more nodes.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE   NAME                 DESCRIPTION
*   char	previous_where [ ]   Contains the last query to LoadUnique.
*                                    Prevents calling LoadUnique more often
*                                    than necessary.
*   char	where [ ]            Contains the current query to LoadUnique.
*   char	wherestr []          Used for where clause on query to 
*                                    LoadUnique which is only used if
*                                    type source information is part of the
*                                    query.
*   int		cnt                  Contains the number of rows returned
*                                    from the LoadUnique query. 
*   int		i                    A loop variable.
*   int         msg_count            The number of missing reports added
*                                    to the list of observed/forecast 
*                                    report data.
*   int		rep_count            The number of non missing reports.
*   int         status               Contains the value returned by the
*                                    strncmp function.
*   ReportList * rPtr                Points to the position in the report
*                                    list to insert a missing report.
*   UniqueList	*ulHead              Points to the head of the list of unique
*                                    data returned from LoadUnique.
*   UniqueList	*ulPtr               Points to the current node being processed
*                                    in the list of unique reports.
*
* DATA FILES AND/OR DATABASE:
*   This routine reads the IngestFilter table in the IHFS database.
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/
void add_missing_reports ( pc_options_struct * pc_options ,
			   ReportList		** obsrepHeadDPtr )
{
   static char		previous_where[220] = "";
   char			where[220] = "";
   char			wherestr[100];
   int			cnt;
   int			i;
   int			rep_count, msg_count = 0;
   int                  status ;
   ReportList           * rPtr = NULL ;
   static UniqueList	*ulHead = NULL; 
   UniqueList		*ulPtr = NULL ;
   
   /* if the user requests that missing reports be shown, then get
      a list of stations from ingest filter that match the requested
      physical element, and if specified, the type-source code also. 
      except for retrievals of observed and forecast data, as occurs when
      retrieving the latest river data, do not consider type-source
      entries for forecast data. */
   
   /* build the where class for the IngestFilter list. */
   
   if (pc_options->Primary)
      sprintf(where, " WHERE (pe LIKE 'H%%' OR pe LIKE 'Q%%') ");
   
   else if (pc_options->PCandPP)
      sprintf(where, " WHERE pe IN ('PC', 'PP') ");
   
   else
   {
      sprintf(where, " WHERE pe ='%s' ", pc_options->selectedAdHocElementString);
   }
   
   if (pc_options->filter_by_typesource)
   {   
      build_type_source_where_filter(wherestr, pc_options);
      strcat(where, wherestr);  	 
   }
  
   else
   {
      if (pc_options->element_type == RIVER_AD_HOC_TYPE && pc_options->time_mode == LATEST)
	      strcat(where, " AND ts NOT LIKE 'C%' ");
      else
	      strcat(where, " AND (ts NOT LIKE 'C%' AND ts NOT LIKE 'F%') ");
   }
   
   strcat ( where , " AND ingest='T' ORDER BY 1 " ) ;
   
   /* only load the data anew if this request is different 
      from the previous one */
   
   if (strcmp(previous_where, where) != 0)
   {      
      strcpy(previous_where, where);

      if (ulHead != NULL)
      {
		 FreeUnique(ulHead);
		 ulHead = NULL;
      }
      if (DEBUG) printf("IngestFilter unique lid %s", where);
      
      ulHead = LoadUnique("lid", "IngestFilter", where, &cnt);  
      
      if ( ulHead != NULL )
      {
         if (DEBUG) printf("  :count=%d\n", ListCount(&ulHead->list));      
      }
   }
   
   /* loop on the unique stations in the ingestfilter filter and
      see if there is a corresponding report already there.  
      note that this loop on ingest filter actually has a 
      simultaneous loop on the report list going on */
   
   if ( * obsrepHeadDPtr != NULL )
   {
      rep_count = ListCount ( & ( * obsrepHeadDPtr )->list ) ;
      rPtr = * obsrepHeadDPtr ;
   }
   else
      rep_count = 0;

   if ( ulHead != NULL )
   {
      ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   }
   
   while ( ulPtr != NULL )
   {
      /* first strip off trailing blanks from the location id */
      for ( i = 0 ; i < LOC_ID_LEN + 1 ; ++i )
      {
		 if ( ulPtr->uchar [ i ] == ' ' )
		 {
		    memset(&ulPtr->uchar[i], 0, 1);
		    break;
		 }
      }
      
      /* If there is a report for the current station being
	 considered from the ingest filter list, then do nothing.
	 if there is no data for the station, and the
	 station "should" have data, then add a "missing" report */            
      if ( rPtr == NULL )
      {
	     add_missing_report ( pc_options , ulPtr->uchar , obsrepHeadDPtr , rPtr ) ;
         msg_count ++ ;
         ulPtr = ( UniqueList * ) ListNext ( & ulPtr->node ) ;
      }
      else
      {
         status = strncmp ( ulPtr->uchar , rPtr->lid , LOC_ID_LEN ) ;

         if ( status == 0 )
         {
            /* Advance to the next nodes in the report and ingest filter 
             * lists. */
	        rPtr = ( ReportList * ) ListNext ( &rPtr->node ) ;
            ulPtr = ( UniqueList * ) ListNext ( &ulPtr->node ) ;

         }
         else if ( status < 0 )
         {
            /* There is an entry in the ingest filter list for which there
             * is no entry in the report list.  This indicates a missing 
             * observed report. Report a missing report and advance the 
             * ingest filter list to the next node. */
            add_missing_report ( pc_options , ulPtr->uchar , obsrepHeadDPtr , 
		                 rPtr ) ;
            msg_count ++ ;

            ulPtr = ( UniqueList * ) ListNext ( & ulPtr->node ) ;
         }
         else
         {
            /* There is an entry in the observation list for which there
	     * is no entry in the Ingest Filter list.  This is likely
             * due to a change in the ingest flag for a location from True
             * to False. Advance to the next node in the report list.*/
	         rPtr = ( ReportList * ) ListNext ( & rPtr->node ) ;
         }
      }

   }  /* end of loop on unique stations from ingest filter */
   
   if ( DEBUG ) printf ("added %d msg to existing %d\n", msg_count, rep_count);

   return ;
}


/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   add_missing_report
* PURPOSE:       Inserts a single missing report, as determined by the
*                add_missing_reports routine, into a list of forecast/observed
*                data.
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME         DESCRIPTION/UNITS
*   I      pc_options_struct * pc_options   Contains the Point Data Control
*                                           Options selected by the user.
*   I      char		     * lid          Contains the location identifier
*                                           selected by the user.
*   I/O    ReportList	    ** listHeadPtr  Contains the pointer to the
*                                           head of the linked list of 
*                                           observed and forecast data.
*   I      ReportList	     * listPtr      Points to the node in the
*                                           list of observed and forecast
*                                           data where the missing report
*                                           should be inserted. 
*
* RETURNS:
*   Nothing.
*
* APIs UTILIZED:
*   NAME               HEADER FILE          DESCRIPTION
*   ListInit           List.h               Initialize the head pointer 
*                                           of a linked list.
*   ListAdd            List.h               Add a node to the end of a linked
*                                           list.
*   ListInsert         List.h               Insert a node into a linked list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME           DESCRIPTION
*   ReportList * onereportPtr   Points to a new ReportList node containing
*                               missing information.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.  If an error is encountered allocating memory for the
*   new ReportList node, then this routine prints a simple diagnostic
*   and returns control to the caller.
*
********************************************************************************
*/
void add_missing_report ( pc_options_struct 	* pc_options ,
			  char			* lid ,
			  ReportList		** listHeadPtr ,
			  ReportList		* listPtr )
{
   ReportList * onereportPtr = NULL ;
   
   /* allocate a report for missing */
   
   if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
   {      
      /* assign fields */
      
      strcpy(onereportPtr->lid, lid);
      
      
      
      if (pc_options->Primary)
	      strcpy(onereportPtr->pe, "--");
      
      else if (pc_options->PCandPP)
	      strcpy(onereportPtr->pe, "P-");
      
      else
      {
	      sprintf(onereportPtr->pe, "%s", pc_options->selectedAdHocElementString);
      }
      
      onereportPtr->dur =       0;
      
      if (pc_options->filter_by_typesource)
      {
      	 //had to remove when adding multiple type/sources for filtering
	     //strcpy(onereportPtr->ts,  pc_options->selectedTypeSrc);
	     strcpy(onereportPtr->ts, "R-");
      }
      else
      {
	     strcpy(onereportPtr->ts, "R-");
      }
      
      strcpy(onereportPtr->extremum, "Z");
      onereportPtr->probability =    -1;
      
      strcpy(onereportPtr->shef_qual_code, "Z");
      onereportPtr->quality_code =         DEFAULT_QC_VALUE;
      
      onereportPtr->value = MISSING_VAL;
      onereportPtr->value2 = MISSING_VAL;
      onereportPtr->validtime = pc_options->valid_timet;				      
      onereportPtr->basistime = 0;
      onereportPtr->threat_index = THREAT_MISSING_DATA ;
      
      /* initialize the list */
      if ( * listHeadPtr == NULL )
      {
	     * listHeadPtr = onereportPtr ;
	     ListInit ( & ( * listHeadPtr )->list ) ;
         ListAdd ( & ( * listHeadPtr )->list , & onereportPtr->node ) ;
      }
      else if ( listPtr != NULL ) 
      {
         ListInsert ( & ( * listHeadPtr )->list , & listPtr->node ,
                      & onereportPtr->node ) ;      
      }
      else
      {
         ListAdd ( & ( * listHeadPtr )->list , & onereportPtr->node ) ;
      }
      
     
      
   }
   
   else
   {
      fprintf(stderr, "Error allocating for ReportList add_missing.\n");
   }
   
   
   return ;
}
