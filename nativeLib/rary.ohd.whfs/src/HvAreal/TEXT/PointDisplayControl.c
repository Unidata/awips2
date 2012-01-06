#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <Xm/Xm.h>

#include "pointcontrol_show.h"
#include "display_control_show.h"
#include "hv_refreshTimer.h"
#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "PointDisplayControl.h"
#include "pointcontrol_mgr.h"


/* The currently selected station */
static Station current_station ;

/*********************************************************************/

void initStations(HvDisplayControl *hdc)
{

   HvStation 	*hvstationHead = NULL;
   HvStation	*hPtr = NULL;
   long		i, count = 0;

   /* Initialize the current station. */
   memset ( & current_station , 0 , sizeof ( Station ) ) ;

   /* load in the general station info */
//   profileTime ("initStations ONCE only! - before reading HvStation");

   hvstationHead = GetHvStation(" ORDER BY lid ");
   if (!hvstationHead)
   {
      fprintf(stderr, " ERROR reading HvStation data!!!\n");
      return;
   }


   /* allocate memory for hdc->stations */

   count = ListCount(&hvstationHead->list);
   if (count > 0)
   {
      hdc->stations = (Station *) malloc(sizeof(Station) * count);
      if (! hdc->stations)
      {
	 fprintf(stderr, "Error in malloc in initStations() \n");
	 return;
      }
   }


   /* load the static data for each station */

  // profileTime("before main loop in initStations()");

   i = 0;
   hPtr = (HvStation *) ListFirst(&hvstationHead->list);
   while (hPtr)
   {
      initStation(&hdc->stations[i], hPtr);


      /* move to the next station */

      i++;
      hPtr = (HvStation *) ListNext(&hPtr->node);
   }

   hdc->numStations = i;

   printf("number of stations=%ld\n", hdc->numStations);

   /* Free the memory used by the linked list of HvStation
      structures. */
   if ( hvstationHead != NULL )
   {
      FreeHvStation ( hvstationHead ) ;
      hvstationHead = NULL ;
   }


   return;
}


/*********************************************************************/
/* this function loads the static data for a particular station
   into the local HydroView structure. */

void initStation(Station 	*station,
		 HvStation	*hvPtr)
{

   //char header[] = "initStation(): ";
   /* load identifiers, lat and lon, river name */

   strcpy(station->lid,  hvPtr->lid);
   strcpy(station->name, hvPtr->name);

   station->loc.lat =    hvPtr->lat;
   station->loc.lon =   -hvPtr->lon;

   strcpy(station->streamName, hvPtr->stream_name);


   /* load in the action and flood threshold values,
      using either the stage or discharge units */

   if ( ! IsNull(CHAR, &hvPtr->primary_pe) )
   {
      if (hvPtr->primary_pe[0] == 'Q')
      {
    	 if (! IsNull(FLOAT, &hvPtr->flood_flow) )
    	    station->floodLevel = hvPtr->flood_flow;
    	 else
    	    station->floodLevel = MISSING;

    	 if (! IsNull(FLOAT, &hvPtr->action_flow) )
    	    station->actionLevel = hvPtr->action_flow;
    	 else
    	    station->actionLevel = MISSING;
      }

      else
      {
    	 if (! IsNull(FLOAT, &hvPtr->flood_stage) )
    	    station->floodLevel = hvPtr->flood_stage;
    	 else
    	    station->floodLevel = MISSING;

    	 if (! IsNull(FLOAT, &hvPtr->action_stage) )
    	    station->actionLevel = hvPtr->action_stage;
    	 else
    	    station->actionLevel = MISSING;
      }
   }


   /* if no primary pe specified, then assume stage units */

   else
   {
      if (! IsNull(FLOAT, &hvPtr->flood_stage))
	      station->floodLevel = hvPtr->flood_stage;
      else
	      station->floodLevel = MISSING;

      if (! IsNull(FLOAT, &hvPtr->action_stage))
	      station->actionLevel = hvPtr->action_stage;
      else
	      station->actionLevel = MISSING;
   }


   /* initialize these. they are loaded by a separate function */

   station->curReport.value     = MISSING;
   station->curReport.validtime = 0;

   station->mofoReport.value     = MISSING;
   station->mofoReport.validtime = 0;


   /* initialize the station characteristics.
      note that a station display class definition does not allow a station
      to be a river data point if it already a fcst pt or reservoir;
      however, a display class can be both a forecast point and a reservoir.
      for the purpose of this definition of station characteristics
      if a station is a fcst pt or reservoir also call it a river station. */

   if (strstr(hvPtr->disp_class, DISP_CLASS_FCSTPT) != NULL)
      station->isFcstPt = 1;
   else
      station->isFcstPt = 0;

   if (strstr(hvPtr->disp_class, DISP_CLASS_RESERVOIR) != NULL)
      station->isReservoir = 1;
   else
      station->isReservoir = 0;

   if (strstr(hvPtr->disp_class, DISP_CLASS_RIVER) != NULL ||
       station->isFcstPt ||  station->isReservoir)
      station->isRiver = 1;
   else
      station->isRiver = 0;

   if ((strstr(hvPtr->disp_class, DISP_CLASS_PRECIP) != NULL) ||
       (strstr(hvPtr->disp_class, DISP_CLASS_SNOW)  != NULL) ||
       (strstr(hvPtr->disp_class, DISP_CLASS_TEMP)  != NULL) ||
       (strstr(hvPtr->disp_class, DISP_CLASS_OTHER)  != NULL))
      station->isNonRiver = 1;
   else
      station->isNonRiver = 0;


   return;
}


/*********************************************************************/

void load_PointData ( HvDisplayControl * hdc )
{
   int          compare_value ;
   long		i , found ;
   ReportList	* reportlistPtr = NULL ;
   ReportList * reportListHead = getReportListHead();

   /* debug dump of point reports */

   /* printReports(reportListHead); */

   /* load the data for each station */
 //  profileTime("before main loop in loadStationsPointData()");

   if ( reportListHead == NULL )
   {
      for (i = 0; i < hdc->numStations; i++)
      {
		 hdc->stations[i].curReport.value     = MISSING;
		 hdc->stations[i].value2    = MISSING;
		 hdc->stations[i].curReport.validtime = 0;
		 hdc->stations[i].isDisplayed         = 0;
	     hdc->stations[i].threat_index = THREAT_MISSING_DATA ;
      }
   }
   else
   {

      reportlistPtr = ( ReportList * ) ListFirst ( & reportListHead->list ) ;

      for (i = 0; i < hdc->numStations; i++)
      {

         /* load the data collected by the external library functions
      	    into the local HydroView structure - the two worlds meet! */

         found = 0;

	 while ( reportlistPtr != NULL )
	 {
        compare_value = strcmp(reportlistPtr->lid, hdc->stations[i].lid) ;

	    if ( compare_value == 0 )
	    {
	       if ( reportlistPtr->use )
	       {
		       found = 1;
		       load_StationPointData(&hdc->stations[i], reportlistPtr);
	       }

	    }
	    else if ( compare_value < 0 )
        {
	         reportlistPtr = (ReportList *) ListNext(&reportlistPtr->node);
             continue ;
        }

        break ; //past the lid that we are looking for, so quit looking
	 }

         /* if station not in report list, then load in missing value. */
         if ( !found )
         {
		    hdc->stations[i].curReport.value     = MISSING;
		    hdc->stations[i].value2    = MISSING;
		    hdc->stations[i].curReport.validtime = 0;
		    hdc->stations[i].isDisplayed         = 0;
	        hdc->stations[i].threat_index = THREAT_MISSING_DATA ;
         }
      }
   }

   return;
}


/*********************************************************************/

void load_StationPointData(Station 	*station,
			   ReportList	*rPtr)
{

   char param_code[PARAM_CODE_LEN + 1];

   strcpy(station->curReport.pe,       rPtr->pe);
   station->curReport.dur =            rPtr->dur;
   strcpy(station->curReport.ts,       rPtr->ts);
   strcpy(station->curReport.extremum, rPtr->extremum);
   station->curReport.probability =    rPtr->probability;

   strcpy(station->curReport.shef_qual_code, rPtr->shef_qual_code);
   station->curReport.quality_code =         rPtr->quality_code;

   station->curReport.value     = rPtr->value;
   station->value2    = rPtr->value2;
   station->curReport.validtime = rPtr->validtime;
   station->curReport.basistime = rPtr->basistime;

   station->isDisplayed = (int )rPtr->use;
   station->threat_index = rPtr->threat_index ;

   station->pos_shift.x = rPtr->x_shift;
   station->pos_shift.y = rPtr->y_shift;


   getParamCodeFromReport(rPtr, param_code);

   memset(param_code, '\0', PARAM_CODE_LEN+1);
   strncpy(station->paramCode, param_code, PARAM_CODE_LEN);

   return;
}

/*********************************************************************/

void load_latest_river_reports(HvDisplayControl 	*hdc,
			       ReportList		*riverHead)
{
   long		i, found;
   ReportList	*reportlistPtr = NULL;


   /* load the data for each station */

//   profileTime("before loop in loadLatestRiverData()");

   for (i = 0; i < hdc->numStations; i++)
   {

      /* load the data collected by the external library functions
	 into the local HydroView structure - the two worlds meet! */

      found = 0;
      if (riverHead)
      {
	 /* loop until we find the entry for the current lid
	    or are at the end of the list */

	 reportlistPtr = riverHead;
	 while (reportlistPtr)
	 {
	    if (strcmp(reportlistPtr->lid, hdc->stations[i].lid) == 0)
	    {
	       if (reportlistPtr->use)
	       {
		  found = 1;
		  load_river_report(&hdc->stations[i], reportlistPtr);
	       }
	       break;
	    }

	    else
	       reportlistPtr = (ReportList *) ListNext(&reportlistPtr->node);
	 }
      }


      /* if station not in report list, then load in missing value. */

      if (!found)
      {
	 hdc->stations[i].mofoReport.value     = MISSING;
	 hdc->stations[i].mofoReport.validtime = 0;
      }

   }


   return;
}


/*********************************************************************/

void load_river_report(Station 		*station,
		       ReportList	*rPtr)
{

   strcpy(station->mofoReport.pe,       rPtr->pe);
   station->mofoReport.dur =            rPtr->dur;
   strcpy(station->mofoReport.ts,       rPtr->ts);
   strcpy(station->mofoReport.extremum, rPtr->extremum);
   station->mofoReport.probability =    rPtr->probability;

   strcpy(station->mofoReport.shef_qual_code, rPtr->shef_qual_code);
   station->mofoReport.quality_code =         rPtr->quality_code;

   station->mofoReport.value     = rPtr->value;
   station->mofoReport.validtime = rPtr->validtime;
   station->mofoReport.basistime = rPtr->basistime;

   /* don't set the use flag; it doesn't apply */

   return;
}


/**************************************************************************/


long findStationIndex(const char * lid)
{
   int 	i, stationIndex = -1;

   HvDisplayControl	*hdc;
   hdc = getHvDisplayControl();


   for (i = 0; i < hdc->numStations; i++)
   {
      if ( strcmp(hdc->stations[i].lid, lid) == 0 )
      {
	 stationIndex = i;
	 break;
      }
   }

   return (stationIndex);
}

/***********************************************************************/

Station * findStationPtr(const char * lid)
{
   int 	i;

   Station	        *sPtr = NULL;
   HvDisplayControl	*hdc = getHvDisplayControl();


   for (i = 0; i < hdc->numStations; i++)
   {
      if (strcmp(hdc->stations[i].lid, lid) == 0 )
      {
	 sPtr = &hdc->stations[i];
	 break;
      }
   }

   return ( sPtr ) ;
}


/***********************************************************************/

Station * findDisplayedStation ( char * station_id )
{
   char search_direction ;
   int status ;
   Node * ( * list_routine ) ( const Node * nodePtr ) ;

   ReportList *reportListHead = getReportListHead();
   ReportList * pReportListNode = NULL ;

   if ( station_id == NULL )
   {
      return NULL ;
   }

   if ( reportListHead == NULL )
   {
      return NULL ;
   }

   /* Test the first character in the station name.  If it is greater than
      "m" then search the linked list from the tail end. */
   if ( * station_id >= 'm' || * station_id >= 'M' )
   {
      search_direction = 'R' ;
      list_routine = ListPrev ;
      pReportListNode = ( ReportList * ) ListLast ( & reportListHead->list ) ;
   }
   else
   {
      search_direction = 'F' ;
      list_routine = ListNext ;
      pReportListNode = ( ReportList * ) ListFirst ( & reportListHead->list ) ;
   }

   while ( pReportListNode != NULL )
   {

      if ( pReportListNode->use == 1 )
      {
         status = strcmp ( station_id , pReportListNode->lid ) ;

         if ( status == 0 )
         {
            /* Copy the identifier, name, and latitude/longitude of the
               selected station into the Station structure. */
            strcpy ( current_station.lid , pReportListNode->lid ) ;
            strcpy ( current_station.name , pReportListNode->name ) ;
            current_station.loc.lat = pReportListNode->latitude ;
            current_station.loc.lon = pReportListNode->longitude ;
            return & current_station ;
         }

         switch ( search_direction )
         {

            case 'F' :

               if ( status < 0 ) return NULL ;
               break ;

            case 'R' :

               if ( status > 0 ) return NULL ;
               break ;

            default :

               fprintf ( stderr , "\nIn routine 'findDisplayedStation':\n"
                                  "There is an error in the logic in this "
                                  "routine.  Reached the default case in the "
                                  "'serach_direction' switch statement.\n" ) ;
               return NULL ;
               break ;

         }

      }

      pReportListNode = ( ReportList * ) list_routine
                                         ( & pReportListNode->node ) ;
   }

   return NULL ;
}

/***********************************************************************/

void refreshStationData ( Widget top_widget )
{
   int 		retrieval_required ;
   int          hv_pointdata_display_token_state ;

   /* Remove the old refreshTimeOut and add it again (all in one function).
      Only do this if the option to display point data in Hydroview/MPE
      was initially set to "ON" via the hv_pointdata_display token.  */
   hv_pointdata_display_token_state = is_riverstatus_displayed ( ) ;

   if ( hv_pointdata_display_token_state == 1 )
   {
      addRefreshTimeOut ( top_widget ) ;
   }

   /* load the latest selected data. this uses whatever pc_options are
      in effect.  it is possible that the refresh loads data different
      than the type of data currently shown, since the user may change
      the pc_options, without updating the map.  in this case, the
      current pc_options will be different from what is shown on the map */
   retrieval_required = 1;
  // pc_RetrieveAndMapData ( top_widget , retrieval_required ) ;

   /* return the cursor to the regular cursor */
   mSetCursor ( M_NORMAL ) ;

   return;
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
void mapHighlightStation( )
{
   char header[] = "mapHighlightStation(): ";

   Boolean      isDisplayed ;
   GC           gc       = _get_map_gc ( ) ;
   Display      *display = _get_map_display ( ) ;
   Pixmap       pixmap   = _get_map_pixmap ( ) ;
   int          halfsize =  HIGHLIGHT_GAGE_SIZE/2;

   /* Retrieve the highlighted station if there is one. */
   /* Determine if it is displayed. */
   isDisplayed = isStationDisplayed ( current_station.lid ) ;

   /* change color - not sure why this is being done. */
   mSetColor ( "White" ) ;

   if ( isDisplayed )
   {


      mConvertLatLon2XY ( current_station.loc.lat ,
                          current_station.loc.lon ,
                          ( int * ) &( current_station.pos.x ) ,
                          ( int * ) &( current_station.pos.y ) ) ;

       //account for the shifting of icons and labels for certain lid|paramcode combos
     // current_station.pos.x += current_station.pos_shift.x;
     // current_station.pos.y += current_station.pos_shift.y;

     int x_shift = current_station.pos_shift.x;
     int y_shift = current_station.pos_shift.y;

     printf("%s lid = %s x_shift = %d y_shift = %d\n",
            header, current_station.lid,  x_shift, y_shift);

      /* change color */
      mSetColor ( "Red" ) ;
      XDrawRectangle(display, pixmap, gc,
                     (current_station.pos.x - halfsize) + x_shift ,
                     (current_station.pos.y - halfsize) + y_shift ,
                     HIGHLIGHT_GAGE_SIZE,
                     HIGHLIGHT_GAGE_SIZE);
   }

   return ;
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

Boolean isStationDisplayed ( char * station_id )
{
   char search_direction ;
   int status ;
   Node * ( * list_routine ) ( const Node * nodePtr ) ;
   ReportList * pReportListNode = NULL ;
   ReportList *reportListHead = getReportListHead();

   if ( station_id == NULL )
   {
      return False ;
   }

   if ( reportListHead == NULL )
   {
      return False ;
   }

   /* Test the first character in the station name.  If it is greater than
      "m" then search the linked list from the tail end. */
   if ( * station_id >= 'm' || * station_id >= 'M' )
   {
      search_direction = 'R' ;
      list_routine = ListPrev ;
      pReportListNode = ( ReportList * ) ListLast ( & reportListHead->list ) ;
   }
   else
   {
      search_direction = 'F' ;
      list_routine = ListNext ;
      pReportListNode = ( ReportList * ) ListFirst ( & reportListHead->list ) ;
   }

   while ( pReportListNode != NULL )
   {

      if ( pReportListNode->use == 1 )
      {
         status = strcmp ( station_id , pReportListNode->lid ) ;

         if ( status == 0 )
         {
            return True ;
         }

         switch ( search_direction )
         {

            case 'F' :

               if ( status < 0 ) return False  ;
               break ;

            case 'R' :

               if ( status > 0 ) return False ;
               break ;

            default :

               fprintf ( stderr , "\nIn routine 'isStationDisplayed':\n"
                                  "There is an error in the logic in this "
                                  "routine.  Reached the default case in the "
                                  "'search_direction' switch statement.\n" ) ;
               return False  ;
               break ;

         }

      }

      pReportListNode = ( ReportList * ) list_routine
                                         ( & pReportListNode->node ) ;
   }

   return False ;
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
Station * locateStation ( int x , int y , int * new_station )
{
  // char header[] = "locateStation(): ";
   double       mindist, dist ;
   double       delx, dely ;
   int          num_stations ;
   ReportList * pReportListNode = NULL ;
   ReportList *reportListHead = getReportListHead();
   Station      station ;

   * new_station = 1 ;
   mindist = 1000;

  // printf("%s  at start of function, x = %d y = %d \n", header, x, y);

   /* Determine if there are any reports to process. */
   if ( reportListHead == NULL )
   {
      return NULL ;
   }

   /* Retrieve the number of stations in the linked list. */
   num_stations = ListCount ( & reportListHead->list ) ;

//    printf("%s  num_stations = %d\n", header, num_stations);


   if ( num_stations == 0 )
   {
     return NULL ;
   }

   /* Walk through the linked list. */
   pReportListNode = ( ReportList * ) ListFirst ( & reportListHead->list ) ;

   while ( pReportListNode != NULL )
   {

  //    printf("%s lid = :%s: \n", header, pReportListNode->lid );

      if ( pReportListNode->use == 1 )
      {
         delx = abs ( x - pReportListNode->pos.x ) ;
         dely = abs ( y - pReportListNode->pos.y ) ;

         /* don't need square root */
         dist = (double) ((delx * delx) + (dely * dely));

      //   printf("%s  dist = %lf pos.x = %ld pos.y = %ld\n", header, dist,
      //           pReportListNode->pos.x ,pReportListNode->pos.y);

         if (dist < mindist)
         {
            mindist = dist ;

       //     printf("%s  before initStationFromReport ()\n", header);
            initStationFromReport(&station, pReportListNode);
         }
         else if (dist == mindist)
         {
            //override a PC selection, if there is one
            if ( (strcmp(pReportListNode->pe, "PP") == 0)  )
            {
                  initStationFromReport(&station, pReportListNode);
            }
         }
      }

      pReportListNode = ( ReportList * ) ListNext ( & pReportListNode->node ) ;
   }

   if ( mindist > CLOSE_ENOUGH_IN_PIXELS )
   {
      return NULL ;
   }
   else
   {
      //test to see if the station (including lid and paramCode) are the same as the
      //currently selection station.
      if ( isSameStation(&station, &current_station) ) //already selected, don't select it again
      {
         * new_station = 0 ;
      }
      else //different station, so select it
      {
         current_station = station ;
      }

      return & current_station ;
   }
}

// -------------------------------------------------------------------------------------------
Station * locateStationWithShifting ( int x , int y , int * new_station )
{
  // char header[] = "locateStationWithShifting(): ";
   double       mindist, dist ;
   double       delx, dely ;
   double       yShift = 0;
   double       newY = 0;
   int          num_stations ;
   ReportList * pReportListNode = NULL ;
   ReportList *reportListHead = getReportListHead();
   Station      station ;

   * new_station = 1 ;
   mindist = 1000;

  // printf("%s  at start of function, x = %d y = %d \n", header, x, y);

   /* Determine if there are any reports to process. */
   if ( reportListHead == NULL )
   {
      return NULL ;
   }

   /* Retrieve the number of stations in the linked list. */
   num_stations = ListCount ( & reportListHead->list ) ;

//    printf("%s  num_stations = %d\n", header, num_stations);


   if ( num_stations == 0 )
   {
     return NULL ;
   }

   /* Walk through the linked list. */
   pReportListNode = ( ReportList * ) ListFirst ( & reportListHead->list ) ;

   while ( pReportListNode != NULL )
   {

  //    printf("%s lid = :%s: \n", header, pReportListNode->lid );

      if ( pReportListNode->use == 1 )
      {
         yShift = determineYDataShift(pReportListNode);

         newY = pReportListNode->pos.y + yShift;

         delx = abs ( x - pReportListNode->pos.x ) ;
         dely = abs ( y - newY ) ;

         /* don't need square root */
         dist = (double) ((delx * delx) + (dely * dely));

      //   printf("%s  dist = %lf pos.x = %ld pos.y = %ld\n", header, dist,
      //           pReportListNode->pos.x ,pReportListNode->pos.y);

         if (dist < mindist)
         {
            mindist = dist ;

       //     printf("%s  before initStationFromReport ()\n", header);
            initStationFromReport(&station, pReportListNode);
         }
         else if (dist == mindist)
         {
            //override a PC selection, if there is one
            if ( (strcmp(pReportListNode->pe, "PP") == 0)  )
            {
                  initStationFromReport(&station, pReportListNode);
            }
         }
      }

      pReportListNode = ( ReportList * ) ListNext ( & pReportListNode->node ) ;
   }

   if ( mindist > CLOSE_ENOUGH_IN_PIXELS )
   {
      return NULL ;
   }
   else
   {
      //test to see if the station (including lid and paramCode) are the same as the
      //currently selection station.
      if ( isSameStation(&station, &current_station) ) //already selected, don't select it again
      {
         * new_station = 0 ;
      }
      else //different station, so select it
      {
         current_station = station ;
      }

      return & current_station ;
   }
}

// -------------------------------------------------------------------------------------------


int isSameStation(Station *station1, Station *station2)
{
    int result = 0;

     if
        (
            ( strcmp ( station1->lid , station2->lid ) == 0 )  &&
            ( strcmp ( station1->paramCode , station2->paramCode ) == 0 )
        )
     {
         result = 1;

     }

     return result;
}

// -------------------------------------------------------------------------------------------

void initStationFromReport(Station *stationPtr, ReportList * pReportListNode)
{

      strcpy ( stationPtr->lid , pReportListNode->lid ) ;
      strcpy ( stationPtr->name , pReportListNode->name ) ;
      stationPtr->loc.lat = pReportListNode->latitude ;
      stationPtr->loc.lon = pReportListNode->longitude ;
      stationPtr->pos_shift.x = pReportListNode->x_shift;
      stationPtr->pos_shift.y = pReportListNode->y_shift;
      getParamCodeFromReport(pReportListNode,  stationPtr->paramCode);

      return;

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

Station * get_current_station ( )
{
   if ( current_station.lid [ 0 ] == 0 )
   {
      return NULL ;
   }
   else
   {
      return & current_station ;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
