/****************************************************************************
   File:           pointcontrol_getdata.c 
   
   This files contains the functions related to the "get data" 
   step in the overall pointcontrol data access features.
   
   getRiverData()
   getRainData()
   getSnowTempOtherData()
   
   build_river_where()
   build_riverstatus_where()
   build_precip_where()
   build_SnowTempOther_where()
   
   durhours_to_shefcode()
   

   History:
      Bryon Lawrence   03/10/02     Tried to reduce the possibility of 
                                    NULL pointer reads occurring in this
                                    routine. This is being done in response
                                    to DR 10530.

   **************************************************************************/

#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#include "CurPC.h"
#include "CurPP.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "load_PCPP_data.h"
#include "Observation.h"
#include "pointcontrol_getdata.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "RiverStatus.h"

#define DEFAULT_HOURS_IN_WINDOW 4
#define MINMAX_DUR_MULTIPLIER 1.5
#define NUM_WHERE_CHAR 500
#define REPLY_LEN 100

/*****************************************************************************
   get_change_hour_window ()
   Retrieves the value of the hv_hours_in_window token.
   ***************************************************************************/
int get_change_hour_window ( )
{
   static char * hv_hours_token_name = "hv_hours_in_window" ;
   char reply [ REPLY_LEN ] ;
   static int first = 1 ; 
   int reply_len ;
   int request_len ;
   int status ;
   static int hours_in_window = DEFAULT_HOURS_IN_WINDOW ;
   
   if ( first == 1 )
   {
      request_len = strlen ( hv_hours_token_name ) ;
      status = get_apps_defaults ( hv_hours_token_name , & request_len , 
                                   reply , & reply_len ) ;

      if ( ( status == 0 ) && ( strlen ( reply ) > 0 ) )
      {
         hours_in_window = atoi ( reply ) ;
      }

      first = 0 ;
   }

   return hours_in_window ;
}

/*****************************************************************************
   getRiverData() 
   Get the river data, which can be either height
   or discharge, either observed or forecast, or
   can be any designated type-source.
   
   ***************************************************************************/
void getRiverData(const pc_options_struct * pc_options,
		  Observation		**obshHead,
		  Observation		**obsdHead,
		  RiverStatus		**rsHead)
{
   char header[] = "-------------- getRiverData(): ------------------- ";
   char 	where[NUM_WHERE_CHAR] ;
   
   /* if not getting the Latest data, then need to go to the
      full PE tables with the data - i.e. discharge and height tables. */
    
   if (pc_options->time_mode != LATEST)
   {    
      /* if getting data for the "primary" river pe, then 
	 do two separate retrievals, since the data may be 
	 in either table for a given location. */
      
      if (pc_options->Primary)
      {
		 build_river_where(pc_options, "H", where);
         
          printf("%s Height where =  :%s: ",header, where);   
		 
		 if (DEBUG) printf("Height %s", where);	 
         *obshHead = GetObservation(where, "Height");
		 
         if (DEBUG && *obshHead != NULL ) 
	            printf("  :count=%d\n", ListCount(&(*obshHead)->list));
		 
		 build_river_where(pc_options, "Q", where);
		 
		 if (DEBUG) printf("Discharge %s", where);	 
		 *obsdHead = GetObservation(where, "Discharge");      
	
    	 if ( DEBUG && *obsdHead != NULL ) 
	            printf("  :count=%d\n", ListCount(&(*obsdHead)->list));
      }
						
      else if (strncmp(pc_options->selectedAdHocElementString, "H", 1) == 0)
      {
		 build_river_where(pc_options, "", where);
         
         printf("%s Height where =  :%s: ",header, where);   
	
		 if (DEBUG) printf("Height %s", where);	 
		 *obshHead = GetObservation(where, "Height");
		
         if ( DEBUG && *obshHead != NULL )
	            printf("  :count=%d\n", ListCount(&(*obshHead)->list));
      }
      
      else if (strncmp(pc_options->selectedAdHocElementString, "Q", 1) == 0)
      {
		 build_river_where(pc_options, "", where);
         
         printf("%s Discharge where =  :%s: ",header, where);   
         
		 
		 if (DEBUG) printf("Discharge %s", where);	 
         
		 *obsdHead = GetObservation(where, "Discharge");
                   
		 if ( DEBUG && *obsdHead != NULL )
	         printf("  :count=%d\n", ListCount(&(*obsdHead)->list));
      }      
   }
   
   /* if getting latest river data, always get the data from
      the RiverStatus table. the where clause is built with 
      knowledge of whether getting primary data, and whether
      getting a specific pe or ts */
   
   else //time mode == LATEST
   {
      build_riverstatus_where(pc_options, where);
      
      printf("%s RiverStatus where =  :%s:\n ",header, where);   
     
      
     // if (DEBUG) printf("RiverStatus %s\n", where);      
	  
      *rsHead = GetRiverStatus(where);
                
      if ( DEBUG && *rsHead != NULL ) 
      {
         printf("%s  :ListCount=%d\n", header, ListCount(&(*rsHead)->list));
      }
   }


    
   return;
}


/*****************************************************************************
   getRainData()
   **************************************************************************/
void getRainData(const pc_options_struct *pc_options,
		 CurPC		        **pcHead,
		 CurPP		        **ppHead)
{   
	
   char header[] = "getRainData(): ";	
   char   ** typeSourceArray = NULL;
   const char   * where = NULL ;
   double       elapsed_time ;
   int 			i = 0;
   int          num_ts ;
   int		pc_count ;
   int          pp_count ;
   
   time_t       curtime_t ;
   time_t       maxtime_t ;
   time_t       mintime_t ;
   struct timeval btime ;
   struct timeval etime ;
   
   
   printf("%s pc_options->filter_by_typesource = %c \n", 
   		  header, pc_options->filter_by_typesource);
   
   if ( ( int ) pc_options->filter_by_typesource == 1 ) 
   {
   	  num_ts = pc_options->type_source_chosen_count;
   	  
   	  typeSourceArray = malloc(num_ts *  sizeof(char *));
   	  
   	  for (i = 0; i < num_ts; i++)
   	  {
   	      typeSourceArray[i] = (char *) malloc(sizeof(TypeSourceString) * sizeof(char));	
   	  	  if (typeSourceArray[i] != NULL)
   	  	  {
   	  	      strcpy(typeSourceArray[i], pc_options->type_source_chosen_array[i]); 	
   	  	  }
   	  }
   }
   else
   {
      num_ts = 0 ;
   }

   /* get the appropriate data */
   time ( & curtime_t ) ;

   /* set the time window based on the time mode */
   if (pc_options->time_mode == LATEST)
   {      
      mintime_t = curtime_t - pc_options->dur_hours*3600 - 3600;
      maxtime_t = curtime_t;
   }
   /* min and max queries are not meaningful; use settime
      in that case */
   else
   {
      mintime_t = pc_options->valid_timet - pc_options->dur_hours*3600;
      maxtime_t = pc_options->valid_timet + pc_options->dur_hours*3600;      
   }
   
      
   if (pc_options->PCandPP) 
   {
      gettimeofday ( & btime , NULL ) ;

      * pcHead = ( CurPC * ) load_PC_raw ( mintime_t , maxtime_t , "" ,
                                          (const char **) typeSourceArray , num_ts , CurRawPrecip , 
                                           & pc_count ) ;

      where = get_pcpp_query ( );
      fprintf ( stdout, "\nSELECT * FROM CurPC %s\n", where );

      gettimeofday ( & etime , NULL ) ;
      elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                     ( double ) ( etime.tv_usec - btime.tv_usec ) / 1000000. ;

      if ( DEBUG && * pcHead != NULL )
      {
		 printf("  :count=%d  %5.3f sec\n", pc_count , elapsed_time ) ;
      }
      
      gettimeofday ( & btime , NULL ) ;

      * ppHead = ( CurPP * ) load_PP_raw ( mintime_t , maxtime_t , "" ,
                                          (const char **) typeSourceArray , num_ts , CurRawPrecip , 
                                           & pp_count ) ;
      
      where = get_pcpp_query ( );
      fprintf ( stdout, "\nSELECT * FROM CurPP %s\n", where );

      gettimeofday ( & etime , NULL ) ;
      elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                     ( double ) ( etime.tv_usec - btime.tv_usec ) / 1000000. ;

      if (DEBUG && * ppHead != NULL )
      {
		 printf("  :count=%d   %5.3f sec\n", pp_count , elapsed_time );
      }
   } 
   
   else //not (pc_options->PCandPP) 
   {      
      
      if (strncmp(pc_options->selectedAdHocElementString, "PC", 2) == 0)
      {
         gettimeofday ( & btime , NULL ) ;

         * pcHead = ( CurPC * ) load_PC_raw ( mintime_t , maxtime_t , "" ,
                                             (const char **) typeSourceArray , num_ts , CurRawPrecip , 
                                              & pc_count ) ;

         where = get_pcpp_query ( );
         fprintf ( stdout, "\nSELECT * FROM CurPC %s\n", where );

         gettimeofday ( & etime , NULL ) ;
         elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                        ( double ) ( etime.tv_usec - btime.tv_usec ) / 
		        1000000. ;
	 
		 if ( DEBUG && * pcHead != NULL )
		 {
		    printf("  :count=%d  %5.3f\n", pc_count , elapsed_time ) ;
		 }
      }
      else //not PC
      {	 
         gettimeofday ( & btime , NULL ) ;
         * ppHead = ( CurPP * ) load_PP_raw ( mintime_t , maxtime_t , "" ,
                                             (const char **) typeSourceArray , num_ts , CurRawPrecip ,
                                              & pp_count ) ;
         where = get_pcpp_query ( );
         fprintf ( stdout, "\nSELECT * FROM CurPP %s\n", where );

         gettimeofday ( & etime , NULL ) ;
         elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                        ( double ) ( etime.tv_usec - btime.tv_usec ) / 
		        1000000. ;
	 
		 if ( DEBUG && * ppHead != NULL )
		 {
		    printf("  :count=%d  %5.3f\n", pp_count, elapsed_time);
		 }
      }     
   }
     
   
   // free memory for ts array  
    for (i = 0; i < num_ts; i++)
   	{
   	    if (typeSourceArray[i] != NULL)
   	  	{
   	        free(typeSourceArray[i]);
   	        typeSourceArray[i] =  NULL;
   	  	}
   	}
   	if (typeSourceArray != NULL)
   	{
   	    free(typeSourceArray);
   	    typeSourceArray = NULL;	
   	}
   
   return;
}


/******************************************************************************
   getSnowTempOtherData()
   ***************************************************************************/
void getSnowTempOtherData(pc_options_struct 	*pc_options,
			  Observation		**obsHead,
			  LatestObsValue	**latHead)
{   
  // char header[] = "getSnowTempOtherData(): ";		
   char 	tablename[40];
   char		where[NUM_WHERE_CHAR];
   char     typeSource[SHEF_TS_LEN + 1];
   int		shef_postlatest;
   int		count;
   
   
   
   /* check whether the latest value is stored
      in the LatestObsValue table and use it if it is
      since this table is usually much smaller than 
      the PE table, and the query will be faster. */
   shef_postlatest = check_ShefPostLatest();
   
   
   /* set the where clause, which is independent of whether being
      read from the PE table or the LatestObsValue table. */
   build_SnowTempOther_where(pc_options, where);
   
   /* look in the applicable PE table if looking for a set time
      of data, or the min or max, or if the latest obs are
      not being written to the latestobsvalue table. */
   
   if ( pc_options->time_mode != LATEST || ! shef_postlatest )
   {
   	  if ( (pc_options->type_source_chosen_count < 1) || (pc_options->filter_by_typesource == 0) )
   	  {
   	     pc_options->type_source_chosen_count = 1;
   	     strcpy(pc_options->type_source_chosen_array[0], "XX");   
   	  }
   
      memset(typeSource, '\0', SHEF_TS_LEN+1);
      strncpy(typeSource, pc_options->type_source_chosen_array[0], SHEF_TS_LEN);
        	  
      getTableName(pc_options->selectedAdHocElementString,
                   typeSource,
                   tablename);

      if (DEBUG) printf("%s %s", tablename, where);
      
      *obsHead = GetObservation(where, tablename);
      
      if (DEBUG)
      {
         /*
	 count = ListCount(&(*obsHead)->list);
	 printf("  :count=%d\n", count);
         */
      }
   }
   
   
   /* even though the extremum code is in the key of the LatestObsValue
      table, we can't look there for extremums because the settime field
      may be set to find an old extremum; i.e. that is not the latest. */
   
   else
   {
      if (DEBUG) printf("LatestObsValue %s", where);
      
      *latHead = GetLatestObsValue(where);
      
      if (DEBUG && *latHead != NULL )
      {
	 count = ListCount(&(*latHead)->list);
	 printf("  :count=%d\n", count);
      }
   }
   
     
   return;
}


/******************************************************************************
   build_river_where()
   Only used if getting data from Height or Discharge table.
   ***************************************************************************/
void build_river_where(const pc_options_struct * pc_options,
		       char		 *pe,
		       char		 *where)
{
   time_t	curtime_t;
   time_t  	mintime_t = ( time_t ) -1 , maxtime_t = ( time_t ) -1 ;
   time_t       lower_change_basetime_t = ( time_t ) -1 ;
   time_t       lower_change_lowertime_t = ( time_t ) -1 ;
   time_t       lower_change_uppertime_t = ( time_t ) -1 ;
   time_t       upper_change_basetime_t = ( time_t ) -1 ;
   time_t       upper_change_uppertime_t = ( time_t ) -1 ;
   time_t       upper_change_lowertime_t = ( time_t ) -1 ;

   char		maxtime_ansi[ANSI_TIME_LEN] ;
   char		mintime_ansi[ANSI_TIME_LEN] ;
   char         lower_change_upper_ansi[ANSI_TIME_LEN] ; 
   char         lower_change_lower_ansi[ANSI_TIME_LEN] ;
   char         upper_change_upper_ansi[ANSI_TIME_LEN] ; 
   char         upper_change_lower_ansi[ANSI_TIME_LEN] ; 
   
   char		durcode;
   char		tbuf [ NUM_WHERE_CHAR ] ;

   int          change_hour_window ;
   float        change_seconds_window ;
   
   /* get the time */
   time(&curtime_t);
   
   /* begin the where clause with the pe filter. the function
      argument pe is only used for special cases, when reading 
      data for the "primary" pe, which can be some H* pe from the
      height table or some Q* pe from the discharge table. */
   if (strlen(pe) > 0)
   {
      sprintf(where, "WHERE pe like '%s%%' ", pe);
   }
   else
   {
      sprintf(where, "WHERE pe = '%s' ", pc_options->selectedAdHocElementString);
   }
   
   /* if a type source is specified, then filter on the type-source */
   
   if (pc_options->filter_by_typesource == 1) 
   {
      memset(tbuf, '\0', NUM_WHERE_CHAR );
      build_type_source_where_filter(tbuf, pc_options);
     // sprintf(tbuf, " AND ts = '%s' ", pc_options->selectedTypeSrc);
      strcat(where, tbuf);
   }
   
   /* set the time window based on the time mode, and filter 
      further if min/max time modes in effect */
   
   if (pc_options->time_mode == MAXSELECT || pc_options->time_mode == MINSELECT)
   {
      mintime_t = pc_options->valid_timet - 
	 ((float ) ( pc_options->dur_hours ) * 3600. * ( float )
                  (MINMAX_DUR_MULTIPLIER));
      maxtime_t = pc_options->valid_timet;
      
      durcode = durhours_to_shefcode(pc_options);
      
      memset(tbuf, '\0', NUM_WHERE_CHAR );
      sprintf(tbuf, " AND extremum = '%c' ", durcode);
      
      strcat ( where , tbuf ) ;
   }
   else if (pc_options->time_mode == LATEST)
   {
      
      mintime_t = curtime_t - pc_options->dur_hours*3600;
      maxtime_t = curtime_t;
   }
   else if (pc_options->time_mode == SETTIME )
   {
      mintime_t = pc_options->valid_timet - pc_options->dur_hours*3600;
      maxtime_t = pc_options->valid_timet + pc_options->dur_hours*3600;      
   }
   else if ( pc_options->time_mode == VALUE_CHANGE )
   {
      /* Retrieve the number of hours that can be searched around the
         end times of the change period. */
      change_hour_window = get_change_hour_window ( ) ;
      change_seconds_window =   ( ( float ) change_hour_window / 2.0 ) 
                                 * 3600. ;

      upper_change_basetime_t = pc_options->valid_timet ;
      lower_change_basetime_t = pc_options->valid_timet -
                                pc_options->dur_hours * 3600 ;

      upper_change_uppertime_t = upper_change_basetime_t + 
                                 change_seconds_window ;
      upper_change_lowertime_t = upper_change_basetime_t - 
                                 change_seconds_window ;

      lower_change_uppertime_t = lower_change_basetime_t +
                                 change_seconds_window ;
      lower_change_lowertime_t = lower_change_basetime_t - 
                                 change_seconds_window ;

   }

   if ( pc_options->time_mode == VALUE_CHANGE )
   {
      timet_to_yearsec_ansi ( upper_change_uppertime_t , 
                              upper_change_upper_ansi );
      timet_to_yearsec_ansi ( upper_change_lowertime_t , 
                              upper_change_lower_ansi );
      timet_to_yearsec_ansi ( lower_change_uppertime_t , 
                              lower_change_upper_ansi );
      timet_to_yearsec_ansi ( lower_change_lowertime_t , 
                              lower_change_lower_ansi );
      memset(tbuf, '\0', NUM_WHERE_CHAR ) ;

      sprintf ( tbuf , " AND ( ( obstime >= '%s' AND obstime <= '%s' ) "
                       " OR ( obstime >= '%s' AND obstime <= '%s' ) ) "
                       " AND value != %f "
	               " ORDER BY lid ASC, ts, obstime DESC" ,
                       upper_change_lower_ansi , upper_change_upper_ansi ,
                       lower_change_lower_ansi , lower_change_upper_ansi ,
                       MISSING_VAL ) ;
   }
   else
   {
      timet_to_yearsec_ansi ( maxtime_t , maxtime_ansi ) ;
      timet_to_yearsec_ansi ( mintime_t , mintime_ansi ) ;
   
      memset(tbuf, '\0', NUM_WHERE_CHAR ) ;

      sprintf(tbuf, 
   	      " AND obstime >= '%s' AND obstime <= '%s' "
	      " AND value != %f "
	      " ORDER BY lid ASC, ts, obstime DESC", 
	      mintime_ansi, maxtime_ansi, MISSING_VAL);
   }
   
   strcat(where, tbuf);
   return;
}


/*******************************************************************************
   build_riverstatus_where()
   Build a where clause to be used for retrievals from the
   RiverStatus table.
   ****************************************************************************/
void build_riverstatus_where(const pc_options_struct * pc_options,
			     char 		*where)
{
    
   char header[] = "build_riverstatus_where(): "; 
   
 
   time_t	curtime_t;
   time_t  	mintime_t = ( time_t ) -1 , maxtime_t = ( time_t ) -1 ;    
   char    	maxtime_ansi[ANSI_TIME_LEN];
   char 	mintime_ansi[ANSI_TIME_LEN];
   char		tbuf[140];   
   
   
   printf("%s start: \n", header);
   
   /* get the current time */
      
   time(&curtime_t);
   
   
   /* filter by pe if not getting "primary" river value */
   
   if (pc_options->Primary) 
      sprintf(where, "WHERE pe like '%%%%' ");
    
   else
   {
      sprintf(where, "WHERE pe = '%s' ", pc_options->selectedAdHocElementString);
   }
   
   /* filter by typesource if requested. */
   
   if (pc_options->filter_by_typesource == 1) 
   {
      memset(tbuf, '\0', 140);
      build_type_source_where_filter(tbuf, pc_options);
 //     sprintf(tbuf, "AND ts = '%s' ", pc_options->selectedTypeSrc);
      strcat(where, tbuf);
   }
   else
   {
      memset(tbuf, '\0', 140);
      sprintf(tbuf, "AND ts NOT LIKE 'C%%' ");
      strcat(where, tbuf);
   }
      
   
   // added 5/31/06 for OB7.2
   // add the dur_hours variable, so that this routine works properly
   // with TIME_STEP_MODE
   // what it was doing in TIME_STEP_MODE before was demanding RiverStatus data only from the future.
   // Now, if in TIME_STEP_MODE, it uses a 24-hour window
   int dur_hours = 0;   
      
   pc_options_struct *overall_options = get_pc_options();
   
   if (overall_options->query_mode == TIME_STEP_MODE)
   {
        dur_hours = 24;     
   }
   else
   {
        dur_hours = pc_options->dur_hours;
   }
   
       
     /* set the time window based on the time mode.
      if min/max given use settime mode */
    
   
   if (pc_options->time_mode == LATEST)
   {
      mintime_t = curtime_t - dur_hours*3600;
      maxtime_t = curtime_t;
   }
   
   else if (pc_options->time_mode == SETTIME)
   {
      mintime_t = pc_options->valid_timet - dur_hours*3600;
      maxtime_t = pc_options->valid_timet + dur_hours*3600;
   }
      
    
   
   timet_to_yearsec_ansi(mintime_t,   mintime_ansi);
   timet_to_yearsec_ansi(maxtime_t,   maxtime_ansi);
   
   memset(tbuf, '\0', 140);
   
  
   
   if (pc_options->time_mode == LATEST)
      sprintf(tbuf,
	      " AND validtime >= '%s' "
	      " AND value != %f "
	      " ORDER BY lid ASC, pe, ts, validtime DESC", 
	      mintime_ansi, MISSING_VAL);
   else
      sprintf(tbuf,
	      " AND validtime >= '%s' AND validtime <= '%s' "
	      " AND value != %f "
	      " ORDER BY lid ASC, pe, ts, validtime DESC", 
	      mintime_ansi, maxtime_ansi, MISSING_VAL);
   
   strcat(where, tbuf);
   
   printf("%s end: where = :%s: \n", header, where);
  
   
   
   return;
}

/*****************************************************************************
   build_SnowTempOther_where()
   
   Build where clause for generic retrievals of data 
   from Observation type tables.  This where clause is 
   also used for retrievals from the LatestObsValue table
   which has a similar structure as the Observation tables.
   ***************************************************************************/
void build_SnowTempOther_where(const pc_options_struct *	pc_options,
			       char 			*where)
{
   time_t	curtime_t;
   time_t  	mintime_t = ( time_t ) -1 , maxtime_t = ( time_t ) -1 ;

   time_t       lower_change_basetime_t = ( time_t ) -1 ;
   time_t       lower_change_lowertime_t = ( time_t ) -1 ;
   time_t       lower_change_uppertime_t = ( time_t ) -1 ;
   time_t       upper_change_basetime_t = ( time_t ) -1 ;
   time_t       upper_change_uppertime_t = ( time_t ) -1 ;
   time_t       upper_change_lowertime_t = ( time_t ) -1 ;

   
   char         maxtime_ansi[ANSI_TIME_LEN] ;
   char         mintime_ansi[ANSI_TIME_LEN] ;

   char         lower_change_upper_ansi[ANSI_TIME_LEN] ;
   char         lower_change_lower_ansi[ANSI_TIME_LEN] ;
   char         upper_change_upper_ansi[ANSI_TIME_LEN] ;
   char         upper_change_lower_ansi[ANSI_TIME_LEN] ;

   char		durcode;   
   char		tbuf [ NUM_WHERE_CHAR ] ;   

   int          change_hour_window ;
   float        change_seconds_window ;
   
   /* get the current time */
      
   time(&curtime_t);
      
   
   /* filter by physical element first */
   sprintf(where, "WHERE pe = '%s' ", pc_options->selectedAdHocElementString);
   
   /* filter by type-source */
   if (pc_options->filter_by_typesource) 
   {
      memset(tbuf, '\0', NUM_WHERE_CHAR);
      build_type_source_where_filter(tbuf, pc_options);
      strcat(where, tbuf);
   }
   
    
   /* set the time window */
   if (pc_options->time_mode == MAXSELECT || pc_options->time_mode == MINSELECT)
   {
      mintime_t = pc_options->valid_timet - 
	 ((float )(pc_options->dur_hours) * 3600. * 
                  (float )(MINMAX_DUR_MULTIPLIER));
      maxtime_t = pc_options->valid_timet;
      
      durcode = durhours_to_shefcode(pc_options);
      
      memset(tbuf, '\0', NUM_WHERE_CHAR);     
      sprintf(tbuf, "AND extremum = '%c' ", durcode);
      
      strcat(where, tbuf);
   }
   else if (pc_options->time_mode == LATEST)
   {      
      mintime_t = curtime_t - pc_options->dur_hours*3600;
      maxtime_t = curtime_t;
      
      memset(tbuf, '\0', NUM_WHERE_CHAR);
      strcat(where, "AND extremum = 'Z' ");
   }
   else if (pc_options->time_mode == SETTIME )
   {
      mintime_t = pc_options->valid_timet - pc_options->dur_hours*3600;
      maxtime_t = pc_options->valid_timet + pc_options->dur_hours*3600;
      
      memset(tbuf, '\0', NUM_WHERE_CHAR);
      strcat(where, "AND extremum = 'Z' ");
   }
   else if (pc_options->time_mode == VALUE_CHANGE )
   {
      /* Retrieve the number of hours that can be searched around the
         end times of the change period. */
      change_hour_window = get_change_hour_window ( ) ;
      change_seconds_window = ( ( float ) change_hour_window / 2.0 ) * 3600. ;

      upper_change_basetime_t = pc_options->valid_timet ;
      lower_change_basetime_t = pc_options->valid_timet -
                                pc_options->dur_hours * 3600 ;

      upper_change_uppertime_t = upper_change_basetime_t +
                                 change_seconds_window ;
      upper_change_lowertime_t = upper_change_basetime_t -
                                 change_seconds_window ;

      lower_change_uppertime_t = lower_change_basetime_t +
                                 change_seconds_window ;
      lower_change_lowertime_t = lower_change_basetime_t -
                                 change_seconds_window ;
   }
   
   if ( pc_options->time_mode == VALUE_CHANGE )
   {
      timet_to_yearsec_ansi ( upper_change_uppertime_t ,
                              upper_change_upper_ansi );
      timet_to_yearsec_ansi ( upper_change_lowertime_t ,
                              upper_change_lower_ansi );
      timet_to_yearsec_ansi ( lower_change_uppertime_t ,
                              lower_change_upper_ansi );
      timet_to_yearsec_ansi ( lower_change_lowertime_t ,
                              lower_change_lower_ansi );
      memset(tbuf, '\0', NUM_WHERE_CHAR ) ;

      sprintf ( tbuf , " AND ( ( obstime >= '%s' AND obstime <= '%s' ) "
                       " OR ( obstime >= '%s' AND obstime <= '%s' ) ) "
                       " AND value != %f "
                       " AND extremum = 'Z' "
                       " ORDER BY lid ASC, ts, obstime DESC" ,
                       upper_change_lower_ansi , upper_change_upper_ansi ,
                       lower_change_lower_ansi , lower_change_upper_ansi ,
                       MISSING_VAL ) ;
   }
   else
   {
      timet_to_yearsec_ansi(mintime_t, mintime_ansi);
      timet_to_yearsec_ansi(maxtime_t, maxtime_ansi);
   
      memset(tbuf, '\0', NUM_WHERE_CHAR);
      sprintf(tbuf,
	      " AND obstime >= '%s' AND obstime <= '%s' "
	      " AND value != %f "
	      " ORDER BY lid ASC, ts, obstime DESC", 
	      mintime_ansi, maxtime_ansi, MISSING_VAL);
   }

   strcat(where, tbuf);
   return;
}


/*****************************************************************************
   durhours_to_shefcode()
   
   The algorithm uses the duration equal to or less than the given
   duration in hours; i.e.. it rounds down.
   
   ***************************************************************************/
char durhours_to_shefcode(const pc_options_struct * pc_options)
{
   char durcode;
   int	dur;
   
   
   /* use a convenient local variable */
   
   dur = pc_options->dur_hours;
   
   
   /* if stack for max codes */
   
   if (pc_options->time_mode == MAXSELECT)
   {
      if (dur >= 24*7)
	 durcode = 'W';
      else if (dur >= 24)
	 durcode = 'X';
      else if (dur >= 18)
	 durcode = 'S';
      else if (dur >= 12)
	 durcode = 'Y';
      else if (dur >=  6)
	 durcode = 'R';
      else if (dur >=  3)
	 durcode = 'E';
      else if (dur >=  1)
	 durcode = 'D';
      else
	 durcode = 'D';
   }
   
   
   /* if stack for min codes */
   
   else
   {
      if (dur >= 24*7)
	 durcode = 'M';
      else if (dur >= 24)
	 durcode = 'N';
      else if (dur >= 18)
	 durcode = 'I';
      else if (dur >= 12)
	 durcode = 'P';
      else if (dur >=  6)
	 durcode = 'H';
      else if (dur >=  3)
	 durcode = 'G';
      else if (dur >=  1)
	 durcode = 'F';
      else
	 durcode = 'F';
   }
     
   return(durcode);
}


/*
   This function creates a portion of a where clause that filters on the type sources in
   pc-options->type_sources_chosen  
*/
void build_type_source_where_filter(char *wherestr,
                                    const pc_options_struct * pc_options_custom)
{
	char header[] = "build_type_source_where_filter(): ";
	int i = 0;
	char formatString[20];
	char catString[100];
	char tsCodeOnly[SHEF_TS_LEN+1];
	memset(catString, '\0', sizeof(catString) * sizeof(char));
	
    for (i = 0; i < pc_options_custom->type_source_chosen_count; i++)
   	{	  	  
   		memset(tsCodeOnly, '\0', SHEF_TS_LEN+1);
   		strncpy(tsCodeOnly, pc_options_custom->type_source_chosen_array[i], SHEF_TS_LEN);
   	    sprintf(formatString, " '%s' ", tsCodeOnly);
   	  	  
   	    strcat(catString, formatString); 	  
   	  	  
   	    if (i !=  (pc_options_custom->type_source_chosen_count - 1) )
   	    {
   	        strcat(catString,  ", ");
   	    }
    }	
	
	
	
	if (pc_options_custom->type_source_chosen_count > 0)
	{
	    sprintf(wherestr, " AND ts in ( %s ) ", catString);
	}
	else
	{
	    strcpy(wherestr, "");	
	}
	
	printf("%s wherestr = :%s: \n", header, wherestr);
	
	return;
}
