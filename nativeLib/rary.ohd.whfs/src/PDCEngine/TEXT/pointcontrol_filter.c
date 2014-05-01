/****************************************************************************
   File: pointcontrol_filter.c 
     
   **************************************************************************/

#include <stdio.h>
#include <math.h>
#include "LocPDC.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_report.h"
#include "GeneralUtil.h"


#define WHERE_CLAUSE_LENGTH 200


/*****************************************************************************
   filter_reports_and_add_info()
   This file filters the report list as per the user instructions.
   reports are filtered by station and/or by value.
   
   This function also conveniently filters out data that is for
   an area, and not for a location.


   History:
   3/10/02     Bryon Lawrence      Modified to reduce the possibility of
                                   NULL pointer dereferences.  Hopefully
                                   this will reduce the occurrences of
                                   segementation violations resulting
                                   from attempts to read unallocated memory.
                                   This is being done in response to 
                                   DR 10530.
				   
   02/25/2003  Jingtao Deng        Modified to remove the primary and secondary
                                   backup service.				   
   03/25/2003  Bryon Lawrence      Modified to incorporate information read
                                   from the new LocPDC view.  This will 
                                   enable the retrieval of the station name,
                                   latitude/longitude, and display class
                                   into the ReportList structure.  
                                   This will also remove dependence on the
                                   HvStation structure in the HvAreal library
                                   for plotting station data.
   
   **************************************************************************/
void filter_reports_and_add_info ( pc_options_struct	* pc_options,
		                   		   ReportList		* obsrepHead2 )
{
   static int		first = 1 ;
      
   ReportList		* rPtr = NULL ;
   LocPDC		* locPDCPtr = NULL ;
   static LocPDC	* locPDCHead = NULL ;
   int			i;
   int			use_report;
   char			where [ WHERE_CLAUSE_LENGTH ] ;
   int                  locpdc_info ;
   int			count_use, count_notuse;
   char			last_locpdc_lid [ LOC_ID_LEN + 1 ] ;
  // char header[] = "filter_reports_and_add_info";
   
   
   /* Continue only if there are data to be processed */
   if (obsrepHead2 == NULL)
   {
      fprintf ( stderr, "\nIn routine 'filter_reports_and_add_info':\n"
                        "Filtering not performed, no data reports.\n");
      return;
   }


   /* Load the information from the LocPDC view the first time this
      routine is called.  This information is required for the data source
      (telem_type column) and service backup filters (hsa column).  It is
      also required for providing the station name, latitude/longitude, 
      and display class for each of the stations in the report list. */
   if ( first )
   {
   	   	
      /* Prepare the where clause. */
      memset ( where , '\0' , WHERE_CLAUSE_LENGTH ) ;

      /* Set up the where clause so that only stations with a nonzero
         post value are loaded. */
      strcpy ( where , "WHERE post != 0 ORDER BY lid" ) ;
      
      SetLocPDCErrorLogging(1);
  
      locPDCHead = GetLocPDC ( where ) ;
 

      if ( locPDCHead == NULL )
      {
         
         fprintf ( stderr , "\nIn routine 'filter_reports_and_add_info':\n"
	                    "No records retrieved from the LocPDC view\n"
                            "for query 'SELECT * FROM LocPDC %s'.\n" ,
                            where  ) ;
         return ;
      }
      
      
      first = 0;
   }
   else
   {
      /* If the not the first time, then just to be safe, make sure that
         the expected data were already retrieved. */
      if ( locPDCHead == NULL )
      {
         fprintf ( stderr , "\nIn routine 'filter_reports_and_add_info': "
	                    "Empty LocPDC table! Check database.\n" ) ;
         return ;
      }
   }
   
   
   
   /* Get the last lid in the list for later use. */
   locPDCPtr = ( LocPDC * ) ListLast ( & locPDCHead->list ) ;
   strcpy(last_locpdc_lid, locPDCPtr->lid);
   locPDCPtr = locPDCHead ;
   count_use = count_notuse = 0;
   
   
 //  printf("%s obsrepHead ListCount = %d \n", header, ListCount(&obsrepHead2->list));
   

   /* Loop on the reports and filter out those as per user specifications.
      At the same time, add the necessary information from the
      LocPDC view. */
   rPtr = ( ReportList * ) ListFirst ( & obsrepHead2->list ) ;   

   while ( rPtr )
   {
      
      /* initialize convenient variable. assume okay */
      use_report = 1;
      
      /* Get the LocPDC view record which corresponds to this this location. 
         This method takes advantage of the sorted nature of both lists, and
	 moves the locPDC pointer along with the report list
	 pointer so that for each report pointer, it does not have
	 to make a pass thru the entire locPDC list, it only
	 looks starting where it left off with the previous report. 
	 obviously, this is more efficient. */
	 
      /* loop until we find the entry for the current lid */
      while ( strcmp ( locPDCPtr->lid , rPtr->lid ) < 0 )
      {
	 /* check if we are already at the end of the list, in 
	    order to avoid an infinite loop condition.  this 
	    would occur if the rPtr->lid is say ZZZZ, but the last
	    item found in stnclass is ZZZX.  this situation is 
	    possible if ZZZZ is a non-point location (i.e. an area)
	    or if the location has its post flag set to False. In this
	    case, the stnclass list will not have the location (see 
	    the query above), but the station may have been added
	    as missing because the ingestfilter check used for missing
	    does not check the location.post setting (it should but 
	    that would slow things down, and its okay not to since the
	    station gets filtered out anyway because the stnclass was not
	    found (see if check above). */
	         
		 if ( strcmp ( locPDCPtr->lid , last_locpdc_lid ) == 0 )
		 {
		    if ( DEBUG )
		    {
		       printf("breaking out of locpdc search since at last stnclass"
		       "%s:%s\n" , locPDCPtr->lid , rPtr->lid ) ;
		    }
		    break;
		 }
		       
		 /* Increment the loop until it is found. */
		 locPDCPtr = ( LocPDC *) ListNext ( & locPDCPtr->node ) ;
		    
		 if ( locPDCPtr == NULL)
		 {
		    if (DEBUG)
		    {
		       printf("breaking out of locpdc search since past"
	                      "last locpdc");
		    }
	                      
		    break;
		 }
	    
     }
      
      /* do a check to make sure that there is a station class entry
	 for the data value.  there may NOT be one in the case
	 that a value for an AREA entity was found in the pe table. 
	 if an entry is not found, since presumably it would not
	 any location info such as lat-lon defined for it. */
	 if ( strcmp ( locPDCPtr->lid , rPtr->lid ) != 0 )
	 {
	 	 /*
		 if (DEBUG) 
		 {
		 	printf("In routine 'filter_reports_and_add_info ':\n"
		            "Ignoring %s; no post or locPDC information.\n" , 
		                        rPtr->lid);	 
		 }
		 */
		 use_report = 0 ;
		 locpdc_info = 0 ;	 
	 }
	 else
	 {
		 locpdc_info = 1 ;
	 }
	 
	 /* add elevation info  */
	 rPtr->elevation = locPDCPtr->elev;
     
     
     
     // in TIME STEP Mode only,
     // do a filter by typeseource.  Ad Hoc mode does this in SQL 5/19/06
     if ( ( pc_options->query_mode == TIME_STEP_MODE)  && ( int ) pc_options->filter_by_typesource== 1 ) 
     {
          //assume it is not found
          use_report = 0;
          
          // if any TS matches, then display the station
          for (i = 0; i < pc_options->type_source_chosen_count; i++)
          {
           //     printf("%s pc_options->type_source_chosen_array[i] = :%s: \n", header, pc_options->type_source_chosen_array[i]);
            
                if (strstr(pc_options->type_source_chosen_array[i], rPtr->ts))
                {
                    use_report = 1;
                    break;
                }
            

          }
     }
     
     
	 
      
      /* if the user requests that the stations are to be filtered by the
	 station data source, then apply the filter. */
      if ( (pc_options->query_mode == AD_HOC_MODE )  &&  (pc_options->filter_by_datasource && locpdc_info ) )
      {
	      use_report = 0;
	 
	   
		 for ( i = 0 ; i < pc_options->data_sources_chosen_count ; i++ )
		 {
		    if ( strcmp ( pc_options->data_sources_chosen_array [ i ] ,
	                 OBSERVER_STRING ) == 0 )
		    {
		        if (locPDCPtr->is_observer [ 0 ] != 'F' )
				    use_report = 1;
		    }
		    
		    else if (strcmp(pc_options->data_sources_chosen_array[i], DCP_STRING) == 0)
		    {
		       	if (locPDCPtr->is_dcp [ 0 ] != 'F' )
			  		use_report = 1;
		    }
		    
		    else if ( strcmp ( pc_options->data_sources_chosen_array [ i ] , 
	                               UNDEFINED_STRING ) == 0 )
		    {
		       if (locPDCPtr->is_dcp[0] != 'T' &&
			   locPDCPtr->is_observer[0] != 'T' &&
			   (strlen(locPDCPtr->telem_type) == 0))
			  		use_report = 1;
		    }
		    
		    else
		    {
		       if (strcmp(pc_options->data_sources_chosen_array[i],
				  locPDCPtr->telem_type) == 0)
			 		 use_report = 1;
		    }
		 }	
         
  //       printf("%s filtering by datasource locPDCPtr->lid = %s  rPtr->lid = %s  use_report = %d \n", header, locPDCPtr->lid, rPtr->lid, use_report);
    
          	 
      }  /* end of if checking whether to filter by data source */
      
      
      
      
      
      /* if the supress non-fcst point options is enabled,
         then filter out any stations that are not forecast points. */
      if ( pc_options->fcstpts_only && locpdc_info )
      {
		 if ( strchr ( locPDCPtr->disp_class , FCSTPT_STNCLASS ) == NULL )
	     {
		    use_report = 0 ;
	     }
      }

      /* Apply the service backup filter if enabled.  This test compares
         the hsa defined for the current location (as given in
         the locPDCPtr) with that selected from the serivce backup option
         on the pointcontrol gui. Multiple HSAs may be selected from this
         option and are represented as a comma separated list in the
         pc_options->hsa_list. */
      if ( pc_options->filter_by_hsa && locpdc_info )
      {
         if ( use_report == 1 )
         {
		    if ( strcmp ( pc_options->hsa_list , ALL_AREAS ) != 0 &&
		         strstr ( pc_options->hsa_list , locPDCPtr->hsa ) == NULL )
	            {
	               use_report = 0 ;
	            }
         }
	 	 
      }
      
      
      /* now suppress missing data and zero data if the
         suppress option is enabled */
      if ( pc_options->suppress_missing )
      {
		 if ( rPtr->value == MISSING_VAL )
	         {
		    use_report = 0;
	         }
      }
      
      
      //pass through value filter
      if (isFilteredOutByValue(pc_options->valueFilterOperation, 
                               pc_options->valueFilterValue, 
                               rPtr->value))
      {
          use_report = 0;	
      }
      
      
      //pass through elevation filter
      if (isFilteredOutByValue(pc_options->elevFilterOperation, 
                               pc_options->elevFilterValue, 
                               locPDCPtr->elev))
      {
          use_report = 0;	
      }
      
      /* REMOVED for OB7 - since a fancy new filter is replacing it
      if ( pc_options->suppress_zeros )
      {
	 if ( rPtr->value == 0.0 )
         {
	    use_report = 0 ;
         }
      }
      */
           
      rPtr->use = use_report;
      
      if ( use_report )
      {
         count_use++ ;
         
         /* Copy the latitude and longitude into the current report
           structure being processed. */
         rPtr->latitude = locPDCPtr->lat ;
         rPtr->longitude = -1 * locPDCPtr->lon ;

         /* Copy the station name into the current report structure being
            processed. */
         memset ( rPtr->name , '\0', REPORTLIST_STATIONNAME_LEN + 1 ) ;
         strncpy ( rPtr->name , locPDCPtr->name , 
                   REPORTLIST_STATIONNAME_LEN ) ;

         /* Copt the display class of the station into the 
            current report structure being processed. */
         memset ( rPtr->disp_class , '\0' ,
                  REPORTLIST_DISPLAYCLASS_NAME_LEN + 1 ) ;
         strncpy ( rPtr->disp_class , locPDCPtr->disp_class ,
                   REPORTLIST_DISPLAYCLASS_NAME_LEN ) ;
      }
      else
      {
		 count_notuse++;
      }
            
      rPtr = ( ReportList * ) ListNext ( & rPtr->node );      
      
   } /* end of loop on reports */
   
   if (DEBUG)
      printf("Total of %d reports; %d filtered out.\n",
	     count_use + count_notuse, count_notuse);
   
   
    
   
   return;
}

/* --------------------------------------------------------------------------------*/

int isFilteredOutByValue(FilterOperation operation, double comparisonValue,  double valueToBeExamined)
{
//	char header[] = "isFilteredOutByValue(): ";
	int filteredOut = 0; 
	/*
	 * Note: this function determines if a value FAILS the test, so if a value does NOT meet the
	 * criteria, the result is TRUE.
	 * */ 
	int passesTest = 0;
		
	if (operation != SHOW_ALL)
	{
		//round to 2 decimal places
		valueToBeExamined = round_to_places(valueToBeExamined, 2);
		comparisonValue =   round_to_places(comparisonValue, 2);
	}
	 
	 
	if (operation == SHOW_ALL)
	{
		 passesTest = 1;	
	}
	if (operation == SHOW_EQUAL)
	{
	    if 	( valueToBeExamined == comparisonValue) 
	    {
	       passesTest = 1;	
	    }
	}
	
	else if (operation == SHOW_NOT_EQUAL)
	{
	    if 	(valueToBeExamined != comparisonValue)
	    {
	         passesTest = 1;		
	    }
	} 
	 
	else if (operation == SHOW_GREATER_EQUAL)
	{
	    if 	(valueToBeExamined >= comparisonValue)
	    {
	         passesTest = 1;		
	    }
	}
	else if (operation == SHOW_GREATER)
	{
		if 	(valueToBeExamined > comparisonValue)
	    {
	         passesTest = 1;		
	    }
	}
	
	else if (operation == SHOW_LESS_EQUAL)
	{
		if 	(valueToBeExamined <=  comparisonValue)
	    {
	         passesTest = 1;		
	    }
	}
	else if (operation == SHOW_LESS)
	{
		if 	(valueToBeExamined < comparisonValue)
	    {
	         passesTest = 1;		
	    }
	}
	
	
	
	/*
	 * filteredOut is the opposite of passesTest
	 * 
	 * filteredOut = !(passesTest)
	 * 
	 * I write it out explicitly for ease of understanding.
	 * */
	
	if (! passesTest)
	{
	    filteredOut = 1;
	}
	else /* passes */
	{
		filteredOut = 0;
	}
	
	return filteredOut;
	
}


