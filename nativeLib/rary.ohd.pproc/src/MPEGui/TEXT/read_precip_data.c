/*=========================================================================*/
/*                         FILE NAME:  read_precip_data.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  initialize_data_RFCW()              */
/*                                     ReadParameters_RFCW()               */
/*                                     ReadGageData_RFCW()                 */
/*                                     ReadRadarData()                     */
/*=========================================================================*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "delete_polygons_show.h"
#include "draw_precip_poly_RFCW.h"
#include "read_precip_data.h"

#include "polygon_RFCW.h"
#include "PseudoGageVal.h"
#include "Location.h"

#include "BinarySearch.h"         /* for binary search */

#include "display_field_data_RFCW.h"
#include "display_precip_data.h"

#include "get_total_precip.h"
#include "load_PCPP_data.h"
#include "mpe_field_names.h"
#include "mpe_log_utils.h"

#include "read_rresult.h"
#include "read_rwbiasstat.h"
#include "read_rwprefs.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "set_single_site_overlays.h"
#include "stage3.h"
#include "time_convert.h"
#include "drawa.h"
#include "get_loc_latlon.h"
#include "QualityCode.h"      /* for quality control */
#include "get_mpe_loc_latlon.h"

/**************************************************************************
  FUNCTION NAME:   initialize_data_RFCW                                  
       FUNCTION:   calls functions for reading and displaying data       
   
   Function type:   void
      
   Functions called:
   read_radar_data
   ReadGageData_RFCW
   read_radcov_grids
   
***************************************************************************/

short int * xmrgfile_row_array = NULL;

void initialize_data_RFCW ( )
{
   const char ** mpe_field_names = NULL;
   enum DisplayFieldData display_field_type = display_mMosaic;
   int i;
   int status;
   
   /* Retrieve the array of valid MPE field names. */
   mpe_field_names = get_mpe_field_names ( ) ;

   /* initialization  */ 
   DataSaved = FALSE;
   
   /* malloc space for 1 dim array for writing xmrg files         */
   xmrgfile_row_array = (short int *) malloc((MAXX)*sizeof(short int));
   
   /* display initial gridded field based on default_display_type  */

   status = 1 ;

   for ( i = 0 ; i < NUM_COLORUSE_ITEMS ; ++ i )
   {
      status = strcmp ( default_display_type, mpe_field_names [ i ] ) ;

      if ( status == 0 )
      {
         display_field_type = ( enum DisplayFieldData ) i;
         break ;
      }
   }

   /* Was the default display type recognized? */
   if ( status != 0 )
   {
      /* The default display type was not recognized. */
      flogMessage ( stderr, "\nIn routine 'initialize_data_RFCW':\n"
                        "Unrecognized default display field type: %s.\n"
                        "Setting the default to MMOSIAC.\n",
                        default_display_type ) ;
      strcpy ( default_display_type , 
               mpe_field_names [ MULTI_SENSOR_MOSAIC ] ) ;
      display_field_type = display_mMosaic ;
   }

   for ( i = 0; i < NUM_MAP_SCREENS; ++i )
   {
      rad_data [ i ].field_type = display_field_type ;
   } 

   
   return;
}


/***************************************************************************
  FUNCTION NAME:  ReadParameters_RFCW()                                  
       FUNCTION:  read in input parameters from RWPrefs, RWBiasStat and  
                  ColorValue tables and initialize other variables     
   
   Function type:  void
      
   Functions called:
   read_rwprefs
   read_rwbiasstat
   set_coloroverlays
     
***************************************************************************/
   
void ReadParameters_RFCW ( )
{   
   int len;
   int int1, int2, int3, int4, int5, int6, int7;
   float float1;
   long int irc;
   
   /* read user preferences from RWPrefs table. if record
      not found for userid, then used values */
   read_rwprefs ( ); 

   /* Set the initial state (on or off) of the overlays
      being used in the single site radar window.  The state will initially
      match that of the overlays in the main GUI because both are coming
      from the overlay configuration file. */  
   set_single_site_overlays ( ) ;
   
   /* read memory span values from RWBiasStat table */
   read_rwbiasstat(&float1,
		   &int1, &int2, &int3,  &int4,
		   &int5, &int6, &int7,
		   memspan_values, &irc);
   
   if (irc != 0)
   {
     logMessage("record not found in RWBiasStat table -- RFCWide stopping\n");
      exit(1);
   }
   

   /* read xmrg filename date format from .Apps_defaults */
   
   len = strlen("st3_date_form");
   memset(date_form,'\0',3);
   get_apps_defaults("st3_date_form",&len,date_form,&len);
   if (len == 0) strcpy(date_form,"mdY");
   

   /* initialize other variables */
   
   num_gage_edit = 0;
   num_rad_edit = 0;
   num_prev_poly = 0;
   
   
   return;   
}

static int compare_gage_lids (  void * string_to_compare,
                                void * string_in_array )
{
   char * gage_id = ( char * ) string_to_compare;
   char * bad_gage_id = * ( char ** ) string_in_array;
   int status;

   status = strcmp ( gage_id, bad_gage_id );
   return status;
}

static int compare_gage_ids ( const void * string_to_compare,
                              const void * string_in_array )
{
   char * gage_id = * ( char ** ) string_to_compare;
   char * bad_gage_id = * ( char ** ) string_in_array;
   int status;

   status = strcmp ( gage_id, bad_gage_id );
   return status;
}

/****************************************************************************
   FUNCTION NAME:  ReadGageData_RFCW()                                          
   FUNCTION: reads gage data from ProcPrecip and PseudoGageVal tables  
   NOTE: the data read by this routine is used for display only  
   
   Function type:   void
   
   Functions called:
   GetProcPrecip
   GetPseuedoGageVal
   
   Key global variables:
   gage - actual gage data
   ngages - number of gages located in area of interest matching the 
   obstime and dur = 1001; read from PseudoGageVal and ProcPrecip tables

   History:

   Jan 21, 2006   Bryon Lawrence    Removed restriction that gages must
                                    be with MPE forecast area.
   
   *************************************************************************/
  
void ReadGageData_RFCW ( )
{
   const char * precip_where = NULL ;
   double       elapsed_time ;
   float        xlt, xln;
   float        xhr, yhr;
   double       dlat, dlon;
   HourlyPC  *  pHourlyPC = NULL ;
   HourlyPC  *  pHourlyPChead = NULL ;
   HourlyPP  *  pHourlyPP = NULL ;
   HourlyPP  *  pHourlyPPhead = NULL ;
   HRAP         hrap_point;
   int          k;
   int		proc_count, pseudo_count;
   int          pc_record_cnt = 0 ;
   int          pp_record_cnt = 0 ;
   int          returnvalue;
   int          z;
   List         PolyList;
   rubber_poly_data * pPolyNode = NULL; 
   
   struct timeval btime ;
   struct timeval etime ;
   struct total_precip total_precip ;
   time_t       checktime;
   time_t       endtime ;
   time_t       starttime ;
   char 	where [ 300 ] ;
   PseudoGageVal * pseudoHead = NULL , * pseudoPtr = NULL ;

   FILE *snowFile = NULL;
   FILE *badGageFile = NULL;
   int len = 0;
   char dirname[120];
   char bad_gages_file_name[120];
   char snow_file_name[120];
   char buf[120];

   char ** bad_gage_array = NULL;
   char ** snow_gage_array = NULL;
   int num_bad_gage_records = 0;
   int num_snow_records = 0;
   int i = 0;
   int bad_gages_exist = 0;
   int snow_gages_exist = 0;
   char * pBadTest = NULL;
   char * pSnowTest = NULL;

   int gage_inside_snow_poly = 0;
   FILE *snowList = NULL;
   float gage_value;
   int hrap_x, hrap_y;
   int icol  = 0, irow = 0;

   /* Log a timer message */
      
   time(&checktime);
  logMessage("reading precip gage data  = %s", asctime(gmtime(&checktime)));
   
   /* set the global variable num_gage_edit */
   num_gage_edit = 0;  
   
   /* Read all records from the HourlyPC and HourlyPP tables for this
      datetime. The global datetime variable is from stage3.h, and is 
      defined as char22 */
   
   yearsec_ansi_to_timet ( datetime , & endtime ) ; 
   starttime = endtime - SECONDS_PER_HOUR ;

   /* Start the timer.  This is meant to determine how long it takes
      to read the hourly data from the HourlyPC and HourlyPP tables 
      and how long it takes to compute the precipitation totals. */ 
   gettimeofday ( & btime , NULL ) ;

   pHourlyPChead = load_PC_hourly ( starttime , endtime , NULL , NULL , 0 ,
                                    & pc_record_cnt ) ;

   /* Write out the query used by load_PC_hourly. */
   precip_where = get_pcpp_query ( ) ;
   flogMessage ( stdout, "SELECT * FROM HourlyPC %s\n", precip_where ) ;
   flogMessage ( stdout, "%d records retrieved from HourlyPC.\n", pc_record_cnt );

   pHourlyPPhead = load_PP_hourly ( starttime , endtime , NULL , NULL , 0 ,
                                    & pp_record_cnt ) ;

   /* Write out the query used by load_PP_hourly. */
   precip_where = get_pcpp_query ( ) ;
   flogMessage ( stdout, "SELECT * FROM HourlyPP %s\n", precip_where ) ;
   flogMessage ( stdout, "%d records retrieved from HourlyPP.\n", pp_record_cnt );

   /* This is a PP/PC record count.  It is not an actual count of stations
      to process gage data for. */ 
   proc_count = pc_record_cnt + pp_record_cnt ;
   
   /* read all records from PseudoGageVal table for this datetime */
   sprintf ( where , " WHERE obstime='%s' " , datetime ) ;
  logMessage( " select * from pseudogageval %s\n" , where ) ;
   
   pseudoHead = GetPseudoGageVal(where);

   if (pseudoHead)
      pseudo_count = ListCount(&pseudoHead->list);
   else
      pseudo_count = 0;
   
      
   /* free any previously loaded data */
   
   if (gage != NULL)
   {
      free(gage);
      gage = NULL;
   }
   
   /* malloc space for structures to hold gage data  
      if there are no gages, then return */
   ngages = proc_count + pseudo_count;
   
   if ( ngages == 0 )
   {
      time(&checktime);
     logMessage("no gage data found; %s", asctime(gmtime(&checktime)));
      return;
   }
     
   gage = ( gage_struct * ) malloc ( ngages * sizeof ( gage_struct ) ) ;
   
   if ( gage == NULL )
   {
      flogMessage(stderr, "malloc failed for gage in read_precip_data\n"
                      "-- program stopping\n" ) ;
      exit(0);
   }
   
   /* read through gage data sets to store data in the gage structure */
   /* Initialize the gage count to 0. */
   k = 0;
   
   pHourlyPP = pHourlyPPhead ;
   pHourlyPC = pHourlyPChead ;
   
   len = 0;
   memset(dirname, '\0', 120);
   memset(bad_gages_file_name, '\0', 120);
   len = strlen("mpe_bad_gages_dir");
   get_apps_defaults("mpe_bad_gages_dir",&len,dirname,&len);
   strcpy(bad_gages_file_name, dirname);
   strcat(bad_gages_file_name, "/mpe_bad_gage_list");


   badGageFile = fopen(bad_gages_file_name, "r");
   if(badGageFile == NULL)
   {
     logMessage ("Warning: Could not open bad gages file %s", bad_gages_file_name);
   }
   else
   {
         bad_gages_exist = 0;
	 memset(buf, '\0', 120);
	logMessage("Opened bad gage list file %s ...\n", bad_gages_file_name);
	 
         fscanf(badGageFile, "%s", buf);
         /* Count the records. */
         while (!feof(badGageFile))
         {
            ++num_bad_gage_records;
            fscanf(badGageFile, "%s", buf);
         }

         if ( num_bad_gage_records > 0 )
         {
             bad_gages_exist = 1;

             /* reset file pointer to beginning of file. */
             rewind ( badGageFile );

             bad_gage_array = ( char ** ) malloc ( sizeof ( char * ) * num_bad_gage_records ); 

             if ( bad_gage_array == NULL )
             {
               logMessage ( "Could not allocate memory for the "
                                   "bad gage array.\n" );
                exit ( 1 );
             }

             for ( i = 0; i < num_bad_gage_records; ++i )
             {
                 bad_gage_array [ i ] = NULL;
             }

             /* Read records from the bad gage file. */
             i = 0;
             fscanf (badGageFile, "%s", buf );

             while(( !feof(badGageFile) ) && (i < num_bad_gage_records ) )
             { 
                bad_gage_array[i] = (char * ) malloc ( sizeof ( char ) * ( LOC_ID_LEN + 1));

                if ( bad_gage_array[i] == NULL )
                {
                  logMessage ( "Could not allocate memory for the "
                                      "bad gage array.\n" );
                   exit ( 1 );
                }
                memset ( bad_gage_array[i], '\0', LOC_ID_LEN + 1 );
                strncpy ( bad_gage_array[i], buf, LOC_ID_LEN );
                ++i;
                fscanf (badGageFile, "%s", buf );
             }

             fclose ( badGageFile );
             badGageFile = NULL;

             qsort ( bad_gage_array, num_bad_gage_records,
                     sizeof ( char * ), compare_gage_ids ); 
         }
   }
  
   get_snow_polygons (&PolyList, date_st3.cdate);
/*  
   len = 0;
   len = strlen("rfcwide_drawpre_dir");
   memset(dirname, '\0', 120); 
   get_apps_defaults("rfcwide_drawpre_dir",&len,dirname,&len);
   memset(snow_file_name, '\0', 120);
   strcpy(snow_file_name, dirname);
   strcat(snow_file_name, "/snow_gages");
   memset(buf, '\0', 120);
   sprintf(buf, "%s", date_st3.cdate);
   strcat(snow_file_name, buf);
   strcat(snow_file_name, "z");

   snowFile = fopen(snow_file_name, "r");
   if(snowFile == NULL)
   {
     logMessage ("Warning: Could not open snow gages file %s", snow_file_name);
   }
   else
   {
         snow_gages_exist = 0;
         memset(buf, '\0', 120);
	logMessage("Opened snow gage list file %s ...\n", snow_file_name);
	 
         fscanf(snowFile, "%s", buf);
         // Count the records.
         while (!feof(snowFile))
         {
            ++num_snow_records;
            fscanf(snowFile, "%s", buf);
         }

         if ( num_snow_records > 0 )
         {
             snow_gages_exist = 1;

             // reset file pointer to beginning of file.
             rewind ( snowFile );

             snow_gage_array = ( char ** ) malloc ( sizeof ( char * ) * num_snow_records ); 

             if ( snow_gage_array == NULL )
             {
               logMessage ( "Could not allocate memory for the "
                                   "bad gage array.\n" );
                exit ( 1 );
             }

             for ( i = 0; i < num_snow_records; ++i )
             {
                 snow_gage_array [ i ] = NULL;
             }

             // Read records from the snow gage file.
             i = 0;
             fscanf (snowFile, "%s", buf );

             while(( !feof(snowFile) ) && (i < num_snow_records ) )
             { 
                snow_gage_array[i] = (char * ) malloc ( sizeof ( char ) * ( LOC_ID_LEN + 1));

                if ( snow_gage_array[i] == NULL )
                {
                  logMessage ( "Could not allocate memory for the "
                                      "snow gage array.\n" );
                   exit ( 1 );
                }
                memset ( snow_gage_array[i], '\0', LOC_ID_LEN + 1 );
                strncpy ( snow_gage_array[i], buf, LOC_ID_LEN );
                ++i;
                fscanf (snowFile, "%s", buf );
             }

             fclose ( snowFile );
             snowFile = NULL;

             qsort ( snow_gage_array, num_snow_records,
                     sizeof ( char * ), compare_gage_ids ); 
         }
   }
*/ 
   
   // process the regular gage values first 
   while ( ( pHourlyPP != NULL ) || ( pHourlyPC != NULL ) )
   {	 
      total_precip = get_total_hourly_precip ( & pHourlyPC ,
                                               & pHourlyPP ,
                                               endtime ,
                                               1 ,
                                               0 ,
                                               PRECIP_TS_RANK | PRECIP_PP , 
                                               1 ,
		                               & pc_record_cnt ,
		                               & pp_record_cnt ) ;
    
      flogMessage ( stdout ,"%s  %s  %s %6.2f %c\n" , total_precip.lid ,
                total_precip.PE , total_precip.TS , total_precip.value ,
                total_precip.qc ) ;
      /* Retrieve the Latitude/Longitude of the station. */
      returnvalue = get_mpe_loc_latlon ( total_precip.lid , & dlat , & dlon ) ;
     
      xlt = (float) dlat;
      xln = (float) dlon;
	 
      if(returnvalue == LATLONNOTFOUND)
      {
         continue;   
      }
      if(xlt == 0. || xln == 0.)
      {
         continue;
      }
      
      /* calculate HRAP coordinates from lat,lon  */
      hrap_point = LatLongToHrapMpe(xlt, xln) ;
      xhr = hrap_point.x;
      yhr = hrap_point.y;
	 
      /* make sure the gage is within the area. if so load the info */
      strcpy ( gage[k].id , total_precip.lid ) ;
      strcpy ( gage[k].ts , total_precip.TS ) ;
      gage[k].gval = total_precip.value;



            /* Test if the gage is in the bad gage list */
           if ( bad_gages_exist == 1 )
           {
               pBadTest = NULL;
               pBadTest = (char *) binary_search ( bad_gage_array, 
                                                   total_precip.lid,
                                                   num_bad_gage_records,
                                                   sizeof ( char * ),
                                                   compare_gage_lids ); 

               if ( pBadTest != NULL )
               {
                 logMessage ( "Gage %s is in bad gage file. Skipped ...\n",
                            total_precip.lid );
	          continue;
               }

           }

                    gage_inside_snow_poly = 0;
      	            pPolyNode = (rubber_poly_data *) ListFirst (&PolyList);
                    while (pPolyNode != NULL)
                    {
                       if (pPolyNode->snow_flag == True)
                       {
                          gage_value = total_precip.value;

                         // Convert the gage value to inches.
                         gage_value /= 25.4;

                         if (gage_value <= pPolyNode->draw_precip_value)
                         {
                            hrap_x = icol;
                            hrap_y = irow;

                            if ((hrap_x <= pPolyNode->maxx) &&
                            (hrap_x >= pPolyNode->minx) &&
                            (hrap_y <= pPolyNode->maxy) && (hrap_y >= pPolyNode->miny))
                            {
                               // Determine if this gage is inside or outside the edit polygon.
                               z = InOutPoly (hrap_x, hrap_y, pPolyNode);

                               if (z == 1)
                               {
                                  gage_inside_snow_poly = 1;
                                  break;     //Don't need to loop through any more polygons.
                               }
                            }
                        }
                    }
                    pPolyNode = (rubber_poly_data *) ListNext (&pPolyNode->node);
                 }
                        
                        if(gage_inside_snow_poly == 1)
                        {
                           gage_inside_snow_poly = 0;
		           if(snowList != NULL)
			   {
			      fprintf(snowList, "%s\n", total_precip.lid);
			      fflush(snowList);
                             logMessage ( "Gage %s is in snow gage file. Skipped ...\n", total_precip.lid );
		           }
                           continue;
                        }




	   /* Test if the gage is in the snow gage list */
/*           if ( snow_gages_exist == 1 )
           {
               pSnowTest = NULL;
               pSnowTest = (char *) binary_search ( snow_gage_array, 
                                                   total_precip.lid,
                                                   num_snow_records,
                                                   sizeof ( char * ),
                                                   compare_gage_lids ); 

               if ( pSnowTest != NULL )
               {
                 logMessage ( "Gage %s is in snow gage file. Skipped ...\n",
                            total_precip.lid );
	          continue;
               }

           }
 */

      /* Initialize the qc value of this gage to indicate that
       * it passed both the spatial consistency and multisensor
       * qc checks. */
      gage[k].qc = 3 ;   
	    
      gage[k].hrap.x = xhr;
      gage[k].hrap.y = yhr;
	    
      gage[k].hrap_loc.x = gage[k].hrap.x - ( float ) XOR ;
      gage[k].hrap_loc.y = gage[k].hrap.y - ( float ) YOR ;
	    
      if ( total_precip.qc == 'M' )
      {
         gage[k].manedit = 1;
      }
      else
      {
         gage[k].manedit = 0;
      }

      if ( total_precip.qc == 'D' )
      {
         gage[k].td = 1;
      }
      else
      {
         gage[k].td = 0;
      }

      if ( total_precip.qc == 'L' )
      {
         gage[k].qc = 2;
      }

      if ( total_precip.qc == 'C' )
      {
         gage[k].qc = 1;
      }

      if ( total_precip.reported_missing == 1 )
      {
         gage[k].reported_missing = 1 ;
      }
      else
      {
         gage[k].reported_missing = 0 ;
      }

      gage[k].is_bad = false;
	   	    
      k++;
   }

   if ( pHourlyPPhead != NULL )
   {
      FreeHourlyPP ( pHourlyPPhead ) ;
      pHourlyPPhead = NULL ;
   }

   if ( pHourlyPChead != NULL )
   {
      FreeHourlyPC ( pHourlyPChead ) ;
      pHourlyPChead = NULL ;
   }
   
   /* process the pseudo gage values */
   
   if (pseudoHead != NULL)
   {  
      pseudoPtr = (PseudoGageVal *)ListFirst(&pseudoHead->list);
      
      while (pseudoPtr)
      { 
	 /* calculate the national HRAP coordinates from lat,lon */
	 
	 xlt = pseudoPtr->lat;
	 xln = pseudoPtr->lon;
	 
	 hrap_point = LatLongToHrapMpe (xlt, xln);
	 xhr = hrap_point.x;
	 yhr = hrap_point.y;
	 
	 
	 /* make sure the gage is within the area. if so load the info */
	 
         /* Pseudo gages are not subject to the SCC and MSC 
            QC checks. */
         gage[k].qc = 3 ;

         strcpy(gage[k].id, pseudoPtr->pseudo_gage_id);
	    
	 gage[k].gval   = pseudoPtr->gage_value;

         if ( gage[k].gval == -999. )
         {
            gage[k].reported_missing = 1;
         }
         else
         {
            /* Convert the pseudo gage value to inches. */
            gage[k].gval = gage[k].gval / 25.4 ;
         }
	    
         gage[k].hrap.x = xhr;
	 gage[k].hrap.y = yhr;
	    
	 gage[k].hrap_loc.x = gage[k].hrap.x - ( float ) XOR;
	 gage[k].hrap_loc.y = gage[k].hrap.y - ( float ) YOR;
	    
	 if (strcmp(pseudoPtr->man_edited, "T") == 0)
	    gage[k].manedit = 1;
	 else
	    gage[k].manedit = 0;
	    
	 gage[k].td  = 0;
	    
	 k++;

         pseudoPtr = (PseudoGageVal *) ListNext(&pseudoPtr->node); 
      }      

      if ( pseudoHead != NULL )
      {
          FreePseudoGageVal ( pseudoHead ) ;
          pseudoHead = NULL ;
      }
   }
   
   
   /* assign the global variable */
   
   ngages = k;
   
   
   /* fill in remaining fields of gage structure and 
      convert the units of the value */
   for (k = 0; k < ngages; k++)
   {    
      if ( gage[k].gval == MISSING_PRECIP )
      {
         /* If the value is missing, set the gage value to
            the MPE missing representation. */
         gage [ k ].gval = -999. ;
      }
 
      strcpy(gage[k].rid, "ZZZ");
      gage[k].xmrg_val  = -999.;
      gage[k].mval      =  -999.;   
      gage[k].rval      =  -999.;   
      strcpy(gage[k].edit, "");
      gage[k].bval      =  -999.; 
      gage[k].loc_val   =  -999.; 
      gage[k].gage_only =  -999.;       
      gage[k].use_in_p3 = 1;
   }


/*
   // Check the gages to determine if any of them are within a "Set"
   //   polygon. If any are, then check their values to determine if
    //  the value of any of these is smaller than the set polygon value.
    //  If this is the case, then flag the gage not to be displayed in
    //  the p3lMosaic. 

   // Load the Polygon List for the avgRmosaic product.
   get_polygons ( display_avgrMosaic, & PolyList, date_st3.cdate );

   pPolyNode = ( rubber_poly_data * ) ListFirst ( & PolyList ); 

   if ( pPolyNode == NULL )
   {
      flogMessage ( stdout, "***No polygons found for %s.\n", date_st3.cdate ); 
   }

   while ( pPolyNode != NULL )
   {

      if ( pPolyNode->set_flag == True )
      {
         for ( k = 0; k < ngages; ++k )
         {
            // Do a rough check of the gage and polygon position.
             //  This will determine whether or not a finer check is
             //  required. 
            if ( gage [ k ].gval <= pPolyNode->draw_precip_value )
            {
               flogMessage ( stdout, "gval: %5.2f poly: %5.2f\n",
                                 gage [ k ].gval, 
                                 pPolyNode->draw_precip_value );
               if ( ( gage [ k ].hrap_loc.x <= pPolyNode->maxx ) &&
                    ( gage [ k ].hrap_loc.x >= pPolyNode->minx ) &&
                    ( gage [ k ].hrap_loc.y <= pPolyNode->maxy ) &&
                    ( gage [ k ].hrap_loc.y >= pPolyNode->miny ) )
               {
                  // Determine if this gage is inside or outside the
                  //   edit polygon. 
                  z = InOutPoly ( gage [ k ].hrap_loc.x, 
                                  gage [ k ].hrap_loc.y,
                                  pPolyNode );

                  if ( z == 1 )
                  {
                     gage [ k ].use_in_p3 = 0;
                  }
               }
            }
         }
      }

      pPolyNode = ( rubber_poly_data * ) ListNext ( & pPolyNode->node );
   }
*/   
   
   /* log messages */
  logMessage("total in area=%d (%d proc, %d pseudo)\n" ,
	  ngages , ngages - pseudo_count , pseudo_count ) ;
   
   /* Get the ending time value. */
   gettimeofday ( & etime , NULL ) ;

   elapsed_time = ( double ) ( etime.tv_sec - btime.tv_sec ) +
                  ( double ) ( etime.tv_usec - btime.tv_usec ) / 1000000. ;

  logMessage ( "reading precip gage done = %5.3f\n" , elapsed_time ) ;
   return;   
}


/***************************************************************************
  FUNCTION NAME:   ReadRadarData                                        
  FUNCTION:        read calculated bias, number of gages reporting from   
                   the RWRadarResult table; malloc space for radar related arrays                 
   
   Function type:  void
      
   Functions called:
   read_rresult
   
   Variable definitions:
   iflign = ignore radar flag 
   
   iflarad = radar availability flag array read from RWRadarResult table
   = 0 -- field available (some values > 0.0)
   = 1 -- field not available
   = 2 -- field available (all values = 0.0)
   
   datafile[n][0] = NOT USED                                     
   
   datafile[n][1] = 0/1 flag for gage availabilty 
   (num_gages field in rwradarresult record)
   = 0 -- >0 gages available
   = 1 -- no gages available
   
   datafile[n][2] = 0/1 flag for radar data availabilty
   = 0 -- radar data available
   = 1 --   "     "  not available
   where n = radar number in list
   
   datafile[n][2] and datafile[n][1] are also used to determine the color 
   of the radar ring
   
   stage1i array contains raw radar field
   stage1u array contains unbiased radar field (= raw radar * bias)
   
   datetime_radar_prod array contains datetimes of the radar products 
   for locating records in the DPARadar and DPAAdapt tables - 
   this is needed because many radar products do not have time stamps 
   exactly at the top-of-the-hour
   
***************************************************************************/
   
void ReadRadarData ( )
{
   int          i, j;
   static int   nmalloc = 0;
   
   
   /* if not malloced */
   
   if (nmalloc == 0)
   {
      stage1i =  (short ***) malloc(NRADARS*sizeof(short **));
      stage1u =  (short ***) malloc(NRADARS*sizeof(short **));
      
      siibiasu = (float *) malloc(NRADARS*sizeof(float));
      iflarad  = (short *) malloc(NRADARS*sizeof(short));
      iflign   = (short *) malloc(NRADARS*sizeof(short));
      
      for (i = 0; i<NRADARS; i++)
      {
	 stage1i[i] = (short **) malloc(131*sizeof(short *));
	 stage1u[i] = (short **) malloc(131*sizeof(short *));
	 
	 for (j=0; j<131; j++)
	 {
	    stage1i[i][j] = (short *) malloc(131*sizeof(short ));
	    stage1u[i][j] = (short *) malloc(131*sizeof(short ));
	    
	    if(!stage1i[i][j] || !stage1u[i][j])
	    {
	      logMessage("malloc failed for radar arrays-- program stopping\n");
	       exit(0);
	    }
	 }
      }
      
      datafile = (int **)malloc(NRADARS*sizeof(int *));
      for (i = 0; i < NRADARS; i++)
	 datafile[i] = (int *) malloc(3*sizeof(int));
      
      datetime_radar_prod = (char **)malloc(NRADARS*sizeof(char *));
      
      for (i = 0; i < NRADARS; i++)
      {
	 datetime_radar_prod[i] = (char *) malloc(22*sizeof(char));
	 memset ( datetime_radar_prod [ i ] , '\0' , 22 ) ;
      }
      
      nmalloc++;
   }
   
   
   /*  read  bias used, radar availability flag, number of gages
      from the RWRadarResult table                               */
   
   read_rresult(datetime);
   
   return;  

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/read_precip_data.c,v $";
 static char rcs_id2[] = "$Id: read_precip_data.c,v 1.32 2007/10/16 13:36:30 varmar Exp $";}
/*  ===================================================  */

}

