/*

       File:                       get_loc_latlon.c
       Date of last revision:      05/21/2003
       Author:                     Gautam Sood

       Purpose:                    This file contains the function that
                                   retrieves the lat/lon values for a
                                   specified lid via a binary search.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "empe_fieldgen.h"
#include "get_loc_latlon.h"

struct latlon_info
{
   char lid [ LOC_ID_LEN + 1];
   double lat;
   double lon;
};

static int first = 1;
static int station_count = 0;
static struct latlon_info * pLatLonInfo = NULL;

/**********************************************************************
   read_station_file ( )

   Read the MPE station file.  This file contains the lid, latitude,
   and longitude of each station defined in the Location table.
   The first record in this file is the number of stations in the file.

 ********************************************************************/
static void  read_station_file ( )
{
   static const char * fname = "mpe_gage_locations";
   char station_dir [PATH_LEN] = {'\0'};
   char station_file_path [ PATH_LEN + FNAME_LEN ] = {'\0'};
   FILE * fp = NULL;
   int i;
   int item_count;
   int length;
   int num_records;
   static const char * rfcwide_gageloc_dir_token = "rfcwide_gageloc_dir"; 

   hpe_fieldgen_getAppsDefaults ( rfcwide_gageloc_dir_token, station_dir ); 

   length = strlen ( station_dir );

   if ( length == 0 )
   {
      sprintf ( message, "Token %s is not defined.  Application will look "
                         "for gage location file in current working "
                         "directory.\n", rfcwide_gageloc_dir_token );
      hpe_fieldgen_printMessage( message );
      strcpy ( station_file_path, fname );
   }
   else
   {
      sprintf ( station_file_path, "%s/%s", station_dir, fname);
   }

   /* Write a log message indicating that the gage file is about to be
      read. */
   hpe_fieldgen_getCurrentTime ( currTime );
   sprintf ( message, "%s = time begin read gage location file.\n"
                      "gage location file=%s", currTime, station_file_path );
   hpe_fieldgen_printMessage( message );

   /* Attempt to open the station data file. */
   fp = fopen ( station_file_path, "r" );
 
   if ( fp == NULL )
   {
      /* Could not open the station file.*/
      sprintf ( message, "Could not open file %s. "
                         "Create gage location file and rerun MPE Fieldgen.\n"
                         "\tProgram exit.", station_file_path );
      shutdown( message );
   }

   /* Read the first record from the station data file. */
   item_count = fscanf ( fp, "%d", & num_records );

   if ( item_count == EOF ||  item_count != 1 )
   {
      /* Could not read the record count from the first record of the
         station file. */
      sprintf ( message, "Could read station count from first record of\n"
                         "file %s. Create gage location file and\n"
                         "rerun MPE Fieldgen.\n\tProgram exit.",
                         station_file_path );
      shutdown( message );
   }

   /* Allocate the station data array to contain enough
      elements to contain the station data. */
   pLatLonInfo = (struct latlon_info * ) malloc ( num_records *
                                         sizeof ( struct latlon_info ) );

   if ( pLatLonInfo == NULL )
   {
      sprintf ( message, "Could not allocate memory for the LatLonInfo "
                         "array in read_station_file function.\n"
                         "\tProgram exit." );
      shutdown( message );
   }

   /* Read each record from the station file, storing its contents 
      into the corresponding element in the station data array. */
   for ( i = 0; ( i < num_records ) && ( !feof ( fp ) ); ++i )
   {
      item_count = fscanf ( fp, "%s%lf%lf",  pLatLonInfo[i].lid, 
                                          & pLatLonInfo[i].lat,
                                          & pLatLonInfo[i].lon );   

      if ( item_count != 3 )
      {
         sprintf ( message, "Could not read lid, lat, and lon from "
                            "record %d of file %s.  File corrupt.\n"
                "\tProgram exit.", i, station_file_path );
         shutdown( message );
      }
   }

   if ( i < num_records )
   {
      sprintf ( message, "File %s is incomplete. The record count "
                         "in the first row of the file is %d.  The number\n"
                         "of records read in are %d.\n"
                         "\tProgram exit.", station_file_path,  num_records, 
                         i );
      shutdown( message );
   }

   station_count = num_records;

   /* Close the station file. */
   fclose ( fp );
   fp = NULL;

   /* Write a log message indicating that the gage file read is complete. */
   hpe_fieldgen_getCurrentTime ( currTime );
   sprintf ( message, "%s = time end read gage location file, %d gages read.", 
             currTime, num_records );
   hpe_fieldgen_printMessage( message );

   return ;
}

/**********************************************************************
   compare_latlon_id()

   Compare function used for the binary search of the id list
   done when checking if the location is a valid one.

   ********************************************************************/
static int compare_latlon_id ( void * search_value,
                               void * array_value )
{
   /* declare and define the values for two variables mapped
      against the input arguments */
   char * pSearchValue = ( char * ) search_value ;
   struct latlon_info * pArrayValue = ( struct latlon_info * ) array_value ;

   /* return the result of the simple string comparison */
   return ( strcmp ( pSearchValue , pArrayValue->lid) ) ;
}

/**********************************************************************
   get_loc_latlon()

  get location's lat and lon.

   ********************************************************************/

int get_mpe_loc_latlon ( char        * lid ,
                         double      * dlat ,
                         double      * dlon )
{
   struct latlon_info * pStationInfo = NULL ;

   /* initialize */
   *dlat = 39.0;
   *dlon = 77.0;

   /* the first time, read the station information from the 
      station file into the station data array. */
   if ( first )
   {
      first = 0;
      read_station_file ( );
   }

   /* get the info for the matching identifier, if there is a match. */
   if ( station_count > 0 )
   {
      pStationInfo = (struct latlon_info *)
         binary_search ( pLatLonInfo, lid, station_count,
                         sizeof ( struct latlon_info), compare_latlon_id );
   }


   /* load in the info */

   if ( pStationInfo != NULL )
   {
      *dlat = pStationInfo->lat;
      *dlon = pStationInfo->lon;
      return(LATLONFOUND);
   }
   else
   {
/*
      sprintf(message, "No lat lon info found for '%s'\n", lid);
      printMessage( message );
*/      
      return(LATLONNOTFOUND);
   }
}

/**********************************************************************
    get_mpe_loc_latlon_list()

    get a whole list of location's lat and lon.

   ********************************************************************/

int get_mpe_loc_latlon_list ( int    * arraySize ,
                              double * dlat ,
                              double * dlon )
{
    int i ;
    *arraySize = 0 ; 

    /* the first time, read the station information from the 
    station file into the station data array. */
    if ( first )
    {
        first = 0;
        read_station_file ( );
    }


    /* load in the info into the lat/lon array */
    if(station_count > 0)
    {
        for(i=0; i < station_count; i++)
        {
            dlat[i] = pLatLonInfo[i].lat ;
            dlon[i] = pLatLonInfo[i].lon ;
        }

        *arraySize = station_count ; 

        return(0);
    }
    else
    {
        return(-1);
    }

}

/**********************************************************************
   free_mpe_latlon_list

   Used to free memory used by latlon_id_list

   ********************************************************************/

void free_mpe_latlon_info ( )
{
   if ( pLatLonInfo != NULL )
   {
      free ( pLatLonInfo ) ;
      pLatLonInfo = NULL ;
      first = 1 ;
   }

}
