#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

static int mpe_station_count = 0;
struct latlon_info * pLatLonInfo = NULL;

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

const char * get_station_list_path ( const char * site_id )
{
   static char message [ LOG_MESSAGE_LEN ];

   static char * station_file_token = "mpe_station_list_dir";
   static char * site_name_token = "mpe_site_id";

   static char station_dir [ GAGEQC_FILENAME_LEN + 1] = {'\0'};
   static char station_file_path [ GAGEQC_FILENAME_LEN + 1] = {'\0'};
   static char site_name [ QPEMAPPER_SITE_NAME_LEN  + 1] = {'\0'};
   static int first = 1;
   int len;
   static int len_path;
   int len_site_name;

   /* Empty the station file path. */
   memset ( station_file_path, '\0',  GAGEQC_FILENAME_LEN + 1);

   if ( first == 1 )
   { 
      first = 0;

      /* Build the path of the master station list file. */
      len = strlen ( station_file_token );
      get_apps_defaults ( station_file_token, &len, station_dir,
                          &len_path ); 

      if ( len_path == 0 )
      {
         sprintf ( message, "Token %s is undefined.  Looking for station\n"
                            "list file in the current directory.\n",
                            station_file_token );
      }

      len = strlen ( site_name_token );
      get_apps_defaults ( site_name_token, & len, site_name,
                          &len_site_name );
   
      if ( len_site_name == 0 )
      {
         sprintf ( message, "Token %s is undefined. Cannot locate station\n"
                         "list file.\n", site_name_token );
         logMessage(message);
         return NULL;        
      }

   }

   if ( site_id != NULL )
   {
      /* Build the file path using the user supplied area name. */
      if ( len_path > 0 )
      {
         sprintf ( station_file_path, "%s/%s_station_list", station_dir,
                   site_id );
      }
      else
      {
         strcpy ( station_file_path, site_id);
      }
   }
   else
   {
      /* Build the file path using all tokens. */
      if ( len_path > 0 )
      {
         sprintf ( station_file_path, "%s/%s_station_list", station_dir,
                   site_name );
      }
      else
      {
         strcpy ( station_file_path, site_name);
      }
   }

   return station_file_path;
}
   

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:       Reads the list of stations used by MpeFieldgen.
*                Initially this will only use the PPH gage data.
*                Eventually, when the DailyQC behavior is modified
*                to disagg 6 to 1 hour gage reports, it will also
*                read the PPD data.
*
*                The PPH data are read from the master station list file.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  int *       num_gages            The number of 1 hour precip
*                                           gages read from the station
*                                           list file. 
*
* RETURNS:
*   DATA TYPE                               DESCRIPTION
*   pLatLonoInfo *                          A dynamically created array
*                                           containing location information
*                                           for each gage read from the station
*                                           list.
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
struct latlon_info * read_mpe_station_list ( int * num_gages )
{
   char kbuf[MAX_STATION_RECORD_LEN + 1];
   char message[LOG_MESSAGE_LEN +1];
   char *p = NULL;
   char pedtsep [ PEDTSEP_LEN + 1 ];
   const char * station_file_path = NULL;

   FILE *fp = NULL;


   int i;
   int ier;
   int item_count;
   int index;
   int num_records;
   int record_num = 1;

   * num_gages = 0;

   /* Initialize the message string. */
   memset ( message ,'\0', LOG_MESSAGE_LEN );

   /* Is this the first time this routine has been called?
      Only read this station list once. */
   if ( pLatLonInfo != NULL )
   {
      return pLatLonInfo;
   }

   /* Retrieve the path of the station. */
   station_file_path = get_station_list_path ( NULL );

   if ( * station_file_path == '\0' )
   {
      sprintf ( message, "No path or name defined for the station list.\n" );
      logMessage ( message );
      return NULL;
   }

   /* Open the station list file for reading. */
   fp = fopen ( station_file_path, "r" );

   if ( fp == NULL )
   {
      sprintf(message,"Could not open file: %s\n", station_file_path);
      logMessage(message);
      return NULL;
   }
   else
   {	
      sprintf(message,"Opened file: %s\n", station_file_path);
      logMessage(message);
   }

   /* Read the number of PPH records in the file. */
   p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

   if ( p == NULL )
   {
      sprintf ( message, "Reached EOF while reading first record of\n"
                "file %s\n", station_file_path );
      logMessage ( message );
      fclose ( fp );
      fp = NULL;
      return NULL;
   }
   
   ier = sscanf ( kbuf, "%d", & num_records );

   /* One item is expected. */
   if ( ier != 1 )
   {
      sprintf ( message, "Could not read the number of 1 hour stations from\n"
                         "%s record %d\n", station_file_path, record_num );
      logMessage ( message );
      fclose ( fp );
      fp = NULL;
      return NULL;
   }

   /* Allocate memory for the array of station locations. */ 
   pLatLonInfo = ( struct latlon_info * ) malloc ( num_records *
                                          sizeof ( struct latlon_info ) );

   if ( pLatLonInfo == NULL )
   {
      sprintf ( message, "Could not allocate memory for %d elements in "
                         "the LatLonInfo array.\n", num_records );
      logMessage ( message );
      return NULL;
   }
   
   /* Read the PPH station list. This is the list of 1 hour precip gages. */
   index = 0;

   for ( i = 0; i < num_records; ++i ) 
   {
      /* Read a record from the file. Store its contents in the lat/lon
         info structure. */
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
      
      if ( p == NULL )
      {
         sprintf ( message, "Reached EOF after record %d of\n"
                   "file %s\n", record_num, station_file_path );
         logMessage ( message );
         break;
      }

      ++record_num;

      item_count = sscanf ( kbuf, "%s%s%lf%lf", pLatLonInfo[index].lid,
                            pedtsep, & pLatLonInfo[index].lat, 
                            & pLatLonInfo[index].lon );

      if ( item_count == 4 )
      {
         ++ index ;
      }
      else
      {
         sprintf ( message, "Record %d in file %s is incomplete.  Record\n"
                            "ignored.\n", record_num, station_file_path );
      }
      
      
   }

   mpe_station_count = index;

   /* Close the station list file. */	 
   fclose(fp);
   fp = NULL;

   * num_gages = mpe_station_count;
   return pLatLonInfo;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

