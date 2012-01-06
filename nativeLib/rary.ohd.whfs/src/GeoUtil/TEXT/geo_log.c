
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"

#include "geo_header_file.h"

#include "GeneralUtil.h"

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

void log_geomsg(char *msgstr,
		FILE *log_file)
{
   if (log_file != NULL)
      fprintf(log_file, "%s", msgstr);
   
   else
      fprintf(stderr, "%s", msgstr);
   
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
/********************************************************

   Open the geo file specified.
   The rank value is needed for geoline data. 
   
********************************************************/
void open_geolog(int	geotable,
		 char	*geotype,
		 int    rank,
		 FILE	**log_file)
{
   int		type_index = 0 ;
   char 	filename[320];
   time_t	runtime;

  int          len=0, rlen=0, istatus=0;
  char         rdir_path[128];

   
   
   /* read geo files. the filenames must match those
      specified for the geomanage interface in hydrobase */
   
   char geoarea_import_logfiles[5][30] =
      {"zones_import.log", "counties_import.log", "states_import.log",
      "basins_import.log", "resvrs_import.log"};
   
   char geoline_import_logfiles[4][30] =
      {"rivers_import.log", "streams_import.log", "hiways_import.log",
      "roads_import.log"};
   
   
   /* find an index to the type of data to help determine the log
      file name. a match should always result as a check was 
      performed earlier */
   
   if (geotable == GEOAREA)
   {
      if (strcmp(geotype, "ZONE") == 0)
	 type_index = 0;
      else if (strcmp(geotype, "COUNTY") == 0)
	 type_index = 1;      
      else if (strcmp(geotype, "STATE")  == 0)
	 type_index = 2;
      else if (strcmp(geotype, "BASIN")  == 0)
	 type_index = 3;
      else if (strcmp(geotype, "RESRVR") == 0)
	 type_index = 4;
   }
   
   else
   {
      if (strcmp(geotype, "STREAM") == 0)
      {
         if (rank == 1)
	   type_index = 0;
	 else
	   type_index = 1;
      }
      else if (strcmp(geotype, "ROAD") == 0)
      {
         if (rank == 1)
	   type_index = 2;
	 else
	   type_index = 3;
      }
   }
   
   
   /* get the appropriate directory.
      allow this to be optional */
   
   len = strlen("whfs_util_log_dir");
   istatus = get_apps_defaults("whfs_util_log_dir", &len, rdir_path, &rlen);

   if (istatus != 0)
   {
      fprintf(stderr, "wwhfs_util_log_dir undefined.\n");
      
      if (geotable == GEOAREA)
	 strcpy(filename, geoarea_import_logfiles[type_index]);
      else
	 strcpy(filename, geoline_import_logfiles[type_index]);
   }
   
   else
   {
      if (geotable == GEOAREA)
	 sprintf(filename, "%s/%s",  
		 rdir_path, geoarea_import_logfiles[type_index]);
      else
	 sprintf(filename, "%s/%s", 
		 rdir_path, geoline_import_logfiles[type_index]);
   }
  
   
   /* open the file */
   
   *log_file = fopen(filename, "w");
   
   if(*log_file == NULL)
   {
      fprintf(stderr, "Error opening log file: %s\n", filename);
      fprintf(stderr, "Logging data to stderr...\n");
      return;
   }
   
   else 
   {
      fprintf(stderr, "Log to file: %s\n", filename);
      
      time(&runtime);
      fprintf(*log_file, "Log file created at %s", 
	      asctime(gmtime(&runtime)));
   }
   
   return;
}
