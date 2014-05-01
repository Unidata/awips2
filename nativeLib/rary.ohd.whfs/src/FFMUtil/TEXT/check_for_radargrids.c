#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "time_convert.h"
#include "time_defs.h"

#include "FfmUtils.h"
#include "GeneralUtil.h"
#include "ArealProductSettings.h"
#include "read_radargrids.h"

#include "DPARadar.h"
#include "DbmsDefs.h"

#include "convert_hrap.h"  /* need size of Stage1,2 HRAP grids */


/*******************************************************************
   check_for_radargrids.c	
   
   PURPOSE
   Checks for the existence of grids of the specified type
   for the specified radar for the specified duration.
   
   Return integer count indicating number of hours with valid data.
   
   *****************************************************************/

int check_for_radargrids(char 		*radid,
			 PrecipType	precipType,
			 time_t		endtime,
			 int		duration)
{
   char		where[160];
   time_t 	starttime, gridtime;
   char		ansitime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status;
   int		found_status, file_should_exist;
   DPARadar	*dpaHead;
   int		num_hours_found;   
   char		filename[FILE_NAME_LEN];
   int		numhrs;
   
   
   /* initialize variables */
      
   num_hours_found = 0;
   
   
   /* loop on the time range for which the value is determined */
   
   numhrs = duration / SECONDS_PER_HOUR;
   starttime = endtime - ((numhrs - 1) * SECONDS_PER_HOUR);
   
   for (gridtime = starttime; gridtime <= endtime; gridtime += SECONDS_PER_HOUR)
   {      	 	                  
      /* found_status: 1 = complete success;
	 -1 db record not found (includes not calculated case!);
	 -2 unable to open file; -3 error reading file;
	 -4 directory not known */
      
      found_status = 1;
      file_should_exist = 0;
      
      
      /* convert the time */
      
      status = timet_to_yearsec_ansi(gridtime, ansitime);     
      
      
      /* get the database record with the gridded data */
   
      /* the stage1/dparadar supplmess flag values are 0= no precipitation
	 detected; 1 = bad rate scan; 2 = not enough data in hour;
	 3 = disk error; 4 = precipitation */
      
      if (precipType == STAGE1_PRECIP)
      {
	 sprintf(where,
		 " WHERE radid = '%s' AND obstime = '%s' " 
		 " AND supplmess IN (0,4) ", radid, ansitime);
 	 dpaHead = GetDPARadar(where);
	 
	 if (dpaHead == (DPARadar *)NULL)
	    found_status = -1;
	 else
	 {
	    if (dpaHead->supplmess == 4 && dpaHead->maxvald > 0.0)
	    {
	       file_should_exist = 1;
	       strcpy(filename, dpaHead->grid_filename);
	    }
	    FreeDPARadar(dpaHead);
	 }
      }
      
      
      else
	 return(-1);
      
      
      /* now check that the data file associated with the record 
	 exists and contains valid data */
      
      if (found_status > 0)
      {	 
	 if (file_should_exist)
	 {
	    found_status = check_grid_file(filename, precipType);
	    if (found_status > 0) num_hours_found++;
	 }
	 
	 else
	    num_hours_found++;
      }
            
      
   }  /* end of loop on time period */
   
   
   
   /* return the number of hours with valid data */
   
   return(num_hours_found);
}


/*******************************************************************
   check_grid_file()
   
   PURPOSE
   Checks that the grid file referenced exists and is valid.
   
   *****************************************************************/
int check_grid_file(char 	*filename,
		    PrecipType	precipType)
{
   short int    shortint_array[LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS];
   float	float_array[LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS];
   
   FILE 	*infile;
   char 	pathname[260];
   int		numread;
   int		read_status;

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];

      
   
   /* assemble the proper pathname and try to open the file */   
   
   if (precipType == STAGE1_PRECIP)		     
   {
      	len = strlen("dpa_grid_dir");
 	istatus = get_apps_defaults("dpa_grid_dir", &len, rdir_path, &rlen);
   }
   else      
   {
	len = strlen("pproc_s2_grid_dir");
	istatus = get_apps_defaults("pproc_s2_grid_dir", &len, rdir_path, &rlen);

   }
   
   
   if (istatus == 0)
   {
      read_status = 1;
      
      sprintf(pathname, "%s/%s", rdir_path, filename);
      
      infile = fopen(pathname, "r");
      if (! infile)
      {
	 fprintf(stderr, "ERROR - unable to open file %s\n", pathname);
	 read_status = -2;
      }
      
      
      /* now read the file containing the grid arrays.
	 if successful, then all is well */
      
      else
      {		  
	 fseek(infile, 4, SEEK_CUR);
	 
	 if (precipType == STAGE1_PRECIP)		     
	    numread = fread(float_array, sizeof(float),
			    LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS,
			    infile);
	 else
	    numread = fread(shortint_array, sizeof(short int),
			    LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS,
			    infile);
	 
	 if (numread != LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS)
	 {
	    fprintf(stderr, "ERROR - incorrect size of file %s\n", pathname);
	    read_status = -3;
	 }
	 
	 fclose(infile);
      }
   }
   
   else
   {
      fprintf(stderr, "ERROR - xxx_GRID_DIR undefined.\n");
      read_status = -1;
   }
   
   return(read_status);
}
