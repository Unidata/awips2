#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "read_radargrids.h"

#include "DbmsDefs.h"
#include "DPARadar.h"

#include "GeneralUtil.h"
#include "GetOS.h"
#include "ReadAndSwap.h"        /* Defined in generalutil. */
#include "time_convert.h"
#include "convert_hrap.h"

/*********************************************************************
   Modification History

   Name               Date                Reason
   Bryon Lawrence     November 16 , 2001  Updated the read_stage1file,
                                          read_stage2file, and read_latestgrid
                                          routines to use the new function
                                          ReadAndSwap. 
   Paul Tilles        Jan 15, 2004        Removed read_stage2grid and
                                          read_stage2file routines

   ********************************************************************/

/*********************************************************************
   read_stage1grid() 
   
   Reads the stage1 grid.
   This functions expects the returned grid array to 
   be malloced by the calling program and assumes the grid
   is a one-dimensional array.
   
   If the function returns a negative value, then an error 
   occurred and no data was read.
   
   ********************************************************************/

int read_stage1grid(char 	*radid,
		    time_t	gridtime,
		    PrecipType	precipType,
		    int		*zero_flag,
		    float 	*bias,
		    float 	*grid_vals)
{
   int		status;
   char		ansitime[ANSI_TIME_LEN + 1];
   DPARadar	*dpaHead;
   char		where[160];
   int		row;
   
   
   /* build the where clause for the radar and 
      observation time specified */
   
   status = timet_to_yearsec_ansi(gridtime, ansitime);
   
   sprintf(where, " WHERE radid = '%s' AND obstime = '%s' ", 
	   radid, ansitime);
   
   
   /* get the database record with the data */   

   dpaHead = GetDPARadar(where);
   
   if (dpaHead == (DPARadar *)NULL)
   {
      printf("Stage 1 grid unavailable for %s %s\n", radid, ansitime);
      return(-1);
   }
   
   
   /* check the supplemental message flag. possible values are:
      0 = not precipitation detected; 1 = bad rate scan
      2 = not enough data in hour; 3 = disk error; 4 = precipitation */
   
   if ((dpaHead->supplmess == 1) || (dpaHead->supplmess == 2) ||
       (dpaHead->supplmess == 3))
   {
      if (dpaHead->supplmess == 1)
      {
	 printf("Stage 1 bad rate scan for %s %s\n", radid, ansitime);
	 return(-2);
      }
      
      if (dpaHead->supplmess == 2)
      {
	 printf("Stage 1 not enough data in scan for %s %s\n", radid, ansitime); 
	 return(-3);
      }
      
      if (dpaHead->supplmess == 3)
      {
	 printf("Stage 1 disk error for %s %s\n", radid, ansitime);
	 return(-4);
      }
   }
   
   
   /* if a zero grid is found, set all values to zero and
      set the indicator value for all zero grid */
   
   if (dpaHead->maxvald == 0.0 || dpaHead->supplmess == 0)
   {      
      for (row = 0; row < (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS); row++)
	 grid_vals[row] = 0.0;
      *zero_flag = 1;
      
      return(0);
   }
   else
      *zero_flag = 0;
      
   
   status = read_stage1file(dpaHead, grid_vals);
   
   
   return(status);
}

   
/*********************************************************************
   read_stage1file() 
   
   Reads the stage1 file.
   
   ********************************************************************/

int read_stage1file(DPARadar	*dpaHead,
		    float 	*grid_vals)
{   
   FILE 	*infile;
   char 	pathname[240];
   int 		numread;
   float	floatarray[LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS];
   int		array_index;
   int		row, col;
   char		ansitime[ANSI_TIME_LEN + 1];
   int		status;

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];
   
   
   /* assemble the proper pathname and try to open the file */

   len = strlen("dpa_grid_dir");
   istatus = get_apps_defaults("dpa_grid_dir", &len, rdir_path, &rlen);

   if ( istatus == 0)
   {
      sprintf(pathname, "%s/%s", rdir_path, dpaHead->grid_filename);
      infile = fopen(pathname, "r");
      if (! infile)
      {
         status = yearsec_dt_to_ansi(dpaHead->obstime, ansitime);
	 fprintf(stderr, "Unable to open file %s; for %s %s\n",
		pathname, dpaHead->radid, ansitime);
	 return(-5);
      }
      
      
      /* now read the file containing the grid arrays */
      
      else
      {
	 fseek(infile, 4, SEEK_CUR);

         /* As of 11/16/01, the call to ReadAndSwap replaces the call to the
            "C" library routine "fread".  This routine automatically performs
            the byte swapping based on whether or not this routine is being
            run on a Linux operating system. */ 
	 numread = ReadAndSwap ( ( void * ) floatarray , sizeof ( float ) ,
			         ( size_t ) LOCAL_HRAP_ROWS*LOCAL_HRAP_COLS ,
			         infile ) ;
	 
	 if ( numread == READ_AND_SWAP_IO_ERROR )
	 {
            status = yearsec_dt_to_ansi(dpaHead->obstime, ansitime);
            fprintf(stderr, "Error reading file for %s %s; numitems read= %d\n",
	           dpaHead->radid, ansitime, numread);	           
	    return -6 ;
	 }
         else if ( numread == READ_AND_SWAP_BAD_VALUE )
         {
            fprintf ( stderr , "In read_stage1file: The call to ReadAndSwap\n"
                               "failed. The size of the data element %d\n"
                               "specified in argument 2 of ReadAndSwap is\n"
                               "not supported.\n" , sizeof ( float ) ) ;  
            return -6 ;
         }
         else if ( numread == READ_AND_SWAP_OS_ERROR )
         {
            fprintf ( stderr , "In read_stage1file: The call to ReadAndSwap\n"
                               "failed.  The type of the operating system\n"
                               "could not be determined.\n" ) ;
            return -6 ;
         }
	 
	 /* assign the data.  convert the data as necessary
	    (from dba to mm then divide by 25.4 to get units in inches) */ 
	 
	 array_index = 0;
	 for (row = 0; row < LOCAL_HRAP_ROWS; row++)
	 {      
	    for (col = 0; col < LOCAL_HRAP_COLS; col++)
	    {	
	       
	       grid_vals[array_index] =
		  pow(10.0, (floatarray[array_index] / 10.0)) / 25.4;
		 
		 
	       /* zero out tiny amounts, resulting from dba logarithmic
		  coding method for raw values */
		     
	       if (grid_vals[array_index] < 0.0001)
	          grid_vals[array_index] = 0.0;
	       
	       
	       /*
	       if (floatarray[array_index] != -99.)
	       {
		  printf("(%d, %d) %f %f|",
			 row, col, floatarray[array_index],
			 grid_vals[array_index]); 
	       }
	       */
	       
	       array_index++;
	    }
	 }
	 
	 fclose(infile);
      }
           
   }
   
   else
   {
      printf("dpa_grid_dir undefined; grid data not found.\n");
      return(-7);
   }
   
   
   return(0);
}
