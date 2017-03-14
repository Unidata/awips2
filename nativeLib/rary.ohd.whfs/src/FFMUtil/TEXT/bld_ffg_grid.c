#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "FfmUtils.h"

#include "time_convert.h"
#include "time_defs.h"
#include "convert_hrap.h"

#include "ToolDefs.h"

#include "ContingencyValue.h"
#include "FcstPrecip.h"
#include "GeoArea.h"
#include "RadarLoc.h"
#include "LineSegs.h"

#include "DbmsDefs.h"

/*******************************************************************
   bld_ffg_grid.c	
   
   PURPOSE
   Builds a consistent set of ffg data in gridded format using
   a combination of ffg data that covers the grid area.
   FFG data is used from any available source, using an
   ordered precedence of FFG data, where first any basin FFG 
   is used, then any county ffg, then any zone ffg.
   The grid is assembled using ffg data since a specified
   ending time for the specified boundary type and duration.
      
   *****************************************************************/

float * bld_ffg_grid(char		*radid,
		     char		*areatypes_usage,
		     int		ffg_hours,
		     time_t		sincetime,
		     int		*ffg_status)
{
   char		where[260];
   time_t 	durtime;
   char		ansi_sincetime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status, i;   
   int		shefdur;
   ContingencyValue	*contingPtr, *cPtr;
   GeoArea		*geoareaPtr, *gaPtr;
   double 	south_row, west_col;
   int		numbins_230;
   int		itype, numtypes;
   float	*ffg_grid;
   
   
   /* initialize the return status */
   
   *ffg_status = -1;   
   
   
   /* get the radar coords info */
   
   status = find_radar_coords(radid, &south_row, &west_col, &numbins_230);
   if (status < 0)
      return(NULL);
   
   printf("bld_ffg_grid; for %s: Srow = %.3f, Wcol = %.3f, numbins230= %d\n",
	  radid, south_row, west_col, numbins_230);
   
   
   /* convert the times for the query */
   
   status = timet_to_yearsec_ansi(sincetime, ansi_sincetime);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting sincetime in bld_ffg_grid(); status = %d\n",
	      status);
      return(NULL);
   }
   
   durtime = SECONDS_PER_HOUR * ffg_hours;
   status = timet_to_shefdur(durtime, &shefdur);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting duration in bld_ffg_grid(); status = %d\n",
	      status);
      return(NULL);
   }
   
   
   /* initialize the output array to all missing */
   
   ffg_grid = (float *)
      malloc(sizeof(float) * (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS));
   if (ffg_grid == NULL)
   {
      fprintf(stderr, "Error in malloc of ffg_grid in bld_ffg_grid().\n");
      return(NULL);
   }
   
   for (i = 0; i < LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS; i++)
      ffg_grid[i] = MISSING;
   
   
   /* determine the number of area types to use; 
      defined by the length of the string !!! */
   
   numtypes = strlen(areatypes_usage);
   
   
   /* get all the FFG data at once. then, when needing the latest
      data for each area, sort through this list.  this approach
      is used for performance purposes to avoid repetitive queries 
      for each area's ffg data.  */
   
   sprintf(where, " WHERE pe = 'PP' AND ts = 'CF' AND "
	   " validtime > '%s' AND dur = %d ORDER BY validtime DESC",
	   ansi_sincetime, shefdur);
   
   contingPtr = GetContingencyValue(where);
   
   if (contingPtr == (ContingencyValue *)(NULL))
   {
      free(ffg_grid);
      ffg_grid = NULL;	 
      return(NULL);
   }
   
   else
      *ffg_status = 1;
	       
         
   /* loop on the possible area types that are considered
      for the grid representation of the data */
   
   for (itype = 0; itype < numtypes; itype++)
   {
      
      /* use the specified precedence for the area types to use
	 when assembling the grid of FFG data. then get the list
	 of areas that match the type */
      
      if (areatypes_usage[itype] == 'B')
	 sprintf(where, " WHERE boundary_type = 'BASIN' ");
      else if (areatypes_usage[itype] == 'C')
	 sprintf(where, " WHERE boundary_type = 'COUNTY' ");
      else if (areatypes_usage[itype] == 'Z')
	 sprintf(where, " WHERE boundary_type = 'ZONE' ");
      else
	 sprintf(where, " WHERE boundary_type = 'invalid' ");
      
      
      /* get the areas being considered */
      
      if ( ( geoareaPtr = GetGeoArea(where) ) != NULL )
      {
	 /* loop on each the areas */
	 
	 gaPtr = (GeoArea *) ListFirst(&geoareaPtr->list);
	 
	 while(gaPtr)
	 {      	    	    
	    
	    /* initialize */
	    
	    cPtr = (ContingencyValue *) ListFirst(&contingPtr->list);	    
	    
	    /* loop on the entries in the list */
	    
	    while(cPtr)
	    {
	       
	       /* check if the station id matches. the data are already
		  sorted by time so the first entry per station is
		  its most recent */
	       
	       if (strcmp(cPtr->lid, gaPtr->area_id) == 0)
	       {		  		  
		  load_data_gridcells(south_row, west_col,
				      cPtr->lid, cPtr->value,
				      ffg_grid);			  
		  break;
	       }
	       
	       
	       cPtr = (ContingencyValue *) ListNext(&cPtr->node);
	    }
	    
	    
	    /* process the next area in the list */
	    
	    gaPtr = (GeoArea *) ListNext(&gaPtr->node);
	    
	 }  /* end of for loop on given area */
	 
	 
	 if (geoareaPtr) FreeGeoArea(geoareaPtr);
	 
      }
      
   }        /* end of loop on the type of area */
   
   
   /* free the list of FFG values */
   
   FreeContingencyValue(contingPtr);
      
   
   return(ffg_grid);
}


/*******************************************************************
   bld_qpf_grid.c	
   
   PURPOSE
   Builds a consistent set of qpf data in gridded format using
   a combination of qpf data that covers the grid area.
   It is limited to using QPF data from basin-based data only!
   The grid is assembled using qpf data since a specified
   ending time for the specified boundary type and duration.
      
   *****************************************************************/

float * bld_qpf_grid(char		*radid,
		     int		qpf_hours,
		     time_t		validtime,
		     int		*qpf_status)
{
   char		where[260];
   time_t 	durtime;
   char		ansi_validtime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status, i;   
   int		shefdur;
   FcstPrecip	*fcstprecipPtr, *fpPtr;
   GeoArea	*geoareaPtr, *gaPtr;
   double 	south_row, west_col;
   int		numbins_230;
   float	*qpf_grid;
   
   
   /* initialize the return status */
   
   *qpf_status = -1;   
   
   
   /* get the radar coords info */
   
   status = find_radar_coords(radid, &south_row, &west_col, &numbins_230);
   if (status < 0)
      return(NULL);
   
   printf("bld_qpf_grid; for %s: Srow = %.3f, Wcol = %.3f, numbins230= %d\n",
	  radid, south_row, west_col, numbins_230);
   
   
   /* convert the times for the query */
   
   status = timet_to_yearsec_ansi(validtime, ansi_validtime);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting validtime in bld_qpf_grid(); status = %d\n",
	      status);
      return(NULL);
   }
   
   durtime = SECONDS_PER_HOUR * qpf_hours;
   status = timet_to_shefdur(durtime, &shefdur);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting duration in bld_qpf_grid(); status = %d\n",
	      status);
      return(NULL);
   }
   
   
   /* initialize the output array to all missing */
   
   qpf_grid = (float *)
      malloc(sizeof(float) * (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS));
   if (qpf_grid == NULL)
   {
      fprintf(stderr, "Error in malloc of qpf_grid in bld_qpf_grid().\n");
      return(NULL);
   }
   
   for (i = 0; i < LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS; i++)
      qpf_grid[i] = MISSING;
   
      
   /* get all the qpf data at once. then, when needing the latest
      data for each area, sort through this list.  this approach
      is used for performance purposes to avoid repetitive queries 
      for each area's qpf data. */
   
   sprintf(where, " WHERE pe = 'PP' AND "
	   " validtime = '%s' AND dur = %d ORDER BY basistime DESC",
	   ansi_validtime, shefdur);
   
   fcstprecipPtr = GetFcstPrecip(where);
   
   if (fcstprecipPtr == (FcstPrecip *)(NULL))
   {
      free(qpf_grid);
      qpf_grid = NULL;	 
      return(NULL);
   }
   
   else
      *qpf_status = 1;
	       
   
   /* use the specified precedence for the area types to use
      when assembling the grid of qpf data. then get the list
      of areas that match the type */
   
   sprintf(where, " WHERE boundary_type = 'BASIN' ");
   
   
   /* get the areas being considered */
   
   if ( ( geoareaPtr = GetGeoArea(where) ) != NULL )
   {
      /* loop on each the areas */
      
      gaPtr = (GeoArea *) ListFirst(&geoareaPtr->list);
      
      while(gaPtr)
      {      	    	    
	 
	 /* initialize */
	 
	 fpPtr = (FcstPrecip *) ListFirst(&fcstprecipPtr->list);	    
	 
	 /* loop on the entries in the list */
	 
	 while(fpPtr)
	 {
	    
	    /* check if the station id matches. the data are already
	       sorted by basis time so the first entry per station is
	       its 'best' */
	    
	    if (strcmp(fpPtr->lid, gaPtr->area_id) == 0)
	    {		  
	       
	       load_data_gridcells(south_row, west_col,
				   fpPtr->lid, fpPtr->value,
				   qpf_grid);	
	       
	       break;
	    }
	    
	    
	    fpPtr = (FcstPrecip *) ListNext(&fpPtr->node);
	 }
	 
	 
	 /* process the next area in the list */
	 
	 gaPtr = (GeoArea *) ListNext(&gaPtr->node);
	 
      }  /* end of for loop on given area */
      
      
      if (geoareaPtr) FreeGeoArea(geoareaPtr);
      
   }
   
   
   /* free the list of qpf values */
   
   FreeFcstPrecip(fcstprecipPtr);
      
   
   return(qpf_grid);
}

/*******************************************************************
   load_data_gridcells()
   
   Loads a data value into the grid cells covered by the value.
   
   The south row, west col values passed in should already 
   be whole numbers even though they are floats.
   
   *****************************************************************/

int load_data_gridcells(double		south_row,
			double		west_col,
			char		*lid,
			double		value,
			float		ffg_grid[])
{
   
   char 	where[120];
   LineSegs	*linesegPtr;
   int		numrows;
   long         *rows = NULL;
   long         *beg_cols = NULL;
   long         *end_cols = NULL;
   int		row, col;
   int		array_index;
   int		irow, jcol;
   int          i;
 
   /* read the HRAP bin coords for the area and
      extract the information from the blob fields. */
      
   sprintf(where, " WHERE area_id = '%s' ", lid);
   
   linesegPtr = GetLineSegs(where);
   if (linesegPtr == NULL)
   {
      fprintf(stderr, "LineSeg information not available for %s\n", lid);
      return(-1);
   }
   
   numrows = ListCount(&linesegPtr->list);
   
   rows = (long *) malloc (sizeof(long) * numrows);
   if(rows == NULL)
   {
      fprintf(stderr, "\nIn 'load_data_gridcells':\n"
             "Could not allocate memory for the rows array\n");
      exit(1);
   }
   beg_cols = (long *) malloc (sizeof(long) * numrows);

   if(beg_cols == NULL)
   {
      fprintf(stderr, "\nIn 'load_data_gridcells':\n"
             "Could not allocate memory for the beg_cols array\n");
      exit(1);
   }
   
   end_cols = (long *) malloc (sizeof(long) * numrows);

   if(end_cols == NULL)
   {
      fprintf(stderr, "\nIn 'load_data_gridcells':\n"
             "Could not allocate memory for the end_cols array\n");
      exit(1);
   }

   for (i = 0; i < numrows; i++)
   {
      rows[i]      = linesegPtr->hrap_row;
      beg_cols[i]  = linesegPtr->hrap_beg_col;
      end_cols[i]  = linesegPtr->hrap_end_col;
   }

   /* at this point, we have the coordinate information for the
      source area and the desination grid.
      loop on the number of rows for this area */
      
   for (irow = 0; irow < numrows; irow++)
   {
      /* loop on the number of columns in each row */

      
      for (jcol = beg_cols[irow]; jcol <= end_cols[irow]; jcol++)
      {
         /* adjust the row, column to match the local grid */
	 
         row = rows[irow] - south_row;
         col = jcol - west_col;
	 
	 
	 if (row >= 0 && row < LOCAL_HRAP_ROWS &&
	     col >= 0 && col < LOCAL_HRAP_COLS)
	 {
	    array_index = (row * LOCAL_HRAP_ROWS) + col;
	   	    
	    
	    /* if the grid cell does not have a value already, load in 
	       the value for this area */
	    
	    if (ffg_grid[array_index] == MISSING)
	    {
	       ffg_grid[array_index] = value;
	       
	       /* printf("lid, row,col; local row,col; value, index ="
		  " %s %d %d %d %d %f %d\n", 
			 lid, rows[irow], jcol, row, col, value, array_index);
			 */
	       	       
	    }
	 }
	 
	    
      }
   }
   
   FreeLineSegs(linesegPtr);
   free(rows);
   free(beg_cols);
   free(end_cols);

   return(0);
}
