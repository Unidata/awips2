/************************************************************
   Write the HRAP bin information for a
   specified area to the Informix database.  
   
   The required fields provide information on the location
   identifier (handbook five id), the number of points, the
   grid factor, the row number, the row beginning columns, and
   the row ending columns.
   
   Returns zero (0) if successful, otherwise less than zero (0).
   All parameters serve as input provided by the calling 
   function.
   
   
   ************************************************************/

#include <stdio.h>
#include <string.h>
#include <memory.h>

#include "write_lineseg.h"


/************************************************************

 ************************************************************/

void write_lineseg(const char *area_id, const HrapBinList *binList) 
{
   char		where[100] ;
   int          i ;	
   int		count ;
   int		status ;
   LineSegs	linesegs ;

   /* setup area_id, rows variables */
   
   strcpy(linesegs.area_id, area_id);
   
   linesegs.area = binList->area;
   
   for(i = 0; i <  binList->numRows; i++)
   {   
      linesegs.hrap_row = binList->rows[i];
      linesegs.hrap_beg_col = binList->beginCols[i];
      linesegs.hrap_end_col = binList->endCols[i];
   
   
   /* check if the record exists in the database */
   
  /* sprintf(where, " where area_id = '%s' ", area_id );*/

   sprintf(where, " where area_id = '%s' "
                  " and hrap_row = '%ld' "
                  " and hrap_beg_col = '%ld' ", 
                    area_id,  
                    linesegs.hrap_row ,
                    linesegs.hrap_beg_col ); 
   
   count = recordCount("LineSegs", where);
      
   
   if (count == 0)
   {
      status = PutLineSegs(&linesegs);
      if (status != 0)
	 fprintf(stderr, "Error %d putting into LineSegs for area_id: %s\n",
		 status, area_id);
   }
   
   else
   {
      status = UpdateLineSegs(&linesegs, where);
      if (status != 0)
	 fprintf(stderr, "Error %d updating LineSegs for area_id: %s\n",
		 status, area_id);
   }
   
   }   
   return;
}
