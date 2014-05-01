#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "FfmUtils.h"
#include "ToolDefs.h"

/**************************************************************************/

void print_floatarray(float 	*floatArray,
                     int 	numRows, 
		     int 	numColumns, 
		     char 	*fileName)
{
   int 		r;
   int 		c;
   int 		i;
   float 	maxval = -999999.;
   int		row_with_max = -1, col_with_max = -1;
   
   
   FILE *fp;
   
   fp = fopen(fileName, "w");
   if (! fp)
   {
      fprintf(stderr, "Cannot open file %s with mode w\n", fileName);
      return;
   }
   
   
   i = 0;
   for (r = 0; r < numRows; r++)
   {
      fprintf(fp, "row %-3d: ", r + 1);
      
      for (c = 0; c < numColumns; c++)
      {
	 
	 if (floatArray[i] == 0.0)
	    fprintf(fp, " zero  " );
	 else if (floatArray[i] < 0.0)
	    fprintf(fp, " lt%-3d ", c+1);
	 else
	    fprintf(fp, "%6.2f ", floatArray[i]);
	 
	 if (floatArray[i] > maxval)
	 {
	    maxval = floatArray[i];
	    row_with_max = r + 1;
	    col_with_max = c + 1;
	 }
	 
	 i++;
      }
      fprintf(fp, "\n");
   }
   
   printf("maxval = %f at row, col => %d, %d\n",
	  maxval, row_with_max, col_with_max);
   fprintf(fp, "maxval row,col counts start at 1, not 0\n");
   fprintf(fp, 
          "maxval = %f at row, col => %d, %d\n",
	  maxval, row_with_max, col_with_max);
   
   
   fclose(fp);
   
   return;
}

/*************************************************************************/

void print_shortarray(short 	*shortArray,
		      int 	numRows, 
		      int 	numColumns,
		      char 	*fileName)
{
   int r;
   int c;
   int i;
   
   
   FILE *fp;
   
   fp = fopen(fileName, "w");
   if (! fp)
   {
      fprintf(stderr, "Cannot open file %s with mode w\n", fileName);
      return;
   }
   
   
   i = 0;
   for ( r = 0; r < numRows; r++)
   {
      for (c = 0; c < numColumns; c++)
      {
	 
	 fprintf(fp, "%d ", shortArray[i]);
	 
	 i++;
      }
      fprintf(fp, "\n");
   }
   
   
   fclose(fp);
   
   return;
}
