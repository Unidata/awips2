
#include "ColoredPoint.h"
#include "string.h" /* Included for the "memset" function used in 
                       in the initColoredPoint routine. */

/***************************************************************************/

int allocateColoredPoints(ColoredPointArray *pointArray, long numPoints)
{
  	long	size = 0;
	long	i = 0;
   
  	/*
		Init pointArray
	*/
	pointArray->points = NULL;
	pointArray->length = 0;
	
	
	/*
		allocate memory for pointArray points
	*/
	size = (numPoints) * sizeof(ColoredPoint);
   	pointArray->points = (ColoredPoint *) malloc (size);
	
	if (pointArray->points)
	     pointArray->length = numPoints;
	else
	     pointArray->length = 0;
  
	
	for ( i = 0 ; i < pointArray->length ; i++ )
	{
	     initColoredPoint(&pointArray->points[i]);   
	}
	
	return (pointArray->length);
}

/***************************************************************************/

void printPointArrayGrid(ColoredPointArray *pointArray,
			    int numRows, int numColumns,
			    char *fileName)
{
     int i;
     int r;
     int c;
     
     
     FILE *fp;
     if (fileName)
     {
	  fp = fopen(fileName, "w");
	  if (! fp)
	  {
	       fprintf(stderr, "Cannot open file %s with mode w\n", fileName); 
	       return;
	  }
     }
     else
     {
	  fp = stdout;	   
     }
    
     
     i = 0;
     for ( r = 0; r < numRows; r++)
     {
           for (c = 0; c < numColumns; c++)
	   {
	        if (i >= pointArray->length)
		{
		     fprintf(fp, "out of bounds in printPointArrayGrid()\n");
		     break;     
		}
		
	        fprintf(fp, "%f ", pointArray->points[i].value);

		i++;
	   }
	   fprintf(fp, "\n");
     }
	
     
     fclose(fp);
     
     return;   
}	
     
/*************************************************************************/

void  printColoredPointArray(ColoredPointArray *pointArray, char *fileName )
{
     
     int i;
     FILE *fp;
     
     
     if (fileName)
     {
	  fp = fopen(fileName, "w");
	  if (! fp)
	  {
	       fprintf(stderr, "Cannot open file %s with mode w\n", fileName); 
	       return;
	  }
     }
     else
     {
	  fp = stdout;	   
     }
     
     
     for (i = 0; i < pointArray->length; i++)
     {
	
	 
	  if ( (i % 10) == 0)
	  {
	       fprintf(fp,"\n");	
	  }
	  fprintf(fp, "%6.2f ", pointArray->points[i].value);
     }
     return;
}
/***************************************************************************/

void freeColoredPointArray(ColoredPointArray *pointArray)
{
     
        if (pointArray->points)
	      free(pointArray->points);
	
	pointArray->points = NULL;
	pointArray->length = 0;
   
 	return;  
}

/***************************************************************************/
void initColoredPoint(ColoredPoint *point)
{
   
     point->row = MISSING;
     point->col = MISSING;
     point->lat = MISSING;
     point->lon = MISSING;
     point->value = MISSING;
     memset ( point->color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
     return;
}
/***************************************************************************/
