#include "create_ofs_linesegs.h"



/***************************************************************************/

void getHrapBinList_OFSArea(long numPoints,
			    double latArray[], 
			    double lonArray[],
			    long rows[],
			    long beginCols[],
			    long endCols[],
			    long *numRows,
			    long *numBins,
			    double *area)
{    
   /* given the numPoints, and lat and lon arrays, this function fills the
      HrapBinList structure by finding all the HRAP bins whose centers are
      inside the area. The info is returned in the latter 6 args*/
   
   Point *points = NULL;
   int i;  
   LineSegment *segments = NULL;
   long numSegments = 0;
   HrapBinList binList;  
   
   
   /* transfer all the array data into the Point structure
      to facilitate use by the latter apps */
   
   points = (Point *) malloc (sizeof(Point) * numPoints);
   
   for (i = 0; i < numPoints; i++)
   {
      points[i].x = lonArray[i];
      points[i].y = latArray[i];
   }
	   
   
   /* now process the data further in the standard manner */
   
   segments = getSegmentsFromPoints(points,
				    numPoints,
				    &numSegments);
   
   getHrapBinListFromSegments(segments,
			      numSegments,
			      &binList);
   
   
   /* transfer the data to the output vars */
   
   for (i = 0; i < binList.numRows; i++)
   {
      /* printf("**%ld: %ld-%ld\n", 
	     binList.rows[i], binList.beginCols[i], binList.endCols[i]);  */
      
      rows[i]      = binList.rows[i];
      beginCols[i] = binList.beginCols[i];
      endCols[i]   = binList.endCols[i];
   }
   
   *numRows = binList.numRows; 
   *numBins = binList.numBins;
   *area    = binList.area;       
   

   /* free memory */
   
   if (points)
   {
      free(points);
      points = NULL;
   }
   
   if (segments)
   {
      free(segments);
      segments = NULL;
   }
   
   
   return;   
}


/***************************************************************************/


