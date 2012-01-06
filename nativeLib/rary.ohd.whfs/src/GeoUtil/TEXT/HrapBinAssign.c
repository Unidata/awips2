#include "HrapBinAssign.h"

/* This enumeration is being given file scope only.  There is no reason
   why the outside world needs to know about it. */  
enum LATLON_BOX_CORNERS { LATLON_NORTHWEST_CORNER , LATLON_NORTHEAST_CORNER , 
                          LATLON_SOUTHEAST_CORNER , LATLON_SOUTHWEST_CORNER } ;

/***************************************************************************/
 
void getHrapBinListFromSegments(const LineSegment segments[],
				long numSegments,
				HrapBinList *binList)
     
{
     /*
          Given an array of line segments, this function
	  fills the HrapBinList variable with the bins inside the
	  area defined by the segments.
	  
	  Algorithm:
	  
	  Find the max and min lat-lon corners of the area.
	  Convert the lat-lon to HRAP coords.
	  Widen the HRAP box to search, to make sure it starts outside
	  the polygon.
	  
	  For each row
	       For each column
	       
	           Draw a segment from the previous column.
	           If that segment intersects one of the polygon's segments,
	           then add the number of intersections to the total number of
	           intersections for the row.  If the row intersections is odd,
	           then the hrap bin is in the polygon, otherwise it is outside.
		   
		   Compute area for each bin that is within the polygon.
		   Add it to total for area.
     */
     
     LineSegment segment; 
     
     Point * points = NULL;
     long numPoints = 0;
     
     Point maxLatLon;
     Point minLatLon;
     
     Point startLatLon;
     Point endLatLon;
     
     double r;
     double c;
    
     double maxCol;
     double minCol;
     double maxRow;
     double minRow;
     
     double singleBinArea = 0;
     
     int i ;
     int inside = 0;
     long index = 0;
     long numIntersections = 0;
     
     /*
     init HrapBinList
     */
     binList->numRows = 0;
     binList->numBins = 0;
     binList->area = 0;
     
     /*
     get the max and min points of the segments
     */  
     getXYMaxMin(segments, numSegments, &maxLatLon, &minLatLon);
     
     /* Determine the HRAP box that will completely enclose the
        latitude / longitude box defined by the max lat/lon and the
        min lat/lon pairs retrieved above.  This is a fix added on
        March 22, 2004.  Formerly the code just converted the max and min
        lat/lon pairs to HRAP coordinates.  Because the HRAP grid is 
        usually skewed with respect to the lat/lon grid, this will not
        always guarantee that the HRAP box will complete contain the
        lat/lon box. */	

     LatLongToHrapByReference ( maxLatLon.y , maxLatLon.x , & r , & c ) ; 
     minCol = c ;
     maxCol = c ;
     minRow = r ;
     maxRow = r ;
     
     for ( i = LATLON_NORTHEAST_CORNER ; 
           i <= LATLON_SOUTHWEST_CORNER ; 
           ++ i )
     {
        switch ( i )
        {
           case LATLON_NORTHEAST_CORNER :

              LatLongToHrapByReference ( maxLatLon.y , minLatLon.x , & r , & c ) ;
              break ;

           case LATLON_SOUTHEAST_CORNER :
              LatLongToHrapByReference ( minLatLon.y , minLatLon.x , & r , & c ) ;
              break ;

           case LATLON_SOUTHWEST_CORNER :
              LatLongToHrapByReference ( minLatLon.y , maxLatLon.x , & r , & c ) ;
              break ;

           default :

              /* Execution should never reach this point. */
              break ;
        }

        if ( c < minCol )
        {
           minCol = c ;
        }
        else if ( c > maxCol ) 
        {
           maxCol = c ;
        }

        if ( r < minRow )
        {
           minRow = r ;
        }
        else if ( r > maxRow )
        {
           maxRow = r ;
        }
     }
     
     maxRow = floor(maxRow);
     maxCol = floor(maxCol);
     minRow = floor(minRow);
     minCol = floor(minCol);
     
     /*
     expand the box to make sure polygon has been covered
     */
     minRow-=2;
     minCol-=2;
     
     maxRow+=2;
     maxCol+=2;
      
     /*
     Scan across a row
     */
     /*
     printf("minRow = %lf maxRow = %lf\n", minRow, maxRow);
     printf("minCol = %lf maxCol = %lf\n", minCol, maxCol);
     */
     
     for (r = minRow + 0.5; r <= maxRow; r++)
     {
	  inside = 0;
	  numIntersections = 0;
	  
	  
	  /*
               init the first lat lon point
          */
          HrapToLatLongByReference(r, minCol - 0.5, &startLatLon.y, &startLatLon.x);
  
	  
	  for (c = minCol + 0.5; c <= maxCol; c++)
	  {
	       
	       /*
	       get the lat lon coordinate from the hrap row and column
	       */
	       HrapToLatLongByReference(r, c, &endLatLon.y, &endLatLon.x);
	       
	       
	       /*
	       create a segment from start to end
	       */
	       initLineSegment(startLatLon, endLatLon, &segment);
		    
		    
	       points = getIntersectionPoints(segment,
					     segments,
					     numSegments,
					     &numPoints);
	       
	       if (points)
	       {
		    
		  
		  /*
		    printf("the segment:\n");
		    printLineSegment(segment);
		    
		    printf("intersects %ld points: \n", numPoints);
		    for (i = 0; i < numPoints; i++)
		    {
		         printPoint(points[i]);	 
		    }
		    printf("\n\n");
		  */ 
		    
		    free(points);
		    points = NULL;
	       }
	       
	       index = binList->numRows;
	       
	       
	       if (numPoints > 0)
	       {	    
		    numIntersections += numPoints;
		    
	       }
	       
	       
	       /*
	            key of algorithm:
		    if the number of intersections is odd,
		    then you are inside the polygon,
		    else outside
	       */
	       if ( (numIntersections % 2) == 1)
	       {
		    
		    /*
		    if previous bin was inside
		    */
		    if (inside)
		    {			
			 binList->endCols[index] = floor(c);
			 
			 binList->numBins++;
			 singleBinArea = getHrapBinArea(r, c);
			 binList->area += singleBinArea;
			
		    }
		    
		    /*
		    previous bin was outside
		    */
		    else
		    {
	
			 binList->rows[index] = floor(r);
			 binList->beginCols[index] = floor(c);
			 binList->endCols[index] = floor(c);
			 
			 binList->numBins++;
			 singleBinArea = getHrapBinArea(r, c);
			 binList->area += singleBinArea;
			 
		         inside = 1;
			 
		    }
		    
	       }
	       
	       
	       /*
	       else if previous bin was inside and,
	       since this one is not, increment the row counter
	       */
	       else /* intersections is even */
	       {
		    if (inside)
		    {
			 inside = 0;
			 binList->numRows++;	 
		    }
	       }
	       
	       
	       startLatLon = endLatLon;
	       
	  }
	  
	  
	  
     }
        
     return;
}
   
/***************************************************************************/

double getHrapBinArea(double row, double col)
{
     /*
          compute area of an Hrap bin.
	 
	  Algorithm:
	  
	  Convert row and column to lat-lon coords
	  Convert row + 1 and column + 1 to lat lon coords
	  Get the differences in lat and lon
	  Convert to nautical miles.
	  
	  get square miles by multiplying the x and y nautical miles by
	  the conversion factor to square miles.
     
     */
     
     double area;   
     
     double lat_length;
     double lon_length;
     
     double nmile_x_length;
     double nmile_y_length;
     
     double lat1;
     double lat2;
     double lon1;
     double lon2;
     
     /*
          get lat and lon positions
     */
     HrapToLatLongByReference(row, col, &lat1, &lon1); 
     HrapToLatLongByReference(row + 1, col + 1, &lat2, &lon2);  
      
     /*
          get differences in lat and lon
     */
     lat_length = fabs(lat1 - lat2);
     lon_length = fabs(lon1 - lon2);
     
     
     /*
          get the nautical miles from lat and lon lengths
     */
     nmile_y_length = NMILE_PER_DEG_LAT * lat_length;
     nmile_x_length = lon_length * NMILE_PER_DEG_LAT*(cos(RAD_PER_DEG*((lat1+lat2)/2.0)));
     
     
     /*
          convert to miles and get the square
     */
     area = (MILES_PER_NMILE * nmile_y_length) *
	    (MILES_PER_NMILE * nmile_x_length);
     
     return area;
}


/***************************************************************************/

void printHrapBinList(const HrapBinList *binList)
{
     /*
        This function prints to standard output the contents of the
	HrapBinList structure
     */
      
     long i;
     
     printf("rows = %ld\n", binList->numRows);
     
     for (i = 0; (i < binList->numRows) && (i < MAX_HRAP_ROWS); i++)
     {
          printf("%6.0ld %6.0ld %6.0ld \n", binList->rows[i],
		 binList->beginCols[i], binList->endCols[i]);	  
     }
     
     printf("number of bins = %ld\n",binList->numBins);
     
     printf("total area  = %6.2f square miles\n", binList->area);
     
     return;     
}


/***************************************************************************/
