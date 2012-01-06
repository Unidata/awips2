
#include "grid.h"
#include "map.h"
#include "map_convert.h"
#include "map_library.h"
#include "map_resource.h"

/***************************************************************************/

void initGrid(Grid 			*grid,
	      const ArealProduct 	*product,
	      FfmComparisonType 	comparisonType)
{
   double lat;
   double lon;
   int gotRadar = 0;
   HvDisplayControl *hdc = getHvDisplayControl();
   float *floatArray = NULL;
   
   
   
   /* free the grid structure */
   freeGrid(grid);
   
   
   /* set the sourceId and the threshold name */
   strncpy(grid->sourceId, product->spec.sourceId, SOURCE_ID_LEN);
   
   
   /* hardcode the number of rows and columns.
      This will need to be changed when displaying things other
      than 1 radar's hrap grid */
   grid->numRows    = LOCAL_HRAP_ROWS;
   grid->numColumns = LOCAL_HRAP_COLS;
   
   gotRadar = getRadarLatLon(grid->sourceId, &lat, &lon);
   printf("initGrid radarId = :%s: lat,lon = %f %f\n",
	  grid->sourceId, lat, lon);
   
   /* allocate the cpArray points in the ColoredPointArray structure */
   
   if (! allocateColoredPoints(&grid->pointArray,
			       grid->numRows * grid->numColumns))
   {
      fprintf(stderr,"\nError, unable to allocate ColoredPoints\n");
      return;   
   }
   
   if (gotRadar)
   {
      
      switch (product->descr.mode)
      {
	 case PRECIP_MODE:
	    if (product->precip_status != -1)
	    {
	       fillHrapPointArray(&grid->pointArray, product->precip_grid,
				  grid->numRows, grid->numColumns,
				  lat, lon);   
	    }
	    break;
	    
	 case FFG_MODE:
	    if (product->ffg_status != -1)
	    {
	       fillHrapPointArray(&grid->pointArray, product->ffg_grid,
				  grid->numRows, grid->numColumns,
				  lat, lon);   
	    }
	    break;
	    
	    
	 case COMPARISON_MODE:
	    if ((product->ffg_status != -1) &&
		(product->precip_status != -1))
	    {
	       
	       floatArray = compareGridArray(product->ffg_grid,
					     product->precip_grid,
					     grid->numRows * grid->numColumns,
					     comparisonType);
	       
	       if (floatArray)
	       {
		  fillHrapPointArray(&grid->pointArray, floatArray,
				     grid->numRows, grid->numColumns,
				     lat, lon);
		  
		  free(floatArray);
		  floatArray = NULL;
	       }
	    }
	    break;

         default:
            break;
      }
      
      
   }
   
   else
   {
      fprintf(stderr, "Radar NOT found in initGrid().\n");	
   }
   
   
   
   /* determine colors for grid points */
   
   copyColorThresholdArray(&grid->thresholdArray,
			   &hdc->displaySettings.areal.ctArray);
   colorizeGrid(&grid->pointArray, &grid->thresholdArray); 
   
   
   return;
}


/***************************************************************************/

float * compareGridArray(float 			*ffgGrid, 
			 float 			*precipGrid, 
			 long 			gridSize,
			 FfmComparisonType 	comparisonType)
{   
   
   float *floatArray = NULL;
   long i;
   
   
   floatArray = (float *) malloc (sizeof(float) * gridSize);
   if (floatArray)
   {
      for (i = 0; i < gridSize; i++)
      {
	 if ((ffgGrid[i]    == OUTSIDE_RADAR_RANGE) ||
	     (precipGrid[i] == OUTSIDE_RADAR_RANGE))
	 {
	    floatArray[i] = OUTSIDE_RADAR_RANGE;   
	 }
	 
	 else if ((ffgGrid[i]    == MISSING) ||
		  (precipGrid[i] == MISSING))
	 {
	    floatArray[i] = MISSING;	     
	 }	 
	 
	 else 
	 {
	    if (comparisonType == COMPARE_DIFFERENCE)
	    {
	       floatArray[i] = precipGrid[i] - ffgGrid[i]; 	  
	    }
	    else if ( comparisonType == COMPARE_RATIO)
	    {
	       floatArray[i] = precipGrid[i] / ffgGrid[i];	  
	    }
	 }
      }
   }
   
   return floatArray;   
}


/***************************************************************************/

void drawGriddedArealData(HvDisplayControl *hdc)
{
   
   /* draw the grid */
   
   drawGrid(&hdc->grid);
   
   return;
}  


/*************************************************************************/

void freeGrid(Grid *grid)
{
   grid->productType = NON_PRODUCT;	
   
   freeColoredPointArray(&grid->pointArray);
   freeColorThresholdArray(&grid->thresholdArray);
   
   return;   
}


/**************************************************************************/

int  getRadarLatLon(char 	*radarId, 
		    double 	*radLat, 
		    double 	*radLon)
{
   RadarLoc *rHead = NULL;
   RadarLoc *rPtr = NULL;
   char where[BUFSIZ];
   int	success = 0;
   
   sprintf(where, " WHERE radid = '%s' ", radarId);
   *radLat = *radLon = 0;
   
   
   if ( ( rHead = GetRadarLoc ( where ) ) )
   {
      rPtr = (RadarLoc *) ListFirst(&rHead->list); 
      
      if (rPtr)
      {
	 *radLat = rPtr->lat;
	 *radLon = rPtr->lon;
	 success = 1;
      }
      
      FreeRadarLoc(rHead);
   }
   
   return success;
} 


/**************************************************************************/

void printFloatArray(float *floatArray,
		     int numRows, int numColumns, char *fileName)
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
	 fprintf(fp, "%f ", floatArray[i]);
	 
	 i++;
      }
      fprintf(fp, "\n");
   }
   
   
   fclose(fp);
   
   return;   
}


/*************************************************************************/

void printShortArray(short *shortArray,
		     int numRows, int numColumns, char *fileName)
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
	 fprintf(fp, "%i ", shortArray[i]);
	 
	 i++;
      }
      fprintf(fp, "\n");
   }
   
   
   fclose(fp);
   
   return;   
}


/*************************************************************************/

void  fillHrapPointArray(ColoredPointArray 	*cpArray, 
			 float 			sourceArray[],
			 long 			numRows, 
			 long 			numCols,
			 double 		radarLat,
			 double 		radarLon)  
{	
   double  rowOffset, colOffset;   
   double  binLat, binLon;   
   double  radarRow, radarCol;   
   double  nationalRow, nationalCol;
   
   int c, r, i = 0;
   int numTimes = 0;
   
   /* fill up point array */
   
   /* determine radar's national hrap row and col */
   
   LatLongToHrapByReference(radarLat, radarLon, &radarRow, &radarCol);
   
   
   
   radarRow = floor(radarRow);
   radarCol = floor(radarCol);
   
   
   /* subtract 66 from radarRow and radarCol to get a national hrap bin
      for what would be a local hrap bin of 0,0 (I know they start at 1,1)
      These are the offsets to convert to a national HRAP scale. */
   
   rowOffset = radarRow - 66;
   colOffset = radarCol - 66;
   
   
   for (r = 1, i = 0; ( (r <= numRows ) && (i < cpArray->length) ); r++)
   {
      
      for (c = 1; ( (c <= numCols)  && (i < cpArray->length) ) ; c++)
      {   
	 
	 nationalRow = r + rowOffset;
	 nationalCol = c + colOffset;
	 
	 HrapToLatLongByReference(nationalRow, nationalCol, &binLat, &binLon );
	 
	 cpArray->points[i].row = nationalRow;
	 cpArray->points[i].col = nationalCol;
	 
	 cpArray->points[i].lat = binLat;
	 cpArray->points[i].lon = binLon;
	 
	 cpArray->points[i].value = sourceArray[i];
	 
	 numTimes++;
	 
	 /*
	 if (numTimes==1)
	 {
	 printf("radarLat = %lf, radarLon = %lf\n", radarLat, radarLon);   
	 printf("nationalRow = %lf, nationalCol = %lf\n", nationalRow, nationalCol);
	 printf("binLat = %lf, binLon = %lf\n", binLat, binLon);
	 }
	 */
	 
	 i++;
      }
   }
   
   return;   
}


/**************************************************************************/

void  colorizeGrid(ColoredPointArray *pointArray,
		   const ColorThresholdArray *colorArray)
{
   
   determineAllPointColors(pointArray, colorArray);
   
   return;
}


/**************************************************************************/

void  determineAllPointColors(ColoredPointArray *pointArray,
			      const ColorThresholdArray *colorArray)
{
   long i = 0 ;
   const char * color = NULL ;

   /* Retrieve a pointer to the static color list information. */
   HvColorList * hcl = getHvColorList ( ) ; 
   
   for (i = 0; i < pointArray->length; i++)
   {
      color = determineColorByThreshold ( pointArray->points[i].value ,
	    			          MISSING ,
				          colorArray ) ;
      strncpy ( pointArray->points[i].color , color ,
                MAX_COLOR_NAME_LENGTH - 1 ) ;

      if ( pointArray->points[i].value == OUTSIDE_RADAR_RANGE )
      {
    	 strncpy ( pointArray->points[i].color ,
                       hcl->outside_radar_range ,
                       MAX_COLOR_NAME_LENGTH - 1 ) ;
      }

      pointArray->points[i].color [ MAX_COLOR_NAME_LENGTH ] = '\0' ; 
   }
   
   return;   
}


/*************************************************************************/

void  drawGrid(Grid *grid)  
{	
   long 	  i = 0;
   ColoredPoint  point;
   
   /* draw the cpArray points */
   
   for (i = 0; i < grid->pointArray.length; i++)
   {
      point = grid->pointArray.points[i];
      
      drawHrapBin(point);
      /*
      if (point.value > 0.001)
      {
      drawHrapBin(point);
      }
      */
      
   }
   
   
   return;		
}


/***************************************************************************/

void drawHrapBin(ColoredPoint point)			       
{
   
   Pixmap	pixmap;
   Display	*display;
   GC		gc;
   
   /* map points of SWS types */
   
   XPoint		corners[4];
   
   /* graphics preparation */
   
   pixmap = _get_map_pixmap ( ) ;
   display = _get_map_display ( ) ;
   gc = _get_map_gc ( ) ; 
   
   
   /* determine the corners of the polygon to be drawn */
   
   getCorners(point, corners);
   
   
   /* change the color */
   
   mSetColor ( point.color ) ;
   
   /* draw the polygon */
   
   XFillPolygon(display, pixmap, gc, corners, 4, Convex, CoordModeOrigin);
   
   return;   
}


/*****************************************************************************/

void getCorners(ColoredPoint point, XPoint corners[])
{	
   
   /* Gets the corners of the polygon to draw for the Hrap bin.
      The point is at the lower left of the HRAP bin, so
      this function lets that be one corner and then finds the
      other 3 corners. */		
   
   /* calculate XPoints of the corners */
   
   HrapToXPoint(point.row,     point.col,      &corners[0]);
   HrapToXPoint(point.row + 1, point.col,      &corners[1]);
   HrapToXPoint(point.row + 1, point.col + 1,  &corners[2]);
   HrapToXPoint(point.row,     point.col + 1,  &corners[3]);
   
   return;   
}


/*****************************************************************************/


void printXPoint(FILE *fp, XPoint point, char *string)
{
   fprintf(fp, "%s  x = %u, y = %u \n", string, point.x, point.y);   
   
   return;
}


/*****************************************************************************/

void HrapToXPoint(double row, double col, XPoint *xPoint)
{

   double lat, lon;
   int x , y;

   HrapToLatLongByReference(row, col, &lat, &lon);

   mConvertLatLon2XY ( ( float ) lat , ( float ) ( -1 * lon ) , & x , & y ) ;

   xPoint->x = ( short ) x ;
   xPoint->y = ( short ) y ;
          
   return;
}

/*****************************************************************************/
