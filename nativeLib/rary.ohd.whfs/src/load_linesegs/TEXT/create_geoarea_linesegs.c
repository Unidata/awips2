#include "DbmsDefs.h"
#include "geo_header_file.h"
#include "create_geoarea_linesegs.h"
#include "HrapBinAssign.h"

/***************************************************************************/
/***************************************************************************/

void createLinesegsForAreas(FILE * infile , char * geotype , int rank ,
                            int geotable , int verbose, int printOnly)
{
   GeoAreaData *gHead = NULL;
   GeoAreaData *gPtr = NULL;

   gHead = read_geodata(infile, geotype, rank, GEOAREA);
   if (gHead == NULL)
   {
      printf("ERROR: Could not read data for geotype %s.\n", geotype);
   }

   gPtr = (GeoAreaData *) ListFirst(&gHead->list);
      
   while (gPtr)
   {	       
       createLinesegsForArea(gPtr, verbose, printOnly);
	 
       gPtr = (GeoAreaData *) ListNext(&gPtr->node);
   }
      
    freeGeoAreaData(& gHead);
   
   return ;   
}

/***************************************************************************/

void createLinesegsForArea(GeoAreaData *gPtr, int verbose, int printOnly)
{
   
   HrapBinList binList;
   
   
   /* do the main processing */
   
   printf ( "Processing area %s: " , gPtr->area_id ) ; 
   getHrapBinListForArea ( gPtr , & binList ) ;
   printf ( " Writing %ld rows\n" , binList.numRows ) ;
   
   
   /* print or write the resulting data */
   
   if (verbose)
   {
      printHrapBinList(&binList);
      printf("\n\n");
   }
   
   if (! printOnly)
   {
      write_lineseg(gPtr->area_id, & binList);

   }
   
   return;   
}


/***************************************************************************/

void getHrapBinListForArea(GeoAreaData *gPtr, HrapBinList *binList)
{    
   /* This function uses areaId to search the database for the
      polygon associated with the area.
      Then it fills the HrapBinList structure by finding
      all the HRAP bins whose centers are inside the area. */
   
   Point *points = NULL;
   long numPoints = 0;
   
   LineSegment *segments = NULL;
   long numSegments = 0;
   
   points   = getPointsFromArea(gPtr, &numPoints);
   
   segments = getSegmentsFromPoints(points,
				    numPoints,
				    &numSegments);
   
   getHrapBinListFromSegments(segments,
			      numSegments,
			      binList);
   
   
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

Point *getPointsFromArea(GeoAreaData *gPtr, 
			 long *numPoints)
{
   
   /* Creates an array of points from the information pointed to
   by the GeoArea pointer.   
   Ensures that  (1) the last point is the same as the first and
   (2) that if n points in a row are the same in the
   database, only one point is propagated to the points array */
   
   Point *points = NULL;
   long i;
   long pointCount = 0;
   pointCount = 0;
   
   points = (Point *) malloc (sizeof(Point) * (gPtr->npts + 1));
 
   if (points)
   {
      /* init the first point */
      
      points[0].y = gPtr->lat[0];
      points[0].x = gPtr->lon[0];
      pointCount++;
      
      /* for each input point from the database, starting with
	 the second point */
      
      for (i = 1; i < gPtr->npts ; i++)
      { 
	 
	 /* if input points are different */
	 
	 if (  ( gPtr->lat[i] != gPtr->lat[i-1] ) ||
	     ( gPtr->lon[i] != gPtr->lon[i-1] )
	     )
	 {
	    points[pointCount].y = gPtr->lat[i];
	    points[pointCount].x = gPtr->lon[i];
	    pointCount++;
	 }
	 
	 else
	 {
	    printf("polygon had duplicate points, extra eliminated by " 
		   "getPointsFromArea()!\n");
	 }
      }
      
      
      /* if the first point and the last point are not the same,
	 add a final point that is the same as the first */
      
      if ( ! pointsEqual(points[0], points[pointCount - 1]) )
      {
	 points[pointCount].y = points[0].y;
	 points[pointCount].x = points[0].x;
	 pointCount++;
	 
	 printf("polygon had to be closed by getPointsFromArea()!\n");        
      }
   }
   
   *numPoints = pointCount;
   
   return points;   
}

