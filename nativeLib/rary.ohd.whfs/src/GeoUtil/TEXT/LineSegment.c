#include "LineSegment.h"


/*************************************************************************/

void initLineSegment(Point p1, Point p2, LineSegment *seg)
{
     
     /*
     Given 2 points, initializes the LineSegment structure.
     
     Determines if the segment is a vertical line.
     Calculates slope and base (if not vertical)
     Stores the x_value if it is vertical.
     
     Calculates the max and min x and y values of the segment.
   
     */
   
   
     double rise;
     double run;
     
     
     /*
     copy the points to the LineSegment structure
     */
     seg->point1 = p1;
     seg->point2 = p2;
     
     
     /*
     calculate rise and runset slope and base of line
     */ 
     rise = seg->point2.y - seg->point1.y;
     run  = seg->point2.x - seg->point1.x;
     
     
     /*
     
     */
     if (run == 0)
     {
	  seg->isVertical = 1;
	  seg->x_value = seg->point1.x;
     }
     else
     {
	  seg->isVertical = 0;
	  seg->x_value = seg->point1.x;
	  
	  seg->slope = rise/run;
	  seg->base = seg->point1.y - (seg->slope * seg->point1.x );    
     }
     
     
     /*
     set the max and min box
     */
     seg->min.y = minDouble(seg->point1.y, seg->point2.y);
     seg->min.x = minDouble(seg->point1.x, seg->point2.x);
     seg->max.y = maxDouble(seg->point1.y, seg->point2.y);
     seg->max.x = maxDouble(seg->point1.x, seg->point2.x);
     
     return;
     
}


/***************************************************************************/

LineSegment *getSegmentsFromPoints(Point *points,
				   long numPoints,
				   long *numSegments)
{
     /*
          Constructs an array from segments from an array of n points.
	  the i th and i+1 th points are used to initialize n-1 segments.
	  
	  The allocated memory must be freed by the user.
     */
     
     
     LineSegment *lines = NULL;
     long size;
     long i;
   
     *numSegments = numPoints - 1;  
     size = (*numSegments) * sizeof(LineSegment);
     
     
     if ( (lines = (LineSegment *) malloc (size) ) )
     {
          for (i = 0; i < *numSegments; i++)	
	  {       	        
	       initLineSegment(points[i], points[i+1], &lines[i]);
	  }
	  
     }
     
     else
     {
          *numSegments = 0;	
     }
     
     return lines;   
}


/***************************************************************************/

void getLineSegmentRelationship(LineSegment s1, LineSegment s2,
			       int *intersect, int *overlap,
			       Point *intersectPoint)
{
     /*
          Determines if two line segments intersect or overlap.
	  If they intersect, then the point of intersection is returned.
	  
	  The value returned by intersectPoint is only valid if
	  *intersect is true.
     
     */
     
     
     *overlap = 0;
     *intersect = 0;
     
     
     /*
     both vertical
     */
     if (s1.isVertical && s2.isVertical)
     {
	  if (s1.x_value == s2.x_value) 	
	  {
	       if ( (s1.min.y > s2.max.y) ||
		   (s2.min.y > s1.max.y)
		   )
	       {
		    *overlap = 0;      	  
	       }
	       else
	       {
		    *overlap = 1;	  
	       }
	  }
     }
     
     
     /* 
     at least one segment is non-vertical
     */
     else  
     {	  
	  getIntersectionOfSegments(s1, s2,
				    intersect,
				    intersectPoint);
     }
     
     
     return;   
}


/**************************************************************************/

void getIntersectionOfSegments(LineSegment s1,
			       LineSegment s2,
			       int *intersect,
			       Point *p)
{
     /*
          Determines the point of intersection (if any) between
	  two segments
     */
     
     double x;
     double y;
     double y1;
     double y2;
     
     LineSegment tempVertical;
     LineSegment tempNonVertical;
     
     int isVertical1 = 0;
     int badDomain1 = 0;
     int isVertical2 = 0;
     int badDomain2 = 0;
     
     *intersect = 0;
     
     if (s1.isVertical && s2.isVertical)
     {
	  
          *intersect = 0;	  
     }
     
     
     /*
     if one is a vertical line and one is not
     */
     else if (s1.isVertical || s2.isVertical)
     {
	  
	  /*
	  assign to tempVertical and
	  tempNonVertical
	  */
	  if (s1.isVertical)
	  {
	       tempVertical = s1;  
	       tempNonVertical = s2;
	  }
	  else
	  {
	       tempVertical = s2;   
	       tempNonVertical = s1;
	  }
	  
	  
	  /*
	  see if vertical segment is in the x range
	  of the non vertical segment
	  */
	  if ( isBetweenInclusive(tempVertical.x_value,
				  tempNonVertical.min.x,
				  tempNonVertical.max.x)
	      )
	  {
	       y = evaluateLineSegmentAtX(tempNonVertical,
					  tempVertical.x_value,
					  &isVertical1, &badDomain1);
	       
	       if ( (!badDomain1) && (isBetweenInclusive(y,
						    tempVertical.min.y,
						    tempVertical.max.y)))
	       {
		    p->x = tempVertical.x_value;
		    p->y = y;
		    *intersect = 1;  
	       }
	       else
	       {
		    *intersect = 0;     
	       }
	       
	  }
	  
     }
         
     else  /* the lines are both non vertical */
     {
          x = (s1.base - s2.base) / (s2.slope - s1.slope);
	  
	  
	  /*
	  verify equation
	  */
 	  y1 = evaluateLineSegmentAtX(s1, x, &isVertical1, &badDomain1);
	  y2 = evaluateLineSegmentAtX(s2, x, &isVertical2, &badDomain2);
	  
	  if (badDomain1 || badDomain2)
          {
	     /*
	       printf("badDomain1 = %d, badDomain2 = %d \n", 
	       badDomain1, badDomain2); 
	      */
	       *intersect = 0;
	  }
	  
	  else if ( ( abs(y1 - y2) > MIN_DOUBLE_ERROR ) )
	  {
	     /*
	       printf("y values: y1 = %lf and y2 = %lf do NOT match\n",
		      y1, y2);
	     */
	       *intersect = 0;     
	  }
	  
	  else
	  {
	     /*
	       printf("y values: y1 = %lf and y2 = %lf match\n",
		      y1, y2);
	      */
	     
	       p->x = x;
	       p->y = y1;
	       *intersect = 1;     
	  }
     }
     
     return;
}


/***************************************************************************/

double evaluateLineSegmentAtX(LineSegment segment,
			      double x,
			      int *isVertical,
			      int *badDomain)
{
     /*
          Determine the Y value of a segment, given x.
	  If the line is Vertical or X is not in the domain, then
	  the returned value is invalid.  Always check the
	  isVertical and badDomain arguments.
     
     */
     double y=0;
     
     *isVertical = 0;
     *badDomain = 0;
     
     
     if (segment.isVertical)
     {
	  y = segment.min.y;
          *isVertical = 1;
	  /*
	  printf("why did you evaluate a vertical line?\n");
	  */
     }
          
     else if ( isBetweenInclusive(x, segment.min.x, segment.max.x) )
     {	
          y = (segment.slope * x) + segment.base;
     }
     
     else
     {    /*
	  printf(" out of x-value's range \n");
	  */
	  *badDomain = 1;
     }
     
     return y;   
}


/***************************************************************************/

Point * getIntersectionPoints(LineSegment s,
			      const LineSegment segments[],
			      long numSegments, long *numPoints)
{
     
     /*
          returns the points of intersection between one segment
	  and an array of segments
	  
	  The memory must be freed by the user.
     
     */
     
     int intersect = 0;
     long i;
     Point intersectPoint;
     Point *points = NULL;
     long actualPoints = 0;
     
     
     points = (Point *) malloc (sizeof(Point) * numSegments);
     
     if (points)
     {
	  for (i = 0; i < numSegments; i++)
	  { 
	       getIntersectionOfSegments(s, segments[i],
					 &intersect,
					 &intersectPoint);
	       
	       if (intersect)
	       {
	            points[actualPoints] = intersectPoint;
		    actualPoints++;
	       }
	       
	  }
     }
     
     *numPoints = actualPoints;
     
     if (actualPoints > 0)
     {
          points = (Point *) realloc(points, (sizeof(Point) * actualPoints ));
     }
     else if (points)
     {
          free (points);
	  points = NULL;
     }
	
     
     return points;
     
}




/***************************************************************************/
int pointsEqual(Point p1, Point p2)
{
     int isEqual = 0;
     
     
     if ((p1.x == p2.x) && (p1.y == p2.y))
     {
          isEqual = 1;
     }
     
     return isEqual;

}


/***************************************************************************/

void getXYMaxMin(const LineSegment segments[],
		     long numSegments,
		     Point *max,
		     Point *min)
{
     
     /*
          Determines the max and min x and y values for an array of
	  segments.
     
     */
     
   
     long i;
    
     /*
     init the returned values
     */
     max->x = segments[0].max.x;
     max->y = segments[0].max.y;
     
     min->x = segments[0].min.x;
     min->y = segments[0].min.y;
     
     
     for (i = 1; i < numSegments; i++)
     { 
	  max->x = maxDouble(segments[i].max.x, max->x);   
	  max->y = maxDouble(segments[i].max.y, max->y);  
	  
	  min->x = minDouble(segments[i].min.x, min->x);   
	  min->y = minDouble(segments[i].min.y, min->y);    
     }
   
     return;   
}


/***************************************************************************/

int isBetweenInclusive(double x, double start, double end)
{
 
     /*
          returns whether (x is >= start) and (x >= end)
     
     */
     
     int isBetween = 0;   
     
     /*
     printf("x = %lf start = %lf end = %lf\n",
	    x, start, end);
     */
     
     if ((x >= start) && (x <= end))
     {
	  
          isBetween = 1;	   
     }
     
     return isBetween;
}


/***************************************************************************/

double minDouble(double num1, double num2 )
{
     /* returns the minimum of two doubles */
   
     if (num1 < num2)
          return num1; 	  
     else
          return num2;	  
}   


/***************************************************************************/

double maxDouble(double num1, double num2 )
{
     /* returns the maximum of two doubles */
   
     if (num1 > num2)
          return num1; 	  
     else
          return num2;	
     
}


/***************************************************************************/
/***************************************************************************/

void printPoint(Point point)
{
     /* prints a point's values */
   
     printf("x = %f  y = %f\n", point.x, point.y); 
     
     return;    
}


/***************************************************************************/

void printLineSegment(LineSegment s)
{
     /*
          Prints the values of a line segment
     */
   
     if (s.isVertical)
     {
	  printf("Vertical segment: x = %f \n from y = %f  to y = %f \n",
		 s.x_value, s.min.y, s.max.y);	 
     }
     
     else
     {
	  
	  printf("point 1: ");
	  printPoint(s.point1);
	  
	  printf("point 2: ");
	  printPoint(s.point2);
	  
	  printf("slope = %f  base = %f \n", s.slope, s.base);
     }	 
     
     
     printf("min point: ");
     printPoint(s.min);
     
     printf("max point: ");
     printPoint(s.max);
     
     return;     
}

