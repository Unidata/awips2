#ifndef LINE_SEGMENT_H
#define LINE_SEGMENT_H


#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "math.h"

#include "GeoArea.h"   /* database functions */

#include "geoutil.h"   /* utilities */

#include "linesegs_defs.h"


/* prototypes */

int pointsEqual(Point p1, Point p2);

LineSegment * getSegmentsFromPoints(Point *points,
				   long numPoints,
				   long *numSegments);  

void initLineSegment(Point p1,
		     Point p2,
		     LineSegment *s);


void getLineSegmentRelationship(LineSegment s1, 
				LineSegment s2,
			        int *intersect, 
				int *overlap,
			        Point *intersectPoint);


double evaluateLineSegmentAtX(LineSegment segment,
			      double x,
			      int *isVertical,
			      int *badDomain);

void getIntersectionOfSegments(LineSegment s1,
			       LineSegment s2,
			       int *intersect,
			       Point *p);  

Point * getIntersectionPoints(LineSegment s,
			      const LineSegment segments[],
			      long numSegments, 
			      long *numPoints);


void getXYMaxMin(const LineSegment segments[],
		     long numSegments,
		     Point *max,
		     Point *min);


void printPoint(Point point);
void printLineSegment(LineSegment s);


int isBetweenInclusive(double x, double start, double end);   
double minDouble(double num1, double num2);
double maxDouble(double num1, double num2);
      


#endif
