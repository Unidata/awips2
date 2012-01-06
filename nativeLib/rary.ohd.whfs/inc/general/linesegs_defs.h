#ifndef LINESEGS_DEFS_H
#define LINESEGS_DEFS_H


/* definitions */

#define MAX_HRAP_ROWS        5000
#define NMILE_PER_DEG_LAT      60

#define PI 3.14159
#define RAD_PER_DEG PI/180

#define MILES_PER_NMILE  KM_PER_NMILE/KM_PER_MILE 
#define NMILES_PER_MILE   .8683910
#define M_PER_NMILE   1852
#define KM_PER_NMILE     1.852
#define KM_PER_MILE      1.609344

#define MIN_DOUBLE_ERROR 0.00000001


/* enumerated data */

typedef enum BinMark
{
        UNKNOWN,
        OUTSIDE,
	INSIDE
} BinMark;


/* structures */

typedef struct Point
{
     double x;
     double y;  
} Point;


typedef struct HrapBin
{
     double row;
     double col;
     
     Point point;
    
     BinMark mark; 
      
} HrapBin;


typedef struct HrapBinList
{
     long rows[MAX_HRAP_ROWS];
     long beginCols[MAX_HRAP_ROWS];
     long endCols[MAX_HRAP_ROWS];
     
     long numRows;
     
     long numBins;
     double area;              /*square miles */
     
} HrapBinList;  
 


typedef  struct LineSegment
{
     /* points defining the segment */
   
     Point point1;
     Point point2;
     
     
     /* slope and base of the line of which the segment is a part */
     
     int isVertical;
     double x_value;
     
     
     double slope;
     double base;
   
     
     /* points that define the box that surrounds the segment */
     
     Point  min;  
     Point  max;  
} LineSegment;  


#endif
