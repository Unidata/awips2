#ifndef DRAW_DAMS_H
#define DRAW_DAMS_H

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "time_defs.h"
#include "HvDisplayControl.h"
#include "HvColorList.h"

#define NID_ID_LEN 10
#define DAMREPORTLIST_NAME_LEN 20

typedef struct
{
   Node node;
   
   char   nidid[ NID_ID_LEN + 1 ] ;
   char   name [ DAMREPORTLIST_NAME_LEN + 1 ] ; 
   
   char	 use;  /* indicates whether to show this value or filter it */	   

   double latitude ;
   double longitude ;
   struct PointPixel  pos ; /* This user may store the pixel values
                               corresponding to the latitude/longitude
                               of this station here.*/

   List	   list;

} DamReportList;


/* driver functions */

void drawAllDamDataSets ( ) ;

void drawDamDataSet( DamReportList * pDamReportList , int x , int y , char * color ) ;

/* draw actual dam icons */

void drawDamIcon ( int x , int y , char * color ) ;
void draw_damcrest_point (int area, int number, int x, int y, char * color );

/* draws the nidid and name around the icon */

void drawDamIconLabels ( DamReportList * pDamReportList , int x , int y ) ;

// from map lib		        
//void _area_draw_fill_half_circle(Pixmap map,int x,int y,int width,int height);		        

#endif
