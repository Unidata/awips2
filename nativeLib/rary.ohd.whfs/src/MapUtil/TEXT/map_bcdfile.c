/*******************************************************************************
* FILENAME:             map_bcdfile.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
* ORIGINAL AUTHOR:      Heather Friedeman
* CREATION DATE:        Unknown
* ORGANIZATION:         OHD/HSEB
* MACHINE:              HP-UX, Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER       DESCRIPTION/REASON
*                   12/02        Bryon Lawrence   Heavily Modified these
*                                                 routines to more
*                                                 efficiently plot data
*                                                 points on the map.
*                   1/31/02      Bryon Lawrence   Modified these routines to  
*                                                 allow filled polygons
*                                                 (such as lakes) to be drawn.
********************************************************************************
*/

/* include files */

#include <float.h>
#include <stdio.h>
#include "stdlib.h"
#include <Xm/Xm.h>
#include "GetOS.h"
#include "map.h"
#include "map_bcdfile.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "Swap4Bytes.h"

/*****************************************************************************
 *
 * Routine: _draw_points
 *
 * Description: this routine draws the points from memory
 *
 ****************************************************************************/

void _draw_points ( Pixmap map , struct _Map_Segment * seg , Boolean fillarea )
{
  int height ;
  int i ;
  int x ;
  int y ;
  int width ;

  XPoint * pts = NULL ;

  float lat , lon ;
  int  num ; 

  width = _get_map_width ( 0 ) ;
  height = _get_map_height ( 0 ) ;

  _set_first_segment ( seg ) ;

  do{
    num = _get_points_in_segment ( ) ;
    pts = (XPoint *)malloc(sizeof(XPoint) * num);

    if (pts == NULL)
    {
      fprintf ( stderr , "In routine \"_draw_points\":\n"
                         "Cannot allocate %d bytes of memory for\n"
                         "\"points\". Num is %d. Size of XPoint is %d.\n",
                         num * sizeof ( XPoint ) , num , 
                         sizeof ( XPoint )  ) ;
      return;
    }

    for ( i = 0 ; i < num ; i++ )
    {
      _get_point_coordinate ( i , & lat , & lon ) ;
      mConvertLatLon2XY ( lat , lon , & x , & y ) ;

      /* Do a range test on the x,y coordinates to make sure they don't
         exceed the bounds of the window. */

      if ( x < 0 )
      {
         x = 0 ;
      }
      else if ( x > width )
      {
         x = width ;
      }

      if ( y < 0 )
      {
         y = 0 ;
      }
      else if ( y > height )
      {
         y = height ;
      }

      pts[i].x = ( short ) x ;
      pts[i].y = ( short ) y ;

    }
    
    /* Test to determine if a filled polygon is to be drawn. */
    if ( fillarea == True )
    {
       _area_draw_fill_polygon ( map , pts , num , Nonconvex ,
                                 CoordModeOrigin ) ;
    }

    /* Must be sure to ALWAYS draw the outline, even when a filled polygon
       is being drawn. */ 
    _area_draw_lines ( map , pts , num ) ;
    if ( pts != NULL ) free(pts);
    pts = NULL ;
  } while(_next_segment() != NULL);
}

void _load_bcd_segment(int num_pts, struct _Map_Segment ** seg ,
                       int * i , float * points )
{
  if (*i == 0){
    _allocate_first_segment( num_pts , seg );
    *i = 1;
  }
  else
    _allocate_segment(num_pts);
  
  _load_points(points);    
}

/*****************************************************************************
 *
 * Routine: _read_bcdfile
 *
 * Description: this routine reads in the bcdfile into memory
 *
 ****************************************************************************/

int _read_bcdfile( char * filename , struct _Map_Segment ** seg )
{
  FILE *fp = NULL ;
  float lat1,lat2,lon1,lon2;
  float llat,llon,rlat,rlon;
  int j = 1 ;
  int num_pts,i=0;
  int n;
  float * points = NULL ;
  OperSys os ;
  size_t number_of_elements = 1 ; 

  /* As of build OB2, FSL is now only providing the bcd overlay
     files in Big Endian byte ordering. If the application using this
     routine is running on Linux, then the bytes in the bcd file
     being read must first be "flipped" so that they can be 
     correctly interpretted and displayed. */
  os = GetOS ( ) ;

  fp = fopen(filename,"r");

  if ( fp == NULL ) {
    fprintf ( stderr , "\nIn routine '_read_bcdfile':\n"
                       "Error: File %s does not exist.  Cannot read "
                       "coordinates\n" , filename ) ;
    return 0;
  }
  else{
    _get_region(&llat,&llon,&rlat,&rlon);

    while(fread(&num_pts,4,1,fp) > 0 ){
      
      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( & num_pts , & number_of_elements ) ;  
      }
      
      /* get boundary of area to be drawn */
      if ( fread(&lat1,4,1,fp)!=1 ) 
      {
	return 0;
      }

      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( ( void * ) & lat1 , & number_of_elements ) ;  
      }
      
      if (fread(&lat2,4,1,fp)!=1) 
      {
	return 0;
      }

      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( ( void * ) & lat2 , & number_of_elements ) ;  
      }
      
      if (fread(&lon1,4,1,fp)!=1) 
      {
	return 0;
      }

      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( ( void * ) & lon1 , & number_of_elements ) ;  
      }
      
      if (fread(&lon2,4,1,fp)!=1) 
      {
         return 0;
      }

      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( ( void * ) & lon2 , & number_of_elements ) ;  
      }

      /* get coordinates */
      n=num_pts*2;
      points = (float *)malloc(sizeof(float) * n);

      if ( points == NULL )
      {
         fprintf ( stderr , "In routine \"_read_bcdfile\":\n"
                         "Cannot allocate %d bytes of memory for\n"
                         "\"points\". N is %d. Size of float is %d.\n"
                         "Pass = %d. Filename = %s.\n" ,
                         n * sizeof ( float ) , n , 
                         sizeof ( float ) , j , filename  ) ;
         return 0  ;
      }

      fread ( points , 8 , num_pts , fp ) ;
       
      if ( os == OS_LINUX )
      {
         Swap4Bytes_ ( ( int * ) points , ( const size_t * ) & n ) ;  
      }

  
      if (llat > -998.0){
	if ((llat >= lat1 && lat1 >= rlat) || (llat >= lat2 && lat2 >= rlat)){
	  if (llon < rlon){
	    if ((llon <= lon1 && lon1 <= rlon) || 
		(llon <= lon2 && lon2 <= rlon)) {
	      _load_bcd_segment(num_pts,seg,&i,points);
	    }
	  }
	  else{
	    if ((llon < lon1 && lon1 <= 180.0) || 
		(-180.0 <= lon1 && lon1 <= rlon) ||
		(llon < lon2 && lon2 <= 180.0) || 
		(-180.0 <= lon2 && lon2 <= rlon)){
	      _load_bcd_segment(num_pts,seg,&i,points);
	    }
	  }
	}
      }
      else
	_load_bcd_segment(num_pts,seg,&i,points);

      free(points);

     ++ j ;
    }

    fclose ( fp ) ;

  } 

  return ( 1 );
}


