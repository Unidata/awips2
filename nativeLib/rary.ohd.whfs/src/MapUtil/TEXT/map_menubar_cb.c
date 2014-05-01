/*****************************************************************************
 *
 * Module: map_menubar_cb.c
 *
 * Description: contains routines that deal with the menubar
 *
 *****************************************************************************/

/* include files */

#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>

#include "GeneralUtil.h"
#include "map.h"
#include "map_bcdfile.h"
#include "map_convert.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "map_shapefile.h"
#include "Xtools.h"

#define NUM_OVERLAYS 50
#define MAX_RECORD_LENGTH 1024
#define MAX_REPLY_LEN 1024
#define MAX_USER_OVERLAYS 21

/* global variables */
char * bcd_data_dir = "bcd_data_dir" ; 
char * shape_data_dir = "shape_data_dir" ;

/* flags for overlays */

struct _Map_Point _map_point ;

struct _Map_Segment _map_segment ;

struct _Overlay overlays [ M_OVERLAYS ] ;

//Added by Ram
//------------
int user_overlays_counter = 0;
struct _Overlay user_overlays [MAX_USER_OVERLAYS] ;
char overlay_names_local[NUM_OVERLAYS][MAX_RECORD_LENGTH];
//------------

struct _RIVERS rivers [ 121 ] ;
  
/* The widget used for the File Selection Dialog Box. */
Widget dialog  = NULL ;

/* The filename constructed from the File Selection Dialog Box. */
char * filename = NULL ;
char region[6];

int region_type;


struct _Map_Segment *cur_seg;
int recenter_flag=M_OFF;

/* If a change is made to the contents of this array, the enum MapOverlays
   data type in map_defines.h must also be changed. */
const char * OverlayNames [ ] = { "M_TOPOGRAPHY", "M_STATE" , "M_BASINS" , 
                                  "M_CITY_TOWN" , "M_COUNTY" , "M_CWA" , 
                                  "M_HIGHWAYS" , "M_ROADS" , "M_NOHIGHWAYS" ,
                                  "M_HRAP_GRID" , "M_LAKES" , "M_RIVERS" ,
                                  "M_STREAMS" , "M_NOSTREAMS" ,
                                  "M_LAT_LON_LINES" , "M_RADAR_LOCATIONS" ,
                                  "M_RADAR_RINGS" , "M_RFC_BOUNDARY" ,
                                  "M_TIMEZONE" , "M_ZONES" , 
                                  "M_TOPOGRAPHY_CONTOUR", 
                                  "M_TOPOGRAPHY_NONE", "M_OVERLAYS" } ; 

/* If a change is made to the contents of this array, the enum MapOverlayTypes
   data type in map_defines.h must also be changed. */
const char * MapOverlayTypes [ ] = { "M_SHAPE" , "M_BCD" , "M_CALCULATE" ,
                                     "M_EXTERNAL" , "M_BITMAP" ,
                                     "M_OVERLAY_END" } ;

/*****************************************************************************
 *
 * Routine: _set_first_segment
 *
 * Description: this routine set the head of the segment for the given overlay 
 *    to the current segment
 *
 ****************************************************************************/

void _set_first_segment( struct _Map_Segment * seg )
{
  cur_seg = seg;
}


void set_user_overlays_counter_and_names(int t, char overlay_names[NUM_OVERLAYS][MAX_RECORD_LENGTH])
{
	user_overlays_counter = t;
	int i,j;
	
	for(i=0;i<NUM_OVERLAYS;i++)
	{
	   for(j=0;j<MAX_RECORD_LENGTH;j++)
	   {
	      overlay_names_local[i][j] = overlay_names[i][j];
	   }
	}
}

/*****************************************************************************
 *
 * Routine: _next_segment
 *
 * Description: set current segment to next item in link list and return.
 *
 ****************************************************************************/

struct _Map_Segment * _next_segment ( )
{
  cur_seg = cur_seg->next ;
  return ( cur_seg ) ;
}

/*****************************************************************************
 *
 * Routine: _get_point_coordinate
 *
 * Description: get the current segment lat lon coordinate for the given point
 *
 ****************************************************************************/

void _get_point_coordinate(int item,float *lat,float *lon)
{
  *lat = cur_seg->points[item].lat;
  *lon = cur_seg->points[item].lon;
}

/*****************************************************************************
 *
 * Routine: _get_points_in_segment
 *
 * Description: get the number of points in the current segment
 *
 ****************************************************************************/

int _get_points_in_segment()
{
  return(cur_seg->num_points);
}

/*****************************************************************************
 *
 * Routine: _allocate_first_segment
 *
 * Description: allocate segment and points for the first segment for the given
 *  overlay
 *
 ****************************************************************************/

int _allocate_first_segment( int num , struct _Map_Segment ** seg )
{
  * seg = (struct _Map_Segment *) malloc(sizeof(_map_segment));

  if ( * seg == NULL )
  {
     fprintf ( stderr , "\nIn routine \"_allocate_first_segment\":\n"
                        "Could not allocate %d bytes for the segment.\n" ,
                        sizeof ( _map_segment ) ) ;
     return -1 ;
  }

  ( * seg )->num_points = num;
  ( * seg )->points = (struct _Map_Point *) malloc(sizeof(_map_point) * num);
 
  if ( ( * seg )->points == NULL )
  {
     fprintf ( stderr , "\nIn routine \"_allocate_first_segment\":\n"
                        "Could not allocate %d bytes for the segment.\n" ,
                        sizeof ( _map_segment ) ) ;
  }

  ( *seg )->next = NULL;
  cur_seg =*seg;
  
  if ( ( * seg)->points == NULL )
    return -1;
  else
    return 1;
}

/*****************************************************************************
 *
 * Routine: _allocate_segment 
 *
 * Description: allocate points for the next segment in the link list
 *
 ****************************************************************************/

int _allocate_segment(int num)
{
  cur_seg->next = (struct _Map_Segment *) malloc(sizeof(_map_segment));

  if ( cur_seg->next == NULL )
  {
     fprintf ( stderr , "\nIn routine \"_allocate_segment\":\n"
                        "Could not allocate %d bytes for \"cur_seg->next\".\n" ,
                        sizeof ( _map_segment ) ) ;
     return -1 ;
  }

  cur_seg = cur_seg->next;
  cur_seg->num_points = num;
  cur_seg->points = (struct _Map_Point *) malloc(sizeof(_map_point) * num);

  if ( cur_seg->points == NULL )
  {
     fprintf ( stderr , "\nIn routine \"_allocate_segment\":\n"
                        "Could not allocate %d bytes for "
                        "\"cur_seg->points\".\n" ,
                        num * sizeof ( _map_point ) ) ;
  }

  cur_seg->next = NULL;

  if (cur_seg->points == NULL)
    return(-1);
  else
    return(1);
}

/*****************************************************************************
 *
 * Routine: _deallocate_segments
 *
 * Description: Deallocates all of the points in all of the segments of the
 *              linked list.
 *
 ****************************************************************************/
void _deallocate_segments ( struct _Map_Segment ** seg )
{
   struct _Map_Segment * temp = NULL ;
   
   if ( seg != NULL )
   {
      temp = * seg ;

      while ( temp != NULL )
      {
         * seg = temp->next ;
         if ( temp->num_points > 0 )
         {
            if ( temp->points != NULL ) free ( temp->points ) ;
            temp->points = NULL ;
         }

         free ( temp ) ;
         temp = * seg ;
      }
   }
}
 
/*****************************************************************************
 *
 * Routine: _load_points
 *
 * Description: load the given points into the current segment
 *
 ****************************************************************************/

void _load_points(float *pts)
{
  int i,j=0;

  for(i=0;i<cur_seg->num_points;i++){
    cur_seg->points[i].lat = pts[j];
    j++;
    cur_seg->points[i].lon = pts[j];
    j++;
  }
}

/*****************************************************************************
 *
 * Routine: _load_coords
 *
 * Description: load the given coordinates into the current segment
 *
 ****************************************************************************/

void _load_coords ( double * lat , int negate_lat , double * lon ,
                    int negate_lon )
{
  int i ;

  for ( i = 0 ; i < cur_seg->num_points ; i++ ) 
  {
    if ( negate_lat == 0 )
    {
       cur_seg->points[i].lat = ( float ) lat [ i ] ;
    }
    else
    {
       cur_seg->points[i].lat = ( float ) lat [ i ] * -1.0 ;
    }
   
    if ( negate_lon == 0 )
    {
       cur_seg->points[i].lon = ( float ) lon [ i ] ;
    }
    else
    {
       cur_seg->points[i].lon = ( float ) lon [ i ] * -1.0 ;
    }

  }

}

/*****************************************************************************
 *
 * Routine: mSetOverlayType
 *
 * Description: this routine sets the overlay type and returns 1 if sets type
 *   returns -1 if the type is not valid for the overlay.
 *
 ****************************************************************************/

int mSetOverlayType ( int overlay , enum MapOverlayTypes type )
{
  if(overlay < 0)
  {
  	overlay *= -1;
	user_overlays [ overlay-1 ].type =  ( int ) type ;
  }
  else
  {
  	overlays [ overlay ].type =  ( int ) type ;
  }
  return M_OK ;
}

/*****************************************************************************
 *
 * Routine: mSetOverlayType
 *
 * Description: Returns a point to the overlay structure for the specified
 *              overlay type. 
 *
 ****************************************************************************/
struct _Overlay * mGetOverlay ( int overlay ) 
{
   if(overlay < 0)
   {
   	overlay *= -1;
	return & user_overlays [ overlay-1 ] ;
   }
   else
   {
   	return & overlays [ overlay ] ;
   }
}

/*****************************************************************************
 *
 * Routine: mSetMemoryFlag
 *
 * Description: This routine sets the "store-in-memory" flag for an overlay.
 *
 ****************************************************************************/

int mSetMemoryFlag ( int overlay , enum MapState store_in_memory )
{
   if(overlay < 0)
   {
   	overlay *= -1;
	user_overlays [ overlay-1 ].store_in_memory = ( int ) store_in_memory ;
   }
   else
   {
   	overlays [ overlay ].store_in_memory = ( int ) store_in_memory ;
   }
   return M_OK ;
}

/*****************************************************************************
 *
 * Routine: mSetOverlayCalcRoutine
 *
 * Description: This routine sets the user-supplied routine that can be
 *              used to calculate and draw an overlay. 
 *
 ****************************************************************************/

int mSetOverlayCalcRoutine ( int overlay , 
                             OverlayCalculatorRoutine pCalcRoutine )
{
   if ( pCalcRoutine != NULL )
   {
      if(overlay < 0)
      {
      	overlay *= -1;
	user_overlays [ overlay-1 ].pCalculateOverlay = pCalcRoutine ;
      }
      else
      {
      	overlays [ overlay ].pCalculateOverlay = pCalcRoutine ;
      }
      return M_OK ;
   }
   else
   {
      return M_ERROR ;
   }
}

/*****************************************************************************
 *
 * Routine: mGetOverlayCalcRoutine
 *
 * Description: This routine returns the user-supplied routine that can be
 *              used to calculate and draw an overlay. 
 *
 ****************************************************************************/
OverlayCalculatorRoutine mGetOverlayCalcRoutine ( int overlay )
{
    if(overlay < 0)
    {
    	overlay *= -1;
	return user_overlays [ overlay-1 ].pCalculateOverlay ;
    }
    else
    {
    	return overlays [ overlay ].pCalculateOverlay ;
    }
}

/*****************************************************************************
 *
 * Routine: mSetOverlayExternalRoutine
 *
 * Description: This routine sets the user-supplied routine that can be
 *              used to calculate and draw an overlay. 
 *
 ****************************************************************************/

int mSetOverlayExternalRoutine ( int overlay , 
                                 ExternalOverlayRoutine pExternalRoutine )
{

   if ( pExternalRoutine != NULL )
   {
      if(overlay < 0)
      {
      	overlay *= -1;
	user_overlays [ overlay-1 ].pExternalOverlay = pExternalRoutine ;
        return M_OK ;
      }
      else
      {
      	overlays [ overlay ].pExternalOverlay = pExternalRoutine ;
	return M_OK ;
      }
   }
   else
   {
      return M_ERROR ;
   }

}

/*****************************************************************************
 *
 * Routine: mDefineOverlay
 *
 * Description: This routine completely defines an overlay.
 *
 ****************************************************************************/
int mDefineOverlay ( int overlay , 
                     enum MapState state , 
                     char * color , 
                     int line_width ,
                     char ** filepath , char ** filename ,
                     int num_files,
                     enum MapOverlayTypes type ,
                     enum MapState store_in_memory , 
                     enum MapState fillarea ,
                     OverlayCalculatorRoutine pCalcRoutine ,
                     ExternalOverlayRoutine pExternalRoutine )
{
    int status ;  /* Indicates whether or not a problem is encountered
                     while defining the overlay.  A value of "1" indicates
                     that an error was encountered while filling this
                     structure and the structure is incomplete. */

    /* Call the init_overlays routine to ensure that the overlays
       have been properly initialized.  This routine is setup up
       to that initialization is only attempted once. */
    _init_overlays ( ) ;

    /* Set the status representing whether or not the overlay is initialially
       displayed. */
    
    if(overlay < 0)
    {
    	overlay *= -1;
	user_overlays[overlay-1 ].status = (int) state;
    	overlay *= -1;
    }
    else
    {
    	overlays [ overlay ].status = ( int ) state ;
    }

    /* Set the color. */
    status = mSetOverlayColor ( overlay , color ) ;

    if ( status != M_OK )
    {
       return status ; 
    }

    if(overlay < 0)
    {
    	overlay *= -1;
	user_overlays [ overlay-1 ].line_width = line_width ;
    	overlay *= -1;
    }
    else
    {
    	overlays [ overlay ].line_width = line_width ;
    }

    /* Set the type of the file. */
    status = mSetOverlayType ( overlay , type ) ;

    if ( status != M_OK )
    {
       return status ;
    }

    /* Copy the "store in memory flag". */
    status = mSetMemoryFlag ( overlay , store_in_memory ) ;

    if ( status != M_OK )
    {
       return status ;
    }

    /* Set the option to fill the polygon.  Note that this
       option is ignored while drawing vectors and lines. */

    if(overlay < 0)
    {
    	if ( fillarea == M_ON )
    	{
       		overlay *= -1;
		user_overlays [ overlay-1 ].fillarea = True ;
    	        overlay *= -1;
    	}
    }
    else
    {
    	if ( fillarea == M_ON )
	{
		overlays [ overlay ].fillarea = True ;
	}
    }

    /* Set the user-supplied routine that calculates and draws an
       overlay. */
    if ( type == M_CALCULATE )
    {
       status = mSetOverlayCalcRoutine ( overlay , pCalcRoutine ) ;

       if ( status != M_OK )
       {
          return status ;
       }
    } 

    /* Set the user-supplied routine that reads an overlay from a
       database table or set of tables. */
    if ( type == M_EXTERNAL )
    {
       status = mSetOverlayExternalRoutine ( overlay , pExternalRoutine ) ;

       if ( status != M_OK )
       {
          return status ;
       }
    } 

    /* Only attempt to set the file overlay information if
       the user has specified that there are files to process. */ 
    if ( num_files > 0 )
    {
       /* Set the bcd filepath(s) and name(s). */
       status = mSetFileInfo ( overlay , num_files , filepath ,
                                  filename ) ;

       if ( status != M_OK )
       {
          return status ;
       }
    }

    return M_OK ;
}

/*****************************************************************************
 *
 * Routine: _set_recenter_flag
 *
 * Description: this routine sets the recenter_flag variable
 *
 ****************************************************************************/

void _set_recenter_flag(int status)
{
  recenter_flag = status;
}

/*****************************************************************************
 *
 * Routine: _get_recenter_flag
 *
 * Description: this routine returns the recenter_flag variable
 *
 ****************************************************************************/

int _get_recenter_flag()
{
  return(recenter_flag);

}

/*****************************************************************************
 *
 * Routine: _about_cb
 *
 * Description: This routine is the "callback" for the "About" item on the
 *              "Help" menu. 
 *
 ****************************************************************************/
void _about_cb(Widget wid,XtPointer client_data,XtPointer call_data)
{
   MotionCallback about_app_cb = NULL ;

   about_app_cb = _get_about_app_cb ( ) ;
   
   if ( about_app_cb != NULL )
   {
      about_app_cb ( wid , client_data , call_data ) ;
   }
}

/*****************************************************************************
 *
 * Routine: _recenter_cb
 *
 * Description: this routine sets the overlay color
 *
 ****************************************************************************/
void _recenter_cb(Widget wid,XtPointer client_data,XtPointer call_data)
{
  recenter_flag = M_ON;
  mSetCursor(M_SELECT);
}
/*****************************************************************************
 *
 * Routine: mSetRegion
 *
 * Description: this routine sets the overlay color
 *
 ****************************************************************************/

void mSetRegion(int type,char *reg)
{
  region_type = type;
  strcpy(region,reg);
}

/*****************************************************************************
 *
 * Routine: mSetOverlayColor
 *
 * Description: this routine sets the overlay color
 *
 ****************************************************************************/

int mSetOverlayColor ( int index , char * color )
{
    int length ;  /* Used to represent the lengths of "C" style strings. */

    if ( color != NULL )
    {
       length = strlen ( color ) ;

       if ( length > 0 && length < COLOR_NAME_LENGTH )
       {
          if(index < 0)
	  {
	  	index *= -1;
		strcpy ( user_overlays [ index-1 ].color , color ) ;
          	return M_OK ;
	  }
	  else
	  {
	  	strcpy ( overlays [ index ].color , color ) ;
		return M_OK ;
	  }
       }
    }
 
    return M_ERROR ;
}

/*****************************************************************************
 *
 * Routine: mGetOverlayColor
 *
 * Description: This routine returns the color for a user-specified overlay. 
 *
 *****************************************************************************/

const char * mGetOverlayColor ( int index )
{
    if(index < 0)
    {
    	index *= -1;
	return user_overlays [ index-1 ].color ;
    }
    else
    {
    	return overlays [ index ].color ;
    }
}

/*****************************************************************************
 *
 * Routine mGetOverlayLineWidth
 *
 * Description: This routine returns the line width for a user-specified overlay. 
 *
 *****************************************************************************/

int mGetOverlayLineWidth ( int index )
{
    if(index < 0)
    {
    	index *= -1;
	return user_overlays [ index-1 ].line_width ;
    }
    else
    {
    	return overlays [ index ].line_width ;
    }
}


/*****************************************************************************
 *
 * Routine: mSetBcdFileInfo
 *
 * Description: This routine sets the bcd file information for the 
 *              overlay being processed. 
 *
 ****************************************************************************/
int mSetFileInfo ( int overlay ,
                   int num_files ,
                   char ** filepath ,
                   char ** filename )
{ 
   int flength ; /* Contains the length of the current filename being
                    processed. */
   int i ;       /* Used as a loop index variable. */
   int plength ; /* Contains the length of the current pathname being
                    processed. */

   if ( num_files <= 0 || num_files > MAX_NUMBER_OF_FILES )
   {
       return M_ERROR ;
   }

   for ( i = 0 ; i < num_files ; i ++ )
   {
      if ( ( filepath [ i ] == NULL ) ||
           ( filename [ i ] == NULL ) )
      {
         return M_ERROR ;
      }

      plength = strlen ( filepath [ i ] ) ;
      flength = strlen ( filename [ i ] ) ;

      if ( ( plength <= 0 || plength >= OVERLAY_PATH_LEN ) ||
           ( flength <= 0 || flength >= OVERLAY_FILENAME_LEN ) )
      {
         return M_ERROR ;
      }

      if(overlay < 0)
      {
      	overlay *= -1;
	strcpy ( user_overlays [ overlay-1 ].filepath [ i ] , filepath [ i ] ) ;
      	strcpy ( user_overlays [ overlay-1 ].filename [ i ] , filename [ i ] ) ;
	overlay *= -1;
      }
      else
      {
      	strcpy ( overlays [ overlay ].filepath [ i ] , filepath [ i ] ) ;
	strcpy ( overlays [ overlay ].filename [ i ] , filename [ i ] ) ;
      }
   }

   if(overlay < 0)
   {
   	overlay *= -1;
	user_overlays [ overlay-1 ].n_files = num_files ;
   }
   else
   {
   	overlays [ overlay ].n_files = num_files ;
   }

   return M_OK ;
}

/******************************************************************************
 *
 * Routine: _draw_shapefile_overlay
 * 
 * Description: this routine draw overlay on pixmap with given color.
 *
 *****************************************************************************/

static void _draw_shapefile_overlay ( char * color ,
                                      int    line_width ,
                                      char * pname ,
                                      char * fname ,
                                      struct _Map_Segment ** seg ,
                                      Pixmap map ,
                                      int store_in_memory ,
                                      enum ShapeFileType * shape_type ,
                                      Boolean fillarea )
{
  char name [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;

  memset ( name , '\0' , OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ) ;

  if ( ( pname != NULL ) && ( pname [ 0 ] != '\0' ) )
  {
     sprintf ( name , "%s/" , pname ) ;
  }

  if ( ( fname != NULL ) && ( fname [ 0 ] != '\0' ) )
  {
     strcat ( name , fname ) ; 
  }
  else
  {
     * seg = NULL ;
     return ;
  }

  _set_foreground ( color ) ;
  mSetLineWidth ( line_width ) ;

  _draw_shapefile ( name , 
                    seg ,
                    map ,
                    store_in_memory ,
                    shape_type ,
                    fillarea ) ;
  mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;
}

/******************************************************************************
 *
 * Routine: _draw_bcdfile_overlay
 * 
 * Description: this routine draw overlay on pixmap with given color.
 *
 *****************************************************************************/

void _draw_bcdfile_overlay ( char * color , 
                             int line_width , 
                             const char * fpath ,
                             const char * fname ,
                             struct _Map_Segment ** seg ,
                             Pixmap map ,
                             enum MapState store_in_memory ,
                             Boolean fillarea )
{
  char name [ OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ] ;

  memset ( name , '\0' , OVERLAY_PATH_LEN + OVERLAY_FILENAME_LEN ) ;

  if ( ( fpath != NULL ) && ( fpath [ 0 ] != '\0' ) )
  {
     sprintf ( name , "%s/" , fpath ) ;
  }

  if ( ( fname != NULL ) && ( fname [ 0 ] != '\0' ) )
  {
     strcat ( name , fname ) ;
  }
  else
  {
      * seg = NULL ;
      return ;
  }

  _set_foreground ( color ) ;
  mSetLineWidth ( line_width ) ;
  
  if ( * seg == NULL )
  {
    if ( _read_bcdfile ( name , seg ) == 0 )
      return;
  }

  if ( * seg != NULL )
  {
     _draw_points ( map , * seg , fillarea ) ;

     if ( store_in_memory == M_OFF )
     {
        _deallocate_segments ( seg ) ;
        * seg = NULL ;
     } 
  }

  mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

}

/******************************************************************************
 *
 * Routine: _draw_rivers
 * 
 * Description: this routine draw the rivers
 *
 *****************************************************************************/

void _draw_rivers(Pixmap pix)
{
  int i=0,k,hit;

/* draw rives for wfo */

  if (region_type == M_WFO){
    while(strcmp(region,rivers[i].wfo) != 0 && i < 121)
      i++;

    if (i == 121)
      printf("Error: Invalid region (%s)\n",region);
    else{
      	if (overlays[M_LAKES].type == M_SHAPE)
      	{
         	_draw_shapefile_overlay ( overlays[M_LAKES].color,
                                   overlays[M_LAKES].line_width ,
                                   rivers[i].shape_filepath ,
                                   rivers[i].shape_filename ,
                                   & ( rivers[i].seg ) ,
                                   pix ,
                                   rivers[i].store_in_memory ,
                                   ( enum ShapeFileType * ) 
                                     & ( rivers[i].type ) ,
                                    False ) ;
      	}
      	else
      	{
		 _draw_bcdfile_overlay ( overlays[M_LAKES].color ,
                                 overlays[M_LAKES].line_width ,
                                 rivers[i].bcd_filepath , 
                                 rivers[i].bcd_filename , 
                                 & ( rivers[i].seg ) ,
                                 pix ,
                                 rivers[i].store_in_memory ,
                                 False ) ;
      	}
     }
     
    }
  

  /* draw rivers in rfc */

  else if (region_type == M_RFC){
    for (i=0; i < 121; i++){
      hit = 0;
      for (k = 0; k < rivers[i].num_of_rfcs; k++){
	if (strcmp(rivers[i].rfc[k],region) == 0)
	  hit = 1;
      }

      if (hit == 1)
      {
         _draw_shapefile_overlay ( overlays[M_LAKES].color ,
                                   overlays[M_LAKES].line_width ,
                                   rivers[i].shape_filepath ,
                                   rivers[i].shape_filename ,
                                   & ( rivers[i].seg ) ,
                                   pix ,
                                   rivers[i].store_in_memory ,
                                   ( enum ShapeFileType * ) 
                                   & ( rivers[i].type ) ,
                                   False ) ;
      }
    } 
  }

  /* draw all the rivers */

  else{
    for (i=0;i < 121;i++)
         _draw_shapefile_overlay ( overlays[M_LAKES].color ,
                                   overlays[M_LAKES].line_width ,
                                   rivers[i].shape_filepath ,
                                   rivers[i].shape_filename ,
                                   & ( rivers[i].seg ) ,
                                   pix ,
                                   rivers[i].store_in_memory ,
                                   ( enum ShapeFileType * ) 
                                   & ( rivers[i].type ) ,
                                   False ) ;
  }
}


/*****************************************************************************
 *
 * Routine: _draw_overlay
 *
 * Description: this routine draws the given overlay 
 *
 ****************************************************************************/

static void _draw_overlay ( Pixmap map , int index , int item )
{
  int j ;

  switch ( overlays [ item].type )
  {
     case M_SHAPE : 

        for ( j = 0 ; j < overlays[item].n_files ; j++ )
        {
           _draw_shapefile_overlay ( overlays [ item].color ,
                                     overlays [ item].line_width ,
   	   		             overlays [ item].filepath [ j ] ,
   			             overlays [ item].filename [ j ] ,
                                     & ( overlays [ item].seg [ j ] ) ,
			             map ,
                                     overlays [ item].store_in_memory ,
                                     & ( overlays [ item].shape_type ) ,
                                     overlays [ item].fillarea ) ;
        }

        break ;

     case M_BCD :

        for ( j = 0 ; j < overlays[ item ].n_files ; j++ )
        {
           _draw_bcdfile_overlay ( overlays [ item].color ,
                                   overlays [ item].line_width ,
                                   overlays [ item].filepath [ j ] ,
                                   overlays [ item].filename [ j ] ,
                                   & ( overlays [ item].seg [ j ] ) ,
                                   map ,
                                   overlays [ item].store_in_memory ,
                                   overlays [ item].fillarea ) ;
        }

        break ;

     case M_CALCULATE :

        if ( overlays [ item].pCalculateOverlay != NULL )
        {
           overlays [ item].pCalculateOverlay ( index , & overlays [ item] ,
                                                 map , NULL ) ;
        }

        break ;

     case M_EXTERNAL:

        if ( ( overlays [ item].pExternalOverlay != NULL )
             && ( overlays [ item].seg [ 0 ] == NULL ) )
        {
           overlays [ item].pExternalOverlay (  & overlays [ item] , 
					 ( enum MapOverlays ) item ,
                                         & overlays [ item].seg [ 0 ] ,
                                         & ( overlays [ item].shape_type ) ,
                                         & ( overlays [ item].fillarea ) ) ;
        }

        /* Determine if there was any data for the overlay.  If there was
           not any, then return here. */
        if ( overlays [ item].seg [ 0 ] == NULL )
        {
           return ;
        }

        /* Set the drawing color */
        mSetColor ( overlays [ item].color ) ;

        /* Set the overlay line width. */
        mSetLineWidth ( overlays [ item].line_width ) ;
        
        _draw_shapefile_data ( overlays [ item].shape_type ,
                               map ,
                               overlays [ item].seg [ 0 ] ,
                               overlays [ item].fillarea ) ;

        if ( overlays [ item].store_in_memory == ( int ) M_OFF )
        {
           _deallocate_segments ( & ( overlays [ item].seg [ 0 ] ) ) ;
           overlays [ item].seg [ 0 ] = NULL ;
        }

	mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

        break ;

     default :
     
        fprintf ( stderr , "In routine _draw_overlay :\n"
                           "Unrecognized overlay type represented by\n"
                           "numeric code %d.\n\n" , overlays [ item].type ) ; 
        break ; 
  }
}
static void _draw_user_overlay ( Pixmap map , int index , int item )
{
  int j ;

  if(item < 0)
  {
     item *= -1;
  }

  switch ( user_overlays [ item-1].type )
  {
     case M_SHAPE : 

        for ( j = 0 ; j < user_overlays[item-1].n_files ; j++ )
        {
           _draw_shapefile_overlay ( user_overlays [ item-1 ].color ,
                                      user_overlays [ item-1 ].line_width ,
   	   		              user_overlays [ item-1 ].filepath [ j ] ,
   			              user_overlays [ item-1 ].filename [ j ] ,
                                     & (  user_overlays [ item-1 ].seg [ j ] ) ,
			             map ,
                                      user_overlays [ item-1 ].store_in_memory ,
                                     & (  user_overlays [ item-1 ].shape_type ) ,
                                      user_overlays [ item-1 ].fillarea ) ;
        }

        break ;

     case M_BCD :

        for ( j = 0 ; j <  user_overlays[ item-1 ].n_files ; j++ )
        {
           _draw_bcdfile_overlay (  user_overlays [ item-1 ].color ,
                                    user_overlays [ item-1 ].line_width ,
                                    user_overlays [ item-1 ].filepath [ j ] ,
                                    user_overlays [ item-1 ].filename [ j ] ,
                                   & (  user_overlays [ item-1 ].seg [ j ] ) ,
                                   map ,
                                    user_overlays [ item-1 ].store_in_memory ,
                                    user_overlays [ item-1 ].fillarea ) ;
        }

        break ;

     case M_CALCULATE :

        if (  user_overlays [ item-1 ].pCalculateOverlay != NULL )
        {
            user_overlays [ item-1 ].pCalculateOverlay ( index , & user_overlays [ item-1 ] ,
                                                 map , NULL ) ;
        }

        break ;

     case M_EXTERNAL:

        if ( (  user_overlays [ item-1 ].pExternalOverlay != NULL )
             && (  user_overlays [ item-1 ].seg [ 0 ] == NULL ) )
        {
            user_overlays [ item-1 ].pExternalOverlay (  &  user_overlays [ item-1 ] , 
					 item ,
                                         &  user_overlays [ item-1 ].seg [ 0 ] ,
                                         & (  user_overlays [ item-1 ].shape_type ) ,
                                         & (  user_overlays [ item-1 ].fillarea ) ) ;
        }

        /* Determine if there was any data for the overlay.  If there was
           not any, then return here. */
        if (  user_overlays [ item-1 ].seg [ 0 ] == NULL )
        {
           return ;
        }

        /* Set the drawing color */
        mSetColor (  user_overlays [ item-1 ].color ) ;

        /* Set the overlay line width. */
        mSetLineWidth (  user_overlays [ item-1 ].line_width ) ;
        
        _draw_shapefile_data (  user_overlays [ item-1 ].shape_type ,
                               map ,
                                user_overlays [ item-1 ].seg [ 0 ] ,
                                user_overlays [ item-1 ].fillarea ) ;

        if (  user_overlays [ item-1 ].store_in_memory == ( int ) M_OFF )
        {
           _deallocate_segments ( & (  user_overlays [ item-1 ].seg [ 0 ] ) ) ;
            user_overlays [ item-1 ].seg [ 0 ] = NULL ;
        }

	mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH ) ;

        break ;

     default :
     
        fprintf ( stderr , "In routine _draw_overlay :\n"
                           "Unrecognized overlay type represented by\n"
                           "numeric code %d.\n\n" ,  user_overlays [ item-1 ].type ) ; 
        break ; 
  }
}



/*****************************************************************************
 *
 * Routine:     _draw_topography_overlay
 *
 * Description: This routine draws the topography overlay if it is on.
 *              The topography overlay is a special case.  It is an
 *              opaque overlay.  The user needs to call it specially
 *              to make sure that it is drawn in the correct order.
 ****************************************************************************/
void _draw_topography_overlay ( int index )
{
   Pixmap map = _get_map_pixmap ( ) ;

   if ( overlays[M_TOPOGRAPHY].status == M_ON )
   {
      _draw_overlay ( map , index , M_TOPOGRAPHY ) ;
   }
}



/*****************************************************************************
 *
 * Routine: _draw_map_overlays
 *
 * Description: this routine draws the overlays that are on
 *
 ****************************************************************************/

void _draw_map_overlays( int index )
{
  int i ;
  Pixmap map = _get_map_pixmap ( ) ;

  for ( i = 0 ; i < M_OVERLAYS ; i++ )
  {   
	if ( overlays[i].status == M_ON )
	{
		_draw_overlay ( map , index , i ) ;
	}
  }
  
  for(i = 0; i < user_overlays_counter; i++ )
  {
  	if ( user_overlays[i].status == M_ON )
  	{
      		_draw_user_overlay ( map , index , -1*(i+1)) ;
  	}
  }
}

/*****************************************************************************
 * Routine: _get_overlay_status
 *
 * Description: This routine returns the status of the overlay.  It determines
 * whether the overlay is toggled "on" or "off".
 *
 ****************************************************************************/

int _get_overlay_status ( int item )
{
   if(item < 0)
   {
        item *= -1;
   	return user_overlays [ item-1].status ;
   }
   else
   {
   	return overlays [ item ].status ;
   }
}

/*****************************************************************************
 * Routine: _set_overlay_status
 * Description: This routine sets the status of the overlay.  Warning!
 *              If you use this routine, it is your responsibility to
 *              make sure that the corresponding toggle button on the
 *              overlay menu is also set or unset depending on the
 *              overlay state. 
 ****************************************************************************/
void _set_overlay_status ( int item ,
                           enum MapState state )
{
   if(item < 0)
   {
        item *= -1;
   	user_overlays [ item-1].status = state ;
   }
   else
   {
   	overlays [ item ].status = state ;
   }
}
   

/*****************************************************************************
 *
 * Routine: _print_image
 *
 * Description: this routine is callback for print image button.  It prints
 * map image to the printer.
 *
 ****************************************************************************/
void _print_image(Widget wid,XtPointer client_data,XtPointer call_data)
{
   char buf [ MAX_MAP_STRING_SIZE ] ;
   char cmd_str [ MAX_MAP_STRING_SIZE ] ;
   char lpr_print [ MAX_MAP_STRING_SIZE ] ;
   char * title = NULL ;
   char whfs_bin_dir [ 128 ];
   int gad_token_len=0 , gad_value_len=0;
   int inverse = 0 ; /* Print the image in "normal" color. */

   /* Set the lpr_print and whfs_bin_dir "C" strings to contain
      null characters just to be safe. */
   memset ( lpr_print , '\0' , 128 ) ;
   memset ( whfs_bin_dir , '\0' , 128 ) ;

   /* Retrieve the title of the main screen to print out. */
   XtVaGetValues ( _get_map_shell ( ) , XmNtitle , &title , NULL ) ;

   /*token for the print command*/
   gad_token_len = strlen("whfs_printcommand");
   get_apps_defaults( "whfs_printcommand", &gad_token_len,lpr_print,
                      &gad_value_len );

   if(strlen(lpr_print) > 0)
   {
     strcpy(cmd_str,lpr_print);
   }
   else
   {
     strcpy(cmd_str, "lp");
   }

   /* token for the whfs bin directory. */
   gad_token_len = strlen ( "whfs_bin_dir" ) ;
   get_apps_defaults( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                      & gad_value_len ) ;

   /* Test for the success of the token retrieval. */
   if ( strlen ( whfs_bin_dir ) == 0 )
   {
      strcpy ( whfs_bin_dir , "." ) ;
   }

   /* This section of code was updated by Bryon Lawrence on
      November 2, 2001.  The "print_image" script is now called,
      and this script takes into account the differences between
      the HP-UX and Linux operating systems when it comes to
      capturing and processing images. */
   sprintf ( buf , "%s/print_image \"%s\" \"%s\" %d " , whfs_bin_dir ,
                   title , cmd_str , inverse ) ;

   mSetCursor ( M_WATCH ) ;
   system ( buf ) ;
   mSetCursor ( M_NORMAL ) ;
}

/*****************************************************************************
 *
 * Routine: _print_image_reverse
 *
 * Description: this routine is callback for print image reverse button.  It
 *  prints map image in reverse video.
 *
 ****************************************************************************/
void _print_image_reverse(Widget wid,XtPointer client_data,XtPointer call_data)
{
   char buf [ MAX_MAP_STRING_SIZE ] ;
   char cmd_str [ MAX_MAP_STRING_SIZE ] ;
   char lpr_print [ MAX_MAP_STRING_SIZE ] ;
   char * title = NULL ;
   char whfs_bin_dir [ 128 ];
   int gad_token_len=0 , gad_value_len=0;
   int inverse = 1 ; /* Print the image in "normal" color. */

   /* Set the lpr_print and whfs_bin_dir "C" strings to contain
      null characters just to be safe. */
   memset ( lpr_print , '\0' , 128 ) ;
   memset ( whfs_bin_dir , '\0' , 128 ) ;

   /* Retrieve the title of the main screen to print out. */
   XtVaGetValues ( _get_map_shell ( ) , XmNtitle , &title , NULL ) ;

   /*token for the print command*/
   gad_token_len = strlen("whfs_printcommand");
   get_apps_defaults( "whfs_printcommand", &gad_token_len,lpr_print,
                      &gad_value_len );

   if(strlen(lpr_print) > 0)
   {
     strcpy(cmd_str,lpr_print);
   }
   else
   {
     strcpy(cmd_str, "lp");
   }

   /* token for the whfs bin directory. */
   gad_token_len = strlen ( "whfs_bin_dir" ) ;
   get_apps_defaults( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                      & gad_value_len ) ;

   /* Test for the success of the token retrieval. */
   if ( strlen ( whfs_bin_dir ) == 0 )
   {
      strcpy ( whfs_bin_dir , "." ) ;
   }

   /* This section of code was updated by Bryon Lawrence on
      November 2, 2001.  The "print_image" script is now called,
      and this script takes into account the differences between
      the HP-UX and Linux operating systems when it comes to
      capturing and processing images. */
   sprintf ( buf , "%s/print_image \"%s\" \"%s\" %d " , whfs_bin_dir ,
                   title , cmd_str , inverse ) ;

   mSetCursor ( M_WATCH ) ;
   system ( buf ) ;
   mSetCursor ( M_NORMAL ) ;
}

/*****************************************************************************
 *
 * Routine: _save_image_cancel_cb
 *
 * Description: This is the callback for the "cancel" button on the
 * File Selection Dialog Box.  This routine "unmanages" the File Selection
 * Diaog GUI making it invisible.
 *
 ****************************************************************************/
static void _save_image_cancel_cb ( Widget wid , XtPointer client_data ,
                                    XtPointer call_data )
{
   if ( ( dialog != NULL ) &&  ( XtIsManaged ( dialog ) ) ) 
        XtUnmanageChild ( dialog ) ;
}

/*******************************************************************************
* MODULE NUMBER:  Not Applicable
* MODULE NAME:    _save_image_ok_cb
* PURPOSE:  This routine launches a file selection GUI to allow the user
*           to choose the file and directory he wants to save the 
*           captured screen image to.  This routine enforces directory and
*           file checking.  It verifies that directories exist and that
*           the user has permission to write to them.  If the user 
*           attempts to overwrite an existing file, a question dialog
*           is launched asking the user if he wishes to proceed. This
*           routine will also warn the user if he tries to overwrite a
*           read-only file or if he tries to write to file that is 
*           a directory.
* 
*           Note that this routine also serves as the callback to the 
*           "Ok" button on the question dialog.  When used in this context
*           all of the file and directory checking has already been 
*           done by this routine when it was previously called by
*           a XmNokCallback Event triggered from the File Selection 
*           Dialog Box. So, when called from the question dialog, all of
*           the file and directory checking is skipped and the image
*           file is created and saved in the user specified path.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Input  Widget      wid            The identifier of the widget the
*                                     event resulting in this callback
*                                     was detected in.      
*   Input  XtPointer   client_data    Any client-supplied data that was
*                                     passed into this routine as a 
*                                     result of the callback event.
*   Input  XtPointer   call_data      This is usually an event-specific
*                                     structure.  For example, when called
*                                     from a button on the File Selection
*                                     Dialog Box, the call data will consist
*                                     of a XmFileSelectionBoxCallbackStruct
*                                     structure.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                         HEADER FILE    DESCRIPTION
*   ErrorDialog                  Xtools.h       Builds a generic error dialog
*                                               popup GUI.
*   get_apps_defaults            GeneralUtil.h  Retrieves token values from the
*                                               environment and apps defaults
*                                               files.
*   QuestionDialogWithCallbacks  Xtools.h       Builds a generic question
*                                               dialog popup GUI.
*  ( Note that this routine relies heavily on Xmotif and Toolkit routines.
*    "access" is a system routine defined in "unistd.h". "stat" is a routine
*    that is defined in "sys/stat.h". )
*
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME           DESCRIPTION
*   char [ ]       buf           System commands are constructedi in this
*                                array before being passed to the system.
*   char [ ]       str           The text for dialog boxes is formatted into
*                                this array before being passed into the
*                                GUI building routines.
*   char *         dirname       Contains the name of the directory selected
*                                by the user.
*   char *         title         Contains the the title of the master viewer
*                                whose image is captured and saved
*   char [ ]       whfs_bin_dir  Contains the directory to the imconv routine.
*   int            gad_token_len The length of the whfs bin directory token
*                                name.
*   int            gad_value_len The length of the retrieved value of the 
*                                whfs bin directory token.
*   int            status        Contains return values from system routine
*                                calls for testing.
*   struct stat    file_info     A structure containing file information 
*                                including its type.
*   unsigned short mode          The mode of a file as retrieved from the 
*                                file_info structure.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* MANIFEST CONSTANTS AND MACROS: 
*   NAME                HEADER       DESCRIPTION
*   F_OK and W_OK       unistd.h     Bit masks for various file access tests. 
*   MAX_MAP_STRING_SIZE See Above    Defines the largest number of characters
*                                    the buf, str, and whfs_bin_dir arrays may
*                                    be. 
*   S_ISDIR             sys/stat.h   A macro to test whether or not a file is
*                                    a directory.
* ERROR HANDLING:
*   This routine does not return error codes per se.  However, this routine
*   will launch an error dialog if one of several conditions are met:    
*       1) The user is attempting to write to a file that is a directory.
*       2) The user is attempting to write to a file that does not have
*          write permissions.
*       3) The user is attempting to save a file to a directory that does
*          no have write permissions.
*       4) The user is attempting to save a file to a directory that does
*          not exist.
*
********************************************************************************
*/
static void _save_image_ok_cb ( Widget wid , XtPointer client_data ,
                                XtPointer call_data )
{
  char buf [ MAX_MAP_STRING_SIZE ] ; 
  char str [ MAX_MAP_STRING_SIZE ] , * dirname = NULL , * title = NULL ;
  char whfs_bin_dir [ MAX_MAP_STRING_SIZE ] ; 
  int gad_token_len = 0 ;
  int gad_value_len = 0 ;
  int status ;
  struct stat file_info ;
  unsigned short mode = 0 ;

  /* Determine if directory and file checking needs to be done. */ 
  if ( wid == dialog )
  {
     /* Retrieve the name of the file. */
     XmFileSelectionBoxCallbackStruct * cbs =
             ( XmFileSelectionBoxCallbackStruct * ) call_data ;

     /* Retrieve the name of the file from the callback structure. */
     if ( ! XmStringGetLtoR ( cbs->value , XmFONTLIST_DEFAULT_TAG ,
                              & filename ) ) return ;

     /* Retrieve the name of the directory from the callback structure. */
     if ( ! XmStringGetLtoR ( cbs->dir , XmFONTLIST_DEFAULT_TAG ,
                              & dirname ) ) return ;

     if ( *filename == '\0' )
     {
        XtFree ( filename ) ; /* Even "" is an allocated byte. */
        filename = NULL ;
        return ;
     }

     /* A file name was selected. Check to see if it exists.  If it does,
        then does the user have permission to overwrite it? */
     status = access ( filename , F_OK ) ;

     if ( status == 0 )
     {
        /* The file exists. Test to make sure it is not a directory. */
        status = stat ( filename , & file_info ) ;

        if ( status != 0 )
        {
           fprintf ( stderr , "In _save_image: Error encountered by \"stat\"\n"
                              "while processing file %s.\n" , filename ) ;
           XtFree ( filename ) ;
           filename = NULL ;
           if ( dirname != NULL ) XtFree ( dirname ) ;
           return ;
        }

        mode = file_info.st_mode ;

        /* S_ISDIR ( ) is a macro defined in sys/stat.h. */
        if ( S_ISDIR ( mode ) )
        {
           /* Launch an error dialog stating that the file exists and the
              user cannot overwrite it. */
           sprintf ( str , "Cannot write to %s. It is a directory.\n" , 
                     filename ) ;
           ErrorDialog ( wid , str ) ; 
           XtFree ( filename ) ;
           filename = NULL ;
           if ( dirname != NULL ) XtFree ( dirname ) ;
           return ;

        }

        /* The file exists. Test for writeability. */
        status = access ( filename , W_OK ) ;

        if ( status != 0 )
        {
           /* Launch an error dialog stating that the file exists and the
              user cannot overwrite it. */
           sprintf ( str , "Cannot overwrite file %s.\n" , filename ) ;
           ErrorDialog ( wid , str ) ; 
           if ( filename != NULL )
           {
              XtFree ( filename ) ;
              filename = NULL ;
           }

           if ( dirname != NULL ) XtFree ( dirname ) ;
           return ;
        }

        /* Prompt the user to make sure he wants to overwrite the file. */
        sprintf ( str , "Are you sure you want to overwrite file %s?\n" ,
                  filename ) ;
        QuestionDialogWithCallbacks ( wid , str , "Overwrite File?\n" ,
                                      _save_image_ok_cb , NULL ) ; 
        return ;
     }
     else
     {
        /* Test to make sure that the directory the user is attempting to
           write to exists and is writeable. */
           status = access ( dirname , F_OK ) ;

        if ( status != 0 )
        {
           /* Launch an error dialog stating that the file exists and the
              user cannot overwrite it. */
           sprintf ( str , "Directory %s does not exist.\n" , dirname ) ;
           ErrorDialog ( wid , str ) ;
           if ( filename != NULL )
           {
              XtFree ( filename ) ;
              filename = NULL ;
           }

           if ( dirname != NULL ) XtFree ( dirname ) ;
           return ;
        }
        else
        {
           /* Check to determine if the user can write to the directory. */
           status = access ( dirname , W_OK ) ;
        
           if ( status != 0 )
           {
              sprintf ( str , "Cannot write to directory %s.\n" , dirname ) ;
              ErrorDialog ( wid , str ) ;

              if ( filename != NULL ) 
              {
                 XtFree ( filename ) ;
                 filename = NULL ;
              }

              if ( dirname != NULL ) XtFree ( dirname ) ;
              return ;
           }
        } 

     }
  }
  
  /* Retrieve the name of the main viewer GUI. */
  XtVaGetValues ( _get_map_shell ( ) , XmNtitle , &title , NULL ) ;

  /* Retrieve the value of the whfs_bin_dir token. */
  memset ( whfs_bin_dir , '\0' , MAX_MAP_STRING_SIZE ) ;
  gad_token_len = strlen ( "whfs_bin_dir" ) ;
  get_apps_defaults ( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                       & gad_value_len ) ;

  /* Test for the success of the token retrieval. */
  if ( strlen ( whfs_bin_dir ) == 0 )
  {
     strcpy ( whfs_bin_dir , "." ) ;
  }

  /* This section of code was updated by Bryon Lawrence on
     November 2, 2001.  The "save_image" script is now called,
     and this script takes into account the differences between
     the HP-UX and Linux operating systems when it comes to
     capturing and processing images. */
  sprintf ( buf , "%s/save_image \"%s\" \"%s\" " , whfs_bin_dir ,
            title , filename ) ;
  system(buf);

  /* Shut down the File Selection Dialog Box. */
  if ( dialog != NULL ) XtUnmanageChild ( dialog ) ;
  
  if ( filename != NULL )
  {
     XtFree ( filename ) ;
     filename = NULL ;
  }

  if ( dirname != NULL ) XtFree ( dirname ) ;

  return ;
}

/*******************************************************************************
* MODULE NUMBER:  Not Applicable
* MODULE NAME:    _save_image
* PURPOSE:       This routine is the callback for the "save as gif" item under 
*                the "File" menu pulldown.  It creates a File Selection Dialog
*                Box GUI through which the user can select a directory name
*                and a file name to save the caputed gif image to.  Right now,
*                the user must be sure that the filename he wishes to save the
*                image has a *.gif extension on it.  Otherwise, the routine 
*                that creates the image file will not know what graphical 
*                format to save it as.
*            
*                The File Selection Dialog Box is created with application
*                modality, meaning that the user cannot access any other
*                hydromap viewer features while the File Selection Box is
*                open.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  Widget      wid                  The id of the parent widget
*                                           launching the File Selection
*                                           Dialog Box.
*   Input  XtPointer   client_data          Any user supplied arguments
*                                           to this callback routine.
*   Input  XtPointe    call_data            Usually contains a structure
*                                           defined by Xmotif that relates
*                                           to the event that is triggering 
*                                           this callback routine. 
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   This routine calls only Xmotif, Xtoolkit, and system routines.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None ( dialog is a global variable defined at the top of this file.)
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void _save_image ( Widget wid , XtPointer client_data , XtPointer call_data )
{
   Arg   arg [ 1 ] ;
   char reply [ MAX_REPLY_LEN ] ;
   static char * whfs_image_dir_token = "whfs_image_dir" ;
   int ac ;
   int reply_len ;
   int request_len ;
   int status ;
   XmString directory ;
   XmString pattern ;

   /* Manage the widget. */
   if ( dialog == NULL )
   {
      ac = 0 ;
      XtSetArg ( arg [ ac ] , XmNtitle , "Save as GIF" ) ; ++ ac ;

      /* Retrieve the default directory to save the images in. */
      request_len = strlen ( whfs_image_dir_token ) ;
      status = get_apps_defaults ( whfs_image_dir_token , 
                                   & request_len ,
                                   reply ,
                                   & reply_len ) ;
   
      if ( status != 0 )
      {
         fprintf ( stderr , "\nIn routine _save_image:\n"
                            "Could not retrieve the directory represented\n"
                            "by the token \"%s\".  Could not set the default\n"
                            "directory mask.\n" , whfs_image_dir_token ) ;
      }
      else
      {
         /* Generate the default pattern to be used in the directory mask. */ 
         directory = XmStringCreateLocalized ( reply ) ;
         pattern = XmStringCreateLocalized ( "*.gif" ) ;

         /* Launch a FileSelectionDialog GUI. */
         dialog = FileDialog ( wid ,
                               arg ,
                               ac ,
                               _save_image_ok_cb ,
                               NULL ,
                               _save_image_cancel_cb ,
                               NULL ,
                               NULL ,
                               NULL ) ;
      
         XtVaSetValues ( dialog , XmNtitle , "Save as GIF" , NULL ) ;
         XtVaSetValues ( dialog , XmNdirectory , directory , NULL ) ;
         XtVaSetValues ( dialog , XmNpattern , pattern , NULL ) ;
         XmStringFree ( directory ) ;
         XmStringFree ( pattern ) ;
      }
   }

   if ( ! XtIsManaged ( dialog ) ) XtManageChild ( dialog ) ;
}

/*****************************************************************************
 *
 * Routine: _set_overlay
 *
 * Description: this routine is callback for overlay toggle buttons.  It sets
 * the status of the overlay and updates the maps.
 *
 ****************************************************************************/

void _set_overlay(Widget wid,XtPointer item,XtPointer call_data)
{

  int k , num_of_maps , index = ( int ) item ;
  Widget rivers_toggle ;
  Widget streams_toggle ;
  Widget no_streams_rivers_toggle ;

  Widget highways_and_roads_toggle ;
  Widget highways_toggle ;
  Widget no_highways_and_roads_toggle ;

  Widget topo_toggle ;
  Widget topo_contour_toggle ;
  Widget no_topo_toggle ;

  XmToggleButtonCallbackStruct * state = 
  ( XmToggleButtonCallbackStruct * ) call_data ;
  
  num_of_maps = _get_num_of_maps ( );

  if ( state->set )
  {
     if ( index == ( int ) M_RIVERS )
     {
        _set_overlay_status ( M_LAKES , M_ON ) ;
        _set_overlay_status ( M_STREAMS , M_OFF ) ;
        streams_toggle = _get_menu_item_toggle_widget ( map_overlay_streams ) ; 
        XmToggleButtonSetState ( streams_toggle , False , False ) ;
        no_streams_rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_no_streams ) ;
        XmToggleButtonSetState ( no_streams_rivers_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_STREAMS )
     {
        _set_overlay_status ( M_LAKES , M_ON ) ;
        _set_overlay_status ( M_RIVERS , M_OFF ) ;
        rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_rivers ) ; 
        XmToggleButtonSetState ( rivers_toggle , False , False ) ;
        no_streams_rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_no_streams ) ;
        XmToggleButtonSetState ( no_streams_rivers_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_NOSTREAMS )
     {
        _set_overlay_status ( M_LAKES , M_OFF ) ;
        _set_overlay_status ( M_STREAMS , M_OFF ) ;
        _set_overlay_status ( M_RIVERS , M_OFF ) ;
        streams_toggle = _get_menu_item_toggle_widget ( map_overlay_streams ) ; 
        XmToggleButtonSetState ( streams_toggle , False , False ) ;
        rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_rivers ) ; 
        XmToggleButtonSetState ( rivers_toggle , False , False ) ;
     }
     
     else if ( index == ( int ) M_ROADS ) 
     {  
         _set_overlay_status ( M_ROADS , M_ON ) ;
        _set_overlay_status ( M_HIGHWAYS , M_OFF ) ;
        _set_overlay_status ( M_NOHIGHWAYS , M_OFF ) ;
        highways_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_only ) ;
        XmToggleButtonSetState ( highways_toggle , False , False ) ;
        no_highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_no_highways ) ;
        XmToggleButtonSetState ( no_highways_and_roads_toggle , False , False ) ;
     }

     else if ( index == ( int ) M_HIGHWAYS )
     {  
        _set_overlay_status ( M_HIGHWAYS , M_ON ) ;
        _set_overlay_status ( M_ROADS , M_OFF ) ;
        _set_overlay_status ( M_NOHIGHWAYS , M_OFF ) ;
        highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_and_roads ) ;
        XmToggleButtonSetState ( highways_and_roads_toggle , False , False ) ;
        no_highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_no_highways ) ;
        XmToggleButtonSetState ( no_highways_and_roads_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_NOHIGHWAYS )
     {
        _set_overlay_status ( M_NOHIGHWAYS , M_ON ) ;
        _set_overlay_status ( M_HIGHWAYS , M_OFF ) ;
        _set_overlay_status ( M_ROADS , M_OFF ) ;
        highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_and_roads ) ;
        XmToggleButtonSetState ( highways_and_roads_toggle , False , False ) ;
        highways_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_only ) ;
        XmToggleButtonSetState ( highways_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_TOPOGRAPHY )
     {
        _set_overlay_status ( M_TOPOGRAPHY_CONTOUR , M_OFF ) ;
        _set_overlay_status ( M_TOPOGRAPHY_NONE , M_OFF ) ;
        topo_contour_toggle = _get_menu_item_toggle_widget ( map_overlay_topo_contour ) ; 
        XmToggleButtonSetState ( topo_contour_toggle , False , False ) ;
        no_topo_toggle = _get_menu_item_toggle_widget ( map_overlay_no_topo ) ;
        XmToggleButtonSetState ( no_topo_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_TOPOGRAPHY_CONTOUR )
     {
        _set_overlay_status ( M_TOPOGRAPHY , M_OFF ) ;
        _set_overlay_status ( M_TOPOGRAPHY_NONE , M_OFF ) ;
        topo_toggle = _get_menu_item_toggle_widget ( map_overlay_topo ) ; 
        XmToggleButtonSetState ( topo_toggle , False , False ) ;
        no_topo_toggle = _get_menu_item_toggle_widget ( map_overlay_no_topo ) ;
        XmToggleButtonSetState ( no_topo_toggle , False , False ) ;
     }
     else if ( index == ( int ) M_TOPOGRAPHY_NONE )
     {
        _set_overlay_status ( M_TOPOGRAPHY , M_OFF ) ;
        _set_overlay_status ( M_TOPOGRAPHY_CONTOUR , M_OFF ) ;
        topo_toggle = _get_menu_item_toggle_widget ( map_overlay_topo ) ; 
        XmToggleButtonSetState ( topo_toggle , False , False ) ;
        topo_contour_toggle = _get_menu_item_toggle_widget ( map_overlay_topo_contour ) ; 
        XmToggleButtonSetState ( topo_contour_toggle , False , False ) ;
     }     
 
     mSetCursor ( M_WATCH ) ;
     if(index < ( int ) M_OVERLAYS && index >= 0)
     {
     	overlays[index].status = M_ON ;
     }
     else  if(index < 0)
     {
	index *= -1;
     	user_overlays[index-1].status = M_ON ;
     }

     for (k = 0;k < num_of_maps; k++)
     {
        mUpdateMap ( k ) ;
     }

     mSetCursor ( M_NORMAL ) ;
  }
  else
  {

     if ( index == ( int ) M_RIVERS )
     {
        _set_overlay_status ( M_RIVERS , M_ON ) ;
        rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_rivers ) ; 
        XmToggleButtonSetState ( rivers_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_STREAMS )
     {
        _set_overlay_status ( M_STREAMS , M_ON ) ;
        streams_toggle = _get_menu_item_toggle_widget ( map_overlay_streams ) ; 
        XmToggleButtonSetState ( streams_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_NOSTREAMS )
     {
        _set_overlay_status ( M_STREAMS , M_ON ) ;
        no_streams_rivers_toggle = _get_menu_item_toggle_widget ( map_overlay_no_streams ) ;
        XmToggleButtonSetState ( no_streams_rivers_toggle , True , False ) ;
     } 
     else if ( index == ( int ) M_ROADS )
     {
        _set_overlay_status ( M_ROADS , M_ON ) ;
        highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_and_roads ) ;
        XmToggleButtonSetState ( highways_and_roads_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_HIGHWAYS )
     {
        _set_overlay_status ( M_HIGHWAYS , M_ON ) ;
        highways_toggle = _get_menu_item_toggle_widget ( map_overlay_highways_only ) ;
        XmToggleButtonSetState ( highways_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_NOHIGHWAYS )
     {
        _set_overlay_status ( M_NOHIGHWAYS , M_ON ) ;
        no_highways_and_roads_toggle = _get_menu_item_toggle_widget ( map_overlay_no_highways ) ;
        XmToggleButtonSetState ( no_highways_and_roads_toggle , True , False ) ;
     } 
     else if ( index == ( int ) M_TOPOGRAPHY )
     {
        _set_overlay_status ( M_TOPOGRAPHY , M_ON ) ;
        topo_toggle = _get_menu_item_toggle_widget ( map_overlay_topo ) ; 
        XmToggleButtonSetState ( topo_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_TOPOGRAPHY_CONTOUR )
     {
        _set_overlay_status ( M_TOPOGRAPHY_CONTOUR , M_ON ) ;
        topo_contour_toggle = _get_menu_item_toggle_widget ( map_overlay_topo_contour ) ; 
        XmToggleButtonSetState ( topo_contour_toggle , True , False ) ;
     }
     else if ( index == ( int ) M_TOPOGRAPHY_NONE )
     {
        _set_overlay_status ( M_TOPOGRAPHY_NONE , M_ON ) ;
        no_topo_toggle = _get_menu_item_toggle_widget ( map_overlay_no_topo ) ;
        XmToggleButtonSetState ( no_topo_toggle , True , False ) ;
     }

     if ( ( index != ( int ) M_RIVERS ) && ( index != ( int ) M_STREAMS ) &&
          ( index != ( int ) M_NOSTREAMS ) && ( index != ( int ) M_ROADS ) &&
          ( index != ( int ) M_HIGHWAYS ) && ( index != ( int) M_NOHIGHWAYS ) &&
          ( index != ( int ) M_TOPOGRAPHY ) && 
          ( index != ( int ) M_TOPOGRAPHY_CONTOUR ) && 
          ( index != ( int ) M_TOPOGRAPHY_NONE ) ) 
     {
        mSetCursor ( M_WATCH ) ;
	
	if(index < ( int ) M_OVERLAYS && index >= 0)
	{
        	overlays[index].status = M_OFF ;
        }
	else if(index < 0)
	{
	        index *= -1;
		user_overlays[index-1].status = M_OFF ;
	}
        for ( k = 0 ; k < num_of_maps ; k++)
        {
           mUpdateMap ( k ) ;
        }

        mSetCursor ( M_NORMAL ) ;
     }

  }
}

/*****************************************************************************
 *
 * Routine: _set_projection
 *
 * Description: this routine is callback for projection toggle buttons.  It 
 * sets the map projection and redraws the map.
 *
 ****************************************************************************/

void _set_projection(Widget wid,XtPointer item,XtPointer call_data)
{
  int k,num_of_maps,proj = (int)item;
  
  _set_map_projection(proj);
  num_of_maps = _get_num_of_maps();

  for (k = 0;k < num_of_maps; k++){
    _set_map_change(k);
    _map_expose(_get_map_widget(k),(XtPointer)k,NULL);
  }

}

/*****************************************************************************
 *
 * Routine: _init_river
 *
 * Description: this routine initializes the river item in rivers array
 *
 ****************************************************************************/

void _init_river(int index,char *wfo,int rfcs,char *rfc1,char *rfc2,char *rfc3,
		 char *rfc4,char *fname,char *bcd)
{
  strcpy(rivers[index].wfo,wfo);
  rivers[index].num_of_rfcs = rfcs;
  rivers[index].seg = NULL;
  strcpy(rivers[index].rfc[0],rfc1);
  strcpy(rivers[index].rfc[1],rfc2);
  strcpy(rivers[index].rfc[2],rfc3);
  strcpy(rivers[index].rfc[3],rfc4);
  strcpy ( rivers[index].shape_filename , fname ) ;  
  strcpy ( rivers[index].bcd_filename , bcd ) ;
}

/*****************************************************************************
 *
 * Routine: _init_rivers
 *
 * Description: this routine initializes the river item in rivers array
 *
 ****************************************************************************/

void _init_rivers()
{
  _init_river(0,"AFC",1,"AKRFC","","","","afc_rvr.shp","afc_rvr.bcd");
  _init_river(1,"AFG",1,"AKRFC","","","","afg_rvr.shp","afg_rvr.bcd");
  _init_river(2,"AJK",1,"AKRFC","","","","ajk_rvr.shp","ajk_rvr.bcd");
  _init_river(3,"ABR",1,"MBRFC","","","","abr_rvr.shp","abr_rvr.bcd");
  _init_river(4,"APX",1,"NCRFC","","","","apx_rvr.shp","apx_rvr.bcd");
  _init_river(5,"ARX",1,"NCRFC","","","","arx_rvr.shp","arx_rvr.bcd");
  _init_river(6,"BIS",1,"MBRFC","","","","bis_rvr.shp","bis_rvr.bcd");
  _init_river(7,"BOU",1,"MBRFC","","","","bou_rvr.shp","bou_rvr.bcd");
  _init_river(8,"CYS",1,"MBRFC","","","","cyc_rvr.shp","cyc_rvr.bcd");
  _init_river(9,"DDC",1,"ABRFC","","","","ddc_rvr.shp","ddc_rvr.bcd");
  _init_river(10,"DLH",1,"NCRFC","","","","dlh_rvr.shp","dlh_rvr.bcd");
  _init_river(11,"DMX",2,"NCRFC","MBRFC","","","dmx_rvr.shp","dmx_rvr.bcd");
  _init_river(12,"DTX",1,"NCRFC","","","","dtx_rvr.shp","dtx_rvr.bcd");
  _init_river(13,"DVN",1,"NCRFC","","","","dvn_rvr.shp","dvn_rvr.bcd");
  _init_river(14,"EAX",1,"MBRFC","","","","eax_rvr.shp","eax_rvr.bcd");
  _init_river(15,"FGF",1,"NCRFC","","","","fgf_rvr.shp","fgf_rvr.bcd");
  _init_river(16,"FSD",1,"MBRFC","","","","fsd_rvr.shp","fsd_rvr.bcd");
  _init_river(17,"GID",1,"MBRFC","","","","gid_rvr.shp","gid_rvr.bcd");
  _init_river(18,"GJT",1,"CBRFC","","","","gjt_rvr.shp","gjt_rvr.bcd");
  _init_river(19,"GLD",1,"MBRFC","","","","gld_rvr.shp","gld_rvr.bcd");
  _init_river(20,"GRB",1,"NCRFC","","","","grb_rvr.shp","grb_rvr.bcd");
  _init_river(21,"GRR",1,"NCRFC","","","","grr_rvr.shp","grr_rvr.bcd");
  _init_river(22,"ICT",1,"ABRFC","","","","ict_rvr.shp","ict_rvr.bcd");
  _init_river(23,"ILX",1,"NCRFC","","","","ilx_rvr.shp","ilx_rvr.bcd");
  _init_river(24,"IND",1,"OHRFC","","","","ind_rvr.shp","ind_rvr.bcd");
  _init_river(25,"IWX",2,"OHRFC","NCRFC","","","iwx_rvr.shp","iwx_rvr.bcd");
  _init_river(26,"JKL",1,"OHRFC","","","","jkl_rvr.shp","jkl_rvr.bcd");
  _init_river(27,"LBF",1,"MBRFC","","","","lbf_rvr.shp","lbf_rvr.bcd");
  _init_river(28,"LMK",1,"OHRFC","","","","lmk_rvr.shp","lmk_rvr.bcd");
  _init_river(29,"LOT",1,"NCRFC","","","","lot_rvr.shp","lot_rvr.bcd");
  _init_river(30,"LXS",1,"NCRFC","","","","lxs_rvr.shp","lxs_rvr.bcd");
  _init_river(31,"MKX",1,"NCRFC","","","","mkx_rvr.shp","mkx_rvr.bcd");
  _init_river(32,"MPX",1,"NCRFC","","","","mpx_rvr.shp","mpx_rvr.bcd");
  _init_river(33,"MQT",1,"NCRFC","","","","mqt_rvr.shp","mqt_rvr.bcd");
  _init_river(34,"OAX",1,"MBRFC","","","","oax_rvr.shp","oax_rvr.bcd");
  _init_river(35,"PAH",2,"OHRFC","LBRFC","","","pah_rvr.shp","pah_rvr.bcd");
  _init_river(36,"PUB",1,"ABRFC","","","","pub_rvr.shp","pub_rvr.bcd");
  _init_river(37,"RIW",1,"MBRFC","","","","riw_rvr.shp","riw_rvr.bcd");
  _init_river(38,"SGF",3,"ABRFC","MBRFC","LBRFC","","sgf_rvr.shp","sgf_rvr.bcd");
  _init_river(39,"TOP",1,"MBRFC","","","","top_rvr.shp","top_rvr.bcd");
  _init_river(40,"UNR",1,"MBRFC","","","","unr_rvr.shp","unr_rvr.bcd");
  _init_river(41,"AKQ",2,"SERFC","MARFC","","","akq_rvr.shp","akq_rvr.bcd");
  _init_river(42,"ALY",1,"NERFC","","","","aly_rvr.shp","aly_rvr.bcd");
  _init_river(43,"BGM",1,"MARFC","","","","bgm_rvr.shp","bgm_rvr.bcd");
  _init_river(44,"BOX",1,"NERFC","","","","box_rvr.shp","box_rvr.bcd");
  _init_river(45,"BTV",1,"NERFC","","","","btv_rvr.shp","btv_rvr.bcd");
  _init_river(46,"BUF",1,"NERFC","","","","buf_rvr.shp","buf_rvr.bcd");
  _init_river(47,"CAE",1,"SERFC","","","","cae_rvr.shp","cae_rvr.bcd");
  _init_river(48,"CAR",1,"NERFC","","","","car_rvr.shp","car_rvr.bcd");
  _init_river(49,"CHS",1,"SERFC","","","","chs_rvr.shp","chs_rvr.bcd");
  _init_river(50,"CLE",1,"OHRFC","","","","cle_rvr.shp","cle_rvr.bcd");
  _init_river(51,"CTP",1,"MARFC","","","","ctp_rvr.shp","ctp_rvr.bcd");
  _init_river(52,"GSP",1,"SERFC","","","","gsp_rvr.shp","gsp_rvr.bcd");
  _init_river(53,"GYX",1,"NERFC","","","","gyx_rvr.shp","gyx_rvr.bcd");
  _init_river(54,"ILM",1,"SERFC","","","","ilm_rvr.shp","ilm_rvr.bcd");
  _init_river(55,"ILN",1,"OHRFC","","","","iln_rvr.shp","iln_rvr.bcd");
  _init_river(56,"LWX",1,"MARFC","","","","lwx_rvr.shp","lwx_rvr.bcd");
  _init_river(57,"MHX",1,"SERFC","","","","mhx_rvr.shp","mhx_rvr.bcd");
  _init_river(58,"OKX",2,"MARFC","NERFC","","","okx_rvr.shp","okx_rvr.bcd");
  _init_river(59,"PBZ",1,"OHRFC","","","","pbz_rvr.shp","pbz_rvr.bcd");
  _init_river(60,"PHI",1,"MARFC","","","","phi_rvr.shp","phi_rvr.bcd");
  _init_river(61,"RAH",1,"SERFC","","","","rah_rvr.shp","rah_rvr.bcd");
  _init_river(62,"RLX",1,"OHRFC","","","","rlx_rvr.shp","rlx_rvr.bcd");
  _init_river(63,"RNK",3,"MARFC","SERFC","OHRFC","","rnk_rvr.shp","rnk_rvr.bcd");
  _init_river(64,"GUM",1,"AKRFC","","","","gum_rvr.shp","gum_rvr.bcd");
  _init_river(65,"HFO",1,"AKRFC","","","","hfo_rvr.shp","hfo_rvr.bcd");
  _init_river(66,"ABQ",1,"WGRFC","","","","abq_rvr.shp","abq_rvr.bcd");
  _init_river(67,"AMA",1,"ABRFC","","","","ama_rvr.shp","ama_rvr.bcd");
  _init_river(68,"BMX",1,"SERFC","","","","bmx_rvr.shp","bmx_rvr.bcd");
  _init_river(69,"BRO",1,"WGRFC","","","","bro_rvr.shp","bro_rvr.bcd");
  _init_river(70,"CRP",1,"WGRFC","","","","crp_rvr.shp","crp_rvr.bcd");
  _init_river(71,"EPZ",1,"WGRFC","","","","epx_rvr.shp","epx_rvr.bcd");
  _init_river(72,"EWX",1,"WGRFC","","","","ewx_rvr.shp","ewx_rvr.bcd");
  _init_river(73,"EYW",1,"SERFC","","","","eyw_rvr.shp","eyw_rvr.bcd");
  _init_river(74,"FFC",1,"SERFC","","","","ffc_rvr.shp","ffc_rvr.bcd");
  _init_river(75,"FWD",1,"WGRFC","","","","fwd_rvr.shp","fwd_rvr.bcd");
  _init_river(76,"HGX",1,"WGRFC","","","","hgx_rvr.shp","hgx_rvr.bcd");
  _init_river(77,"JAN",1,"LMRFC","","","","jan_rvr.shp","jan_rvr.bcd");
  _init_river(78,"JAX",1,"SERFC","","","","jax_rvr.shp","jax_rvr.bcd");
  _init_river(79,"LCH",1,"LMRFC","","","","lch_rvr.shp","lch_rvr.bcd");
  _init_river(80,"LIX",1,"LMRFC","","","","lix_rvr.shp","lix_rvr.bcd");
  _init_river(81,"LUB",1,"WGRFC","","","","lub_rvr.shp","lub_rvr.bcd");
  _init_river(82,"LZK",2,"LMRFC","ABRFC","","","lzk_rvr.shp","lzk_rvr.bcd");
  _init_river(83,"MAF",1,"WGRFC","","","","maf_rvr.shp","maf_rvr.bcd");
  _init_river(84,"MEG",1,"LMRFC","","","","meg_rvr.shp","meg_rvr.bcd");
  _init_river(85,"MFL",1,"SERFC","","","","mfl_rvr.shp","mfl_rvr.bcd");
  _init_river(86,"MLB",1,"SERFC","","","","mlb_rvr.shp","mlb_rvr.bcd");
  _init_river(87,"MOB",2,"SERFC","MBRFC","","","mob_rvr.shp","mob_rvr.bcd");
  _init_river(88,"MRX",1,"LMRFC","","","","mrx_rvr.shp","mrx_rvr.bcd");
  _init_river(89,"OHX",1,"OHRFC","","","","ohx_rvr.shp","ohx_rvr.bcd");
  _init_river(90,"OUN",1,"ABRFC","","","","oun_rvr.shp","oun_rvr.bcd");
  _init_river(91,"SHV",1,"LMRFC","","","","shv_rvr.shp","shv_rvr.bcd");
  _init_river(92,"SJT",1,"WGRFC","","","","sjt_rvr.shp","sjt_rvr.bcd");
  _init_river(93,"SJU",1,"SERFC","","","","sju_rvr.shp","sju_rvr.bcd");
  _init_river(94,"TAE",1,"SERFC","","","","tae_rvr.shp","tae_rvr.bcd");
  _init_river(95,"TBW",1,"SERFC","","","","tbw_rvr.shp","tbw_rvr.bcd");
  _init_river(96,"TSA",1,"ABRFC","","","","tsa_rvr.shp","tsa_rvr.bcd");
  _init_river(97,"BOI",1,"NWRFC","","","","boi_rvr.shp","boi_rvr.bcd");
  _init_river(98,"BYZ",1,"MBRFC","","","","byz_rvr.shp","byz_rvr.bcd");
  _init_river(99,"EKA",1,"CNRFC","","","","eka_rvr.shp","eka_rvr.bcd");
  _init_river(100,"FGZ",1,"CBRFC","","","","fgz_rvr.shp","fgz_rvr.bcd");
  _init_river(101,"GGW",1,"MBRFC","","","","hgw_rvr.shp","hgw_rvr.bcd");
  _init_river(102,"HNX",1,"CNRFC","","","","hnx_rvr.shp","hnx_rvr.bcd");
  _init_river(103,"LKN",1,"CNRFC","","","","lkn_rvr.shp","lkn_rvr.bcd");
  _init_river(104,"LOX",1,"CNRFC","","","","lox_rvr.shp","lox_rvr.bcd");
  _init_river(105,"MFR",2,"CNRFC","NWRFC","","","mfr_rvr.shp","mfr_rvr.bcd");
  _init_river(106,"MSO",1,"NWRFC","","","","mso_rvr.shp","mso_rvr.bcd");
  _init_river(107,"MTR",1,"CNRFC","","","","mtr_rvr.shp","mtr_rvr.bcd");
  _init_river(108,"OTX",1,"NWRFC","","","","otx_rvr.shp","otx_rvr.bcd");
  _init_river(109,"PDT",1,"NWRFC","","","","pdt_rvr.shp","pdt_rvr.bcd");
  _init_river(110,"PIH",1,"NWRFC","","","","pih_rvr.shp","pih_rvr.bcd");
  _init_river(111,"PQR",1,"NWRFC","","","","pqr_rvr.shp","pqr_rvr.bcd");
  _init_river(112,"PSR",1,"CORFC","","","","psr_rvr.shp","psr_rvr.bcd");
  _init_river(113,"REV",1,"CNRFC","","","","rev_rvr.shp","rev_rvr.bcd");
  _init_river(114,"SEW",1,"NWRFC","","","","sew_rvr.shp","sew_rvr.bcd");
  _init_river(115,"SGX",1,"CNRFC","","","","sgx_rvr.shp","sqx_rvr.bcd");
  _init_river(116,"SLC",1,"CORFC","","","","slc_rvr.shp","slc_rvr.bcd");
  _init_river(117,"STO",1,"CNRFC","","","","sto_rvr.shp","sto_rvr.bcd");
  _init_river(118,"TFX",1,"MBRFC","","","","tfx_rvr.shp","tfx_rvr.bcd");
  _init_river(119,"TWC",1,"CORFC","","","","twc_rvr.shp","twc_rvr.bcd");
  _init_river(120,"VEF",2,"CNRFC","CORFC","","","vef_rvr.shp","vef_rvr.bcd");
}


/*****************************************************************************
 *
 * Routine: _init_overlay
 *
 * Description: This routine initializes an overlay structure representing
 *              a drawable overlay on the map.
 *
 ****************************************************************************/

static void _init_overlay ( int item )
{
  int i ;  /* A loop index variable. */

  if(item < M_OVERLAYS && item >= 0)
  {
  
  	overlays [ item ].status = M_OFF ;
  	memset ( overlays [ item ].color , '\0' , COLOR_NAME_LENGTH ) ;

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     	memset ( overlays [item].filepath [ i ] , '\0' , 
              	OVERLAY_FILENAME_LEN );
  	}

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     	memset ( overlays [item].filename [ i ] , '\0' , 
              	OVERLAY_FILENAME_LEN );
  	}

  	overlays[item].n_files = 0 ;
  	overlays[item].type = M_NONE ;
  	overlays[item].store_in_memory = M_OFF ;

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     		overlays[item].seg [ i ] = NULL;
  	}

  	overlays[item].pCalculateOverlay = NULL ; 
  	overlays[item].pExternalOverlay = NULL ;
   }
   else if(item < 0)
   {	
  	item *= -1;
	user_overlays [ item-1].status = M_OFF ;
  	memset ( user_overlays [ item-1].color , '\0' , COLOR_NAME_LENGTH ) ;

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     	memset ( user_overlays [item-1].filepath [ i ] , '\0' , 
              	OVERLAY_FILENAME_LEN );
  	}

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     	memset ( user_overlays [item-1].filename [ i ] , '\0' , 
              	OVERLAY_FILENAME_LEN );
  	}

  	user_overlays[item-1].n_files = 0 ;
  	user_overlays[item-1].type = M_NONE ;
  	user_overlays[item-1].store_in_memory = M_OFF ;

  	for ( i = 0 ; i < MAX_NUMBER_OF_FILES ; i ++ )
  	{
     		user_overlays[item-1].seg [ i ] = NULL;
  	}

  	user_overlays[item-1].pCalculateOverlay = NULL ; 
  	user_overlays[item-1].pExternalOverlay = NULL ;
   }
}

/*****************************************************************************
 *
 * Routine: _init_overlays
 *
 * Description: this routine initializes the ovelays array structure
 *
 ****************************************************************************/

void _init_overlays ( )
{
  /* Make sure that the initialization is done only once. */
  int i ; /* A loop indexing variable. */
  static int overlays_initialized = 0 ;

  /* Initialize each of the overlays. */
  if ( overlays_initialized == 0 )
  {
     for ( i = 0 ; i < (int) M_OVERLAYS ; i ++ )
     {
        _init_overlay ( i ) ;
     }
     for ( i = 1 ; i <= user_overlays_counter ; i ++ )
     {
        _init_overlay ( i*-1 ) ;
     }

     overlays_initialized = 1 ;
  }

}

/*****************************************************************************
 *
 * Routine: _close_map_screen
 *
 * Description: this routine closes map screen
 *
 ****************************************************************************/

void _close_map_screen(Widget wid,XtPointer none,XtPointer none2)
{
  int * stand_alone = ( int * ) none ;
  int status = 1;
  CleanCallback clean_routine = NULL ;

  /* Free the dynamically allocated memory used by any of the overlays. */
  _delete_overlays ( ) ;

  /* Check if the user has supplied a routine to free dynamically allocated 
     memory used by his application. */
  clean_routine = _get_clean_routine ( ) ;

  if ( clean_routine != NULL )
  {
     status = clean_routine ( ) ;
  }

  /* If the clean routine has a return value of 0, do not close
     the viewer.  This allows the clean routine to query the user
     if he wants to save data, etc. */
  if ( status == 0 )
  {
     return;
  }


/* What type of gui are we trying to close?  Is it stand alone or is it 
     the child of another gui? */

  if ( * stand_alone == 0 )
  {
     XtPopdown ( _get_map_shell ( ) ) ;
     exit ( 0 ) ;
  }
  else
  {
     XtDestroyWidget ( _get_map_shell ( ) ) ;
     exit ( 0 ) ;
  }

}

/*****************************************************************************
 *
 * Routine: _show_legend
 *
 * Description: this routine displays or removes the legend on the map
 *
 ****************************************************************************/
void _show_legend(Widget wid,XtPointer client_data,XtPointer call_data)
{
  extern struct _Map map [ ] ;

  XmToggleButtonCallbackStruct *state = 
    (XmToggleButtonCallbackStruct *)call_data;

  if (state->set)
  {
     /* If the legend is "outside" of the viewer, then handle it in a 
        special way. */
     if ( map [ 0 ].legend_draw != NULL )
     {
        map [ 0 ].legend_draw ( wid , client_data , call_data ) ;
     }
  
     _turn_legend_on ( ) ;

  }
  
  /* turn legend off */

  else{

     if ( map [ 0 ].legend_draw != NULL )
     {
        map [ 0 ].legend_draw ( wid , client_data , call_data ) ;
     }

    _turn_legend_off();

  }

}

/*****************************************************************************
 *
 * Routine: _show_toolbar
 *
 * Description: this routine displays or removes the toolbar
 *
 ****************************************************************************/
void _show_toolbar(Widget wid,XtPointer client_data,XtPointer call_data)
{
  XmToggleButtonCallbackStruct *state = 
    (XmToggleButtonCallbackStruct *)call_data;
  
  /* display legend */


  if (state->set){
    _turn_toolbar_on();
  }
  
  /* turn legend off */

  else{
    _turn_toolbar_off();
  }
}

/*****************************************************************************
 *
 * Routine: _delete_overlays
 *
 * Description: This routine frees (deallocates) the memory used by 
 *              the overlays. 
 *
 ****************************************************************************/

void _delete_overlays ( )
{
   int i ;  /* A loop indexing variable. */
   int j ;  /* A loop indexing variable. */

   for ( i = 0 ; i < M_OVERLAYS ; i ++ ) 
   {
      for ( j = 0 ; j < ( int ) MAX_NUMBER_OF_FILES ; j ++ )
      {
	 if ( overlays [ i ].seg [ j ] != NULL && i != M_OVERLAYS)
         {
            	_deallocate_segments ( & overlays [ i ].seg [ j ] ) ;
            	overlays [ i ].seg [ j ] = NULL ;
         }
      }
   }
      for(i=0;i<user_overlays_counter;i++)
      {
         for(j=0;j<(int)MAX_NUMBER_OF_FILES;j++)
	 {
	    if ( user_overlays [ i ].seg [ j ] != NULL )
            {
               	   _deallocate_segments ( & user_overlays [ i ].seg [ j ] ) ;
            	   user_overlays [ i ].seg [ j ] = NULL ;
            }
	 }
      }
}

void _set_map_font ( Widget wid , XtPointer clientdata , XtPointer calldata )
{
   int font_type = ( int ) clientdata ;
   int i;  /* Array index variable. */
   
   enum menu_items item ; 
   
   Widget font_miniscule_toggle ;
   Widget font_tiny_toggle ;
   Widget font_small_toggle ; 
   Widget font_medium_toggle ;
   Widget font_large_toggle ;
   Widget font_huge_toggle ; 
  
   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) calldata ;
   
   item = map_tool_font_small ;
 
   if ( state->set )
   { 
      if ( font_type == 0 ) 
      {
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , False , False ) ;
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , False , False ) ; 
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , False , False ) ;
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , False , False ) ;
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , False , False ) ;
      }
      else if ( font_type == 1 )
      {


         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , False , False ) ;
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , False , False ) ;
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , False , False ) ;
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , False , False ) ;
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , False , False ) ;
      }   
      else if ( font_type == 2 )
      {

         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , False , False ) ;
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , False , False ) ;
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , False , False ) ;
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , False , False ) ;
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , False , False ) ;
      }
      else if ( font_type == 3 )
      {
         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , False , False ) ;
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , False , False ) ;
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , False , False ) ;
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , False , False ) ;
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , False , False ) ;

      }
      else if ( font_type == 4 ) 
      {
         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , False , False ) ;
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , False , False ) ;
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , False , False ) ;
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , False , False ) ;
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , False , False ) ;
      }
      else if ( font_type == 5 )
      {
         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , False , False ) ;
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , False , False ) ;
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , False , False ) ;
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , False , False ) ;
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , False , False ) ;
      }

   }  
   else
   {
      font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
      XmToggleButtonSetState ( font_small_toggle , False , False ) ;

      if ( font_type == 0 )
      {
         font_miniscule_toggle = _get_menu_item_toggle_widget ( map_tool_font_miniscule ) ;
         XmToggleButtonSetState ( font_miniscule_toggle , True , False ) ;
      }
      else if ( font_type == 1 )
      {
         font_tiny_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_small ) ;
         XmToggleButtonSetState ( font_tiny_toggle , True , False ) ;
      }
      else if ( font_type == 2 )
      {
         font_small_toggle = _get_menu_item_toggle_widget ( map_tool_font_small ) ;
         XmToggleButtonSetState ( font_small_toggle , True , False ) ;
      }
      else if ( font_type == 3 )
      {
         font_medium_toggle = _get_menu_item_toggle_widget ( map_tool_font_normal ) ;
         XmToggleButtonSetState ( font_medium_toggle , True , False ) ;
      }
      else if ( font_type == 4 )
      {
         font_large_toggle = _get_menu_item_toggle_widget ( map_tool_font_large ) ;
         XmToggleButtonSetState ( font_large_toggle , True , False ) ;
      }
      else if ( font_type == 5 )
      {
         font_huge_toggle = _get_menu_item_toggle_widget ( map_tool_font_very_large ) ;
         XmToggleButtonSetState ( font_huge_toggle , True , False ) ;
      }
   }
   mSetFont ( font_type ) ;

   for ( i = 0; i < _get_num_of_maps ( ); ++ i )
   {
      mUpdateMap ( i ) ;
   }
}

/*****************************************************************************
 *
 * Routine: _set_map_pdsize
 *
 * Description: Allow users to change the point data size. Three options -
 * small, medium (default) and large
 *
 ****************************************************************************/
void _set_map_pdsize ( Widget wid , XtPointer clientdata , XtPointer calldata )
{
   int pdsize_type = ( int ) clientdata ;
   int i;  /* Array index variable. */
   
   enum menu_items item ; 
   
   Widget pdsize_vsmall_toggle ;
   Widget pdsize_small_toggle ;
   Widget pdsize_medium_toggle ;
   Widget pdsize_large_toggle ;    
  
   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) calldata ;
   
   item = map_tool_pdsize_small ;
 
   if ( state->set )
   { 
      if ( pdsize_type == M_S_VSMALL ) 
      {
         pdsize_small_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_small ) ;
         XmToggleButtonSetState ( pdsize_small_toggle , False , False ) ;
         pdsize_medium_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_medium ) ;
         XmToggleButtonSetState ( pdsize_medium_toggle , False , False ) ;
         pdsize_large_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_large ) ;
         XmToggleButtonSetState ( pdsize_large_toggle , False , False ) ; 
         
      }
      if ( pdsize_type == M_S_SMALL ) 
      {
         pdsize_vsmall_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_vsmall ) ;
         XmToggleButtonSetState ( pdsize_vsmall_toggle , False , False ) ;
         pdsize_medium_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_medium ) ;
         XmToggleButtonSetState ( pdsize_medium_toggle , False , False ) ;
         pdsize_large_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_large ) ;
         XmToggleButtonSetState ( pdsize_large_toggle , False , False ) ; 
         
      }
      else if ( pdsize_type == M_S_MEDIUM  )
      {
         pdsize_vsmall_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_vsmall ) ;
         XmToggleButtonSetState ( pdsize_vsmall_toggle , False , False ) ;
         pdsize_small_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_small ) ;
         XmToggleButtonSetState ( pdsize_small_toggle , False , False ) ;
         pdsize_large_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_large ) ;
         XmToggleButtonSetState ( pdsize_large_toggle , False , False ) ;        
      }   
      else if ( pdsize_type == M_S_LARGE  )
      {
         pdsize_vsmall_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_vsmall ) ;
         XmToggleButtonSetState ( pdsize_vsmall_toggle , False , False ) ;
         pdsize_small_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_small ) ;
         XmToggleButtonSetState ( pdsize_small_toggle , False , False ) ;
         pdsize_medium_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_medium ) ;
         XmToggleButtonSetState ( pdsize_medium_toggle , False , False ) ;        
      }
    
   }  
   else
   {
      pdsize_small_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_small ) ;
      XmToggleButtonSetState ( pdsize_small_toggle , False , False ) ;

      if ( pdsize_type == M_S_VSMALL ) 
      {
         pdsize_vsmall_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_vsmall ) ;
         XmToggleButtonSetState ( pdsize_vsmall_toggle , True , False ) ;
      }
      if ( pdsize_type == M_S_SMALL )
      {
         pdsize_small_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_small ) ;
         XmToggleButtonSetState ( pdsize_small_toggle , True , False ) ;
      }
      else if ( pdsize_type == M_S_MEDIUM )
      {
         pdsize_medium_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_medium ) ;
         XmToggleButtonSetState ( pdsize_medium_toggle , True , False ) ;
      }
      else if ( pdsize_type == M_S_LARGE )
      {
         pdsize_large_toggle = _get_menu_item_toggle_widget ( map_tool_pdsize_large ) ;
         XmToggleButtonSetState ( pdsize_large_toggle , True , False ) ;
      }     
   }
   mSetPDSize ( pdsize_type ) ;

   for ( i = 0; i < _get_num_of_maps ( ); ++ i )
   {
      mUpdateMap ( i ) ;
   }
}

/*****************************************************************************
 *
 * Routine: _update_overlay_menu
 *
 * Description: this routine greys out overlays whose files do not exist.
 *
 ****************************************************************************/

void _update_overlay_menu ( int index )
{
  int i ;
  Pixmap map = _get_map_pixmap ( ) ;

  for ( i = M_STATE ; i < M_OVERLAYS ; i++ )
  {
	if ( overlays[i].status == M_ON )
	{
		_draw_overlay ( map , index , i ) ;
	}
  }

  for(i = 0; i < user_overlays_counter; i++ )
  {
  	if ( user_overlays[i].status == M_ON )
  	{
      		_draw_user_overlay ( map , index , i ) ;
  	}
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/MapUtil/RCS/map_menubar_cb.c,v $";
 static char rcs_id2[] = "$Id: map_menubar_cb.c,v 1.30 2007/05/23 22:04:03 whfs Exp $";}
/*  ===================================================  */

}
