/*****************************************************************************
 *
 * Module: map_shapefile.c
 *
 * Description: contains routines to that read and draw the shapefile data
 *  files
 *
 *****************************************************************************/

/* include files */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <Xm/Xm.h>
#include "GetOS.h"
#include "map_bcdfile.h"
#include "map_convert.h"
#include "map_draw.h"
#include "map_menubar_cb.h"
#include "map_shapefile.h"
#include "Swap4Bytes.h"

/* global variables */

int type ;  /* Holds the numeric code representing the type 
               of the shape file. */ 
int size ;  /* Contains the size of the shapefile. */

/******************************************************************************
 *
 * Routine: _swap4bytes
 * 
 * Description: routine to reverse byte order. value 12345678h would be stored
 * as 78563412h
 *
 *****************************************************************************/

static int _swap4bytes(int dw)
{
  register long tmp;

  tmp = (dw & 0x000000FF);
  tmp = ((dw & 0x0000FF00) >> 0x08) | (tmp  << 0x08);
  tmp = ((dw & 0x00FF0000) >> 0x10) | (tmp  << 0x08);
  tmp = ((dw & 0xFF000000) >> 0x18) | (tmp  << 0x08);
  return(tmp);
}

/******************************************************************************
 *
 * Routine: _swap8bytes
 * 
 * Description: routine to reverse byte order. value 12345678h would be stored
 * as 78563412h
 *
 *****************************************************************************/

static double _swap8bytes(double qw)
{
  union{
    struct{
      long l1;
      long l2;
    }ll;
    
    double d;
  }ld;

  long tmp;

  ld.d = qw;
  tmp = _swap4bytes(ld.ll.l1);
  ld.ll.l1 = _swap4bytes(ld.ll.l2);
  ld.ll.l2 = tmp;

  return(ld.d);
}

/******************************************************************************
 *
 * Routine: _read_shapefile_header
 * 
 * Description: this routine that reads the header of the shape file and gets 
 * the shapefile type
 *
 *****************************************************************************/

static void _read_shapefile_header ( int fp , OperSys os )
{
  char buffer[100];

  read(fp,(char *)buffer,100);

  /* copy data from buffer */

  memcpy ( & type , ( buffer + 32 ) , 4 ) ;

  /* Copy the size of file information from the
     buffer to the file_size global variable. */
  memcpy ( & size , ( buffer + 24 ) , 4 ) ;

  /* translate from little endian to big endian. 
     Note that the "size" information is stored as
     big endian by default. */
  if ( os != OS_LINUX )
  {
     type = _swap4bytes ( type ) ;
  }
  else
  {
     size = _swap4bytes ( size ) ;
  }

}


/******************************************************************************
 *
 * Routine: _get_shapefile_point
 * 
 * Description: this routine draws the points on the pixmap from the given
 * shapefile.
 *
 *****************************************************************************/

int  _get_shapefile_point(int fp,double *lat,double *lon)
{
  char buffer[28]; 
  double tmp;

  if(read(fp,(char *)buffer,28) == 28){
    
    memcpy(&tmp,(buffer + 12),8);
    *lon = _swap8bytes(tmp);
    
    memcpy(&tmp,(buffer + 20),8);
    *lat = _swap8bytes(tmp);
    return(1);
  }
  else
    return(0);
}

/******************************************************************************
 *
 * Routine: _read_point_shapefile
 * 
 * Description: This routine reads the points into a linked list of
 *              _Map_Segment structures from the shapefile.
 *
*****************************************************************************/
void _read_point_shapefile ( int fp ,
                             OperSys os ,
                             struct _Map_Segment ** seg )
{
  double lat , lon ;      /* Used as temp variables to contain the 
                             latitude and longitude information. */
  float * points = NULL ;
  int i = 0 ;             /* Used as an array indexing variable. */
  int num_points ;
  size_t temp ;  /* Temporarily contains calculations while wapping bytes. */

  /* Check to make sure that the shapefile is the right "size".
     Note that the definition of the "size" variable is global to
     this file.  First, subtract the header size.  The header is
     always 100 bytes in a shapefile. */
  size -= SHAPEFILE_HEADER_LENGTH ;

  /* In a shapefile containing point data, each record is always
     28 bytes long.  If size is not evenly divisible by 28, then
     there is a problem. */
  if ( ( size <= 0 ) || ( ( size % SHAPEFILE_POINT_RECORD_LENGTH ) != 0 ) )
  {
     fprintf ( stderr , "In routine _read_point_shapefile:\n"
                        "The shapefile does not contain valid\n"
                        "point information.\n\n" ) ;
     * seg = NULL ;
     return ;
  }

  /* Determine the number of records ( the number of points ) contained
     within this shapefile. */
  num_points = size / SHAPEFILE_POINT_RECORD_LENGTH ;

  /* Allocate the memory that will be needed to contain all of the
     points in this shapefile.  Only one node in the linked list will
     be used to contain this information. Note that this is multiplied
     by a factor of two because there are two data elements (one latitude
     and one longitude) used to represent a point per record in this
     file. */
  points = ( float * ) malloc ( sizeof ( float ) * num_points * 2 ) ;

  if ( points == NULL )
  {
     fprintf ( stderr , "In routine _read_point_shapefile:\n"
                        "Attempt to malloc memory failed.\n\n" ) ;
     * seg = NULL ;
     return ;
  }

  while ( _get_shapefile_point ( fp , &lat , &lon ) == 1 )
  {
     points [ i++ ] = ( float ) lat ;
     points [ i++ ] = ( float ) lon ;
  }

  /* Depending on the operating system, "flip" the bytes in each of the
     floating point values contained within the "points" array. */
  if ( os != OS_LINUX )
  {
     temp = ( size_t ) ( 2 * num_points ) ;
     Swap4Bytes_ ( ( int * ) points , & temp ) ;
  }

  /* All of the points have been retrieved.  Create the map segment. */
  _allocate_first_segment ( num_points , seg ) ;
  _load_points ( points ) ;
  free ( points ) ;
  points = NULL ;

}

/******************************************************************************
 *
 * Routine: _draw_point_shapefile
 * 
 * Description: this routine draws the points on the pixmap from the given
 * shapefile.
 *
 *****************************************************************************/

static void _plot_points ( Pixmap map , struct _Map_Segment * seg )
{

  int i , num , x , y ;
  float lat , lon ;

  if ( seg != NULL )
  {
     /* Set the linked list pointer to the head of the linked list of
        segment information.  For point data, the linked list will
        normally have only one node. */
     _set_first_segment ( seg ) ;

     do
     {
        num = _get_points_in_segment ( ) ;
    
        for ( i = 0 ; i < num ; i++ ) 
        {
           /* Retrieve the lat/lon pair for the current point being
              processed. */
           _get_point_coordinate ( i , & lat , & lon ) ;

           /* Convert this lat/lon pair into an x/y cartesian coordinate
              which can be plotted on the pix map. */
           mConvertLatLon2XY ( lat , lon , & x , & y ) ;
          
           /* Plot the point. */
           _area_draw_fill_circle(map,x,y,2,2);
        }

     } while ( ( _next_segment ( ) ) != NULL ) ; 
  }

}

/******************************************************************************
 *
 * Routine: _read_polyline_shapefile
 * 
 * Description: this routine draws the polylines define in the shapefile on 
 * the pixmap.
 *
 *****************************************************************************/
static void _read_polyline_shapefile ( int fp , 
                                       OperSys os , 
                                       struct _Map_Segment ** seg )
{
  char buffer [ 8 ] , * rec = NULL ;
  double lat , lon , temp ;
  float * points = NULL ;
  int first = 1 ;
  int i , j , k=0 ;
  int num_of_points , num_of_parts , num_points_in_part , len ;
  int * parts = NULL ;
  int tmp ;

  /* Initialize the segment to NULL */
  * seg = NULL ;

  while ( read ( fp , ( char * ) buffer , 8 ) == 8 )
  {
    /* Retrieve the record length. */
    memcpy ( & len , ( buffer + 4 ) , 4 ) ;

    if ( os == OS_LINUX )
    {
       len = _swap4bytes ( len ) ; 
    }

    /* The length is in 16 bit words.  Convert it to represent the
       actual number of bytes. */
    len *= 2 ;

    /* process record */
    rec = ( char * ) malloc ( sizeof ( char ) * len ) ;

    /* Test for memory allocation problems. */
    if ( rec == NULL )
    {
       if ( * seg != NULL )
       {
         /* This routine will set  ( * seg ) = NULL */
         _deallocate_segments ( seg ) ;
       }

       fprintf ( stderr , "In routine _read_polyline_shapefile:\n"
                          "Could not malloc memory for %d bytes.\n"
                          "Construction of the linked list of\n"
                          "polygon shapefile segments has been\n"
                          "aborted.\n\n" , len ) ;
       return ;
    }

    read ( fp , rec , len ) ;
    memcpy ( &tmp , ( rec + SHAPEFILE_POLYGON_NUMPARTS_POS ) , 
             sizeof ( int ) ) ;

    /* The "number of parts" is stored in little endian byte 
       order.  This ordering needs to be "flipped" when 
       running on HP-UX. */
    if ( os != OS_LINUX )
    {
       num_of_parts = _swap4bytes ( tmp ) ;
    }
    else
    {
       num_of_parts = tmp ;
    }

    /* Allocate an integer array to contain the "parts" definitions. */
    parts = ( int * ) malloc ( sizeof ( int ) * num_of_parts ) ;

    /* Test for memory allocation problems. */
    if ( parts == NULL )
    {
       if ( rec != NULL )
       {
          free ( rec ) ;
          rec = NULL ;
       }

       if ( * seg != NULL )
       {
         /* This routine will set  ( * seg ) = NULL */
         _deallocate_segments ( seg ) ;
       }

       fprintf ( stderr , "In routine _read_polyline_shapefile:\n"
                          "Could not malloc memory for %d bytes.\n"
                          "Construction of the linked list of\n"
                          "polygon shapefile segments has been\n"
                          "aborted.\n\n" , num_of_parts ) ;
       return ;
    }

    memcpy ( parts , ( rec + SHAPEFILE_POLYGON_PARTS_POS ) , 
             num_of_parts * sizeof ( int ) ) ;

    /* If on HP-UX, flip the ordering of the bytes in the 
       parts "array". */
    if ( os != OS_LINUX )
    {
       Swap4Bytes_ ( parts , ( size_t * ) & num_of_parts ) ;
    }

    memcpy ( & tmp , ( rec + SHAPEFILE_POLYGON_NUMPOINTS_POS ) , 
             sizeof ( int ) ) ;

    /* The "number of points" is stored in little endian byte 
       ordering.  This ordering needs to be "flipped" when 
       running on HP-UX. */
    if ( os != OS_LINUX )
    {
       num_of_points = _swap4bytes ( tmp ) ;
    }
    else
    {
       num_of_points = tmp ;  
    }
    
    /* Get the points for each part and store them into a segment. Each 
       segment will represent a new polygon. */
    j = SHAPEFILE_POLYGON_PARTS_POS + ( sizeof ( int ) * num_of_parts ) ;

    for ( k = 0 ; k < num_of_parts ; k ++ ) 
    {
       if ( ( k + 1 ) == num_of_parts )
       {
          num_points_in_part = num_of_points - parts [ k ] ;
       }
       else
       {
          num_points_in_part = parts [ k + 1 ] - parts [ k ] ; 
       }

       points = ( float * ) malloc ( sizeof ( float ) * num_points_in_part
                                                     * 2 ) ;

       /* Test for memory allocation problems. */
       if ( points == NULL )
       {
          if ( parts != NULL )
          {
             free ( parts ) ;
             parts = NULL ;
          }
          
          if ( rec != NULL )
          {
             free ( rec ) ;
             rec = NULL ;
          }

          if ( * seg != NULL )
          {
            /* This routine will set  ( * seg ) = NULL */
            _deallocate_segments ( seg ) ;
          }

          fprintf ( stderr , "In routine _read_polyline_shapefile:\n"
                             "Could not malloc memory for %d bytes.\n"
                             "Construction of the linked list of\n"
                             "polygon shapefile segments has been\n"
                             "aborted.\n\n" , 2 * num_points_in_part ) ;
          return ;
       }

       for ( i = 0 ; i < num_points_in_part ; i++ )
       {

          memcpy ( & temp , ( rec + j ) , sizeof ( double ) ) ;

          if ( os != OS_LINUX )
          {
             lon = _swap8bytes(temp);
          }
          else
          {
             lon = temp ;
          }
      
          j += 8 ; 
          memcpy ( & temp , ( rec + j ) , sizeof ( double ) ) ;

          if ( os != OS_LINUX )
          {
             lat = _swap8bytes(temp);
          }
          else
          {
             lat = temp ;
          }

          points [ 2 * i ] = ( float ) lat ;
          points [ ( 2 * i ) + 1 ] = ( float ) lon ;
          j += 8 ;
       }

       if ( first == 1 )
       {
          _allocate_first_segment ( num_points_in_part , seg ) ;
          first = 0 ;
       }
       else
       {
          _allocate_segment ( num_points_in_part ) ;
       }

       _load_points ( points ) ;

       if ( points != NULL )
       {
          free ( points ) ;
          points = NULL ;
       }
    }

    if ( parts != NULL )
    {
       free ( parts ) ;
       parts = NULL ;
    }
    
    if ( rec != NULL )
    {
       free ( rec ) ;
       rec = NULL ;
    }
  }
}

/******************************************************************************
 *
 * Routine: _read_polygon_shapefile
 * 
 * Description: This routine reads polygons from a shape file.
 *
 *****************************************************************************/

static void _read_polygon_shapefile ( int fp , 
                                      OperSys os ,
                                      struct _Map_Segment ** seg )
{
  char buffer [ 8 ] , * rec = NULL ;
  double lat , lon , temp ;
  float * points = NULL ;
  int first = 1 ;
  int i , j , k=0 ;
  int num_of_points , num_of_parts , num_points_in_part , len ;
  int * parts = NULL ;
  int tmp ;

  /* Initialize the segment to NULL */
  * seg = NULL ;

  while ( read ( fp , ( char * ) buffer , 8 ) == 8 )
  {
    /* Retrieve the record length. */
    memcpy ( & len , ( buffer + 4 ) , 4 ) ;

    if ( os == OS_LINUX )
    {
       len = _swap4bytes ( len ) ; 
    }

    /* The length is in 16 bit words.  Convert it to represent the
       actual number of bytes. */
    len *= 2 ;

    /* process record */
    rec = ( char * ) malloc ( sizeof ( char ) * len ) ;

    /* Test for memory allocation problems. */
    if ( rec == NULL )
    {
       if ( * seg != NULL )
       {
         /* This routine will set  ( * seg ) = NULL */
         _deallocate_segments ( seg ) ;
       }

       fprintf ( stderr , "In routine _read_polygon_shapefile:\n"
                          "Could not malloc memory for %d bytes.\n"
                          "Construction of the linked list of\n"
                          "polygon shapefile segments has been\n"
                          "aborted.\n\n" , len ) ;
       return ;
    }

    read ( fp , rec , len ) ;
    memcpy ( & tmp , ( rec + SHAPEFILE_POLYGON_NUMPARTS_POS ) , 
             sizeof ( int ) ) ;

    /* The "number of parts" is stored in little endian byte 
       order.  This ordering needs to be "flipped" when 
       running on HP-UX. */
    if ( os != OS_LINUX )
    {
       num_of_parts = _swap4bytes ( tmp ) ;
    }
    else
    {
       num_of_parts = tmp ;
    }

    /* Allocate an integer array to contain the "parts" definitions. */
    parts = ( int * ) malloc ( sizeof ( int ) * num_of_parts ) ;

    /* Test for memory allocation problems. */
    if ( parts == NULL )
    {
       if ( rec != NULL )
       {
          free ( rec ) ;
          rec = NULL ;
       }

       if ( * seg != NULL )
       {
         /* This routine will set  ( * seg ) = NULL */
         _deallocate_segments ( seg ) ;
       }

       fprintf ( stderr , "In routine _read_polygon_shapefile:\n"
                          "Could not malloc memory for %d bytes.\n"
                          "Construction of the linked list of\n"
                          "polygon shapefile segments has been\n"
                          "aborted.\n\n" , num_of_parts ) ;
       return ;
    }

    memcpy ( parts , ( rec + SHAPEFILE_POLYGON_PARTS_POS ) , 
             num_of_parts * sizeof ( int ) ) ;

    /* If on HP-UX, flip the ordering of the bytes in the 
       parts "array". */
    if ( os != OS_LINUX )
    {
       Swap4Bytes_ ( parts , ( size_t * ) & num_of_parts ) ;
    }

    memcpy ( & tmp , ( rec + SHAPEFILE_POLYGON_NUMPOINTS_POS ) , 
             sizeof ( int ) ) ;

    /* The "number of points" is stored in little endian byte 
       ordering.  This ordering needs to be "flipped" when 
       running on HP-UX. */
    if ( os != OS_LINUX )
    {
       num_of_points = _swap4bytes ( tmp ) ;
    }
    else
    {
       num_of_points = tmp ;  
    }
    
    /* Get the points for each part and store them into a segment. Each 
       segment will represent a new polygon. */
    j = SHAPEFILE_POLYGON_PARTS_POS + ( sizeof ( int ) * num_of_parts ) ;

    for ( k = 0 ; k < num_of_parts ; k ++ ) 
    {
       if ( ( k + 1 ) == num_of_parts )
       {
          num_points_in_part = num_of_points - parts [ k ] ;
       }
       else
       {
          num_points_in_part = parts [ k + 1 ] - parts [ k ] ; 
       }

       points = ( float * ) malloc ( sizeof ( float ) * num_points_in_part
                                                     * 2 ) ;

       /* Test for memory allocation problems. */
       if ( points == NULL )
       {
          if ( parts != NULL )
          {
             free ( parts ) ;
             parts = NULL ;
          }
             
          if ( rec != NULL )
          {
             free ( rec ) ;
             rec = NULL ;
          }

          if ( * seg != NULL )
          {
            /* This routine will set  ( * seg ) = NULL */
            _deallocate_segments ( seg ) ;
          }

          fprintf ( stderr , "In routine _read_polygon_shapefile:\n"
                             "Could not malloc memory for %d bytes.\n"
                             "Construction of the linked list of\n"
                             "polygon shapefile segments has been\n"
                             "aborted.\n\n" , 2 * num_points_in_part ) ;
          return ;
       }

       for ( i = 0 ; i < num_points_in_part ; i++ )
       {

          memcpy ( & temp , ( rec + j ) , sizeof ( double ) ) ;

          if ( os != OS_LINUX )
          {
             lon = _swap8bytes(temp);
          }
          else
          {
             lon = temp ;
          }
      
          j += 8 ; 
          memcpy ( & temp , ( rec + j ) , sizeof ( double ) ) ;

          if ( os != OS_LINUX )
          {
             lat = _swap8bytes(temp);
          }
          else
          {
             lat = temp ;
          }

          points [ 2 * i ] = ( float ) lat ;
          points [ ( 2 * i ) + 1 ] = ( float ) lon ;
          j += 8 ;
       }

       if ( first == 1 )
       {
          _allocate_first_segment ( num_points_in_part , seg ) ;
          first = 0 ;
       }
       else
       {
          _allocate_segment ( num_points_in_part ) ;
       }

       _load_points ( points ) ;

       if ( points != NULL )
       {
          free ( points ) ;
          points = NULL ;
       }
    }

    if ( parts != NULL )
    {
       free ( parts ) ;
       parts = NULL ;
    }
    
    if ( rec != NULL )
    {
       free ( rec ) ;
       rec = NULL ;
    }
  }
}

/******************************************************************************
 *
 * Routine: _draw_shapefile
 * 
 * Description: this routine draws the shapefile data on the pixmap
 *
 *****************************************************************************/
void _draw_shapefile ( char * filename ,
                       struct _Map_Segment ** seg ,
                       Pixmap map ,
                       int store_in_memory ,
                       enum ShapeFileType * shape_type ,
                       Boolean fillarea )
{

  int fp ;
  OperSys os ;

  /* Determine if the shapefile data has been read in. */
  if ( * seg == NULL )
  {
     /* The data has not been read in yet.  If the user 
        chooses not to store the mapdata in memory, then 
        the file will always be read in. */
     fp = open ( filename , 0 ) ;

     if ( fp == -1 )
     {
       printf("Error: unable to open file %s\n",filename);
       return ;
     }
     else
     {
        /* Determine the operating system type.  This information will be
           needed for the reason being that data is stored in both big and
           little endian format in a shapefile.  See the ESRI Shapefile
           Technical Description white paper for more information. */ 
        os = GetOS ( ) ;

        if ( os == OS_ERROR )
        {
           fprintf ( stderr , "In routine _draw_shapefile:\n"
                              "The call to GetOS ( ) could not determine\n"
                              "the type of operating system.\n\n" ) ;
           close ( fp ) ;
           return ; 
        }

        _read_shapefile_header ( fp , os ) ;
     }
    
     switch ( type )
     {
        /* point */
        case S_POINT: 
           _read_point_shapefile ( fp , os , seg ) ;
           break ;
      
        /* polygon */
        case S_POLYGON: 
           _read_polygon_shapefile ( fp , os , seg ) ;
           break ;
      
        /* polyline */
        case S_POLYLINE: 
           _read_polyline_shapefile ( fp , os , seg ) ;
           break ;
      
        /* other */
        default: 
           printf ("\nError: Unknown type(%d) for shapefile %s\n\n" , type ,
                    filename ) ;
           close ( fp ) ;
           return ;
           break ;
    }

    close ( fp ) ;

  }
 
  _draw_shapefile_data ( ( enum ShapeFileType ) type , map , * seg ,
                         fillarea ) ;

  *shape_type = ( enum ShapeFileType ) type ;

  /* Check to determine if the user wants to keep the shapefile data
     stored in memory. */
  if ( store_in_memory == 0 )
  {
     /* Note that this routine will set *seg to NULL. */ 
     _deallocate_segments ( seg ) ;
  }

}

/******************************************************************************
 *
 * Routine: _draw_shapefile_data
 * 
 * Description: This routine draws the shapefile data on the pixmap 
 *              according to the type of data 
 *
 *****************************************************************************/

void _draw_shapefile_data ( enum ShapeFileType type , Pixmap map , 
                            struct _Map_Segment * seg ,
                            Boolean fillarea )
{

  /* Plot the appropriate shapefile data. */
  switch ( type )
  {

     /* point */
     case S_POINT : 
        _plot_points ( map , seg ) ;
        break ;

     /* polygon */
     case S_POLYGON : 
        _draw_points ( map , seg , fillarea ) ;
        break ;

     /* polyline */
     case S_POLYLINE : 
        _draw_points ( map , seg , fillarea ) ;
        break ;

     /* other */
     default :
        fprintf ( stderr , "\nIn routine _draw_shapefile_data:\n"
                           "Unrecognized shapefile type \"%d\"\n\n" , type ) ;
        break ;
  }

}
