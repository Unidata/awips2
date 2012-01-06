/*****************************************************************************
 *
 * Module: map_draw.c
 *
 * Description: contains routines to create map displays
 *
 *****************************************************************************/

/* include files */

#include "stdio.h"
#include "stdlib.h"
#include <Xm/Xm.h>
#include <X11/X.h>
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"
#include "map.h"

/* global variables */

extern unsigned long foreground_pixel, background_pixel, overlay_pixel1,
			overlay_pixel2;
extern unsigned long overlay_plane_mask;

/*****************************************************************************
 *
 * Routine: _area_draw_circle
 *
 * Description: this routine draws a circle for the given location and size 
 *
 ****************************************************************************/

void _area_draw_circle(Pixmap map,int x,int y,int width,int height)
{

  XDrawArc(_get_map_display(),map,_get_map_gc(),x,y,width,height,0,23040);
}
/*****************************************************************************
 *
 * Routine: _area_draw_fill_circle
 *
 * Description: this routine draws a filled circle for the given location and
 * size 
 *
 ****************************************************************************/

void _area_draw_fill_circle(Pixmap map,int x,int y,int width,int height)
{

  XFillArc(_get_map_display(),map,_get_map_gc(),x,y,width,height,0,23040);
}

/*****************************************************************************
 *
 * Routine: _area_draw_fill_half_circle
 *
 * Description: this routine draws a filled half circle to form a D for the
 * given location and size 
 *
 ****************************************************************************/

void _area_draw_fill_half_circle(Pixmap map,int x,int y,int width,int height)
{

  XFillArc(_get_map_display(),map,_get_map_gc(),x,y,width,height,5760,-11520);
}

/*****************************************************************************
 *
 * Routine: _area_draw_line
 *
 * Description: this routine draws a line for the given location
 *
 ****************************************************************************/

void _area_draw_line(Pixmap map,int x1,int y1,int x2,int y2)
{
  XDrawLine(_get_map_display(),map,_get_map_gc(),x1,y1,x2,y2);
}

/*****************************************************************************
 *
 * Routine: _area_draw_lines
 *
 * Description: this routine draws a line for the given location
 *
 ****************************************************************************/

void _area_draw_lines(Pixmap map,XPoint *points,int num_points)
{
  XDrawLines(_get_map_display(),map,_get_map_gc(),points,num_points,
	     CoordModeOrigin);
}

/*****************************************************************************
 *
 * Routine: _area_draw_box
 *
 * Description: this routine draws a box for the given location and size 
 *
 ****************************************************************************/

void _area_draw_box(Pixmap map,int x,int y,int width,int height)
{

  XDrawRectangle(_get_map_display(),map,_get_map_gc(),x,y,width,height);
}

/*****************************************************************************
 *
 * Routine: _area_draw_fill_box
 *
 * Description: this routine draws a filled box for the given location & size 
 *
 ****************************************************************************/

void _area_draw_fill_box(Pixmap map,int x,int y,int width,int height)
{
  XFillRectangle(_get_map_display(),map,_get_map_gc(),x,y,width,height);
}

/*****************************************************************************
 *
 * Routine: _area_draw_text
 *
 * Description: this routine draws text on the map for the given location
 *
 ****************************************************************************/

void _area_draw_text(Pixmap map,int x,int y,char *text)
{
  XDrawString(_get_map_display(),map,_get_map_gc(),x,y,text,strlen(text));
}

/*****************************************************************************
 *
 * Routine: _area_draw_fill_polygon
 *
 * Description: this routine draws a filled polygon for the given points
 *
 ****************************************************************************/

void _area_draw_fill_polygon(Pixmap map,XPoint *pts,int num,int shape,int mode)
{
  XFillPolygon(_get_map_display(),map,_get_map_gc(),pts,num,shape,mode);
}
/*****************************************************************************
 *
 * Routine: mDrawCircle
 *
 * Description: this routine draws a circle for the given location and size 
 *
 ****************************************************************************/

void mDrawCircle(int area, int number, int x,int y,int width,int height)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number);
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( number );

    _area_draw_circle(pix,x,y,width,height);
  }
}

/*****************************************************************************
 *
 * Routine: mDrawFillCircle
 *
 * Description: this routine draws a filled circle for the given location and
 * size 
 *
 ****************************************************************************/

void mDrawFillCircle(int area,int number,int x,int y,int width,int height)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if ( area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number);
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_fill_circle(pix,x,y,width,height);
  }
}

/*****************************************************************************
 *
 * Routine: mDrawLine
 *
 * Description: this routine draws a line for the given location
 *
 ****************************************************************************/

void mDrawLine(int area,int number,int x1,int y1,int x2,int y2)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number);
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_line(pix,x1,y1,x2,y2);
  }
}

/*****************************************************************************
 *
 * Routine: mDrawBox
 *
 * Description: this routine draws a box for the given location and size 
 *
 ****************************************************************************/

void mDrawBox(int area,int number,int x,int y,int width,int height)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number);
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_box(pix,x,y,width,height);
  }
}

/*****************************************************************************
 *
 * Routine: mDrawFillBox
 *
 * Description: this routine draws a filled box for the given location & size 
 *
 ****************************************************************************/

void mDrawFillBox(int area,int number,int x,int y,int width,int height)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number);
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_fill_box(pix,x,y,width,height);
  }
}
/*****************************************************************************
 *
 * Routine: mDrawFillBox
 *
 * Description: this routine draws a filled box for the given location & size 
 *
 ****************************************************************************/

void mDrawFillPolygon(int area,int number,XPoint *pts,int num,int shape,int mode)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number) ;
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_fill_polygon(pix,pts,num,shape,mode);
  }
}

/*****************************************************************************
 *
 * Routine: mDrawText
 *
 * Description: this routine draws text on the map for the given location
 *
 ****************************************************************************/

void mDrawText(int area,int number,int x,int y,char *text)
{
  Pixmap pix;

  if (number >= _get_num_of_maps())
    printf("Error: invalid map reference number (%d).  There are %d maps define.\n",
	   number,_get_num_of_maps());
  else{
    if (area == M_MAP)
      pix = _get_maps_pixmap(number);
    else if ( area == M_LEGEND )
      pix = _get_legend_pixmap(number) ;
    else if ( area == M_USER )
      pix = mGetUserPixmap ( ) ;
    else
      pix = _get_map_pixmap ( ) ;

    _area_draw_text(pix,x,y,text);
  }
}

#ifdef _MAPLIB_SYMBOL_ROUTINES

/*****************************************************************************
 *
 * Routine: _draw_weather_station
 *
 * Description: this routine draws weather station
 *
 ****************************************************************************/

void _draw_weather_station(int area,int number,int x,int y,char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  _area_draw_fill_circle(pix,x,y,5,5);
}

/*****************************************************************************
 *
 * Routine: _draw_river_data_point
 *
 * Description: this routine draws river data point
 *
 ****************************************************************************/
void _draw_river_data_point(int area,int number,int x,int y,char *color)
{
  int pos_x ;
  int pos_y ;
  XPoint triangle [ 3 ] ;

  _set_foreground ( color ) ;
  pos_x = x + RIVER_GAGE_X ;
  pos_y = y + RIVER_GAGE_Y ;

  triangle[0].x = pos_x ;
  triangle[0].y = pos_y - RIVER_GAGE_SIZE ;
  triangle[1].x = pos_x + RIVER_GAGE_SIZE ;
  triangle[1].y = pos_y + RIVER_GAGE_SIZE ;
  triangle[2].x = pos_x - RIVER_GAGE_SIZE ;
  triangle[2].y = pos_y + RIVER_GAGE_SIZE ;

  mDrawFillPolygon ( area , 0 , triangle , 3 , Convex , CoordModeOrigin ) ;
}

/*****************************************************************************
 *
 * Routine: _draw_river_forecast_point
 *
 * Description: this routine draws river forecast point
 *
 ****************************************************************************/

void _draw_river_forecast_point(int area,int number,int x,int y,char *color)
{
  int pos_x ;
  int pos_y ;
  unsigned int size = FCST_POINT_SIZE ;
 
  _set_foreground ( color ) ;

  pos_x = x + FCST_POINT_OFFSET_X ;
  pos_y = y + FCST_POINT_OFFSET_Y ;

  mDrawCircle ( area , number , pos_x , pos_y , ( int ) size , ( int ) size ) ;

}

/*****************************************************************************
 *
 * Routine: _draw_river_reservoir
 *
 * Description: this routine draws a reservoir symbol
 *
 ****************************************************************************/
void _draw_river_reservoir ( int area , int number , int x , int y , 
                             char * color )
{
  int pos_x ;
  int pos_y ;

  pos_x = x + RESERVOIR_OFFSET_X ;
  pos_y = y + RESERVOIR_OFFSET_Y ;

  _set_foreground ( color ) ;
  mDrawFillBox ( area , number , pos_x , pos_y , RESERVOIR_WIDTH ,
                 RESERVOIR_HEIGHT ) ;
}

/*****************************************************************************
 *
 * Routine: _draw_river_data_point_at_reservoir
 *
 * Description: this routine draws river data point at a reservoir
 *
 ****************************************************************************/

void _draw_river_data_point_at_reservoir(int area,int number,int x,int y,
					 char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _draw_river_data_point(area,number,x,y,color);
  _area_draw_box(pix,x-7,y+7,14,5);
}

/*****************************************************************************
 *
 * Routine: _draw_river_forecast_point_at_reservoir
 *
 * Description: this routine draws river forecast point at a reservoir
 *
 ****************************************************************************/

void _draw_river_forecast_point_at_reservoir(int area,int number,int x,int y,
					     char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _draw_river_forecast_point(area,number,x,y,color);
  _area_draw_box(pix,x-7,y+7,14,5);
}
  
/*****************************************************************************
 *
 * Routine: _draw_undefine_station
 *
 * Description: this routine draws undefine station
 *
 ****************************************************************************/

void _draw_undefine_station(int area,int number,int x,int y,char *color)
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( )  ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  mSetFont(M_MEDIUM);
  _area_draw_text(pix,x,y,"?");
}

/*****************************************************************************
 *
 * Routine: _draw_other_station
 *
 * Description: this routine draws undefine station
 *
 ****************************************************************************/
void _draw_other_station ( int area , int number , int x , int y ,
                           char * color )
{
  int pos_x ;
  int pos_y ;
  unsigned int size = GENERAL_STATION_SIZE ;
 
  _set_foreground ( color ) ;

  pos_x = x + X_OTHER_OFFSET ;
  pos_y = y + Y_OTHER_OFFSET ;

  mDrawFillCircle ( area , number , pos_x , pos_y , ( int ) size , 
                    ( int ) size ) ;
}

#endif

/*****************************************************************************
 *
 * Routine: _draw_damcrest_point
 *
 * Description: this routine draws the icon for the damcrest point
 *
 ****************************************************************************/
void _draw_damcrest_point (int area, int number, int x, int y, char * color )
{
  Pixmap pix;

  if (area == M_MAP)
    pix = _get_maps_pixmap(number);
  else if ( area == M_LEGEND )
    pix = _get_legend_pixmap(number) ;
  else if ( area == M_USER )
    pix = mGetUserPixmap ( ) ;
  else
    pix = _get_map_pixmap ( ) ;

  _set_foreground ( color ) ;
  _area_draw_fill_half_circle( pix, x-8, y-5, 10, 10);
}

#ifdef _MAPLIB_SYMBOL_ROUTINES

/*****************************************************************************
 *
 * Routine: _draw_river_forecast_point_with_weather_station
 *
 * Description: this routine draws river forecast point
 *
 ****************************************************************************/

void _draw_river_forecast_point_with_weather_station(int area,int number,
						     int x,int y,
						     char *color)
{
  _draw_river_forecast_point(area,number,x,y,color);
  _draw_weather_station(area,number,x-2,y-1,"Orange");
  _set_foreground ( color );
}

/*****************************************************************************
 *
 * Routine: mDrawSymbol
 *
 * Description: this routine draws a symbol
 *
 ****************************************************************************/

void mDrawSymbol(int area,int number,int x,int y,char *color, int symbol)
{
  if (area < 4){
    if (number < _get_num_of_maps()){
  
      if (symbol == M_WEATHER_STATION)
	_draw_weather_station(area,number,x,y,color);
      
      else if (symbol == M_RIVER_DATA_POINT)
	_draw_river_data_point(area,number,x,y,color);
      
      else if (symbol == M_RIVER_FORECAST_POINT)
	_draw_river_forecast_point(area,number,x,y,color);
      else if ( symbol == M_RESERVOIR_POINT )
        _draw_river_reservoir ( area, number, x , y, color ) ;
      else if (symbol == M_RIVER_DATA_POINT_AT_RESERVOIR)
	_draw_river_data_point_at_reservoir(area,number,x,y,color);
      
      else if (symbol == M_RIVER_FORECAST_POINT_AT_RESERVOIR)
	_draw_river_forecast_point_at_reservoir(area,number,x,y,color);
      
      else if (symbol == M_RIVER_FORECAST_POINT_WITH_WEATHER_STATION)
	_draw_river_forecast_point_with_weather_station(area,number,x,y,color);
      
      else if (symbol == M_UNDEFINE_STATION )
	_draw_undefine_station(area,number,x,y,color);
      else if ( symbol == M_OTHER_STATION )
        _draw_other_station ( area , number , x , y , color ) ;
      else if ( symbol == M_DAMCREST_POINT )
        _draw_damcrest_point ( area , number , x , y , color ) ;
      else
	printf("Error: Invalid symbol type\n");
    }
    else
      printf("Error: Invalid map number (%d).  There are %d maps\n",number,
	     _get_num_of_maps());
  }
  else
    printf("Error: Invalid area (%d).  There are 4 drawing areas\n",area);
}

#endif

int area_draw_planes ()
{
  int screen_num = 0;
  int default_depth = 0;
  static char * name [] = {"Red", "Yellow", "Green", "Green"};
  XColor exact_defs [MAX_CELLS];
  Colormap default_cmap;
  int ncolors = 4;
  unsigned long plane_masks[MAX_PLANES];
  unsigned long colors[MAPLIB_MAX_COLORS];
  int i;
  XVisualInfo visual_info;
  
  screen_num = XScreenNumberOfScreen(_get_screen()); 
  default_depth = DefaultDepth(_get_map_display(), screen_num);
  default_cmap = DefaultColormap(_get_map_display(), screen_num);
  if(default_depth == 1)
  {
    background_pixel = WhitePixel(_get_map_display(), screen_num);
    foreground_pixel = BlackPixel(_get_map_display(), screen_num);
    printf("Using black and white, first case\n");
    return(CANNOT_OVERLAY);
  }

  if(!XMatchVisualInfo(_get_map_display(), screen_num, default_depth,
			PseudoColor, &visual_info))
  {
    if(!XMatchVisualInfo(_get_map_display(), screen_num, default_depth,
			DirectColor, &visual_info))
    {
      background_pixel = WhitePixel(_get_map_display(), screen_num);
      foreground_pixel = BlackPixel(_get_map_display(), screen_num);
      printf("Using black and white, second case\n");
      return(CANNOT_OVERLAY);
    }
  }

  if(XAllocColorCells(_get_map_display(), default_cmap, False, plane_masks,
				1, colors, 2) == 0)
  {
    if(XAllocColorCells(_get_map_display(), default_cmap, False, plane_masks,
				0,colors, 3) == 0)
    {
       background_pixel = WhitePixel(_get_map_display(), screen_num);
       foreground_pixel = BlackPixel(_get_map_display(), screen_num);
       printf("Using black and white, third case\n");
       return(CANNOT_OVERLAY);
    }
    else
    {
      ncolors = 3;
    }
  }  
  for(i = 0; i < ncolors; i++)
  {
    if(!XParseColor(_get_map_display(), default_cmap, name[i], &exact_defs[i]))
    {
      fprintf(stderr, "Basic: color name %s is not in database", name[i]);
      exit(0);
    }      
    exact_defs[i].flags = DoRed | DoGreen | DoBlue;
  }
  printf("Got RBG values\n");
  
  exact_defs[0].pixel = colors[0];
  exact_defs[1].pixel = colors[1];
  exact_defs[2].pixel = colors[0] | plane_masks[0];
  exact_defs[3].pixel = colors[1] | plane_masks[0];
  
  XStoreColors(_get_map_display(), default_cmap, exact_defs, ncolors);
    
  printf("Stored colors\n");

  background_pixel = exact_defs[2].pixel;
  foreground_pixel = exact_defs[3].pixel;

  if(ncolors == 4)
  {
    overlay_pixel1 = exact_defs[2].pixel;
    overlay_pixel2 = exact_defs[3].pixel;
    overlay_plane_mask = plane_masks[0];
    printf("Set can_overlay\n");
    return(CAN_OVERLAY);
  }
  else
  {
    overlay_pixel1 = exact_defs[2].pixel;
    printf("Set cannot_overlay\n");
    return(CANNOT_OVERLAY);
  }
}
