#include "mapBackgrounds.h"
#include "map_library.h"
#include "map.h"
#include "map_resource.h"

/***************************************************************************/

void drawMapBackground ( HvDisplayControl *hdc , HvColorList *hcl ) 
{
     BackgroundDisplaySettings *bds = &hdc->displaySettings.background; 
     Display * display = _get_map_display ( ) ;
     GC gc = _get_map_gc ( ) ;
     Pixmap pix = _get_maps_pixmap ( 0 ) ;
     
     static  MapArea 	 *resvrs = NULL;
     static  MapArea 	 *basins = NULL; 
     static  MapArea 	 *counties = NULL;
     static  MapArea 	 *zones = NULL;
     
     static  MapLine     *streams = NULL;
     static  MapLine	 *rivers = NULL;
     
     static  MapLine	 *roads = NULL;
     static  MapLine     *hiways = NULL;
     
     static  MapPoint	 *cities = NULL;
     static  MapPoint    *towns = NULL;
     
     static  MapPoint	 *radars = NULL;
     
     static long	 numStreams = 0;
     static long	 numRivers = 0;
     
     static long	 numResvrs = 0;
     
     static long	 numBasins = 0;
     static long	 numCounties = 0;
     static long	 numZones = 0;
     
     static long	 numCities = 0;
     static long	 numTowns = 0;
     
     static long	 numRoads = 0;
     static long	 numHiways = 0;
     
     static long	 numRadars = 0;
     
    
     /*
     Display map background
     */	
     	 
         
     if (bds->showRivers)
     {
	  if ( ! rivers )
	       readMapLinesFromDb(&rivers, &numRivers, "STREAM", 1);
	  drawMapLines(rivers, numRivers, hcl->water_color,
		       False, gc, display, pix);
     }	  

     if (bds->showStreams)
     {
	  if ( ! streams)
	       readMapLinesFromDb(&streams, &numStreams, "STREAM", 2);
	  drawMapLines(streams, numStreams, hcl->water_color,
		       False, gc, display, pix);
     }
     

     if (bds->showReservoirs)
     {
	  if ( ! resvrs )
	  {
	       readMapAreasFromDb(&resvrs, &numResvrs, "RESRVR");
	  }
	  drawMapAreas(resvrs, numResvrs,
		       hcl->water_color,
		       hcl->water_color,
		       True,
		       False,
		       hdc,
		       gc, display, pix);
     }
     

  
	
     if (bds->showBasins)
     {
      	  if (! basins)
	  {
	     readMapAreasFromDb(&basins, &numBasins, "BASIN");
	  }
	  
	  if (basins)
	  {
	 	 drawMapAreas(basins, numBasins,
			      hcl->basin_color,
			      hcl->basin_color,
			      False,
			      True,
			      hdc,
		              gc, display, pix);
	  }

     }
     
     
     if (bds->showCounties)
     {
	  if (! counties)
	  {
	       readMapAreasFromDb(&counties, &numCounties, "COUNTY");
	  }
	  
	  drawMapAreas(counties, numCounties,
		       hcl->county_color,
		       hcl->county_color,
		       False,
		       True,
		       hdc,
		       gc, display, pix);
     }
     
     
     if (bds->showZones)
     {
	  if (! zones)
	  {
	       readMapAreasFromDb(&zones, &numZones, "ZONE");
	  }
	  
	  drawMapAreas(zones, numZones,
		       hcl->zone_color,
		       hcl->zone_color,
		       False,
		       True,
		       hdc,
		       gc, display, pix);
     }
   
     
     if (bds->showCities)
     {
	  if ( ! cities)
	       read_cities(&cities, &numCities, 1);  /* reads from database */
	  
	  drawMapPoints(cities, numCities, hcl->city_color,
			True, gc, display, pix);
     }
     
     if (bds->showTowns)
     {
	  if ( ! towns)
	       read_cities(&towns, &numTowns, 2);  /* reads from database */
	  
	  drawMapPoints(towns, numTowns, hcl->city_color,
			True, gc, display, pix);
     }
     
    
	
     if (bds->showHiways)
     {
	  if (! hiways)
	       readMapLinesFromDb(&hiways, &numHiways, "ROAD", 1);  
	  
	  drawMapLines(hiways, numHiways,  hcl->road_color,
		       False, gc, display, pix);
	  
     }
     
     if (bds->showRoads)
     {
	  if (! roads)
	       readMapLinesFromDb(&roads, &numRoads, "ROAD", 2);  
	  
	  drawMapLines(roads, numRoads,  hcl->road_color,
		       False, gc, display, pix);
	  
     }
     
     
     if (bds->showRadars)
     {
	  if ( ! radars )
	       read_radars(&radars, &numRadars);  /* reads from database */
	  
	  draw_radars(radars, numRadars,  hcl->radar_color, gc, display, pix);
     
     }
     return;  
}

/***************************************************************************/

void	drawMapAreas(MapArea *areas, long numAreas ,
		     char * boundaryColor ,
		     char * fillColor ,
		     Boolean fillArea ,
		     Boolean showLabel ,
		     HvDisplayControl *hdc ,
		     GC gc , Display *dpy , Pixmap pix )
{   
    
     int		i;
     Boolean		showValue1 = False;
     Boolean		showValue2 = False;
     Boolean		showId = False;
     Boolean		showName = False;
     
     
     for (i = 0; i < numAreas; i++)
     {
	  
	  
	  /*
	  	draw no labels, only the map
	  */
	  drawMapArea(&areas[i],
		      boundaryColor,
		      fillColor,
		      True,
		      fillArea,
		      False,
		      False,
		      False,
		      False,
		      gc, dpy, pix);
     }
     
     
     for (i = 0; i < numAreas; i++)
     {
	  /*
	  	draw no map, only labels (if desired)
	  */
	  if (showLabel)
	  {
	        showId = hdc->displaySettings.areal.showId;
	  	showName = hdc->displaySettings.areal.showName;
	  }
	  else
	  {
	        showId = False;
		showName = False;
	  }
	  
	  
	  drawMapArea(&areas[i],
		      boundaryColor,
		      fillColor,
		      False,
		      fillArea,
		      showId,
		      showName,
		      showValue1,
		      showValue2, gc, dpy, pix);
     }
     
     
     
     return;
     
}

/***************************************************************************/

void	drawMapArea(MapArea *area, 
		    char * boundaryColor ,
		    char * fillColor ,
		    Boolean drawArea,
		    Boolean fillArea,
		    Boolean showId,
		    Boolean showName,
		    Boolean showValue1,
		    Boolean showValue2,
		    GC gc, Display *dpy, Pixmap pix)
{
     
     HvColorList *hcl = getHvColorList();
     
     int   i;
     M_XYPoint		 xypos;
     M_XYPoint		 interiorXY;
     char		 buf[BUFSIZ];
     
     /*
     Draws an area on the screen  
     */
     
     /*
     determine interior X Y point
     */
     mConvertLatLon2XY ( area->interiorPoint.lat ,
                         area->interiorPoint.lon ,
                         ( int * ) & ( interiorXY.x ) ,
                         ( int * ) & ( interiorXY.y ) ) ;
     
     if (drawArea)
     {
	  
	  /*
     	  assign the area's x-y coords from lat lon coords   
	  */
	  for (i = 0; i < area->npts; i++)
	  {
             mConvertLatLon2XY ( area->actual[i].lat ,
                                 area->actual[i].lon ,
                                 ( int * ) & ( xypos.x ) ,
                                 ( int * ) & ( xypos.y ) ) ;
	       area->poly[i].x = xypos.x;
	       area->poly[i].y = xypos.y;
	  }
	  
	  /*
	  draw the area as  filled area if fillArea is true
	  */    
	  if (fillArea)
	  {
              mSetColor ( fillColor ) ;
	      XFillPolygon(dpy, pix, gc,
		           area->poly, area->npts, 
			   Nonconvex, CoordModeOrigin);
	  }
	  
	  
	  /*
	  always draw the boundary
	  */
          mSetColor ( fillColor ) ;
	  XDrawLines(dpy, pix, gc,
		     area->poly, area->npts,
		     CoordModeOrigin);
     }
     
     
     /*
     Draw the labels as requested
     */
     
     mSetColor( hcl->labelColor );
     
     if (showId)
     {
	  XDrawString(dpy, pix, gc,
		      interiorXY.x + AREA_ID_X_OFFSET,
		      interiorXY.y + AREA_ID_Y_OFFSET,
		      area->id,
		      strlen(area->id));
     }
     
     
     if (showName)
     {
	  
	  XDrawString(dpy, pix, gc,
		      interiorXY.x + AREA_NAME_X_OFFSET,
		      interiorXY.y + AREA_NAME_Y_OFFSET,
		      area->name,
		      strlen(area->name)); 
     }
     
     
     if (showValue1)
     {
	  if (area->value1 != MISSING)
	  {
	       sprintf(buf, "%6.2f", area->value1);
	  }
	  else
	  {
	       strcpy(buf, "MSG");  
	  }
	  
	  XDrawString(dpy, pix, gc,
		      interiorXY.x + AREA_VALUE1_X_OFFSET,
		      interiorXY.y + AREA_VALUE1_Y_OFFSET,
		      buf,
		      strlen(buf)); 
     }
     
     if (showValue2)
     {
	  if (area->value2 != MISSING)
	  {
	       sprintf(buf, "%6.2f", area->value2);
	  }
	  else
	  {
	       strcpy(buf, "MSG");  
	  }
	  
	  XDrawString(dpy, pix, gc,
		      interiorXY.x + AREA_VALUE2_X_OFFSET,
		      interiorXY.y + AREA_VALUE2_Y_OFFSET,
		      buf,
		      strlen(buf)); 
     }
     
     return;
     
}
/***************************************************************************/

void	drawMapLines(MapLine *lines , long numLines , char * lineColor ,
		     Boolean showLabel, GC gc, Display *dpy, Pixmap pix)
{
     M_XYPoint		 xypos;
     int		 i;
     int	  	 j;
     
     
     /*
     Loop through rivers.
     */
     mSetColor( lineColor ) ;
     for (i = 0; i < numLines; i++)
     {
	  for (j = 0; j < lines[i].npts; j++)
	  {
               mConvertLatLon2XY ( lines[i].actual[j].lat ,
                                   lines[i].actual[j].lon ,
                                   ( int * ) & ( xypos.x ) ,
                                   ( int * ) & ( xypos.y ) ) ;
	       lines[i].poly[j].x = xypos.x;
	       lines[i].poly[j].y = xypos.y;
	  }
	  
	  XDrawLines(dpy, pix, gc,
		     lines[i].poly, lines[i].npts,
		     CoordModeOrigin);
     }
     return;
}


/***************************************************************************/

void	drawMapPoints(MapPoint *points, long numPoints,
		      char * pointColor,
		      Boolean showLabel, GC gc, Display *dpy, Pixmap pix)
{
     int		i;
     
    
     
     for (i = 0; i < numPoints; i++)
     {
          mConvertLatLon2XY ( points[i].loc.lat ,
                              points[i].loc.lon ,
                              ( int * ) & ( points[i].pos.x ) ,
                              ( int * ) & ( points[i].pos.y ) ) ;
	  	  
	  mSetColor ( pointColor ) ;
	  XFillArc(dpy, pix, gc,
		   points[i].pos.x -3,
		   points[i].pos.y -3,
		   6, 6, 0, 360 * 64);
	  
	  if (showLabel)
	  {
	       XDrawString(dpy, pix, gc,
			   points[i].pos.x + 8,
			   points[i].pos.y + 6,
			   points[i].name,
			   strlen(points[i].name));
	  }
     }
     
     return;
}

/***************************************************************************/

void	draw_radars(MapPoint *radarPts, long num_radars,
		    char * radar_color,
		    GC gc, Display *dpy, Pixmap pix)
{
     
     int	  i;
    
     /*
         draw dot
     */
     mSetColor ( radar_color ) ;

     for (i = 0; i < num_radars; i++)
     {     
          mConvertLatLon2XY ( radarPts[i].loc.lat ,
                              radarPts[i].loc.lon ,
                              ( int * ) & ( radarPts[i].pos.x ) ,
                              ( int * ) & ( radarPts[i].pos.y) ) ;
	  
	  XFillArc(dpy, pix, gc,
		   radarPts[i].pos.x -3,
		   radarPts[i].pos.y -3,
		   6, 6, 0, 360 * 64);
	  
	  XDrawString(dpy, pix, gc,
		      radarPts[i].pos.x + 8,
		      radarPts[i].pos.y + 6,
		      radarPts[i].name,
		      strlen(radarPts[i].name));
     }
     
  
     /*
     draw coverage ring for each radar
     */
     
     for (i = 0; i < num_radars; i++)
     {		       
	  drawRadarRing(radarPts[i], radar_color, gc, dpy, pix);  
     }
     return;
}

/***************************************************************************/

void drawRadarRing(MapPoint radarPt,
		   char * radar_color,
		   GC gc, Display *dpy, Pixmap pix)
{
     XArc	  radar_arc;
     
     M_XYPoint 	  xylr,xyul;
     M_MapPoint   lrcorner,ulcorner;
     
     double    	  lat_radius_in_degs;
     double	  lon_radius_in_degs;
     double	  lat;
     double	  lon;
     
     lat = radarPt.loc.lat;
     lon = radarPt.loc.lon;
     
     /* meters->nmi->deg */
     lat_radius_in_degs = RADAR_COVER_M / (M_PER_NMILE*NMILE_PER_DEG_LAT) ;
     
     
     lon_radius_in_degs = RADAR_COVER_M /
	  (M_PER_NMILE*NMILE_PER_DEG_LAT*cos(RAD_PER_DEG*lat)) ;
     
     
     ulcorner.lat = lat + lat_radius_in_degs ;
     ulcorner.lon = lon - lon_radius_in_degs ;
     
     lrcorner.lat = lat - lat_radius_in_degs ;
     lrcorner.lon = lon + lon_radius_in_degs ;
     
     /*
     convert to xy coords
     */
     mConvertLatLon2XY ( ulcorner.lat , ulcorner.lon ,  
                         ( int * ) & ( xyul.x ) , 
                         ( int * ) & ( xyul.y ) ) ; 
     mConvertLatLon2XY ( lrcorner.lat , lrcorner.lon ,  
                         ( int * ) & ( xylr.x ) , 
                         ( int * ) & ( xylr.y ) ) ; 
    
     radar_arc.x = xyul.x;
     radar_arc.y = xyul.y;
     
     radar_arc.width = (unsigned short) abs(xylr.x - xyul.x);
     radar_arc.height = (unsigned short) abs(xylr.y - xyul.y); 
     
     radar_arc.angle1 = (short) 0;
     radar_arc.angle2 = (short) (360*64);
     
     /*
     if (inRangeCorner(xyul))
     {
     
	  XDrawArc(dpy, pix, gc,
		   radar_arc.x, radar_arc.y,
		   radar_arc.width, radar_arc.height,
		   radar_arc.angle1, radar_arc.angle2);
   
     }
     
     */
     
     XDrawArc(dpy, pix, gc,
		   radar_arc.x, radar_arc.y,
		   radar_arc.width, radar_arc.height,
		   radar_arc.angle1, radar_arc.angle2);
     
     return;   
} 
/***************************************************************************/


int inRangeXArc(XArc arc)
{
     
     
     if ((arc.x >= 0)     && (arc.x <= MAX_X) &&
	 (arc.y >= 0)     && (arc.y <= MAX_Y) &&
	 (arc.width > 0)  && (arc.width < MAX_X) && 
	 (arc.height > 0) && (arc.height < MAX_Y))
	  return 1;	   
     else
	  return 0;
}

/***************************************************************************/

void printM_XYPoint(M_XYPoint point)
{
     printf("point.x = %ld  point.y = %ld\n", point.x, point.y);
      
     return;     
}

/***************************************************************************/

int inRangeCorner(M_XYPoint p1)
{
     
     if (   (p1.x >= 0)  && (p1.x <= MAX_X) &&
	    (p1.y >= 0) && (p1.y <= MAX_Y) 
        )
     {
 	  return 1;
     }
     else
     {
	  return 0;
     }
  
}


/***************************************************************************/
