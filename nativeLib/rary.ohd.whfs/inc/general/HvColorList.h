#ifndef HVCOLORLIST_H
#define HVCOLORLIST_H 

#define	MAX_NEXRAD_PRECIP_COLORS	16
#define MAX_COLOR_NAME_LENGTH 20

#include <Xm/Xm.h>
#include "ColorThreshold.h"


/* This will use some memory.  But there should only ever be one instance
   of this structure in an application.  So the memory concerns are minor. */

typedef struct HvColorList
{
     /* The color of the map's background. */
     char background_color [ MAX_COLOR_NAME_LENGTH ] ;

     /* Colors for the different possible types of gage symbols. */ 
     char gagecolor [ MAX_COLOR_NAME_LENGTH ] ,
	  nonFloodColor [ MAX_COLOR_NAME_LENGTH ] ,
	  nearFloodColor [ MAX_COLOR_NAME_LENGTH ] ,
	  floodColor [ MAX_COLOR_NAME_LENGTH ] ,
	  noDataColor [ MAX_COLOR_NAME_LENGTH ] ,
      noStageColor [ MAX_COLOR_NAME_LENGTH ] ,
      otherStationColor [ MAX_COLOR_NAME_LENGTH ] ,
	  nonHeightStationColor [ MAX_COLOR_NAME_LENGTH ] ;
     /*
     Colors for the various overlays
     */
     
     char labelColor [ MAX_COLOR_NAME_LENGTH ] ,
	  alert_color [ MAX_COLOR_NAME_LENGTH ] ,
	  asos_color [ MAX_COLOR_NAME_LENGTH ] ,
	  meso_color [ MAX_COLOR_NAME_LENGTH ] ,
	  precip_color [ MAX_COLOR_NAME_LENGTH ] ,
	  coop_color [ MAX_COLOR_NAME_LENGTH ] ,
	  riv_data_color [ MAX_COLOR_NAME_LENGTH ] ,
	  res_data_color [ MAX_COLOR_NAME_LENGTH ] ,
	  radar_color [ MAX_COLOR_NAME_LENGTH ] ,
	  state_border_color [ MAX_COLOR_NAME_LENGTH ] ,
	  water_color [ MAX_COLOR_NAME_LENGTH ] ,
	  basin_color [ MAX_COLOR_NAME_LENGTH ] ,
	  city_color [ MAX_COLOR_NAME_LENGTH ] ,
      latlon_color [ MAX_COLOR_NAME_LENGTH ] ,
      rfc_color [ MAX_COLOR_NAME_LENGTH ] ,
      timezone_color [ MAX_COLOR_NAME_LENGTH ] ,
	  road_color [ MAX_COLOR_NAME_LENGTH ] ,
	  county_color [ MAX_COLOR_NAME_LENGTH ] ,
	  zone_color [ MAX_COLOR_NAME_LENGTH ] ;

     char outside_radar_range [ MAX_COLOR_NAME_LENGTH ] ;
     
      
     unsigned long
	  nexrad_color[MAX_NEXRAD_PRECIP_COLORS];
    
     ColorThresholdArray color_threshold_array;
     
} HvColorList;


/*
	Prototypes
*/
HvColorList *getHvColorList();
void initHvColorList( HvColorList *hcl);
unsigned long get_color(Colormap cmap, char *spec);


#endif /* #ifndef HVCOLORLIST_H */
