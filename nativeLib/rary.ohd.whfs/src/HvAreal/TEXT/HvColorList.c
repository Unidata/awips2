/*
	File:		hv_mainColors.c
	Date:		3/19/97
	Author:		Chip Gobs
	
	Purpose:	This routine allocates some colorcells 
			for this app's use
   
   			NOTE: the color allocation scheme for the 
   			whole system needs to be re-examined!
*/

#include <string.h>
#include "HvColorList.h"
#include "map_resource.h"

/**********************************************************************/

HvColorList *getHvColorList()
{
 	static HvColorList hvColorList ;
        static int initialized = 0 ;

        if ( initialized == 0 )
        {
           initHvColorList ( & hvColorList ) ;
           initialized = 1 ;
        }
	
	return &hvColorList;
}

/**********************************************************************/

void initHvColorList( HvColorList *hcl)
{
      	/* Colormap cmap; */
        memset ( hcl->background_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->background_color , "Black" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
	/*
		River gage colors
	*/
        memset ( hcl->gagecolor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->gagecolor , "Black" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->nonFloodColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->nonFloodColor , "Green" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->nearFloodColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->nearFloodColor , "Yellow" ,
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->floodColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->floodColor , "Red" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->noDataColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->noDataColor , "Gray75" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->noStageColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->noStageColor , "ForestGreen" , MAX_COLOR_NAME_LENGTH ) ;
        memset ( hcl->nonHeightStationColor  , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->nonHeightStationColor  , "Green" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->otherStationColor  , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->otherStationColor  , "Red" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
	
        memset ( hcl->nonHeightStationColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->nonHeightStationColor , "Orange" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
	   
	/*
		Color of label strings
	*/
        memset ( hcl->labelColor , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->labelColor , "White" , MAX_COLOR_NAME_LENGTH - 1 ) ;
	
	/*
		Political boundary color, (counties)
	*/
        memset ( hcl->state_border_color , '\0' , 
                 MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->state_border_color , "White" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
	
	/*
		Colors of gage types
	*/
        memset ( hcl->alert_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->alert_color , "Purple" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->asos_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->asos_color , "DeepPink" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->meso_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->meso_color , "Maroon" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->precip_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->precip_color , "Pink" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->coop_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->coop_color , "Orange" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->riv_data_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->riv_data_color , "DeepSkyBlue" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->res_data_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->res_data_color , "DarkOrchid" , 
        MAX_COLOR_NAME_LENGTH - 1 ) ;
	
	/*
		Radar coverage color
	*/
        memset ( hcl->radar_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->radar_color , "Red" , MAX_COLOR_NAME_LENGTH - 1 ) ;
	
	/*
		Colors of map overlays
	*/
        memset ( hcl->water_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->water_color , "Royalblue" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->basin_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->basin_color , "Gray75" , 
                  MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->county_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->county_color , "Gray75" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->zone_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->zone_color , "Gray75" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->city_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->city_color , "White" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->latlon_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->latlon_color , "Orange" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->rfc_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->rfc_color , "Pink" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->timezone_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->timezone_color , "Grey" , MAX_COLOR_NAME_LENGTH - 1 ) ;
        memset ( hcl->road_color , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->road_color , "Purple" , MAX_COLOR_NAME_LENGTH - 1 ) ;
	
        /* The color assigned to grid boxes that lie outside of the
           range of a radar site. */
        memset ( hcl->outside_radar_range , '\0' , MAX_COLOR_NAME_LENGTH ) ;
        strncpy ( hcl->outside_radar_range , "Black" ,
                  MAX_COLOR_NAME_LENGTH - 1 ) ;

	/*
	nexrad_color[0] = gray75;

  	nexrad_color[1] = get_color(cmap,"#9C9C9C");
 	nexrad_color[2] = get_color(cmap,"#767676");
  	nexrad_color[3] = get_color(cmap,"#FAAAAA");
   	nexrad_color[4] = get_color(cmap,"#EE8C8C");
   	nexrad_color[5] = get_color(cmap,"#C97070");
   	nexrad_color[6] = get_color(cmap,"#00FB90");
   	nexrad_color[7] = get_color(cmap,"#00BB00");
	nexrad_color[8] = get_color(cmap,"#FFFF70");
  	nexrad_color[9] = get_color(cmap,"#D0D060");
	nexrad_color[10] = get_color(cmap,"#FF6060");
	nexrad_color[11] = get_color(cmap,"#DA0000");
	nexrad_color[12] = get_color(cmap,"#AE0000");
	nexrad_color[13] = get_color(cmap,"#0000FF");
	nexrad_color[14] = get_color(cmap,"#FFFFFF");
	nexrad_color[15] = get_color(cmap,"#E700FF");
	*/
	
	return;
}

/**********************************************************************/


unsigned long get_color(Colormap cmap, char *spec)
{
     int status;
     XColor color;
     Display *display;
     
     display = _get_map_display ( ) ;
     
     
     status = XParseColor(display, cmap, spec, &color);
     status = XAllocColor(display, cmap, &color);
     
     return (color.pixel);
   
}

/**********************************************************************/
