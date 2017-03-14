#include "ArealData.h"
#include "HvColorList.h"
#include "map.h"
#include "map_resource.h"

/********************************************************************************/

void drawBoundedArealData(HvDisplayControl *hdc)
{
     int	i;
     const char * color;
     char areaColor [ MAX_COLOR_NAME_LENGTH ] ; 
     Display *display = _get_map_display ( ) ;
     GC	gc       = _get_map_gc ( ) ;
     Pixmap pix = _get_map_pixmap ( ) ;
     HvColorList *hcl = getHvColorList();
  
     if (hdc->areas != NULL)
     {

	  for (i = 0; i < hdc->numAreas; i++)
	  { 
	       color = determineColorByThreshold(hdc->areas[i].value1,
					                         MISSING,
                                            &hdc->displaySettings.areal.ctArray );
                                            
               memset(areaColor, '\0', MAX_COLOR_NAME_LENGTH);
               strncpy ( areaColor , color , MAX_COLOR_NAME_LENGTH - 1 ) ; 
               areaColor [ MAX_COLOR_NAME_LENGTH ] = '\0' ;
	       
	       /*
	       draw only the polygon (and maybe fill it)
	       */
	       drawMapArea(&hdc->areas[i],
			   hcl->labelColor,
			   areaColor, 
			   True,
			   hdc->displaySettings.areal.fillArea, 
			   False,
			   False,
			   False,
			   False,
			   gc, display, pix);
	  }
	  

	  for (i = 0; i < hdc->numAreas; i++)
	  { 
 
	       /*
	       draw only the labels, no area
	       */
	       drawMapArea(&hdc->areas[i],
			   hcl->labelColor,
			   areaColor, 
			   False, 
			   False,
			   hdc->displaySettings.areal.showId,
			   hdc->displaySettings.areal.showName,
			   hdc->displaySettings.areal.showValue1,
			   hdc->displaySettings.areal.showValue2,
			   gc, display, pix);
	  }

	  
     }
     return;  
     
}

/***************************************************************************/


long getMatchingAreaIndex(char *lid, MapArea *areas, long numAreas)
{
     long index = -1;     
     long i;
     
     for (i = 0; i < numAreas; i++)     
     {
	    if (strcmp(areas[i].id, lid) == 0)	  
	    {
		 index = i;
		 break;
	    }
     }
     
     return index;
}

/************************************************************************/
