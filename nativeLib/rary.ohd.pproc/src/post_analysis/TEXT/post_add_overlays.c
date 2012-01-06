/*=========================================================================*/
/*                    FILE PATH/NAME:  post_add_overlays.c         */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   add_overlays()                     */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "stageiii_structures.h"
#include "overlay.h"
#include <stdio.h>

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

void  post_analysis_add_overlays();

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/post_add_overlays.c                      */
/*  FUNCTION NAME:   add_overlays                                          */
/*       FUNCTION:                                                         */
/*                                                                         */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap

Functions called:
   show_states
   show_rivers
   show_basin_boundaries
   show_cities_and_towns
   show_county

*************************************** BEGIN add_overlays ************/

void post_analysis_add_overlays(data)
   draw_struct *data;
{
 if (data->states_on == 1)
    {
     data->states_on = 0;
     show_states(data->w,data,NULL);
    }
 if (data->rivers_on == 1)
    {
     data->rivers_on = 0;
     show_rivers(data->w,data,NULL);
    }
 if (data->basins_on == 1)
    {
     data->basins_on = 0;
     show_basin_boundaries(data->w,data,NULL);
    }
 if (data->cities_on == 1)
    {
     data->cities_on = 0;
     show_cities_and_towns(data->w,data,NULL);
    }
 if (data->county_on == 1)
    {
     data->county_on = 0;
     show_county(data->w,data,NULL);
    }


}

/**************************************** END add_overlays ************/
