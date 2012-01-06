/*=========================================================================*/
/*                    FILE PATH/NAME:          add_overlays.c              */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   add_overlays()                     */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "map_resource.h"
#include "overlay.h"
#include "post_functions.h"
#include "show_radar_rings.h"

/***************************************************************************/
/*  FUNCTION NAME:   add_overlays                                          */
/*       FUNCTION:                                                         */
/*                                                                         */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap

Functions called:
   show_rfc_boundaries
   show_states
   show_rivers
   show_basin_boundaries
   show_cities_and_towns
   show_county
   show_radar_rings

******************************************** BEGIN add_overlays ************/

extern draw_struct * ds_array [ 4 ] ;

void add_overlays ( int ss_number )
{
   Arg wargs [ 5 ] ;
   Dimension height ;
   Dimension width ;
   Display * dpy = NULL ;
   draw_struct * pDrawMsData = NULL ;
   draw_struct * pDrawStruct = NULL ;
   int i ;
   int n ;

   /* Retrieve the display in which the application is being shown. */
   dpy = _get_map_display ( ) ;

   /* Reset each of the pixmaps representing a pane in the 
      single site radar window to its base state without
      any overlays. */
   for ( i = MsData ; i <= St1iiData ; ++ i )
   {
      pDrawStruct = & ds_array [ i ] [ ss_number ] ;
      pDrawMsData = & ds_array [ 0 ] [ ss_number ] ;

      n=0;
      XtSetArg(wargs[n], XmNwidth, &width); n++;
      XtSetArg(wargs[n], XmNheight, &height); n++;
      XtGetValues ( pDrawStruct->w , wargs , n ) ;

      XCopyArea ( dpy , pDrawStruct->pixbase , pDrawStruct->pix ,
                  pDrawStruct->gc[0] , 0 , 0 , width , height , 0 , 0 ) ;
   }
   
   if ( pDrawMsData->rfc_on == 1 )
   {
     show_rfc_boundaries ( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }
       
   if ( pDrawMsData->states_on == 1 )
   {
     MPEUtil_show_states ( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }

   if ( pDrawMsData->rivers_on == 1 )
   {
     MPEUtil_show_rivers ( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }

   if ( pDrawMsData->basins_on == 1 )
   {
     MPEUtil_show_basin_boundaries ( pDrawMsData->w , ( XtPointer ) ss_number , 
                             NULL ) ;
   }

   if ( pDrawMsData->cities_on == 1 )
   {
     MPEUtil_show_cities_and_towns ( pDrawMsData->w , ( XtPointer ) ss_number , 
                             NULL ) ;
   }

   if ( pDrawMsData->county_on == 1 )
   {
     MPEUtil_show_county ( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }
 
   if ( pDrawMsData->rings_on == 1 )
   {
     show_radar_rings( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }
   
   if ( pDrawMsData->gages_on == 1 )
   {
     show_ss_gages_RFCW ( pDrawMsData->w , ( XtPointer ) ss_number , NULL ) ;
   }
}

/********************************************* END add_overlays ************/
