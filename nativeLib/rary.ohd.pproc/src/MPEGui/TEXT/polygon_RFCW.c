/*=========================================================================*/
/*                         FILE NAME:  polygon_RFCW.c                      */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   start_end_rubber_poly_RFCW()       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>

#include "polygon_RFCW.h"
#include "drawa.h"
#include "draw_precip_poly_RFCW.h"
#include "List.h"
#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"

#include "stage3.h"
#include "stage3_globals.h"
#include "stage3_interface.h"
#include "Xtools.h"

extern int draw_poly_flag;
int draw_precip_value_popup_RFCW_Up = 0;

/* Polygon state variables.  These were
 moved here so that a polygon could be
 automatically closed if the user
 forgets to close it and then
 performs an operation on it. */
static Boolean polygon_closed = True;
static XPoint orig_point;
static XPoint prev_point;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/* Algorithm to recursively determine the coordinates of an out-of-
 bounds polygon vertex when it is moved to a valid position on
 the boundary of the MPE area's HRAP grid. */
/***************************************************************************/
/*  FUNCTION NAME:   create_valid_vertex ( )                               */
/*       FUNCTION:   Algorithm to recursively calculate the coordinates    */
/*                   of an out-of-bounds polygon vertex so that it is      */
/*                   moved to a valid position on the  boundary of the     */
/*                   HRAP grid representing the MPE forecast area.         */
/***************************************************************************

 Function type:
 void

 Called by function:
 (callback) button press in main window

 Functions called:

 ***************************************** BEGIN start_end_rubber_poly_RFCW ***/

static HRAP create_valid_vertex(float x, float y, float prev_x, float prev_y,
        float minx, float maxx, float miny, float maxy)
{
    float intercept;
    float slope;
    float temp_x;
    float temp_y;

    HRAP snapped_point;

    temp_x = x;
    temp_y = y;

    if (x < minx || x > maxx)
    {
        if (x < minx)
        {
            x = minx;
        }
        else
        {
            x = maxx;
        }

        if (prev_x != minx && prev_x != maxx)
        {
            /* Compute the slope of the line connecting the current point
             with the previous point. */
            if ( (prev_y - temp_y ) != 0)
            {
                slope = (prev_y - temp_y ) / (prev_x - temp_x );

                /* Compute the hrap y intercept. */
                intercept = prev_y - slope * prev_x;

                /* Compute the new hrap y coordinate for a hrap x of minx. */
                y = slope * x + intercept;
            }
        }

        snapped_point = create_valid_vertex(x, y, prev_x, prev_y, minx, maxx,
                miny, maxy) ;
    }
    else if (y < miny || y > maxy)
    {
        if (y < miny)
        {
            y = miny;
        }
        else
        {
            y = maxy;
        }

        if (prev_y != miny && prev_y != maxy)
        {
            /* Compute the slope of the line connecting the current point
             with the previous point. */
            /* Be careful about the undefined slope with a vertical line. */

            if ( (prev_x - temp_x ) != 0)
            {
                slope = (prev_y - temp_y ) / (prev_x - temp_x );

                /* Compute the hrap y intercept. */
                intercept = prev_y - slope * prev_x;

                /* Compute the new hrap x coordinate. */
                x = (y - intercept ) / slope;
            }
        }

        snapped_point = create_valid_vertex(x, y, prev_x, prev_y, minx, maxx,
                miny, maxy) ;
    }
    else
    {
        snapped_point.x = ( int ) x ;
        snapped_point.y = ( int ) y ;
    }

    return snapped_point;
}

/***************************************************************************/
/*  FUNCTION NAME:   start_end_rubber_poly_RFCW()                          */
/*       FUNCTION:   either start a new polygon or close the current       */
/*                    one depending on button push                         */
/*                   if drawing polygons in main window, right button      */
/*                    press pops up slider bar for defining precip value   */
/***************************************************************************

 Function type:
 void

 Called by function:
 (callback) button press in main window

 Functions called:

 ***************************************** BEGIN start_end_rubber_poly_RFCW ***/

void start_end_rubber_poly_RFCW(Widget w, XtPointer clientdata, XEvent * event,
        Boolean * continue_to_dispatch_return)
{
    clicks * mouse_event = (clicks * ) event ;
    point current_point;
    point hrap_point;
    rubber_poly_data * data = ( rubber_poly_data * ) clientdata ;

    float latit, longt;
    HRAP hrap;
    HRAP new_hrap;
    char * msg = "Cursor out of Range";

    /*------------------------------------------------------------------*/
    /*     If button one (left) was pressed and npoints == 0,           */
    /*      are starting new polygon.                                   */
    /*     If button one (left) was pressed and npoints > 0,            */
    /*      are adding new line to polygon.                             */
    /*     If other button (> 1) was pressed, close polygon.            */
    /*     If drawing polygons in main window, right button press pops  */
    /*      up slider bar for defining precip value.                    */
    /*------------------------------------------------------------------*/

    current_point.x = mouse_event->x;
    current_point.y = mouse_event->y;

    mConvertXY2LatLon(current_point.x, current_point.y, &latit, &longt) ;
    hrap = LatLongToHrapMpe(latit, (-1) * longt) ;
    
    hrap.x -= XOR;
    hrap.y -= YOR;
    
            
    if ((hrap.x < 0 ) || (hrap.x > MAXX ) || (hrap.y < 0 ) || (hrap.y > MAXY ))
    {
        /* Check the position of the previous polygon point.  If the polygon
         point is not on the MPE area boundary, then the out of bounds point will
         be placed on the MPE area boundary based on a line drawn between it and
         the previous point.

         If the previous point is on the MPE boundary, then place the current
         point on the mpe_boundary. */

        if (data->npoints == 0)
        {
            /* There is no previous point.  Do not allow the user to initiate 
             a polygon outside of the MPE area. */
            logMessage("Error: Cursor Out Of Range: \n");
            InfoDialog(w, msg) ;
            return;
        }
        else
        {
            hrap = create_valid_vertex( hrap.x ,
                            hrap.y ,
                            data->hrap [ data->npoints - 1 ].x ,
                            data->hrap [ data->npoints - 1 ].y ,
                            ( float ) 0 , ( float ) MAXX , ( float ) 0 ,
                            ( float ) MAXY ) ;

            /* Recompute the pixel point based on this new HRAP value. */
            hrap_point.x = ( int ) hrap.x + XOR;
            hrap_point.y = ( int ) hrap.y + YOR;
            new_hrap = HrapToLatLongMpe(hrap_point) ;
            mConvertLatLon2XY(new_hrap.y, -1 * new_hrap.x, &current_point.x,
                    &current_point.y) ;
        }
    }

    if (polygon_closed == True)
    {
        polygon_closed = False;
        data->npoints = 0;
        data->max_points = MAX_POINTS_IN_POLYGON;
        num_draw_precip_poly++;

        /* Allocate memory for the array of Polygon Points. */
        data->hrap = ( HRAP * ) malloc(data->max_points * sizeof(HRAP));

        if (data->hrap == NULL)
        {
            logMessage("Polygon point memory allocation error. Exiting.\n");
            exit( 1);
        }
    }

    /* Test if the array of polygon points needs to be resized. */
    if (data->npoints >= data->max_points)
    {
        data->max_points += MAX_POINTS_IN_POLYGON;
        data->hrap = ( HRAP * ) realloc(data->hrap, data->max_points
                * sizeof(HRAP));

        if (data->hrap == NULL)
        {
            logMessage("Polygon point memory reallocation error. Exiting.\n");
            exit( 1);
        }
    }

    data->hrap [ data->npoints ].x = hrap.x;
    data->hrap [ data->npoints ].y = hrap.y;

    if (data->npoints == 0) /*--- starting a new polygon ----*/
    {
        orig_point.x = mouse_event-> x;
        orig_point.y = mouse_event-> y;
        prev_point.x = mouse_event-> x;
        prev_point.y = mouse_event-> y;
    }

    /*--------------------------------------------------------------*/
    /*     Draw line - should just be a dot for a new polygon       */
    /*--------------------------------------------------------------*/
    mSetColor("White") ;

    mDrawLine(M_MAP, M_INDEX, prev_point.x, prev_point.y, current_point.x,
            current_point.y);

    prev_point.x = current_point.x;
    prev_point.y = current_point.y;

    data->npoints ++;

    /*--------------------------------------------------------------*/
    /*     Other than left button pushed.                           */
    /*--------------------------------------------------------------*/

    if (mouse_event->first_button_press == 2 || mouse_event->first_button_press
            == 3)
    {
        /* The polygon has been closed. */
        closePolygon(data);

    }

    _map_expose(w, ( XtPointer ) M_INDEX, NULL);

}

/*******************************************************************************
 * MODULE NAME:  close_polygon
 * PURPOSE:      Close and complete the edit polygon.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
 *	None
 * 
 * RETURNS:
 *   DATA TYPE   NAME                        DESCRIPTION
 *   None
 * 
 * APIs UTILIZED:
 *   NAME                             HEADER FILE             DESCRIPTION
 *   mDrawLine                        map_library.h           Draw a line between
 *                                                            two points.
 *   show_display_edit_precip_polyDS  draw_precip_poly_RFCW.h Launches the edit
 *                                                            precipitation window.
 * 
 * LOCAL DATA ELEMENTS (OPTIONAL):
 *   DATA TYPE  NAME                         DESCRIPTION
 *   None
 * 
 * DATA FILES AND/OR DATABASE:
 *   None
 * 
 * ERROR HANDLING:
 *    ERROR CODE                             DESCRIPTION
 *   None
 ********************************************************************************
 */
void closePolygon(rubber_poly_data * data)
{
    if (polygon_closed == False)
    {
        /* Only perform this operation if the polygon has
         * already been closed.
         */
        polygon_closed = True;

        /*--------------------------------------------------------------*/
        /*     Draw a second line from the initial point in polygon to  */
        /*     close the loop.                                          */
        /*--------------------------------------------------------------*/
        mDrawLine(M_MAP, M_INDEX, prev_point.x, prev_point.y, orig_point.x,
                orig_point.y);

        /* If in edit precipitation polygon mode,
         * then launch the Edit precipitation polygon
         * window. */
        if ( !draw_precip_value_popup_RFCW_Up)
        {
            show_display_edit_precip_polyDS(toplevel) ;
            draw_precip_value_popup_RFCW_Up = 1;
        }

        /* Initialize all of the new polygon's state variables 
         * to default values. */
        data->set_flag = False;
        data->raise_flag = False;
        data->lower_flag = False;
        data->scale_flag = False;
        data->draw_source = display_subValue;
        data->polygon_number = num_draw_precip_poly;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
