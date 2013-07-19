/*******************************************************************************
* FILENAME:            draw_precip_poly_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototypes for the following routines in
*                      the draw_precip_poly_RFCW.c file:
*                      exit_draw_precip_RFCW
*                      read_draw_precip_value_RFCW
*                      setup_draw_precip_RFCW
*                      start_end_rubber_poly_RFCW
*                      write_draw_precip_data_RFCW
*		       raise_draw_precip_value_RFCW
*		       lower_draw_precip_value_RFCW
*                      setSubPrecipValueRFCW
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 11, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE              PROGRAMMER        DESCRIPTION/REASON
* February 11, 2002  Bryon Lawrence    Original Coding
********************************************************************************
*/
#ifndef DRAW_PRECIP_POLY_RFCW_H
#define DRAW_PRECIP_POLY_RFCW_H

#include <Xm/Xm.h>

#include "polygon_RFCW.h"

void apply_edit_polygons ( int ** precip_data_array,
                           const char * cdate,
                           int year,
                           int month,
                           int day,
                           int hour,
                           double scale,
                           double factor,
                           enum DisplayFieldData field,
                           int rows,
                           int cols,
                           int xorigin,
                           int yorigin,
                           int add_flag,
                           int draw_only_persistent ); 

void closePolygon( rubber_poly_data * poly_struct );

void free_poly_temp ( );

void show_display_edit_precip_polyDS ( Widget w ) ;

void display_edit_precip_poly_callbacks ( ) ;

void read_draw_precip_value_RFCW ( Widget w,  XtPointer poly_struct, 
                                   XtPointer calldata) ;

void process_draw_precip_value_RFCW ( Widget w , XtPointer clientdata ,
                                      XtPointer calldata );

void popdown_draw_precip_value_gui ( ) ;

void setSubPrecipValueRFCW ( Widget w, XtPointer client_data, 
                             XtPointer calldata) ;

void setup_draw_precip_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata ) ;

void start_end_rubber_poly_RFCW ( Widget w , XtPointer clientdata , 
                                  XEvent * event ,
                                  Boolean * continue_to_dispatch_return ) ;

void exit_draw_precip_RFCW ( Widget w , XtPointer clientdata , 
                              XtPointer calldata ) ;

void write_draw_precip_data_RFCW ( Widget w , XtPointer clientdata ,
                                   XtPointer calldata ) ;

void popdown_draw_precip_value_RFCW ( Widget w ,  XtPointer shell , 
                                      XtPointer call_data ) ;

void read_edit_precip_value( Widget w, rubber_poly_data * poly_struct, 
                             caddr_t * call_data) ;

void select_poly_for_edit ( Widget w, XtPointer clientdata ,
                            XEvent * event , Boolean * continue_to_dispatch ) ;

void cancel_edit ( Widget w , rubber_poly_data * data , caddr_t call_data ) ;

void get_poly_action ( const rubber_poly_data * pPolygonNode,
                       char * action, char * value );

void free_polygon_list ( ) ;

int InOutPoly( int x , int y , const rubber_poly_data * data);

#endif /* #ifndef DRAW_PRECIP_POLY_RFCW_H */
