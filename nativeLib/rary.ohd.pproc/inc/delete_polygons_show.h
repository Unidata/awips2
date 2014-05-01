/*******************************************************************************
* FILENAME:            delete_polygons_show.h
* DESCRIPTION:         Contains the prototype for the
*                      show_deletepolygonDS routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 13, 2005
* ORGANIZATION:        OHD/HSEB/WHFS
* MACHINE/OS:          IBM/Redhat Linux 
* MODIFICATION HISTORY:
*    DATE           PROGRAMMER        DESCRIPTION/REASON
*    July 3, 2005   Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef DELETE_POLYGONS_SHOW_H
#define DELETE_POLYGONS_SHOW_H

#include <Xm/Xm.h>

#include "mpe_field_names.h"
#include "polygon_RFCW.h"

void add_polygon ( rubber_poly_data * pPolyNode );

const char * check_for_polygon_file ( enum DisplayFieldData field,
                                      const char * cdate,
                                      int * status );

void get_display_fieldname ( enum DisplayFieldData field_type,
                             char * field );

void get_polygons ( enum DisplayFieldData field,
                    List * pPolyList,
                    const char * cdate );

void get_snow_polygons ( List * pPolyList, const char * cdate );

void show_deletepolygonDS ( Widget w, XtPointer clientdata,
                            XtPointer calldata );

void update_polygon_list ( );

void write_polygons ( const rubber_poly_data * pPolyHead ,
                      enum DisplayFieldData field,
                      const char * cdate );
#endif
