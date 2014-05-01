/*******************************************************************************
* FILENAME:
* GENERAL INFORMATION:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef DRAWMPELEGEND_H
#define DRAWMPELEGEND_H

#include <Xm/Xm.h>

#include "map.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"

void dontDrawHvLegendPointInfo ( ) ;
void drawHvLegend ( int * map_index ) ;
 
void drawHvLegendPointInfo ( ) ;
enum MapState getHvDrawLegendStatus ( ) ;
Widget getHvLegendHrapBasinText ( ) ;
Widget getHvLegendHrapCountyText ( ) ;
Widget getHvLegendHrapXYText ( ) ;
Widget getHvLegendHrapValueText ( ) ;


void draw_mpe_legend(int * map_index, int offset,
                     enum MapState mpe_data_flag,
                     enum DisplayFieldData display_field_type,
                     unsigned int width,
                     unsigned int height,
                     int y);

void draw_pdc_legend(int * map_index, int offset,
                     unsigned int width,
                     unsigned int height);
  
  
void draw_pdc_legend_color_bar(int * map_index,
                     unsigned int bar_width,
                     unsigned int bar_height,
                     int value_text_y,
                     int bar_y,
                     int value_text_left_shift);  
                     
#endif /* #ifndef DRAWMPELEGEND_H */
