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

#include "map_defines.h"

void dontDrawMpeLegendPointInfo ( ) ;
void drawMpeLegend ( int * map_index ) ;
void drawMpeLegendPointInfo ( ) ;
enum MapState getMpeDrawLegendStatus ( ) ;
Widget getMpeLegendHrapBasinText ( ) ;
Widget getMpeLegendHrapCountyText ( ) ;
Widget getMpeLegendHrapXYText ( ) ;
Widget getMpeLegendHrapValueText ( ) ;

#endif /* #ifndef DRAWMPELEGEND_H */
