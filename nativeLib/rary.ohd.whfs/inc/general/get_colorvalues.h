/*******************************************************************************
* FILENAME:             get_colorvalues.h
* DESCRIPTION:          Contains the prototype for the get_colorvalues
*                       routine.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        September 5, 2006
* ORGANIZATION:         OHD-11, HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*       DATE         PROGRAMMER        DESCRIPTION/REASON
*       9/5/2006     B. Lawrence       Original Coding 
********************************************************************************
*/

#ifndef GET_COLORVALUES_H
#define GET_COLORVALUES_H

#include "ColorValue.h"
#include "NamedColorSetGroup.h"

ColorValue * get_colorvalues ( const char * user_id , 
                               const char * application_name ,
                               const char * color_use_name ,
                               int duration , 
                               char threshold_unit ,
                               int  * numcol ,
                               int  * numlev,
                               const NamedColorSetGroup * pColorSetGroup );


#endif /* #ifndef GET_COLORVALUES_H */
