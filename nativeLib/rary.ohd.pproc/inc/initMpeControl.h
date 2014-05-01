/*******************************************************************************
* FILENAME:              initMpeControl.h
* GENERAL INFORMATION:
* DESCRIPTION:           This file contains the prototype and any 
*                        user-defined types associated with the
*                        initMpeControl routine.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         February 11, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER      DESCRIPTION/REASON
* February 11, 2002  Bryon Lawrence  Original Coding
********************************************************************************
*/

#ifndef INITMPECONTROL_H
#define INITMPECONTROL_H

#include <Xm/Xm.h>

#include "HydroStatus.h"
#include "map_defines.h"

HydroStatus initMpeControl ( Widget w , GC * pGraphicsContext ) ;

#endif /* #ifndef INITMPECONTROL_H */
