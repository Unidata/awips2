/*******************************************************************************
* FILENAME:             read_rresult.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       read_rresult routine in the read_rresult.ec
*                       source file.
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 6, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                 PROGRAMMER        DESCRIPTION/REASON
* February 6, 2002     Bryon Lawrence    Original Coding 
********************************************************************************
*/

#ifndef READ_RRESULT_H
#define READ_RRESULT_H

#include "read_rresult.h"

enum RadarIgnoreFlag { DontIgnoreRadar , IgnoreRadar } ;

void read_rresult ( char * datetime ) ;

void read_DAArresult ( char * datetime ) ;

#endif /* #ifndef READ_RRESULT_H */
