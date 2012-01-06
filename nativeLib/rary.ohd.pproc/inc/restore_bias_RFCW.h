
/*******************************************************************************
* FILENAME:             restore_bias_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       restore_bias_RFCW routine in the restore_bias_RFCW.c
*                       source file.
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                    PROGRAMMER         DESCRIPTION/REASON
* February 11, 2002     Moria Shebsovich      Revision
* November 4, 2004      Bryon Lawrence        Added the const qualifier to
*                                             the rid argument of the
*                                             restore_bias_RFCW function.
********************************************************************************
*/

#ifndef RESTORE_BIAS_RFCW_H
#define RESTORE_BIAS_RFCW_H

void restore_bias_RFCW ( const char * rid, const char * datetime );

#endif /* #ifndef RESTORE_BIAS_RFCW_H */



