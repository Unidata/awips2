/*******************************************************************************
* FILENAME:             save_rfcwide.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       	save_rfcwide,
* 				save_merged_RFCW,
*				void save_gif_rfcwide 
*			routines in the save_rfcwide.c source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                  PROGRAMMER         DESCRIPTION/REASON
* February 11, 2002   Moria Shebsovich      Original Coding 
* November 17, 2006   Bryon Lawrence        Added pproc
********************************************************************************
*/

#ifndef SAVE_RFCWIDE_H
#define SAVE_RFCWIDE_H

#include <Xm/Xm.h>

void save_rfcwide ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void save_merged_RFCW ( char filename [ ], int len, int map_number, 
                        char * proc_flag, long * irc ) ;

void save_gif_rfcwide ( char fname [ ] ) ;

#endif /* #ifndef SAVE_RFCWIDE_H */
