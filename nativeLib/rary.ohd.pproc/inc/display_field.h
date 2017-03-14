/*******************************************************************************
* FILENAME:              display_field.h
* GENERAL INFORMATION:
* DESCRIPTION:           Contains the prototypes for the "display_field"
*                        and "display_field_free_memory" routines.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         February 27, 2002
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP Unix / Dell Linux
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
* 2/27/02      Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef DISPLAY_FIELD_H
#define DISPLAY_FIELD_H

#include <Xm/Xm.h>

int display_mpe_data ( int map_index ) ;
void display_field ( char fname [ ] , int len_fname, int map_index ) ;
void display_field_free_memory ( ) ;
void initialize_display_memory ( int map_index );

#endif /* #ifndef DISPLAY_FIELD_H */
