
/*******************************************************************************
* FILENAME:              display_field_data_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:           This file contains user defined enumeration 
*			 data type and the prototype
*                        of the display_field_data_RFCW routine.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         February 12, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER       DESCRIPTION/REASON
* February 27, 2002  Moria Shebsovich Revision
* March 8, 2004      Bryon Lawrence   Added display_mlMosaic member to the
*                                     DisplayFieldData enumeration.  This is
*                                     for the new local biased multi-sensor
*                                     mosaic product.
********************************************************************************
*/

#include "mpe_field_names.h"
#include "stage3.h"

#ifndef DISPLAY_FIELD_DATA_RFCW_H
#define DISPLAY_FIELD_DATA_RFCW_H

void display_field_data_RFCW ( enum DisplayFieldData, 
			       int ** data_array_tmp , date_struct date ,
			       int addition_flag ) ;

void display_field_read_xmrg ( int ** data_array_tmp , char * fname ,
			       int addition_flag ) ;

void display_field_read_spe ( int ** data_array_tmp , char * fname ,
                              int addition_flag ) ;

#endif /* #ifndef DISPLAY_FIELD_DATA_RFCW_H */
