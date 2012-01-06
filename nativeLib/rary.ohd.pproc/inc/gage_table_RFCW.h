/*******************************************************************************
* FILENAME:              gage_table_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:           This header file contains the prototypes for the
*                        create_gage_table_RFCW, table_data_RFCW, 
*                        change_rc_RFCW, sort_RFCW, and v_scrollbar_moved_RFCW
*                        routines.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         February 7, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE                  PROGRAMMER          DESCRIPTION/REASON
* Feburary 7, 2002      Bryon Lawrence      Original Coding
********************************************************************************
*/

#ifndef GAGE_TABLE_RFCW_H
#define GAGE_TABLE_RFCW_H

#include <Xm/Xm.h>

#include "display_field_data_RFCW.h"
#include "menus.h"
#include "stage3.h"

void change_rc_RFCW ( cb_struct * cb_data ) ;

void create_gage_table_RFCW ( draw_struct * data ) ;

void free_gage_table_memory ( ) ;

void init_gage_table_memory ( ) ;

void sort_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata ) ;

char * table_data_RFCW ( int i , int j ) ;

void v_scrollbar_moved_RFCW ( Widget w , XtPointer clientdata , 
                              XtPointer calldata ) ;
void lookup_mosaic_data ( int ** data_array_tmp , gage_struct *gage ,
                              enum DisplayFieldData display_array ) ; 
void lookup_radar_index ( int ** data_array_field , gage_struct * gage );	

void data_array_free_memory ( int *** data_array_tmp ) ;   

#endif /* #ifndef GAGE_TABLE_RFCW_H */
