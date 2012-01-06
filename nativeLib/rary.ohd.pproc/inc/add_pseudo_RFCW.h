/*******************************************************************************
* FILENAME:             add_pseudo_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototypes  for the 
*                       following routines 
*				locate_pseudo_RFCW,
* 				create_pseudo_popup_RFCW, 
* 				write_pseudo_RFCW,
* 				add_pseudo_RFCW,
* 				popdown_pseudo_RFCW
*			in the add_pseudo_RFCW.c source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                    PROGRAMMER          DESCRIPTION/REASON
* February 11, 2002     Moria Shebsovich       Original Coding 
********************************************************************************
*/

#ifndef ADD_PSEUDO_RFCW_H
#define ADD_PSEUDO_RFCW_H

#include <Xm/Xm.h>
#include "stage3.h"

void locate_pseudo_RFCW( Widget w, XtPointer clientdata,  XEvent * event , Boolean *continue_to_dispatch_return ) ;
void create_pseudo_popup_RFCW ( pseudo_struct * pseudo ) ; 
void write_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata ) ;
void add_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata ) ;
void popdown_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata ) ;

#endif /* #ifndef ADD_PSEUDO_RFCW_H */
