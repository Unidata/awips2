/*******************************************************************************
* FILENAME:             edit_bias_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototypes for the 
*                       	create_editbias_popup_RFCW,
*				write_editbias_RFCW				
*			routines in the edit_bias_RFCW.c source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 12, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                  PROGRAMMER         DESCRIPTION/REASON
* February 12, 2002   Moria Shebsovich      Original Coding 
********************************************************************************
*/

#ifndef EDIT_BIAS_RFCW_H
#define EDIT_BIAS_RFCW_H

#include <Xm/Xm.h>

#include "stage3.h"

void create_editbias_popup_RFCW ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata ) ;
void write_editbias_RFCW ( Widget w, XtPointer clientdata , 
                           XtPointer calldata ) ;

#endif /* #ifndef EDIT_BIAS_RFCW_H */
