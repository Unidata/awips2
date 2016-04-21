/*******************************************************************************
* FILENAME:            restore_date.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          restore_date.h
* DESCRIPTION:         Contains the prototype for the "restore_date" routine.
*                      This routine restores the previously selected date
*                      when the "Cancel" option of the "Choose Dates" GUI 
*                      is chosen.  This routine is a callback function.  It 
*                      adheres to the standard X/Motif callback prototype.
*
* ORIGINAL AUTHOR:     Unknown
* CREATION DATE:       Unknown
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE              PROGRAMMER      DESCRIPTION/REASON
*          1        February 7, 2002  Bryon Lawrence  Modified this header
*                                                     file to include 
*                                                     documentation and
*                                                     #ifndef multiple
*                                                     include/redefinition
*                                                     protection. 
********************************************************************************
*/

#ifndef RESTORE_DATE_H
#define RESTORE_DATE_H

#include <Xm/Xm.h>

void restore_date ( Widget w , XtPointer clientdata , XtPointer calldata ) ;

#endif /* #ifndef RESTORE_DATE_H */
