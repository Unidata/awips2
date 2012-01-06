/*******************************************************************************
* FILENAME:              edit_radcov.h
* GENERAL INFORMATION:
* DESCRIPTION:           This file contains the prototypes
*                        of the save_edit_radcov and 
*                        create_edit_radcov_interface routines.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         February 12, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER      DESCRIPTION/REASON
* February 12, 2002  Bryon Lawrence  Original Coding
********************************************************************************
*/

#ifndef EDIT_RADCOV_H
#define EDIT_RADCOV_H

void save_edit_radcov ( Widget w , XtPointer clientdata, XtPointer calldata) ;
void create_edit_radcov_interface ( int site ) ;

#endif /* #ifndef EDIT_RADCOV_H */
