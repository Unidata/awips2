/*******************************************************************************
* FILENAME:             update_rwr_save.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*			update_rwr_save routine in the update_rwr_save.c i
*                       source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell-Linux 
* MODIFICATION HISTORY:
* DATE                  PROGRAMMER         DESCRIPTION/REASON
* February 11, 2002     Bryon Lawrence     Original Coding 
* November 5, 2004      Bryon Lawrence     Added const qualifiers to 
*                                          rfc, dt, and fldtype 
*                                          arguments.
********************************************************************************
*/

#ifndef UPDATE_RWR_SAVE_H
#define UPDATE_RWR_SAVE_H

#include "date_struct_variable.h"

void update_rwr_save ( const char * rfc , const date_struct * dt , 
		       const char * fldtype , long int * irc ) ;

#endif /* #ifndef UPDATE_RWR_SAVE_H */
