/*******************************************************************************
* FILENAME:             read_adapt_param_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototypes for the 
*                       	read_adapt_param_RFCW,
*				popdown_viewer
*			routine in the read_adapt_param_RFCW.ec source file.
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

#ifndef READ_ADAPT_PARAM_RFCW_H
#define READ_ADAPT_PARAM_RFCW_H

#include <Xm/Xm.h>

void read_adapt_param_RFCW ( const char * rid, const char * datetime,
		             int * status ) ;
void popdown_viewer ( Widget w, XtPointer shell, XtPointer call_data) ;

#endif /* #ifndef READ_ADAPT_PARAM_RFCW_H */
