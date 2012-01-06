/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:  This file contains the prototype for the 
*                       rerun_rfcwgen routines in the rerun_rfcwgen.c
*                       source file.
* DESCRIPTION:
*
* ORIGINAL AUTHOR:	Hmap_mpe Team
* CREATION DATE:	February 11, 2002
* ORGANIZATION:		OHD / HSEB
* MACHINE:		HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE                  PROGRAMMER        DESCRIPTION/REASON
* February 11, 2002     Bryon Lawrence    Original Coding 
********************************************************************************
*/

#ifndef RERUN_RFCWGEN_H
#define RERUN_RFCWGEN_H

#include <Xm/Xm.h>

void rerun_rfcwgen ( Widget w , XtPointer clientdata , XtPointer calldata ) ;

void set_mpe_editor_call();
void unset_mpe_editor_call();
#endif /* #ifndef RERUN_RFCWGEN_H */
