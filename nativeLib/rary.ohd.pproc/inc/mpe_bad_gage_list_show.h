/*******************************************************************************
* FILENAME:            mpe_bad_gage_list_show.h
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*            MODULE 1: 
*         DESCRIPTION:
*
* ORIGINAL AUTHOR:     Ram Varma
* CREATION DATE:       May 07, 2007
* ORGANIZATION:        OHD HSEB
* MACHINE:             HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE           PROGRAMMER        DESCRIPTION/REASON
*          1        May 07, 2007 Ram Varma    Original Coding
********************************************************************************
*/

#include <stdlib.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Scale.h>
#include <Xm/Xm.h>

#include "mpe_bad_gage_list.h"

void show_mpe_bad_gage_list(Widget w, XtPointer calldata , XtPointer clientdata);

void mpe_bad_gage_list_callbacks();

void init_mpe_bad_gage_list();

void deleteSelectedStationCallback(Widget w , XtPointer calldata , XtPointer clientdata );

void okCallback( Widget w , XtPointer calldata , XtPointer clientdata );

void closeCallback (Widget w , XtPointer calldata , XtPointer clientdata );
