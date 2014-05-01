
/*******************************************************************************
* FILENAME:         ItemChooserDialog.h
* DESCRIPTION:      This file contains the prototypes and includes
* 					for the generic item chooser dialog
* 					Xtools.h contains the prototype for show_item_chooser()
* ORIGINAL AUTHOR:  Chip Gobs
* CREATION DATE:    December 12, 2005
* ORGANIZATION:     OHD / HSEB
* MACHINE:          Linux
* MODIFICATION HISTORY:
*  DATE         PROGRAMMER        DESCRIPTION/REASON
*  12/19/2005	Chip Gobs	     Original Coding
********************************************************************************
*/

#ifndef ITEM_CHOOSER_DIALOG_H
#define ITEM_CHOOSER_DIALOG_H

#include "Xtools.h"


void loadList(LongText * itemStringArray,
	      int selectedPositionArray[], int itemCount);						   
void ic_apply(Widget w, XtPointer ptr, XtPointer cbs);
void ic_close(Widget w, XtPointer ptr, XtPointer cbs);
void ic_cleanup();
							   
#endif /* #ifndef ITEM_CHOOSER_DIALOG_H */
