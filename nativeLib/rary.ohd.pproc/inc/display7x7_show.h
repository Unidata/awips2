/*******************************************************************************
* FILENAME:        display7x7_show.h
* DESCRIPTION:     This header file contains the prototypes for the following
*                  routines:
*
*                     apply7x7Callback 
*                     close7x7Callback 
*                     get_cellname_array 
*                     hrap_cells 
*                     missing7x7Callback 
*                     scale7x7Callback 
*                     show_display7x7DS 
*                     undo7x7Callback 
*
* ORIGINAL AUTHOR: Bryon Lawrence
* CREATION DATE:   April 23, 2003
* ORGANIZATION:    OHD HSEB WHFS
* MACHINE:         HP-UX, Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*      1         Bryon Lawrence    Original Coding  
********************************************************************************
*/

#ifndef DISPLAY7X7_SHOW_H
#define DISPLAY7X7_SHOW_H

#include <Xm/Xm.h>

#define NUM_7X7_COLS 7 
#define NUM_7X7_ROWS 7 

extern const char * hrap_cells [ NUM_7X7_ROWS ] [ NUM_7X7_COLS ] ;

void apply7x7Callback ( Widget w , XtPointer calldata , 
                        XtPointer clientdata ) ;

void close7x7Callback ( Widget w , XtPointer calldata ,
                        XtPointer clientdata ) ;

void display7x7_callbacks ( ) ;

void missing7x7Callback ( Widget w , XtPointer calldata ,
                         XtPointer clientdata ) ;

void set_display7x7_gage_number ( int num ) ;

void scale7x7Callback ( Widget w , XtPointer calldata ,
                        XtPointer clientdata ) ;

void show_display7x7DS ( Widget w ) ;

void undo7x7Callback ( Widget w , XtPointer calldata ,
                       XtPointer clientdata ) ;

void badGageToggle7x7Callback ( Widget w , XtPointer calldata ,
                       XtPointer clientdata ) ;

#endif /* ifndef DISPLAY7X7_SHOW_H */
