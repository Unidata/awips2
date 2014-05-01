/*******************************************************************************
* FILENAME:              newhour_RFCW.h
* GENERAL INFORMATION: 
* DESCRIPTION:           Contains the prototype of the newhour_RFCW callback
*                        routine and user-specified typedefs and enumerations.
*
* ORIGINAL AUTHOR:       Hmap_mpe team.
* CREATION DATE:         February 6, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
*  DATE                  PROGRAMMER        DESCRIPTION/REASON
*  February 6, 2002      Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef NEWHOUR_RFCW_H
#define NEWHOUR_RFCW_H

#include <Xm/Xm.h>

/* User-defined types. */ 
typedef enum ControlMenuItemInfo { PrevHour = 1 , Quit = 0 , NextHour = -1 , 
                                   ClearData = 2 ,
                                   OkHourPopup = 99 } ControlMenuItemInfo ;

/* Function Prototype. */
void newhour_RFCW ( Widget w , XtPointer clientdata, XtPointer calldata ) ;
void hour_sensitive_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void sensitize_save_buttons ( );

#endif /* #ifndef NEWHOUR_RFCW_H */
