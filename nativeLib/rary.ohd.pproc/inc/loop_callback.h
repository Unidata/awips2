
/*******************************************************************************
* FILENAME:            loop_callback.h
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef _LOOP_CALLBACK_H
#define _LOOP_CALLBACK_H

#include <Xm/Xm.h>

#define MAX_TIMELAPSE_TOKEN_LEN 1024
#define DEFAULT_TIMELAPSE_DURATION 1000

int get_loop_callback_duration ( ) ;

XtIntervalId get_loop_callback_interval_id ( ) ;

void loop_callback ( XtPointer clientdata , XtIntervalId * id ) ;

void set_loop_callback_interval_id ( XtIntervalId id ) ;

#endif /* #ifndef _LOOP_CALLBACK_H */
