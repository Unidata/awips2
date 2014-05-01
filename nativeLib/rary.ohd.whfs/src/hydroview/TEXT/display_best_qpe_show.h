/*******************************************************************************
* FILENAME:            display_best_qpe_show.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains function prototypes for routines 
*                      associated with the creation and control
*                      of the Display Best Estimate QPE Window.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 23, 2005
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef DISPLAY_BEST_QPE_SHOW_H
#define DISPLAY_BEST_QPE_SHOW_H

#include <Xm/Xm.h>

#define MAX_ACCUMULATION_INTERVAL 72
#define MAX_TIMELAPSE_DURATION 24

/* User Defined Types. */
enum BestQPESource {QPELocalSource, QPERFCSource, NumQPESourceItems };
enum BestQPEAccumArea { QPEGrid, QPEBasin, QPECounty, QPEZone, 
                        NumQPEAreaItems } ;

enum QPEEndTimeOper { QPEDayIncrement, QPEDayDecrement, QPEHourIncrement,
                      QPEHourDecrement, QPENoOperation } ;

extern char * QPEAreaTypes [ NumQPEAreaItems ] ;

/* Function prototypes. */
void show_bestQpeDS ( Widget w );

#endif /* #ifndef DISPLAY_BEST_QPE_SHOW_H */
