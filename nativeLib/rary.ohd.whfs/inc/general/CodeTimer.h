/*******************************************************************************
* FILENAME:             CodeTimer.h
* ORIGINAL AUTHOR:      Gautam Sood
* CREATION DATE:        January 4, 2006
* DESCRIPTION:          Contains the prototypes for the CodeTimer routines
* ORGANIZATION:         OHD-11, HSEB, WHFS
* MACHINE:              Redhat Linux
*
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
* 
* 
********************************************************************************
*/

#ifndef CODETIMER_H_
#define CODETIMER_H_
#include <sys/time.h>

typedef struct
{
	int is_running;
	struct timeval start_time;
	struct timeval end_time;
	double elapsed_time;
} codetimer;

void init_timer( codetimer *timer);
void start_timer( codetimer *timer );
double stop_timer( codetimer *timer );
void stop_timer_and_print_elapsed_time( codetimer *timer, char *message, FILE *filePtr );
void restart_timer( codetimer *timer );
double get_elapsed_time( codetimer *timer, int calculate );
void print_elapsed_time( codetimer *timer, char *message, FILE *filePtr );
double calculate_elapsed_time( codetimer *timer );

#endif /*CODE_TIMER_H_*/
