/*******************************************************************************
* FILENAME:             CodeTimer.c
* ORIGINAL AUTHOR:      Gautam Sood
* CREATION DATE:        January 4, 2006
* DESCRIPTION:          Allows the user to create timers within C code to time
* 						certain segments of code.  All times are in milliseconds.
*
* ORGANIZATION:         OHD-11, HSEB, WHFS
* MACHINE:              Redhat Linux
*
* MODIFICATION HISTORY:
*     MODULE      DATE         PROGRAMMER        DESCRIPTION/REASON
*
* 
* 
* Function                                         Description
* ----------------------------------------------------------------------------------------------
* start_timer                                   Starts the timer
*
* stop_timer                                    Stops the timer and returns the elapsed time 
*
* stop_timer_and_print_elapsed_time             Stops the timer and prints out the elapsed 
*                                               time to a file or stdout if no file pointer is specified
*
* restart_timer                                 Restarts the timer and resets the starttime to 
*                                               the current time 
* 
* get_elapsed_time                              Returns the current elapsed time
* 
* print_elapsed_time                            Prints out the currently stored elapsed time
* 
* calculate_elapsed_time                        Calculates the elapsed time
* 
* ----------------------------------------------------------------------------------------------
* 
* Example of Usage:
* 
* #include <stdio.h>
* #include <stdlib.h>
* #include <unistd.h>
* #include <sys/time.h>
* #include <string.h>
* #include <values.h>
* #include "code_timer.h"

* int main( int argc, char *argv[] )
* {
*   codetimer code_timer1;
*   codetimer code_timer2;
*   int i;
*   long total;
*
*   start_timer( &code_timer1 );  // Starts timer1
*
*   for ( i = 0; i < MAXINT; i++ )
*   {
* 		total += i;
*   }
*   stop_timer_and_print_elapsed_time( &code_timer1, "Timer1 finished in", NULL ); // Stops timer1
*                                                                                  // and prints out the total elapsed time
*   restart_timer( &code_timer1 ); // Restarts timer1
*   start_timer( &code_timer2 );   // Starts timer2
*   total = 0;
*   for ( i = 0; i < MAXINT; i++ )
*   {
*       total += i;
*       if ( i == 5000 )
*       {
*           print_elapsed_time( &code_timer2, "Current elapsed timer2", NULL );  // Prints out the current elapsed time for timer2
*       }
*   }
*
*   stop_timer_and_print_elapsed_time( &code_timer1, "Timer1 Finished in", NULL );   // Stops timer1 and prints the elapsed time
*   stop_timer_and_print_elapsed_time( &code_timer2, "Timer2 Finished in", NULL );   // Stops timer2 and prints the elapsed time
*   printf( "Timer1 value : %lf\nTimer2 value : %lf\n", get_elapsed_time( &code_timer1 ), get_elapsed_time( &code_timer2 ) );  // Gets the elapsed time for timers1 & 2
*
*   return 0;
* }
*
* Output:
* 
* Timer1 finished in 5632.750 millis
* Current elapsed timer2 0.000 millis
* Timer1 Finished in 13096.025 millis
* Timer2 Finished in 7463.314 millis
* Timer1 value : 13096.025000
* Timer2 value : 7463.314000
* 
* 
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include "CodeTimer.h"


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   init_timer
* PURPOSE:       This routine initializes the code timer structure
		 Needs to be used once before using restart_timer()
		 for each codetimer variable
* INPUTS:        codetimer 
* OUTPUT:        none
* RETURNS:       none
********************************************************************************/
void init_timer( codetimer *timer )
{	
	timer->elapsed_time = 0;
	timer->is_running = 0;
}

/*******************************************************************************/


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   start_timer
* PURPOSE:       This routine starts the code timer.
* INPUTS:        codetimer 
* OUTPUT:        none
* RETURNS:       none
********************************************************************************/
void start_timer( codetimer *timer )
{	
	timer->elapsed_time = 0;
	timer->is_running = 1;
	gettimeofday( &timer->start_time, NULL );
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   stop_timer
* PURPOSE:       This routine stops the code_timer and returns the elapsed time
* INPUTS:        codetimer 
* OUTPUT:        none
* RETURNS:       elapsed time in milliseconds
********************************************************************************/
double stop_timer( codetimer *timer )
{
	gettimeofday( &timer->end_time, NULL );

    timer->elapsed_time += calculate_elapsed_time( timer );

	timer->is_running = 0;
	
	return ( timer->elapsed_time );
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   stop_timer_and_print_elapsed_time
* PURPOSE:       This routine stops the code_timer and calls print_elapsed_time to 
*                print out the elapsed time.
* INPUTS:        codetimer
*                message - Message to be included with the output
*                file pointer - File pointer.  If null, will output to stdout
* OUTPUT:        none
* RETURNS:       none
********************************************************************************/
void stop_timer_and_print_elapsed_time( codetimer *timer, char *message, FILE *filePtr )
{
	double elapsed_time = 0;
	elapsed_time = stop_timer( timer );
	
	print_elapsed_time( timer, message, filePtr );	
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   restart_timer
* PURPOSE:       This routine restarts the timer
* INPUTS:        codetimer 
* OUTPUT:        none
* RETURNS:       none
********************************************************************************/
void restart_timer( codetimer *timer )
{
    
    // want exactly 1 in order to be sure bad
    //values  aren't sneaking in
    
    timer->is_running = 1;
    gettimeofday( &timer->start_time, NULL );
	
 
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   get_elapsed_time
* PURPOSE:       This routine gets the elapsed time.
* INPUTS:        codetimer
*                recalculate - If 1, will recalculate the elapsed time.  If 0,
*                              will return currently stored elapsed time.
* OUTPUT:        none
* RETURNS:       elapsed time in milliseconds
********************************************************************************/
double get_elapsed_time( codetimer *timer, int recalculate )
{
    double total_time = timer->elapsed_time;
    
	if ( recalculate )
	{
        total_time = calculate_elapsed_time( timer );
    }
    return total_time;
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   print_elapsed_time
* PURPOSE:       This routine prints the elapsed time to either a file or stdout
* INPUTS:        codetimer
*                message - Message to be included with the output
*                file pointer - File pointer.  If null, will output to stdout
* OUTPUT:        elapsed time in milliseconds along with a message to a file or standard out
* RETURNS:       none
********************************************************************************/
void print_elapsed_time( codetimer *timer, char *message, FILE *filePtr )
{
	if ( filePtr == NULL )
	{
		filePtr = stdout;
	}
	
	fprintf( filePtr, "%s %.3lf millis\n", message, timer->elapsed_time );
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   calculate_elapsed_time
* PURPOSE:       This routine calculates the elapsed time.  It is only called internally
* INPUTS:        codetimer
* OUTPUT:        none
* RETURNS:       (double) elapsed time in milliseconds 
********************************************************************************/
double calculate_elapsed_time( codetimer *timer )
{
	double total_time_in_seconds = 0;
    double total_time_in_millis = 0;
	
    total_time_in_seconds = ( timer->end_time.tv_sec - timer->start_time.tv_sec ) +
                            ( ( timer->end_time.tv_usec - timer->start_time.tv_usec ) / 1000000.0 );
	
	total_time_in_millis = total_time_in_seconds * 1000;
	
	return total_time_in_millis;
}
