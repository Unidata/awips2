/*  function datim2 - get system time and return in char form  */
 
#include <stdio.h>
#include <time.h>

void datim2(char time_out[28])
{
struct  tm      *time_pointer;
time_t          tp;
char            *time_out_pointer;

static  char    *month[12] = {"Jan", "Feb", "Mar", "Apr",
			      "May", "Jun", "Jul", "Aug",
			      "Sep", "Oct", "Nov", "Dec"};

time_out_pointer = &time_out[0];

    time(&tp);
    time_pointer = localtime(&tp);

    sprintf( time_out_pointer,
	     " %s %2d, %d - %02d:%02d:%02d.00",
	     month[time_pointer->tm_mon],
	     time_pointer->tm_mday,
             (time_pointer->tm_year < 1900) ? time_pointer->tm_year + 1900
                                            : time_pointer->tm_year,
	     time_pointer->tm_hour,
	     time_pointer->tm_min,
             (time_pointer->tm_sec < 59) ? time_pointer->tm_sec
                                         : 59
           );

/*  strftime(time_out, 28, " %b %d, %Y - %H:%M:%S.00 ", &tp);  */


}
