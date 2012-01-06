 /*
    ouptime
    is called to compute the number of seconds since jan 1 1980 00:00:00.
    It returns a long integer.
                                                                        */
#include <time.h>                       /* old time component structure */

static int mdays[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 } ;

long ouptime(int year, int month, int day, int hour, int minute, int second)

{
int ndays ;
long times ;

/*  COMPUTE TIME IN SECONDS SINCE JAN 1, 1980 00:00:00  */

    /* number of seconds for years */
/*    times = 31536000L * (long)(year - 1980) ; */
    times = 31536000L * (long)(year - 1970) ; 
/*    times += 86400L * (long)((year - 1977)/4); */    /* account for leap years */
    times += 86400L * (long)((year - 1969)/4);    /* account for leap years */

    /* number of seconds for days in year */
    ndays = mdays[month-1] + day - 1 ;
    times += 86400L * (long)ndays ;
    

    /* take into account leap years */
    if ( ( year % 4 == 0 && year % 100 != 0 ) || year % 400 == 0 ) 
            if (month > 2)
                  times += 86400L ;

    /* now add hours, minutes and seconds */
    times += 3600L * hour ;
    times += 60L * minute ;
    times += second ;

/*  RETURN THE TIME IN SECONDS  */
    return(times) ;
}
