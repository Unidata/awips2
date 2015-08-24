/*! @file superlu_timer.c
 * \brief Returns the time used
 *
 * <pre>
 * Purpose
 * ======= 
 * 
 * Returns the time in seconds used by the process.
 *
 * Note: the timer function call is machine dependent. Use conditional
 *       compilation to choose the appropriate function.
 * </pre>
 */


#ifdef SUN 
/*
 * 	It uses the system call gethrtime(3C), which is accurate to 
 *	nanoseconds. 
*/
#include <sys/time.h>

double SuperLU_timer_() {
    return ( (double)gethrtime() / 1e9 );
}

#elif _WIN32

#include <time.h>

double SuperLU_timer_()
{
    clock_t t;
    t=clock();

    return ((double)t)/CLOCKS_PER_SEC;
}

#else

#ifndef NO_TIMER
#include <sys/types.h>
#include <sys/times.h>
<<<<<<< HEAD
#include <time.h>
#include <sys/time.h>
#endif

#ifndef CLK_TCK
#define CLK_TCK 60
#endif
/*! \brief Timer function
 */ 
=======
#include <sys/time.h>
#include <unistd.h>
#endif

/*! \brief Timer function
 */ 

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
double SuperLU_timer_()
{
#ifdef NO_TIMER
    /* no sys/times.h on WIN32 */
    double tmp;
    tmp = 0.0;
<<<<<<< HEAD
#else
    struct tms use;
    double tmp;
    times(&use);
    tmp = use.tms_utime;
    tmp += use.tms_stime;
#endif
    return (double)(tmp) / CLK_TCK;
=======
    /* return (double)(tmp) / CLK_TCK;*/
    return 0.0;
#else
    struct tms use;
    double tmp;
    int clocks_per_sec = sysconf(_SC_CLK_TCK);

    times ( &use );
    tmp = use.tms_utime;
    tmp += use.tms_stime;
    return (double)(tmp) / clocks_per_sec;
#endif
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

#endif

