/* getcpu
*   Calculate CPU time used in thousandths of a second.  Original
*   version used clock() from the standard C library (libc.a).
*   The current version now uses times() also in libc.a.  This
*   function may be called from a C or FORTRAN routine.
*
*   Written by D. Page   3 September 1992
*   Modified by Dave St. 6 May 1999
*/

#include <time.h>
#include <sys/times.h>
#include <limits.h>

void getcpu(long *cpu_thsdth_sec)
{
  struct tms   tim;

    (void) times(&tim);

    *cpu_thsdth_sec = ( 1000L/(long)CLOCKS_PER_SEC ) *
                      ( (long)tim.tms_utime + (long)tim.tms_stime );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/getcpu.c,v $";
 static char rcs_id2[] = "$Id: getcpu.c,v 1.2 1999/07/06 06:42:36 page Exp $";}
/*  ===================================================  */

}
