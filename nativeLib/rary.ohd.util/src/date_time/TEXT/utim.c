/* ============================================================================
** pgm: utim .. get real, cpu, user, system times using sys-rtn "times"
**
** use:     call utim(real_tm, user_tm, system_tm)
**
** out: real_tm .... current real time (thousandth sec) from computer - INT
** out: user_tm .... current user time (thousandth sec) from computer - INT
** out: system_tm .. current system time (thousandth sec) from computer - INT
**
** rqd: system-routine: times
** ============================================================================
*/

#include <time.h>
#include <sys/times.h>
#include <limits.h>

void utim(long *clk_thsdth, long *usr_thsdth, long *sys_thsdth)
{
 clock_t      clktim;
 struct tms   tim;
 long int     mult;

    clktim = times(&tim);

    mult = 1000L/(long)CLOCKS_PER_SEC;

    *clk_thsdth = (clock_t)( mult * (long)clktim );
    *usr_thsdth = (clock_t)( mult * (long)tim.tms_utime );
    *sys_thsdth = (clock_t)( mult * (long)tim.tms_stime );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/utim.c,v $";
 static char rcs_id2[] = "$Id: utim.c,v 1.1 1997/12/31 20:52:50 page Exp $";}
/*  ===================================================  */

}
