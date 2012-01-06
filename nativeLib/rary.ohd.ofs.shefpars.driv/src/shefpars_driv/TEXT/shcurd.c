/* =====================================================================
** pgm: shcurd .. Read from computer the local date (4-digit year)
**
** use:     call chcurd(yr,mo,da)
**
** out: yr ...... 4-digit year number - int
** out: mo ...... month number (01-12) - int
** out: da ...... day number (01-31) - int
**
** rqd: system-routine: time, localtime
** =====================================================================
*/
#include <stdio.h>
#include <time.h>

void shcurd(int *yr,int *mo,int *da)
{
 struct tm   *ptm;
 time_t        tp;

    (void) time(&tp);
    ptm = localtime(&tp);

    *yr = (ptm->tm_year < 1900)  ?  ptm->tm_year + 1900  :  ptm->tm_year;
    *mo = ptm->tm_mon + 1;
    *da = ptm->tm_mday;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shcurd.c,v $";
 static char rcs_id2[] = "$Id: shcurd.c,v 1.2 1999/07/07 11:33:40 page Exp $";}
/*  ===================================================  */

}
