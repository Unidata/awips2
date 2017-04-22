/* =====================================================================
** pgm: ddrmcl .. Read frm machine lcl cal dt-tm
**
** use:     call ddrmcl(y1,m1,d1,h1,n1,s1)
**
** out: y1 ...... 4-digit year number - int
** out: m1 ...... month number (01-12) - int
** out: d1 ...... day number (01-31) - int
** out: h1 ...... hour number local (0-23) - int
** out: n1 ...... minute number (0-59) - int
** out: s1 ...... second number (0-59) - int
**
** rqd: system-routine: time, localtime
**
** lvl: DD0
** =====================================================================
*/
#include <stdio.h>
#include <time.h>

void ddrmcl(int *y1,int *m1,int *d1,int *h1,int *n1,int *s1)
{
 struct tm   *ptm;
 time_t        tp;

    (void) time(&tp);
    ptm = localtime(&tp);

    *y1 = (ptm->tm_year < 1900)  ?  ptm->tm_year + 1900  :  ptm->tm_year;
    *m1 = ptm->tm_mon + 1;
    *d1 = ptm->tm_mday;
    *h1 = ptm->tm_hour;
    *n1 = ptm->tm_min;
    *s1 = (ptm->tm_sec < 59)  ?  ptm->tm_sec  :  59;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddrmcl.c,v $";
 static char rcs_id2[] = "$Id: ddrmcl.c,v 1.2 1999/07/06 11:22:07 page Exp $";}
/*  ===================================================  */

}
