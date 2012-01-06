/* =====================================================================
** pgm: ddrmjl .. Read frm machine lcl jul dt-tm
**
** use:     call ddrmjl(j1,y1,h1,n1,s1)
**
** out: j1 ...... day of year (001-366) - int
** out: y1 ...... year number (1753-2199) or (00-99 after 1900) - int
** out: h1 ...... hour number local (0-23) - int
** out: n1 ...... minute number (0-59) - int
** out: s1 ...... second number (0-59) - int
**
** rqd: system-routine: time, localtime
**
** lvl: DD1
** =====================================================================
*/
#include <stdio.h>
#include <time.h>

void ddrmjl(int *j1,int *y1,int *h1,int *n1,int *s1)
{
 struct  tm      *ptm;
 long            tp;

    time(&tp);
    ptm = localtime(&tp);

    *j1 = ptm->tm_yday + 1;
    *y1 = (ptm->tm_year < 1900)  ?  ptm->tm_year + 1900  :  ptm->tm_year;
    *h1 = ptm->tm_hour;
    *n1 = ptm->tm_min;
    *s1 = (ptm->tm_sec < 60)  ?  ptm->tm_sec  :  59;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddrmjl.c,v $";
 static char rcs_id2[] = "$Id: ddrmjl.c,v 1.1 2001/06/13 08:56:14 mgm Exp $";}
/*  ===================================================  */

}
