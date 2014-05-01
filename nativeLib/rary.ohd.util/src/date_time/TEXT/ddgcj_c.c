/* =====================================================================
** pgm: ddgcj_c .. Get frm cal-dt, jul-dt (get julda)
**
** use:     ddgcj_c(&j1,&y1,&m1,&d1)
**
** out: j1 ...... day of year (001-366) - int *
**  in: y1 ...... 4-digit year number - int *
**  in: m1 ...... month number (01-12) - int *
**  in: d1 ...... day number (01-31) - int *
**
** lvl: DD1
** =====================================================================
*/
void ddgcj_c(int *j1,int *y1,int *m1,int *d1)
{
  int   j1t,y1t,m1t,d1t;
  int   diyt[] = { 0,0,31,59,90,120,151,181,212,243,273,304,334 };

      y1t = *y1;
      m1t = *m1;
      d1t = *d1;

      if ( (m1t < 1) || (m1t > 12) ) m1t = 1;
      j1t = diyt[m1t] + d1t;
      if (    (m1t  > 2)
           && (y1t == (y1t/4)*4)
           && (y1t != 1800)
           && (y1t != 1900)
           && (y1t  < 2100  ||  y1t == (y1t/400)*400
                            ||  y1t != (y1t/100)*100 )
         ) j1t += 1;

      if ( y1t == 1752 && j1t > 246 ) j1t -= 11;

  *j1 = j1t;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgcj_c.c,v $";
 static char rcs_id2[] = "$Id: ddgcj_c.c,v 1.1 2005/02/25 15:24:32 dsa Exp $";}
/*  ===================================================  */

  return;
}
