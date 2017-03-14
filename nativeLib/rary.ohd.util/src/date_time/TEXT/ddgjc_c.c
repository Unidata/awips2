/* =====================================================================
** pgm: ddgjc_c .. Get frm jul-dt, cal-dt (get mo, da)
**
** use:     ddgjc_c(&j1,&y1,m1,d1)
**
**  in: j1 ...... day of year (001-366) - int *
**  in: y1 ...... 4-digit year number - int *
** out: m1 ...... month number (01-12) - int *
** out: d1 ...... day number (01-31) - int *
**
** lvl: DD1
**
** cmt: Alternate algorithm:      M1T = (J1T*9 + 269)/275
** cmt:                           D1T = J1T - (M1T*275)/9 + 30
** =====================================================================
*/
void ddgjc_c(int *j1,int *y1,int *m1,int *d1)
{
  int   ii, j1t, y1t, m1t, d1t;

  j1t = *j1;
  y1t = *y1;

      ii = 2;
      if (    (y1t == (y1t/4)*4)
           && (y1t != 1800)
           && (y1t != 1900)
           && (y1t  < 2100  ||  y1t == (y1t/400)*400
                            ||  y1t != (y1t/100)*100 )
         ) ii = 1;

      if ( (y1t == 1752) && (j1t > 246) ) j1t += 11;

      if ( j1t > (61-ii) ) j1t += ii;
      m1t = (j1t*67 + 2012)/2048;
      d1t = j1t - (m1t*489)/16 + 30;

  *m1 = m1t;
  *d1 = d1t;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjc_c.c,v $";
 static char rcs_id2[] = "$Id: ddgjc_c.c,v 1.1 2005/02/25 15:24:42 dsa Exp $";}
/*  ===================================================  */

  return;
}
