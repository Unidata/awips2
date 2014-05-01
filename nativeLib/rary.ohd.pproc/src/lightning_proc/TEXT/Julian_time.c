#include "light_data.h"

/*  
    Get date time from Julian time.          
*/

/* =====================================================================
** pgm: ddgsc3 .. Get frm se-sum, cal-dt (frm yr-1970)
**
** use:     ddgsc3(&su1,&y1,&m1,&d1,&h1,&n1,&s1)
**
**  in: su1 ..... second-sum since Jan 1, 1970 (begin min 0) - int *
** out: y1 ...... 4-digit year number - int *
** out: m1 ...... month number (01-12) - int *
** out: d1 ...... day number (01-31) - int *
** out: h1 ...... hour number (0-23) - int *
** out: n1 ...... minute number (0-59) - int *
** out: s1 ...... second number (0-59) - int *
**
** rqd: ddgdj3,ddgjc
**
** lvl: DD2
** =====================================================================
*/
void ddgsc3(int *su1,int *y1,int *m1,int *d1,int *h1,int *n1,int *s1)
{
  int   nu1t, du1t, hu1t, su1t, j1t, y1t, m1t, d1t;
  void  ddgdj3(int *, int *, int *);
  void  ddgjc(int *, int *, int *, int *);

  su1t = *su1;

      nu1t  = su1t/60;
      su1t -= 60*nu1t;
      hu1t  = nu1t/60;
      nu1t -= 60*hu1t;
      du1t  = hu1t/24;
      hu1t -= 24*du1t;
      du1t += 1;

      ddgdj3(&du1t, &j1t, &y1t);
      ddgjc(&j1t, &y1t, &m1t, &d1t);

  *y1 = y1t;
  *m1 = m1t;
  *d1 = d1t;
  *h1 = hu1t;
  *n1 = nu1t;
  *s1 = su1t;
}
/* =====================================================================
** pgm: ddgdj3 .. Get frm da-sum, jul-dt (frm yr-1970)
**
** use:     ddgdj3(&du1,&j1,&y1)
**
**  in: du1 ..... day-sum since Jan 1, 1970 - int *
** out: j1 ...... day of year (001-366) - int *
** out: y1 ...... 4-digit year number - int *
**
** lvl: DD1
**
** cmt:   25567 is the day number for 31 Dec 1969 starting from 1900.
** cmt:   36890 is the day number for 31 Dec 2000 starting from 1900.
** cmt:    1461 is the number of days in a 4 year period.
** cmt:   36524 is the number of days in a 100 year period.
** cmt:  146097 is the number of days in a 400 year period.
** =====================================================================
*/
void ddgdj3(int *du1,int *j1,int *y1)
{
  int   du1t, j1t, y1t, per, dlf, pyr, per100, per400;

  du1t = *du1;

      du1t += 25567;
      if ( du1t <= 36890 )
      {
        du1t -= 365;
        per = (du1t-1)/1461;
        dlf = du1t - 1461*per;
        pyr = (4*dlf - 1)/1461;
        j1t = dlf - 365*pyr;
        y1t = 1900 + 4*per + pyr + 1;
      }
      else
      {
        du1t -= 36890;
        per400 = (du1t-1)/146097;
        du1t -= 146097*per400;

        if ( du1t < 146097 )
        {
          per100 = (du1t-1)/36524;
          du1t -= 36524*per100;

          per = (du1t-1)/1461;
          dlf = du1t - 1461*per;
          pyr = (4*dlf - 1)/1461;
          j1t = dlf - 365*pyr;
          y1t = 2000 + 400*per400 + 100*per100 + 4*per + pyr + 1;
        }
        else
        {
          j1t = 366;
          y1t = 2000 + 400*(per400+1);
        }
      }

  *j1 = j1t;
  *y1 = y1t;
}
/* =====================================================================
** pgm: ddgjc .. Get frm jul-dt, cal-dt (get mo, da)
**
** use:     ddgjc(&j1,&y1,m1,d1)
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
void ddgjc(int *j1,int *y1,int *m1,int *d1)
{
  int   ii, j1t, y1t, m1t, d1t;

  j1t = *j1;
  y1t = *y1;

      ii = 2;
      if (      y1t == (y1t/4)*4
           &&   y1t != 1800
           &&   y1t != 1900
           && ( y1t  < 2100 || y1t == (y1t/400)*400
                            || y1t != (y1t/100)*100 ) ) ii = 1;

      if ( (y1t == 1752) && (j1t > 246) ) j1t += 11;

      if ( j1t > (61-ii) ) j1t += ii;
      m1t = (j1t*67 + 2012)/2048;
      d1t = j1t - (m1t*489)/16 + 30;

  *m1 = m1t;
  *d1 = d1t;
}

/***
main ()
{
int jul, yr, mon, day, hr, mnt, sec;
printf("test print \n");
jul=937450764;
printf("jul=%d\n",jul);
ddgsc3(&jul,&yr,&mon,&day,&hr,&mnt,&sec);
printf("%d\t%d\t%d\t%d\t%d\t%d\t%d\n",jul,yr,mon,day,hr,mnt,sec);
}***/
