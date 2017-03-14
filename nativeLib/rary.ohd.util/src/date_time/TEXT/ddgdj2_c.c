/* =====================================================================
** pgm: ddgdj2_c .. Get frm da-sum, jul-dt (frm yr-1900)
**
** use:     ddgdj2_c(&du1,&j1,&y1)
**
**  in: du1 ..... day-sum since Jan 1, 1900 - INT
** out: j1 ...... day of year (001-366) - INT
** out: y1 ...... 4-digit year number - INT
**
** lvl: DD1
**
** cmt:   36890 is the day number for 31 Dec 2000.
** cmt:    1461 is the number of days in a 4 year period.
** cmt:   36524 is the number of days in a 100 year period.
** cmt:  146097 is the number of days in a 400 year period.
** =====================================================================
*/
void ddgdj2_c(int *du1,int *j1,int *y1)
{
  int   du1t, j1t, y1t, per, dlf, pyr, per100, per400;

  du1t = *du1;

      if ( du1t <= 365 )
      {
        j1t = du1t;
        y1t = 1900;
      }
      else if ( du1t <= 36890)
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgdj2_c.c,v $";
 static char rcs_id2[] = "$Id: ddgdj2_c.c,v 1.1 2005/02/25 15:24:40 dsa Exp $";}
/*  ===================================================  */

  return;
}
