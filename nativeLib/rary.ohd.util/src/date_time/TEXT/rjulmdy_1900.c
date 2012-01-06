/* =====================================================================
** pgm: rjulmdy_1900 .. Get frm da-sum, cal-dt (frm yr-1900)
**
** rqd: ddgdj2_c, ddgjc_c
**
** lvl: DD2
** =====================================================================
*/
int rjulmdy_1900(long jdate, short *mdy)
{
  int   j1t, istatus;
  int   y1, m1, d1, du1t;
  void  ddgdj2_c(int *,int *,int *);
  void  ddgjc_c(int *,int *,int *,int *);

    istatus = 0;
    mdy[2]  = (short) 0;
    mdy[0]  = (short) 0;
    mdy[1]  = (short) 0;

    du1t = (int) jdate;

    if ( du1t < 1 )
      { istatus = -1; }
    else
      {
        ddgdj2_c(&du1t,&j1t,&y1);
        ddgjc_c(&j1t,&y1,&m1,&d1);
        mdy[2] = (short) y1;
        mdy[0] = (short) m1;
        mdy[1] = (short) d1;
      }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/rjulmdy_1900.c,v $";
 static char rcs_id2[] = "$Id: rjulmdy_1900.c,v 1.2 2006/01/25 19:58:27 dws Exp $";}
/*  ===================================================  */

  return (istatus);
}
