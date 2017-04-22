/* =====================================================================
** pgm: rmdyjul_1900 .. Get frm cal-dt, da-sum (frm yr-1900)
**
** rqd: ddgjd2_c,ddgcj_c
**
** lvl: DD2
** =====================================================================
*/
int rmdyjul_1900(short *mdy, long *jdate)
{
  int   j1t, istatus;
  int   y1, m1, d1, du1;
  void  ddgcj_c(int *,int *,int *,int *);
  void  ddgjd2_c(int *,int *,int *);

    istatus = 0;
    *jdate  = (long) 0;

    if ( mdy[0] < 1  ||  mdy[0] > 12 )
      { istatus = -1205; }
    else if ( mdy[1] < 1  ||  mdy[1] > 900 )
      { istatus = -1206; }
    else if ( mdy[2] < 1900  ||  mdy[2] > 2200 )
      { istatus = -1204; }
    else
      {
        y1 = (short) mdy[2];
        m1 = (short) mdy[0];
        d1 = (short) mdy[1];

        ddgcj_c(&j1t,&y1,&m1,&d1);
        ddgjd2_c(&du1,&j1t,&y1);
        *jdate = (long) du1;
      }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/rmdyjul_1900.c,v $";
 static char rcs_id2[] = "$Id: rmdyjul_1900.c,v 1.2 2006/01/25 19:58:33 dws Exp $";}
/*  ===================================================  */

  return (istatus);
}
