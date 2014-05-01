/* =====================================================================
** pgm: ddgcd2_c .. Get frm cal-dt, da-sum (frm yr-1900)
**
** use:     ddgcd2_c(&du1,&y1,&m1,&d1)
**
** out: du1 ..... day-sum since Jan 1, 1900 - int *
**  in: y1 ...... 4-digit year number (1900 plus) - int *
**  in: m1 ...... month number (01-12) - int *
**  in: d1 ...... day number (01-31) - int *
**
** rqd: ddgjd2_c,ddgcj_c
**
** lvl: DD2
** =====================================================================
*/
void ddgcd2_c(int *du1,int *y1,int *m1,int *d1)
{
  int   j1t;
  void  ddgcj_c(int *,int *,int *,int *);
  void  ddgjd2_c(int *,int *,int *);

      ddgcj_c(&j1t,y1,m1,d1);
      ddgjd2_c(du1,&j1t,y1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgcd2_c.c,v $";
 static char rcs_id2[] = "$Id: ddgcd2_c.c,v 1.1 2005/02/25 15:23:58 dsa Exp $";}
/*  ===================================================  */

  return;
}
