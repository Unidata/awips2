/* =====================================================================
** pgm: ddgdc2_c .. Get frm da-sum, cal-dt (frm yr-1900)
**
** use:     ddgdc2_c(&du1,&y1,&m1,&d1)
**
**  in: du1 ..... day-sum since Jan 1, 1900 - int *
** out: y1 ...... 4-digit year number - int *
** out: m1 ...... month number (01-12) - int *
** out: d1 ...... day number (01-31) - int *
**
** rqd: ddgdj2_c,ddgjc_c
**
** lvl: DD2
** =====================================================================
*/
void ddgdc2_c(int *du1,int *y1,int *m1,int *d1)
{
  int   du1t,j1t;
  void  ddgdj2_c(int *,int *,int *);
  void  ddgjc_c(int *,int *,int *,int *);

  du1t = *du1;

      ddgdj2_c(&du1t,&j1t,y1);
      ddgjc_c(&j1t,y1,m1,d1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgdc2_c.c,v $";
 static char rcs_id2[] = "$Id: ddgdc2_c.c,v 1.1 2005/02/25 15:24:38 dsa Exp $";}
/*  ===================================================  */

  return;
}
