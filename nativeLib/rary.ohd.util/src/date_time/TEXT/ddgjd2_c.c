/* =====================================================================
** pgm: ddgjd2_c .. Get frm jul-dt, da-sum (frm yr-1900)
**
** use:     ddgjd2_c(&du1,&j1,y1)
**
** out: du1 ..... day-sum since Jan 1, 1900 - int *
** out:           (equal to 0 if year is less that 1900)
**  in: j1 ...... day of year (001-366) - int *
**  in: y1 ...... 4-digit year number (1900 plus) - int *
**
** lvl: DD1
** =====================================================================
*/
void ddgjd2_c(int *du1,int *j1,int *y1)
{
  int   du1t, j1t, y1t, yii, yjj;

  j1t = *j1;
  y1t = *y1;

      du1t = 0;
      if ( y1t >= 1900 )
      {
        yii = y1t - 1900;
        if ( yii > 0 )
        {
          du1t = 365*yii + (yii-1)/4;
          if ( yii > 200 )
          {
            yjj = yii - 101;
            du1t = du1t - yjj/100 + yjj/400;
          }
        }
        du1t += j1t;
      }

  *du1 = du1t;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjd2_c.c,v $";
 static char rcs_id2[] = "$Id: ddgjd2_c.c,v 1.1 2005/02/25 15:24:44 dsa Exp $";}
/*  ===================================================  */

  return;
}
