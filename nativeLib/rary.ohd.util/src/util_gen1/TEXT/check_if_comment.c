/* Check if string is a comment.

*/
   
/**********************************************************************/

void check_if_comment (char *string, int *istat)

{
int   i, ldebug, len, len2, iblank;

   ldebug=0;

   *istat=0;
   
   len=strlen(string);
   len2=len-1;

/* check of length is zero */
   if (len2 == 0)
      *istat=1;
      else {
      /* check if blank */
         iblank=1;
         for (i = 0; i < len2; i++) {
            if (strncmp(&string[i]," ",1) != 0) {
               iblank=0;
               break;
               }
            }
         if (iblank == 1)
            *istat=1;
      /* check if comment */
         if (strncmp(&string[0],"#",1) == 0)
            *istat=1;
         if (strncmp(&string[0],"$",1) == 0)
            *istat=1;
         }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/check_if_comment.c,v $";
 static char rcs_id2[] = "$Id: check_if_comment.c,v 1.2 2001/02/14 19:23:27 dws Exp $";}
/*  ===================================================  */

}
