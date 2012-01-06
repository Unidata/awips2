/*  chkdate routine checks number of dates in the input buffer  */
/*       e.g. buf= .UHGCHNG 04029312Z 03209312Z -->*iRet = 2    */
/*  input:  buf                                                 */
/*  output: iRet - number of dates in buf, 1,2 or 3             */
/****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void chkdates(int *iRet, char buf[100]) {

char *delim=" " ;
char *token, tbuf[80];
int   i, nspace=0, ntokens=0;
int   len = strlen(buf);


for (i=len;i>0;i--){
   if(isdigit(buf[i]) || isalpha(buf[i]))break;
   else
   nspace++;
}


strncpy(tbuf, buf, (len-nspace+1));
tbuf[len-nspace+1]='\0';


token = strtok(tbuf,delim);

while(1) {
   if( token != NULL) {
      ntokens++;
      token = strtok(NULL, " ");
   }
   else {
      break;
   }

}
 *iRet = ntokens;
 return ;
 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/chkdates.c,v $";
 static char rcs_id2[] = "$Id: chkdates.c,v 1.1 2004/08/11 19:00:28 wkwock Exp $";}
/*  ===================================================  */

}
