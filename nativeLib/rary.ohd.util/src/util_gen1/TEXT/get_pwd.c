/**********************************************************************/

/* Routine to get the current directory.

*/

#include <stdio.h>

void get_pwd (char *pwd, int *length)
                     
{
/***********************************************/
/* In linux, if PWD is not export, it will     */
/* core dump.  So I put in some code to detect */
/* wether the PWD token is defined or not.  If */
/* it is not defined, then return pwd="" and   */
/* length=0. ---kwz, 8-26-02, bug found during */
/* beta test r22:r20-16                        */
/*                                             */
/************************************************/
/*   strcpy (pwd,getenv("PWD"));
   *length=strlen(pwd);
*/

  char *TempStr;

  TempStr=getenv("PWD") ;
  if (TempStr==NULL) /*PWD token not found*/
  { strcpy (pwd,"");
    *length=0;
  }
  else /*PWD token found*/
  { strcpy (pwd,getenv("PWD"));
    *length=strlen(pwd);
  }

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_pwd.c,v $";
 static char rcs_id2[] = "$Id: get_pwd.c,v 1.2 2002/10/10 15:01:15 dws Exp $";}
/*  ===================================================  */

}
