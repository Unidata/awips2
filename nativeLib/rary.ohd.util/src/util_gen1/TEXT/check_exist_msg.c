/* Routine to print a message if a directory or file does not exist.

*/

#include <Xm/Xm.h>

void check_exist_msg (char *name, char *type, int iexist, char *msgstr,
                      char *msgtype, Widget widget, int imsg_window,
                      int *ireturn) {

int ldebug;

   ldebug=0;

   if (ldebug > 0) printf ("enter check_exist_msg");

   if (iexist == 0) {
      sprintf (msgstr,"%s: %s %s not found.",
               msgtype,type,name);
      printf ("%s\n",msgstr);
      if (imsg_window == 1)
         msg_window (widget,msgstr);
      *ireturn=0;
      if (strcmp(msgtype,"ERROR") == 0) *ireturn=1;
      }
   if (iexist == -1) {
      sprintf (msgstr,"ERROR: %s is a directory but should be a %s.",name,type);
      printf ("%s\n",msgstr);
      msg_window (widget,msgstr);
      *ireturn=1;
      }
   if (iexist == -2) {
      sprintf (msgstr,"ERROR: %s %s found but has a size of zero.",type,name);
      printf ("%s\n",msgstr);
      msg_window (widget,msgstr);
      *ireturn=1;
      }

   if (ldebug > 0) printf ("exit check_exist_msg - *ireturn=%i\n",*ireturn);
      

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/check_exist_msg.c,v $";
 static char rcs_id2[] = "$Id: check_exist_msg.c,v 1.2 2002/02/13 15:55:28 dws Exp $";}
/*  ===================================================  */

}
