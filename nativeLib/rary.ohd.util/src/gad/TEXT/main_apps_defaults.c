#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define  LEN_REPLY  512

extern int   get_apps_defaults( char *, int *, char *, int * );

int gad (int arg_count, char *arg_values[])
{
   char  returned_value[LEN_REPLY];
   int   arg_len;
   int   returned_len;

   memset(returned_value, '\0', LEN_REPLY);

   if (arg_count >= 2)
   {
      arg_len = (int)strlen(arg_values[1]);
      if (arg_len > 128) arg_len = 128;
      get_apps_defaults(arg_values[1], &arg_len, returned_value, &returned_len);
   }

   printf ("%s\n",returned_value);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/gad/RCS/main_apps_defaults.c,v $";
 static char rcs_id2[] = "$Id: main_apps_defaults.c,v 1.3 2000/03/14 14:52:30 page Exp $";}
/*  ===================================================  */

   return (EXIT_SUCCESS);
}
