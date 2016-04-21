#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GeneralUtil.h"

/*  TEST CALLING PROGRAM 
void main()
{
   char	token_val[100];
   int token_len;   
   get_hydro_tokens("APPS_DEFAULTS_SITE", "/awips/hydroapps/.Apps_defaults_site",
		    "DATABASE", token_val, &token_len);   
   printf("token=%s:%d:\n", token_val, token_len);   
   return;
}
*/


/****************************************************************************
   
   Allows a program to get an hydro toekn, typically defined as a 
   token in an application defaults file, in a totally self-sufficient
   manner.  Basically, this function calls get_apps_defaults but allows the
   definition of the applications file it will use, so that this definition
   does not have to be performed before running the program.
   This method is useful for programs that are spawned by other programs.
   
   The environment variable which contains the filename
   must be one of the following: 
   APPS_DEFAULTS, APPS_DEFAULTS_USER, APPS_DEFAULTS_SITE
   
   Mark Glaudemans; Jan27, 2000

   History:	Russell Erb	08/06/2001   
   		change return type of function from int to void
   ****************************************************************************/

void get_hydro_tokens(char	*file_env,
		     char 	*file_name,
		     char	*token_name,
		     char	*token_val,
		     int	*val_len)
{
   char	env_expr[250];
   int	status;
   int	token_len;
    
   
   /* initialize */
   
   token_val[0] = '\0';
   *val_len = 0;
   
   
   /* determine length of input token */
   
   token_len = strlen(token_name);
   
   /* define the specified environment variable with
      the specified value. */
   
   strcpy(env_expr, file_env);
   strcat(env_expr, "=");
   strcat(env_expr, file_name);
   
   status = putenv(env_expr);
   if (status != 0)
   {
      fprintf(stderr, "Error on putenv(), expression:%s:\n", env_expr);
      return;
   }
   
   
   /* call the get_apps_defaults function to return the 
      value of the specified token, which will be read
      from the specified file. */
   
   status = get_apps_defaults(token_name, &token_len, 
			      token_val, val_len);

   return;
}

