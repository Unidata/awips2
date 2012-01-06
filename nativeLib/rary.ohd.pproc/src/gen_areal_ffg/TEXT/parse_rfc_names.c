#include "gen_areal_ffg.h"

/*------------------------------------------------------------*/
/*   routine to parse RFC names from the gaff_rfc_list token  */
/*------------------------------------------------------------*/

int parse_rfc_names()

{

int len, num_rfc;
char rfc_list[50];
char tokensep[] = ",";
char *token;

/*-----------------------------------------------------------*/
/*   read gaff_rfc_list token                                */
/*   if token not found, then stop                           */
/*-----------------------------------------------------------*/

len = strlen("gaff_rfc_list");
get_apps_defaults("gaff_rfc_list",&len,rfc_list,&len);
if(len == 0)
{
   printf("gaff_rfc_list token not found -- program stopping\n");
   exit(1);
}
else
   printf("rfc list = %s\n",rfc_list);

/*-------------------------------*/
/*   parse RFC names from list   */
/*   token separator is ","      */
/*-------------------------------*/

num_rfc = 0;
token = strtok(rfc_list, tokensep);

while(token != NULL)
{

   num_rfc++;

   strcpy(rfc_names[num_rfc-1],token);
   rfc_names[num_rfc-1][5] = '\0';
   /*  printf("rfc name = %s\n",rfc_names[num_rfc-1]);  */

   token = strtok(NULL, tokensep);

}

return(num_rfc);

}
