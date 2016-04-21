#include "gen_areal_ffg.h"

/*-------------------------------------------------------------*/
/*   routine to parse durations from the gaff_durations token  */
/*-------------------------------------------------------------*/

int parse_durations()

{

int len, num_dur;
char dur_list[50], dur[3];
char tokensep[] = ",";
char *token;

/*-----------------------------------------------------------*/
/*   read gaff_durations token                               */
/*   if token not found, then set default                    */
/*-----------------------------------------------------------*/

len = strlen("gaff_durations");
get_apps_defaults("gaff_durations",&len,dur_list,&len);
if(len == 0)
{
   printf("gaff_durations token not found -- default = 1 hr used\n");
   ffg_durations[0] = 1;
   num_dur = 1;
}
else
   printf("ffg durations = %s\n",dur_list);

/*-------------------------------*/
/*   parse durations from list   */
/*   token separator is ","      */
/*-------------------------------*/

num_dur = 0;
token = strtok(dur_list, tokensep);

while(token != NULL)
{

   memset(dur, '\0', 3);
   strcpy(dur,token);
   ffg_durations[num_dur] = atoi(dur);
   /* printf("duration = %d\n",ffg_durations[num_dur]); */

   token = strtok(NULL, tokensep);

   num_dur++;

}

return(num_dur);

}
