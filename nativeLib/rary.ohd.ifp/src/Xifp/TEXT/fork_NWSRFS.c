#define PATH_LENGTH 100

#include <stdio.h>
#include <signal.h>

void catch_signal();

fork_nwsrfs()
{
int     forecastProgram_pid;
char    bin_path[PATH_LENGTH];
char    sys_path[PATH_LENGTH];
char    oper_path[PATH_LENGTH];
int     len, len2;
char    scripts_path[PATH_LENGTH];

/* MR932 */
/* catching a terminated or stopped process */
signal(18, catch_signal);

forecastProgram_pid = fork();

if(forecastProgram_pid == 0)
  {       /*      We've begun the NWSRFS forecast component...    */

   /* call routine to get the path names for bin, oper, and sys files 

   memset(bin_path, '\0', PATH_LENGTH);
   memset(sys_path, '\0', PATH_LENGTH);
   memset(oper_path, '\0', PATH_LENGTH);

   strcpy(oper_path, (char *)getenv("HOME"));
   strcat(oper_path, "/");

   len = strlen("ifp_nwsrfs_bin_dir");
   get_apps_defaults("ifp_nwsrfs_bin_dir", &len, bin_path, &len2);
   len = strlen("rfs_sys_dir");
   get_apps_defaults("rfs_sys_dir", &len, sys_path, &len2);
   len = strlen("ifp_fs5files"); 
   get_apps_defaults("ifp_fs5files", &len, &oper_path[strlen(oper_path)], &len2); */

   len = strlen("ifp_scripts_dir"); 
   get_apps_defaults("ifp_scripts_dir", &len, scripts_path, &len2);

   /* - added by HDM team 
   *  call start_ifp_nwsrfs script to setup library path, classpath for dhm operation
   *  and startup ifp_nwsrfs program 
   */
   strcat(scripts_path, "/start_ifp_nwsrfs");
  
   execl(scripts_path,"start_ifp_nwsrfs",NULL);
   

 /*  commented out by dhm team
 *  execl(bin_path, "ifp_nwsrfs", "-sys", sys_path,
 *        "-oper", oper_path, NULL);
 */
   printf("The NWSRFS forecast program has died...\n");
  }



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/Xifp/RCS/fork_NWSRFS.c,v $";
 static char rcs_id2[] = "$Id: fork_NWSRFS.c,v 1.6 2007/05/16 16:48:13 aivo Exp $";}
/*  ====forecastProgram_pid===============================================  */

}
/* MR932 */
/* Wait for termination of child processes */
void catch_signal(int signl)
{
  if (signl==18)wait(0);
}

