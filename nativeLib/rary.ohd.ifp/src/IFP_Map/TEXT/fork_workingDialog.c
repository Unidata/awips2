#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
/*  DHM team: Modifed code to return process ID to the parent (seed.c).
/*  kill process if error occured.(November 20,2006)
/*  
*/
pid_t fork_working_dialog()
{

	pid_t     working_dialog_pid;
    int     len, len2;
	char    bin_command[80];

    working_dialog_pid = fork();


    if(working_dialog_pid == 0)
	{
	/* call routine to get the path name for bin files */
	    memset(bin_command, '\0', 80);
        len = strlen("ifp_bin_dir");
	    get_apps_defaults("ifp_bin_dir", &len, bin_command, &len2);

	    strcat(bin_command, "/working_dialog");

	    execl(bin_command, NULL);
	    printf("The working dialog has died...\n");
	}
    return (working_dialog_pid);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/fork_workingDialog.c,v $";
 static char rcs_id2[] = "$Id: fork_workingDialog.c,v 1.1 1995/09/08 14:55:27 page Exp $";}
/*  ===================================================  */

}
