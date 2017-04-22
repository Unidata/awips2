// +++ 	Name: tmbUtil.C
//	Purpose: Library or system routines to check if the process is running
//		 in the local workstation and to check if the user is logon to
//		 local or remote workstations.
//	Note:	 These routines are hightly dependent on OS platforms.
//
//      History: May 05 - Added routine to check workspace or desktops.
//      20-Jul,2005 - cannot assume env. variable "GDMSESSION" is always set
//                    to KDE for the user. It can be "Default".
//      16-Nov,2005 - DR16842: Use string instead of character to parse data.
// ---*************************************************************************
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include "tmbUtil.H"
#include <fstream>
#include <sstream>
#include <stdlib.h>
using namespace std;



// +++ 	Name: check_local_login
//	Purpose: An interface call to see if the user is still login to
//	the local console or not.
// ---*************************************************************************
int check_local_login(char * searched_user)
{
    char command[512];
    int n =0;
    sprintf(command,"$FXA_HOME/bin/consoleUser %s",
		    searched_user);
    n = system(command)>>8;
//    n = call_consoleUser(command, searched_user);
    return n;
}

// +++ 	Name: call_consoleUser
//	Purpose: Call the consoleUser program via pipe to see
//	if the user is still login to the console or not.
// ---*************************************************************************
int call_consoleUser(char *command, char *searched_user)
{
    FILE *file_pointer;
    char line[1024];
    int pid;
    char user[256];

    int n =0;

    file_pointer = popen(command, "r");

    if (file_pointer == NULL)
    {
	printf("\n file _point is NULL\n");
	return 0;
    }
    while ((fgets(line, sizeof line, file_pointer)) != NULL){
        sscanf(line,"%d %s\n",&pid,&user[0]);
    }
    pclose(file_pointer);

    if (pid != 1)
    {
       if (strstr(line,searched_user)!=NULL)
               n++;
    }
    return n;
}


// +++ 	Name: check_remote_login
//	Purpose: An interface call to see if the user is still login to
//	the local console or not.
// ---*************************************************************************
int check_remote_login(char *hostname)
{
    char searched_user[128];
    char command[512];
    int n = 0;
    sprintf(searched_user,"%s",getenv("USER"));

    sprintf(command,"ssh %s $FXA_HOME/bin/consoleUser %s",hostname,
		    searched_user);
    n = system(command)>>8;

    return n;
}


// +++ 	Name: another_atoi
//	Purpose: A version of ascii to a long integer routine.
// ---*************************************************************************
int another_atoi(char *buffer)
{
    int rc;
    char* p=buffer;
    if (!isdigit(*p))
        return 0;
    rc = strtol(buffer,&p, 10);
    return *p==0 ? rc : 0;
}

// +++ 	Name: findProcess
//	Purpose: A system routine to check the /proc/processID/cmdline to
//	see if the given process is running or not.
//	Note:	 This routines are hightly dependent on OS platforms.
// ---*************************************************************************
int findprocess(char* processName)
{
    DIR *dirp;
    struct dirent *dp;
    char *tokens[256];
    char fname[512];
    char buf[512];
    char *p;
    int pid;
    FILE *fp;
    int ntok, nchars, isnull;
    int rc = 0;
    int mypid;

    dirp = opendir("/proc");

    mypid = getpid();

    while ((dp = readdir(dirp)) != NULL) {
        pid = another_atoi(dp->d_name);
        if (pid <= 0 || pid == mypid)
            continue;
        sprintf(fname, "/proc/%d/cmdline", pid);
        fp = fopen(fname, "r");
        if (!fp)
            continue;
        nchars = fread(buf, sizeof(char), sizeof(buf), fp);
        (void)  fclose(fp);
        isnull = 1;
        ntok = 0;
        for (p=buf; p<buf+nchars; p++) {
            if (*p == '\0')
                isnull = 1;
            else {
                if (isnull)
                    tokens[ntok++] = p;
                isnull = 0;
            }
        }

        if (strcmp(tokens[0],processName) == 0)
            rc++;
    }
    (void) closedir(dirp);
    return rc;
}
// +++ 	Name: killprocess
//	Purpose: A system routine to check the /proc/processID/cmdline to
//	see if the given process is running or not.
//	Note:	 This routines are hightly dependent on OS platforms.
// ---*************************************************************************
int killprocess(char* processName)
{
    DIR *dirp;
    struct dirent *dp;
    char *tokens[256];
    char fname[512];
    char buf[512];
    char *p;
    int pid;
    FILE *fp;
    int ntok, nchars, isnull;
    int rc = 0;
    int mypid;
    char command[512];
    int n =0;

    dirp = opendir("/proc");

    mypid = getpid();
    while ((dp = readdir(dirp)) != NULL) {
        pid = another_atoi(dp->d_name);
        if (pid <= 0 || pid == mypid)
            continue;
        sprintf(fname, "/proc/%d/cmdline", pid);
        fp = fopen(fname, "r");
        if (!fp)
            continue;
        nchars = fread(buf, sizeof(char), sizeof(buf), fp);
        (void)  fclose(fp);
        isnull = 1;
        ntok = 0;
        for (p=buf; p<buf+nchars; p++) {
            if (*p == '\0')
                isnull = 1;
            else {
                if (isnull)
                    tokens[ntok++] = p;
                isnull = 0;
            }
        }
        if (strcmp(tokens[0],processName) == 0) {
    	    sprintf(command,"kill -9 %d", pid);
    	    n = system(command);
            rc++;
	}
    }
    (void) closedir(dirp);
    return rc;
}

// +++ 	Name: check_desktops_or_workspace
//	Purpose: A system routine to check number of desktops (KDE) or
//	workspace (GNOME).
//	Note:	 This routines are hightly dependent on OS platforms.
// ---*************************************************************************
int check_desktops_or_workspace()
{
    	FILE *file_pointer;
	char home[256];
	char gdms[256];
        char line[1024];
	int number;
	int max_number=0;
	int rc=0;
	int count=0;
        sprintf(gdms,"%s",getenv("GDMSESSION"));

    if (strcmp(gdms,"GNOME") == 0) {
    // GDMSESSION must be GNOME
	rc = 0;
        // get workspace for Gnome
        file_pointer = popen("gconftool-2 --get /apps/metacity/general/num_workspaces","r");
        while ((fgets(line, sizeof line, file_pointer)) != NULL){
            sscanf(line,"%d \n",&count);
        }
        pclose(file_pointer);
	if (count > 1)  rc = count;
	return (rc);

    }
    else {
    // GDMSESSION must be KDE, Failsafe or Default
        sprintf(home,"%s/.kde/share/config/kdeglobals",getenv("HOME"));
	ifstream globalFile (home);
	if (! globalFile.is_open()) {
        // Test dcop for kde
        	file_pointer = popen("dcop kwin KWinInterface currentDesktop","r");
        	if (file_pointer == NULL)
        	while ((fgets(line, sizeof line, file_pointer)) != NULL){
            		sscanf(line,"%d \n",&count);
         	}
        	pclose(file_pointer);
		if (count > 1) rc = count;
		return (rc);
        }

        while (!globalFile.eof()) {
            string buffer;
            getline(globalFile, buffer);
            if (buffer.compare(0, 7, "Number=") == 0) {
                istringstream is(buffer.substr(buffer.find("=")+1));
                if (is >> number) {
                    count++;
                    if (number > max_number)  max_number = number;
                }
            }
        }

        globalFile.close();
	rc = 0;
	if (max_number > 1) rc = max_number;
	return (rc);
    }
    return (rc);
}

