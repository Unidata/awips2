// ******************************************************************
// +++ Procstruct.C: Linked list of structures containing process ifnfo
//
//
// --- **************************************************************

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <pwd.h>

#include "Procstruct.H"


Procstruct::Procstruct (
    void
) {
    next = NULL;
    cmd = NULL;
}

Procstruct::~Procstruct (
    void
) {
    if (cmd) {
        free (cmd);
	cmd = NULL;
    }
    if (next) {
        delete (next);
	next = NULL;
    }
}


Procstruct *
Procstruct::buildProcStructList (
) {

    // ----- Open the /proc directory for reading.
    DIR * procDirStream = opendir ("/proc");

    // ----- We're going to be setting up a linked list of processes.
    Procstruct *	firstProc = new Procstruct;
    Procstruct *	currentProc = firstProc;

    // ----- Step through the subdirectories in /proc.
    struct dirent *	procDirEnt;
    while (procDirEnt = readdir (procDirStream)) {

        // ----- Is this entry related to a process?
	if (isPid (procDirEnt->d_name)) {

	    // ----- Create a new Procstruct to hold the data for
	    // -----    the next pass.  
	    Procstruct * nextProc = new Procstruct;

	    // ----- Set the pointer in the current struct to point
	    // -----    to the next one.
	    currentProc->next = nextProc;

	    // ----- Load the process ID.
	    currentProc->pid = atoi (procDirEnt->d_name);

	    // ----- Build the name of the process subdirectory.
	    // -----    It looks like /proc/25783
	    char statFileName[128];
	    statFileName[0] = 0;
	    strcat (statFileName, "/proc/");
	    strcat (statFileName, procDirEnt->d_name);

	    // ----- Call the stat function to get the user ID for this
	    // -----    process.
	    struct stat procDirStatBuf;
            stat (statFileName, &procDirStatBuf);

	    // ----- Load the user ID into the process info structure.
	    currentProc->uid = procDirStatBuf.st_uid;

	    // ----- Build the name of the stat file.  It looks like
	    // -----    It looks like /proc/25783/stat
	    strcat (statFileName, "/stat");

	    // ----- Open the stat file for reading.  Just move on
            // -----    if the file is no longer present.
	    FILE * statStream = fopen (statFileName, "r");
            if (!statStream) continue;

	    // ----- Read stuff from the stat file, load it into the
	    // -----    process info struct.  If we run into the 
	    // -----    situation where the command string is too long
	    // -----    for our buffer, set the parent process id to 
	    // -----    -1 as an indicator that this record is bogus.
	    int pid, ppid;
	    char cmd[128], status;
	    fscanf (statStream, "%d %127s %c %d", &pid, cmd, &status, &ppid);
	    fclose (statStream);
	    if (strlen (cmd) > 126) currentProc->ppid = -1;
	    else currentProc->ppid = ppid;
	    currentProc->cmd = 
	        (char *) malloc (sizeof (char) * (strlen(cmd) + 1));
	    strcpy (currentProc->cmd, cmd);

	    // ----- Set up for the next pass through the loop.
	    currentProc = nextProc;
	}
    }

    // ----- Close the /proc dir.
    closedir (procDirStream);
    return (firstProc);
}

int isPid (
    char * dirEntryString
) {
    return (isdigit (dirEntryString[0]));
}

