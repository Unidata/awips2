// ******************************************************************
// +++ getConsoleUser.C: Function to get uid of user logged in at console
//
//
//    Only works for gnome or kde.
//
//    The routine searches the /proc file system for process
//    subdirectories.  When it finds such a directory, it gets the
//    program name (among other things) out of the stat file.  If
//    it finds a program named gnome-session or a program named
//    ksmserver it assumes that the user running the program is
//    logged in at the console. In OB6 (Metacity WM), startkde is
//    used instead of ksmserver.
//
//    So getConsoleUser, as currently set up, will work only if the
//    console user can be assumed to be logged in under either kde
//    or gnome.  To make it applicable to other desktops, you
//    could add more program names to the test.
//
//    If you can count on the login being managed by gdm, then I
//    think you could do it in a desktop-independent manner by
//    looking at the descendents of the gdm processes.
//
//    History: April 8th,2005 - replaced ksmserver by startkde
//             during the search for KDE environment
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
#include "getConsoleUser.H"


// *********************************************************************
// +++ getConsoleUser: Return UID of user logged in at console, 1 if none
//
//   Only works for gnome and kde.  Search /proc for gnome-session or
//   ksmserver.
//
//   No memory cleanup.  That must be added if this routine will ever
//   be called more than once.
//
// --- *****************************************************************
uid_t
getConsoleUser (
    void
) {

    // ----- Set a default value for consoleUserId.  This is what
    // -----    we return if we don't find any of the console
    // -----    processes.
    uid_t		consoleUserId = 1;

    // ----- Get a list of all the processes.
    Procstruct * procStructList = Procstruct::buildProcStructList ();

    // ----- Search through the list for one of the magic processes.
    for (Procstruct * procPtr = procStructList; procPtr->next;
        procPtr = procPtr->next) {
	if (strcmp (procPtr->cmd, "(gnome-session)") == 0 ||
	    strcmp (procPtr->cmd, "(startkde)") == 0) {
	    // ----- We found one of the processes.  Set up the return
	    // -----    value and exit the loop.
	    consoleUserId = procPtr->uid;
	    break;
	}
    }

    // ----- Clean up the process list.
    delete (procStructList);

    // ----- Return the console user ID.
    return (consoleUserId);
}


