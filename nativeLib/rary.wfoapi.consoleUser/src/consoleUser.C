// ******************************************************************
// +++ consoleUser.C: Get uid and name of user logged in at console
//
//    Usage:
//        consoleUser
//
//    Only works for gnome or kde.
//
//    Prints one line to stdout.  That line contains the UID
//    and the user name, separated by whitespace, of the user
//    logged in at the console.  If no user is logged in at
//    the console the UID of (1) and the associated user name
//    is returned.
//
//    Exit value is always 0 (zero).
//
//    The program searches the /proc file system for process
//    subdirectories.  When it finds such a directory, it gets the
//    program name (among other things) out of the stat file.  If
//    it finds a program named gnome-session or a program named
//    ksmserver it assumes that the user running the program is
//    logged in at the console.
//
//    So consoleUser, as currently set up, will work only if the
//    console user can be assumed to be logged in under either kde
//    or gnome.  To make it applicable to other desktops, you
//    could add more program names to the test.
//
//    If you can count on the login being managed by gdm, then I
//    think you could do it in a desktop-independent manner by
//    looking at the descendents of the gdm processes.
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
#include <iostream>
#include <pwd.h>

#include "getConsoleUser.H"

int main (
    int		argc,
    char *	argv[]
) {
	if (argc != 2)
	{
		std::cout << "Usage: " << argv[0]
		   << " " << "PW_NAME" << std::endl;
		exit(EXIT_FAILURE);
	}

    uid_t consoleUser = getConsoleUser ();
    passwd * consoleUserPwEntry = getpwuid (consoleUser);
//    printf ("%d %s\n", (int)consoleUser, consoleUserPwEntry->pw_name);
    if (strcmp(consoleUserPwEntry->pw_name,argv[1])==0)
    	exit (1);
    else
	exit(0);
}

