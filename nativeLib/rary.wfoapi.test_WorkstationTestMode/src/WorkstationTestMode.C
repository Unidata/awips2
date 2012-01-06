// ****************************************************************
// +++ WorkstationTestMode.C: Function to determine workstation test mode
//
//    Using system(), invoke the getTestMode executable three
//    times with the --returnValue option to find out what the
//    exit values for test, practice, and operational are.  Then
//    call getTestMode once more to get the real value.
//  
//    Return the corresponding WorkstationTestMode::Wstm value
//    to the caller.
//
//    If this routine is called more than once, it only has to
//    complete the first three calls to getTestMode the first
//    time it is called.
//
// History:
// 04-jan-05 davison	Initial.
//
// --- *************************************************************
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "WorkstationTestMode.H"

WorkstationTestMode::Wstm 
WorkstationTestMode::checkWorkstationTestMode (
    void
) {
    static int MAX_CMD_LINE = 2048;
    static int getTestMode_exitValue_test = 0;
    static int getTestMode_exitValue_practice = 0;
    static int getTestMode_exitValue_operational = 0;
    static char * getTestModeCommand = 
        (char *) malloc (sizeof (char) * MAX_CMD_LINE);

    static int firstCall = 1;
    if (firstCall) {
        firstCall = 0;

        char gtm[] = "/bin/getTestMode";
	char gtm_test[] = "/bin/getTestMode --returnValue=test";
	char gtm_practice[] = "/bin/getTestMode --returnValue=practice";
	char gtm_operational[] = "/bin/getTestMode --returnValue=operational";

	int max_fxa_home_string_length = 
	    MAX_CMD_LINE - strlen (gtm_operational) - 1;
	char * fxa_home = getenv ("FXA_HOME");
	if (!fxa_home || strlen (fxa_home) > max_fxa_home_string_length) {
	    printf ("WorkstationTestMode: Env Var FXA_HOME unreasonable"
	        " or undefined. Exiting.\n");
	    exit (1);
	}

        getTestModeCommand[0] = 0;
	strcat (getTestModeCommand, fxa_home);
	strcat (getTestModeCommand, gtm_test);
	getTestMode_exitValue_test = system (getTestModeCommand);

        getTestModeCommand[0] = 0;
	strcat (getTestModeCommand, fxa_home);
	strcat (getTestModeCommand, gtm_practice);
	getTestMode_exitValue_practice = system (getTestModeCommand);

	getTestModeCommand[0] = 0;
	strcat (getTestModeCommand, fxa_home);
	strcat (getTestModeCommand, gtm_operational);
	getTestMode_exitValue_operational = system (getTestModeCommand);

        getTestModeCommand[0] = 0;
	strcat (getTestModeCommand, fxa_home);
	strcat (getTestModeCommand, gtm);
    }

    int getTestMode_exitValue = system (getTestModeCommand);

    if (getTestMode_exitValue == getTestMode_exitValue_test) 
        return (TEST);
    else if (getTestMode_exitValue == getTestMode_exitValue_practice)
        return (PRACTICE);
    else if (getTestMode_exitValue == getTestMode_exitValue_operational)
        return (OPERATIONAL);
    else return (PANIC);
}


