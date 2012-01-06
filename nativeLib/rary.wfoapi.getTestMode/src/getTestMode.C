// *************************************************************
// +++ getTestMode.C: Application Program interface to the workstation mode
//
//	Purpose: This program is an interface to the AWIPS applications such
//		 as D-2D, WWA and RiverPro. It requests the running TMBs from
//		 both graphic and text workstation on the test status or the
//		 current operating mode. It returns the status to the calling
//		 application via the exit call. The AWIPS applications can then
//		 retrieve the return value from the $status variable in the
//		 the calling script.
//
//	Inputs:  This program normally take no input arguments to run. But for
//		 demonstration and help purposes. It takes a single argument
//		 as followings:
//		 (1) getTestMode --returnValue=test
//		 (2) getTestMode --returnValue=practice
//		 (3) getTestMode --returnValue=operational
//		 (4) getTestMode -verbose
//
//
//  Overview of getTestMode Design:
//
//  The getTestMode binary executable is the interface for
//  application programs to the workstation mode maintenance
//  system.  Applications should invoke getTestMode and
//  check its exit status to determine the workstation mode.
//
//  Usage:
//      getTestMode [--verbose] [--returnValue=test|practice|operational]
//
//  (For an overview of the workstation mode maintenance
//  system, please see the header comment in tmcp.C.)
//
//  If you provide the --returnValue option to getTestMode,
//  it will not check the workstation mode, but will instead
//  return the value for the specified mode.  That option is
//  provided to make the getTestMode interface self-
//  documenting.
//
//  If you provide the --verbose option to getTestMode, then
//  the exit value will also be printed to stdout.
//
//  Certain error conditions (please see the header comment
//  in tmcp.C) make it impossible for getTestMode to
//  determine the workstation mode.  In that case, it will
//  return value different from test, practice or
//  operational (shorthand: panic mode).  If the workstation
//  is in panic mode, it means there is no way for
//  getTestMode (and hence the application that calls it) to
//  determine what mode the user believes the workstation is
//  in.  The application must take appropriate action.  (In
//  most cases, this probably means that the application
//  would display an error message on its own GUI and then
//  exit.)
//
//  To determine the workstation test mode, getTestMode
//  checks for tmb processes running on the text and
//  graphics workstations.  If tmb is not running on either
//  of those workstations, that means the workstation is in
//  normal operational mode.
//
//  If the tmb process is running on both workstations,
//  getTestMode interrogates the tmb processes to find out
//  what mode the workstation is in.  If both tmb instances
//  report the same mode, getTestMode reports that mode back
//  to the application.  If the two instances report
//  different modes, or if there is a tmb process running on
//  one workstation and not on the other, then getTestMode
//  returns a panic mode value.
//
//  In the errant test message meeting on 02-mar-05, FSL
//  (Darien and Jim) asked for a version of getTestMode that could
//  be used for development testing by developers who might not
//  be working in a standard AWIPS runtime configuration with an
//  LX and an associated XT. The problem developers would run
//  into in that situation is that they would not be able to test
//  their applications' behavior under workstation test and
//  practice modes since those modes require that both LX
//  and XT be present.
//  To meet this need, Stowe Davison suggest that getTestMode look for
//  a file in the user's home directory called ~/devEnvTestMode.txt.The
//  file would contain one of the following four strings, and nothing else:
//  TEST
//  PRACTICE
//  OPERATIONAL
//  PANIC
//
//  If getTestMode found a file of that name and if the file contained
//  one and only one of the above strings, then getTestMode would
//  return the corresponding value.  That way a developer could force
//  whichever value was needed from getTestMode, and all the rest
//  of getTestMode's behavior would be unaffected.
//
// History:
// 23-dec-04 P. Wu   Initial
// 03-Mar-05 P. Wu   Added code to support development control environment
// 15-mar-05 P. Wu - SLC RFC requests that Xt (Text) workstation may not exist.
//		    In that case, the test mode program will still running on
//		    the graphic station alone (runLocal=1).
// ---*************************************************************************
#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <ctype.h>
#include "LogStream.H"
#include "testmode.H"

#define DEVRUN true

using namespace std;
static int rstatus = 0;

// +++ 	Function Name: ExitWithHelp *******************************************
//Purpose:  List all the options or command line arguments available to the
//terminal.
//
// ---*************************************************************************
void ExitWithHelp() {
    fprintf(stderr,"Usage: [--returnValue=test|practice|operational] ");
    fprintf(stderr," [--verbose] \n");
    exit(0);
}

// +++ 	Function Name: getCmdArgs   *******************************************
//Purpose:  Parse the command line arguments and looking for test, practice,
//operational key words and also "verbose" option.
//
// ---*************************************************************************
void getCmdArgs(int argc, char** argv, int *returnValue, int *vbose)
{
    int i;
    for (i=1; i < argc; i++) {
        if (strncmp(argv[i],"--returnValue=test",18) == 0)
                *returnValue = TEST_MODE;
        else if (strncmp(argv[i],"--returnValue=practice",22) == 0)
                *returnValue = PRACTICE_MODE;
        else if (strncmp(argv[i],"--returnValue=operational",25) == 0)
                *returnValue = OPERATION_MODE;

        if (strncmp(argv[i],"--verbose",9) == 0)
            *vbose=1;
    }
}

// +++ 	Function Name: thisIsDevelopmentEnvironment ***************************
//Purpose: Determine if developer has a devEnvTestMode.txt file under the $HOME
//	directory. If so, getTestMode will return the mode based on the content.
// ---*************************************************************************
int thisIsDevelopmentEnvironment()
{
	char buffer[256];
	char home[256];
	int i;
        sprintf(home,"%s/devEnvTestMode.txt",getenv("HOME"));

	ifstream developementFile (home);
	if (! developementFile.is_open()) {
		return (0);
        }
	developementFile.getline (buffer,100);

        developementFile.close();

	for (i=0; buffer[i]!=0;i++)
            buffer[i] = toupper(buffer[i]);
	if (strncmp(buffer,"TEST",4)==0)
		rstatus = TEST_MODE;
	if (strncmp(buffer,"PRACTICE",8)==0)
		rstatus = PRACTICE_MODE;
	if (strncmp(buffer,"OPERATIONAL",11)==0)
		rstatus = OPERATION_MODE;
	if (strncmp(buffer,"PANIC",5)==0)
		rstatus = PANIC_MODE;

	if (rstatus == 0)
		return 0;
	else
  		return 1;
}

// +++ 	Function Name:  getDevelopmentEnvironmentTestMode *******************
//Purpose: If developer has a devEnvTestMode.txt file under the $HOME
//	directory, getTestMode will return the mode based on the content.
// ---*************************************************************************
void getDevelopmentEnvironmentTestMode(int *rc1, int *rc2)
{
    *rc1 = rstatus;
    *rc2 = rstatus;
}
// +++ 	Function Name: main driver   ******************************************
//
//Purpose:  This is the main interface to retrieve current status (mode) from
//the TMBs programs. The first step is to process the command line arguments.
//then it follows by getting the hostname and the port number. By calling
//the getTestModeQuery routine to access the status from each of the 2 TMBs, it
//can determine the latest mode status.
//
// ---*************************************************************************
int main(int argc, char *argv[])
{
    char hostname[256];
    char otherhostname[256];
    int len;
    int rc1, rc2;
    int verbose = 0;
    int returnValue=0;
    int port;
    char command[512];
    int rc = 0;
    int  runLocal = 0; // Meaning both xt and lx workstations are present

    logVerbose << "Startup" << std::endl;

    if (argc > 3) ExitWithHelp();

    getCmdArgs(argc, argv, &returnValue, &verbose);

    if (argc == 2) {
        if (returnValue != 0)
                exit(returnValue);
        if (verbose == 0) ExitWithHelp();
    }

    if (argc == 3) {
        if (verbose == 0) ExitWithHelp();

        if (returnValue != 0 ) {
            if (verbose)
                printf("%d\n",returnValue);
            exit(returnValue);
        }
    }

    gethostname(hostname,256);
    len = strlen(hostname);

    if (hostname[0] == 'x')
        strcpy(otherhostname,"lx");
    else
        strcpy(otherhostname,"xt");

    memcpy(&otherhostname[2],&hostname[2],len-1);

    // If this is running in lx,
    // Check if the text workstation is present or not
    if (hostname[0] == 'l') {
        sprintf(command,"ping -c 1 -w 1 %s >/dev/null 2>/dev/null",otherhostname);
        rc = system(command);
        if (rc != 0)
            runLocal = 1;
    }

#if DEVRUN
    runLocal = 1;
    strcpy(hostname, "localhost");
#endif

    ignore_pipe();

    port = get_port(DEFAULT_PORT_NUM, SOCK_STREAM);

    if (thisIsDevelopmentEnvironment())
	    getDevelopmentEnvironmentTestMode(&rc1, &rc2);
    else {
    	rc1 = getTestModeQuery(hostname, port);

        if (!runLocal)
    	    rc2 = getTestModeQuery(otherhostname, port);
        else
            rc2 = rc1;
    }

    if (rc1 == OPERATION_MODE && rc2 == OPERATION_MODE) {
        if(verbose) fprintf(stderr,"15\n");
        exit(OPERATION_MODE);
    }
    else if (rc1 == TEST_MODE && rc2 == TEST_MODE) {
        if(verbose) fprintf(stderr,"11\n");
        exit(TEST_MODE);
    }
    else if (rc1 == PRACTICE_MODE && rc2==PRACTICE_MODE) {
       if(verbose)  fprintf(stderr,"12\n");
        exit(PRACTICE_MODE);
    }
    else {
        if(verbose) fprintf(stderr,"13\n");
        exit(PANIC_MODE);
    }
}

