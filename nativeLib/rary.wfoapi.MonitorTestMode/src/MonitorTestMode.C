// +++ 	Name: MonitorTestMode.c
//Purpose: This program is acting like a monitoring agent to the running
//	 TMB processes from the graphic and text workstations. In a
//	 normal operations, there are 2 MonitorTestMode programs running
//	 and one on each of the graphic and text workstations. Every
//	 few seconds, it will request the test mode status from both
//	 of the TMBS and immediately launch a GUI program (showBanner) that
//	 displays a small blinking window with the corresponding status to
//	 the screen(s) or running workstation.
//
//Inputs:  This program takes no input arguments to run.
//
//History:
//15-mar-05 P. Wu - SLC RFC requests that Xt (Text) workstation may not exist.
//		    In that case, the test mode program will still running on
//		    the graphic station alone (runLocal=1).
//30-mar-05 P. Wu - Correct an error. Should check "l" instead of "x" in ping
//                  call.
//
//15-may-05 P. Wu - Make the banner movable in test/practice mode.
// ---*************************************************************************
#include <stdlib.h>
#include <iostream>
#include "testmode.H"
#include "tmbUtil.H"
#include "sockhelp.H"

// +++ 	Function Name: main driver   ******************************************
//
//Purpose:  This is the main interface to monitor current status (mode) from
//the TMBs programs. It first gets the hostname and the port number. By
//continuosly calling the getTestModeQuery routine to access the status from
//each of the 2 TMBs, it can determine the latest mode status and thus
//displaying the status via the showBanner programs. It also checks if the
//user is still logon and acts appropriately by calling tmb_exit to request
//the TMB program to exit.
//
// ---*************************************************************************
int main(int argc, char *argv[])
{
    char hostname[256];
    char otherhostname[256];
    char command[512];
    char kcommand[512];
    char windowManager[32];
    char user[128];
    int len;
    int rc, rc1, rc2;
    int port;
    int n;
    int nsec = 3;
    int nskip = 0;
    int runLocal = 0; // Meaning both lx and xt workstations are present
    int new_state = 0; // means un-initialized
    int run_status = 0; // means initialized 1: running
    bool userLoggedIn = true;

    // Check for input argument
    if (argc == 2) {
        n = atoi(argv[1]);
        if (n > 0 && n < 10)
                nsec = n;
    }

    std::cout << "Startup" << std::endl;
    sprintf(user,"%s",getenv("USER"));
    gethostname(hostname,256);
    len = strlen(hostname);

    if (hostname[0] == 'x') strcpy(otherhostname,"lx");
    else strcpy(otherhostname,"xt");

    memcpy(&otherhostname[2],&hostname[2],len-1);

    // If this is not running in xt,
    // Check if the text workstation is present or not
    if (hostname[0] != 'x') {
        sprintf(command,"ping -c 1 -w 1 %s >/dev/null 2>/dev/null",otherhostname);
        rc = system(command);
        if (rc != 0)
            runLocal = 1;
    }

    ignore_pipe();

    port = get_port(DEFAULT_PORT_NUM, SOCK_STREAM);

    std::cout << "port=" << port << std::endl;
    std::cout << "nsec=" << nsec << std::endl;

    sprintf(kcommand,"%s/bin/showBanner",getenv("FXA_HOME"));

    rc = findprocess("kwrapper");
    if (rc > 0)
	    strcpy(windowManager,"kde");
    	else
	    strcpy(windowManager,"gnome");

    if (hostname[0] != 'x')
        sleep (2);

    while (1) {

        sleep(nsec);
        // Is the user still logon?

        // Ensure That Both The Local User And Remote User Are Logged In.
        // However, In This Case We Only Check The Remote Login If The
        // Local User Is Not Logged In?
        if (check_local_login(user) == 0)
        {
            rc = 0;
            if (!runLocal)
            {
                rc = check_remote_login(otherhostname);
            }

            if (rc == 0)
            {
                std::cout << "User logout on both local & remote consoles."
                    " Exiting." << std::endl;

                sprintf(command,"$FXA_HOME/bin/tmb_exit %s %s ",hostname,
                    DEFAULT_PORT_NUM);
                rc = system(command)>>8;
                std::cout << "First invocation of tmb_exit" << std::endl;
                if (!runLocal) {
                    sprintf(command,"$FXA_HOME/bin/tmb_exit %s %s ",
				    otherhostname, DEFAULT_PORT_NUM);
                    rc = system(command)>>8;
                }
            }
            else
            	run_status = 0;
        }

        // Only Perform The Following Steps If There Is Both A
        // Text Workstation And A Graphical Station.
        if (!runLocal)
        {
        	// Ensure That The Remote User Is Still Logged In.
        	if (check_remote_login(otherhostname) == 0)
        	{
        		// The Remote User Is No Longer Logged In, Panic!

        		// We Will Only Maintain This State If We Are Not In
        		// Or Have Not Switched Back To Operational Mode.
        		rc1 = getTestModeQuery(hostname, port);
        		if (rc1 != OPERATION_MODE)
        		{
        			rc1 = PANIC_MODE;
        		}
        		else
        		{
        			/* Ensure That We Terminate */
        			rc2 = rc1;
        		}
        		userLoggedIn = false;
        	}
        }

        // Only Verify Test Modes If Both Users Are Still
        // Logged In.
        if (userLoggedIn)
        {
        	rc1 = getTestModeQuery(hostname, port);
        		std::cout << "getTestModeQuery to " << hostname << " returned " <<
        		rc1 << std::endl;

        	if (!runLocal)
        	{
        		rc2 = getTestModeQuery(otherhostname, port);
        		std::cout << "getTestModeQuery to " << otherhostname <<
        			" returned " << rc2 << std::endl;
        	}
        	else
        	{
        		rc2 = rc1; // running only on local machine
        	}

        	if (rc1 != rc2)
        	{
        		if (nskip < 1)
        		{
                    nskip++;
                    continue;
        		}
        		std::cout << "Different modes on lx and xt!" << std::endl;
        		std::cout << "lx: " << rc1 << std::endl;
        		std::cout << "xt: " << rc2 << std::endl;
        	}
        }

        if (rc1 == OPERATION_MODE && rc2 == OPERATION_MODE)
        {
            killprocess(kcommand);
            fprintf(stderr,"15\n");
            std::cout << "Operational mode.  Exiting." << std::endl;
            exit(1);
        }
        else if (rc1 == TEST_MODE && rc2 == TEST_MODE)
        {
        	if (new_state == TEST_MODE && run_status != 0 )
        		continue;
            sprintf(command,"$FXA_HOME/bin/showBanner_script %s "
                            "test %d 100 780 %s &", hostname, nsec, windowManager);
            std::cout << "Test Mode" << std::endl;
            new_state = TEST_MODE;
        }
        else if (rc1 == PRACTICE_MODE && rc2==PRACTICE_MODE)
        {
        	if (new_state == PRACTICE_MODE && run_status != 0)
        		continue;
            sprintf(command,"$FXA_HOME/bin/showBanner_script %s "
                            "practice %d 100 780 %s &",hostname, nsec, windowManager);
            std::cout << "Practice  Mode" << std::endl;
            new_state = PRACTICE_MODE;
        }
        else
        {
        	std::cout << "Panic Mode " << new_state << "  " << run_status << std::endl;
        	//if (new_state == PANIC_MODE && run_status != 0)
        	// continue;
            sprintf(command,"$FXA_HOME/bin/showBanner_script %s "
                            "obnoxious %d 100 780 %s &",hostname, nsec+1, windowManager);
            std::cout << "Panic Mode" << std::endl;
            new_state = PANIC_MODE;
            // sleep (1);
        }

        std::cout << "Starting showBanner_script with cmd line """ << command <<
                """" << std::endl;

        if (run_status == 1)
        	killprocess(kcommand);

       	rc1 = system(command);
	run_status = 1;


    } // while
}
