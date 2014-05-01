// +++ 	Name: tmb.c
//Purpose: This is a test mode server program that keep track of the running
//	status. Two copies of the program and one each on the graphic and
//	text workstations should be run during the testing. It is launched
//	by the TMCP program. The main duty of this tmb program is to monitor
//	if the user has logoff from the console. Is so, it will change the
//	test status to a panic mode. It also acts as a server to return the
//	status to the client (MonitorTestMode) programs.
//
//Inputs:  This program takes four input arguments to run.
//	(1) Port number e.g. 1234
//	(2) mode: test=0 and practice=1
//	(3) other TMB hostname-to-communicate to
//	(4) Launcher name: tmb or tmcp
//
// History:
// 23-dec-04 P. Wu	Initial
// ---*************************************************************************
#include <ctype.h>
#include <sys/time.h>
#include <iostream>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/times.h>
#include <limits.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include "testmode.H"
#include "tmbUtil.H"

#define SOCKETTIMEOUT 500       // in milli-seconds or 0.5 seconds.

//Checking user login every second. But it is set to 4 secs. for now.
#define LOGIN_CHECK_DELTA 4000      // in milli-seconds or 4 seconds.
//Checking Monitor program every 7 seconds if it is still running.
#define MONITOR_CHECK_DELTA 7000

void handleLoginCheck(char *user, int runLocal);
void handleMonitorCheck(char *monitor, char* hostname, char *wManager );

int Mode;                       // Mode status:Test|Practice|Panic.
int PortNumber;                 // Port Number: default to 1234
char otherTMBHostname[256];     // The other TMB hostname that is running.
float timeNow;                  // Current system time.
struct timeval currentTime;     // Current time needed by gettimeofday.
float timeToCheckLogin;         // The time to check user login.
float timeToCheckMonitor;       // The time to check Monitor program.
static  struct sockdata_t buf;  // Incoming socket buffer.
static  char kcommand[512];

// The socket file descriptor for our "listening" socket
int listening_socket=-1;

// Array of connected sockets so we know who we are talking to
int connectlist[NUM_OF_PORTS];

// Socket file descriptors we want to wake up for, using select()
fd_set socks;
int socketConnected = 0;


// +++ 	Function Name: setnonblocking *****************************************
//Purpose: Manipulate socket file descriptor and set option to non-blocking.
//
// ---*************************************************************************
void setnonblocking(int sock)
{
	int opts;

	opts = fcntl(sock,F_GETFL);
	if (opts < 0) {
		perror("fcntl(F_GETFL)");
		exit(EXIT_FAILURE);
	}
	opts = (opts | O_NONBLOCK);
	if (fcntl(sock,F_SETFL,opts) < 0) {
		perror("fcntl(F_SETFL)");
		exit(EXIT_FAILURE);
	}
	return;
}

// +++ 	Function Name: build_connect_list ************************************
//Purpose:  To build the connection list for the listening sockets and the
//          maximum port is set to 5.
//
// ---*************************************************************************
int build_connect_list(int highsock) {
	int listnum;	     // Current item in connectlist for loops.

	FD_ZERO(&socks);

	FD_SET(listening_socket,&socks);

	// Loops through all the possible connections and adds those sockets
        // to the fd_set.

	for (listnum = 0; listnum < NUM_OF_PORTS; listnum++) {
		if (connectlist[listnum] != 0) {
			FD_SET(connectlist[listnum],&socks);
			if (connectlist[listnum] > highsock)
				highsock = connectlist[listnum];
		}
	}
        return highsock;
}

// +++ 	Function Name: handle_new_connection *********************************
//Purpose:  To handle a new connection and loop through the connection list
//          to find a spot for it in the queue.
//
// ---*************************************************************************
void handle_new_connection() {
	int listnum;	// Current item in connectlist for loops.
	int connection; // Socket file descriptor for incoming connections.

	// We have a new connection coming in! We'll try to find a spot for it
        // in connectlist.
	connection = accept(listening_socket, NULL, NULL);
	if (connection < 0) {
		perror("accept");
		exit(EXIT_FAILURE);
	}

        // Set non-blocking socket connection
	setnonblocking(connection);

	for (listnum=0; (listnum<NUM_OF_PORTS)&&(connection != -1);listnum ++)
		if (connectlist[listnum] == 0) {
			connectlist[listnum] = connection;
			connection = -1;
		}
	if (connection != -1) {
		// No room left in the queue!
		printf("\nNo room left for new client.\n");
		close(connection);
	}
}

// +++ 	Function Name: handleTMCPShutdownMessage ******************************
//Purpose:  Close the listening socket and exit the program.
//
// ---*************************************************************************
void handleTMCPShutdownMessage()
{
        // Clean up all opened sockets and exit
        close(listening_socket);
        exit(0);
}

// +++ 	Function Name:  handleGetTestModeQuery ********************************
//Purpose:  Send the current mode status to the requestor via a return socket.
//
// ---*************************************************************************
void handleGetTestModeQuery(int return_socket)
{
        struct sockdata_t output_buf;

	sprintf(output_buf.sender,"tmb");

        if (Mode == TEST_MODE)
	      sprintf(output_buf.msg,"Test");
        else if (Mode == PRACTICE_MODE)
 	      sprintf(output_buf.msg,"Practice");
        else if (Mode == PANIC_MODE)
 	      sprintf(output_buf.msg,"Panic");

        send(return_socket, (const void*)&output_buf, sizeof(output_buf), 0);
}

// +++ 	Function Name:  handleSocketInput *************************************
//Purpose:  To process any receiving message from the input socket list.
//          Currently, the request options can be:
//          (1) to exit
//          (2) to return the mode status to the sender.
//
// ---*************************************************************************
void handleSocketInput(int listnum)
{
// listnum is the current item in connectlist for loops

	if (recv(connectlist[listnum], (void*)&buf, sizeof(buf), 0) <= 0)
       	{
		// Connection closed, close this end and free up entry in
                // connectlist.
		close(connectlist[listnum]);
		connectlist[listnum] = 0;
	}
	else
	{
		// We got some data, so process it and send it back.
		if (strcmp(buf.msg,"Exit")==0)
	  	{
                    handleTMCPShutdownMessage();
	        }
		else if (strcmp(buf.msg,"RequestForTestModeInfo")==0)
	  	{
                    handleGetTestModeQuery(connectlist[listnum]);
	        }
	}
}

// +++ 	Function Name:  read_socks  *******************************************
//Purpose:  This is a high level interface to process incoming socket messages.
//
// ---*************************************************************************
int read_socks(int highsock)
{
    int listnum;	     // Current item in connectlist for loops.
    int readsocks;	     // Number of sockets ready for reading.
    struct timeval timeout;  // Timeout for select.

    timeout.tv_sec = 0;
    timeout.tv_usec = SOCKETTIMEOUT;

    readsocks = select(highsock+1, &socks, (fd_set *) 0,
        (fd_set *) 0, &timeout);

    if (readsocks < 0) {
        perror("select");
        if (errno == EINTR)
            return 0;
        // Need to bail out ...
	close(listening_socket);
        exit(EXIT_FAILURE);
    }
    if (FD_ISSET(listening_socket,&socks))
	handle_new_connection();

    for (listnum = 0; listnum < NUM_OF_PORTS; listnum++) {
	if (FD_ISSET(connectlist[listnum],&socks))
   	    handleSocketInput(listnum);
    } // for (all entries in queue)
    return 1;
}

// +++ 	Function Name:  main  *************************************************
//Purpose:  This is the main program of the TMB. It invloves checking the
//          input arguments and set up the listening sockets. The last block
//          is basically looping forever to process any incoming socket
//          requests.
//
// ---*************************************************************************
int main (int argc, char *argv[])
{
	struct sockaddr_in server_address; // bind info structure.
	char *ascport;      // ASCII version of the server port.
	int port;           // The port number after conversion from ascport.
	int reuse_addr = 1; // Used so we can re-bind to our port while a
                            // previous connection is still in TIME_WAIT state.
        char hostname[256];
        char command[512];
        char monitor[512];
        int highsock;	    // Highest #'d file descriptor, needed for select()
        char user[128];
        int rc;
        char windowManager[32];
        int runLocal=0;

        // argv[1]: port number
        // argv[2]: mode: test=0 and practice=1
        // argv[3]: other TMB hostname-to-communicate to
        // argv[4]: runLocal: 0=Both, 1=run on Graphic workstation only


	// Make sure we got a port number as a parameter.
	if (argc < 4) {
		printf("Usage: %s PORT\r\n",argv[0]);
		exit(EXIT_FAILURE);
	}

        // Check if another copy of tmb is already running or not
        sprintf(command,"%s/bin/tmb",getenv("FXA_HOME"));
        if (findprocess(command)>1)
	      exit(0);

        if (daemon(1,1) == -1)
                exit(0);

        sprintf(monitor,"%s/bin/MonitorTestMode",getenv("FXA_HOME"));
        sprintf(kcommand,"%s/bin/showBanner",getenv("FXA_HOME"));

        rc = findprocess("kwrapper");
        if (rc > 0)
	    strcpy(windowManager,"kde");
    	else
	    strcpy(windowManager,"gnome");

        // Record the mode passing in.
        Mode = atoi(argv[2]);
        PortNumber = atoi(argv[1]);
        strcpy(otherTMBHostname,argv[3]);
        runLocal = atoi(argv[4]);
        gethostname(hostname,256);

        sprintf(user,"%s",getenv("USER"));

	// Set a flag to indicate this tmb is running on graphic ws
	// else this must be running on text workstation.

        ignore_pipe();

	// Obtain a file descriptor for our "listening" socket.
	listening_socket = socket(AF_INET, SOCK_STREAM, 0);
	if (listening_socket < 0) {
		perror("socket");
		exit(EXIT_FAILURE);
	}
	// So that we can re-bind to it without TIME_WAIT problems.
	setsockopt(listening_socket, SOL_SOCKET, SO_REUSEADDR, &reuse_addr,
		sizeof(reuse_addr));

	// Set socket to non-blocking with our setnonblocking routine.
	setnonblocking(listening_socket);

	// Get the address information, and bind it to the socket.
	ascport = argv[1];              // Read what the user gave us
	port = atoport(ascport,  NULL); // convert to an int
	memset((char *) &server_address, 0, sizeof(server_address));
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	server_address.sin_port = port;
	if (bind(listening_socket, (struct sockaddr *) &server_address,
	  sizeof(server_address)) < 0 ) {
		perror("bind");
		close(listening_socket);
		exit(EXIT_FAILURE);
	}

	// Set up queue for incoming connections.
	listen(listening_socket,NUM_OF_PORTS);

	// Since we start with only one socket, the listening socket, it is
        // the highest socket so far.
	highsock = listening_socket;
	memset((char *) &connectlist, 0, sizeof(connectlist));

        gettimeofday (&currentTime, NULL);
        timeNow = currentTime.tv_sec * 1000 + currentTime.tv_usec / 1000;
	timeToCheckLogin = timeNow + LOGIN_CHECK_DELTA;
        // Make it a 10 seconds wait before checking MonitorTMB
	timeToCheckMonitor = timeNow + MONITOR_CHECK_DELTA;

        while (1) {     // Main server loop - forever.
            highsock = build_connect_list(highsock);

            if (!read_socks(highsock))
                continue;

            gettimeofday ( &currentTime, NULL );
            timeNow = currentTime.tv_sec * 1000 + currentTime.tv_usec / 1000;

            // Is the tester still login to the main console?
            if (timeNow > timeToCheckLogin) {
                timeToCheckLogin = timeNow + LOGIN_CHECK_DELTA;
                handleLoginCheck(user, runLocal);
            }

            // Is the local MonitorTestMode program still running?
            if (timeNow > timeToCheckMonitor) {
	           timeToCheckMonitor = timeNow + MONITOR_CHECK_DELTA;
                   handleMonitorCheck(monitor, hostname, windowManager);
            }
        } // while(1)
} // main


// +++ 	Function Name:  handleLoginCheck  *************************************
//Purpose:  Check if user is still login to the console. If not, set to panic
//          mode. Don't bother to call if it is already in panic mode.
//
// ---*************************************************************************
void handleLoginCheck(char *user, int runLocal)
{
    if (Mode !=  PANIC_MODE)
    {
        if (check_local_login(user) == 0)
            Mode =  PANIC_MODE;
    }
    else {
            if (runLocal == 1) {
                close(listening_socket);
                exit(0);
            }

    }
}

// +++ 	Function Name:  handleMonitorCheck  ***********************************
//Purpose:  Check if MonitorTestMode is still running. If not, call showBanner.
//
// ---*************************************************************************
void handleMonitorCheck(char *check_monitor, char* hostname, char *wManager)
{
    char command[512];
    int rc;

    if (findprocess(check_monitor) < 1) {
       Mode =  PANIC_MODE;
       // Run showbanner program directly
       sprintf(command,"$FXA_HOME/bin/showBanner_script %s "
                       "obnoxious 3 5 915 %s &", hostname, wManager);

        rc = system(command)>>8;
   }
}



