// +++ 	Name: sockUtil.C
//	Purpose: A system library that supports the sockets operations. Most
//		 of the routines were adopted from sockfaq web site.
//
// History:
// 23-dec-04 P. Wu	Initial
//
// ---*************************************************************************
#include <stdio.h>
#include <errno.h>
#include <sys/times.h>
#include <sys/types.h>
#include <limits.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <time.h>
#include <iostream>
#include "sockhelp.H"
#include "testmode.H"

// +++ 	Function Name: atoport
//	Purpose: Take a service name, and a service type, and return a port
//	number.  If the service name is not found, it tries it as a decimal
//	number.  The number returned is byte ordered for the network.
// ---*************************************************************************
int atoport(const char *service, const char *proto)
{
	int port;
	long int lport;
	struct servent *serv;
	char *errpos;

	// First try to read it from /etc/services
	serv = getservbyname(service, proto);

	if (serv != NULL)
		port = serv->s_port;
	else { // Not in services, maybe a number?
		lport = strtol(service,&errpos,0);
		if ( (errpos[0] != 0) || (lport < 1) || (lport > 65535) )
			return -1; // Invalid port address.
		port = htons(lport);
	}
	return port;
}

// +++ 	Function Name: in_addr
//	Purpose: Converts ascii text to in_addr struct.  NULL is returned if
//	the address can not be found.
// ---*************************************************************************
struct in_addr *atoaddr(const char *address)
{
	struct hostent *host;
	static struct in_addr saddr;

	/* First try it as aaa.bbb.ccc.ddd. */
	saddr.s_addr = inet_addr(address);
	if (saddr.s_addr != -1) {
		return &saddr;
	}
	host = gethostbyname(address);
	if (host != NULL) {
		return (struct in_addr *) *host->h_addr_list;
	}
	return NULL;
}

// +++ 	Function Name: get_connection
// Purpose: This function listens on a port,and returns connections. It forks
// returns off internally, so your main function doesn't have to worry
// about that.  This can be confusing if you don't know what is going on.
// The function will create a new process for every incoming connection,
// so in the listening process, it will never return.  Only when a connection
// comes in, and we create a new process for it will the function return.
// This means that your code that calls it should _not_ loop.

// The parameters are as follows:
// socket_type: SOCK_STREAM or SOCK_DGRAM (TCP or UDP sockets)
// port: The port to listen on.  Remember that ports < 1024 are
// reserved for the root user.  Must be passed in network byte
// order (see "man htons").
// listener: This is a pointer to a variable for holding the file
// descriptor of the socket which is being used to listen.  It
// is provided so that you can write a signal handler to close
// it in the event of program termination.  If you aren't interested,
// just pass NULL.  Note that all modern unixes will close file
// descriptors for you on exit, so this is not required.
// ---*************************************************************************

int get_connection(int socket_type, u_short port, int *listener)
{
	struct sockaddr_in address;
	int listening_socket;
	int connected_socket = -1;
	int new_process;
	int reuse_addr = 1;

        // Setup internet address information.This is used with the bind() call.
	memset((char *) &address, 0, sizeof(address));
	address.sin_family = AF_INET;
	address.sin_port = port;
	address.sin_addr.s_addr = htonl(INADDR_ANY);

        listening_socket = socket(AF_INET, socket_type, 0);
        if (listening_socket < 0) {
            perror("socket");
            exit(EXIT_FAILURE);
        }

        if (listener != NULL)
            *listener = listening_socket;

        setsockopt(listening_socket, SOL_SOCKET, SO_REUSEADDR, &reuse_addr,
                   sizeof(reuse_addr));

        if (bind(listening_socket, (struct sockaddr *) &address,
            sizeof(address)) < 0) {
                perror("bind");
                close(listening_socket);
                exit(EXIT_FAILURE);
        }

        if (socket_type == SOCK_STREAM) {
        // Queue up to five connections before having them automatically
        // rejected.
            listen(listening_socket, NUM_OF_PORTS);

            while(connected_socket < 0) {
                connected_socket = accept(listening_socket, NULL, NULL);
                if (connected_socket < 0) {
                // Either a real error occured, or blocking was interrupted for
                // some reason. Only abort execution if a real error occured.
                    if (errno != EINTR) {
                        perror("accept");
                        close(listening_socket);
                        exit(EXIT_FAILURE);
                    } else {
                        continue;  // don't fork - do the accept again.
                    }
            }

            new_process = fork();
            if (new_process < 0) {
                perror("fork");
                close(connected_socket);
                connected_socket = -1;
            }
            else { // We have a new process...
                if (new_process == 0) {
                // This is the new process.
                    close(listening_socket); // Close out copy of this socket.
                    if (listener != NULL)
                        *listener = -1; // Closed in this process.  We are not
                                        // responsible for it.
                }
                else {
                // This is the main loop. Close copy of connected socket, and
                // continue loop.
                    close(connected_socket);
                    connected_socket = -1;
                }
            }
        }
        return connected_socket;
    }
    else
        return listening_socket;
}

// +++ 	Function Name: get_port
//	Purpose: A high level interface to take a service name, and a service
//	type, and return a port number.
// ---*************************************************************************
int get_port(const char *service, int type)
{
    // First convert service from a string, to a number.
    int port = -1;

    if (type == SOCK_STREAM)
        port = atoport(service, "tcp");
    if (type == SOCK_DGRAM)
        port = atoport(service, "udp");
    return port;
}

// +++ 	Function Name: make_connection
//	Purpose: This is a generic function to make a connection to a given
//	server/port.  service is the port name/number, type is either
//	SOCK_STREAM or SOCK_DGRAM, and netaddress is the host name to connect
//	to.  The function returns the socket, ready for action.
// ---*************************************************************************
int make_connection(const char *service, int type, const char *netaddress,
                int port)
{
    struct in_addr *addr;
    int sock, connected;
    struct sockaddr_in address;

    if (port == -1)
    {
        return -1;
    }

    addr = atoaddr(netaddress);
    if (addr == NULL) {
        // Invalid network address.
        return -1;
    }

    memset((char *) &address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_port = (port);
    address.sin_addr.s_addr = addr->s_addr;

    sock = socket(AF_INET, type, 0);


    if (type == SOCK_STREAM) {
        connected = connect(sock, (struct sockaddr *) &address,
                            sizeof(address));
        if (connected < 0) {
            // perror("connect");
            close(sock); // PWU - added to close the socket
            return -1;
        }
        return sock;
    }
    // Otherwise, must be for udp, so bind to address.
    if (bind(sock, (struct sockaddr *) &address, sizeof(address)) < 0) {
        perror("bind");
        return -1;
    }
    return sock;
}

// +++ 	Function Name: ignore_pipe
// Purpose: This ignores the SIGPIPE signal.  This is usually a good idea,
// since the default behaviour is to terminate the application. SIGPIPE is
// sent when you try to write to an unconnected socket.  You should
// check your return codes to make sure you catch this error! */
// ---*************************************************************************
void ignore_pipe(void)
{
struct sigaction sig;

    sig.sa_handler = SIG_IGN;
    sig.sa_flags = 0;
    sigemptyset(&sig.sa_mask);
    sigaction(SIGPIPE,&sig,NULL);
}

// +++ 	Function Name: getTestModeQuery
// Purpose: To request the latest mode status from TMB at a given input
// hostname via a socket request. Then return the result mode status to
// the caller. The default is OPERATION_MODE - meaning that not TMBs are
// running.
// ---*************************************************************************
int getTestModeQuery(char *hostname, int port)
{
    int sock=-1;
    struct sockdata_t input_buf;
    struct sockdata_t output_buf;
    ssize_t status = -1;

    sock = make_connection(DEFAULT_PORT_NUM, SOCK_STREAM, hostname, port);
    std::cout << "Connected on socket " << sock << std::endl;
    if (sock == -1) {
        close(sock);
        return OPERATION_MODE;
    }

    sprintf(input_buf.sender,"gtm");
    sprintf(input_buf.msg,"RequestForTestModeInfo");

    send(sock,(const void*) &input_buf, sizeof(input_buf),0);
    std::cout << "Sent socket msg """ << input_buf.msg << """" << std::endl;

    status = recv(sock, (void*) &output_buf,sizeof(output_buf),0);
    close(sock);

    if (status != -1) {
	  std::cout << "Received socket msg """ << output_buf.msg
	      << """" << std::endl;
	  if (strcmp(output_buf.msg,"Test")== 0) {
		  return TEST_MODE;
	  }
	  else if (strcmp(output_buf.msg,"Practice") == 0) {
		  return PRACTICE_MODE;
	  }
	  else if (strcmp(output_buf.msg,"Panic") == 0) {
		  return PANIC_MODE;
	  }
	  else {
		  return OPERATION_MODE;
	  }
    }
    else
    {
    	std::cout << "Warning - No socket msg was received; everything will crash!" << std::endl;
    	return PANIC_MODE;
    }
}

