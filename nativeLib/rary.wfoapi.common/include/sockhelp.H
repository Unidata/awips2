
#ifndef _SOCKHELP_H_
#define _SOCKHELP_H_

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <unistd.h>
#include <netinet/in.h>
#include <limits.h>
#include <netdb.h>
#include <arpa/inet.h>

#define NUM_OF_PORTS 5   /* changed from 5 to 1 */

int atoport(const char *service, const char *proto);
struct in_addr *atoaddr(const char *address);
int get_connection(int socket_type, u_short port, int *listener);
int get_port(const char *service, int type);
int make_connection(const char *service, int type, const char *netaddress,
                int port);
void ignore_pipe(void);
int getTestModeQuery(char *hostname, int port);
#endif

