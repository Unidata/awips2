// +++ 	Name: tmbUtil.H
//Purpose: This is a header file for tmbUtil.C. A utility library
//          that check if user is logon or a process is running or not.
// ---*************************************************************************

#ifndef _TMBUTIL_H_
#define _TMBUTIL_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int check_local_login(char * user);
int check_remote_login(char *hostname);
int call_consoleUser(char *command, char *searched_user);
int another_atoi(char *buffer);
int findprocess(char* processName);
int killprocess(char* processName);
int check_desktops_or_workspace();
#endif

