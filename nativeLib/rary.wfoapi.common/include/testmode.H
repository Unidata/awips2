// +++ 	Name: testmode.H
//Purpose: This is a general header file for most of the test mode programs.
// ---*************************************************************************

#ifndef _TEST_MODE_H_
#define _TEST_MODE_H_

#include "sockhelp.H"
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define BF_SIZE 128
struct sockdata_t
{
   char msg[BF_SIZE];
   char sender[BF_SIZE];
};

#define TEST_MODE 11
#define PRACTICE_MODE 12
#define PANIC_MODE 13
#define OPERATION_MODE 15
#define EXIT_MODE 16

#define DEFAULT_PORT_NUM "1234"

#define FAIL 0
#define PASS 1
#endif

