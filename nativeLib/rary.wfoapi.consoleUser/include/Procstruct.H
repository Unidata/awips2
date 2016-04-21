// ******************************************************************
// +++ Procstruct.H: Interface to get linked list of process structures 
//
//
// History
// 17-dec-04	S. Davison	Initial
//
// --- **************************************************************

#ifndef _Proctstruct_H_
#define _Proctstruct_H_

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <pwd.h>



class Procstruct {
    public:
        Procstruct * 	next;
        int 		pid;
        int 		ppid;
        char * 		cmd;
	uid_t		uid;
	Procstruct (void);
	~Procstruct (void);
        static Procstruct * buildProcStructList (void);
};

static int isPid (char *);

#endif

