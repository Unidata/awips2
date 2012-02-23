/************************************************************************
 * GEMINC.H								*
 *									*
 * This include file will include all necessary standard C include	*
 * files.								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * S. Jacobs/NMC	 7/95	Added sys/wait.h, stdarg.h, ctype.h	*
 * S. Jacobs/NCEP	11/95	Added sys/time.h			*
 * G. Krueger/EAI	 3/96	Added limits.h				*
 * T. Piper/GSC		 5/01	Added strings.h for AIX only		*
 * T. Piper/SAIC	10/01	Added float.h for Linux 2.4.2		*
 * D. Kidwell/NCEP      11/01   Added cond. compile of ipc and msg      *
 * T. Piper/SAIC	 3/03	Removed malloc.h			*
 * R. Tian/SAIC		10/04	Added pwd.h				*
 ***********************************************************************/

#ifndef _GEMINC_H
#define _GEMINC_H

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <pwd.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef AIX
#include <strings.h>
#endif
#include <time.h>
#include <unistd.h>

/*
 * The following conditional compile section is required for successful
 * compilation of GEMPAK code when using the melbufr library on Linux 
 * platforms.  NO_MSG_IPC is defined as part of BUFRFLAGS for Linux in
 * the .cshrc file.
 */
#ifndef  NO_MSG_IPC
#include <sys/ipc.h>
#include <sys/msg.h>
#endif

#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <Xm/XmAll.h>

#endif  /* _GEMINC_H */
