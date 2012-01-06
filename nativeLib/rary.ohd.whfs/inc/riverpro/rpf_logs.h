/*********************************************************************
   rpf_logs.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef RPF_LOGS_H
#define RPF_LOGS_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_file_defs.h"
#include "function_defs.h"


#include "rpf_converts.h"            /* other functions protos */

#include "DbmsDefs.h"

#include "GeneralUtil.h"              /* for get_apps_defaults() proto */



void get_envdirs(char user_suffix[]);

void log_msg(const char *msgname,
	     const char *addmsg);

void close_msglog();


/* this prototype is here, even though the function is not.
   this function actually has two versions, one for the 
   interactive version of RiverPro, the other for the non-interactive
   version.  both have the same signature. */
   
void abort_rpf(char *outmsg);

 
#endif
