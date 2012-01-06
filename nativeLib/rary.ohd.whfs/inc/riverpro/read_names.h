/*********************************************************************
   read_names.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef READ_NAMES_H
#define READ_NAMES_H

#include "rpf_err_defs.h"           /* definitions */
#include "rpf_file_defs.h" 

#include "temp_names_struct.h"      /* structures */

#include "read_lines.h"		    /* protos */



void read_names(char			*selected_office,
		templatenames_struct	*templatenames);


#endif
