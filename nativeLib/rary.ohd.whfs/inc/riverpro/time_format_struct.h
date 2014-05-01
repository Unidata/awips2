/*******************************************************************
   time_format_struct.h
   
   PURPOSE
   To serve as include file for structures and constants for the time
   value in the output.
   
   NOTES

********************************************************************/

#ifndef TIME_FORMAT_STRUCT_H
#define TIME_FORMAT_STRUCT_H

#include "template_defs.h"      /* definitions */

/* define a structure for the name and the index to each format */

typedef struct 
{
   char 		name[MAXLEN_TIME_FORMAT];
   time_format_types	index;
} time_format_struct;  
   
#endif
